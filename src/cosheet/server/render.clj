(ns cosheet.server.render
  (:require (cosheet [entity :as entity]
                     [query :refer [matching-elements matching-items]]
                     [utils :refer [multiset multiset-diff
                                    multiset-to-generating-values
                                    replace-in-seqs]]
                     [debug :refer [simplify-for-print current-value]]
                     [orderable :as orderable]
                     [dom-utils
                      :refer [into-attributes dom-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq cache]])
            (cosheet.server
             [referent :refer [item-referent
                               elements-referent query-referent
                               union-referent parallel-union-referent
                               difference-referent
                               item-or-exemplar-referent
                               semantic-elements-R semantic-to-list-R
                               canonicalize-list immutable-semantic-to-list]]
             [hierarchy :refer [canonical-info
                                canonical-set-to-list canonical-to-list
                                hierarchy-node? hierarchy-node-descendants
                                hierarchy-node-members
                                hierarchy-node-next-level hierarchy-node-extent
                                hierarchy-nodes-extent
                                hierarchy-node-items-referent
                                hierarchy-by-canonical-info
                                item-maps-by-elements
                                items-hierarchy-by-elements
                                hierarchy-node-example-elements]])))

;;; Code to create hiccup style dom for a database entity.

;;; In the following, as well as in other parts of the server code,
;;; most functions don't take reporters as arguments, but may return
;;; reporters. Their names have a suffix of -R.

;;; For a basic entity, we show its contents and its semantic
;;; elements. We don't show its non-semantic elements, which are
;;; identified by, themselves, having :non-semantic elements.  An
;;; element may be marked as a tag by having an element whose content
;;; is :tag. This make that element displayed like a tag, so the :tag
;;; mark is semantic.
;;; So, for example, the entity:
;;;    ("Joe"
;;;        ("married" (1 :order :non-semantic)
;;;        (39 (2 :order :non-semantic)
;;;            ("age" :tag)
;;;            "doubtful"))
;;; would be rendered to dom that tries to convey:
;;;   Joe
;;;     married
;;;     age: 39
;;;             doubtful

;;; We use attributes, as supported by hiccup, to store both html
;;; attributes, and additional attributes that are used by the server.
;;; There are removed by the dom manager before dom is sent to the client.
(def server-specific-attributes
  [      :key  ; A unique client side key (further described below).
    :commands  ; a map from command symbol to expression
               ; to carry out that command. The function
               ; of the expression will be called with:
               ; the mutable store, the key of the item,
               ; the additional arguments passed from
               ; the UI, and the rest of the command expression.
      :target  ; The item (or virtual new item) that the dom refers to
               ; It is itself a map, with some of these keys
               ; :item-referent             Item(s) referred to
               ; :subject-referent          Subject(s) of the virtual item(s)
               ; :adjacent-referent         item(s) adjacent to new item(s)
               ; :adjacent-groups-referent  Groups of item(s) adjacent to
               ;                            new item(s)
               ; :position                  :before or :after item/adjacent
               ; :template                  Added item(s) should satisfy this.
               ; :parent-key                The key of the parent of a virtual
               ;                            new item.
     :sibling  ; The sibling items (or virtual new sibling) that the dom
               ; belongs to, a map with the same keys as :target
         :row  ; The row (or virtual new row) that the dom belongs to,
               ; a map with the same keys as :target
      :column  ; The analog of :row for a column.
   ])

;;; The value of the style attribute is represented with its own map,
;;; rather than as a string, so it can be accumulated. Conviently,
;;; reagent accepts that format too.

;;; We don't create the entire dom in one call, because we want to be
;;; able to reuse subsidiary parts of the dom that the client has,
;;; even if a containing level of the dom changes. For example, if the
;;; containing dom node adds a new child, we don't want to re-compute,
;;; or re-transmit its other children.

;;; Instead, we generate dom that has subsidiary components. These are
;;; specified as
;;;   [:component {:key <key>  (See below.)
;;;                <other attributes to add to the definition's
;;;                 result>}
;;;               definition}
(defn make-component
  "Make a component dom with the given attributes and definition."
  [attributes definition]
  (assert (map? attributes))
  (assert (:key attributes))
  [:component attributes definition])

;;; The dom_tracker code understands these components. It will give
;;; the client a dom with these subsidiary components, with the initially
;;; provided attributes already present, and it will create additional
;;; computations to compute the dom for the components, passing them
;;; as updates to the client once they are computed.

;;; Each component is uniquely identified with a key, as is any other
;;; dom node that the user might interact with. (The dom for a
;;; component need not have a key; the component can add it when
;;; necessary.) There must never be two components or doms with the
;;; same key, even during updates, or all sorts of confusion can
;;; result. The key of a component must also not change throughout the
;;; life of its parent dom, because we keep a mapping between client
;;; dom ids and server keys, which will be broken if the key changes.

;;; The heart of a key is the id of the item the dom is about. But
;;; since there can be several dom nodes about same item, we need more
;;; than that. We thus use a sequence of the path of containment in
;;; the dom, with additional information added to the sequence if
;;; containment is not sufficient to fully disambiguate.

;;; Many DOM generating functions take a map argument, inherited, that
;;; gives information determined by their container. This includes:
(def starting_inherited
  {            :width 1.0  ; A float, giving the width of this dom element
                           ; compared to the minimum width for two column
                           ; format.
              :priority 0  ; How important it is to render this item earlier.
                           ; (Lower is more important.)
           :parent-key []  ; The key of the parent dom of the dom.
                           ; The parent might not actually have a key, if the
                           ; user can't interact with it, but this is a
                           ; that is unique to the parent, and can thus be
                           ; used to generate unique keys for its children.
;                :subject  ; The referent of the subject(s) of the item
                           ; the dom is about, if any. Only required to
                           ; be present if the item is an exemplar.
;               :template  ; The template that the item for this dom,
                           ; and any of its twins, must satisfy, if any.
;  :selectable-attributes  ; Attributes that the topmost selectable parts
                           ; of the dom should have, if any. Typically,
                           ; these are commands for things like new-row.
   })

(defn orderable-comparator
  "Compare two sequences each of whose first element is an orderable."
  [a b]
  (orderable/earlier? (first a) (first b)))

(defn order-items-R
  "Return the items in the proper sort order."
  [items]
  (if (empty? (rest items))
    items
    (expr-let [order-info
               (expr-seq map #(entity/label->content % :order) items)]
      (map second (sort orderable-comparator
                        (map (fn [order item]
                               ;; It is possible for an item not to have
                               ;; order information, especially
                               ;; temporarily while information is being
                               ;; propagated. Tolerate that.
                               (vector (or order orderable/initial) item))
                             order-info items))))))

(defn condition-satisfiers-R
  "Return a sequence of elements of an entity sufficient to make it
  satisfy the condition and nothing extra. The condition must be in
  list form.  and have a nil content.  If part of a condition is not
  satisfied by any element, ignore that part."
  [entity condition]
  (when (not (empty? condition))
    (assert (and (sequential? condition)
                 (nil? (first condition))))
    (expr-let [satisfiers
               (entity/call-with-immutable
                entity
                #(let [elements (entity/elements %)
                       canonical-elements (expr-seq map canonical-info
                                                    elements)]
                   (multiset-to-generating-values
                    (multiset (map canonical-info (rest condition)))
                    canonical-elements elements)))]
      (map #(entity/in-different-store % entity) satisfiers))))

(def item-DOM-R)

(defn vertical-stack
  "If there is only one item in the doms, return it. Otherwise, return
  a vertical stack of the items, with the given class."
  [doms & {:keys [class] :or {class "stack"}}]
  (if (= (count doms) 1)
       (first doms)
       (into [:div (if (empty? doms) {} {:class class})] doms)))

(defn item-component
  "Make a component dom for the given item."
  [item exclude-elements inherited]
  (let [key (conj (:parent-key inherited) (:item-id item))
        excluded (if (empty? exclude-elements) nil (vec exclude-elements))]
    (make-component {:key key} [item-DOM-R item excluded inherited])))

(defn virtual-item-DOM
  "Make a dom for a place that could hold an item, but doesn't.
  inherited must specify one of adjacent-referent or adjacent-groups-referent."
  [key position inherited]
  [:div (into-attributes
         (:selectable-attributes inherited)
         {:class "editable"
          :key key
          :commands {:set-content nil}
          :target (assoc (select-keys inherited
                                      [:template :adjacent-referent
                                       :adjacent-groups-referent])
                         :position position
                         :subject-referent (:subject inherited))})])

(defn item-stack-DOM
  "Given a list of items and a matching list of elements to exclude,
  and attributes that the doms for each of the items should have,
  generate DOM for a vertical list of a component for each item.  The
  excludeds, attributes and inherited can be either a single value to
  be used for all items, or a sequence of values, one per item."
  [items excludeds attributes inherited]
  (vertical-stack
   (map (fn [item excluded attributes inherited]
          (add-attributes (item-component item excluded inherited) attributes))
        items
        (if (sequential? excludeds) excludeds (repeat excludeds))
        (if (sequential? attributes) attributes (repeat attributes))
        (if (sequential? inherited) inherited (repeat inherited)))
   :class "item-stack"))

(defn item-target
  "Return a target for the given item."
  [item inherited]
  (let [template (:template inherited)]
    (cond-> {:item-referent (item-or-exemplar-referent
                             item (:subject inherited))}
      template (assoc :template template))))

(defn hierarchy-add-adjacent-target
  "Given a hierarchy node, generate attributes for the target of
  a command to add an item that would be adjacent to the hierarchy node."
  [hierarchy-node inherited]
  (let [subject (:subject inherited)
        ancestor-props (first (multiset-diff
                               (:cumulative-properties hierarchy-node)
                               (:properties hierarchy-node)))
        conditions (concat (canonical-set-to-list ancestor-props)
                           (rest (:template inherited)))]
   (cond->
       {:subject-referent subject
        :adjacent-groups-referent (hierarchy-node-items-referent
                                   hierarchy-node subject)}
     (not (empty? conditions))
     (assoc :template (list* nil conditions)))))

(defn add-adjacent-sibling-command
  "Given a node from a hierarchy over elements and inherited, update
  inherited to add an element adjacent to the sibling, if that would be
  different than just adding a twin."
  [hierarchy-node inherited]
  (if (empty? (:properties hierarchy-node))
    inherited
    (update-in
     inherited [:selectable-attributes]
     #(into-attributes
       % {:commands {:add-sibling {:select-pattern (conj (:parent-key inherited)
                                                       [:pattern])}}
          :sibling (hierarchy-add-adjacent-target hierarchy-node inherited)}))))

(defn hierarchy-members-DOM
  "Given a hierarchy node with tags as the properties, generate DOM
  for the members. The members of the node may contain an additional
  :exclude-elements field that gives more of their elements not to
  show, typically the ones that satisfy the :template of inherited."
  [hierarchy-node inherited]
  (let [members (hierarchy-node-members hierarchy-node)
        property-list (canonical-set-to-list
                       (:cumulative-properties hierarchy-node))
        template (:template inherited)
        inherited-down (if (or template (not (empty? property-list)))
                         (assoc inherited :template
                                (concat (or template '(nil)) property-list))
                         inherited)]
    (if (empty? members)
      (let [subject (:subject inherited)
            adjacent-item (:item (first (hierarchy-node-descendants
                                         hierarchy-node)))
            adjacent-referent (item-or-exemplar-referent adjacent-item subject)
            example-elements (hierarchy-node-example-elements hierarchy-node)
            key (conj (:parent-key inherited)
                      :example-element
                      (:item-id (first example-elements)))]
        (virtual-item-DOM key :before (assoc inherited-down :adjacent-referent
                                             adjacent-referent)))
      (let [items (map :item members)
            excludeds (map #(concat (:property-elements %)
                                    (:exclude-elements %))
                           members)]
        (item-stack-DOM items excludeds {} inherited-down)))))

(defn hierarchy-properties-DOM-R
  "Return DOM for example elements that give rise to the properties
  of one hierarchy node. Inherited describes the context of the properties."
  [hierarchy-node attributes inherited]
  (let [example-elements (hierarchy-node-example-elements hierarchy-node)]
    (expr-let [ordered-elements (order-items-R example-elements)
               satisfiers (expr-seq
                           map #(condition-satisfiers-R % (:template inherited))
                           ordered-elements)]
      (item-stack-DOM ordered-elements satisfiers
                      attributes inherited))))

(defn tagged-items-properties-DOM-R
  "Given a hierarchy node for tags, Return DOM for example elements
  that give rise to the properties of the node.
  Inherited describes the overall context of the node."
  [hierarchy-node inherited]
  (let [items-referent (hierarchy-node-items-referent
                        hierarchy-node (:subject inherited))
        example-descendant (first (hierarchy-node-descendants
                                   hierarchy-node))
        tags-parent-key (conj (:parent-key inherited)
                              (:item-id (:item example-descendant))
                              :outside)
        inherited-for-tags (assoc (add-adjacent-sibling-command
                                   hierarchy-node inherited)
                                  :parent-key tags-parent-key
                                  :template '(nil :tag)
                                  :subject items-referent)]
    (expr-let
        [dom (if (empty? (:properties hierarchy-node))
               (virtual-item-DOM (conj tags-parent-key :tags) :after
                                 (assoc inherited-for-tags
                                        :adjacent-referent items-referent))
               (hierarchy-properties-DOM-R
                hierarchy-node {:class "tag"} inherited-for-tags))]
        ;; Even if stacked, we need to mark the stack as "tag" too.
        (add-attributes dom {:class "tag"}))))

(def tagged-items-one-column-subtree-DOM-R)

(defn tagged-items-one-column-descendants-DOM-R
   "Return DOM for all descendants of the hierarchy node, as a single column."
  [hierarchy-node inherited]
  (let [members (hierarchy-node-members hierarchy-node)
        nested-inherited (add-adjacent-sibling-command hierarchy-node inherited)
        items-dom (when (not (empty? members))
                    (hierarchy-members-DOM hierarchy-node nested-inherited))]
    (expr-let
        [child-doms (when (:children hierarchy-node)
                      (expr-seq map #(tagged-items-one-column-subtree-DOM-R
                                      % false nested-inherited)
                                (:children hierarchy-node)))]
      (vertical-stack (if items-dom (cons items-dom child-doms) child-doms)))))

(defn tagged-items-one-column-subtree-DOM-R
  "Return DOM for the given hierarchy node and its descendants,
  as a single column."
  [hierarchy-node must-show-empty-labels inherited]
  (expr-let
      [descendants-dom (tagged-items-one-column-descendants-DOM-R
                        hierarchy-node inherited)]
    (if (empty? (:properties hierarchy-node))
        (if must-show-empty-labels
          (expr-let [properties-dom (tagged-items-properties-DOM-R
                                     hierarchy-node inherited)]
            [:div {:class "horizontal-tags-element narrow"}
             properties-dom descendants-dom])
          descendants-dom)
        (expr-let [properties-dom (tagged-items-properties-DOM-R
                                   hierarchy-node inherited)]
          [:div {:class "wrapped-element tag"}
           ;; If the properties-dom is an item-stack, we need to mark it as tag.
           (add-attributes properties-dom {:class "tag"})
           [:div {:class "indent-wrapper"} descendants-dom]]))))

(defn tagged-items-one-column-DOM-R
  [hierarchy inherited]
  (expr-let [doms (expr-seq map #(tagged-items-one-column-subtree-DOM-R
                                  % true inherited)
                            hierarchy)]
    (vertical-stack doms)))

;;; Wrappers are used when one logical cell is broken into several
;;; adjacent divs. This typically means that the first and last of the
;;; divs need to be styled differently from the others. A wrapper will
;;; be called with the div to wrap and with whether it is the first
;;; and/or last div of the logical cell.

(defn nest-wrapper
  "Return a wrapper that nests one level more deeply. This is used
  when a logical cell has been broken into several pieces and one of those
  pieces has been further subdivided.
  We are given an outer wrapper and the position of our sub-part in
  the logical cell. We may get further subdivided. So we return a new wrapper
  that itself expects to be called several times, each time given the position
  of the call with respect to all calls of the subcell. For each call,
  call position->class with the position of the call with respect to all calls
  of our logical cell, and call the outer wrapper with the same information."
  [outer-wrapper position->class generated-first generated-last]
  (fn [body called-first called-last]
    (let [first (and generated-first called-first)
          last (and generated-last called-last)]
      (outer-wrapper
       [:div {:class (position->class first last)} body]
       first last))))

(defn identity-wrapper
  "Return a wrapper that does no wrapping -- just returns the body."
  [body called-first called-last]
  body)

(defn nest-horizontal-tag-wrapper
  [outer-wrapper generated-first generated-last]
  (nest-wrapper outer-wrapper
                (fn [first last] (cond-> "tag horizontal-header"
                                   first (str " top-border")
                                   (not first) (str " indent")
                                   last (str " bottom-border")))
                generated-first generated-last))

(def tagged-items-two-column-subtree-DOMs-R)

(defn tagged-items-two-column-children-DOMs-R
  "Return a seq of doms for the children of the hierarchy node,
  as two columns."
  [hierarchy-node header-wrapper inherited]
  (let [children (:children hierarchy-node)]
    (when children
      (let [wrapper (nest-horizontal-tag-wrapper header-wrapper false false)
            last-wrapper (nest-horizontal-tag-wrapper header-wrapper false true)
            wrappers (concat (map (constantly wrapper) (butlast children))
                             [last-wrapper])]
        (expr-let [child-lists
                   (expr-seq map (fn [child-node wrapper]
                                   (tagged-items-two-column-subtree-DOMs-R
                                    child-node wrapper inherited))
                             children wrappers)]
          (apply concat child-lists))))))

(defn tagged-items-two-column-subtree-DOMs-R
  "Return a sequence of DOMs for the given hierarchy node and its descendants,
  as two columns."
  [hierarchy-node header-wrapper inherited]
  (let [tags-inherited (update-in inherited [:width] #(* % 0.25))
        items-inherited (update-in
                         (add-adjacent-sibling-command hierarchy-node inherited)
                         [:width] #(* % 0.6875))
        members-dom (hierarchy-members-DOM hierarchy-node items-inherited)
        no-children (empty? (:children hierarchy-node))
        wrapper (nest-horizontal-tag-wrapper header-wrapper
                                             true no-children)]
    (expr-let [properties-dom (tagged-items-properties-DOM-R
                               hierarchy-node tags-inherited) 
               child-doms (tagged-items-two-column-children-DOMs-R
                           hierarchy-node header-wrapper inherited)]
      (cons [:div {:class "horizontal-tags-element wide"}
             (wrapper properties-dom true no-children) members-dom]
            child-doms))))

(defn tagged-items-two-column-DOM-R
  [hierarchy inherited]
  (expr-let [dom-lists (expr-seq map #(tagged-items-two-column-subtree-DOMs-R
                                       % identity-wrapper inherited)
                            hierarchy)]
    (vertical-stack (apply concat dom-lists))))

(def tagged-items-two-column-DOM-R)

(defn elements-DOM-R
  "Make a dom for a stack of elements.
   Don't show elements of the elements that are implied by the template."
  [elements must-show-empty-labels inherited]
  (expr-let
      [ordered-elements (order-items-R elements)
       all-labels (expr-seq map #(matching-elements '(nil :tag) %)
                            ordered-elements)
       excludeds (expr-seq map #(condition-satisfiers-R % (:template inherited))
                           ordered-elements)]
    (let [labels (map (fn [all minus]
                        (seq (clojure.set/difference (set all) (set minus))))
                      all-labels excludeds)
          no-labels (every? empty? labels)]
      (if (and no-labels (not must-show-empty-labels))
        (item-stack-DOM ordered-elements excludeds {} inherited)
        (expr-let [item-maps (item-maps-by-elements ordered-elements labels)
                   augmented (map (fn [item-map excluded]
                                    (assoc item-map :exclude-elements excluded))
                                  item-maps excludeds)
                   hierarchy (hierarchy-by-canonical-info augmented)]
          (if (or (< (:width inherited) 1.0) no-labels)
            (tagged-items-one-column-DOM-R hierarchy inherited)
            (tagged-items-two-column-DOM-R hierarchy inherited)))))))

(defn item-content-DOM
  "Make dom for the content part of an item."
  [item content inherited]
  ;; We don't currently handle items as content. That would need
  ;; more interaction and UI design work to deal with the distinction
  ;; between elements of an item and elements on its content.
  (assert (entity/atom? content))
  (let [is-placeholder (and (symbol? content)
                            (= (subs (str content) 0 3) "???"))]
    ;; Any attributes we inherit take precedence over basic commands,
    ;; but nothing else.
    [:div (into-attributes
           (into-attributes {:commands {:set-content nil
                                        :delete nil
                                        :add-element nil
                                        :add-twin nil}}
                            (:selectable-attributes inherited))
           {:class (cond-> "content-text editable"
                     is-placeholder (str " placeholder"))
            :target (item-target item inherited)})
     (cond (= content :none) "" ;; TODO: Make this have special formatting.
           is-placeholder "???"                     
           true (str content))]))

(defn item-without-labels-DOM-R
  "Make a dom for an item,
   given that any of its labels are in excluded-elements.
   We only record the key on the content, not the whole item
   if it contains more than the content."
  [item excluded-elements inherited]
  (println "Generating DOM for"
           (simplify-for-print (conj (:parent-key inherited) (:item-id item))))
  (expr-let [content (entity/content item)
             elements (semantic-elements-R item)]
    (let [key (conj (:parent-key inherited) (:item-id item))
          referent (item-or-exemplar-referent item (:subject inherited))
          elements (remove (set excluded-elements) elements)
          content-dom (add-attributes
                       (item-content-DOM item content inherited)
                       ;; Give it a unique key.
                       ;; This will be overridden to the item's key
                       ;; if the item has nothing but this content.
                       {:key (conj key :content)})]
      (if (empty? elements)
        (add-attributes content-dom {:class "item"})
        (let [inherited-down
              (-> inherited
                  (update-in [:priority] inc)
                  (assoc :parent-key key
                         :subject referent)
                  (dissoc :template :selectable-attributes))]
          (expr-let [elements-dom (elements-DOM-R
                                   elements true inherited-down)]
            [:div {:class "item with-elements"}
             content-dom elements-dom]))))))

;;; TODO: This needs an inherited property saying whether this is in a
;;; selection, so that new items should be :none, rather then the empty string.
;;; TODO: Replace some of the calls to this with calls to
;;; item-without-labels-DOM-R, since the often has excluded any labels.
(defn item-DOM-R
   "Make a dom for an item.
   If the item is a tag, the caller is responsible for tag formatting."
  ([item excluded-elements inherited]
   (item-DOM-R item excluded-elements false inherited))
  ([item excluded-elements must-show-empty-labels inherited]
   (let [key (conj (:parent-key inherited) (:item-id item))]
     (expr-let [labels (entity/label->elements item :tag)]
       (let [labels (remove (set excluded-elements) labels)
             excluded (concat labels excluded-elements)]
         (expr-let [dom (item-without-labels-DOM-R
                         item excluded inherited)]
           (if (and (empty? labels) (not must-show-empty-labels))
             dom
             (let [item-referent (item-or-exemplar-referent
                                    item (:subject inherited))
                     inherited-down (-> inherited
                                        (assoc :template '(nil :tag)
                                               :subject item-referent
                                               :parent-key key)
                                        (dissoc :selectable-attributes))]
               (if (empty? labels)
                 [:div {:class "horizontal-tags-element narrow"}
                  (add-attributes
                   (virtual-item-DOM (conj key :tags) :after
                                     (assoc inherited-down
                                            :adjacent-referent item-referent))
                   {:class "tag"})
                  dom]
                 (expr-let [ordered-labels (order-items-R labels)
                            tags (expr-seq
                                  map #(condition-satisfiers-R % '(nil :tag))
                                  ordered-labels)]
                   [:div {:class "item wrapped-element"}
                    (add-attributes
                     (item-stack-DOM ordered-labels tags {:class "tag"}
                                     inherited-down)
                     {:class "tag"})
                    [:div {:class "indent-wrapper tag"} dom]]))))))))))

;;; --- Tables ---

(def base-table-column-width 150)

(defn table-column-elements-referent
  "Generate the referent for the elements affected by a table header,
  including elements in table request items at or below this header,
  and also all elements in rows that are brought up by the header. The
  arguments are the info-maps of all the header requests the header
  applies to, info maps that cover the conditions it brings up in
  rows, info maps that cover conditions it must not bring up, a
  referent to the rows of the table, and to the subject of the header
  requests."
  [info-maps extent-info-maps negative-info-maps rows-referent header-subject]
  (let [header-refs (vec (map #(item-or-exemplar-referent (:item %)
                                                          header-subject)
                              info-maps)) 
        make-elements-ref #(elements-referent (:item %) rows-referent)
        positive-refs (map make-elements-ref extent-info-maps)
        positives-ref (union-referent (vec (concat header-refs positive-refs)))]
    (if (empty? negative-info-maps)
       positives-ref
       (let [negative-refs (map make-elements-ref negative-info-maps)
             negatives-ref (union-referent (vec negative-refs))]
         (difference-referent positives-ref negatives-ref)))))

(defn attributes-for-header-delete-command
  "Return attributes for the delete command, if it needs special attributes."
  [node example-elements rows-referent subject]
  ;; The special cases are where there is just one element.
  (when (= (count example-elements) 1)
    (let [next-level (hierarchy-node-next-level node)
          non-trivial-children (filter hierarchy-node? next-level)
          delete-referent
          ;; If we don't have children, then deletion removes the
          ;; column request, while if we do, it removes the element
          ;; from requests and from elements in the rows.
          (if (empty? non-trivial-children)
            (item-or-exemplar-referent (:item (first next-level)) subject)
            ;; We don't include the item maps at the next level,
            ;; because removing the element from the corresponding
            ;; requests would leave them with no elements.
            (item-or-exemplar-referent
             (first example-elements)
             (table-column-elements-referent
              (mapcat hierarchy-node-descendants non-trivial-children)
              (hierarchy-nodes-extent non-trivial-children) nil
              rows-referent subject)))]
      {:commands {:delete {:delete-referent delete-referent}}})))

(defn attributes-for-header-add-column-command
  "Return attributes for an add a column command, given the column
  request items that gave rise to the column. elements-template gives
  the template for new elements, while inherited gives the environment
  of the header."
  [column-items elements-template inherited]
  (let [subject (:subject inherited)
        new-elements-template (cons '??? (rest elements-template))
        new-header-template (list* (concat (or (:template inherited) '(nil))
                                           [new-elements-template]))
        ;; There is an item for the new column, which has an element
        ;; satisfying the element template. We want to select that
        ;; element.
        ;; TODO: This doesn't handle header items that are below other
        ;; header items, as there is no query that can pick out the
        ;; new item as opposed to the the copied items. The solution is to
        ;; add an :exclusive option on referent variables, which means
        ;; that they can't match the same item as any other exclusive
        ;; referent. With that, we introduce variables to match each
        ;; of the header items that are above, leaving just the new
        ;; one to match the :v variable.
        element-variable `(:variable
                            (:v :name)
                            (~elements-template :condition)
                            (true :reference))
        select-pattern (conj (:parent-key inherited)
                             [:pattern `(nil ~element-variable)])]
    {:commands {:add-column {:select-pattern select-pattern}}
     :column {:adjacent-groups-referent (parallel-union-referent
                                         (map #(item-or-exemplar-referent
                                                % subject)
                                              column-items))
              :subject-referent subject
              :position :after
              :template new-header-template}}))

(defn add-add-column-commands
  "For each element, which must be in list form, modify inherited to
  have its :selectable-attributes include a new column command, whose
  template includes all the elements above the element."
  [element-lists column-requests header-inherited inherited]
  (let [element-lists-above (map #(concat (take % element-lists))
                                 (range (count element-lists)))]
    (map (fn [element-lists]
           (let [modified-header-inherited
                 (update-in header-inherited [:template]
                            #(list* (concat (or % '(nil)) element-lists)))]
             (update-in inherited [:selectable-attributes]
                        #(into-attributes
                          % (attributes-for-header-add-column-command
                             column-requests (:template inherited)
                             modified-header-inherited)))))
         element-lists-above)))

(defn add-table-header-formatting
  "Given a dom of header elements, return a dom with all the appropriate
  formatting."
  [dom num-columns is-tag]
  (let [width (+ (* num-columns (- base-table-column-width 2)) 2)]
    (add-attributes dom {:class (cond-> "column-header"
                                  is-tag (str " tag"))
                         :style {:width (str width "px")}})))

(defn table-header-node-elements-DOM-R
  "Generate the dom for one node of a table header hierarchy given its
  elements. column-requests gives the items that requested all columns
  that this node covers. header-inherited gives the context of
  the header definition(s) of this column, while inherited gives the
  context for elements of both the header and its entire column."
  [example-elements column-requests header-inherited inherited]
  (assert (not (empty? example-elements)))
  (expr-let
      [ordered-elements (order-items-R example-elements)
       element-lists (expr-seq map semantic-to-list-R ordered-elements)
       excludeds (expr-seq map #(condition-satisfiers-R % (:template inherited))
                           ordered-elements)]
    (let [is-tag (some #{:tag} (:template inherited))]
      (item-stack-DOM
        ordered-elements excludeds
        (if is-tag {:class "tag"} {})
        (add-add-column-commands
         element-lists column-requests header-inherited inherited)))))

(defn table-header-node-DOM-R
  "Generate the dom for a node of a table header hierarchy. The
  row-scope-referent should specify all the row items from which this
  header selects elements."
  [node elements-template rows-referent inherited]
  (let [subject (:subject inherited)
        descendants (hierarchy-node-descendants node) 
        example-elements (hierarchy-node-example-elements node)
        column-referent (table-column-elements-referent
                           descendants (hierarchy-node-extent node) nil
                           rows-referent subject)
        delete-attributes (attributes-for-header-delete-command
                           node example-elements rows-referent subject)
        column-inherited (let [temp (assoc inherited
                                           :template elements-template
                                           :subject column-referent
                                           :width (* 0.75 (count descendants)))]
                           (if delete-attributes
                             (assoc temp :selectable-attributes
                                    delete-attributes)
                             (dissoc temp :selectable-attributes)))]
    (table-header-node-elements-DOM-R
     example-elements (map :item descendants) inherited column-inherited)))

(def table-header-subtree-DOM-R)

(defn table-header-next-level-DOM-R
  "Given something that is either a hieararchy node or element,
  generate its DOM."
  [below non-trivial-siblings elements-template rows-referent
   inherited]
  (if (hierarchy-node? below)
    (table-header-subtree-DOM-R
     below false elements-template rows-referent inherited)
    ;; This is a member that is displayed underneath its node. Since
    ;; the display of the node already presents all the properties, we
    ;; need a header DOM with no elements.
    (let [request-referent (item-or-exemplar-referent
                            (:item below) (:subject inherited))
          exclude-from-members (hierarchy-nodes-extent non-trivial-siblings)
          column-subject (table-column-elements-referent
                          [below] [below] exclude-from-members
                          rows-referent (:subject inherited))
          inherited (-> inherited
                        (assoc :subject column-subject
                               :template elements-template)
                        (update-in
                         [:selectable-attributes]
                         #(into-attributes
                           (into-attributes
                            %
                            (attributes-for-header-add-column-command
                             [(:item below)] elements-template inherited))
                           {:commands
                            {:delete {:delete-referent request-referent}}})))
          key (conj (:parent-key inherited) (:item-id (:item below)))
          is-tag (some #{:tag} elements-template)]
      (cond-> (add-attributes
               (virtual-item-DOM
                key :after (assoc inherited :adjacent-referent column-subject))
               {:style {:width (str base-table-column-width "px")}})
        is-tag (add-attributes {:class "tag"})))))

(defn table-header-subtree-DOM-R
  "Generate the dom for a subtree of a table header hierarchy. 
  elements-template gives what new header items need to satisfy.
  rows-referent should specify all the row items from which this
  header selects elements."
  [node top-level elements-template rows-referent inherited]
  (expr-let
      [node-dom (table-header-node-DOM-R
                 node elements-template rows-referent inherited)]
    (let [node-dom (cond-> node-dom
                     top-level (add-attributes {:class "top-level"}))
          next-level (hierarchy-node-next-level node)
          non-trivial-children (filter hierarchy-node? next-level)]
      (expr-let
          [dom
           (if (and (= (count next-level) 1) (empty? non-trivial-children))
             node-dom
             (let [properties-list (canonical-set-to-list (:properties node))
                   inherited (update-in inherited [:template]
                                        #(list* (concat (or % '(:none))
                                                        properties-list)))
                   is-tag (some #{:tag} elements-template)]
               (expr-let
                   [dom-seqs (expr-seq
                              map #(table-header-next-level-DOM-R
                                     % non-trivial-children
                                     elements-template rows-referent inherited)
                              next-level)]
                 [:div (cond-> {:class "column-header-stack"}
                         top-level (into-attributes {:class "top-level"})
                         is-tag (into-attributes {:class "tag"}))
                  (add-attributes node-dom {:class "with-children"})
                  (into [:div {:class "column-header-sequence"}] dom-seqs)])))]
        (let [is-tag (some #{:tag} elements-template)
              num-columns (count (hierarchy-node-descendants node))]
          (add-table-header-formatting dom num-columns is-tag))))))

(defn table-header-DOM-R
  "Generate DOM for column headers given the hierarchy. elements-template
  gives what new header items need to satisfy.
  The column will contain those elements of the rows that match the templates."
  [hierarchy elements-template rows-referent inherited]
  (expr-let [columns (expr-seq
                      map #(table-header-subtree-DOM-R
                             % true elements-template
                             rows-referent inherited)
                      hierarchy)]
    (into [:div {:class "column-header-sequence"}]
          columns)))

(defn table-hierarchy-node-column-descriptions
  "Given a hierarchy node, for each column under the node,
  return a map:
     :column-item Item that identifies the column.
        :template Condition that each element of the column must satisfy.
      :exclusions Seq of conditions that elements must not satisfy."
  [node]
  (let [next-level (hierarchy-node-next-level node)
        non-trivial-children (filter hierarchy-node? next-level)
        condition (cons nil (canonical-set-to-list
                             (:cumulative-properties node)))
        excluded-conditions (map #(replace-in-seqs
                                   (cons nil (map canonical-to-list
                                                  (:property-canonicals %)))
                                   :none nil)
                                 (hierarchy-nodes-extent non-trivial-children))]
    (mapcat (fn [below]
              (if (hierarchy-node? below)
                (table-hierarchy-node-column-descriptions below)
                [{:column-item (:item below)
                  :template condition
                  :exclusions excluded-conditions}]))
            next-level)))

(defn table-cell-items-DOM-R
  "Return the dom for one cell of a table, given its items.
  Inherited gives the context of each item in the cell."
  [items new-row-template inherited]
  (let [inherited (-> inherited
                      (assoc :width 0.75)
                      (update-in [:selectable-attributes]
                                 #(into-attributes
                                   % {:commands {:add-row nil}
                                      :row {:item-referent (:subject inherited)
                                            :template new-row-template}})))]
    (expr-let
        [dom (if (empty? items)
               ;; TODO: Get our left neighbor as an arg, and pass it
               ;; in as adjacent information for new-twin.
               (virtual-item-DOM
                (:parent-key inherited) :after
                (assoc inherited :adjacent-referent (:subject inherited)))
               (elements-DOM-R items false inherited))]
      (add-attributes dom {:class "table-cell has-border"}))))

(defn table-cell-DOM-R
  "Return the dom for one cell of a table, given its column description."
  [row-item new-row-template
   {:keys [column-item template exclusions]} ;; A column header description
   inherited]
  (let [inherited-down (assoc inherited
                              :parent-key (conj (:parent-key inherited)
                                                (:item-id column-item))
                              :template template)]
    (expr-let [matches (matching-elements template row-item)
               do-not-show (when exclusions
                             (expr-seq map #(matching-elements % row-item)
                                       exclusions))]
      (let [elements (seq (clojure.set/difference
                           (set matches)
                           (set (apply concat do-not-show))))]
        (table-cell-items-DOM-R elements new-row-template inherited-down)))))

(defn table-row-DOM-R
  "Generate dom for a table row."
  [row-item row-key new-row-template column-descriptions inherited]
  (let [inherited (-> inherited
                      (assoc :parent-key row-key)
                      (update-in [:subject]
                                 #(item-or-exemplar-referent row-item %)))]
    (expr-let [cells (expr-seq map #(table-cell-DOM-R
                                     row-item new-row-template % inherited)
                               column-descriptions)]
      (into [:div {}] cells))))

(defn table-row-DOM-component
  "Generate a component for a table row."
  [row-item new-row-template column-descriptions inherited]
  (let [row-key (conj (:parent-key inherited) (:item-id row-item))]
    (make-component
     {:key row-key :class "table-row"}
     [table-row-DOM-R
      row-item row-key new-row-template column-descriptions inherited])))

(defn add-element-to-entity-list
  [entity element]
  (concat (if (sequential? entity) entity (list entity))
          element))

(defn table-DOM-R
  "Return a hiccup representation of DOM, with the given internal key,
  describing a table."
  ;; The following elements of item describe the table:
  ;;  :row-query  The content is an item whose list form gives the
  ;;              requirements for an item to appear as a row. When the
  ;;              query is created, an extra [:top-level
  ;;              :non-semantic] element is added, to keep the query,
  ;;              which is also in the database, from matching itself.
  ;;     :column  The semantics gives the requirements for an element
  ;;              of a row to appear in this column. The :column
  ;;              element has, itself, a :non-semantic element, to
  ;;              make it not part of the semantics of the column
  ;;              specifier. Generally, the content of the content
  ;;              will be the keyword :none, to indicate no constraint
  ;;              on the content of an element in the row, without
  ;;              breaking the rule that the database doesn't contain
  ;;              nil. The exception is the special content :other,
  ;;              which means to show everything not shown in any
  ;;              other column.
  ;; TODO: Make there there be an element on a table descriptor that
  ;;       says what elements of new columns must have, rather than
  ;;       the current '(nil :tag)
  ;; TODO: Add the "other" column if a table requests it.
  [table-item inherited]
  (println "Generating DOM for table" (simplify-for-print table-item))
  (assert (satisfies? entity/StoredEntity table-item))
  (let [store (:store table-item)
        table-key (conj (:parent-key inherited) (:item-id table-item))
        inherited (assoc inherited :parent-key table-key)]
    (expr-let [row-query-item (entity/label->content table-item :row-query)]
      ;; Don't do anything if we don't yet have the table information filled in.
      (when row-query-item
        (expr-let
            [basic-row-query (semantic-to-list-R row-query-item)
             row-query (add-element-to-entity-list
                        (replace-in-seqs basic-row-query :none nil)
                        ['(:top-level :non-semantic)])
             row-items (expr order-items-R
                         (matching-items row-query store))
             columns (expr order-items-R
                       (entity/label->elements table-item :column))
             ;; Unlike row headers for tags, where the header
             ;; information is computed from the items of the elements,
             ;; here the header information is explicitly provided by
             ;; the table definition. So the members of the hierarchy
             ;; are the column definitions.
             columns-elements (expr-seq map semantic-elements-R columns)
             columns-lists (expr-seq map #(expr-seq map semantic-to-list-R %)
                                     columns-elements)

             hierarchy (hierarchy-by-canonical-info
                        (map (fn [column elements lists]
                               {:item column
                                :property-elements elements
                                :property-canonicals (map canonicalize-list
                                                          lists)})
                             columns columns-elements columns-lists))
             headers (table-header-DOM-R
                      hierarchy '(nil :tag) (query-referent row-query)
                      (assoc inherited
                             :subject (item-referent table-item)
                             :template '(:none (:column :non-semantic))))]
          (let [column-descriptions (mapcat
                                     table-hierarchy-node-column-descriptions
                                     hierarchy)
                rows (expr-seq map #(table-row-DOM-component
                                     % row-query column-descriptions inherited)
                               row-items)]
            (into [:div {:class "table"}
                   headers]
                  rows)))))))

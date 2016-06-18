(ns cosheet.server.render
  (:require [clojure.walk :refer [postwalk]]
            (cosheet [entity :as entity]
                     [query :refer [matching-elements matching-items]]
                     [utils :refer [multiset multiset-diff multiset-union
                                    multiset-to-generating-values update-last]]
                     [debug :refer [simplify-for-print current-value]]
                     [orderable :as orderable]
                     [dom-utils
                      :refer [into-attributes dom-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq cache]])
            (cosheet.server
             [referent :refer [item-referent
                               elements-referent query-referent
                               union-referent difference-referent
                               item-or-exemplar-referent
                               semantic-elements-R semantic-to-list-R
                               canonicalize-list immutable-semantic-to-list]]
             [hierarchy :refer [canonical-info canonical-set-to-list
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
               ; :item-referent       Item(s) referred to
               ; :subject-referent    Subject(s) of the virtual item(s)
               ; :adjacents-referent  Groups of item(s) adjacent to new item(s)
               ; :position            :before or :after item/adjacent
               ; :template            Added item(s) should satisfy this.
               ; :parent-key          The key of the parent of a virtual
               ;                      new item.
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
;;; dom node that the user might interact with. There must never be
;;; two doms with the same key, even during updates, or all sorts of
;;; confusion can result. The key of a component must also not change
;;; throughout the life of its parent dom, because we keep a mapping
;;; between client dom ids and server keys, which will be broken if
;;; the key changes.

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
;       :subject-referent  ; The referent of the subject(s) of the item
                           ; the dom is about, if any. If present, the
                           ; item is an exemplar.
;               :template  ; The template that the item for this dom,
                           ; and any of its siblings, must satisfy, if any.
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
  a vertical stack of the items."
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
  "Make a dom for a place that could hold an item, but doesn't."
  [key adjacents-referent position inherited]
  (let [template (:template inherited)]
    [:div (into-attributes
           (:selectable-attributes inherited)
           {:class "editable"
            :key key
            :commands {:set-content nil}
            :target (cond-> {:subject-referent (:subject-referent inherited)
                             :adjacents-referent adjacents-referent
                             :position position}
                      template (assoc :template template))})]))

(defn item-stack-DOM
  "Given a list of items and a matching list of elements to exclude,
  and attributes that the doms for each of the items should have,
  generate DOM for a vertical list of a component for each item."
  [items excludeds attributes inherited]
  (vertical-stack
   (map (fn [item excluded]
          (add-attributes (item-component item excluded inherited) attributes))
        items excludeds)
   :class "item-stack"))

(defn item-target
  "Return a target for the given item."
  [item inherited]
  (let [template (:template inherited)]
    (cond-> {:item-referent (item-or-exemplar-referent
                             item (:subject-referent inherited))}
      template (assoc :template template))))

(defn item-content-DOM
  "Make dom for the content part of an item."
  [item content key inherited]
  ;; We don't currently handle items as content. That would need
  ;; more interaction and UI design work to deal with the distinction
  ;; between elements of an item and elements on its content.
  (assert (entity/atom? content))
  (let [is-placeholder (and (symbol? content)
                            (= (subs (str content) 0 3) "???"))]
    [:div (into-attributes
           (:selectable-attributes inherited)
           {:class (cond-> "content-text editable"
                     is-placeholder (str " placeholder"))
            :key key
            :target (item-target item inherited)
            :commands {:set-content nil
                       :delete nil
                       :add-element nil
                       :add-sibling nil}})
     (cond (= content :none) ""
           is-placeholder "???"                     
           true (str content))]))

(defn hierarchy-add-adjacent-target
  "Given a hierarchy node, generate attributes for the target of
  a command to add an item that would be adjacent to the hierarchy node."
  [hierarchy-node inherited]
  (let [subject-referent (:subject-referent inherited)
        ancestor-props (first (multiset-diff
                               (:cumulative-properties hierarchy-node)
                               (:properties hierarchy-node)))
        conditions (concat (canonical-set-to-list ancestor-props)
                           (rest (:template inherited)))]
   (cond->
       {:subject-referent subject-referent
        :adjacents-referent (hierarchy-node-items-referent
                             hierarchy-node subject-referent)}
     (not (empty? conditions))
     (assoc :template (list* nil conditions)))))

(defn add-adjacent-row-command
  "Given a hierarchy node and inherited, update inherited to add
  a row adjacent to the node, if that would be different than just
  adding a sibling."
  [hierarchy-node inherited]
  (if (empty? (:properties hierarchy-node))
    inherited
    (update-in
     inherited [:selectable-attributes]
     #(into-attributes
       % {:commands {:add-row nil}
          :row (hierarchy-add-adjacent-target hierarchy-node inherited)}))))

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
      (let [subject (:subject-referent inherited)
            adjacent-item (:item (first (hierarchy-node-descendants
                                         hierarchy-node)))
            referent (item-or-exemplar-referent adjacent-item subject)
            example-elements (hierarchy-node-example-elements hierarchy-node)
            key (conj (:parent-key inherited)
                      :example-element
                      (:item-id (first example-elements)))]
        (virtual-item-DOM key referent :before inherited-down))
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
                        hierarchy-node (:subject-referent inherited))
        example-descendant (first (hierarchy-node-descendants
                                   hierarchy-node))
        tags-parent-key (conj (:parent-key inherited)
                              (:item-id (:item example-descendant))
                              :outside)
        inherited-for-tags (assoc inherited
                                  :parent-key tags-parent-key
                                  :template '(nil :tag)
                                  :subject-referent items-referent)]
    (expr-let
        [dom (if (empty? (:properties hierarchy-node))
               (virtual-item-DOM (conj tags-parent-key :tags) items-referent
                                 :after inherited-for-tags)
               (hierarchy-properties-DOM-R
                hierarchy-node {:class "tag"} inherited-for-tags))]
        ;; Even if stacked, we need to mark the stack as "tag" too.
        (add-attributes dom {:class "tag"}))))

(def tagged-items-one-column-subtree-DOM-R)

(defn tagged-items-one-column-descendants-DOM-R
   "Return DOM for all descendants of the hierarchy node, as a single column."
  [hierarchy-node inherited]
  (let [members (hierarchy-node-members hierarchy-node)
        nested-inherited (add-adjacent-row-command hierarchy-node inherited)
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

(def tagged-items-one-column-subtree-DOM-R)

;;; Wrappers are used when one logical cell is broken into several
;;; adjacent divs. This typically means that the first and last of the
;;; divs need to be styled differently from the others. A wrapper will
;;; be called with the div to wrap and with whether it is the first
;;; and/or last div of the logical cell.

(defn nest-wrapper
  "Return a wrapper that nests one level more deeply. This is used
  when a logical cell has been broken into several pieces and one of those
  pieces has been further subdivided.
  We are given an outer wrapper and with the position of our sub-part in
  the logical cell. We  may get further subdivided. So we return a new wrapper
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
                         (add-adjacent-row-command hierarchy-node inherited)
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
        (item-stack-DOM  elements excludeds {} inherited)
        (expr-let [item-maps (item-maps-by-elements ordered-elements labels)
                   augmented (map (fn [item-map excluded]
                                    (assoc item-map :exclude-elements excluded))
                                  item-maps excludeds)
                   hierarchy (hierarchy-by-canonical-info augmented #{})]
          (if (or (< (:width inherited) 1.0) no-labels)
            (tagged-items-one-column-DOM-R hierarchy inherited)
            (tagged-items-two-column-DOM-R hierarchy inherited)))))))

(defn item-DOM-R
  "Make a dom for an item."
  [item excluded-elements inherited]
  (println "Generating DOM for"
           (simplify-for-print (conj (:parent-key inherited) (:item-id item))))
  (expr-let [content (entity/content item)
             elements (semantic-elements-R item)]
    (let [key (conj (:parent-key inherited) (:item-id item))
          referent (item-or-exemplar-referent item
                                              (:subject-referent inherited))
          elements (remove (set excluded-elements) elements)]
      (if (empty? elements)
        (add-attributes (item-content-DOM item content key inherited)
                        {:class "item"})
        (let [content-key (conj key :content) ;; Has to be different.
              content-dom (item-content-DOM
                           item content content-key inherited)
              inherited-down (-> inherited
                                 (update-in [:priority] inc)
                                 (assoc :parent-key key
                                        :subject-referent referent)
                                 (dissoc :template :selectable-attributes))]
          (expr-let [elements-dom (elements-DOM-R
                                   elements true inherited-down)]
            [:div {:class "item with-elements" :key key}
             content-dom elements-dom]))))))

;;; Tables

(def base-table-column-width 150)

(defn table-header-referent
  "Generate the referent for all items affected by a table
  header. This means all table request items at or below this header,
  and, for each row, the elements that the header brings up.
  The arguments are the info-maps of all the header requests it
  applies to, info maps that cover the conditions it applies to in
  rows, and info maps that cover conditions it must not apply to, as
  well as a referent to the rows of the table."
  [info-maps extent-info-maps negative-info-maps rows-referent inherited]
  (let [make-elements-ref #(elements-referent (:item %) rows-referent)
        positives (union-referent
                   (vec (concat
                         ;; The header requests the header applies to.
                         (map #(item-or-exemplar-referent
                                (:item %) (:subject-referent inherited))
                              info-maps)
                         ;; The row elements the header applies to.
                         (map make-elements-ref extent-info-maps))))]
    (if (empty? negative-info-maps)
      positives
      (let [negatives (union-referent
                       (vec (map make-elements-ref negative-info-maps)))]
        difference-referent positives negatives))))

(defn add-column-command-attributes
  "Return attributes for add a column command, given the item that
  requests the column. element-template gives the template of new
  elements, while inherited describes the header, but not its affect
  on the overall column."
  [column-item element-template inherited]
  (let [template (:template inherited) 
        new-element-template (cons '??? (rest element-template))
        new-column-template (cons (first template)
                                  (cons new-element-template) (rest template))
        ;; There is an item for the new column, which has an element
        ;; satisfying the new element template. We want to select that
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
                            (~element-template :condition)
                            (true :reference))
        select-pattern (conj (:parent-key inherited)
                             [:pattern `(nil ~element-variable)])]
    {:commands {:add-column {:select-pattern select-pattern}}
     :column {:adjacents-referent (item-or-exemplar-referent
                                   column-item (:subject inherited))
              :subject-referent (:subject inherited)
              :position :after
              :template new-column-template}}))

(defn table-header-node-elements-DOM
  "Generate the dom for one node of a table header hierarchy given its
  elements. The elements-condition gives the condition that all the
  elements in the node satisfy. The header-only-subject gives the
  subject for just the header definition(s) of this column, and not
  the elements in rows. inherited, on the other hand, gives the
  context for the header and its entire column."
  [column-item elements width rightmost
   element-template header-only-subject delete-key inherited]
  (expr-let
      [ordered-elements (order-items-R elements)
       element-lists (expr-seq map semantic-to-list-R ordered-elements)]
    (let [element-subject (item-or-exemplar-referent
                           column-item (:subject inherited))
          inherited-down (-> (if delete-key
                               (assoc inherited :selectable-attributes
                                      {:commands {:delete {:delete-key
                                                           delete-key}}})
                               (dissoc inherited :selectable-attributes))
                             (assoc :subject element-subject
                                    :template element-template))
          dom (if (empty? ordered-elements)
                (virtual-item-DOM
                 (conj (:parent-key inherited) (:item-id column-item))
                 (item-or-exemplar-referent column-item header-only-subject)
                 :after inherited-down)
                (vertical-stack
                 (map-indexed
                  (fn [position element]
                    ;; Make the new column condition also require the
                    ;; elements above this one in the stack for this node.
                    (let [column-condition
                          (list* (concat (:template inherited)
                                         (take position element-lists)))] 
                      (add-attributes
                       (item-component element nil inherited-down)
                       (add-column-command-attributes
                        column-item element-template
                        (into inherited {:subject header-only-subject
                                         :template column-condition}))))))
                 :class "item-stack"))]
      (cond-> (let [dom (add-attributes dom {:class "column-header"})]
                [:div {:class "column-header-container" 
                       :style {:width (str width "px")}} dom])
        rightmost (add-attributes {:class "rightmost"})
        (= element-template '(nil :tag)) (add-attributes {:class "tags"})))))

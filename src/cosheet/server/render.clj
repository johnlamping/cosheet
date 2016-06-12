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
                               semantic-elements-R
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
  {         :narrow false  ; If true, the item must fit in a narrow space.
              :priority 0  ; How important it is to render this item earlier.
                           ; (Lower is more important.)
           :parent-key []  ; The key of the parent dom of the dom.
;       :subject-referent  ; The referent of the subject of the item
                           ; the dom is about, if any.
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
       [:div {:class "editable"
              :key key
              :commands {:set-content nil}
              :target (cond-> {:subject-referent (:subject-referent inherited)
                               :adjacents-referent adjacents-referent
                               :position position}
                        template (assoc :template template))}]))

;; TODO: Need test to cover a non-trivial stack.
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
        parent-key (:parent-key inherited)
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
     (assoc :template (list* nil conditions))
     parent-key
     (assoc :parent-key parent-key))))

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
  that give rise to the properties of the node. Inherited describes
  the context of the items."
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
    (if (empty? (:properties hierarchy-node))
      (add-attributes
          (virtual-item-DOM (conj tags-parent-key :tags) items-referent
                            :after inherited-for-tags)
          
          {:class "tag"})
      (hierarchy-properties-DOM-R
       hierarchy-node {:class "tag"} inherited-for-tags))))

(def tagged-items-one-column-subtree-DOM-R)

(defn tagged-items-one-column-descendants-DOM-R
   "Return DOM for all descendants of the hierarchy node, as a single column."
  [hierarchy-node inherited]
  (let [members (hierarchy-node-members hierarchy-node)
        nested-inherited (if (empty? (:properties hierarchy-node))
                           inherited
                           (update-in
                            inherited [:selectable-attributes]
                            #(into-attributes
                              % {:commands {:add-row nil}
                                 :row (hierarchy-add-adjacent-target
                                       hierarchy-node inherited)})))
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
    (vertical-stack doms)))(def tagged-items-one-column-subtree-DOM-R)

;;; Wrappers are used when one logical cell is broken into several
;;; adjacent divs. This typically means that the first and last of the
;;; divs need to be styled differently from the others. A wrapper will
;;; be called with the div to wrap and with whether it is the first
;;; and/or last div of the logical cell.

(defn nest-wrapper
  "Return a wrapper that nests one level more deeply. This is used
  when a logical has been broken into several pieces and one of those
  pieces has been further subdivided.
  We are called with a wrapper and with the position of our group of
  pieces for that wrapper. Return a new wrapper that itself expects to
  be called several times, calling the outer wrapper each time, and
  passing it the appropriate position information. The inner wrapper
  will call position->class with whether it is first among the inner
  wrappers ultimate subdivision and whether it is last, and will use
  the result as the class of the wrapper."
  [outer-wrapper position->class generated-first generated-last]
  (fn [body called-first called-last]
    (outer-wrapper [:div {:class (position->class first last)} body]
                   (and generated-first called-first)
                   (and generated-last called-last))))

(defn identity-wrapper
  "Return a wrapper that does no wrapping -- just returns the body."
  [body called-first called-last]
  body)

(defn horizontal-tag-wrapper-class
  [first last]
  (cond-> "tag horizontal-header"
    first (str " top-border")
    (not first) (str " indent")
    last (str " bottom-border")))

(def tagged-items-two-column-subtree-DOM-R)

(defn tagged-items-two-column-children-DOM-R
  "Return a seq of doms for the children of the hierarchy node,
  as two columns."
  [hierarchy-node header-wrapper inherited]
  (let [children (:children hierarchy-node)]
    (when children
      (let [wrapper (nest-wrapper header-wrapper
                                  horizontal-tag-wrapper-class false false)
            last-wrapper (nest-wrapper header-wrapper
                                       horizontal-tag-wrapper-class false true)
            wrappers (concat (map (constantly wrapper) (butlast children))
                             [last-wrapper])]
        (map (fn [child-node wrapper]
               (tagged-items-two-column-subtree-DOM-R
                child-node wrapper inherited))
             children wrappers)))))

(defn tagged-items-two-column-subtree-DOM-R
  "Return a sequence of DOMs for the given hierarchy node and its descendants,
  as two columns."
  [hierarchy-node header-wrapper inherited]
  (let [members (hierarchy-node-members hierarchy-node)
        children (:children hierarchy-node)
        ;; TODO: Factor this out of here and the one column guy.
        nested-inherited (if (empty? (:properties hierarchy-node))
                           inherited
                           (update-in
                            inherited [:selectable-attributes]
                            #(into-attributes
                              % {:commands {:add-row nil}
                                 :row (hierarchy-add-adjacent-target
                                       hierarchy-node inherited)})))
        members-dom (hierarchy-members-DOM hierarchy-node nested-inherited)]
    (expr-let [properties-dom (tagged-items-properties-DOM-R
                               hierarchy-node inherited)]
      (cons [:div {:class "horizontal-tags-element wide"}
             ((nest-wrapper header-wrapper
                            horizontal-tag-wrapper-class true (empty? children))
                properties-dom true (empty? children))
             members-dom]
            (tagged-items-two-column-children-DOM-R
             hierarchy-node inherited)))))

(defn tagged-items-two-column-DOM-R
  [hierarchy inherited]
  (expr-let [doms (expr-seq map #(tagged-items-one-column-subtree-DOM-R
                                  % identity-wrapper inherited)
                            hierarchy)]
    (vertical-stack doms)))

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
          (if (or (:narrow inherited) no-labels)
            (tagged-items-one-column-DOM-R hierarchy inherited)
            (tagged-items-two-column-DOM-R hierarchy inherited)))))))

(defn item-DOM-R
  "Make a dom for an item."
  [item excluded-elements inherited]
  (println "Generating DOM for" (simplify-for-print item))
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
                                 (dissoc :template :selectable-attribute))]
          (expr-let [elements-dom (elements-DOM-R
                                   elements true inherited-down)]
            [:div {:class "item with-elements" :key key}
             content-dom elements-dom]))))))

 

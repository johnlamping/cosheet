(ns cosheet.server.item-render
  (:require (cosheet [canonical :refer [canonical-set-to-list]]
                     [entity :as entity]
                     [query :refer [matching-elements]]
                     [utils :refer [multiset-diff assoc-if-non-empty
                                    map-with-first-last
                                    add-elements-to-entity-list]]
                     [debug :refer [simplify-for-print]]
                     [hiccup-utils
                      :refer [into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq expr-filter]])
            (cosheet.server
             [model-utils :refer [visible-elements-R visible-item?-R
                                  split-out-labels-R
                                  visible-non-labels-R visible-labels-R]]
             [referent :refer [item-referent virtual-referent ]]
             [hierarchy :refer [replace-hierarchy-leaves-by-nodes
                                hierarchy-node-descendants
                                hierarchy-node-leaves
                                hierarchy-by-canonical-info
                                item-maps-by-elements-R
                                hierarchy-node-example-elements]]
             [order-utils :refer [order-items-R]]
             [render-utils :refer [virtual-element-DOM item-stack-DOM-R
                                   nest-if-multiple-DOM condition-satisfiers-R
                                   transform-inherited-for-children
                                   transform-inherited-for-labels
                                   transform-inherited-attributes
                                   remove-inherited-for-item
                                   add-inherited-attribute
                                   remove-inherited-attribute
                                   inherited-attributes
                                   content-attributes
                                   subject-referent-given-inherited
                                   item-referent-given-inherited
                                   hierarchy-node-items-referents
                                   hierarchy-node-items-referent
                                   hierarchy-node-DOM-R]])))

(declare item-without-labels-DOM-R)

(defn hierarchy-adjacent-virtual-target
  "Given a hierarchy node, generate attributes for the target of
  a command to add an item that would be adjacent to the hierarchy node."
  [hierarchy-node inherited]
  (let [ancestor-props (first (multiset-diff
                               (:cumulative-properties hierarchy-node)
                               (:properties hierarchy-node)))
        conditions (concat (canonical-set-to-list ancestor-props)
                           (rest (add-elements-to-entity-list
                                  (:template inherited) nil)))]
    {:referent (virtual-referent
                (list* 'anything conditions)
                (subject-referent-given-inherited inherited)
                (hierarchy-node-items-referents
                 hierarchy-node inherited))
     :select-pattern (conj (:key-prefix inherited) [:pattern])}))

(defn add-adjacent-sibling-command
  "Given inherited, and a node from a hierarchy over elements, update
  inherited to have a command to add an adjacent sibling element."
  [inherited hierarchy-node]
  (-> inherited
      (remove-inherited-attribute :add-sibling)
      (add-inherited-attribute
       [#{:label :optional} #{:content}
        {:add-sibling (hierarchy-adjacent-virtual-target
                       hierarchy-node inherited)}])))

(defn hierarchy-leaf-items-DOM-R
  "Given a hierarchy node with tags as the properties, generate DOM
  for leaves that are items. The leaves of the node may contain an additional
  :exclude-elements field that gives more of the item's elements not to
  show, typically the ones that satisfy the :template of inherited."
  [hierarchy-node inherited]
  (let [leaves (hierarchy-node-leaves hierarchy-node)
        property-list (canonical-set-to-list
                       (:cumulative-properties hierarchy-node))
        inherited-down (if (not (empty? property-list))
                         (assoc inherited :template
                                (add-elements-to-entity-list
                                 (or (:template inherited) 'anything)
                                 property-list))
                         inherited)]
    (if (empty? leaves)
      (let [adjacent-item (:item (first (hierarchy-node-descendants
                                         hierarchy-node)))
            adjacent-referent (item-referent-given-inherited
                               adjacent-item inherited)
            example-elements (hierarchy-node-example-elements hierarchy-node)]
        (virtual-element-DOM
         adjacent-referent :before
         (-> inherited-down
             (update :key-prefix
                     #(conj % :example-element
                            (:item-id (first example-elements))))
             (assoc :select-pattern
                    (conj (:key-prefix inherited) [:pattern])))))
      (let [items (map :item leaves)
            excludeds (map #(concat (:property-elements %)
                                    (:exclude-elements %))
                           leaves)]
        (item-stack-DOM-R
         item-without-labels-DOM-R items excludeds :vertical
         inherited-down)))))

(defn opposite-direction
  [direction]
  (case direction
    :horizontal :vertical
    :vertical :horizontal))

(defn add-labels-DOM
  "Add label dom to an inner dom. Direction gives the direction of the label
  with respect to the inner dom. It can also be :vertical-wrapped, which puts
  it above the inner dom, but with an indentation on the left too."
  [labels-dom inner-dom direction]
  (if (= direction :vertical-wrapped)
    [:div {:class "wrapped-element tag"}
     labels-dom
     [:div {:class "indent-wrapper"} inner-dom]]
    [:div {:class (case direction
                    :vertical "vertical-tags-element"
                    :horizontal "horizontal-tags-element tag")}
     labels-dom inner-dom]))

(defn wrap-with-labels-DOM
  "Direction gives the direction of the label with respect to the inner dom.
  :vertical is interpreted as :vertical-wrapped."
  [labels-dom inner-dom direction]
  (add-labels-DOM labels-dom inner-dom (if (= direction :vertical)
                                         :vertical-wrapped direction)))

(defn virtual-label-DOM
  "Return a dom for a virtual label"
  [inherited]
  (add-attributes
   (virtual-element-DOM nil :after
                        (-> inherited
                            transform-inherited-for-labels
                            (update :key-prefix #(conj % :tags))))
   {:class "tag"}))

(defn label-stack-DOM-R
  "Given a non-empty list of label elements, return a stack of their doms.
   Inherited should be halfway transformed to children."
  [label-elements inherited]
  (expr-let [ordered-labels (order-items-R label-elements)
             tags (expr-seq map #(condition-satisfiers-R % '(nil :tag))
                            ordered-labels)]
    (item-stack-DOM-R item-without-labels-DOM-R
                      ordered-labels tags :vertical
                      (-> inherited
                          transform-inherited-for-labels
                          (add-inherited-attribute {:class "tag"})))))

(defn non-empty-labels-wrapper-DOM-R
  "Given a dom for an item, not including its labels, and a non-empty 
  list of labels, make a dom that includes the labels wrapping the item.
  Inherited should be half way to the children."
  [inner-dom label-elements direction inherited]
  (expr-let [stack (label-stack-DOM-R label-elements inherited)]
    (wrap-with-labels-DOM stack inner-dom direction)))

(defn labels-wrapper-DOM-R
  "Given a dom for an item, not including its labels, and a list of labels,
  make a dom that includes any necessary labels wrapping the item.
  inherited should be half way to the children." 
  [dom label-elements must-show-label inherited]
  (if (not (empty? label-elements))
    (non-empty-labels-wrapper-DOM-R dom label-elements :vertical inherited)
    (if (not must-show-label)
      dom
      [:div {:class "horizontal-tags-element tag virtual-wrapper narrow"}
       (virtual-label-DOM inherited)
       dom])))

(defn virtual-element-with-label-DOM
  "Return the dom for a virtual element of an item."
  [virtual-content direction inherited]
  (let [dom (virtual-element-DOM nil :after inherited)
        template (if (:template inherited)
                   (cons virtual-content (rest (:template inherited)))
                   virtual-content)
        virtual-ref (virtual-referent
                     template
                     (subject-referent-given-inherited inherited))
        inherited-for-label (-> inherited
                                (assoc :template '(anything :tag)
                                       :subject-referent virtual-ref))
        labels-dom (add-attributes
                    (virtual-element-DOM
                     nil :after
                     (update inherited-for-label :key-prefix #(conj % :label)))
                    {:class "tag"})]
    (add-labels-DOM labels-dom dom direction)))

(defn tagged-items-properties-DOM-R
  "Given a hierarchy node for tags, Return DOM for example elements
  that give rise to the properties of the node.
  Inherited describes the overall context of the node."
  [hierarchy-node inherited]
  (let [items-referent (hierarchy-node-items-referent
                        hierarchy-node inherited)
        example-descendant (first (hierarchy-node-descendants
                                   hierarchy-node))
        tags-key-prefix (conj (:key-prefix inherited) :label)
        inherited-for-children (-> inherited
                                   (add-adjacent-sibling-command hierarchy-node)
                                   (transform-inherited-for-children
                                    tags-key-prefix items-referent))]
    (expr-let
        [dom (if (empty? (:properties hierarchy-node))
               (virtual-element-DOM
                nil :after
                (->
                 (transform-inherited-for-labels inherited-for-children)
                 (update :key-prefix
                         ;; Need to make it different from sibling virtuals.
                         #(conj % (:item-id (:item example-descendant))))
                 (assoc :select-pattern (conj tags-key-prefix [:pattern]))))
               (label-stack-DOM-R
                (hierarchy-node-example-elements hierarchy-node)
                inherited-for-children))]
        ;; Even if stacked, we need to mark the stack as "tag" too.
        (add-attributes dom {:class "tag"}))))

(defn tagged-items-whole-hierarchy-node-DOM-R
  "Return the dom for all of a tagged items hierarchy node.
  direction gives which way to lay out the contained items."
  [node child-doms [must-show-labels direction] inherited]
  (assert (#{:horizontal :vertical} direction))
  (let [leaves (hierarchy-node-leaves node)
        only-item (when (and (empty? child-doms) (= (count leaves) 1))
                    (:item (first leaves)))
        ;; If there is only one item, we put any item specific attributes
        ;; on the overall item including labels, while if there are several
        ;; items, we can only put item specific attributes on each item.
        inherited-for-leaves (cond-> (add-adjacent-sibling-command
                                      inherited node)
                               only-item
                               (remove-inherited-for-item only-item))]
    (expr-let [leaf-dom (when (seq leaves)
                          (hierarchy-leaf-items-DOM-R
                           node inherited-for-leaves))
               properties-dom (when (or (seq (:properties node))
                                        must-show-labels)
                                (tagged-items-properties-DOM-R
                                 node inherited))]
      (let [descendants-dom (nest-if-multiple-DOM
                          (if leaf-dom
                            (cons leaf-dom child-doms)
                            child-doms)
                          direction)]
        (cond-> (if (empty? (:properties node))
                  (if must-show-labels
                    (cond-> (add-labels-DOM properties-dom descendants-dom
                                            (opposite-direction direction))
                      true
                      (add-attributes {:class "virtual-wrapper"})
                      (= direction :vertical)
                      (add-attributes {:class "narrow"}))
                    descendants-dom)
                  (add-labels-DOM properties-dom descendants-dom
                                  (case direction
                                    :vertical :vertical-wrapped
                                    :horizontal :vertical)))
          only-item
          (add-attributes (inherited-attributes inherited only-item)))))))

(defn tagged-items-for-horizontal-DOMs-R
  [hierarchy inherited]
  (expr-seq map #(hierarchy-node-DOM-R
                  % tagged-items-whole-hierarchy-node-DOM-R
                  (fn [node _ inherited] [[false :horizontal] inherited])
                  [true :horizontal] inherited)
            hierarchy))

(defn tagged-items-for-one-column-DOMs-R
  [hierarchy inherited]
  (expr-seq map #(hierarchy-node-DOM-R
                  % tagged-items-whole-hierarchy-node-DOM-R
                  (fn [node _ inherited] [[false :vertical] inherited])
                  [true :vertical] inherited)
            hierarchy))

(defn horizontal-tag-wrapper
  "Return a modifier for a horizontal tag dom that is logically part of
  a possibly larger entity."
  [body is-first is-last]
  [:div {:class (cond-> "tag horizontal-header"
                  is-first (str " top-border")
                  (not is-first) (str " indent")
                  is-last (str " bottom-border"))}
   body])

(defn horizontal-value-wrapper
  "Return a modifier for a value in a horizontal tag layout that is 
  logically part of a larger entity."
  [body is-first is-last]
  (if (and is-last (not is-first))
    [:div {:class "horizontal-value-last"} body]
    body))

(defn tagged-items-two-column-items-DOMs-R
  "Return the item doms for the node and all its children."
  [node child-doms inherited]
  (let [inherited-for-items (update
                             (add-adjacent-sibling-command inherited node)
                             :width #(* % 0.6875))]
    (expr-let [leaves-dom (hierarchy-leaf-items-DOM-R node inherited-for-items)]
      (map-with-first-last
       horizontal-value-wrapper
       (cons leaves-dom (apply concat child-doms))))))

(defn tagged-items-two-column-label-DOMs-R
  "Return the label doms for the node and all its children."
  [node child-doms inherited]
  (expr-let [properties-dom (tagged-items-properties-DOM-R
                             node inherited)]
    (map-with-first-last
     horizontal-tag-wrapper
     (cons properties-dom (apply concat child-doms)))))

(defn tagged-items-for-two-column-DOMs-R
  [hierarchy inherited]
  (let [inherited-for-tags (-> inherited
                               (transform-inherited-attributes :label)
                               (update :width #(* % 0.25)))
        ;; If there is only one item below a top level node,
        ;; we put any item specific attributes on the overall node,
        ;; including labels, while if there are several
        ;; items, we can only put item specific attributes on each item.
        only-items (map #(let [leaves (hierarchy-node-leaves %)]
                           (when (and (empty? (:child-nodes %))
                                      (= (count leaves) 1))
                             (:item (first leaves))))
                        hierarchy)]
    (expr-let [label-doms (expr-seq
                           map #(hierarchy-node-DOM-R
                                 % tagged-items-two-column-label-DOMs-R
                                 inherited-for-tags)
                           hierarchy)
               items-doms (expr-seq
                           map (fn [node only-item]
                                 (hierarchy-node-DOM-R
                                   node tagged-items-two-column-items-DOMs-R
                                   (cond-> inherited
                                     only-item
                                     (remove-inherited-for-item only-item))))
                           hierarchy only-items)]
      (map
       (fn [label-dom items-dom only-item]
         (cond-> [:div {:class "horizontal-tags-element tag wide"}
                  label-dom items-dom]
           only-item
           (add-attributes (inherited-attributes inherited only-item))))
       (apply concat label-doms)
       (apply concat items-doms)
       (apply concat (map (fn [doms only-item]
                            (map (constantly only-item) doms))
                          items-doms only-items))))))

(defn element-DOMs-R
  "Make a dom for a stack of elements.
   If implied-template is non-nil, don't show sub-elements implied by it.
   If must-show-labels is true, show a space for labels, even if
   there are none. If, additionally, it is :wide, show them with substantial
   space, if there is any space available."
  [elements must-show-labels implied-template direction inherited]
  (expr-let
      [ordered-elements (order-items-R elements)
       all-labels (expr-seq map visible-labels-R ordered-elements)
       excludeds (expr-seq map #(when implied-template
                                  (condition-satisfiers-R % implied-template))
                           ordered-elements)]
    (let [labels (map (fn [all minus]
                        (seq (clojure.set/difference (set all) (set minus))))
                      all-labels excludeds)
          no-labels (every? empty? labels)]
      (if (and no-labels (not must-show-labels))
        (expr-let [stack (item-stack-DOM-R
                          item-without-labels-DOM-R ordered-elements
                          excludeds direction inherited)]
          [stack])
        (expr-let [item-maps (item-maps-by-elements-R ordered-elements labels)
                   augmented (map (fn [item-map excluded]
                                    (assoc item-map :exclude-elements excluded))
                                  item-maps excludeds)
                   hierarchy (hierarchy-by-canonical-info augmented)]
          (case direction
            :vertical
            (if (or (< (:width inherited) 1.0)
                    (and no-labels (not (= must-show-labels :wide))))
              (tagged-items-for-one-column-DOMs-R hierarchy inherited)
              (tagged-items-for-two-column-DOMs-R hierarchy inherited))
            :horizontal
            (tagged-items-for-horizontal-DOMs-R
             (replace-hierarchy-leaves-by-nodes hierarchy) inherited)))))))

(defn elements-DOM-R
  "Make doms for elements.
   If implied-template is non-nil, don't show sub-elements implied by it.
   If must-show-labels is true, show a space for labels, even if
   there are none. If, additionally, it is :wide, show them with substantial
   space, if there is any space available."
  [elements must-show-labels implied-template direction inherited]
  (expr-let [doms (element-DOMs-R elements must-show-labels
                                  implied-template direction inherited)]
    (nest-if-multiple-DOM doms direction)))

(defn labels-and-elements-DOM-R
  "Generate the dom for a set of elements, some of which may be labels.
  virtual-dom, if present, will appear after the elements.
  elements-must-show-labels determines whether the elements must show labels.
  inherited must be half way to the children."
  [elements virtual-dom must-show-label elements-must-show-labels
   direction inherited]
  (expr-let [[labels non-labels] (split-out-labels-R elements)
             elements-dom
               (when (or non-labels virtual-dom)
                 (expr-let
                     [element-doms
                      (when non-labels
                        (element-DOMs-R
                         non-labels elements-must-show-labels nil direction
                         (transform-inherited-attributes
                          inherited :element)))]
                   [:div {:class "item elements-wrapper"}
                    (nest-if-multiple-DOM
                     (cond-> element-doms
                       virtual-dom (concat [virtual-dom]))
                     direction)]))]
    (cond
      (and labels elements-dom)
      (non-empty-labels-wrapper-DOM-R elements-dom labels direction
                                      inherited)
      labels
      (label-stack-DOM-R elements inherited)
      (and must-show-label elements-dom)
      (wrap-with-labels-DOM
       (virtual-label-DOM inherited) elements-dom direction)
      elements-dom
      elements-dom
      true
      (add-attributes (virtual-label-DOM inherited)
                      {:class "elements-wrapper"}))))

(defn item-content-DOM
  "Make dom for the content part of an item."
  [item-referent content inherited]
  ;; We don't currently handle items as content. That would need
  ;; more interaction and UI design work to deal with the distinction
  ;; between elements of an item and elements on its content.
  (assert (entity/atom? content))
  (let [anything (#{'anything 'anything-immutable} content)]
    [:div (into-attributes
           (content-attributes inherited)
           {:class (cond-> "content-text editable"
                     (= content 'anything-immutable) (str " immutable"))
            :target (assoc (select-keys inherited [:template])
                            :referent item-referent)})
     (if anything "\u00A0..." (str content))]))

(defn item-content-and-non-label-elements-DOM-R
  "Make a dom for a content and a group of non-label elements,
  all of the same item. Inherited should be half way to the children."
  [content elements inherited]
  (let [content-dom (add-attributes
                     (item-content-DOM
                      (:subject-referent inherited) content inherited)
                     ;; Give it a unique key.
                     ;; This will be overridden to the item's key
                     ;; if the item has nothing but this content.
                     {:key (conj (:key-prefix inherited) :content)})]
    (if (empty? elements)
      (add-attributes content-dom {:class "item"})
      (let [inherited-down
            (-> (transform-inherited-attributes inherited :element)
                (update :priority inc)
                (assoc :template '(anything)))]
        (expr-let [elements-dom (elements-DOM-R
                                 elements true nil :vertical inherited-down)]
          [:div {:class "item with-elements"}
           content-dom elements-dom])))))

(defn item-content-and-elements-DOM-R
  "Make a dom for a content and a group of elements, all of the same item.
  Inherited should be half way to the children."
  [content elements must-show-label inherited]
  (expr-let [[labels non-labels] (split-out-labels-R elements)
             content-and-elements-dom
             (item-content-and-non-label-elements-DOM-R
              content non-labels inherited)]
    (labels-wrapper-DOM-R
     content-and-elements-dom labels must-show-label inherited)))

(defn item-without-labels-DOM-R
  "Make a dom for an item or exemplar for a group of items,
   given that any of its labels are in excluded-elements.
   Inherited must contain :subject-referent.
   We only record the key on the content, not the whole item
   (unless it is just the content)."
  ([item excluded-elements inherited]
   (let [referent (item-referent-given-inherited item inherited)]
     (item-without-labels-DOM-R item referent excluded-elements inherited)))
  ([item referent excluded-elements inherited]
   (println
    "Generating DOM for"
    (simplify-for-print (conj (:key-prefix inherited) (:item-id item))))
   (let [inherited-down
         (transform-inherited-for-children
          inherited
          (conj (:key-prefix inherited) (:item-id item)) referent)]
     (expr-let [content (entity/content item)
                elements (visible-elements-R item)
                dom (item-content-and-non-label-elements-DOM-R
                     content (remove (set excluded-elements) elements)
                     inherited-down)]
       (add-attributes
        dom
        (inherited-attributes inherited item))))))

(defn item-DOM-R
  "Make a dom for an item or an exemplar of a group of items.
   If the item is a tag, the caller is responsible for tag formatting."
  [item excluded-elements inherited
   & {:keys [referent do-not-show-content must-show-label]}]
  (println
   "Generating DOM for"
   (simplify-for-print (conj (:key-prefix inherited) (:item-id item))))
  (let [referent (or referent (item-referent-given-inherited item inherited))
        inherited-down
        (-> inherited
            (transform-inherited-for-children
             (conj (:key-prefix inherited) (:item-id item)) referent)
            (add-inherited-attribute
             [#{:label} #{:content} {:expand {:referent referent}}]))]
    (expr-let [content (entity/content item)
               elements (visible-elements-R item)
               dom (if do-not-show-content
                     (labels-and-elements-DOM-R
                      elements nil must-show-label true :vertical
                      inherited-down)
                     (item-content-and-elements-DOM-R
                      content (remove (set excluded-elements) elements)
                      must-show-label inherited-down))]
      (add-attributes dom (inherited-attributes inherited item)))))


;;; TODO: Move the downward inherited computation to the caller.
(defn horizontal-label-hierarchy-node-DOM
  "Generate the DOM for a node in a hierarchy that groups items by their
   labels, has at most one leaf per node and is laid our horizontally.
   Don't generate the DOM for its children."
  [node {:keys [top-level]} inherited]
  (let [example-elements (hierarchy-node-example-elements node) 
        descendants-referent (hierarchy-node-items-referent node inherited)
        item (:item (first (hierarchy-node-descendants node)))
        inherited-down (-> inherited
                           (assoc :subject-referent descendants-referent )
                           (update :key-prefix
                                   #(conj % (:item-id item))))
        content (entity/content item)
        non-labels (when item (visible-non-labels-R item))]
    (if (empty? (:properties node))
      (let [inner-dom (item-content-and-non-label-elements-DOM-R
                       content non-labels inherited-down)]
        [:div {:class (cond-> "tag wrapped-element virtual-wrapper"
                        (not top-level)
                        (str " merge-with-parent"))}
         (cond-> (virtual-element-DOM
                  descendants-referent :after
                  (-> inherited-down
                      transform-inherited-for-labels
                      (update :key-prefix #(conj % :label))
                      (assoc :select-pattern (conj (:key-prefix inherited-down)
                                                   [:pattern]))))
           true
           (add-attributes {:class "tag"})
           (not top-level)
           (add-attributes {:class "merge-with-parent"}))
         [:div {:class "indent-wrapper tag"}
          (add-attributes
           inner-dom
           {:key (:key-prefix inherited-down)
            :class "item"})]])
      (if (empty? (:child-nodes node))
        (item-content-and-elements-DOM-R
         content (concat example-elements non-labels) false inherited-down)
        (label-stack-DOM-R example-elements inherited-down)))))

(defn element-hierarchy-child-info
  "Generate the function-info and inherited for children of
   a hierarchy node of an element hierarchy.
  The function-info is a map with at least
     :top-level          If this is a top level node
  Inherited describes the column requests."
  [node function-info inherited]
  (let [children (:child-nodes node)]
    [(assoc function-info
            :top-level false)
     (-> inherited
         (update :key-prefix  #(conj % :nested))
         (update :template
                 #(add-elements-to-entity-list
                   % (canonical-set-to-list (:properties node)))))]))

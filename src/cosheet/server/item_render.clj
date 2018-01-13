(ns cosheet.server.item-render
  (:require (cosheet [canonical :refer [canonical-set-to-list]]
                     [entity :as entity]
                     [query :refer [matching-elements]]
                     [utils :refer [multiset-diff assoc-if-non-empty
                                    map-with-first-last]]
                     [debug :refer [simplify-for-print]]
                     [hiccup-utils
                      :refer [into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq expr-filter]])
            (cosheet.server
             [referent :refer [item-or-exemplar-referent
                               item-referent virtual-referent
                               semantic-elements-R semantic-element?-R ]]
             [hierarchy :refer [replace-hierarchy-leaves-by-nodes
                                hierarchy-node-descendants
                                hierarchy-node-leaves
                                hierarchy-by-canonical-info
                                item-maps-by-elements-R
                                hierarchy-node-example-elements]]
             [order-utils :refer [order-items-R]]
             [render-utils :refer [virtual-item-DOM item-stack-DOM
                                   copy-alternate-request-to-target
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
                                   hierarchy-node-parallel-items-referent
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
                           (rest (:template inherited)))]
    {:referent (virtual-referent
                (when (seq conditions) (list* nil conditions))
                (subject-referent-given-inherited inherited)
                (hierarchy-node-parallel-items-referent
                 hierarchy-node inherited)
                :selector (when (:selector-category inherited) :first-group))
     :select-pattern (conj (:key-prefix inherited) [:pattern])}))

(defn add-adjacent-sibling-command
  "Given inherited, and a node from a hierarchy over elements, update
  inherited to have a command to add an adjacent sibling element."
  [inherited hierarchy-node]
  (-> inherited
      (remove-inherited-attribute :add-sibling)
      (add-inherited-attribute
       [#{:label :optional} #{:content}
        {:add-sibling (copy-alternate-request-to-target
                       (hierarchy-adjacent-virtual-target
                        hierarchy-node inherited)
                       inherited)}])))

(defn hierarchy-leaf-items-DOM
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
                                (concat (or (:template inherited) '(nil))
                                        property-list))
                         inherited)]
    (if (empty? leaves)
      (let [adjacent-item (:item (first (hierarchy-node-descendants
                                         hierarchy-node)))
            adjacent-referent (item-referent-given-inherited
                               adjacent-item inherited)
            example-elements (hierarchy-node-example-elements hierarchy-node)
            key (conj (:key-prefix inherited)
                      :example-element
                      (:item-id (first example-elements)))]
        (virtual-item-DOM key adjacent-referent :before inherited-down))
      (let [items (map :item leaves)
            excludeds (map #(concat (:property-elements %)
                                    (:exclude-elements %))
                           leaves)]
        (item-stack-DOM
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
                    :horizontal "horizontal-tags-element")}
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
   (virtual-item-DOM (conj (:key-prefix inherited) :tags)
                     nil :after
                     (transform-inherited-for-labels inherited))
   {:class "tag"}))

(defn label-stack-DOM-R
  "Given a non-empty list of label elements, return a stack of their doms."
  [label-elements inherited]
  (expr-let [ordered-labels (order-items-R label-elements)
             tags (expr-seq map #(condition-satisfiers-R % '(nil :tag))
                            ordered-labels)]
    (item-stack-DOM item-without-labels-DOM-R
                    ordered-labels tags :vertical
                    (add-inherited-attribute inherited {:class "tag"}))))

(defn non-empty-labels-wrapper-DOM-R
  "Given a dom for an item, not including its labels, and a non-empty 
  list of labels, make a dom that includes the labels wrapping the item.
  Inherited should be half way to the children."
  [inner-dom label-elements direction inherited]
  (expr-let [stack (label-stack-DOM-R
                    label-elements (transform-inherited-for-labels inherited))]
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
      [:div {:class "horizontal-tags-element narrow"}
       (virtual-label-DOM inherited)
       dom])))

(defn virtual-element-DOM-R
  "Return the dom for a virtual element."
  [virtual-content elements must-show-label direction inherited]
  (expr-let [adjacent (when (seq elements)
                        (expr-let [ordered (order-items-R elements)]
                          (item-referent-given-inherited
                           (last ordered) inherited)))]
    (let [dom (virtual-item-DOM (conj (:key-prefix inherited) :virtual)
                                adjacent :after inherited)]
      (if must-show-label
        (let [labels-dom
              (let [selector (when (:selector-category inherited)
                               :first-group)
                    inherited-down
                    (-> inherited
                        (assoc
                         :template '(nil :tag)
                         :subject-referent
                         (let [template
                               (if (:template inherited)
                                 (cons virtual-content
                                       (rest (:template inherited)))
                                 virtual-content)]
                           (virtual-referent
                            template
                            (subject-referent-given-inherited inherited)
                            adjacent :selector selector))))]
                (add-attributes
                 (virtual-item-DOM
                  (conj (:key-prefix inherited) :virtual :label)
                  adjacent :after inherited-down)
                 {:class "tag"}))]
          (add-labels-DOM labels-dom dom direction))
        dom))))

(defn tagged-items-properties-DOM-R
  "Given a hierarchy node for tags, Return DOM for example elements
  that give rise to the properties of the node.
  Inherited describes the overall context of the node."
  [hierarchy-node inherited]
  ;; We have to use a parallel union referent here, so that the groupings
  ;; of the subject are preserved if we are part of a header.
  (let [items-referent (hierarchy-node-parallel-items-referent
                        hierarchy-node inherited)
        example-descendant (first (hierarchy-node-descendants
                                   hierarchy-node))
        tags-key-prefix (conj (:key-prefix inherited) :label)
        inherited-for-tags (-> inherited
                               (transform-inherited-attributes :label)
                               (add-adjacent-sibling-command hierarchy-node)
                               (assoc :key-prefix tags-key-prefix
                                      :template '(nil :tag)
                                      :subject-referent items-referent))]
    (expr-let
        [dom (if (empty? (:properties hierarchy-node))
               (virtual-item-DOM (conj tags-key-prefix
                                       ;; Need to make it different from
                                       ;; sibling virtuals.
                                       (:item-id (:item example-descendant))
                                       :virtual)
                                 nil :after inherited-for-tags)
               (label-stack-DOM-R
                (hierarchy-node-example-elements hierarchy-node)
                inherited-for-tags))]
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
                               (remove-inherited-for-item only-item))
        descendants-dom (nest-if-multiple-DOM
                          (if (empty? leaves)
                            child-doms
                            (cons (hierarchy-leaf-items-DOM
                                   node inherited-for-leaves)
                                  child-doms))
                          direction)]
    (expr-let [properties-dom (when (or (seq (:properties node))
                                        must-show-labels)
                                (tagged-items-properties-DOM-R
                                 node inherited))]
      (cond-> (if (empty? (:properties node))
                (if must-show-labels
                  (cond-> (add-labels-DOM properties-dom descendants-dom
                                          (opposite-direction direction))
                    (= direction :vertical)
                    (add-attributes {:class "narrow"}))
                  descendants-dom)
                (add-labels-DOM properties-dom descendants-dom
                                (case direction
                                  :vertical :vertical-wrapped
                                  :horizontal :vertical)))
        only-item
        (add-attributes (inherited-attributes inherited only-item))))))

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
                             :width #(* % 0.6875))
        leaves-dom (hierarchy-leaf-items-DOM node inherited-for-items)]
    (map-with-first-last
     horizontal-value-wrapper
     (cons leaves-dom (apply concat child-doms)))))

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
         (cond-> [:div {:class "horizontal-tags-element wide"}
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
       all-labels (expr-seq map #(expr-filter semantic-element?-R
                                              (matching-elements '(nil :tag) %))
                            ordered-elements)
       excludeds (expr-seq map #(when implied-template
                                  (condition-satisfiers-R % implied-template))
                           ordered-elements)]
    (let [labels (map (fn [all minus]
                        (seq (clojure.set/difference (set all) (set minus))))
                      all-labels excludeds)
          no-labels (every? empty? labels)]
      (if (and no-labels (not must-show-labels))
        [(item-stack-DOM
           item-without-labels-DOM-R ordered-elements excludeds direction
           inherited)]
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
  must-show-labels determines whether the elements must show labels.
  inherited must be half way to the children."
  [elements virtual-dom must-show-label must-show-labels
   direction inherited]
  (expr-let
      [tags (expr-seq map #(matching-elements :tag %) elements)]
    (let [pairs (map vector tags elements)
          labels (seq (keep (fn [[tags elem]] (when (seq tags) elem))
                            pairs))
          non-labels (seq (keep (fn [[tags elem]] (when (empty? tags) elem))
                                pairs))]
      (expr-let [elements-dom
                 (when (or non-labels virtual-dom)
                   (expr-let
                       [element-doms
                        (when non-labels
                          (element-DOMs-R
                           non-labels must-show-labels nil direction
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
          (label-stack-DOM-R elements
                             (-> inherited
                                 transform-inherited-for-labels
                                 (add-inherited-attribute {:class "tag"})))
          (and must-show-label elements-dom)
          (wrap-with-labels-DOM
           (virtual-label-DOM inherited) elements-dom direction)
          elements-dom
          elements-dom
          true
          (add-attributes (virtual-label-DOM inherited)
                          {:class "elements-wrapper"}))))))

(defn item-content-DOM
  "Make dom for the content part of an item."
  [item-referent content inherited]
  ;; We don't currently handle items as content. That would need
  ;; more interaction and UI design work to deal with the distinction
  ;; between elements of an item and elements on its content.
  (assert (entity/atom? content))
  (let [anything (#{'anything 'anything-immutable} content)]
    ;; Any attributes we inherit take precedence over basic commands,
    ;; but nothing else.
    [:div (into-attributes
           (into-attributes (select-keys inherited [:selector-category])
                            (content-attributes inherited))
           {:class (cond-> "content-text editable"
                     (= content 'anything-immutable) (str " immutable"))
            :target (copy-alternate-request-to-target
                     (assoc (select-keys inherited [:template])
                            :referent item-referent)
                     inherited)})
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
                (assoc :template '(nil)))]
        (expr-let [elements-dom (elements-DOM-R
                                 elements true nil :vertical inherited-down)]
          [:div {:class "item with-elements"}
           content-dom elements-dom])))))

(defn item-content-and-elements-DOM-R
  "Make a dom for a content and a group of elements, all of the same item.
  Inherited should be half way to the children."
  [content elements must-show-label inherited]
  (expr-let [tags (expr-seq map #(matching-elements :tag %) elements)]
    (let [labels (seq (mapcat (fn [tags element] (when (seq tags) [element]))
                              tags elements))
          non-labels (seq (mapcat (fn [tags element] (when (empty? tags)
                                                       [element]))
                                  tags elements))]
      (expr-let [content-and-elements-dom
                 (item-content-and-non-label-elements-DOM-R
                  content non-labels inherited)]
        (labels-wrapper-DOM-R
           content-and-elements-dom labels must-show-label inherited)))))

(defn item-without-labels-DOM-R
  "Make a dom for an item or exemplar for a group of items,
   given that any of its labels are in excluded-elements.
   Either the referent for the item/group must be provided,
   or inherited must contain :subject-referent.
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
                elements (semantic-elements-R item)
                dom (item-content-and-non-label-elements-DOM-R
                     content (remove (set excluded-elements) elements)
                     inherited-down)]
       (add-attributes
        dom
        (inherited-attributes inherited item))))))

(defn item-DOM-R
  "Make a dom for an item or exemplar of a group of items.
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
               elements (semantic-elements-R item)
               dom (if do-not-show-content
                     (labels-and-elements-DOM-R
                      elements nil must-show-label false :vertical inherited)
                     (item-content-and-elements-DOM-R
                      content (remove (set excluded-elements) elements)
                      must-show-label inherited-down))]
      (add-attributes dom (inherited-attributes inherited item)))))

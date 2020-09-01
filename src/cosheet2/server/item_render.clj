(ns cosheet2.server.item-render
  (:require (cosheet2 [canonical :refer [canonical-set-to-list]]
                      [entity :as entity :refer [label? description->entity
                                                 has-keyword?]]
                      [query :refer [matching-elements]]
                      [utils :refer [multiset-diff assoc-if-non-empty
                                     map-with-first-last
                                     add-elements-to-entity-list
                                     separate-by]]
                      [debug :refer [simplify-for-print]]
                      [hiccup-utils
                       :refer [into-attributes add-attributes]]
                      [expression :refer [expr expr-let expr-seq expr-filter]])
            (cosheet2.server
             [model-utils :refer [semantic-elements
                                  semantic-non-label-elements
                                  semantic-label-elements
                                  semantic-to-list]]
             [hierarchy :refer [replace-hierarchy-leaves-by-nodes
                                hierarchy-node-descendants
                                hierarchy-node-leaves
                                hierarchy-node-logical-leaves
                                hierarchy-by-canonical-info
                                item-maps-by-elements
                                hierarchy-node-example-elements]]
             [order-utils :refer [order-entities semantic-entity?]]
             [render-utils :refer [make-component item-stack-DOM
                                   nest-if-multiple-DOM condition-satisfiers
                                   hierarchy-node-DOM
                                   transform-specification-for-elements]])))
(comment
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
                  (list* (:elements-template inherited) conditions)
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
          elements-template (:elements-template inherited)
          inherited-for-label (-> inherited
                                  (assoc :template (list elements-template :tag)
                                         :subject-referent virtual-ref))
          labels-dom (add-attributes
                      (virtual-element-DOM
                       nil :after
                       (update inherited-for-label :key-prefix #(conj % :label)))
                      {:class "tag"})]
      (add-labels-DOM labels-dom dom direction)))

  (defn labeled-items-for-horizontal-DOMs-R
    [hierarchy inherited]
    (expr-seq map #(hierarchy-node-DOM-R
                    % labeled-items-whole-hierarchy-node-DOM-R
                    (fn [node _ inherited] [[false :horizontal] inherited])
                    [true :horizontal] inherited)
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

  (defn labeled-items-two-column-items-DOMs-R
    "Return the item doms for the node and all its children."
    [node child-doms inherited]
    (let [inherited-for-items (update
                               (add-adjacent-sibling-command inherited node)
                               :width #(* % 0.6875))]
      (expr-let [leaves-dom (hierarchy-leaf-items-DOM-R node inherited-for-items)]
        (map-with-first-last
         horizontal-value-wrapper
         (cons leaves-dom (apply concat child-doms))))))

  (defn labeled-items-two-column-label-DOMs-R
    "Return the label doms for the node and all its children."
    [node child-doms inherited]
    (expr-let [properties-dom (labeled-items-properties-DOM-R
                               node inherited)]
      (map-with-first-last
       horizontal-tag-wrapper
       (cons properties-dom (apply concat child-doms)))))

  (defn labeled-items-for-two-column-DOMs-R
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
                                   % labeled-items-two-column-label-DOMs-R
                                   inherited-for-tags)
                             hierarchy)
                 items-doms (expr-seq
                             map (fn [node only-item]
                                   (hierarchy-node-DOM-R
                                    node labeled-items-two-column-items-DOMs-R
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
    (expr-let [[labels non-labels] (separate-by label? elements)
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
                  elements (semantic-elements-R item)
                  dom (item-content-and-non-label-elements-DOM-R
                       content (remove (set excluded-elements) elements)
                       inherited-down)]
         (add-attributes
          dom
          (inherited-attributes inherited item))))))

  (defn horizontal-label-hierarchy-node-DOM
    "Generate the DOM for a node in a hierarchy that groups items by their
   labels, has at most one leaf per node and is laid out horizontally.
   Don't generate the DOM for its children.
   If referent-f is present, it computes the referent for the leaf, given
   the node and inherited."
    [node {:keys [referent-f top-level]} inherited]
    (let [example-elements (hierarchy-node-example-elements node) 
          item (:item (first (hierarchy-node-logical-leaves node)))
          content (when item (entity/content item))
          non-labels (when item (semantic-non-labels-R item))
          node-referent (if referent-f
                          (referent-f node inherited)
                          (hierarchy-node-items-referent node inherited))
          ;; Set template to what a new leaf has to have.
          inherited (assoc inherited
                           :template (concat '("")
                                             (canonical-set-to-list
                                              (:cumulative-properties node)))
                           :subject-referent node-referent)]
      (if (empty? (:properties node))
        ;; We must be a leaf of a node that has children. We put a virtual
        ;; cell where our labels would go.
        (let [inherited-down (update inherited :key-prefix
                                     #(conj % (:item-id item)))
              label-dom (cond->
                            (virtual-element-DOM
                             node-referent :after
                             (-> inherited-down
                                 transform-inherited-for-labels
                                 (assoc :select-pattern
                                        (conj (:key-prefix inherited)
                                              [:pattern]))))
                          true
                          (add-attributes {:class "tag"})
                          (not top-level)
                          (add-attributes {:class "merge-with-parent"}))
              inner-dom (item-content-and-non-label-elements-DOM-R
                         content non-labels inherited-down)]
          [:div {:class (cond-> "tag wrapped-element virtual-wrapper"
                          (not top-level)
                          (str " merge-with-parent"))}
           label-dom
           [:div {:class "indent-wrapper tag"}
            (add-attributes
             inner-dom
             {:key (:key-prefix inherited-down)
              :class "item"})]])
        (if (empty? (:child-nodes node))
          (let [all-elements (concat example-elements non-labels)]
            (item-content-and-elements-DOM-R
             item content all-elements false
             (update inherited :key-prefix #(conj % (:item-id item)))))
          (label-stack-DOM-R example-elements
                             ;; A generic label could match many, which we don't
                             ;; want. Rather than find all the competing labels
                             ;; of all our children, don't match multiple at all.
                             ;; TODO: Actually do go through all the children.
                             (dissoc inherited :match-multiple))))))

  (defn element-hierarchy-child-info
    "Generate the function-info and inherited for children of
   a hierarchy node of an element hierarchy.
  The function-info is a map with at least
     :top-level  Whether this is a top level node.
  Inherited describes the items."
    [node function-info inherited]
    (let [children (:child-nodes node)]
      [(assoc function-info
              :top-level false)
       (-> inherited
           (update :key-prefix #(conj % :nested))
           ;; Set :template to the minimum to be a child node.
           ;; This is used by the virtual column of a table header.
           (update :template
                   #(add-elements-to-entity-list
                     % (canonical-set-to-list (:properties node)))))]))
  )
(defn render-virtual-DOM [] (assert false))
(defn action-info-virtual [] (assert false))

(defn opposite-direction
    [direction]
    (case direction
      :horizontal :vertical
      :vertical :horizontal))

(defn virtual-element-DOM
  "Make a dom for a place where there could be an element, but isn't"
  [specification]
  (assert (:twin-template specification))
  (make-component (assoc specification
                         :render-dom render-virtual-DOM
                         :sub-action-info action-info-virtual)))

(defn add-labels-DOM
  "Add label dom to an inner dom. Direction gives the direction of the label
  with respect to the inner dom. It can also be :vertical-wrapped, which puts
  it above the inner dom, but with an indentation on the left too."
    [labels-dom inner-dom direction]
    (if (= direction :vertical-wrapped)
      [:div {:class "wrapped-element label"}
       labels-dom
       [:div {:class "indent-wrapper"} inner-dom]]
      [:div {:class (case direction
                      :vertical "vertical-labels-element label"
                      :horizontal "horizontal-labels-element label")}
       labels-dom inner-dom]))

(defn wrap-with-labels-DOM
  "Direction gives the direction of the label with respect to the inner dom.
  :vertical is interpreted as :vertical-wrapped."
    [labels-dom inner-dom direction]
  (add-labels-DOM labels-dom inner-dom
                  (if (= direction :vertical) :vertical-wrapped direction)))

(defn label-stack-DOM
  "Given a non-empty list of label elements, return a stack of their doms.
   Inherited should be halfway transformed to children."
  [label-elements specification]
  (let [ordered-labels (order-entities label-elements)
        label-tags (map #(condition-satisfiers % '(nil :label))
                       ordered-labels)]
    (item-stack-DOM ordered-labels label-tags :vertical
                    (into-attributes specification {:class "label"}))))

(defn virtual-label-DOM
    "Return a dom for a virtual label"
  [specification]
  (assert (:twin-template specification))
  (virtual-element-DOM
   (-> specification
       (assoc :position :after
              :relative-id :virtual-label)
       (update :twin-template
               #(if (has-keyword? % :label)
                  %
                  (add-elements-to-entity-list % [:label])))
       (into-attributes {:class "label"}))))

(defn non-empty-labels-wrapper-DOM
  "Given a dom for an item, not including its labels, and a non-empty 
  list of labels, make a dom that includes the labels wrapping the item."
  [inner-dom label-elements direction specification]
  (let [stack (label-stack-DOM label-elements specification)]
    (wrap-with-labels-DOM stack inner-dom direction)))

(defn labels-wrapper-DOM
  "Given a dom for an item, not including its labels, and a list of labels,
  make a dom that includes any necessary labels wrapping the item.
  specification should be the one for the item." 
  [dom label-elements specification]
  (if (and (empty? label-elements) (not (:must-show-label specification)))
    dom
    (let [elements-spec (transform-specification-for-elements specification)]
      (if (not (empty? label-elements))
        (non-empty-labels-wrapper-DOM
         dom label-elements :vertical elements-spec)
        [:div {:class "horizontal-tags-element tag virtual-wrapper narrow"}
         (virtual-label-DOM elements-spec)
         dom]))))

(def labeled-items-for-two-column-DOMs)
(def labeled-items-for-horizontal-DOMs)
(def exemplar-action-data)

(defn labeled-items-properties-DOM
  "Given a hierarchy node for labels, Return DOM for example elements
  that give rise to the properties of the node. The specification applies
  to the overall node."
    [hierarchy-node specification]
  (let [descendant-items (map :item (hierarchy-node-descendants hierarchy-node))
        descendant-ids (map :item-id descendants)
        example-descendant-id (first descendant-ids)
        child-spec (assoc specification
                          :action_data [exemplar-action-data descendant-ids])]
      (let [dom (if (empty? (:properties hierarchy-node))
                  (virtual-element-DOM
                   (assoc specification
                          ;; TODO: Track hierarchy depth in the spec, and use
                          ;; it to uniquify virtual labels.
                          :relative-id :virtual-label
                          :relative-identity [:virtual-label
                                              example-descendant-id]))
                 (label-stack-DOM
                  (hierarchy-node-example-elements hierarchy-node)
                  child-spec))]
        ;; Even if stacked, we need to mark the stack as "label" too.
        (add-attributes dom {:class "label"}))))

(defn hierarchy-leaf-items-DOM
  "Given a hierarchy node with labels as the properties, generate DOM
  for leaves that are items. The leaves of the node may contain an additional
  :exclude-elements field that gives more of the item's elements not
  to show, typically the ones that satisfy the :twin-template of the
  specification. The specification should be the one for the overall
  node."
  [hierarchy-node specification]
  (let [leaves (hierarchy-node-leaves hierarchy-node)
        property-list (canonical-set-to-list
                       (:cumulative-properties hierarchy-node))
        leaf-spec (if (empty? property-list)
                    specification
                    (update specification :twin-template
                            #(add-elements-to-entity-list
                              % property-list)))]
    (if (empty? leaves)
      (let [adjacent-item (:item (first (hierarchy-node-descendants
                                         hierarchy-node)))
            example-elements (hierarchy-node-example-elements hierarchy-node)]
        (virtual-element-DOM
         (assoc leaf-spec
                :relative-id :virtual
                :relative-identity [:virtual
                                    (:item-id (first example-elements))]
                :adjacent-id (:item-id adjacent-item)
                :direction :after)))
      (let [items (map :item leaves)
            excludeds (map #(concat (:property-elements %)
                                    (:exclude-elements %))
                           leaves)]
        (item-stack-DOM
         items excludeds :vertical leaf-spec)))))

(defn labeled-items-whole-hierarchy-node-DOM-R
  "Return the dom for everything at and under a labeled items hierarchy node.
  direction gives which way to lay out the contained items.
  The specification must give :direction."
  [node child-doms {:keys [must-show-labels direction] :as specification}]
  (assert (#{:horizontal :vertical} direction))
  (let [leaves (hierarchy-node-leaves node)
        only-item (when (and (empty? child-doms) (= (count leaves) 1))
                    (:item (first leaves)))]
    (let [leaf-dom (when (seq leaves)
                     (hierarchy-leaf-items-DOM node specification))
          properties-dom (when (or (seq (:properties node))
                                   must-show-labels)
                           (labeled-items-properties-DOM
                            node specification))]
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
          (add-attributes (select-keys specification [:class])))))))

(defn labeled-items-for-one-column-DOMs
  [hierarchy specification]
  (let [top-level-spec (assoc specification
                              :must-show-labels true
                              :direction :vertical)
        child-specification-f (fn [_ specification]
                                (dissoc specification :must-show-labels))]
    (map #(hierarchy-node-DOM %
                              labeled-items-whole-hierarchy-node-DOM
                              child-specification-f
                              top-level-spec)
         hierarchy)))

(defn items-with-labels-DOM
  "Make a dom for a sequence of items.
   If implied-template is non-nil, don't show elements implied by it.
   If must-show-labels is true, show a space for labels, even if
   there are none. If, additionally, it is :wide, show them with substantial
   space, if there is significant space available."
  [elements implied-template must-show-labels direction specification]
  (let
      [ordered-elements (order-entities elements)
       all-labels (map semantic-label-elements ordered-elements)
       excludeds (map (if implied-template
                        #(condition-satisfiers % implied-template)
                        (constantly nil))
                      ordered-elements)]
    (let [labels (map (fn [all exclusions]
                        (clojure.set/difference (set all) (set exclusions)))
                      all-labels excludeds)
          no-labels (every? empty? labels)]
      (if (and no-labels (not must-show-labels))
        (item-stack-DOM ordered-elements excludeds
                        direction specification)
        (let [item-maps (item-maps-by-elements ordered-elements labels)
              augmented (map (fn [item-map excluded]
                               (assoc item-map :exclude-element-ids
                                      (map :item-id excluded)))
                             item-maps excludeds)
              hierarchy (hierarchy-by-canonical-info augmented)]
          (case direction
            :vertical
            (if (or (< (:width specification) 1.0)
                    (and no-labels (not (= must-show-labels :wide))))
              (labeled-items-for-one-column-DOMs hierarchy specification)
              (labeled-items-for-two-column-DOMs hierarchy specification))
            :horizontal
            (labeled-items-for-horizontal-DOMs
             (replace-hierarchy-leaves-by-nodes hierarchy) specification)))))))

(defn item-content-DOM
  "Make dom for the content part of an item."
  [item attributes]
  ;; We don't currently handle content that itself non-trivial
  ;; entities. That would need more interaction and UI design work to
  ;; deal with the distinction between elements of an item and
  ;; elements on its content.
  (let [content (entity/content item)]
    (assert (entity/atom? content))
    (let [anything (#{'anything 'anything-immutable} content)]
      [:div (cond-> (-> (select-keys attributes [:class])
                        (into-attributes {:class "content-text editable"} ))
              (has-keyword? item :label)
              (into-attributes (:class "label")))
       (if anything "\u00A0..." (str content))])))

(defn render-content-only-DOM
  "Render a dom spec for only the content of an item."
  [{:keys [relative-id item-id class]} store]
  (assert (= relative-id :content))
  (item-content-DOM (description->entity item-id store)
                    (if class {:class class} {})))

(defn item-content-and-non-label-elements-DOM
    "Make a dom for a content and a group of non-label elements."
    [item elements specification]
  (let [content-dom (make-component
                     (cond-> (-> (select-keys specification
                                              [:twin-template :class :width])
                                 (assoc :relative-id :content
                                        :item-id (:item-id item)
                                        :render-dom render-content-only-DOM))
                       (has-keyword? item :label)
                       (into-attributes {:class "label"})))]
      (if (empty? elements)
        content-dom
        (let [elements-spec (transform-specification-for-elements specification)
              elements-dom (items-with-labels-DOM
                            elements nil true :vertical elements-spec)]
          [:div (cond-> {:class "with-elements"}
                  (has-keyword? item :label)
                  (str " label"))
           content-dom elements-dom]))))

(defn render-item-DOM
  "Render a dom spec for an item (which may be an exemplar of a
  group of items)."
  [{:keys [relative-id item-id must-show-label excluded-element-ids]
    :as specification}
   store]
  (println "Generating DOM for" (simplify-for-print relative-id))
  (let [entity (description->entity (or item-id relative-id) store)
        elements (remove (set (map #(description->entity % store)
                                   excluded-element-ids))
                         (semantic-elements entity))
        [labels non-labels] (separate-by label? elements)]
    (-> (if (and (empty? elements) (not must-show-label))
          (item-content-DOM entity specification)
          (let [inner-spec (update specification :twin-template
                                   #(add-elements-to-entity-list
                                     % (map semantic-to-list labels)))
                inner-dom (item-content-and-non-label-elements-DOM
                           entity non-labels inner-spec)]
            (labels-wrapper-DOM
             inner-dom labels specification)))
        (add-attributes {:class "item"}))))

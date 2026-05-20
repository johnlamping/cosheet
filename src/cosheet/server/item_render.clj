(ns cosheet.server.item-render
  (:require
   (cosheet
    [canonical :refer [canonical-set-to-list]]
    [store :refer [make-item-id]]
    [entity :refer [id->entity id->updating-entity-R
                    content label-element? primitive? object? element?
                    interned-object? universal-object?
                    label-object?
                    elements label->elements content->elements
                    name-label link-type object-type
                    make-object-list recursively-in-different-store
                    entity-complexity 
                    add-elements-to-entity]]
    [query :refer [matching-elements]]
    [utils :refer [multiset-diff assoc-if-non-empty
                   map-with-first-last
                   separate-by]]
    [debug :refer [simplify-for-print]]
    [hiccup-utils :refer [dom-attributes into-attributes add-attributes
                          merge-classes]]
    [reporter-macros :refer [app-R let-R]])
   (cosheet.server
    [model-utils :refer [semantic-elements
                         semantic-non-label-elements semantic-label-elements
                         semantic-to-list entity->canonical-semantic
                         elements-to-change-to-satisfy-fixed-term-elements]]
    [hierarchy :refer [replace-hierarchy-leaves-by-nodes
                       hierarchy-node-descendants
                       hierarchy-node-leaves
                       hierarchy-node-logical-leaves
                       hierarchy-by-canonical-info
                       item-maps-by-elements
                       hierarchy-node-example-elements]]
    [order-utils :refer [ordered-entities]]
    [render-utils :refer [make-sequential-template
                          ensure-label-object-content
                          make-virtual-label-template
                          make-component
                          nest-if-multiple-DOM
                          condition-satisfiers
                          hierarchy-node-DOM
                          transform-specification-for-elements
                          transform-specification-for-labels
                          transform-specification-for-non-contained-labels
                          specification-item-id]]
    [action-data :refer [default-get-action-data
                         default-get-do-batch-edit-action-data
                         get-item-or-exemplar-action-data
                         get-item-do-batch-edit-action-data
                         get-pass-through-action-data
                         get-virtual-action-data
                         parallel-items-get-action-data
                         parallel-items-get-do-batch-edit-action-data
                         compose-action-data-getter]])))

(defn opposite-orientation
    [orientation]
    (case orientation
      :horizontal :vertical
      :vertical :horizontal))

(def render-item-DOM-R)

(defn item-component
  "Make a component dom to display the given item. The item's id becomes
  the relative-id, and the render-dom and get-action-data are filled in."
  [item specification]
  (assert (not (:relative-id specification))
          (:relative-id specification))
  (assert (not (:render-dom specification))
          (:render-dom specification))
  (make-component (assoc specification
                         :relative-id (:item-id item)
                         :render-dom render-item-DOM-R
                         :get-action-data (or (:get-action-data specification)
                                              default-get-action-data))))

(defn item-minus-excluded-component
  "Make a component dom to display the given item, minus the excluded
  elements."
  [item excluded-elements specification]
  (assert (empty? (:excluded-element-ids specification))
          [excluded-elements specification])
  (assert (not (:get-action-data specification))
          (:get-action-data specification))
  (let [new-spec (cond-> specification
                   (seq excluded-elements)
                   (assoc :excluded-element-ids
                          (vec (map :item-id excluded-elements))))]
    (item-component item new-spec)))

(defn item-stack-DOM
  "Given a list of items and a matching list of elements to exclude,
  generate components for each item, and put them in a DOM.
  If there is more than one item, make the stack in the given orientation."
  [items excludeds orientation specification]
  (let [components (map #(item-minus-excluded-component %1 %2 specification)
                        items excludeds)]
    (nest-if-multiple-DOM components orientation)))

(defn get-virtual-DOM-rendering-data [spec store]
  [])

(defmethod print-method
  cosheet.server.item_render$get_virtual_DOM_rendering_data
  [v ^java.io.Writer w]
  (.write w "virt-RD"))

(defn render-virtual-DOM [spec ms]
  [:div (into-attributes (select-keys spec [:class])
                         {:class "editable virtual"})])

(defmethod print-method
  cosheet.server.item_render$render_virtual_DOM
  [v ^java.io.Writer w]
  (.write w "virt-DOM"))

(defn add-labels-DOM
  "Add a labels dom to an inner dom. Orientation gives the orientation
  between the doms. It can also be :vertical-wrapped, which puts the
  label above the inner dom, but with an indentation on the left too."
    [labels-dom inner-dom orientation]
    (if (= orientation :vertical-wrapped)
      [:div {:class "wrapped-element label"}
       labels-dom
       [:div {:class "indent-wrapper"} inner-dom]]
      [:div {:class (case orientation
                      :vertical "vertical-labels-element label"
                      :horizontal "horizontal-labels-element")}
       labels-dom inner-dom]))

(defn wrap-with-labels-DOM
  "Orientation gives the orientation of the label with respect to the inner dom.
  :vertical is interpreted as :vertical-wrapped."
    [labels-dom inner-dom orientation]
  (add-labels-DOM labels-dom inner-dom
                  (if (= orientation :vertical) :vertical-wrapped orientation)))

(defn virtual-DOM-component
  "Make a component for a place where there could be an entity, but
  isn't. The specification may have a :get-action-data, in which case
  it will be run before get-virtual-action-data.
  The specification keys unique to the virtual action data are
  :template :sibling :position and :use-bigger."
  [specification]
  (assert (:template specification) specification)
  (make-component
   (-> specification 
       (assoc :render-dom render-virtual-DOM)
       (dissoc :get-do-batch-edit-action-data)
       (update :get-action-data
               #(compose-action-data-getter % get-virtual-action-data)))))

(defn virtual-label-DOM-component
  "Return a dom for a virtual label. The label must occur inside an
  overall component for the item it modifies. The specification should
  have a template for what needs to be on the label element, beyond
  the label. We don't require relative-id, but our callers must use it
  if there are multiple virtual labels under the same component, to
  distinguish their ids."
  [{:keys [relative-id template] :as specification}]
  (assert template specification)
  (virtual-DOM-component
   (-> specification
       (assoc :relative-id (or relative-id :virtual-label)
              :position :after
              :template (make-virtual-label-template template)
              :is-object-name true)
       (into-attributes {:class "label"}))))

(defn virtual-entity-and-label-DOM
  "Return the dom for a virtual entity and a virtual label for it.
   The arguments are the same as for virtual-DOM-component."
  [{:keys [template class] :as specification} orientation]
  (let [dom (virtual-DOM-component specification)
        labels-dom (virtual-label-DOM-component
                    (-> specification
                        (dissoc :relative-id)
                        (assoc :template (make-sequential-template
                                          [template 'anything]))))]
    (cond-> (wrap-with-labels-DOM labels-dom dom orientation)
      class
      (add-attributes {:class class}))))

(defn add-parallel-item-ids
  "Add :parallel-ids and the appropriate action-data getters for a DOM
  that refers to several items."
  [specification item-ids]
  (-> (assoc specification :parallel-ids item-ids)
      (update :get-action-data
              (fn [getter]
                [parallel-items-get-action-data
                 (or getter get-item-or-exemplar-action-data)]))
      (update :get-do-batch-edit-action-data
              (fn [getter]
                [parallel-items-get-do-batch-edit-action-data
                 (or getter get-item-do-batch-edit-action-data)]))))

(defn add-parallel-item-ids-for-label
  "Add :parallel-ids and the appropriate action-data getters for a
  label that covers a DOM that refers to several items. The incoming
  specification should be what is expected for the items the label is
  about."
  [specification item-ids]
  (cond-> (assoc specification :parallel-ids item-ids)
    ;; The default action getters are right, unless the current getter was
    ;; not already the default.
    (:get-action-data specification)
    (update :get-action-data
            (fn [getter] (compose-action-data-getter
                          [parallel-items-get-action-data getter]
                          get-item-or-exemplar-action-data)))
    (:get-do-batch-edit-action-data specification)
    (update :get-do-batch-edit-action-data
            (fn [getter] (compose-action-data-getter
                          [parallel-items-get-do-batch-edit-action-data getter]
                          get-item-do-batch-edit-action-data)))))

(defn label-stack-DOM
  "Given a non-empty list of label elements, return a stack of their doms."
  [label-elements specification]
  (let [ordered-labels (ordered-entities label-elements)
        ;; TODO: !!! Get rid of this once it's clear it's not
        ;; needed. (Once the :label tags are removed from the tests
        ;; and elsewhere.)
        label-tags (map #(condition-satisfiers % '(nil :label))
                        ordered-labels)]
    (item-stack-DOM ordered-labels label-tags :vertical
                    (-> specification
                        (update :template ensure-label-object-content)
                        (into-attributes {:class "label"})))))

(defn non-empty-labels-wrapper-DOM
  "Given a dom for an item, not including its labels, and a non-empty 
  list of labels, make a dom that includes the labels wrapping the item."
  [inner-dom label-elements orientation specification]
  (let [stack (label-stack-DOM label-elements specification)]
    (wrap-with-labels-DOM stack inner-dom orientation)))

(defn labels-wrapper-DOM
  "Given a dom for an item, not including its labels, and a list of labels,
  make a dom that includes any necessary labels wrapping the item.
  specification should be the one for the item." 
  [dom label-elements specification]
  (add-attributes
   (if (and (empty? label-elements) (not (:must-show-label specification)))
     dom
     (let [labels-spec (transform-specification-for-labels specification)]
       (if (not (empty? label-elements))
         (non-empty-labels-wrapper-DOM
          dom label-elements :vertical labels-spec)
         [:div {:class "horizontal-labels-element virtual-wrapper narrow"}
          (virtual-label-DOM-component labels-spec)
          dom])))
    (select-keys specification [:class])))

(defn hierarchical-elements-property-elements-DOM
  "Given a node of a hierarchy of entity info maps for a sequence of
  elements organized by their labels, Return DOM for example elements
  that give rise to the properties of the node, which will be
  labels. The specification should apply to the overall node."
  [hierarchy-node specification]
  (let [descendant-items (map :item (hierarchy-node-descendants hierarchy-node))
        descendant-ids (map :item-id descendant-items)
        ;; Note: parallel-items-get-do-batch-edit-action-data assumes that
        ;;       we take the first of the descendants.
        example-descendant-id (first descendant-ids)
        labels-spec (transform-specification-for-non-contained-labels
                     specification)]
    (let [dom (if (empty? (:properties hierarchy-node))
                (do
                  (assert (label-object? (content (:template labels-spec)))
                          labels-spec)
                  (virtual-label-DOM-component
                   ;; TODO: Track hierarchy depth in the spec, and use
                   ;; it to uniquify virtual labels.
                   (-> labels-spec
                       (assoc :relative-id [example-descendant-id
                                            :virtual-label])
                       (add-parallel-item-ids descendant-ids))))
                (label-stack-DOM
                 (hierarchy-node-example-elements hierarchy-node)
                 (add-parallel-item-ids-for-label labels-spec descendant-ids)))]
      ;; Even if stacked, we need to mark the stack as "label" too.
      (add-attributes dom {:class "label"}))))

(defn hierarchy-leaf-elements-DOM
  "Given a node of a hierarchy of entity info maps for a sequence of
  elements organized by their labels, generate DOM for its leaf
  elements, or a virtual DOM if there are no leaves. The leaves of the
  node may contain an additional :exclude-elements field that gives
  more of the item's elements not to show, typically the ones that
  satisfy the :template of the specification. The specification should
  be the one for the overall hierarchy."
  [hierarchy-node specification]
  (assert (empty? (:excluded-element-ids specification)) specification)
  (let [leaves (hierarchy-node-leaves hierarchy-node)
        property-list (canonical-set-to-list
                       (:cumulative-properties hierarchy-node))
        leaf-spec (cond-> (dissoc specification :orientation)
                    (not (empty? property-list))
                    (update :template
                            #(add-elements-to-entity % property-list)))]
    (if (empty? leaves)
      (let [adjacent-item (:item (first (hierarchy-node-descendants
                                         hierarchy-node)))
            example-elements (hierarchy-node-example-elements hierarchy-node)]
        (virtual-DOM-component
         (assoc leaf-spec
                :relative-id :virtual
                :sibling true
                :position :after)))
      (let [items (map :item leaves)
            excludeds (map #(concat (:property-elements %)
                                    (:exclude-elements %))
                           leaves)]
        (item-stack-DOM items excludeds :vertical leaf-spec)))))

(defn hierarchical-elements-node-f-DOM
  "This is a node-f for hierarchy-node-DOM. It takes a node of a
  hierarchy of entity info maps for a sequence of elements organized
  by their labels and it takes doms for all its child nodes. It makes
  the dom for the leaves, and assembles that with the child doms. The
  specification must give orientation in which to lay out the
  contained elements. (That is totally different from the orientation
  of an element, which is either :source or :target)."
  [node child-doms {:keys [must-show-label orientation] :as specification}]
  (assert (#{:horizontal :vertical} orientation) orientation)
  (assert (empty? (:excluded-element-ids specification)) specification)
  (let [leaves (hierarchy-node-leaves node)
        only-item (when (and (empty? child-doms) (= (count leaves) 1))
                    (:item (first leaves)))]
    (let [leaf-dom (when (seq leaves)
                     (hierarchy-leaf-elements-DOM
                      node (dissoc specification :must-show-label)))
          descendants-dom (nest-if-multiple-DOM (if leaf-dom
                                                  (cons leaf-dom child-doms)
                                                  child-doms)
                                                orientation)
          properties-dom (when (or (seq (:properties node))
                                   must-show-label)
                           (hierarchical-elements-property-elements-DOM
                            node specification))]
      (cond-> (if (empty? (:properties node))
                (if must-show-label
                  (cond-> (add-labels-DOM properties-dom descendants-dom
                                          (opposite-orientation orientation))
                    true
                    (add-attributes {:class "virtual-wrapper"})
                    (= orientation :vertical)
                    (add-attributes {:class "narrow"}))
                  descendants-dom)
                (add-labels-DOM properties-dom descendants-dom
                                :vertical-wrapped))
        only-item
        (add-attributes (select-keys specification [:class]))))))

(defn hierarchical-elements-in-horizontal-DOM
  [hierarchy specification]
  (map #(hierarchy-node-DOM
         % hierarchical-elements-node-f-DOM
         (fn [node specification] (assoc specification :must-show-label false))
         (assoc specification
                :must-show-label (not (:immutable specification))
                :orientation :horizontal))
       hierarchy))

(defn horizontal-label-wrapper
  "Return a modifier for a horizontal label dom that is logically part of
  a possibly larger entity."
  [body is-first is-last]
  [:div {:class (cond-> "label horizontal-header"
                  is-first (str " top-border")
                  (not is-first) (str " indent")
                  is-last (str " bottom-border"))}
   body])

(defn horizontal-value-wrapper
  "Return a modifier for a value in a horizontal label layout that is 
   logically part of a larger entity."
  [body is-first is-last]
  (if (and is-last (not is-first))
    [:div {:class "horizontal-value-last"} body]
    body))

(defn one-column-of-hierarchy-two-column-DOM
  "Given a hierarchy of entity info maps for a sequence of elements,
   organized by their labels, make a column of doms, based on running
   the node-fn on each node, and wrapper-fn on each collection of
   doms."
  [hierarchy node-fn wrapper-fn specification width-multiplier]
  (let [specification (update specification :width #(* % width-multiplier))]
    (map (fn [node]
           (hierarchy-node-DOM
            node
            (fn [node child-doms specification]
              (let [dom (node-fn node specification)]
                (map-with-first-last
                 wrapper-fn
                 (cons dom (apply concat child-doms)))))
            specification))
         hierarchy)))

(defn hierarchical-elements-in-two-column-DOM
  "Given a hierarchy of entity info maps for a sequence of elements,
  organized by their labels, make a two column dom for them; one
  column for the labels, and one for the contents and
  sub-elements. The specification should apply to each item the
  hierarchy is over."
  [hierarchy specification]
  (let [;; If there is only one item below a top level node, we put
        ;; any item specific attributes, including labels, on the
        ;; overall node as well, while if there are several items,
        ;; we can only put them on each item.
        only-items (map #(let [leaves (hierarchy-node-leaves %)]
                           (when (and (empty? (:child-nodes %))
                                      (= (count leaves) 1))
                             (:item (first leaves))))
                        hierarchy)]
    (let [label-spec (transform-specification-for-non-contained-labels
                      specification)
          label-doms (one-column-of-hierarchy-two-column-DOM
                      hierarchy
                      hierarchical-elements-property-elements-DOM horizontal-label-wrapper
                      label-spec 0.25)          
          items-doms (one-column-of-hierarchy-two-column-DOM
                      hierarchy
                      hierarchy-leaf-elements-DOM horizontal-value-wrapper
                      specification 0.6875)]
      (map
       (fn [label-dom items-dom only-item]
         (cond-> [:div {:class "horizontal-labels-element label wide"}
                  label-dom items-dom]
           only-item
           (add-attributes (select-keys specification [:class]))))
       (apply concat label-doms)
       (apply concat items-doms)
       (apply concat (map (fn [doms only-item]
                            (map (constantly only-item) doms))
                          items-doms only-items))))))

(defn hierarchical-elements-in-one-column-DOM
  "Given a hierarchy of entity info maps for a sequence of elements,
  organized by their labels, make a one column dom for them. The
  specification should apply to each item the hierarchy is over."
  [hierarchy specification]
  (let [top-level-spec (assoc specification
                              :must-show-label (not (:immutable specification))
                              :orientation :vertical)
        child-specification-f (fn [_ specification]
                                (dissoc specification :must-show-label))]
    (map #(hierarchy-node-DOM %
                              hierarchical-elements-node-f-DOM
                              child-specification-f
                              top-level-spec)
         hierarchy)))

;;; The next two functions make stacks of components for entities.

(defn non-label-elements-DOM
  "Make a dom for a sequence of elements, all of which must not be labels.
   If implied-template is non-nil, don't show elements implied by it.
   If must-show-label is true, show a space for labels, even if
   there are none. If, additionally, it is :wide, show them with substantial
   space, if there is significant space available."
  [elements implied-template must-show-label orientation specification]
  (let [ordered-elements (ordered-entities elements)
        all-labels (map semantic-label-elements ordered-elements)
        excludeds (map (if implied-template
                         #(condition-satisfiers % implied-template)
                         (constantly nil))
                       ordered-elements)]
    (let [labels (map (fn [all exclusions]
                        (clojure.set/difference (set all) (set exclusions)))
                      all-labels excludeds)
          no-labels (every? empty? labels)]
      (if (and no-labels (not must-show-label))
        (item-stack-DOM ordered-elements excludeds orientation specification)
        (let [item-maps (item-maps-by-elements ordered-elements labels)
              augmented (map (fn [item-map excluded]
                               (assoc item-map :exclude-elements excluded))
                             item-maps excludeds)
              hierarchy (hierarchy-by-canonical-info augmented)
              doms (case orientation
                     :vertical
                     ((if (or (< (:width specification) 1.0)
                              (and no-labels (not (= must-show-label :wide))))
                        hierarchical-elements-in-one-column-DOM
                        hierarchical-elements-in-two-column-DOM)
                      hierarchy specification)
                     :horizontal
                     (hierarchical-elements-in-horizontal-DOM
                      (replace-hierarchy-leaves-by-nodes hierarchy)
                      specification))]
          (nest-if-multiple-DOM doms orientation))))))

(defn labels-and-elements-DOM
  "Generate the dom for a set of elements, some of which may be labels.
  virtual-dom, if present, will appear after the elements.
  elements-must-show-labels determines whether the elements must show labels.
  The specifications should be appropriate for each of the elements."
  ;; This function is only called from outside item-render.
  [elements virtual-dom must-show-label elements-must-show-labels
   orientation specification]
  (assert (not (:relative-id specification))
          (:relative-id specification))
  (let [[labels non-labels] (separate-by label-element? elements)
        elements-dom
        (when (or non-labels virtual-dom)
          (let [elements-dom
                (when non-labels
                  (non-label-elements-DOM
                   non-labels nil elements-must-show-labels
                   orientation specification))]
            (nest-if-multiple-DOM
             (remove nil? [elements-dom virtual-dom]) orientation)))]
    (cond
      (and labels elements-dom)
      (non-empty-labels-wrapper-DOM
       elements-dom labels orientation specification)
      labels
      (label-stack-DOM elements specification)
      (and must-show-label elements-dom)
      (wrap-with-labels-DOM
       (virtual-label-DOM-component specification) elements-dom orientation)
      elements-dom
      elements-dom
      true
      (virtual-label-DOM-component
       (add-attributes specification {:class "elements-wrapper"})))))

;;; The next functions handle the parts of the dom for an element

(defn element-primitive-content-DOM
  "Make dom for a primitive that is the content part of an item."
  [item primitive {:keys [class] :as specification}]
  (assert (primitive? primitive) primitive)
  (let [anything (= 'anything primitive)]
    [:div (cond-> (into-attributes
                   {:class class}
                   {:class (cond-> "content-text"
                             anything (str " placeholder"))})
            (label-element? item)
            (into-attributes (:class "label"))
            anything
            (into-attributes (:class "placeholder")))
     (if anything "\u00A0..." (str primitive))]))

(defn css-class-for-name
  "Return the class to use in formatting the name of this object"
  [object]
  (cond (seq (content->elements object link-type)) "label"
        (seq (content->elements object object-type)) "class"
        true "name"))

(defn render-object-reference-DOM-R
  "Produce dom for an object reference. This means that we just show its
  name, and editing the displayed name doesn't change the object, but
  selects (or creates) an object with the provided name. The
  specification should have a :relative-id of :content, and an
  auxiliary-item-id that gives the id of the object."
  [{:keys [auxiliary-item-id relative-id] :as specification} store]
  (assert (= relative-id :content))
  (let-R [object (id->updating-entity-R auxiliary-item-id store)]
    (let [names (-> (label->elements object name-label)
                    ordered-entities)
          num-names (count names)
          specification (->
                         specification
                         (dissoc :auxiliary-item-id :relative-id :render-dom)
                         (assoc :omit-universal-elements true
                                :is-object-name true
                                :get-action-data get-pass-through-action-data)
                         (into-attributes
                          {:class (css-class-for-name object)}))]
      (assert (> num-names 0))
      (if (= num-names 1)
        (item-component (first names)
                        (into-attributes specification
                                         {:class "object-reference"}))
        (into [:div {:class "object-reference vertical-stack"}]
              (map #(item-component % specification) names))))))

(defn object-reference-component
  "Return a component to display an object in object-reference form."
  [object specification]
  (assert (object? (:template specification))
          (:template specification))
  (make-component (assoc specification
                         :relative-id :content
                         :auxiliary-item-id (:item-id object)
                         :render-dom render-object-reference-DOM-R
                         :get-action-data get-pass-through-action-data)))

(defn display-content-object-as-if-interned?
  "Return true if an object that is being shown as a content, and with
  the given template, should be shown as if it were interned object,
  even though it has a generic name. We return true if the object
  matches the template, with nothing extra. These objects can result
  from add-twin on an interned object, and we want to show them
  in the same format as their twin."
  [template object]
  (and (seq (label->elements object name-label))
       ;; The template and object might mention identified objects
       ;; with the same id, but from different stores. That shouldn't
       ;; count as a difference.
       (= (entity->canonical-semantic
           (recursively-in-different-store object nil))
          (entity->canonical-semantic
           (recursively-in-different-store template nil)))))

(defn element-content-DOM
  "Make dom for the content of an element."
  [element {:keys [immutable template] :as specification}]
  (let [contents (content element)
        editable (not immutable)
        specification (cond-> (-> (select-keys specification [:class :width])
                                  (assoc :template (content template)))
                        editable (into-attributes {:class "editable"}))]
    (cond (primitive? contents)
          (element-primitive-content-DOM element contents specification)
          (and (object? contents)
               (or (interned-object? contents)
                   (display-content-object-as-if-interned?
                    (content template) contents)))
          (object-reference-component contents specification)
          true
          ;; TODO: !!! We don't currently handle ordinary content that
          ;; is itself a structured entity. We will need that for
          ;; anonymous objects.
          (assert false contents))))

(defn render-content-only-DOM
  "Given an item that represents an element, render a dom spec for only
  its content."
  [{:keys [relative-id auxiliary-item-id] :as specification} store]
  (assert (= relative-id :content) relative-id)
  (let-R [element (id->updating-entity-R auxiliary-item-id store)]
    (element-content-DOM
     element (select-keys specification [:class :width :immutable :template]))))

(defmethod print-method
  cosheet.server.item_render$render_content_only_DOM
  [v ^java.io.Writer w]
  (.write w "content-DOM"))

(defn element-content-and-non-label-elements-DOM
  "Make a DOM for a content and a group of non-label elements."
  [element elements specification]
  (let [content-dom
        (make-component
         (cond-> (-> (select-keys specification
                                  [:template :class :width])
                     (assoc :relative-id :content
                            :auxiliary-item-id (:item-id element)
                            :render-dom render-content-only-DOM
                            :get-action-data get-pass-through-action-data))
           (label-element? element)
           (into-attributes {:class "label"})))]
      (if (empty? elements)
        content-dom
        (let [elements-spec (transform-specification-for-elements specification)
              elements-dom (non-label-elements-DOM
                            elements nil
                            (or (:must-show-label specification) true)
                            :vertical elements-spec)]
          [:div {:class (cond-> "with-elements"
                          (label-element? element)
                          (merge-classes "label"))}
           content-dom elements-dom]))))

(defn element-content-labels-and-non-label-elements-DOM
  "Given an element, its labels, and its non-label elements, and it dom
  specification, generate its dom."
  [element labels non-labels {:keys [must-show-label] :as specification}]
  (-> (if (and (empty? labels) (empty? non-labels) (not must-show-label))
        (element-content-DOM element specification)
        (let [inner-spec (-> specification
                             (dissoc :class)
                             (update :template
                                     #(add-elements-to-entity
                                       ;; This might come from a column header.
                                       (content %)
                                       ;; We have exactly the required labels.
                                       (map semantic-to-list
                                            (semantic-label-elements
                                             element)))))
              inner-dom (element-content-and-non-label-elements-DOM
                         element non-labels inner-spec)]
          (labels-wrapper-DOM
           inner-dom labels specification)))
      (add-attributes {:class "item"})))

(defn element-DOM
  "Render a dom spec given the immutable entity for an item (which may
  be an exemplar of a group of items)."
  [entity {:keys [excluded-element-ids] :as specification}]
  "Produce dom for an entity that is not a named object"
  (let [elements (remove
                  (set (map #(id->entity % (:store entity))
                            excluded-element-ids))
                  (semantic-elements entity))
        [labels non-labels] (separate-by label-element? elements)
        labels (cond->> labels
                 (:omit-universal-elements specification)
                 (remove #(universal-object? (content %))))]
    (cond-> (element-content-labels-and-non-label-elements-DOM
             entity labels non-labels
             (dissoc specification :class :omit-universal-elements))
      (:class specification)
      (add-attributes {:class (:class specification)}))))

(defn render-item-DOM-R
  "Render a dom spec for a store item (which may be an exemplar of a
  group of items). This is the default renderer."
  [{:keys [relative-id] :as specification}  store]
  (println "Generating DOM for" (simplify-for-print relative-id))
  (assert (:width specification)
          [specification
           (semantic-to-list (id->entity relative-id store))])
  (assert (not (:auxiliary-item-id specification))
          [specification
           (semantic-to-list (id->entity relative-id store))])
  (let-R [entity (id->updating-entity-R
                  (specification-item-id specification) store)]
    (cond (element? entity)
          (element-DOM entity specification)
          :else
          (assert false "Can only handle elements."))))

(defmethod print-method
  cosheet.server.item_render$render_item_DOM_R
  [v ^java.io.Writer w]
  (.write w "item-DOM"))

(defn horizontal-label-hierarchy-node-DOM
  "Generate the DOM for a node in a hierarchy that groups items by their
   labels, has at most one leaf per node and doesn't have both leaves and
   children.
   Don't generate or include the DOM for its children."
  [node {:keys [top-level] :as specification}]
  (let [specification (dissoc specification :top-level)
        example-elements (hierarchy-node-example-elements node)
        leaf-info (first (hierarchy-node-leaves node))
        leaf (:item leaf-info)
        labels (when leaf (semantic-label-elements leaf))
        non-labels (when leaf (semantic-non-label-elements leaf))
        leaf-component (when leaf
                         (let [ancestor-props
                               (clojure.set/difference
                                (set labels)
                                (set (hierarchy-node-example-elements node)))
                               ancestor-ids (map :item-id ancestor-props)]
                           (item-component
                            leaf
                            (cond-> (assoc specification :width 0.75)
                              (seq ancestor-ids)
                              (assoc :excluded-element-ids ancestor-ids)))))
        descendant-ids (map #(-> % :item :item-id)
                            (hierarchy-node-descendants node)) ]
    (cond
      (empty? (:properties node))
      ;; Since we don't add any properties, we must hold what would be
      ;; a leaf of a node that has children. We put a virtual cell
      ;; where our labels would go.
      (let [label-dom (cond-> (virtual-label-DOM-component
                               (assoc (add-parallel-item-ids specification
                                                             descendant-ids)
                                      :class ""
                                      :template 'anything
                                      :relative-id [(:item-id leaf) :nested]))
                        (not top-level)
                        (add-attributes {:class "merge-with-parent"}))]
        (assert leaf leaf)
        [:div {:class (cond-> "label wrapped-element virtual-wrapper"
                        (not top-level)
                        (str " merge-with-parent"))}
         label-dom
         [:div {:class "indent-wrapper label"} leaf-component]])
      
      (empty? (:child-nodes node))
      leaf-component
      
      true
      (do
        ;; Since the node has children, our input condition implies
        ;; that it must not have a leaf.
        (assert (not leaf) node)
        (label-stack-DOM
         example-elements
         (-> (add-parallel-item-ids-for-label specification descendant-ids)
             (assoc :template 'anything
                    :width (* 0.75 (count (hierarchy-node-descendants
                                           node))))))))))

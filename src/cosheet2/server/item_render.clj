(ns cosheet2.server.item-render
  (:require (cosheet2 [canonical :refer [canonical-set-to-list]]
                      [store :refer [StoredItemDescription]]
                      [entity :as entity :refer [label? description->entity
                                                 has-keyword?]]
                      [query :refer [matching-elements]]
                      [utils :refer [multiset-diff assoc-if-non-empty
                                     map-with-first-last
                                     add-elements-to-entity-list
                                     separate-by]]
                      [debug :refer [simplify-for-print]]
                      [hiccup-utils
                       :refer [dom-attributes into-attributes add-attributes]]
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
             [order-utils :refer [ordered-entities semantic-entity?]]
             [render-utils
              :refer [make-component
                      item-stack-DOM nest-if-multiple-DOM
                      condition-satisfiers
                      hierarchy-node-DOM
                      transform-specification-for-elements
                      transform-specification-for-labels
                      transform-specification-for-non-contained-labels]]
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

(defn ensure-label
  "Give the list form of an entity, add a :label keyword if it doesn't
   already have one."
  [template]
  (cond
    ;; The template for a virtual entity can be a vector. The last
    ;; element is the one that must be a label.
    (vector? template) (vec (concat (butlast template)
                                    [(ensure-label (last template))]))
    (has-keyword? template :label) template
    true (add-elements-to-entity-list template [:label])))

(defn get-item-rendering-data
  "Return the rendering data for a dom that presents an item.
   This is the default for :get-rendering-data."
  [specification mutable-store]
  (let [id (or (:item-id specification) (:relative-id specification))]
    (assert (satisfies? StoredItemDescription id) id)
    [[mutable-store [id]]]))

(defmethod print-method
  cosheet2.server.item_render$get_item_rendering_data
  [v ^java.io.Writer w]
  (.write w "item-RD"))

(defn get-virtual-DOM-rendering-data [spec store]
  [])

(defmethod print-method
  cosheet2.server.item_render$get_virtual_DOM_rendering_data
  [v ^java.io.Writer w]
  (.write w "virt-RD"))

(defn render-virtual-DOM [spec]
  [:div (into-attributes (select-keys spec [:class])
                         {:class "editable virtual"})])

(defmethod print-method
  cosheet2.server.item_render$render_virtual_DOM
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
  it will be run before get-virtual-action-data. The specification
  keys unique to the virtual action data
  are :template :sibling :position and :use-bigger."
  [specification]
  (assert (:template specification) specification)
  (make-component
   (-> specification 
       (assoc :render-dom render-virtual-DOM
              :get-rendering-data get-virtual-DOM-rendering-data)
       (dissoc :get-do-batch-edit-action-data)
       (update :get-action-data
               #(compose-action-data-getter % get-virtual-action-data)))))

(defn virtual-label-DOM-component
  "Return a dom for a virtual label. The label must occur inside an
  overall component for its element. The specification should be for
  elements of the item."
  [specification]
  (assert (:template specification))
  (virtual-DOM-component
   (-> specification
       (assoc :relative-id (or (:relative-id specification) :virtual-label)
              :position :after)       
       (update :template ensure-label)
       (into-attributes {:class "label"}))))

(defn virtual-entity-and-label-DOM
  "Return the dom for a virtual entity and a virtual label for it.
   The arguments are the same as for virtual-DOM-component."
  [specification orientation]
  (let [dom (virtual-DOM-component specification)
        template (:template specification)
        labels-dom (virtual-DOM-component
                    (assoc specification
                           :relative-id :virtual-label
                           :class "label"
                           :template [template '(anything :label)]))]
    (cond-> (wrap-with-labels-DOM labels-dom dom orientation)
      (:class specification)
      (add-attributes {:class (:class specification)}))))

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
  label that covers a DOM that refers to several items. The
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
        label-tags (map #(condition-satisfiers % '(nil :label))
                       ordered-labels)]
    (item-stack-DOM ordered-labels label-tags :vertical
                    (-> specification
                        (update :template ensure-label)
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

(defn labeled-items-properties-DOM
  "Given a hierarchy node for labels, Return DOM for example elements
  that give rise to the properties of the node, given a specification
  that applies to the overall node."
    [hierarchy-node specification]
  (let [descendant-items (map :item (hierarchy-node-descendants hierarchy-node))
        descendant-ids (map :item-id descendant-items)
        ;; Note: parallel-items-get-do-batch-edit-action-data assumes that
        ;;       we take the first of the descendants.
        example-descendant-id (first descendant-ids)
        labels-spec (transform-specification-for-non-contained-labels
                     specification)]
    (let [dom (if (empty? (:properties hierarchy-node))
                (virtual-DOM-component
                 ;; TODO: Track hierarchy depth in the spec, and use
                 ;; it to uniquify virtual labels.
                 (-> labels-spec
                     (assoc :relative-id [example-descendant-id
                                          :virtual-label])
                     (add-parallel-item-ids descendant-ids)
                     (update :template ensure-label)))
                (label-stack-DOM
                 (hierarchy-node-example-elements hierarchy-node)
                 (add-parallel-item-ids-for-label labels-spec descendant-ids)))]
      ;; Even if stacked, we need to mark the stack as "label" too.
      (add-attributes dom {:class "label"}))))

(defn hierarchy-leaf-items-DOM
  "Given a hierarchy node with labels as the properties, generate DOM
  for leaves that are items. The leaves of the node may contain an additional
  :exclude-elements field that gives more of the item's elements not
  to show, typically the ones that satisfy the :template of the
  specification. The specification should be the one for the overall
  hierarchy."
  [hierarchy-node specification]
  (assert (empty? (:excluded-element-ids specification)) specification)
  (let [leaves (hierarchy-node-leaves hierarchy-node)
        property-list (canonical-set-to-list
                       (:cumulative-properties hierarchy-node))
        leaf-spec (cond-> (dissoc specification :orientation)
                    (not (empty? property-list))
                    (update :template
                            #(add-elements-to-entity-list % property-list)))]
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
        (item-stack-DOM
         items excludeds :vertical leaf-spec)))))

(defn labeled-items-whole-hierarchy-node-DOM
  "Return the dom for everything at and under a labeled items hierarchy node.
  orientation gives which way to lay out the contained items.
  The specification must give :orientation."
  [node child-doms {:keys [must-show-label orientation] :as specification}]
  (assert (#{:horizontal :vertical} orientation) orientation)
  (assert (empty? (:excluded-element-ids specification)) specification)
  (let [leaves (hierarchy-node-leaves node)
        only-item (when (and (empty? child-doms) (= (count leaves) 1))
                    (:item (first leaves)))]
    (let [leaf-dom (when (seq leaves)
                     (hierarchy-leaf-items-DOM
                      node (dissoc specification :must-show-label)))
          descendants-dom (nest-if-multiple-DOM (if leaf-dom
                                                  (cons leaf-dom child-doms)
                                                  child-doms)
                                                orientation)
          properties-dom (when (or (seq (:properties node))
                                   must-show-label)
                           (labeled-items-properties-DOM
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

(defn labeled-items-for-horizontal-DOMs
  [hierarchy specification]
  (map #(hierarchy-node-DOM
         % labeled-items-whole-hierarchy-node-DOM
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

(defn one-column-of-two-column-DOMs
  [hierarchy node-fn wrapper-fn specification]
  (map (fn [node]
           (hierarchy-node-DOM
            node
            (fn [node child-doms specification]
              (let [dom (node-fn node specification)]
                (map-with-first-last
                 wrapper-fn
                 (cons dom (apply concat child-doms)))))
            specification))
         hierarchy))

(defn labeled-items-two-column-items-DOMs
  "Return the item doms for the node and all its children."
  [hierarchy specification]
  (one-column-of-two-column-DOMs
   hierarchy hierarchy-leaf-items-DOM horizontal-value-wrapper
   (update specification :width #(* % 0.6875))))

(defn labeled-items-two-column-label-DOMs
  "The specification should be the one for the items."
  [hierarchy specification]
  (one-column-of-two-column-DOMs
   hierarchy labeled-items-properties-DOM horizontal-label-wrapper
   (-> specification
       transform-specification-for-non-contained-labels
       (update :template ensure-label)
       (update :width #(* % 0.25)))))

(defn labeled-items-for-two-column-DOMs
  "The specification should apply to each item the hierarchy is over."
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
    (let [label-doms (labeled-items-two-column-label-DOMs
                      hierarchy specification)
          items-doms (labeled-items-two-column-items-DOMs
                       hierarchy specification)]
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

(defn labeled-items-for-one-column-DOMs
  "The specification should apply to each item the hierarchy is over."
  [hierarchy specification]
  (let [top-level-spec (assoc specification
                              :must-show-label (not (:immutable specification))
                              :orientation :vertical)
        child-specification-f (fn [_ specification]
                                (dissoc specification :must-show-label))]
    (map #(hierarchy-node-DOM %
                              labeled-items-whole-hierarchy-node-DOM
                              child-specification-f
                              top-level-spec)
         hierarchy)))

;;; The next two functions make stacks of components for entities.

(defn non-label-entities-DOM
  "Make a dom for a sequence of items, all of which must not be labels.
   If implied-template is non-nil, don't show elements implied by it.
   If must-show-label is true, show a space for labels, even if
   there are none. If, additionally, it is :wide, show them with substantial
   space, if there is significant space available."
  [entities implied-template must-show-label orientation specification]
  (let [ordered-entities (ordered-entities entities)
        all-labels (map semantic-label-elements ordered-entities)
        excludeds (map (if implied-template
                         #(condition-satisfiers % implied-template)
                         (constantly nil))
                       ordered-entities)]
    (let [labels (map (fn [all exclusions]
                        (clojure.set/difference (set all) (set exclusions)))
                      all-labels excludeds)
          no-labels (every? empty? labels)]
      (if (and no-labels (not must-show-label))
        (item-stack-DOM ordered-entities excludeds
                        orientation specification)
        (let [item-maps (item-maps-by-elements ordered-entities labels)
              augmented (map (fn [item-map excluded]
                               (assoc item-map :exclude-elements excluded))
                             item-maps excludeds)
              hierarchy (hierarchy-by-canonical-info augmented)
              doms (case orientation
                     :vertical
                     ((if (or (< (:width specification) 1.0)
                              (and no-labels (not (= must-show-label :wide))))
                        labeled-items-for-one-column-DOMs
                        labeled-items-for-two-column-DOMs)
                      hierarchy specification)
                     :horizontal
                     (labeled-items-for-horizontal-DOMs
                      (replace-hierarchy-leaves-by-nodes hierarchy)
                      specification))]
          (nest-if-multiple-DOM doms orientation))))))

(defn labels-and-elements-DOM
  "Generate the dom for a set of elements, some of which may be labels.
  virtual-dom, if present, will appear after the elements.
  elements-must-show-labels determines whether the elements must show labels.
  The specifications should be appropriate for each of the elements."
  [elements virtual-dom must-show-label elements-must-show-labels
   orientation specification]
  (assert (nil? (:relative-id specification)))
  (let [[labels non-labels] (separate-by label? elements)
        elements-dom
        (when (or non-labels virtual-dom)
          (let [elements-dom
                (when non-labels
                  (non-label-entities-DOM
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

;;; The next functions handle the parts of the dom for an entity.

(defn item-content-DOM
  "Make dom for the content part of an item."
  [item specification]
  ;; We don't currently handle content that itself non-trivial
  ;; entities. That would need more interaction and UI design work to
  ;; deal with the distinction between elements of an item and
  ;; elements on its content.
  (let [content (entity/content item)]
    (assert (entity/atom? content))
    (let [anything (= 'anything content)
          editable (not (:immutable specification))]
      [:div (cond-> (-> (select-keys specification [:class])
                        (into-attributes
                         {:class (cond-> "content-text"
                                   editable (str " editable")
                                   anything (str " placeholder"))}))
              (has-keyword? item :label)
              (into-attributes (:class "label"))
              anything
              (into-attributes (:class "placeholder")))
       (if anything "\u00A0..." (str content))])))

(defn render-content-only-DOM
  "Render a dom spec for only the content of an item."
  [{:keys [relative-id item-id class]} store]
  (assert (= relative-id :content))
  (item-content-DOM (description->entity item-id store)
                    (if class {:class class} {})))

(defmethod print-method
  cosheet2.server.item_render$render_content_only_DOM
  [v ^java.io.Writer w]
  (.write w "content-DOM"))

(defn item-content-and-non-label-elements-DOM
  "Make a dom for a content and a group of non-label elements."
  [item elements specification]
  (let [content-dom
        (make-component
         (cond-> (-> (select-keys specification
                                  [:template :class :width])
                     (assoc :relative-id :content
                            :item-id (:item-id item)
                            :render-dom render-content-only-DOM
                            :get-action-data get-pass-through-action-data))
           (has-keyword? item :label)
           (into-attributes {:class "label"})))]
      (if (empty? elements)
        content-dom
        (let [elements-spec (transform-specification-for-elements specification)
              elements-dom (non-label-entities-DOM
                            elements nil
                            (or (:must-show-label specification) true)
                            :vertical elements-spec)]
          [:div {:class (cond-> "with-elements"
                          (has-keyword? item :label)
                          (str " label"))}
           content-dom elements-dom]))))

(defn item-content-labels-and-non-label-elements-DOM
  [entity labels non-labels {:keys [must-show-label] :as specification}]
  (-> (if (and (empty? labels) (empty? non-labels) (not must-show-label))
        (item-content-DOM entity specification)
        (let [inner-spec (-> specification
                             (dissoc :class)
                             (update :template
                              #(add-elements-to-entity-list
                                % (map semantic-to-list
                                       (semantic-label-elements entity)))))
              inner-dom (item-content-and-non-label-elements-DOM
                         entity non-labels inner-spec)]
          (labels-wrapper-DOM
           inner-dom labels specification)))
      (add-attributes {:class "item"})))

(defn render-item-DOM
  "Render a dom spec for an item (which may be an exemplar of a
  group of items). This is the default renderer."
  [{:keys [relative-id item-id must-show-label excluded-element-ids]
    :as specification}
   store]
  (println "Generating DOM for" (simplify-for-print relative-id))
  (assert (:width specification)
          [specification
           (semantic-to-list (description->entity relative-id store))])
  (let [entity (description->entity (or item-id relative-id) store)
        elements (remove (set (map #(description->entity % store)
                                   excluded-element-ids))
                         (semantic-elements entity))
        [labels non-labels] (separate-by label? elements)]
    (cond-> (item-content-labels-and-non-label-elements-DOM
             entity labels non-labels (dissoc specification :class))
      (:class specification)
      (add-attributes {:class (:class specification)}))))

(defmethod print-method
  cosheet2.server.item_render$render_item_DOM
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
                           (make-component
                            (cond-> (assoc specification
                                           :relative-id (:item-id leaf)
                                           :width 0.75)
                              (seq ancestor-ids)
                              (assoc :excluded-element-ids ancestor-ids)))))
        descendant-ids (map #(-> % :item :item-id)
                            (hierarchy-node-descendants node)) ]
    (if (empty? (:properties node))
      ;; We must be a leaf of a node that has children. We put a virtual
      ;; cell where our labels would go.
      (let [label-dom (cond-> (virtual-DOM-component
                               (assoc (add-parallel-item-ids specification
                                                             descendant-ids)
                                      :class "label"
                                      :template '(anything :label)
                                      :relative-id [(:item-id leaf) :nested]))
                        (not top-level)
                        (add-attributes {:class "merge-with-parent"}))]
        (assert leaf leaf)
        [:div {:class (cond-> "label wrapped-element virtual-wrapper"
                        (not top-level)
                        (str " merge-with-parent"))}
         label-dom
         [:div {:class "indent-wrapper label"} leaf-component]])
      (if (empty? (:child-nodes node))
        leaf-component
        (do
          ;; Since the node has children, our input condition implies
          ;; that it must not have a leaf.
          (assert (not leaf) node)
          (label-stack-DOM
           example-elements
           (-> (add-parallel-item-ids-for-label specification descendant-ids)
               (assoc :template '(anything :label)
                      :width (* 0.75 (count (hierarchy-node-descendants
                                             node)))))))))))

(ns cosheet.server.item-render
  (:require
   (cosheet
    [canonical :refer [canonical-set-to-list canonicalize]]
    [store :refer [make-item-id]]
    [entity :refer [id->entity id->updating-entity-R
                    content label-element? primitive? object? element?
                    interned-object? universal-object?
                    label-object? name-element? target-entity
                    elements label->elements content->elements
                    name-label link-type object-type
                    all-presumed-interned-in-different-store
                    entity-complexity
                    add-elements-to-entity]]
    [query :refer [matching-elements]]
    [utils :refer [multiset-diff assoc-if-non-empty
                   map-with-first-last
                   separate-by]]
    [debug :refer [simplify-for-print]]
    [calculator :refer [current-value]]
    [hiccup-utils :refer [dom-attributes into-attributes add-attributes
                          merge-classes]]
    [reporter-macros :refer [app-R let-R]])
   (cosheet.server
    [model-utils :refer [semantic-elements
                         semantic-non-label-elements semantic-label-elements
                         semantic-to-tree semantic-to-tree-excluding-elements
                         entity->canonical-semantic]]
    [hierarchy :refer [replace-hierarchy-leaves-by-nodes
                       hierarchy-node-descendants
                       hierarchy-node-leaves
                       hierarchy-node-logical-leaves
                       hierarchy-by-canonical-info
                       item-maps-by-elements
                       hierarchy-node-example-elements]]
    [order-utils :refer [ordered-entities]]
    [render-utils :refer [make-sequential-template
                          display-type
                          ensure-label-object ensure-label-object-content
                          replace-final-label-content
                          make-component
                          nest-if-multiple-DOM
                          condition-satisfiers
                          hierarchy-node-DOM
                          final-template
                          inherited-specification-keys
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

(def render-item-DOM-R)

(def css-class-for-name)

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
  (assert (empty? (:element-ids-to-exclude specification))
          [excluded-elements (:element-ids-to-exclude specification)])
  (assert (not (:get-action-data specification))
          (:get-action-data specification))
  (let [new-spec (cond-> specification
                   (seq excluded-elements)
                   (assoc :element-ids-to-exclude
                          (set (map :item-id excluded-elements))))]
    (item-component item new-spec)))

(defn item-stack-DOM
  "Given a list of items and a matching list of their elements to exclude,
  generate components for each item, and put them in a DOM.  If there
  is more than one item, make the stack in the given orientation."
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
  (let [template (final-template (:template spec))]
    [:div (cond-> (into-attributes (select-keys spec [:class])
                                   {:class "editable virtual"})
            (object? template)
            (into-attributes {:class (css-class-for-name template)}))]))

(defmethod print-method
  cosheet.server.item_render$render_virtual_DOM
  [v ^java.io.Writer w]
  (.write w "virt-DOM"))

(defn add-labels-DOM
  "Add a labels dom to an inner dom. Orientation gives the orientation
  between the doms."
    [labels-dom inner-dom orientation]
  [:div {:class (case orientation
                  :vertical "vertical-labels-element link-type"
                  :horizontal "horizontal-labels-element")}
   labels-dom inner-dom])

(defn wrap-with-labels-DOM
  "Wrap the inner dom so it appears to be surrounded by the labels-dom.
  If the orientation is :vertical, indent the inner dom."
  [labels-dom label-type inner-dom orientation]
  (assert (#{:link-type :object-type} label-type))
  (if (= orientation :vertical)
    [:div {:class (str "label-wrapping-elements " (name label-type))}
     labels-dom
     [:div {:class "wrapped-elements"}
      inner-dom]]
    (add-labels-DOM labels-dom inner-dom orientation)))

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
       (assoc :render-dom render-virtual-DOM
              :virtual true)
       (dissoc :get-do-batch-edit-action-data)
       (update :get-action-data
               #(compose-action-data-getter % get-virtual-action-data)))))

(defn virtual-label-DOM-component
  "Return a dom for a virtual label. The dom must appear inside an
  overall dom for the item it modifies. The specification's template
  must describe a label element, although it need not specify a name.

  We don't use relative-id, but our callers must provide it if there
  are multiple virtual labels under the same component, so they will
  have different client ids.
  
  A key thing we do is replace the content of the element in the
  template with the empty string, and record the actual template for
  the label object in :virtual-object-reference-template. This is
  necessary because the code for virtual action data doesn't get the
  name information, so it can't look-up or create the right object;
  any object it made would be thrown away, anyway. So we have it make
  an empty string as a placeholder. Then do-set-object, which does get
  the object's name, will then be able to find or make the right kind
  of object."
  [{:keys [relative-id template] :as specification}]
  (assert template specification)
  (let [final (final-template template)]
    (assert (label-element? final) final)
    (let [label-type (display-type (content final))]      
      (virtual-DOM-component
       (-> specification
           (assoc :relative-id (or relative-id :virtual-label)
                  :position :after
                  :template (replace-final-label-content template "")
                  :virtual-object-reference-template
                  (ensure-label-object
                   (content (final-template template)) label-type)
                  :is-object-name true)
           (into-attributes {:class (name label-type)}))))))

(defn virtual-element-and-label-DOM
  "Return the dom for a virtual element and a virtual label for it.
   The arguments are the same as for virtual-DOM-component."
  [{:keys [template class] :as specification} orientation]
  (let [dom (virtual-DOM-component specification)
        label-type (if (object? (final-template template))
                     :object-type
                     :link-type)
        labels-dom (virtual-label-DOM-component
                    (-> specification
                        (dissoc :relative-id)
                        (assoc :template
                               (make-sequential-template
                                template `(~(ensure-label-object
                                              'anything label-type))))))]
    (cond-> (wrap-with-labels-DOM labels-dom :link-type dom orientation)
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
        label-type (display-type (content (first label-elements)))]
    (item-stack-DOM ordered-labels
                    (map (constantly '()) ordered-labels)
                    :vertical
                    (-> specification
                        (update :template #(ensure-label-object-content
                                            % label-type))
                        (into-attributes {:class (name label-type)})))))

(defn non-empty-labels-wrapper-DOM
  "Given a dom for an item, not including its labels, and a non-empty 
  list of labels, make a dom that includes the labels wrapping the item."
  [inner-dom label-elements orientation specification]
  (let [stack (label-stack-DOM label-elements specification)
        label-type (if (object? (:template specification))
                     :object-type
                     :link-type)]
    (wrap-with-labels-DOM stack label-type inner-dom orientation)))

(defn labels-wrapper-DOM
  "Given a dom for an item, not including its labels, and a list of labels,
  make a dom that includes any necessary labels wrapping the item.
  specification should be the one for the item." 
  [dom label-elements specification]
  (add-attributes
   (if (and (empty? label-elements) (not (:must-show-label specification)))
     dom
     (let [labels-spec (transform-specification-for-labels
                        specification :link-type)]
       (if (not (empty? label-elements))
         (non-empty-labels-wrapper-DOM
          dom label-elements :vertical labels-spec)
         [:div {:class "horizontal-labels-element virtual-wrapper narrow"}
          (virtual-label-DOM-component labels-spec)
          dom])))
    (select-keys specification [:class])))

(defn displayable-example-labels
  "The example label elements of a hierarchy node that should be
  displayed: its example elements, minus any whose content is a
  universal object. Universal-object labels take part in building the
  hierarchy, so that a leaf's items agree on them, but are not shown."
  [hierarchy-node]
  (remove #(universal-object? (content %))
          (hierarchy-node-example-elements hierarchy-node)))

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
                     specification :link-type)
        display-labels (displayable-example-labels hierarchy-node)]
    (let [dom (if (empty? display-labels)
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
                 display-labels
                 (add-parallel-item-ids-for-label labels-spec descendant-ids)))]
      ;; Even if stacked, we need to mark the stack as "link-type" too.
      (add-attributes dom {:class "link-type"}))))

(defn hierarchy-leaf-elements-DOM
  "Given a node of a hierarchy of entity info maps for a sequence of
  elements organized by their labels, generate DOM for its leaf
  elements, or a virtual DOM if there are no leaves. The leaves of the
  node may contain an additional :exclude-elements field that gives
  more of the item's elements not to show, typically the ones that
  satisfy the :template of the specification. The specification should
  be the one for the overall hierarchy."
  [hierarchy-node specification]
  (assert (empty? (:element-ids-to-exclude specification)) specification)
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
  (assert (empty? (:element-ids-to-exclude specification)) specification)
  (let [leaves (hierarchy-node-leaves node)
        display-labels (displayable-example-labels node)
        only-item (when (and (empty? child-doms) (= (count leaves) 1))
                    (:item (first leaves)))]
    (let [leaf-dom (when (seq leaves)
                     (hierarchy-leaf-elements-DOM
                      node (dissoc specification :must-show-label)))
          descendants-dom (nest-if-multiple-DOM (if leaf-dom
                                                  (cons leaf-dom child-doms)
                                                  child-doms)
                                                orientation)
          properties-dom (when (or (seq display-labels)
                                   must-show-label)
                           (hierarchical-elements-property-elements-DOM
                            node specification))]
      (cond-> (if (empty? display-labels)
                (if must-show-label
                  (cond-> (add-labels-DOM properties-dom descendants-dom
                                          (opposite-orientation orientation))
                    true
                    (add-attributes {:class "virtual-wrapper"})
                    (= orientation :vertical)
                    (add-attributes {:class "narrow"}))
                  descendants-dom)
                (wrap-with-labels-DOM properties-dom :link-type
                                      descendants-dom :vertical))
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
  [:div {:class (cond-> "link-type horizontal-header"
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
                      specification :link-type)
          label-doms (one-column-of-hierarchy-two-column-DOM
                      hierarchy
                      hierarchical-elements-property-elements-DOM
                      horizontal-label-wrapper
                      label-spec 0.25)          
          items-doms (one-column-of-hierarchy-two-column-DOM
                      hierarchy
                      hierarchy-leaf-elements-DOM
                      horizontal-value-wrapper
                      specification 0.6875)]
      (map
       (fn [label-dom items-dom only-item]
         (cond-> [:div {:class "horizontal-labels-element link-type wide"}
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

(defn non-label-element-group-doms
  "Return a seq of doms for a sequence of non-label elements, all treated
  the same. This is the core used by non-label-elements-DOM. It never
  shows the elements' name labels. See non-label-elements-DOM for the
  other arguments."
  [elements implied-template must-show-label orientation specification]
  (let [ordered-elements (ordered-entities elements)
        ;; We keep universal-object labels here, so that the hierarchy
        ;; groups items that differ on them into different leaves. They
        ;; are dropped when the hierarchy's labels are displayed.
        all-labels (map semantic-label-elements ordered-elements)
        excludeds (map (fn [element]
                         (when implied-template
                           (condition-satisfiers
                            element implied-template)))
                       ordered-elements)
        labels (map (fn [all exclusions]
                      (clojure.set/difference (set all) (set exclusions)))
                    all-labels excludeds)
        ;; Universal-object labels aren't displayed, so whether there are
        ;; labels to show depends only on the other labels.
        no-labels (every? (fn [ls]
                            (every? #(universal-object? (content %)) ls))
                          labels)]
    (if (and no-labels (not must-show-label))
      (map #(item-minus-excluded-component %1 %2 specification)
           ordered-elements excludeds)
      (let [item-maps (item-maps-by-elements ordered-elements labels)
            augmented (map (fn [item-map excluded]
                             (assoc item-map :exclude-elements excluded))
                           item-maps excludeds)
            hierarchy (hierarchy-by-canonical-info augmented)]
        (case orientation
          :vertical
          ((if (or (< (:width specification) 1.0)
                   (and no-labels (not (= must-show-label :wide))))
             hierarchical-elements-in-one-column-DOM
             hierarchical-elements-in-two-column-DOM)
           hierarchy specification)
          :horizontal
          (hierarchical-elements-in-horizontal-DOM
           (replace-hierarchy-leaves-by-nodes hierarchy)
           specification))))))

(defn non-label-elements-DOM
  "Make a dom for a sequence of elements, all of which must not be labels.
  Elements that are names are put first, with their name labels hidden
  but with css class \"name\". The name and non-name elements and the
  virtual-dom all go in a single stack, so the virtual-dom can expand to
  fill the available area.
  If implied-template is non-nil, don't show the elements' labels implied
  by it.  If must-show-label is true, show a virtual dom for labels where
  there are none. If, additionally, it is :wide, show them with
  substantial space, if there is significant space available.
  If virtual-dom is present, append it after the elements' doms."
  [elements implied-template must-show-label virtual-dom orientation
   specification]
  (let [[names non-names] (separate-by name-element? elements)
        names-doms (when (seq names)
                     (non-label-element-group-doms
                      names implied-template must-show-label orientation
                      (into-attributes specification {:class "name"})))
        non-names-doms (when (seq non-names)
                         (non-label-element-group-doms
                          non-names implied-template must-show-label
                          orientation specification))]
    (nest-if-multiple-DOM
     (concat names-doms non-names-doms (when virtual-dom [virtual-dom]))
     orientation)))

;;; This function is only called from outside item-render, typically
;;; for conditions.
(defn labels-and-elements-DOM
  "Generate the dom for a set of elements, some of which may be
  labels. virtual-dom will appear after any elements, and it must be
  present if there are no elements.
  elements-must-show-labels determines whether the elements must show
  labels of their own.  The specification should be appropriate for
  each of the elements."
  [elements virtual-dom must-show-label elements-must-show-labels
   orientation specification label-type]
  (assert (not (:relative-id specification))
          (:relative-id specification))
  (assert (or (seq elements) virtual-dom))
  (let [[labels non-labels] (separate-by label-element? elements)
        labels (seq (remove #(universal-object? (content %)) labels))
        elements-dom
        (when (or non-labels virtual-dom)
          (let [elements-dom
                (when non-labels
                  (non-label-elements-DOM
                   non-labels nil elements-must-show-labels
                   nil orientation specification))]
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
       (virtual-label-DOM-component
        (transform-specification-for-labels specification label-type))
       label-type
       elements-dom orientation)

      true ; The logic guarantees there will be elements-dom if there
           ; are no labels.
      elements-dom)))

;;; The next functions handle the parts of the dom for an element

(defn css-class-for-name
  "Return the class to use in formatting the name of this object"
  [object]
  (cond (seq (content->elements object link-type)) "name link-type"
        (seq (content->elements object object-type)) "name object-type"
        true "name"))

(defn element-primitive-content-DOM
  "Make dom for a primitive that is the content part of an item."
  [item primitive {:keys [class]}]
  (assert (primitive? primitive) primitive)
  (let [anything (= 'anything primitive)]
    [:div (cond-> (into-attributes
                   {:class class}
                   {:class (cond-> "content-text"
                             anything (str " placeholder"))})
            (label-element? item)
            (into-attributes (:class "link-type"))
            (name-element? item)
            ;; Note: we won't be re-run when the elements target
            ;; entity change. But that's OK because we only depend on
            ;; its types links, and those aren't allowed to change.
            (into-attributes {:class (css-class-for-name
                                      (target-entity item))})
            anything
            (into-attributes (:class "placeholder")))
     (if anything "\u00A0..." (str primitive))]))

(defn render-object-reference-DOM-R
  "Produce dom for an object reference. This means that we just show its
  name, and editing the displayed name doesn't change the object, but
  selects (or creates) an object with the provided name. The
  specification should have a :relative-id of :content, and an
  target-item-id that gives the id of an example object."
  [{:keys [target-item-id relative-id] :as specification} store]
  (assert (= relative-id :content))
  (let-R [object (id->updating-entity-R target-item-id store)]
    (let [names (-> (label->elements object name-label)
                    ordered-entities)
          num-names (count names)
          specification (->
                         specification
                         (dissoc :target-item-id :relative-id :render-dom)
                         (assoc :is-object-name true
                                :get-action-data get-pass-through-action-data))]
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
  ;; Our specification should be the element that holds the object.
  (assert (let [template (:template specification)]
            (or (= template 'anything) (element? template))))
  (make-component (assoc specification
                         :relative-id :content
                         :target-item-id (:item-id object)
                         :render-dom render-object-reference-DOM-R
                         :get-action-data get-pass-through-action-data)))

(defn display-content-object-as-if-interned?
  "Return true if an object that is being shown as a content, and with
  the given template, should be shown as if it were interned object,
  even though it has a generic name. We return true if the object
  matches the template, with nothing extra. These objects can result
  from add-twin on an interned object, and we want to show them
  in the same format as their twin."
  [template-element element]
  (let [template (content template-element)
        object (content element)]
    (and (seq (label->elements object name-label))
         ;; The template and object might mention identified objects
         ;; with the same id, but from different stores. That shouldn't
         ;; count as a difference. We also exclude the element itself,
         ;; which appears among the object's elements as a reversed link.
         (= (canonicalize
             (semantic-to-tree-excluding-elements
              object #{(:item-id element)}))
            (entity->canonical-semantic
             (all-presumed-interned-in-different-store template nil))))))

(defn element-content-DOM
  "Make dom for the content of an element."
  [element {:keys [immutable template] :as specification}]
  (let [contents (content element)
        reference-contents (and (object? contents)
                                (or (interned-object? contents)
                                    (display-content-object-as-if-interned?
                                     template element)))
        ;; Only directly-editable (primitive) content is editable here.
        ;; Entire object contents are not. (Their parts can be.)
        editable (and (not immutable)
                      (not (object? contents)))
        kept-spec (select-keys specification
                               (concat [:class :template]
                                       inherited-specification-keys))
        specification (cond-> kept-spec
                        editable (into-attributes {:class "editable"}))]
    (cond (primitive? contents)
          (element-primitive-content-DOM element contents specification)
          reference-contents
          (object-reference-component contents specification)
          true
          (do (assert (object? contents))
              (item-component
               contents
               (-> specification
                   (assoc :template (content template)
                          :element-ids-to-exclude #{(:item-id element)})
                   (into-attributes {:class "object"})))))))

(defn render-element-content-DOM-R
  "Given an item that represents an element, render a dom for
  its content."
  [{:keys [relative-id target-item-id] :as specification} store]
  (assert (= relative-id :content) relative-id)
  (let-R [element (id->updating-entity-R target-item-id store)]
    (element-content-DOM
     element (select-keys specification [:class :width :immutable :template]))))

(defmethod print-method
  cosheet.server.item_render$render_element_content_DOM_R
  [v ^java.io.Writer w]
  (.write w "content-DOM"))

(defn element-content-and-non-label-elements-DOM
  "Make a DOM for a content and a group of non-label elements."
  [element elements specification]
  (let [content-dom
        (make-component
         (cond-> (-> (select-keys specification
                                  (concat [:template :class]
                                          inherited-specification-keys))
                     (assoc :relative-id :content
                            :target-item-id (:item-id element)
                            :render-dom render-element-content-DOM-R
                            :get-action-data get-pass-through-action-data))
           (label-element? element)
           (into-attributes {:class "link-type"})))]
      (if (empty? elements)
        content-dom
        (let [elements-spec (transform-specification-for-elements specification)
              elements-dom (non-label-elements-DOM
                            elements nil
                            (or (:must-show-label specification) true)
                            nil :vertical elements-spec)]
          [:div {:class (cond-> "with-elements"
                          (label-element? element)
                          (merge-classes "link-type"))}
           content-dom elements-dom]))))

(defn element-content-labels-and-non-label-elements-DOM
  "Given an element, its labels, and its non-label elements, and it dom
  specification, generate its dom."
  [element labels non-labels {:keys [must-show-label] :as specification}]
  (-> (if (and (empty? labels) (empty? non-labels) (not must-show-label))
        (element-content-DOM element specification)
        (let [inner-spec (-> specification
                             (dissoc :class)
                             ;; The template for making a copy inside our
                             ;; labels is the content of the original
                             ;; template, plus all our labels.
                             (update :template
                                     #(add-elements-to-entity
                                       ;; This might come from a column header.
                                       `(~(content %))
                                       ;; We have exactly the required labels.
                                       (map semantic-to-tree
                                            (semantic-label-elements
                                             element)))))
              inner-dom (element-content-and-non-label-elements-DOM
                         element non-labels inner-spec)]
          (labels-wrapper-DOM
           inner-dom labels specification)))
      (add-attributes {:class "element"})))

(defn element-DOM
  "Render a dom spec given the immutable entity for an item (which may
  be an exemplar of a group of items)."
  [entity {:keys [element-ids-to-exclude] :as specification}]
  (let [elements (cond->> (semantic-elements entity)
                   element-ids-to-exclude
                   (remove #(element-ids-to-exclude (:item-id %))))
        [labels non-labels] (separate-by label-element? elements)
        labels (seq (remove #(universal-object? (content %)) labels))]
    (cond-> (element-content-labels-and-non-label-elements-DOM
             entity labels non-labels
             (dissoc specification :class))
      (:class specification)
      (add-attributes {:class (:class specification)}))))

(defn object-DOM
  "Render a dom for an object. Shows labels (classes) wrapping names,
  then other elements. Labels are indented to the right."
  [entity {:keys [template element-ids-to-exclude object-ids-to-contract]
           :as specification}]
  (let [entity-id (:item-id entity)
        contract (contains? object-ids-to-contract entity-id)
        elements (cond->> (semantic-elements entity)
                   element-ids-to-exclude
                   (remove #(element-ids-to-exclude (:item-id %))))
        [labels non-labels] (separate-by label-element? elements)
        ;; When contracting, also hide any label that itself has
        ;; other elements.
        labels (seq (cond->> (remove #(universal-object? (content %)) labels)
                      contract (remove #(seq (semantic-elements %)))))
        [names others] (separate-by name-element? non-labels)
        elem-spec (update (transform-specification-for-elements specification)
                          :object-ids-to-contract (fnil conj #{}) entity-id)
        ;; The name is shown even when contracting; the other elements
        ;; are shown only when not contracting. We give the name the
        ;; "name" class and exclude its name label from what is shown.
        names-dom (non-label-elements-DOM
                   names `(~'anything (~name-label))
                   (boolean (and (not contract) (seq others)))
                   nil :vertical (assoc elem-spec :class "name"))
        inner-dom (if contract
                    names-dom
                    (let [others-dom
                          (if (seq others)
                            (non-label-elements-DOM
                             others template true nil :vertical elem-spec)
                            (virtual-DOM-component
                             (assoc elem-spec :relative-id :virtual)))]
                      (nest-if-multiple-DOM [names-dom others-dom] :vertical)))
        labels-spec (transform-specification-for-labels
                     specification :object-type)]
    (cond-> (wrap-with-labels-DOM
             (if labels
               (label-stack-DOM labels labels-spec)
               (virtual-label-DOM-component labels-spec))
             :object-type
             inner-dom
             :vertical)
      (:class specification)
      (add-attributes {:class (:class specification)}))))

(defn render-item-DOM-R
  "Render a dom spec for a store item (which may be an exemplar of a
  group of items). This is the default renderer."
  [{:keys [relative-id target-item-id width] :as specification} store]
  (println "Generating item DOM for" (simplify-for-print relative-id))
  (let [updating-entity (id->updating-entity-R relative-id store)]
     (assert (and width (not target-item-id))
             [specification
              (semantic-to-tree (current-value updating-entity))])
     (let-R [entity updating-entity]
       (cond (element? entity)
             (element-DOM entity specification)
             (object? entity)
             (object-DOM entity specification)
             :else
             (assert false "Can only handle elements and objects.")))))

(defmethod print-method
  cosheet.server.item_render$render_item_DOM_R
  [v ^java.io.Writer w]
  (.write w "item-DOM"))

(defn hierarchy-node-labels-DOM
  "Generate the DOM for a node in a hierarchy that groups items by their
  labels, has at most one leaf per node and doesn't have both leaves and
  children.
  Don't generate or include the DOM for its children.
  This is used by column headers and the like, where the DOM for child
  nodes won't be nested inside the DOM of their parents."
  [node {:keys [top-level] :as specification}]
  (let [specification (dissoc specification :top-level)
        leaf-info (first (hierarchy-node-leaves node))
        leaf (:item leaf-info)
        labels (when leaf (semantic-label-elements leaf))
        leaf-component (when leaf
                         (let [ancestor-props
                               (clojure.set/difference
                                (set labels)
                                (set (hierarchy-node-example-elements node)))
                               ancestor-ids (set (map :item-id ancestor-props))]
                           (item-component
                            leaf
                            (cond-> (assoc specification :width 0.75)
                              (seq ancestor-ids)
                              (assoc :element-ids-to-exclude ancestor-ids)))))
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
                                      :template `(~(ensure-label-object
                                                    'anything :link-type))
                                      :relative-id [(:item-id leaf) :nested]))
                        (not top-level)
                        (add-attributes {:class "merge-with-parent"}))]
        (assert leaf leaf)
        [:div {:class (cond-> "link-type label-wrapping-elements virtual-wrapper"
                        (not top-level)
                        (str " merge-with-parent"))}
         label-dom
         [:div {:class "wrapped-elements link-type"} leaf-component]])
      
      (empty? (:child-nodes node))
      leaf-component
      
      true
      (let [display-labels (displayable-example-labels node)]
        ;; Since the node has children, our input condition implies
        ;; that it must not have a leaf.
        (assert (not leaf) node)
        (if (seq display-labels)
          (label-stack-DOM
           display-labels
           (-> (add-parallel-item-ids-for-label specification descendant-ids)
               (assoc :template 'anything
                      :width (* 0.75 (count (hierarchy-node-descendants
                                             node))))))
          ;; All the node's labels are universal objects, which are used
          ;; only to build the hierarchy and are not shown; put a virtual
          ;; label where they would go.
          (virtual-label-DOM-component
           (-> (add-parallel-item-ids specification descendant-ids)
               (assoc :template `(~(ensure-label-object 'anything :link-type))
                      :relative-id [(first descendant-ids) :nested]
                      :width (* 0.75 (count (hierarchy-node-descendants
                                             node)))))))))))

(ns cosheet2.server.tabs-render
  (:require (cosheet2 [entity :refer [description->entity label->elements]]
                      [debug :refer [simplify-for-print]]
                      [hiccup-utils :refer [dom-attributes
                                            into-attributes add-attributes]]
                      [expression :refer [expr expr-let expr-seq cache]]
                      [canonical :refer [canonicalize-list canonical-to-list
                                         canonical-set-to-list
                                         common-canonical-multisets]])
            (cosheet2.server
             [hierarchy :refer [hierarchy-by-all-elements
                                hierarchy-node?
                                hierarchy-node-descendants
                                hierarchy-node-next-level
                                hierarchy-node-example-elements
                                replace-hierarchy-leaves-by-nodes]]
             [order-utils :refer [ordered-entities]]
             [model-utils :refer [semantic-elements
                                  new-tab-elements]]
             [render-utils :refer [hierarchy-node-DOM make-component]]
             [item-render :refer [virtual-DOM-component
                                  labels-and-elements-DOM]]
             [action-data :refer [get-item-or-exemplar-action-data-for-ids
                                  get-tab-action-data]])))

(def base-tab-width 150)

(defn get-tab-elements-rendering-data
  [specification mutable-store]
  [mutable-store [(:example-element-ids specification)]])

(defmethod print-method
  cosheet2.server.tabs_render$get_tab_elements_rendering_data
  [v ^java.io.Writer w]
  (.write w "tab-RD"))

(defn render-tab-elements-DOM
  [specification immutable-store]
  "Generate the dom for a node of the tabs hierarchy, but not any of
  its children. The specification must already have get-action-data
  that targets each of the tab items, and get-tab-action-data if there
  is only one tab item."
  (let [{:keys [example-element-ids]} specification
        example-elements (map #(description->entity % immutable-store)
                              example-element-ids)]
    (if (seq example-element-ids)
      (let [dom (labels-and-elements-DOM
                 example-elements nil false false :vertical {})
            elements-elements (map semantic-elements example-elements)]
        (cond-> dom
          (not (empty? (apply concat elements-elements)))
          (add-attributes {:class "complex"})))        
      (add-attributes
       (virtual-DOM-component {:tamplate 'any})
       {:class "empty-child"}))))

(defmethod print-method
  cosheet2.server.tabs_render$render_tab_elements_DOM
  [v ^java.io.Writer w]
  (.write w "tab-DOM"))

(defn tab-elements-DOM-component
  [node specification]
  (let [{:keys [template nesting-depth]} specification 
        tab-ids (map #(:item-id (:item %)) (hierarchy-node-descendants node))
        example-element-ids (map :item-id
                                 (hierarchy-node-example-elements node))
        relative-id (cond-> (first tab-ids)
                      (> nesting-depth 0) (vector (str nesting-depth)))]
    (make-component
     (cond->
         {:relative-id relative-id
          :template template
          :render-dom render-tab-elements-DOM
          :get-rendering-data get-tab-elements-rendering-data
          :example-element-ids example-element-ids}
       (= (count tab-ids) 1)
       (assoc :get-tab-action-data
              [get-tab-action-data (first tab-ids)])
       (not= [relative-id] tab-ids)
       (assoc :get-action-data [get-item-or-exemplar-action-data-for-ids
                                tab-ids])))))

(defn tabs-child-info
  "Adjust the specification for child tabs."
  [node specification]
  (-> specification
      (update :template
              #(list* (concat % (canonical-set-to-list (:properties node)))))
      ;; The same tab item is the basis for the relative id of a node
      ;; and all its "first born" descendants. And all those nodes
      ;; are logically inside the same overall component. We add a
      ;; suffix to make their keys distinct.
      (update :nesting-depth inc)))

(defn tabs-node-DOM
  "In addition to :template, the specification must have :is-chosen-tab,
   which is true if the tab is the currently chosen tab, or is a
   parent of that tab."
  [node child-doms specification]
  (let [elements-dom (tab-elements-DOM-component node specification)
        dom (if (empty? child-doms)
              (add-attributes elements-dom {:class "tab"})
              [:div {:class "tab-tree"}
               (add-attributes elements-dom {:class "multi-tab"})
               (into [:div {:class "tab-sequence"}]
                     ;; We list the tabs in reverse order, so the
                     ;; logically first tab will have priority in
                     ;; the stacking order.  Then in the style, we
                     ;; say to lay them out in reverse row order.
                     (reverse child-doms))])]
    (cond-> dom
      (:is-chosen-tab specification)
      (add-attributes {:class "chosen"}))))

(defn virtual-tab-DOM
  [{:keys [template first-tab-id]}]
  (virtual-DOM-component
   {:relative-id :virtual-tab
    :class "tab virtualTab"}
   {:template template
    :sibling first-tab-id
    :position :before
    :use-bigger true}))

(defn get-tabs-rendering-data
  [specification mutable-store]
  (let [client-state (:client-state specification)]
    [[mutable-store [(:relative-id specification)]]
     [client-state [(:root-id client-state)]]]))

(defn render-tabs-DOM
  "Return a reporter giving the DOM for the elements of the given
  item as tabs."
  [specification immutable-store immutable-client-state]
  (let [{:keys [relative-id]} specification
        tabs-entity (description->entity relative-id immutable-store)
        chosen-tab-id (:root-id immutable-client-state)
        tabs-spec {:template (cons "" (cons "" new-tab-elements))
                   :nesting-depth 0}
        tabs (ordered-entities (label->elements tabs-entity :tab))
        hierarchy (hierarchy-by-all-elements tabs)
        hierarchy (replace-hierarchy-leaves-by-nodes hierarchy)
        tab-doms (map (fn [node]
                        (hierarchy-node-DOM
                         node tabs-node-DOM tabs-child-info
                         (let [chosen-one
                               (seq (filter #(= (:item %) chosen-tab-id)
                                            (hierarchy-node-descendants node)))]
                           (cond-> tabs-spec
                             chosen-one (assoc :is-chosen-tab true)))))
                      hierarchy)
        virtual-tab-dom (virtual-tab-DOM (assoc tabs-spec :first-tab-id
                                                (:item-id (first tabs))))]
      [:div {:class "tabs-wrapper"}
       [:div#batch-edit.tool
        [:img {:src "../icons/edit.gif"}]
        [:div.tooltip "batch edit (C-B)"]]
       [:div.toolgap]
       (into [:div {:class "tabs-holder"}]
             ;; We list the tabs in reverse order, so the logically first
             ;; tab will have priority in the stacking order.
             ;; Then in the style, we say to lay them out in reverse
             ;; row order.
             ;; We put an empty div at the end, so clicks beyond the
             ;; virtual tab won't be referred to the virtual tab, which
             ;; would otherwise be the closest element.
             ;; TODO: Put virtual tab here
             (concat [[:div] virtual-tab-dom] (reverse tab-doms)))]))

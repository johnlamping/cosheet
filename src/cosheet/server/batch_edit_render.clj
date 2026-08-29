(ns cosheet.server.batch-edit-render
  (:require (cosheet [reporter :refer [universal-category]]
                      [entity :refer [id->entity updating-immutable
                                      make-tree-object
                                      label-element? label->elements
                                      add-elements-to-entity
                                      target-entity]]
                      [query :refer [matching-elements matching-items]]
                      [query-calculator :refer [matching-item-ids-R]]
                      [utils :refer [separate-by]]
                      [hiccup-utils :refer [add-attributes]]
                      [reporter-macros :refer [let-R]])
            (cosheet.server
             [hierarchy :refer [replace-hierarchy-leaves-by-nodes
                                hierarchy-node-non-immediate-descendant-cover
                                hierarchy-by-labels]]
             [render-utils :refer [hierarchy-node-DOM make-component
                                   make-sequential-template
                                   ensure-label-object]]
             [model-utils :refer [semantic-elements
                                  semantic-to-tree
                                  pattern-to-fixed-term]]
             [order-utils :refer [ordered-entities]]
             [item-render :refer [add-labels-DOM label-stack-DOM
                                  labels-and-elements-DOM
                                  hierarchy-node-labels-DOM
                                  virtual-DOM-component
                                  virtual-label-DOM-component]]
             [action-data :refer [get-pass-through-action-data]])))

(defn row-match-query
  "Convert a row-condition-like query entity into a fixed-term object pattern
   that matches row objects."
  [query-entity]
  (pattern-to-fixed-term
   (make-tree-object
    (map semantic-to-tree (semantic-elements query-entity)))))

(defn row-match-count-R
  "Return a reporter whose value is the number of row objects matching
   the query given by the reporter."
  [query-R mutable-store]
  (let-R [query-entity query-R]
    (let-R [matches (matching-item-ids-R (row-match-query query-entity)
                                         mutable-store)]
      (count matches))))

(defn match-count-R
  "Return a reporter whose value is the number of matches to the query
  given by the reporter, with the qualifier added as an element."
  [query-R query-qualifier mutable-store]
  (let-R [query-entity query-R]
    (let [query (-> query-entity
                    semantic-to-tree
                    pattern-to-fixed-term
                    (add-elements-to-entity [query-qualifier]))]
      (let-R [matches (matching-item-ids-R query mutable-store)]
        (count matches)))))

(defn get-batch-count-rendering-data
  [{:keys [query-id]} mutable-store]
  (let [mutable-query-entity (id->entity query-id mutable-store)
        query-R (updating-immutable mutable-query-entity)]
    [[(row-match-count-R query-R mutable-store)
      [universal-category]]
     [(match-count-R query-R :row-condition mutable-store)
      [universal-category]]]))

(defn render-batch-count-DOM
  [spec row-match-count header-match-count]
  [:div {:class "batch-query-match-counts"}
   (str row-match-count " row matches.  "
        header-match-count " table matches.")])

(defn batch-count-component
  [query-id]
  (make-component {:relative-id :batch-count
                   :query-id query-id
                   :render-dom render-batch-count-DOM
                   :get-rendering-data get-batch-count-rendering-data
                   :get-action-data get-pass-through-action-data}))

(defn render-batch-query-DOM
  "Return the dom for the query selector."
  [{:keys [query-id] :as specification} store]
  (->
   (labels-and-elements-DOM
    (semantic-elements (id->entity query-id store))
    nil false false :horizontal
    (-> (select-keys specification [:query-id :stack-id])
        (assoc :template 'anything
               :width 0.75
               :immutable true))
    :link-type)
   (add-attributes {:class "query-condition"})))

(defn batch-query-component
  ;; We need the stack-id as well as the query-id, as our
  ;; action data has to be able to find elements in it that match a
  ;; query element.
  [query-id stack-id]
  (make-component
   {:relative-id :batch-query
    :query-id query-id
    :stack-id stack-id
    :item-id query-id
    :render-dom render-batch-query-DOM}))

;;; TODO: This has to not return any queries, both for the batch edit,
;;; and for tables.
(defn batch-edit-matching-rows
  [{:keys [query-id stack-id do-not-match-query]} store]
  (let [query-entity (id->entity query-id store)
        stack-entity (id->entity stack-id store)
        query (pattern-to-fixed-term (semantic-to-tree query-entity))
        row-query (add-elements-to-entity query [:row-condition])
        matching-table-conditions (matching-items row-query store)]
    (distinct
     (concat
      (when (not do-not-match-query) [query-entity])
      [stack-entity]
      (matching-items (row-match-query query-entity) store)
      matching-table-conditions
      (map #(first (label->elements (target-entity %) :column-headers))
           matching-table-conditions)))))

(defn get-batch-edit-stack-element-action-data
  [{:keys [item-id relative-id excluding-ids stack-id]
    :as specification} ; Also uses query-id, do-not-match-query.
   containing-action-data action store]
  (let [stack-entity (id->entity stack-id store)
        selecting-query (-> (or item-id relative-id)
                            (id->entity store)
                            semantic-to-tree
                            pattern-to-fixed-term)
        excluding-queries (map #(-> %
                                    (id->entity store)
                                    semantic-to-tree
                                    pattern-to-fixed-term)
                               excluding-ids)
        to-search (batch-edit-matching-rows specification store)
        matches (mapcat (fn [entity]
                          ;; TODO: This is inefficient. Instead, use
                          ;; extended-by? to filter the matching
                          ;; elements.
                          (clojure.set/difference
                           (set (matching-elements selecting-query entity))
                           (set (mapcat #(matching-elements % entity)
                                        excluding-queries))))
                        to-search)]
    (assoc containing-action-data :subject-ids (map :item-id matches))))

(defn get-batch-edit-stack-virtual-element-subject-action-data
  [specification containing-action-data action store]
  (let [matches (batch-edit-matching-rows specification store)]
    (assoc containing-action-data :subject-ids (map :item-id matches))))

(defn stack-subtree-DOM
  "Generate the dom for a subtree of a stack selector hierarchy, given
  the doms for all the children."
  [node child-doms specification]
  (let [is-leaf (empty? child-doms)
        node-dom (hierarchy-node-labels-DOM
                  node
                  (assoc specification :get-action-data
                         get-batch-edit-stack-element-action-data))
        class (cond-> "batch-stack link-type"
                is-leaf (str " leaf"))]
    (if (empty? child-doms)
      (add-attributes node-dom {:class class})
      [:div {:class class}
       (add-attributes node-dom {:class "with-children"})
       (into [:div {:class "horizontal-label-sequence"}]
             child-doms)])))

(defn stack-child-info
  [node specification]
  (let [competition (hierarchy-node-non-immediate-descendant-cover node)]
    (assoc specification
           :top-level false
           :excluding-ids (map #(:item-id (:item %)) competition))))

(defn stack-top-level-subtree-DOM
  "Generate the dom for a top level subtree of a table header hierarchy.
  Inherited describes the column requests."
  [node specification]
  (hierarchy-node-DOM
   node stack-subtree-DOM stack-child-info
   (assoc specification :top-level true)))

;;; TODO: This should use the template of the last non-virtual stack
;;; entry as the adjacent-template for the virtuals.
(defn stack-virtual-DOM
    "Return the DOM for a virtual element in the stack section."
    [specification]
    (let [dom (virtual-DOM-component
               (assoc specification
                      :relative-id :stack-virtual
                      :template `(~'anything
                                  (~(ensure-label-object 'anything :link-type)))
                      :get-action-data
                      get-batch-edit-stack-virtual-element-subject-action-data
                      :do-not-match-query true))
          label-dom (virtual-label-DOM-component
                     (assoc
                      specification
                      :template (make-sequential-template
                                 'anything
                                 `(~(ensure-label-object
                                      'anything :link-type)))
                      :relative-id :stack-virtual-label
                      :get-action-data
                      get-batch-edit-stack-virtual-element-subject-action-data
                      :do-not-match-query true))]
      (add-labels-DOM label-dom dom :vertical)))

(defn stack-DOM
  "Generate DOM for a horizontal layout of the stack selector,
   with their labels shown in a hierarchy above them. For these, we
  want to match all elements."
  [{:keys [query-id stack-id]} store]
  ;; TODO: Add-twin is a problem here with not knowing what template
  ;; to use, because it is different for different items. Add-twin
  ;; needs to know about :column, the same way it knows about
  ;; selectors. If a template has :column, but the target is not
  ;; suitable, then the :column should be removed from the template
  ;; for that item.
  (let [specification {:query-id query-id
                       :stack-id stack-id}
        query-entity (id->entity query-id store)
        query-elements (ordered-entities (semantic-elements query-entity))
        [query-labels query-non-labels] (separate-by label-element?
                                                     query-elements)
        stack-entity (id->entity stack-id store)
        stack-elements (ordered-entities (semantic-elements stack-entity))
        [stack-labels stack-non-labels] (separate-by label-element?
                                                     stack-elements)
        labels-dom (if (empty? stack-labels)
                     ;; TODO: If there are no labels, add a virtual one,
                     ;; if the stack entity says to.
                     [:div {}]
                     (label-stack-DOM
                      stack-labels
                      (assoc specification
                             :width 0.75
                             :get-action-data
                             get-batch-edit-stack-element-action-data)))
        stack-hierarchy (-> (hierarchy-by-labels stack-non-labels)
                            replace-hierarchy-leaves-by-nodes)
        stack-doms (map #(stack-top-level-subtree-DOM % specification)
                        stack-hierarchy)]
    (into [:div {:class "horizontal-labeled-element-list batch-stack"}
           labels-dom]
          (concat stack-doms [(stack-virtual-DOM specification)]))))

(defn get-batch-edit-rendering-data
  [{:keys [query-id stack-id]} mutable-store]
  [[mutable-store (remove nil? [query-id stack-id])]])

(defn render-batch-edit-DOM
  "Return the DOM for batch editing, given the item specifying the query,
  and the item, if any, giving the sub-part of the query to operate on
  in batch mode."
  [{:keys [query-id stack-id] :as specification} store]
  (let [count-dom (batch-count-component query-id)
        query-dom (batch-query-component query-id stack-id)
        stack-dom (when stack-id
                    (stack-DOM specification store))
        batch-dom [:div {:class "batch-edit"}
                   query-dom
                   [:div {:class "batch-main"}
                    count-dom
                    stack-dom]]]
    [:div
     [:div {:class "exit-batch"} "➔"
      [:div#quit-batch-edit.tool
       [:img {:src "../icons/table_view.gif"}]
       [:div.tooltip "back to table view (C-Q)"]]]
     batch-dom]))

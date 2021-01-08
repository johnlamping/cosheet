(ns cosheet2.server.batch-edit-render
  (:require (cosheet2 [reporter :refer [universal-category]]
                      [entity :refer [description->entity updating-immutable
                                      elements to-list]]
                      [query :refer [matching-elements matching-items
                                     extended-by?]]
                      [query-calculator :refer [matching-item-ids-R]]
                      [query-impl :refer [disjoint-combinations]]
                      [debug :refer [simplify-for-print]]
                      [utils :refer [add-elements-to-entity-list separate-by]]
                      [hiccup-utils :refer [add-attributes]]
                      [expression :refer [expr-let]])
            (cosheet2.server
             [hierarchy :refer [replace-hierarchy-leaves-by-nodes
                                hierarchy-node-descendants
                                hierarchy-node-non-immediate-descendant-cover
                                hierarchy-by-labels]]
             [render-utils :refer [hierarchy-node-DOM make-component]]
             [model-utils :refer [semantic-elements semantic-to-list
                                  pattern-to-query]]
             [order-utils :refer [ordered-entities]]
             [item-render :refer [add-labels-DOM
                                  labels-and-elements-DOM
                                  horizontal-label-hierarchy-node-DOM
                                  virtual-DOM-component
                                  virtual-label-DOM-component]]
             [action-data :refer [item-complexity best-match
                                  get-pass-through-action-data]])))

(defn match-count-R
  "Return a reporter whose value is the number of matches to the query
  given by the reporter, with the qualifier added."
  [query-R query-qualifier mutable-store]
  (expr-let [query-entity query-R]
    (let [query (-> query-entity
                    semantic-to-list
                    pattern-to-query
                    (add-elements-to-entity-list [query-qualifier]))]
      (expr-let [matches (matching-item-ids-R query mutable-store)]
        (count matches)))))

(defn get-batch-count-rendering-data
  [{:keys [query-id]} mutable-store]
  (let [mutable-query-entity (description->entity query-id mutable-store)
        query-R (updating-immutable mutable-query-entity)] 
    [[(match-count-R query-R :top-level mutable-store)
      [universal-category]]
     [(match-count-R query-R :row-condition mutable-store)
      [universal-category]]]))

(defn render-batch-count-DOM
  [spec row-match-count header-match-count]
  [:div {:class "batch-query-match-counts"}
   (str row-match-count " row matches.  "
        header-match-count " table header matches.")])

(defn batch-count-component
  [query-id]
  (make-component {:relative-id :batch-count
                   :query-id query-id
                   :render-dom render-batch-count-DOM
                   :get-rendering-data get-batch-count-rendering-data
                   :get-action-data get-pass-through-action-data}))

(defn get-matches
  "Return the maches for the entity, for each query, each set of
  matches ordered from least complex."
  [entity queries]
  (map (fn [query] (let [matches (matching-elements query entity)]
                     (sort-by item-complexity matches)))
       queries))

(defn get-consistent-exemplar
  "Given a seq of matches for each of several positions, choose a
  match for each position such that there are no overlaps, and
  preferring earlier matches. Return the match at the given position."
  [matches position]
  (let [combinations (disjoint-combinations matches)]
    (nth (first combinations) position)))

(defn select-elements-from-entities
  "Take a seq of items, a seq of element queries, and a position in
  that seq. For each item, find a consistent match, if possible, for all the
  queries. For each successful item, return the match for
  the query at the position."
  [items queries position]
  (keep (fn [item]
          (let [matches (get-matches item queries)]
            (get-consistent-exemplar matches position)))
        items))

(defn select-elements-from-split-entities
  "Take a seq of items, a seq of element queries, and a position in
  that seq. For each item, find a consistent match, if possible, for
  all the queries, conjoined with the split condition. For each
  successful item, return the match at the position. Then do the same
  for the queries conjoined with the negation of the split condition."
  [items queries split-condition position]
  (mapcat
   (fn [item]
     (let [matches (get-matches item queries)
           separates (map (fn [query-matches]
                            (separate-by #(extended-by? split-condition %)
                                         query-matches))
                          matches)]
              (keep (fn [matches] (get-consistent-exemplar matches position))
                    [(map first separates) (map second separates)])))
   items))

(defn get-batch-edit-query-element-action-data
  "Return the item itself, plus one exemplar element for each affected
   table condition, table header, row. Make sure that the chosen item
   is compatible with a match of the entire query."
  [{:keys [item-id relative-id query-id stack-selector-id]}
   containing-action-data action store]
  (let [id (or item-id relative-id)
        item (description->entity id store)
        query-entity (description->entity query-id store)
        query (pattern-to-query (semantic-to-list query-entity))
        ;; To make sure that the item we pick is compatible with a
        ;; match for the entire query, we have to consistently pick
        ;; items for each element of the query, not just our dom's
        ;; element. Then, we use the pick corresponding to our
        ;; element. We pick items for the more complex query elements
        ;; first, both for efficiency, and to make it more likely to
        ;; choose more prosaic tuples of matches.
        query-elements (semantic-elements query-entity)
        ordered-query-elements(reverse (sort-by item-complexity query-elements))
        element-queries (map #(pattern-to-query (semantic-to-list %))
                             ordered-query-elements)
        item-index (.indexOf ordered-query-elements item)
        _ (assert (not= item-index -1))
        top-level-entities (matching-items
                            (add-elements-to-entity-list query [:top-level])
                            store)
        header-entities (matching-items
                         (add-elements-to-entity-list query [:row-condition])
                         store)
        stack-selector-item (when stack-selector-id
                               (let [item-query (pattern-to-query
                                                 (semantic-to-list item)) 
                                     elems (matching-elements
                                            item-query
                                            (description->entity
                                             stack-selector-id store))]
                                 (when-let [best (best-match item-query elems)]
                                   [best])))
        items (concat
               [item]
               stack-selector-item 
               (select-elements-from-entities
                top-level-entities element-queries item-index)
               ;; A single table specification item looks like two
               ;; items in the UI: the query, and the column specs. We
               ;; treat each as if they were separate items
               (select-elements-from-split-entities
                header-entities element-queries '(nil :column) item-index))]
    (assoc containing-action-data :target-ids (map :item-id items))))

(defn batch-query-virtual-DOM
  "Return the DOM for a virtual element in the row selector part of the display,
   that is an element of the query item."
  []
  ;; TODO: !!! This needs a preliminary get-action-data that returns all the
  ;;       entities and headers.
  (let [dom (virtual-DOM-component
             {:relative-id :virtual}
             {:template '(anything (anything :label))})
        label-dom (virtual-label-DOM-component {:template '(anything :label)})]
    (add-labels-DOM label-dom dom :vertical)))

(defn batch-query-DOM
  "Return the dom for the query selector."
  [{:keys [query-id] :as specification} store]
  (labels-and-elements-DOM
   (semantic-elements (description->entity query-id store))
   (batch-query-virtual-DOM)
   true true :horizontal
   (assoc specification
          :relative-id :batch-query
          :template 'anything
          :get-action-data get-batch-edit-query-element-action-data)))

(defn get-batch-edit-stack-element-action-data
  [{:keys [item-id relative-id excluding-ids query-id stack-selector-id]}
   containing-action-data action store]
  (let [query-entity (description->entity query-id store)
        stack-selector-entity  (description->entity stack-selector-id store)
        query (pattern-to-query (semantic-to-list query-entity))
        selecting-query (-> (or item-id relative-id)
                            (description->entity store)
                            semantic-to-list
                            pattern-to-query)
        excluding-queries (map #(-> %
                                    (description->entity store)
                                    semantic-to-list
                                    pattern-to-query)
                               excluding-ids)
        to-search (distinct
                   (concat
                    [query-entity stack-selector-entity]
                    (matching-items
                     (add-elements-to-entity-list query [:top-level])
                     store)
                    (matching-items
                     (add-elements-to-entity-list query [:row-condition])
                     store)))
        matches (mapcat (fn [entity]
                          ;; TODO: This is inefficient. Instead, use
                          ;; extended-by? to filter the matching
                          ;; elements.
                          (clojure.set/difference
                           (set (matching-elements selecting-query entity))
                           (set (mapcat #(matching-elements % entity)
                                        excluding-queries))))
                        to-search)]
    (assoc containing-action-data :target-ids (map :item-id matches))))

(defn stack-selector-subtree-DOM
  "Generate the dom for a subtree of a table header hierarchy, given
  the doms for all the children."
  [node child-doms specification]
  (let [is-leaf (empty? child-doms)
        node-dom (horizontal-label-hierarchy-node-DOM
                  node
                  (assoc specification :get-action-data
                         get-batch-edit-stack-element-action-data))
        class (cond-> "column-header tag"
                is-leaf (str " leaf"))]
    (if (empty? child-doms)
      (add-attributes node-dom {:class class})
      [:div {:class class}
       (add-attributes node-dom {:class "with-children"})
       (into [:div {:class "horizontal-label-sequence"}]
             child-doms)])))

(defn stack-selector-child-info
  [node specification]
  (let [competition (hierarchy-node-non-immediate-descendant-cover node)]
    (assoc specification
           :top-level false
           :excluding-ids (map :item-id competition))))

(defn stack-selector-top-level-subtree-DOM
  "Generate the dom for a top level subtree of a table header hierarchy.
  Inherited describes the column requests."
  [node specification]
  (hierarchy-node-DOM
   node stack-selector-subtree-DOM stack-selector-child-info
   (assoc specification :top-level true)))

(defn stack-selector-DOM
  "Generate DOM for a horizontal layout of the stack selector,
   with their labels shown in a hierarchy above them. For these, we
  want to match all elements."
  [{:keys [query-id stack-selector-id]} store]
  ;; TODO: Add-twin is a problem here with not knowing what template
  ;; to use, because it is different for different items. Add-twin
  ;; needs to know about :column, the same way it knows about
  ;; selectors. If a template has :column, but the subject is not
  ;; suitable, then the :column should be removed.
  (let [query-entity (description->entity query-id store)
        stack-selector-entity  (description->entity stack-selector-id store)
        elements (ordered-entities (semantic-elements stack-selector-entity))
        hierarchy (-> (hierarchy-by-labels elements)
                      replace-hierarchy-leaves-by-nodes)
        specification {:query-id (:item-id query-entity)
                       :stack-selector-id (:item-id stack-selector-entity)}
        doms (map #(stack-selector-top-level-subtree-DOM % specification)
                  hierarchy)]
    (into [:div {:class "batch-stack"}] doms)))

(defn get-batch-edit-rendering-data
  [{:keys [query-id stack-selector-id]} mutable-store]
  [[mutable-store (remove nil? [query-id stack-selector-id])]])

(defn render-batch-edit-DOM
  "Return the DOM for batch editing, given the item specifying the query,
  and the item, if any, giving the sub-part of the query to operate on
  in batch mode."
  [{:keys [query-id stack-selector-id] :as specification} store]
  (let [count-dom (batch-count-component query-id)
        query-dom (batch-query-DOM specification store)
        stack-dom (when stack-selector-id
                    (stack-selector-DOM specification store))
        inner-dom [:div {:class "batch-stack-wrapper"}
                   count-dom
                   (if stack-selector-id
                     [:div {:class "horizontal-tags-element batch-stack"}
                      query-dom
                      [:div {:class "batch-stack"} stack-dom]]
                     (add-attributes query-dom {:class "batch-stack"}))]]
    [:div
     [:div#quit-batch-edit.tool
      [:img {:src "../icons/table_view.gif"}]
      [:div.tooltip "table view (C-Q)"]]
     [:div {:class "query-result-wrapper"} inner-dom]]))

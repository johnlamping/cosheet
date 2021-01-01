(ns cosheet2.server.batch-edit-render
  (:require (cosheet2 [reporter :refer [universal-category]]
                      [entity :refer [description->entity updating-immutable]]
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
             [action-data :refer [item-complexity best-matching-id]])))

(defn match-count-R
  "Return a reporter whose value is the number ox1f matches to the row selector."
  [query-entity query-qualifier mutable-store]
  (let [query (pattern-to-query (semantic-to-list query-entity))
        qualified-query (add-elements-to-entity-list query query-qualifier)]
    (expr-let
        [matches (matching-item-ids-R qualified-query mutable-store)]
      (count matches))))

(defn get-batch-count-rendering-data
  [{:keys [query-id stack-selector-id]} mutable-store]
  [[(match-count-R query-id :top-level mutable-store)
    [universal-category]]
   [(match-count-R query-id :row-condition mutable-store)
    [universal-category]]])

(defn render-batch-count-DOM
  [row-match-count header-match-count]
  [:div {:class "batch-query-match-counts"}
   (str row-match-count " row matches.  "
        header-match-count " table header matches.")])

(defn batch-count-component
  [query-id stack-selector-id]
  (make-component {:query-id query-id
                   :stack-selector-id stack-selector-id
                   :render-dom render-batch-count-DOM
                   :get-rendering-data get-batch-count-rendering-data}))

(defn get-matches
  "Return the maches for the item, for each query, ordered from least complex."
  [item queries]
  (map (fn [query]
         (let [matches (matching-elements query item)]
           (sort-by item-complexity matches)))
       queries))

(defn get-consistent-exemplar
  "Given a seq matches for each of several positions, find a
   consistent (non-overlapping) combination, and return the match at
   the given position."
  [matches position]
  (let [combinations (disjoint-combinations matches)]
    (nth (first combinations) position)))

(defn select-elements-from-entities
  "Given a seq of items, a seq of element queries, and a position in
   that seq, find a consistent match for all the queries for each item,
   and return the match at the position."
  [items queries position]
  (keep (fn [item]
          (let [matches (get-matches item queries)]
            (get-consistent-exemplar matches position)))
        items))

(defn select-elements-from-split-entities
  "Given a seq of items, a seq of element queries, and a position in
  that seq, find a consistent match for all the queries, conjoined
  with the split condition for each item, and return the match at the
  position. Then do the same for the queries conjoined with the
  negation of the split condition."
  [items queries split-condition position]
  (mapcat (fn [item]
            (let [matches (get-matches item queries)
                  separates (map (fn [query-matches]
                                   (separate-by
                                    #(extended-by? split-condition %)
                                    query-matches))
                                 matches)]
              (keep (fn [matches] (get-consistent-exemplar matches position))
                    [(map first separates) (map second separates)])))
        items))

(defn get-batch-edit-query-element-action-data
  "Return the item itself, plus one exemplar element for each affected
   table condition, table header, row. Make sure that the chosen item
   is compatible with a match of the entire query."
  [specification containing-action-data action store
   query-entity stack-selector-entity]
  (let [query (pattern-to-query (semantic-to-list query-entity))
        ;; To make sure that the item we pick is compatible with a
        ;; match for the entire query, we have to consistently pick
        ;; items for each element of the query, and then return the
        ;; one corresponding to our element. We pick the item for the
        ;; most complex query element first, both for efficiency, and
        ;; for choosing the most prosaic tuples of matches.
        query-elements (semantic-elements query-entity)
        ordered-query-elements(reverse (sort-by item-complexity query-elements))
        element-queries (map pattern-to-query ordered-query-elements)
        id (or (:item-id specification) (:relative-id specification))
        item (description->entity id store)
        item-index (.indexOf item ordered-query-elements)
        top-level-entities (matching-items
                         (add-elements-to-entity-list query :top-level) store)
        header-entities (matching-items
                      (add-elements-to-entity-list query :row-condition) store)]
    (assert (not= item-index -1))
    (let [items (cond->
                    (concat [item]
                            (select-elements-from-entities
                             top-level-entities element-queries item-index)
                            ;; A single table specification item looks like
                            ;; two items in the UI: the query, and the
                            ;; column specs. We treat each as if they were
                            ;; separate items
                            (select-elements-from-split-entities
                             header-entities
                             element-queries '(nil :column) item-index)))
          ids (concat (map :item-id items)
                      (remove nil? [(when stack-selector-entity
                              (best-matching-id
                               id (:item-id stack-selector-entity) store))]))]
      (assoc containing-action-data :target-ids ids))))

(defn get-batch-edit-stack-element-action-data
  [{:keys [hierarchy-node]} containing-action-data action store
   query-entity stack-selector-entity]
  ;; TODO: !!! Look for excluding-cover, to handle a node that must
  ;;       exclude its siblings.
  (let [query (pattern-to-query (semantic-to-list query-entity))
        items (map :item (hierarchy-node-descendants hierarchy-node))
        item-queries (map #(pattern-to-query (semantic-to-list %)) items)
        to-search (concat
                   [query-entity stack-selector-entity]
                   (matching-items
                    (add-elements-to-entity-list query :top-level) store)
                   (matching-items
                    (add-elements-to-entity-list query :row-condition) store))
        matches (distinct (mapcat (fn [entity]
                                    (mapcat #(matching-elements % entity)
                                            item-queries))
                                  to-search))]
    (assoc containing-action-data :target-ids (map :item-id matches))))

(defn stack-selector-subtree-DOM
  "Generate the dom for a subtree of a table header hierarchy, given
  the doms for all the children."
  [node child-doms function-info specification]
  (let [is-leaf (empty? child-doms)
        node-dom (horizontal-label-hierarchy-node-DOM
                  node (assoc specification
                              :hierarchy-node node))
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
  (assoc specification
         :top-level false
         :excluding-cover (hierarchy-node-non-immediate-descendant-cover node)))

(defn stack-selector-top-level-subtree-DOM
  "Generate the dom for a top level subtree of a table header hierarchy.
  Inherited describes the column requests."
  [node specification]
  (hierarchy-node-DOM
   node stack-selector-subtree-DOM stack-selector-child-info
   {:top-level true}
   specification))

;;; The subject referent should give the match to the rows, to the headers,
;;; and to the items that specify the pattern.
(defn stack-selector-DOM
  "Generate DOM for a horizontal layout of the stack selector,
   with their labels shown in a hierarchy above them."
  [query-entity stack-selector-entity]
  (let [elements (ordered-entities (semantic-elements stack-selector-entity))
        hierarchy (-> (hierarchy-by-labels elements)
                      replace-hierarchy-leaves-by-nodes)
        specification {:get-action-data
                       [get-batch-edit-stack-element-action-data
                        query-entity stack-selector-entity]}
        doms (map #(stack-selector-top-level-subtree-DOM % specification)
                  hierarchy)]
    (into [:div {:class "horizontal-label-sequence"}] doms)))

(defn batch-query-virtual-DOM
  "Return the DOM for a virtual element in the row selector part of the display,
   that is an element of the query item."
  []
  (let [dom (virtual-DOM-component
             {:relative-id :virtual}
             {:template '(anything (anything :label))})
        label-dom (virtual-label-DOM-component {:template '(anything :label)})]
    (add-labels-DOM label-dom dom :vertical)))

(defn batch-query-DOM
  "Return the dom for row selector."
  [query-entity stack-selector-entity]
  (let [query (pattern-to-query (semantic-to-list query-entity))
        virtual-dom (batch-query-virtual-DOM)
        ;; We need to take the current versions of the query elements,
        ;; as :match-multiple only works with immutable items.
        current-query-elements (semantic-elements query)
        batch-dom
        ;; TODO: Add-twin is a problem here with not knowing what
        ;; template to use, because it is different for different
        ;; items. Add-twin needs to know about :column, and copy it if
        ;; and only if a twin's entity has it.
        (labels-and-elements-DOM
         current-query-elements virtual-dom
         true true :horizontal
         {:get-action-data [get-batch-edit-query-element-action-data
                            query-entity stack-selector-entity]})]
    batch-dom))

(defn get-batch-edit-rendering-data
  [{:keys [query-id stack-selector-id]} mutable-store]
  [[mutable-store (remove nil? [query-id stack-selector-id])]])

(defn render-batch-edit-DOM
  "Return the DOM for batch editing, given the item specifying the query,
  and the item, if any, giving the sub-part of the query to operate on
  in batch mode."
  [{:keys [query-id stack-selector-id]} store]
  (let [query-entity (description->entity query-id store)
        stack-selector-entity (when stack-selector-id
                                (description->entity stack-selector-id store))
        count-dom (batch-count-component query-id stack-selector-id)
        query-dom (batch-query-DOM query-entity stack-selector-entity)
        inner-dom (if stack-selector-id
                    ;; For elements of our items, we want to match all
                    ;; exclusively matched elements.
                    (let [stack-selector (description->entity
                                          stack-selector-id store)
                          elements-dom (stack-selector-DOM
                                        query-entity stack-selector-entity)]
                      [:div {:class "batch-stack-wrapper"}
                       count-dom
                       [:div {:class "horizontal-tags-element batch-stack"}
                        query-dom
                        [:div {:class "batch-stack"} elements-dom]]])
                    [:div {:class "batch-stack-wrapper"}
                     count-dom
                     (add-attributes query-dom {:class "batch-stack"})])]
    [:div
     [:div#quit-batch-edit.tool
      [:img {:src "../icons/table_view.gif"}]
      [:div.tooltip "table view (C-Q)"]]
     [:div {:class "query-result-wrapper"} inner-dom]]))

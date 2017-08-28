(ns cosheet.server.batch-edit-render
  (:require (cosheet [entity :as entity]
                     [query :refer [matching-elements matching-items]]
                     [debug :refer [simplify-for-print]]
                     [hiccup-utils :refer [dom-attributes
                                           into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq]])
            (cosheet.server
             [referent :refer [item-referent exemplar-referent
                               elements-referent query-referent
                               union-referent-if-needed union-referent
                               difference-referent
                               item-or-exemplar-referent
                               semantic-elements-R semantic-to-list-R
                               pattern-to-condition condition-to-template]]
             [render-utils :refer [add-inherited-attribute]]
             [item-render :refer [must-show-label-item-DOM-R]])))

(defn top-level-items-referent
  "Return a referent to all the top level items that match the query item."
  [query-item]
  (query-referent (list (item-referent query-item) :top-level)))

(defn selected-batch-referent
  "Return the referent for the selected batch item and everything affected
  by it."
  [query-item selected-batch-item]
  (let [row-condition (list (item-referent query-item) :row-condition)]
    (if (= (:id query-item) (:id selected-batch-item))
      (union-referent
       [;; All rows matching the query
        (top-level-items-referent query-item)
        ;; All table conditions matching the query
        (query-referent row-condition)])
      (when-let [subject (entity/subject selected-batch-item)]
        (let [subject-referent (selected-batch-referent query-item subject)
              referent (elements-referent (item-referent selected-batch-item)
                                          subject-referent)]
          (if (= subject query-item)
            (union-referent [referent
                             ;; All table headers where the row condition
                             ;; matches the query and the header matches the
                             ;; selected batch item.
                             (elements-referent
                              (list (item-referent selected-batch-item) :column)
                              (query-referent
                               (list nil :table row-condition)))])
            referent))))))

(defn batch-edit-DOM-R
  "Return the DOM for batch editing, given the item specifying the query,
  and the item, if any, giving the sub-part of the query to operate on
  in batch mode."
  [query-item selected-batch-item inherited]
  (let [inherited-for-query (assoc inherited
                                   :selector-category :batch-query)
        inherited-for-batch (cond-> (assoc inherited
                                           :selector-category :batch-selected)
                              ;; If the selected item is the whole query,
                              ;; then a delete could remove the whole condition
                              ;; of a table. Have it just remove top level
                              ;; items, instead.
                              (= query-item selected-batch-item)
                              (add-inherited-attribute
                               [[#{:content}
                                 {:delete
                                  {:referent
                                   (top-level-items-referent query-item)}}]]))
        selected-referent (selected-batch-referent
                           query-item selected-batch-item)]
    (expr-let [query-dom (must-show-label-item-DOM-R
                          query-item nil inherited-for-query)
               batch-dom (if selected-referent
                           (must-show-label-item-DOM-R
                            selected-batch-item selected-referent nil
                            inherited-for-batch)
                           [:div])]
      [:div {:class "batch-holder"}
       (add-attributes query-dom {:class "batch-query"})
       (add-attributes batch-dom {:class "batch-selected"})])))

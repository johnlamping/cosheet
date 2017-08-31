(ns cosheet.server.batch-edit-render
  (:require (cosheet [store :refer [call-dependent-on-id]]
                     [entity :as entity]
                     [query :refer [matching-elements matching-items]]
                     [debug :refer [simplify-for-print]]
                     [hiccup-utils :refer [dom-attributes
                                           into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq]])
            (cosheet.server
             [referent :refer [item-referent elements-referent query-referent
                               union-referent instantiate-to-items]]
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
    (if (= query-item selected-batch-item)
      (union-referent
       [;; The item, itself.
        (item-referent query-item)
        ;; All rows matching the query.
        (top-level-items-referent query-item)
        ;; All table conditions matching the query.
        (query-referent row-condition)])
      (when-let [subject (when selected-batch-item
                           (entity/subject selected-batch-item))]
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

(defn selected-batch-non-header-referent
  "Return the referent for every non-header affected by a selected batch item."
  [query-item selected-batch-item]
  (let [row-condition (list (item-referent query-item) :row-condition)]
    (if (= (:id query-item) (:id selected-batch-item))
      (top-level-items-referent query-item)
      (when-let [subject (entity/subject selected-batch-item)]
        (let [subject-referent (selected-batch-non-header-referent
                                query-item subject)]
          (elements-referent (item-referent selected-batch-item)
                             subject-referent))))))

(defn batch-edit-DOM-R
  "Return the DOM for batch editing, given the item specifying the query,
  and the item, if any, giving the sub-part of the query to operate on
  in batch mode."
  [query-item selected-batch-item store inherited]
  (let [inherited-for-query (assoc inherited
                                   :selector-category :batch-query)]
    (expr-let
        [query-dom (must-show-label-item-DOM-R
                    query-item nil inherited-for-query)
         remaining-dom
         (when selected-batch-item
           (let [selected-referent (selected-batch-referent
                                    query-item selected-batch-item)
                 selected-non-header-referent
                 (selected-batch-non-header-referent
                  query-item selected-batch-item)
                 inherited-for-batch
                 (cond->
                     (-> inherited
                         (assoc :selector-category :batch-selected)
                         ;; Make its doms have different keys.
                         (update :key-prefix #(conj % :batch)))
                   ;; If the selected item is the whole query,
                   ;; then a delete could remove the whole condition
                   ;; of a table. Have it just remove top level
                   ;; items, instead.
                   (= query-item selected-batch-item)
                   (add-inherited-attribute
                    [#{:content}
                     {:delete
                      {:referent selected-non-header-referent}}]))]
             (expr-let
                 [batch-dom (must-show-label-item-DOM-R
                             selected-batch-item selected-referent nil
                             inherited-for-batch)
                  count-dom
                  (call-dependent-on-id
                   store nil
                   (fn [store]
                     (let [total (count (instantiate-to-items
                                         selected-referent store))
                           non-header (count (instantiate-to-items
                                              selected-non-header-referent
                                              store))]
                       [:div {:class "batch-query-match-counts"}
                        (str non-header " data matches. "
                             (- total non-header 1) " header matches.")])))]
               [count-dom
                (add-attributes batch-dom {:class "batch-selected"})])))]
      (into [:div {:class "batch-holder"}
             [:div#quit-batch-edit {:class "quit-batch-edit tool"}
              "Return to tabs"]
             (add-attributes query-dom {:class "batch-query"})]
            remaining-dom))))

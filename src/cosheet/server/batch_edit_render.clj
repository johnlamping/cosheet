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

(defn query-items-referent
  [query-item]
  (union-referent
   [;; The item, itself.
    (item-referent query-item)
    ;; All rows matching the query.
    (top-level-items-referent query-item)
    ;; All table conditions matching the query.
    (query-referent (list (item-referent query-item) :row-condition))]))

(defn selected-batch-referent
  "Return the referent for the selected batch item and everything affected
  by it, given the query item and a referent for everything relevant
  to the query."
  [selected-batch-item query-item query-referent]
  (if (= query-item selected-batch-item)
    query-referent
    (when-let [subject (when selected-batch-item
                         (entity/subject selected-batch-item))]
      (let [subject-referent (selected-batch-referent
                              subject query-item query-referent)]
        (elements-referent (item-referent selected-batch-item)
                           subject-referent)))))

(defn batch-edit-stack-DOM-R
  "Return the dom for the edit stack part of the batch edit display."
  [query-item selected-batch-item store inherited]
  (expr-let
      [doms
       (when selected-batch-item
         (let [selected-referent (selected-batch-referent
                                  selected-batch-item query-item
                                  (query-items-referent query-item))
               selected-non-header-referent
               (selected-batch-referent
                selected-batch-item query-item
                (top-level-items-referent query-item))
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
    (into [:div {:class "batch-stack-wrapper"}] doms)))

(defn batch-edit-DOM-R
  "Return the DOM for batch editing, given the item specifying the query,
  and the item, if any, giving the sub-part of the query to operate on
  in batch mode."
  [query-item selected-batch-item store inherited]
  (let [inherited-for-query
        (-> inherited
            (assoc :selector-category :batch-query)
            (add-inherited-attribute
             [(:item-id selected-batch-item) {:class "batch-selected-item"}])
            )]
    (expr-let
        [query-dom (must-show-label-item-DOM-R
                    query-item nil inherited-for-query)
         stack-dom (batch-edit-stack-DOM-R
                    query-item selected-batch-item store inherited)]
      [:div {:class "batch-holder"}
       [:div#quit-batch-edit.tool
              [:img {:src "../icons/exit.gif"}]
        [:div.tooltip "exit batch edit (C-B)"]]
       (add-attributes query-dom {:class "batch-query"})
       stack-dom])))

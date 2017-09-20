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
                               parallel-union-referent
                               exemplar-referent
                               union-referent instantiate-to-items
                               semantic-elements-R]]
             [render-utils :refer [add-inherited-attribute]]
             [item-render :refer [must-show-label-item-DOM-R
                                  virtual-element-DOM-R
                                  labels-and-elements-DOM-R]])))

(defn top-level-items-referent
  "Return a referent to all the top level items that match the query item."
  [query-item]
  (query-referent (list (item-referent query-item) :top-level)))

(defn table-conditions-referent
  " All table conditions matching the query."
  [query-item]
  (query-referent (list (item-referent query-item) :row-condition)))

(defn query-items-referent
  [query-item]
  (union-referent
   [(item-referent query-item)
    (top-level-items-referent query-item)
    (table-conditions-referent query-item)]))

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
  [query-item query-elements selected-batch-item store inherited]
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
               (-> inherited
                   (assoc :selector-category :batch-selected)
                   ;; Make its doms have different keys.
                   (update :key-prefix #(conj % :batch)))]
           (expr-let
               [batch-dom
                (if (= query-item selected-batch-item)
                  (let [query-and-rows-referent
                        (union-referent [(item-referent query-item)
                                         (top-level-items-referent query-item)])
                        conditions-referent
                        (table-conditions-referent query-item)
                        inherited
                        (->
                         inherited-for-batch
                         ;; We use a function for the subject, so that
                         ;; all column headers will be changed, even if
                         ;; there are several matching headers in one table.
                         (assoc
                          :subject-referent
                          (fn
                            ([]
                             (union-referent [query-and-rows-referent
                                              conditions-referent]))
                            ([item]
                             (let [item-ref (item-referent item)]
                               (parallel-union-referent
                                [(exemplar-referent
                                  item-ref query-and-rows-referent)
                                 (elements-referent
                                  item-ref conditions-referent)])))))
                         ;; If the selected item is the whole query, then a
                         ;; delete could remove the whole condition of a
                         ;; table. Have it just remove top level rows, instead.
                         (add-inherited-attribute
                          [#{:content}
                           {:delete
                            {:referent selected-non-header-referent}}]))]
                    ;; TODO: We want to add a virtual element here, but
                    ;; it has to know to add the necessary stuff to table
                    ;; headers or not to add to headers at all. (Maybe use
                    ;; a union of two virtual referents.)
                    ;; Add-twin might be a problem too.
                    (labels-and-elements-DOM-R
                     query-elements false true true :horizontal inherited))
                  (must-show-label-item-DOM-R
                   selected-batch-item selected-referent nil
                   inherited-for-batch))
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
            (assoc :selector-category :batch-query
                   :subject-referent (item-referent query-item))
            (add-inherited-attribute
             [(:item-id selected-batch-item) {:class "batch-selected-item"}]))]
    (expr-let
        [condition-elements (semantic-elements-R query-item)
         query-dom (labels-and-elements-DOM-R
                    condition-elements
                    #(virtual-element-DOM-R
                      'anything % true :vertical inherited-for-query)
                    true true :horizontal inherited-for-query)
         old-query-dom (must-show-label-item-DOM-R
                    query-item nil inherited-for-query)
         stack-dom (batch-edit-stack-DOM-R
                    query-item condition-elements selected-batch-item store
                    inherited)]
      [:div {:class "batch-holder"}
       [:div#quit-batch-edit.tool
              [:img {:src "../icons/exit.gif"}]
        [:div.tooltip "exit batch edit (C-Q)"]]
       (add-attributes query-dom {:class "batch-query"})
       stack-dom])))

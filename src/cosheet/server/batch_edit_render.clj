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
             [render-utils :refer [make-component]]
             [item-render :refer [must-show-label-item-DOM-R]])))

(defn batch-edit-DOM-R
  "Return the DOM for batch editing, given the item specifying the query,
  and the item, if any, giving the sub-part of the query to operate on
  in batch mode."
  [query-item selected-batch-item inherited]
  (let [inherited-for-query (assoc inherited
                                   :selector-category :batch-query)]
    (expr-let [query-dom (must-show-label-item-DOM-R
                          query-item nil inherited-for-query)]))
  )

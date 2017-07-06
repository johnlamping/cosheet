(ns cosheet.server.hierarchy-render
  (:require (cosheet [debug :refer [simplify-for-print]]
                     [expression :refer [expr expr-let expr-seq expr-filter]])
            (cosheet.server
             [hierarchy :refer [hierarchy-node-example-elements]]
             [order-utils :refer [order-items-R]]
             [render-utils :refer [item-stack-DOM
                                   condition-satisfiers-R]])))

(defn hierarchy-properties-DOM-R
  "Return DOM for example elements that give rise to the properties
  of one hierarchy node. Inherited describes the context of the properties.
  dom-fn should be item-DOM-R, or item-without-labels-DOM-R, or similar."
  [dom-fn hierarchy-node inherited]
  (let [example-elements (hierarchy-node-example-elements hierarchy-node)]
    (expr-let [ordered-elements (order-items-R example-elements)
               satisfiers (expr-seq
                           map #(condition-satisfiers-R % (:template inherited))
                           ordered-elements)]
      (item-stack-DOM
       dom-fn ordered-elements satisfiers inherited))))

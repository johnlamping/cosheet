(ns cosheet2.map-state
  (:require (cosheet2
             [reporter :refer [new-reporter reporter-value
                               change-data! change-data-control-return!]]
             [expression :refer [expr category-change]]
             [category-change-calculator :refer [category-change-calculator]])))

;;; Support a reporter that holds a map. The entries in the map may
;;; themselves be reporters.

(defn new-map-state
  [initial]
  (assert (map? initial))
  (new-reporter :value initial))

(defn map-state-get-current [map-state key]
  (reporter-value (key (reporter-value map-state))))

(defn map-state-get [map-state key]
  (expr key (category-change [key] map-state)))

(defn map-state-change-value! [map-state key fun]
  (change-data! map-state
                (fn [data]
                  [(assoc-in data [:value key] (fun (key (:value data))))
                   [key]
                   [key]])))

(defn map-state-change-value-control-return! [map-state key fun]
  (change-data-control-return!
   map-state
   (fn [data] (let [[new-val result] (fun (key (:value data)))]
                [(assoc-in data [:value key] new-val)
                 [key]
                 [key] result]))))

(defn map-state-reset! [map-state key value]
  (map-state-change-value! map-state key (constantly value)))


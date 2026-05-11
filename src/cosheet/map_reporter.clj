(ns cosheet.map-reporter
  (:require (cosheet
             [reporter :refer [make-reporter reporter-value
                               change-data! change-data-control-return!
                               change-value!]]
             [reporter-macros :refer [app-R]]
             [category-change-calculator :refer [category-change-calculator
                                                 category-change-R]])))

;;; Support a reporter that holds a map. The entries in the map may
;;; themselves be reporters.

;;; Unlike mutable-map, map-reporter gives all the reporter
;;; functionality. But it doesn't support as much parallelism as
;;; mutable-map.

(defn make-map-reporter
  [initial]
  (assert (map? initial))
  (make-reporter :value initial))

(defn map-reporter-get-current [map-state key]
  (reporter-value (key (reporter-value map-state))))

(defn map-reporter-get [map-state key]
  (app-R key (category-change-R [key] map-state)))

(defn map-reporter-change-value! [map-state key fun]
  (change-data! map-state
                (fn [data]
                  [(assoc-in data [:value key] (fun (key (:value data))))
                   [key]
                   [key]])))

(defn map-reporter-change-value-control-return! [map-state key fun]
  (change-data-control-return!
   map-state
   (fn [data] (let [[new-val result] (fun (key (:value data)))]
                [(assoc-in data [:value key] new-val)
                 [key]
                 [key]
                 result]))))

(defn map-reporter-reset!
  "Change the state to agree with the provided map on its values."
  [map-state map]
  (change-value! map-state
                 (fn [data]
                   [(into data map)
                    (vec (keys map))
                    (vec (keys map))])))

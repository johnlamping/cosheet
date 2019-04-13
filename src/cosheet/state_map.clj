(ns cosheet.state-map
  (:require (cosheet [mutable-manager
                      :refer [new-mutable-manager-data
                              current-mutable-value
                              get-or-make-reporter
                              describe-and-swap!
                              describe-and-swap-control-return!]]
                     [task-queue :refer [new-priority-task-queue]])))

;;; Support for a mutable manager over a map.

(defn new-state-map
  ([initial] (new-state-map initial (new-priority-task-queue)))
  ([initial queue]
   (assert (map? initial))
   (new-mutable-manager-data initial queue)))

(defn state-map-get [state-map key & rest]
  (apply get-or-make-reporter [key] get state-map key rest))

(defn state-map-get-current-value [state-map key & rest]
  (apply get (current-mutable-value state-map) key rest))

(defn state-map-reset! [state-map key value]
  (describe-and-swap! state-map (fn [data] [(assoc data key value) [key]])))

(defn state-map-swap! [state-map key fun]
  (describe-and-swap! state-map (fn [data] [(update data key fun) [key]])))

(defn state-map-swap-control-return! [state-map key fun]
  (describe-and-swap-control-return!
   state-map (fn [data]
               (let [[new-value return-value] (fun (get data key))]
                          [(assoc data key new-value) [key] return-value]))))

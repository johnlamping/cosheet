(ns cosheet.state-map
  (:require (cosheet [mutable-manager
                      :refer [new-mutable-manager-data
                              get-or-make-reporter
                              describe-and-swap!]])))

;;; Support for a mutable manager over a map.

(defn new-state-map
  [initial]
  (assert (map? initial))
  (new-mutable-manager-data initial))

(defn state-map-get [state-map key & rest]
  (apply get-or-make-reporter [key] get state-map key rest))

(defn state-map-reset! [state-map key value]
  (describe-and-swap! state-map (fn [data] [(assoc data key value) [key]])))

(defn state-map-swap! [state-map key fun]
  (describe-and-swap! state-map (fn [data] [(update data key fun) [key]])))


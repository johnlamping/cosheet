(ns cosheet.client-utils
  #+cljs (:require [reagent.core :as reagent :refer [atom]])
  )

(defn replace-in-struct
  "Replace items in the structure that match keys in the map
   with the result in the map"
  [smap struct]
  (letfn [(r [struct]
            (if (contains? smap struct)
              (smap struct)
              (cond (map? struct) (into {} (map r struct))
                    (vector? struct) (vec (map r struct))
                    ;; We need the or below, because (list* nil) = nil.
                    (list? struct) (or (list* (map r struct)) '())
                    :else struct)))]
    (r struct)))

(defn update-atom-map
  "Update the atom containing a map of atoms to have the new data,
  creating or deleting atoms as indicated"
  [amap update]
  (swap! amap
         (fn [amap]
           (reduce (fn [amap [key value]]
                     (cond (nil? value) (dissoc amap key)
                           (contains? amap key) (do (reset! (amap key) value)
                                                    amap)
                           :else (assoc amap key (atom value))))
                   amap update))))


(ns cosheet.hiccup-utils
  (:require [clojure.string :as string]))

(defn combine-maps
  "Add the information of the second map into that of the first,
   using the combiner to combine overlapping information. The combiner
   is given the key and the two values."
  [combiner m1 m2]
  (reduce (fn [accumulator [key value]]
            (update-in accumulator [key]
                       #(if % (combiner key % value) value)))
          (or m1 {}) (or m2 {})))

(defn map-combiner
  "Combine maps. Otherwise, just return the second."
  [key v1 v2]
  (if (and (map? v1) (map? v2))
    (combine-maps map-combiner v1 v2)
    v2))

(defn into-attributes
  "Add attributes to an attribute map,
   correctly handling multiple classes or styles, or commands."
  [accumulator attributes]
  (combine-maps (fn [key v1 v2]
                  (case key
                    :class (if (empty? v2)
                             v1
                             (let [v1s (string/split v1 #" ")
                                   v2s (string/split v2 #" ")]
                               (string/join
                                " " (concat v1s (remove (set v1s) v2s)))))
                    (map-combiner key v1 v2)))
                accumulator attributes))

(defn dom-attributes
  "Return the current specified attributes of a dom."
  [dom]
  (let [[dom-tag second & _] dom]
    (if (map? second) second {})))

(defn add-attributes
  "Add attributes to a hiccup dom descriptor."
  [dom attributes]
  (if attributes
    (let [dom (if (sequential? dom) dom [:div dom])
          [dom-tag second & remainder] dom
          [attr remainder] (if (map? second)
                             [second remainder]
                             [{} (rest dom)])]
      (into [dom-tag (into-attributes attr attributes)]
            remainder))
    dom))

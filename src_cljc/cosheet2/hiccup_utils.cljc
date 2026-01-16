(ns cosheet2.hiccup-utils
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
  "If both arguments are maps, combine them.
   Otherwise, just return the second."
  [key v1 v2]
  (if (and (map? v1) (map? v2))
    (combine-maps map-combiner v1 v2)
    v2))

(defn merge-classes
  [c1 c2]
  (cond (empty? c2) c1
        (empty? c1) c2
        true (let [c1s (string/split c1 #" ")
                   c2s (string/split c2 #" ")]
               (string/join
                " " (concat c1s (remove (set c1s) c2s))))))

(defn into-attributes
  "Add attributes to an attribute map,
   correctly handling multiple classes or styles, or commands."
  [accumulator attributes]
  (combine-maps (fn [key v1 v2]
                  (case key
                    :class (merge-classes v1 v2)
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
  (if (seq attributes)
    (let [dom (if (sequential? dom) dom [:div dom])
          [dom-tag second & remainder] dom
          [attr remainder] (if (map? second)
                             [second remainder]
                             [{} (rest dom)])]
      (into [dom-tag (into-attributes attr attributes)]
            remainder))
    dom))

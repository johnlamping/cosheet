(ns cosheet.dom-utils)

(defn into-attributes
  "Add attributes to an attribute map,
   correctly handling multiple classes or styles, or commands."
  [accumulator attributes]
  (reduce (fn [accumulator [key value]]
            (update-in accumulator [key]
                       (fn [current]
                         (if current
                           (case key
                             :class (if (empty? value)
                                      current
                                      (str current " "
                                           (clojure.string/trim value)))
                             :style (into current value)
                             :commands (into current value))
                           value))))
          accumulator attributes))

(defn dom-attributes
  "Return the current specified attributes of a dom."
  [dom]
  (let [[dom-tag second & _] dom]
    (if (map? second) second {})))

(defn add-attributes
  "Add attributes to a hiccup dom descriptor."
  [dom attributes]
  (let [[dom-tag second & remainder] dom
        [attr remainder] (if (map? second)
                           [second remainder]
                           [{} (rest dom)])]
    (into [dom-tag
           (into-attributes attr attributes)]
          remainder)))

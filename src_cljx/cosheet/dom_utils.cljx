(ns cosheet.dom-utils)

(defn into-attributes
  "Add attributes to an attribute map,
   correctly handling multiple classes or styles."
  [accumulator attributes]
  (reduce (fn [accumulator [key value]]
            (update-in accumulator [key]
                       (fn [current]
                         (if current
                           (case key
                             :class (str current " " value)
                             :style (into current value))
                           value))))
          accumulator attributes))

(defn dom-attributes
  "Return the current specified attributes of a dom."
  [dom]
   (let [[dom-tag second & _] dom]
     (cond
       (= dom-tag :component) (:attributes second)
       (map? second) second
       :else {})))

(defn add-attributes
  "Add attributes to a hiccup dom descriptor."
  [dom attributes]
  (let [[dom-tag second & remainder] dom]
    (if (= dom-tag :component)
      (do (assert (empty? remainder))
          [:component
           (update-in second [:attributes] #(into-attributes % attributes))])
      (let [[attr remainder] (if (map? second)
                               [second remainder]
                               [{} (rest dom)])]
        (into [dom-tag
               (into-attributes attr attributes)]
              remainder)))))

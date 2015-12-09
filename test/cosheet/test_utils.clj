(ns cosheet.test-utils)

(def check)

(defn special-form? [pattern]
  (and (sequential? pattern) (= ::test (first pattern))))

(defn report-difference
  [value pattern]
  [:!= value pattern])

(defn check-sequence [value pattern]
  (let [vcount (count value)
        pcount (count pattern)
        padded-value (cond-> value (< vcount pcount)
                             (concat (repeat (- pcount vcount) nil)))
        padded-pattern (cond-> pattern (< pcount vcount)
                               (concat (repeat (- pcount vcount) nil)))
        diffs (map check padded-value padded-pattern)]
    (when (not-every? nil? diffs)
      (if (every? identity diffs)
        (report-difference value pattern)
        (drop-last (count (take-while nil? (reverse diffs))) diffs)))))

(defn check-set [value pattern]
  (let [unmatched-values (clojure.set/difference value pattern)
        unmatched-patterns (clojure.set/difference pattern value)]
    ;; The remainders might still match via check; try all combinations.
    (let [[unmatched-values unmatched-patterns]
          (reduce
           ;; In this reduce, unmatched values will start empty, and grow,
           ;; while unmatched-patterns starts at all of them, and shrinks.
           (fn [[unmatched-values unmatched-patterns] value]
             (let [match (some #(and (nil? (check value %)) %)
                               unmatched-patterns)]
               (if match
                 [unmatched-values (disj unmatched-patterns match)]
                 [(conj unmatched-values value) unmatched-patterns])))
           [#{} unmatched-patterns] unmatched-values)]
      (when (or (not (empty? unmatched-values))
                (not (empty? unmatched-patterns)))
        (report-difference unmatched-values unmatched-patterns)))))

(defn check-map [value pattern]
  (let [keys (clojure.set/union (set (keys value)) (set (keys pattern)))]
    (let [errors (reduce
                  (fn [errors key]
                    (let [error (check (value key) (pattern key))]
                      (cond-> errors error (assoc key error))))
                  {} keys)]
      (when (not (empty? errors))
        (if (= (count keys) (count errors))
          (report-difference value pattern)
          errors)))))

(def matchers {sequential? check-sequence
               map? check-map
               set? check-set})

(defn check
  "Check that the value matches the pattern, returning nil if it matches,
   and a subpart of the value if it doesn't, showing each location with
   a difference.
   A pattern can be a sequence, a map, or a set, or it can be the
   special form [::test fn & args, which will cause the function
   to be called with the value and the arguments, and will return
   whatever the function does.
   Typically, the special forms are created by a special form function."
  [value pattern]
  (if (special-form? pattern)
    (apply (second pattern) value (nnext pattern))
    (when (not= value pattern)
      (let [type (some #(and (% pattern) %) (keys matchers))]
        (if (and type (type value))
          ((matchers type) value pattern)
          (report-difference value pattern))))))

;;; Functions that make special forms

(defn- always-pass [value] nil)

(defn any [] [::test always-pass])

(defn- check-as-sets [value pattern]
  (check (set value) (set pattern)))

(defn as-set [pattern] [::test check-as-sets pattern])

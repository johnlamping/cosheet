(ns cosheet.test-utils
  (require [clojure.test :refer [assert-expr do-report]]))

(def differences)

(defn special-form? [pattern]
  (and (sequential? pattern) (= ::test (first pattern))))

(defn report-difference
  [value pattern]
  [:!= value pattern])

(defn sequence-differences [value pattern]
  (let [vcount (count value)
        pcount (count pattern)
        padded-value (cond-> value (< vcount pcount)
                             (concat (repeat (- pcount vcount) ::nothing)))
        padded-pattern (cond-> pattern (< pcount vcount)
                               (concat (repeat (- vcount pcount) ::nothing)))
        diffs (map differences padded-value padded-pattern)]
    (when (not-every? nil? diffs)
      (if (every? (fn [diff] (and (sequential? diff) (= :!= (first diff))))
                  diffs)
        (report-difference value pattern)
        (drop-last (count (take-while nil? (reverse diffs))) diffs)))))

(defn set-differences [value pattern]
  (let [unmatched-values (clojure.set/difference value pattern)
        unmatched-patterns (clojure.set/difference pattern value)]
    ;; The remainders might still match via differences; try all combinations.
    (let [[unmatched-values unmatched-patterns]
          (reduce
           ;; In this reduce, unmatched values will start empty, and grow,
           ;; while unmatched-patterns starts at all of them, and shrinks.
           (fn [[unmatched-values unmatched-patterns] value]
             (let [match (some #(and (nil? (differences value %)) %)
                               unmatched-patterns)]
               (if match
                 [unmatched-values (disj unmatched-patterns match)]
                 [(conj unmatched-values value) unmatched-patterns])))
           [#{} unmatched-patterns] unmatched-values)]
      (when (or (not (empty? unmatched-values))
                (not (empty? unmatched-patterns)))
        (report-difference unmatched-values unmatched-patterns)))))

(defn map-differences [value pattern]
  (let [all-keys (clojure.set/union (set (keys value)) (set (keys pattern)))]
    (let [errors (reduce
                  (fn [errors key]
                    (let [error (differences (get value key ::nothing)
                                             (get pattern key ::nothing))]
                      (cond-> errors error (assoc key error))))
                  {} all-keys)]
      (when (not (empty? errors))
        (if (empty? (clojure.set/intersection
                     (set (keys value)) (set (keys pattern))))
          (report-difference value pattern)
          errors)))))

(def matchers {sequential? sequence-differences
               map? map-differences
               set? set-differences})

(defn differences
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

(defn- no-difference [value] nil)
(defn- test-pred [value pred]
  (when (not (pred value))
    (report-difference value pred)))

(defn any
  ([] [::test no-difference])
  ([pred] [::test test-pred pred]))

(defn- differences-as-sets [value pattern]
  (differences (set value) (set pattern)))

(defn as-set [pattern] [::test differences-as-sets pattern])

;;; Define check as a macro for the is test.

(defmethod assert-expr 'check [msg form]
  (let [args (rest form)
        pred (first form)]
    `(let [values# (list ~@args)
           result# (apply differences values#)]
       (if result#
         ;; A non-nil result indicates a failure, and describes it.
         (do-report {:type :fail, :message ~msg,
                  :expected '~form, :actual result#})
         (do-report {:type :pass, :message ~msg,
                  :expected '~form, :actual (cons ~pred values#)}))
       result#)))


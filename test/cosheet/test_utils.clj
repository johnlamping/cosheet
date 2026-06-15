(ns cosheet.test-utils
  (:require [clojure.set]
            [clojure.test :refer [assert-expr do-report]]))

(declare differences as-set-pred)

(defn special-form? [pattern]
  (and (sequential? pattern) (= ::test (first pattern))))

(defn report-difference
  [value pattern]
  [:!= value pattern])

(defn differences-as-sets [value pattern]
  (if (and (sequential? value) (sequential? pattern))
    (differences (set value) (set pattern))
    (differences value pattern)))

(defn sequence-differences
  "Return [diff matches mismatches], where diff is nil if value matches
   pattern and otherwise describes their difference. The two sequences
   themselves count as one match (same type), plus all matches and
   mismatches from each position (padded to the longer length)."
  [value pattern]
  (let [vcount (count value)
        pcount (count pattern)
        padded-value (cond-> value (< vcount pcount)
                             (concat (repeat (- pcount vcount) ::nothing)))
        padded-pattern (cond-> pattern (< pcount vcount)
                               (concat (repeat (- vcount pcount) ::nothing)))
        results (map differences padded-value padded-pattern)
        diffs (map first results)
        matches (inc (reduce + (map second results)))
        mismatches (reduce + (map #(nth % 2) results))]
    [(when (not-every? nil? diffs)
       (if (every? (fn [diff] (and (sequential? diff) (= :!= (first diff))))
                   diffs)
         (report-difference value pattern)
         (vec (drop-last (count (take-while nil? (reverse diffs))) diffs))))
     matches mismatches]))

(defn- pair-score
  "Greedy-pairing score: matches^2 / (matches + mismatches). Higher
   is better."
  [matches mismatches]
  (let [total (+ matches mismatches)]
    (if (zero? total) 0.0 (/ (double (* matches matches)) total))))

(defn set-differences
  "Return [diff matches mismatches], where diff is nil if value matches
   pattern and otherwise describes their difference. The two sets
   themselves count as one match (same type), plus the matches and
   mismatches from pairing the elements: first by exact equality, then
   by `(differences v p)` returning nil, then by greedy pairing on
   pair-score (matches^2 / (matches + mismatches)). Leftover unpaired
   elements each count as one mismatch.
   When greedy partial matches occurred, diff is a set of the per-pair
   diffs, plus a final [:!= <unmatched-values> <unmatched-patterns>]
   entry if anything is still unmatched."
  [value pattern]
  (let [intersection (clojure.set/intersection value pattern)
        exact-matches (reduce + (map #(second (differences % %))
                                     intersection))
        unmatched-values (clojure.set/difference value pattern)
        unmatched-patterns (clojure.set/difference pattern value)
        ;; The remainders might still match via differences; try all
        ;; combinations, and capture how many matches the pairing
        ;; contributes.
        [unmatched-values unmatched-patterns diff-nil-matches]
        (reduce
         (fn [[unmatched-values unmatched-patterns acc] v]
           (let [match (some (fn [p]
                               (let [[d m _] (differences v p)]
                                 (when (nil? d) [p m])))
                             unmatched-patterns)]
             (if match
               [unmatched-values
                (disj unmatched-patterns (first match))
                (+ acc (second match))]
               [(conj unmatched-values v) unmatched-patterns acc])))
         [#{} unmatched-patterns 0] unmatched-values)
        base-matches (+ 1 exact-matches diff-nil-matches)]
    (if (and (empty? unmatched-values) (empty? unmatched-patterns))
      [nil base-matches 0]
      (if (or (empty? unmatched-values) (empty? unmatched-patterns))
        [(report-difference unmatched-values unmatched-patterns)
         base-matches
         (max (count unmatched-values)
              (count unmatched-patterns))]
        ;; Build pair-info once, keeping only pairs with positive
        ;; matches. Then greedily pick the highest-scoring pair and
        ;; remove any pair sharing its value or pattern. If no pair
        ;; has positive matches, fall back to a single diff entry.
        (let [pair-info
              (vec (for [v unmatched-values p unmatched-patterns
                         :let [[d m mm] (differences v p)]
                         :when (and (pos? m) (>= m mm))]
                     [v p d m mm (pair-score m mm)]))]
          (if (empty? pair-info)
            [(report-difference unmatched-values unmatched-patterns)
             base-matches
             (max (count unmatched-values)
                  (count unmatched-patterns))]
            (loop [remaining pair-info
                   pair-diffs #{}
                   acc-matches 0
                   acc-mismatches 0
                   leftover-vals unmatched-values
                   leftover-patterns unmatched-patterns]
              (if (empty? remaining)
                [(cond-> pair-diffs
                   (or (seq leftover-vals) (seq leftover-patterns))
                   (conj (report-difference leftover-vals
                                            leftover-patterns)))
                 (+ base-matches acc-matches)
                 (+ acc-mismatches
                    (max (count leftover-vals)
                         (count leftover-patterns)))]
                (let [[bv bp bd bm bmm _]
                      (apply max-key #(nth % 5) remaining)]
                  (recur (vec (remove (fn [[v p _ _ _ _]]
                                        (or (= v bv) (= p bp)))
                                      remaining))
                         (conj pair-diffs bd)
                         (+ acc-matches bm)
                         (+ acc-mismatches bmm)
                         (disj leftover-vals bv)
                         (disj leftover-patterns bp)))))))))))

(defn map-differences
  "Return [diff matches mismatches], where diff is nil if value matches
   pattern and otherwise describes their difference. The two maps
   themselves count as one match (same type), plus the matches and
   mismatches from each key (over the union of both maps' keys)."
  [value pattern]
  (let [all-keys (clojure.set/union (set (keys value)) (set (keys pattern)))
        per-key (reduce
                 (fn [acc key]
                   (let [[error m mm] (differences
                                       (get value key ::nothing)
                                       (get pattern key ::nothing))]
                     (-> acc
                         (update :matches + m)
                         (update :mismatches + mm)
                         (cond-> error (update :errors assoc key error)))))
                 {:errors {} :matches 0 :mismatches 0} all-keys)
        errors (:errors per-key)
        matches (inc (:matches per-key))
        mismatches (:mismatches per-key)
        diff
        (when (not (empty? errors))
          (if (empty? (clojure.set/intersection
                       (set (keys value)) (set (keys pattern))))
            (report-difference value pattern)
            errors))]
    [diff matches mismatches]))

(def matchers {sequential? sequence-differences
               map? map-differences
               set? set-differences})

(defn differences
  "Check that the value matches the pattern. Returns
   [diff matches mismatches], where diff is nil if value matches
   pattern and describes a difference otherwise, matches is the count
   of items that matched, and mismatches is the count of items that
   did not. Two items of the same type count as one match plus all of
   their submatches.
   A pattern can be a sequence, a map, or a set, or it can be the
   special form [::test fn & args], which will cause the function
   to be called with the value and the arguments. The function is
   expected to return a diff (nil for match, otherwise a description);
   its (matches, mismatches) are inferred as (1, 0) on match and
   (0, 1) on mismatch.
   As a special case, when the special form's function is as-set-pred,
   differences-as-sets is called directly so that its
   [diff matches mismatches] triple is preserved."
  [value pattern]
  (cond
    (special-form? pattern)
    (let [f (second pattern)
          args (nnext pattern)]
      (if (= f as-set-pred)
        (apply differences-as-sets value args)
        (let [result (apply f value args)]
          (if (nil? result) [nil 1 0] [result 0 1]))))
    :else
    (let [type (some #(and (% pattern) %) (keys matchers))]
      (if (and type (type value))
        ((matchers type) value pattern)
        (if (= value pattern)
          [nil 1 0]
          [(report-difference value pattern) 0 1])))))

;;; Functions that make special forms

(defn- anything [value]
  (when (= value ::nothing)
    (report-difference value "anything")))
(defn- test-pred [value pred]
  (when (not (pred value))
    [:not pred value]))

(defn any
  ([] [::test anything])
  ([pred] [::test test-pred pred]))

(defn- as-set-pred [value pattern]
  (first (differences-as-sets value pattern)))

(defn as-set [pattern] [::test as-set-pred pattern])

;;; Define check as a macro for the is test.

;;; Used in (is (check <value> <pattern>))
;;; Handled by the method on assert-expr.
(def check)

(defmethod assert-expr 'check [msg form]
  (let [args (rest form)
        pred (first form)]
    `(let [values# (list ~@args)
           result# (apply differences values#)
           diff# (first result#)]
       (if diff#
         ;; A non-nil diff indicates a failure, and describes it.
         (do-report {:type :fail, :message ~msg,
                     :expected '~form, :actual diff#})
         (do-report {:type :pass, :message ~msg,
                     :expected '~form, :actual (cons ~pred values#)}))
       diff#)))

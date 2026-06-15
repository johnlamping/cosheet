(ns cosheet.test-utils-test
  (:require [clojure.test :refer [deftest is]]
            [cosheet.test-utils :refer [check differences map-differences
                                        sequence-differences set-differences
                                        any as-set]]))

(deftest sequence-differences-test
  ;; Matching sequences: 1 for the type plus 1 per matched position.
  (is (= (sequence-differences [1 2 3] [1 2 3]) [nil 4 0]))
  ;; Empty sequences match on type only.
  (is (= (sequence-differences [] []) [nil 1 0]))
  ;; All-different scalars: type matches, every position mismatches.
  (is (= (sequence-differences [1 2 #{3}] [4 5 6])
         [[:!= [1 2 #{3}] [4 5 6]] 1 3]))
  ;; Partial-match sequence: 2 of 3 positions match.
  (is (= (sequence-differences [1 2 3] [1 2 #{4}])
         [[nil nil [:!= 3 #{4}]] 3 1]))
  ;; Unequal lengths: pad to the longer length.
  (is (= (sequence-differences [1 2] [1 2 3])
         [[nil nil [:!= :cosheet.test-utils/nothing 3]] 3 1]))
  ;; Nested sequence mismatch contributes submatches and submismatches.
  ;; Per position: (3, 1). Total: 1 + 3 + 3 = 7 matches, 0 + 1 + 1 = 2
  ;; mismatches.
  (is (= (sequence-differences [[1 2 3] [4 5 6]] [[1 2 4] [4 5 7]])
         [[[nil nil [:!= 3 4]] [nil nil [:!= 6 7]]] 7 2])))

(deftest set-differences-test
  ;; Matching sets: 1 type match + 1 per element.
  (is (= (set-differences #{1 2 3} #{1 2 3}) [nil 4 0]))
  ;; Empty sets match on type only.
  (is (= (set-differences #{} #{}) [nil 1 0]))
  ;; All-different scalars: only the type matches; each element is one
  ;; mismatch.
  (is (= (set-differences #{1 2 3} #{4 5 6})
         [[:!= #{1 2 3} #{4 5 6}] 1 3]))
  ;; Partial match: 2 exact + 1 mismatch.
  (is (= (set-differences #{1 2 3} #{1 2 4})
         [[:!= #{3} #{4}] 3 1]))
  ;; Partial matches inside set.
  (is (= (set-differences #{[1 2 3] [4 6] 7} #{[1 2] [5 6] 7})
         [#{[nil nil [:!= 3 :cosheet.test-utils/nothing]]
            [[:!= 4 5]]}
          7 2]))
  ;; Two sets of partially-overlapping sequences: greedy pairing should
  ;; pair them up by pair-score and emit a set of per-pair diffs (no
  ;; leftover entry since everything got paired).
  (is (= (set-differences #{[1 2 3] [4 5 6]} #{[1 2 4] [4 5 7]})
         [#{[nil nil [:!= 3 4]] [nil nil [:!= 6 7]]} 7 2]))
  ;; When greedy pairing runs out of positively-scored pairs, the
  ;; remaining elements show up in a final [:!= ...] entry within the
  ;; set. Scalars against scalars (different types in this mix) score
  ;; 0 and end up unpaired.
  (is (= (set-differences #{[1 2 3] 10} #{[1 2 4] :foo})
         [#{[nil nil [:!= 3 4]] [:!= #{10} #{:foo}]} 4 2]))
  ;; Greedy pairing prefers the higher pair-score. Pairing the inner
  ;; set with #{[1 2 3 4 5]} matches the most of the long vector (4
  ;; submatches) and leaves the two scalars as mismatches. That scores
  ;; higher than pairing with #{{:a 1} :b}, which matches both other
  ;; items but leaves the long vector unmatched).
  (is (= (set-differences #{#{[0 2 3 4 5] {:a 1} :b}}
                          #{#{[1 2 3 4 5]} #{{:a 1} :b}})
           [#{#{[[:!= 0 1]] ; Nested error
                [:!= #{{:a 1} :b} #{}]} ; Unmatched values
              [:!= #{} #{#{{:a 1} :b}}]} ; Unmatched pattern
            7 4])
      [#{[:!= #{{:a 1} :b} #{}] [:!= #{} #{#{{:a 1} :b}}]} 6 3])
  ;; Now, we shorten the vector, and the better match is now to go
  ;; with the other two items.
  (is (= (set-differences #{#{[0 2 3] {:a 1} :b}}
                          #{#{[1 2 3]} #{{:a 1} :b}})
         [#{[:!= #{} #{#{[1 2 3]}}] ; Unmatched value
            [:!= #{[0 2 3]} #{}]} ; Unmatched pattern
          5 2]))
  ;; When no pair has positive score, fall back to a single
  ;; [:!= unmatched-vals unmatched-patterns] entry.
  ;; matches = 1 (set type), mismatches = max(2, 2) = 2.
  (is (= (set-differences #{1 2} #{3 4})
         [[:!= #{1 2} #{3 4}] 1 2]))
  ;; When no pair matches on a significant fraction, we should see
  ;; just a complete set difference
  (is (= (set-differences #{[1 2 3 0]} #{[4 5 6 0]})
         [[:!= #{[1 2 3 0]} #{[4 5 6 0]}] 1 1])))

(deftest map-differences-test
  ;; Matching maps: 1 type match + 1 per key.
  (is (= (map-differences {:a 1 :b 2} {:a 1 :b 2}) [nil 3 0]))
  ;; Empty maps: type only.
  (is (= (map-differences {} {}) [nil 1 0]))
  ;; Disjoint keys: type matches but each key only present on one side
  ;; is a mismatch.
  (is (= (map-differences {:a 1} {:b 2})
         [[:!= {:a 1} {:b 2}] 1 2]))
  ;; One mismatched value: 1 of 2 keys match.
  (is (= (map-differences {:a 1 :b 2} {:a 1 :b 3})
         [{:b [:!= 2 3]} 2 1]))
  ;; A nested mismatch contributes per-key submatches and submismatches.
  ;; :a fully matches (1, 0); :b's nested seq contributes (3, 1).
  ;; Total: 1 (map type) + 1 + 3 = 5 matches; 0 + 1 = 1 mismatch.
  (is (= (map-differences {:a 1 :b [1 2 3]} {:a 1 :b [1 2 4]})
         [{:b [nil nil [:!= 3 4]]} 5 1])))

(deftest differences-test
  ;; differences returns [diff matches mismatches]. On a match, diff is
  ;; nil and matches reflects type + submatches.
  (is (= (differences 1 1) [nil 1 0]))
  (is (= (differences [1 2 3] [1 2 3]) [nil 4 0]))
  (is (= (differences #{1 2 3} #{1 2 3}) [nil 4 0]))
  (is (= (differences [1 2 3] [1 2 4]) [[nil nil [:!= 3 4]] 3 1]))
  (is (= (differences #{1 2 3} #{1 2 4}) [[:!= #{3} #{4}] 3 1]))
  (is (= (differences {:a 1 :b 2} {:a 1 :b 2}) [nil 3 0]))
  (is (= (differences {:a 1 :b 2} {:a 1 :b 3}) [{:b [:!= 2 3]} 2 1]))
  ;; Different types.
  (is (= (differences [1 2 3] #{1 2 3})
         [[:!= [1 2 3] #{1 2 3}] 0 1]))
  ;; Special form matches: (1, 0); mismatches: (0, 1).
  (is (= (differences 42 (any)) [nil 1 0]))
  (is (= (differences 42 (any string?)) [[:not string? 42] 0 1])))

(deftest check-test
  ;; check passes for matches.
  (is (check 1 1))
  (is (check [1 2 3] [1 2 3]))
  (is (check #{1 2 3} #{1 2 3}))
  (is (check 42 (any)))
  (is (check [1 2 3] (as-set [3 2 1])))
  ;; check fails for mismatches: it reports :fail with the diff in
  ;; :actual. Bind clojure.test/report to capture rather than emit, so
  ;; the intentional failures don't fail this test.
  (let [captured (atom [])]
    (binding [clojure.test/report (fn [m] (swap! captured conj m))]
      (is (check 1 2))
      (is (check [1 2 3] [1 2 4]))
      (is (check 42 (any string?))))
    (is (= [:fail :fail :fail] (mapv :type @captured)))
    (is (= [[:!= 1 2]
            [nil nil [:!= 3 4]]
            [:not string? 42]]
           (mapv :actual @captured)))))

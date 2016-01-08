(ns cosheet.test-utils-test
  (:require [clojure.test :refer [deftest is assert-expr do-report]]
            [cosheet.test-utils :refer [differences any as-set]]
            ; :reload
            ))

(deftest differences-test
  (is (= (differences 1 1) nil))
  (is (= (differences 1 2) [:!= 1 2]))
  (is (= (differences 1 (any)) nil))
  (is (= (differences 1 (any number?)) nil))
  (is (= (differences 1 (any map?)) [:!= 1 map?]))
  (is (= (differences [1 2] [1 2]) nil))
  (is (= (differences [1 2] [1 (any)]) nil))
  (is (= (differences [1 2] [1 3]) [nil [:!= 2 3]]))
  (is (= (differences [1 2] [3 4]) [:!= [1 2] [3 4]]))
  (is (= (differences [1 2] [1 2 3])
         [nil nil [:!= :cosheet.test-utils/nothing 3]]))
  (is (= (differences [1 2 3] [1 2])
         [nil nil [:!= 3 :cosheet.test-utils/nothing]]))
  (is (= (differences [1 2 4 5 6] [1 2 3 5 6]) [nil nil [:!= 4 3]]))
  (is (= (differences #{1 2 3} #{2 3 4 5}) [:!= #{1} #{4 5}]))
  (is (= (differences #{1 2 3} #{2 3}) [:!= #{1} #{}]))
  (is (= (differences #{2 3} #{2 3 4 5}) [:!= #{} #{4 5}]))
  (is (= (differences #{1 2} #{1 (any)}) nil))
  (is (= (differences #{1 2 [1]} #{1 [(any)] [2 (any)]})
         [:!= #{2} #{[2 (any)]}]))
  (is (= (differences [1 2] (as-set [2 1])) nil))
  (is (= (differences [1 2] (as-set [3 1])) [:!= #{2} #{3}]))
  (is (= (differences 1 (as-set [2 1])) [:!= 1 [2 1]]))
  (is (= (differences [1 2 3] (as-set [2 1])) [:!= #{3} #{}]))
  (is (= (differences {1 2, 3 4} {1 2, 3 (any)}) nil))
  (is (= (differences {1 2} {3 4}) [:!= {1 2} {3 4}]))
  (is (= (differences {1 2} {1 4}) {1 [:!= 2 4]}))
  (is (= (differences {1 2, 3 5} {1 4, 3 5}) {1 [:!= 2 4]}))
  (is (= (differences {1 2, 5 6} {1 2, 3 4})
         {5 [:!= 6 :cosheet.test-utils/nothing]
          3 [:!= :cosheet.test-utils/nothing 4]}))
  (is (= (differences {1 [9 10]
                       3 [5 6 #{7 8} 4]}
                      {1 (as-set [10 9])
                       3 [5 6 #{7 8 9} 4]})
         {3 [nil nil [:!= #{} #{9}]]}))
  (is (= (differences [{:info {:a 1} :members [:i]}]
                      [{:info {:a 2} :members [:i]}])
         [{:info {:a [:!= 1 2]}}])))


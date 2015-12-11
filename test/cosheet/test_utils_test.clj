(ns cosheet.test-utils-test
  (:require [clojure.test :refer [deftest is]]
            [cosheet.test-utils :refer :all]
            ; :reload
            ))

(deftest check-test
  (is (= (check 1 1) nil))
  (is (= (check 1 2) [:!= 1 2]))
  (is (= (check 1 (any)) nil))
  (is (= (check 1 (any number?)) nil))
  (is (= (check 1 (any map?)) [:!= 1 map?]))
  (is (= (check [1 2] [1 2]) nil))
  (is (= (check [1 2] [1 (any)]) nil))
  (is (= (check [1 2] [1 3]) [nil [:!= 2 3]]))
  (is (= (check [1 2] [3 4]) [:!= [1 2] [3 4]]))
  (is (= (check [1 2] [1 2 3]) [nil nil [:!= :cosheet.test-utils/nothing 3]]))
  (is (= (check [1 2 3] [1 2]) [nil nil [:!= 3 :cosheet.test-utils/nothing]]))
  (is (= (check [1 2 4 5 6] [1 2 3 5 6]) [nil nil [:!= 4 3]]))
  (is (= (check #{1 2 3} #{2 3 4 5}) [:!= #{1} #{4 5}]))
  (is (= (check #{1 2 3} #{2 3}) [:!= #{1} #{}]))
  (is (= (check #{2 3} #{2 3 4 5}) [:!= #{} #{4 5}]))
  (is (= (check #{1 2} #{1 (any)}) nil))
  (is (= (check #{1 2 [1]} #{1 [(any)] [2 (any)]}) [:!= #{2} #{[2 (any)]}]))
  (is (= (check [1 2] (as-set [2 1])) nil))
  (is (= (check [1 2 3] (as-set [2 1])) [:!= #{3} #{}]))
  (is (= (check {1 2, 3 4} {1 2, 3 (any)}) nil))
  (is (= (check {1 2} {3 4}) [:!= {1 2} {3 4}]))
  (is (= (check {1 2} {1 4}) {1 [:!= 2 4]}))
  (is (= (check {1 2, 3 5} {1 4, 3 5}) {1 [:!= 2 4]}))
  (is (= (check {1 2, 5 6} {1 2, 3 4})
         {5 [:!= 6 :cosheet.test-utils/nothing]
          3 [:!= :cosheet.test-utils/nothing 4]}))
  (is (= (check {1 [9 10]
                 3 [5 6 #{7 8} 4]}
                {1 (as-set [10 9])
                 3 [5 6 #{7 8 9} 4]})
         {3 [nil nil [:!= #{} #{9}]]}))
  (is (= (check [{:info {:a 1} :members [:i]}]
                [{:info {:a 2} :members [:i]}])
         [{:info {:a [:!= 1 2]}}])))

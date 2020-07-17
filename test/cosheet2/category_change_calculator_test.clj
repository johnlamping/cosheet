(ns cosheet2.category-change-calculator-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet2 [task-queue :refer [new-priority-task-queue]]
                      [reporter  :refer [new-reporter reporter-value
                                         set-value! change-value!]]
                      [calculator :refer [new-calculator-data
                                          compute request unrequest
                                          computation-value
                                          modify-and-act]]
                      [expression :refer [category-change]]
                      [utils :refer :all]
                      [category-change-calculator :refer :all]
                      [cache-calculator :refer [canonicalize-reporter]]
                      [test-utils :refer [check any]]                      
                      [propagation-test-utils :refer [check-propagation]])
            ; :reload
            ))

(deftest category-change-calculator-test
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        r (new-reporter :name :r :value 1)
        r1 (category-change [1 2] r)]
    (request r1 cd)
    (compute cd)
    (is (= (canonicalize-reporter r1) r))
    (is (= (reporter-value r1) 1))
    ;; Check that we see an unmarked change
    (set-value! r 2)
    (compute cd)
    (is (= (reporter-value r1) 2))
    ;; Check that we don't see a change whose categories we don't care about.
    (change-value! r (fn [v] [3 :change [3]]))
    (compute cd)
    (is (= (reporter-value r1) 2))
    ;; Check that we do see a change whose categories we do care about.
    (change-value! r (fn [v] [1 :change [3 1]]))
    (compute cd)
    (is (= (reporter-value r1) 1))))

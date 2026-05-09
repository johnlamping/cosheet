(ns cosheet2.category-change-calculator-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet2 [task-queue :refer [new-priority-task-queue]]
                      [reporter  :refer [make-reporter
                                         reporter-value reporter-data
                                         set-value! change-value! valid?]]
                      [calculator :refer [new-calculator-data
                                          compute request unrequest]]
                      [utils :refer :all]
                      [category-change-calculator :refer :all]
                      [test-utils :refer [check any]])
            ; :reload
            ))

(deftest category-change-calculator-test
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        r (make-reporter :name :r :value 1)
        r1 (category-change-R [1 2] r)]
    (request r1 cd)
    (compute cd)
    (is (= (:value-source (reporter-data r1)) r))
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
    (is (= (reporter-value r1) 1))
    (unrequest r1)
    (is (not (valid? r1)))))

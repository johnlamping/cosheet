(ns cosheet2.expression-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet2 [reporter :refer [new-reporter reporter-data
                                        universal-category invalid]]
                      [expression :refer :all]
                      [application-calculator :refer [application-calculator]]
                      [cache-calculator :refer [cache-calculator]]
                      [category-change-calculator
                       :refer [category-change-calculator category-change-R]]
                      [calculator :refer [current-value]])
            ; :reload
            ))

(deftest expression-test
  (let [r (new-reporter)]
    (is (= (dissoc (reporter-data (app-R r 2 3)) :trace)
           {:application [r 2 3]
            :calculator application-calculator
            :value invalid
            :valid false
            :priority Double/MAX_VALUE}))
    (is (= (dissoc (reporter-data (cache-R r 2 3)) :trace)
           {:application [r 2 3]
            :calculator cache-calculator
            :value invalid
            :valid false
            :priority Double/MAX_VALUE
            :value-source-priority-delta 1
            :cache-key [r 2 3]}))
    (is (= (reporter-data (category-change-R [2 3] r))
           {:categories [2 3]
            :calculator category-change-calculator
            :value-source r
            :value invalid
            :valid false
            :priority Double/MAX_VALUE}))
    (is (= (category-change-R nil r) r))
    (is (= (category-change-R [universal-category] r) r)))
  
  ;; Try cases where the expression should evaluate to a constant.
  (is (= (app-R + (app-R inc 1) 3)
         5))
  (is (= (let-R [x 1 y 2]
           (+ (* 3 x) y))
         5))
  (is (= (let-R [x 1 y x]
           (* 3 y))
         3))
  (is (= (let-R [[x y] [1 3]
                 z (+ x y)]
           z)
         4))
  (is (= (seq-R (app-R map
                       (fn [x] (app-R inc x))
                       [1 (app-R inc 1) 3]))
         [2 3 4]))
  
  ;; Try cases where the expression references a reporter.
  (let [r3 (new-reporter :value 3)]
    (is (= (current-value (app-R + (app-R inc 1) r3))
           5))
    (is (= (current-value (cache-R + (cache-R inc 1) r3))
           5))
    (is (= (current-value (let-R [x 1 y 2]
                            (app-R + (app-R * r3 x) y)))
           5))
    (is (= (current-value (let-R [x 1 y x]
                            (app-R * r3 y)))
           3))
    (is (= (current-value (let-R [[x y] (app-R vector 1 r3)
                                   z (+ x y)]
                            z))
           4))
    (is (= (current-value (seq-R (app-R map
                                        (fn [x] (app-R inc x))
                                        [1 (app-R inc 1) r3])))
           [2 3 4]))))






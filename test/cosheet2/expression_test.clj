(ns cosheet2.expression-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet2 [reporter :as reporter]
                      [expression :refer :all]
                      [application-calculator :refer [application-calculator]]
                      [calculator :refer [current-value]])
            ; :reload
            ))

(deftest expression-test
  (let [r (reporter/new-reporter)]
    (is (= (dissoc (reporter/data (expr r 2 3)) :trace)
           {:application [r 2 3]
            :calculator application-calculator
            :value invalid
            :priority Double/MAX_VALUE})))
  
  ;; Try cases where the expression should evaluate to a constant.
  (is (= (expr + (expr inc 1) 3)
         5))
  (is (= (expr-let [x 1 y 2]
           (+ (* 3 x) y))
         5))
  (is (= (expr-let [x 1 y x]
           (* 3 y))
         3))
  (is (= (expr-let [[x y] [1 3]
                    z (+ x y)]
           z)
         4))
  (is (= (expr-seq map
                   (fn [x] (expr inc x))
                   [1 (expr inc 1) 3])
         [2 3 4]))
  (is (= (expr-filter #(= (mod % 3) 0)
                      [1 2 3 4 5 6])
         [3 6]))
  
  ;; Try cases where the expression references a reporter.
  (let [r3 (reporter/new-reporter :value 3)]
    (is (= (current-value (expr + (expr inc 1) r3))
           5))
    ;; TODO: Put this back when cache is implemented.
    ;;  (is (= (current-value (cache + (cache inc 1) r3))
    ;;      5))
    (is (= (current-value (expr-let [x 1 y 2]
                            (expr + (expr * r3 x) y)))
           5))
    (is (= (current-value (expr-let [x 1 y x]
                            (expr * r3 y)))
           3))
    (is (= (current-value (expr-let [[x y] (expr vector 1 r3)
                                     z (+ x y)]
                            z))
           4))
    (is (= (current-value (expr-seq map
                                    (fn [x] (expr inc x))
                                    [1 (expr inc 1) r3]))
           [2 3 4]))
    
    (is (= (current-value (expr-filter #(expr = (expr mod % r3) 0)
                                       [1 2 r3 4 5 6]))
           [3 6]))))






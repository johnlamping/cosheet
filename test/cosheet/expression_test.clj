(ns cosheet.expression-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [reporters :as reporter]
                     [expression :refer :all]
                     [debug :refer [current-value trace-current]])
            ; :reload
            ))

(deftest expression-test
  (let [r (reporter/new-reporter)]
    (is (= (dissoc (reporter/data (expr r 2 3)) :trace)
           {:expression [r 2 3] :manager-type :eval :value invalid})))
  (is (= (current-value (expr + (expr inc 1) 3)) 5))
  (is (= (current-value (cache + (cache inc 1) 3)) 5))
  (is (= (current-value (expr-let [x 1 y 2] (+ (* 3 x) y))) 5))
  (is (= (current-value (expr-let [x 1 y x] (* 3 y))) 3))
  (is (= (current-value (expr-let [[x y] [1 2] z (+ x y)] z)) 3))
  (is (= (current-value (expr-seq map
                                  (fn [x] (expr inc x))
                                  [1 (expr inc 1) 3]))
         [2 3 4])))






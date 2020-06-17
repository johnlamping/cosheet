(ns cosheet2.expression-manager-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet2 [reporter :refer [new-reporter set-value! data data-value
                                        valid? reporter?]]
                      [calculator :refer :all])
            ; :reload
            ))

(defn fib [n s]
  (if (<= n 1)
    s   
    (new-reporter :application [+ (fib (- n 1) s) (fib (- n 2) s)]
                  :calculator (fn [& _] nil))))

(deftest new-calculator-data-test
  (let [cd (new-calculator-data (atom :q))]
    (is (= @(:queue cd) :q))
    (is (not (nil? (:cache cd))))))

(defn activated? [r]
  (if (reporter? r)
    (let [data (data r)]
      (and (or (= (:calculator-data data) :cd)
               (valid? (data-value data)))
           (every? activated? (:application data))))
    true))

(deftest propagate-calculator-data!-test
  (let [state (new-reporter :value 0)
        f6 (fib 6 state)]
    (is (not (activated? f6)))
    (propagate-calculator-data! f6 :cd)
    (is (activated? f6))))

(deftest current-value-test
  (let [state (new-reporter :value 0)
        fib6 (fib 6 state)]
    (is (= (current-value fib6) 0))
    (set-value! state 1)
    (is (= (current-value fib6) 13))))

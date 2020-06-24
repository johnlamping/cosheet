(ns cosheet2.cache-calculator-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet2 [mutable-map :as mm]
                      [task-queue :refer [current-tasks
                                          new-priority-task-queue]]
                      [reporter :as reporter]
                      [calculator :refer [new-calculator-data current-value
                                          compute request unrequest
                                          computation-value
                                          modify-and-act]]
                      [expression :refer [expr cache expr-seq expr-let new-application]]
                      [utils :refer :all]
                      [cache-calculator :refer :all]
                      [test-utils :refer [check any]]                      
                      [propagation-test-utils :refer [check-propagation]])
            ; :reload
            ))

(deftest canonicalize-application-test
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        r0 (reporter/new-reporter :name :r0 :application [:a :b])
        r1 (reporter/new-reporter :name :r1 :application [:a :b])
        r00 (reporter/new-reporter :name :r0 :application [r0 r0])
        r11 (reporter/new-reporter :name :r2 :application [r1 r1])]
    (request r0 cd)
    (adjust-cache-membership r0 cd)
    (request r00 cd)
    (adjust-cache-membership r00 cd)
    (is (= (canonicalize-application [r1 r0] cd) [r0 r0]))
    (is (= (canonicalize-application [r1 r11] cd) [r0 r00]))))

(deftest cache-membership-test
   (let [cd (new-calculator-data (new-priority-task-queue 0))
         r0 (get-or-make-reporter [:a :b] "ab" cd)]
     (is (= (:application (reporter/data r0)) [:a :b]))
     (is (= (:calculator-data (reporter/data r0)) cd))
     (is (not= (get-or-make-reporter [:a :b] "ab" cd) r0))
     (request r0 cd)
     (adjust-cache-membership r0 cd)
     (is (= (get-or-make-reporter [:a :b] "ab" cd) r0))
     (unrequest r0 cd)
     (adjust-cache-membership r0 cd)
     (is (not= (get-or-make-reporter [:a :b] "ab" cd) r0))))

(deftest cache-manager-test
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        r0 (reporter/new-reporter :name :r0 :value 1)
        r1 (reporter/new-reporter :name :r1
                                  :application [inc r0]
                                  :calculator cache-calculator
                                  :calculator-data cd)
        r2 (reporter/new-reporter :name :r2
                                  :application [inc r0]
                                  :calculator cache-calculator
                                  :calculator-data cd)]
    ;; Nothing should happen when there is no demand for r1.
    (cache-calculator r1 cd)
    (is (not (contains? (reporter/data r1) :value-source)))
    ;; With demand, the cached reporter should be created.
    (request r1 cd)
    (is (contains? (reporter/data r1) :value-source))
    ;; And we should pick it up for the other reporter with the same expr.
    (request r2 cd)
    (is (= (:value-source  (reporter/data r1))
           (:value-source  (reporter/data r2))))
    (is (= (:value-source  (reporter/data r1))
           (mm/mm-get (:cache cd) [inc r0])))
    (let [orig-source (:value-source  (reporter/data r1))]
      ;; Lose interest in r1 then get it back, and the same value
      ;; source should come back.
      (unrequest r1 cd)
      (is (not (contains? (reporter/data r1) :value-source)))
      (request r1 cd)
      (is (= (:value-source (reporter/data r1))
             orig-source))
      ;; Now, lose interest in both reporters with that application,
      ;; and the cache should drop it.
      (unrequest r1 cd)
      (unrequest r2 cd)
      (is (not= (:value-source (reporter/data r1))
                orig-source))
      (is (nil? (mm/mm-get (:cache cd) [inc r0]))))))

;;; Test that caching is working by doing a recursive computation that would
;;; take a very long time if it weren't cached.
(deftest fib-cache-test
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        base (reporter/new-reporter :value 0)]
    (letfn [(fib [n] (if (<= n 1)
                       base
                       (expr + (cache fib (- n 1)) (cache fib (- n 2)))))]
      ;; Since the base is 0, fib should be 0 everywhere, and since
      ;; the computations should be cached, this should be fast.
      (let [f45 (fib 45)]
        (is (= (computation-value f45 cd) 0))
        (check-propagation f45)
        (reporter/set-value! base 1)
        ;; Now it should be the right value.
        (is (= (computation-value f45 cd) 1836311903))
        (check-propagation f45)
        (reporter/set-value! base reporter/invalid)
        ;; Now it should be invalid.
        (is (= (not (reporter/valid? (computation-value f45 cd)))))
        (check-propagation f45)
        (unrequest f45 cd)
        (compute cd)
        (is (= (mm/current-contents (:cache cd))) {})))))

;; Test that caching works with recomputations of subsidiary
;; computations. This tests that :old-value-source of
;; application reporters is getting kept around long enough.
(deftest reuse-test
  (let [r1 (reporter/new-reporter :value 1)
        rs (reporter/new-reporter :value [1 2 3])
        counter (atom 0)
        counting-plus (fn counting-plus [x y]
                        (swap! counter inc)
                        (+ x y))
        dependency-introducer (fn [x] (cache counting-plus r1 x))
        r (expr-let [s1 (expr-seq map dependency-introducer rs)
                     s2 (expr-seq map dependency-introducer s1)]
            s2)
        cd (new-calculator-data (new-priority-task-queue 0))]
    (is (= (computation-value r cd) [3 4 5]))
    (is (= @counter 4))
    (reporter/set-value! rs [1 2 3 4])
    (is (= (computation-value r cd) [3 4 5 6]))
    (is (= @counter 5))))

(ns cosheet2.cache-calculator-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet2 [mutable-map :refer [mm-get current-contents]]
                      [task-queue :refer [new-priority-task-queue
                                          run-all-pending-tasks]]
                      [reporter :refer [new-reporter reporter-data
                                        reporter-value
                                        set-value! valid? invalid]]
                      [calculator :refer [new-calculator-data current-value
                                          compute request unrequest
                                          computation-value]]
                      [expression :refer [expr cache expr-seq expr-let]]
                      [utils :refer :all]
                      [cache-calculator :refer :all]
                      [test-utils :refer [check any]]                      
                      [propagation-test-utils :refer [check-propagation]])
            ; :reload
            ))

(deftest canonicalize-application-test
  (let [r0 (new-reporter :name :r0 :application [:a :b])
        r1 (new-reporter :name :r1 :application [:a :b]
                         :value-source r0 :value-source-is-canonical true)
        r2 (new-reporter :name :r1 :application [:a :b]
                         :value-source r0)
        r00 (new-reporter :name :r0 :application [r0 r0])
        r11 (new-reporter :name :r2 :application [r1 r1]
                          :value-source r00 :value-source-is-canonical true)]
    (is (= (canonicalize-application [r1 r0]) [r0 r0]))
    (is (= (canonicalize-application [r2 r0]) [r2 r0]))
    (is (= (canonicalize-application [r1 r11]) [r0 r00]))))

(deftest cache-membership-test
   (let [cd (new-calculator-data (new-priority-task-queue 0))
         r0 (get-or-make-reporter [:a :b] "ab" cd)]
     (is (= (:application (reporter-data r0)) [:a :b]))
     (is (= (:calculator-data (reporter-data r0)) cd))
     (is (not= (get-or-make-reporter [:a :b] "ab" cd) r0))
     (request r0 cd)
     (adjust-cache-membership r0 cd)
     (is (= (get-or-make-reporter [:a :b] "ab" cd) r0))
     (unrequest r0 cd)
     (adjust-cache-membership r0 cd)
     (is (not= (get-or-make-reporter [:a :b] "ab" cd) r0))))

(deftest cache-calculator-test
  (let [queue (new-priority-task-queue 0)
        cd (new-calculator-data queue)
        r0 (new-reporter :name :r0 :value 1)
        r1 (new-reporter :name :r1
                         :application [inc r0]
                         :calculator cache-calculator
                         :calculator-data cd)
        r2 (new-reporter :name :r2
                         :application [inc r0]
                         :calculator cache-calculator
                         :calculator-data cd)]
    ;; Nothing should happen when there is no demand for r1.
    (cache-calculator r1 cd)
    (is (not (contains? (reporter-data r1) :value-source)))
    ;; With demand, the cached reporter should be created.
    (request r1 cd)
    (is (contains? (reporter-data r1) :value-source))
    ;; And we should pick it up for the other reporter with the same expr.
    (request r2 cd)
    (is (= (:value-source (reporter-data r1))
           (:value-source (reporter-data r2))))
    (is (= (:value-source (reporter-data r1))
           (mm-get (:cache cd) [inc r0])))
    (is (= (canonicalize-application [r1])
           (canonicalize-application [r2])))
    (run-all-pending-tasks queue)
    (is (= (reporter-value r1) 2))
    (let [orig-source (:value-source  (reporter-data r1))]
      ;; Lose interest in r1 then get it back, and the same value
      ;; source should come back.
      (unrequest r1 cd)
      (is (not (contains? (reporter-data r1) :value-source)))
      (is (not (valid? r1)))
      (is (valid? r2))
      (is (not= (canonicalize-application [r1])
                (canonicalize-application [r2])))
      (request r1 cd)
      (run-all-pending-tasks queue)
      (is (valid? r1))
      (is (= (:value-source (reporter-data r1))
             orig-source))
      ;; Now, lose interest in both reporters with that application.
      ;; The cache should drop it.
      (unrequest r1 cd)
      (unrequest r2 cd)
      (is (not (valid? r1)))
      (is (not (valid? r2)))
      (is (not= (:value-source (reporter-data r1))
                orig-source))
      (is (nil? (mm-get (:cache cd) [inc r0]))))))

;;; Test that caching is working by doing a recursive computation that would
;;; take a very long time if it weren't cached.
(deftest fib-cache-test
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        base (new-reporter :value 0)]
    (letfn [(fib [n] (if (<= n 1)
                       base
                       (expr + (cache fib (- n 1)) (cache fib (- n 2)))))]
      ;; Since the base is 0, fib should be 0 everywhere, and since
      ;; the computations should be cached, this should be fast.
      (let [f45 (fib 45)]
        (is (= (computation-value f45 cd) 0))
        (check-propagation f45)
        (set-value! base 1)
        ;; Now it should be the right value.
        (is (= (computation-value f45 cd) 1836311903))
        (check-propagation f45)
        (set-value! base invalid)
        ;; Now it should be invalid.
        (is (= (not (valid? (computation-value f45 cd)))))
        (check-propagation f45)
        (unrequest f45 cd)
        (compute cd)
        (is (= (current-contents (:cache cd))) {})))))

;; Test that caching works with recomputations of subsidiary
;; computations. This tests that :old-value-source of
;; application reporters is getting kept around long enough.
(deftest reuse-test
  (let [r1 (new-reporter :value 1)
        rs (new-reporter :value [1 2 3])
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
    (set-value! rs [1 2 3 4])
    (is (= (computation-value r cd) [3 4 5 6]))
    (is (= @counter 5))))

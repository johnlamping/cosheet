(ns cosheet2.cache-calculator-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            [clojure.math :as math]
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

(deftest cache-key-test
  (let [r0 (new-reporter :name :r0 :application [:a :b])
        r1 (new-reporter :name :r1 :application [:a :b]
                         :value-source r0 :cache-key [:a :b])
        r2 (new-reporter :name :r1 :application [:a :b]
                         :value-source r0)
        r00 (new-reporter :name :r0 :application [r0 r0])
        r11 (new-reporter :name :r2 :application [r1 r1]
                          :value-source r00 :cache-key [[:a :b] [:a :b]])]
    (is (= (#'cosheet2.cache-calculator/cache-key [r1 r0])
           [[:a :b] r0]))
    (is (= (#'cosheet2.cache-calculator/cache-key [r2 r0])
           [r2 r0]))
    (is (= (#'cosheet2.cache-calculator/cache-key [r1 r11])
           [[:a :b] [[:a :b] [:a :b]]]))))

(deftest cache-membership-test
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        data {:application [:a :b]
              :name "ab"
              :cache-key (#'cosheet2.cache-calculator/cache-key [:a :b])}
        r0 (get-or-make-reporter data cd)]
     (is (= (:application (reporter-data r0)) [:a :b]))
     (is (= (:calculator-data (reporter-data r0)) cd))
     (is (not= (get-or-make-reporter data cd) r0))
     (request r0 cd)
     (#'cosheet2.cache-calculator/adjust-cache-membership r0 [:a :b] cd)
     (is (= (get-or-make-reporter data cd) r0))
     (unrequest r0)
     (#'cosheet2.cache-calculator/adjust-cache-membership r0 [:a :b] cd)
     (is (not= (get-or-make-reporter data cd) r0))))

(deftest cache-calculator-test
  (let [queue (new-priority-task-queue 0)
        cd (new-calculator-data queue)
        r0 (new-reporter :name :r0 :value 1)
        r1 (apply new-reporter
                  :name :r1
                  :application [inc r0]
                  :calculator-data cd
                  (data-for-forwarding-reporter [inc r0]))
        r2 (apply new-reporter
                  :name :r2
                  :application [inc r0]
                  :calculator-data cd
                  (data-for-forwarding-reporter [inc r0]))]
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
    (is (= (#'cosheet2.cache-calculator/cache-key [r1])
           (#'cosheet2.cache-calculator/cache-key [r2])))
    (run-all-pending-tasks queue)
    (is (= (reporter-value r1) 2))
    (let [orig-source (:value-source  (reporter-data r1))]
      ;; Lose interest in r1 then get it back, and the same value
      ;; source should come back.
      (unrequest r1)
      (is (not (contains? (reporter-data r1) :value-source)))
      (is (not (valid? r1)))
      (is (valid? r2))
      (is (= (#'cosheet2.cache-calculator/cache-key [r1])
             (#'cosheet2.cache-calculator/cache-key [r2])))
      (request r1 cd)
      (run-all-pending-tasks queue)
      (is (valid? r1))
      (is (= (:value-source (reporter-data r1))
             orig-source))
      ;; Now, lose interest in both reporters with that application.
      ;; The cache should drop it.
      (unrequest r1)
      (unrequest r2)
      (is (not (valid? r1)))
      (is (not (valid? r2)))
      (is (not= (:value-source (reporter-data r1))
                orig-source))
      (is (nil? (mm-get (:cache cd) [inc r0])))
      ;; Now try reporters with applications that access the cached value.
      ;; Initially, none of the cached values are valid.
      (let [r11 (apply new-reporter
                       :name :r11
                       :application [inc r1]
                       :calculator-data cd
                       (data-for-forwarding-reporter [inc r1]))
            r22 (apply new-reporter
                       :name :r22
                       :application [inc r2]
                       :calculator-data cd
                       (data-for-forwarding-reporter [inc r2]))]
        ;; Ask for them, and make sure they share their computation
        (request r11 cd)
        (request r22 cd)
        (is (:value-source (reporter-data r11)))
        (is (= (:value-source (reporter-data r11))
               (:value-source (reporter-data r22))))))))

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
      (let [f45 (fib 45)] ; fib(45) is the largest that fits in 64 bits.
        (is (= (computation-value f45 cd) 0))
        (check-propagation f45)
        (set-value! base 1)
        ;; Now it should be the right value.
        (is (= (computation-value f45 cd)
               ;; The formula for the 45th fibonacci number.
               (int (/ (math/pow (/ (+ 1 (math/sqrt 5)) 2) 46)
                       (math/sqrt 5)))))
        (check-propagation f45)
        (set-value! base invalid)
        ;; Now it should be invalid.
        (is (= (not (valid? (computation-value f45 cd)))))
        (check-propagation f45)
        (unrequest f45)
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

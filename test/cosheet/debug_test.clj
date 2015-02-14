(ns cosheet.debug-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [mutable-map :as mm]
                     [debug :refer :all]
                     [compute :refer :all]
                     [state :refer :all]
                     [store :refer :all]
                     [entity :refer :all]
                     store-impl)
            ; :reload
            ))

(defn fib [n s]
  (if (<= n 1)
    s
    (expr + (expr fib (- n 1) s) (expr fib (- n 2) s))))

(deftest current-value-fib-test
  (let [state (new-state :value 0)
        fib6 (fib 6 state)]
    (is (= (current-value fib6) 0))
    (state-set state 1)
    (is (= (current-value fib6) 13))))

(deftest trace-current-fib-test
  (let [state (new-state :value 0)
        fib6 (fib 6 state)]
    (is (= (first (trace-current fib6)) 0))
    (state-set state 1)
    (is (= (first (trace-current fib6)) 13))
    ;(pprint (simplify-for-print (trace-current fib6)))
    ))

(deftest simplify-for-print-test
  (let [s (new-element-store)
        ms (new-mutable-store s)
        i (make-id "i")
        ci (cosheet.store-impl/->ImplicitContentId i)
        m (mm/new-mutable-map)]
    (is (= (simplify-for-print 1) 1))
    (is (= (simplify-for-print (new-state :value 1)) '(State 1)))
    (is (= (simplify-for-print (expr 1 2)) '(:expr 1 2)))
    (is (= (simplify-for-print s) 'Store))
    (is (= (simplify-for-print ms)
           'MutableStore))
    (is (= (simplify-for-print i) 'Id-i))
    (is (= (simplify-for-print ci) 'content-Id-i))
    (is (= (simplify-for-print (description->entity i s)) 'Entity-Id-i))
    (is (= (simplify-for-print (description->entity ci s))
           'Entity-content-Id-i))
    (is (= (simplify-for-print (description->entity i ms)) 'Entity-Id-i))
    (is (= (simplify-for-print (description->entity ci ms))
           'Entity-content-Id-i))
    (is (= (simplify-for-print [#{{1 i}}]) [#{{1 'Id-i}}]))
    (mm/assoc-in! m [:a] i)
    (is (= (simplify-for-print m) {:a 'Id-i}))
    (is (= (simplify-for-print inc) 'inc))
    (is (= (simplify-for-print simplify-for-print) 'debug/simplify-for-print))))




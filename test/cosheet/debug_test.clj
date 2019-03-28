(ns cosheet.debug-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [mutable-map :as mm]
                     [debug :refer :all]
                     [test-utils :refer [check as-set]]
                     [canonical :refer [canonicalize-list]]
                     [reporter :refer [new-reporter set-value! data-atom]]
                     [expression-manager :refer [new-expression-manager-data
                                                 request compute]]
                     [expression :refer [expr expr-let cache]]
                     [store :refer :all]
                     store-impl
                     [store-utils :refer [add-entity]]
                     [entity :refer :all])
            ; :reload
            ))

(defn fib [n s]
  (if (<= n 1)
    s   
    (expr + (fib (- n 1) s) (fib (- n 2) s))))

(defn count-ones [x]
  (cond (= x 1)
        1
        (sequential? x)
        (apply + (map count-ones x))
        true
        0))

(deftest trace-current-test
  (let [state (new-reporter :value 0)
        fib6 (fib 6 state)
        md (new-expression-manager-data)]
    (is (= (first (trace-current fib6)) 0))
    (set-value! state 1)
    (is (= (first (trace-current fib6)) 13))
    (is (= (count-ones (trace-current fib6)) 13))
    ;; Now try it after it had been computed.
    (request fib6 md)
    (compute md)
    (is (= (first (trace-current fib6)) 13))
    (is (= (count-ones (trace-current fib6)) 13))))

(defn deep-node [reporter]
  (let [data (cosheet.reporter/data reporter)
        source (:value-source data)
        required (distinct (concat (:needed-values data)
                                   (keys (:subordinate-values data))
                                   (when source [source])))]
    (if (empty? required)
      reporter
      (deep-node (first (sort-by hash required))))))

(deftest generate-backtrace-test
  (let [state (new-reporter :value 0)
        fib6 (fib 6 state)
        md (new-expression-manager-data)]
    (request fib6 md)
    (set-value! state 1)
    (compute md)
    (is (= (map #(map cosheet.reporter/value %)
                (generate-backtrace (deep-node fib6))))
        [[+ 1 1]
         [+ 3 2]
         [+ 8 5]])))

(deftest store-as-list-test
  (let [s (-> (new-element-store)
              (add-entity nil '("a" "b"))
              first
              (add-entity nil '("x" ("y" "z")))
              first)]
    (is (check (canonicalize-list (store-as-list s))
               (canonicalize-list '(("a" "b") ("x" ("y" "z"))))))))

(deftest simplify-for-print-test
  (let [s (new-element-store)
        ms (new-mutable-store s)
        i (make-id "i")
        ci (cosheet.store-impl/->ImplicitContentId i)
        r (new-reporter :expression [+ 1 (new-reporter :application [+ 2 3])])
        m (mm/new-mutable-map)]
    (is (= (simplify-for-print 1) 1))
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
    (is (= (simplify-for-print []) []))
    (is (= (simplify-for-print [#{{1 i}}]) [#{{1 'Id-i}}]))
    (is (= (simplify-for-print r) '("R" _PLUS_ 1 (_PLUS_ 2 3))))
    (mm/assoc-in! m [:a] i)
    (is (= (simplify-for-print m) {:a 'Id-i}))
    (is (= (simplify-for-print inc) 'inc))
    (is (= (simplify-for-print simplify-for-print) 'debug/simplify-for-print))))

(deftest profile-test
  (let [ms (new-reporter :value #{:a :b})
        min-r (expr min (expr clojure.set/intersection ms #{:a})
                    (expr clojure.set/intersection ms #{:b}))
        rc (cache max
                  min-r
                  (expr min (expr clojure.set/intersection ms #{:c})))
        r (expr list rc rc)]
    (swap! (data-atom min-r)
           #(assoc % :value-source
                   (expr assoc (expr clojure.set/intersection ms #{:a}))))
    (is (check (reporters-profile [r])
               {'min {'intersection 1 'assoc 1}
                nil {'intersection 4
                     'min 2
                     'max 1
                     'assoc 1
                     'Primordial 1}}))))

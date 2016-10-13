(ns cosheet.server.render-utils-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [store :refer [make-id]]
             [entity :as entity  :refer [to-list]]
             [expression :refer [expr expr-let expr-seq]]
             [canonical :refer [canonicalize-list]]
             [debug :refer [envs-to-list simplify-for-print]]
             [test-utils :refer [check any as-set evals-to
                                 let-mutated item->immutable]])
            (cosheet.server
             [render-utils :refer :all]
             [referent :refer [elements-referent union-referent]])
            ; :reload
            ))

(deftest condition-satisfiers-R-test
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil :a)))))
             [:a]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil :a :c)))))
             (as-set [:a :c])))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil :a :b)))))
             [:a]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil :a :b)))))
             [:a]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil :a :a :b)))))
             [:a :a]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil :a :a :b)))))
             [:a])))

(deftest add-alternate-to-target-test
  (let [ia (make-id "a")
        ib (make-id "b")
        ref (union-referent [ia ib])
        subject (elements-referent '(nil :x) ref)]
    (is (check (add-alternate-to-target {:item-referent ref} {})
               {:item-referent ref}))
    (is (check (add-alternate-to-target {:item-referent ref}
                                        {:alternate :some-alternate})
               {:item-referent ref
                :alternate :some-alternate}))))

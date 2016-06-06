(ns cosheet.server.render2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [orderable :as orderable]
             [entity :as entity  :refer [to-list description->entity
                                         label->elements]]
             [query :refer [matching-elements matching-items]]
             [reporters :as reporter]
             [expression :refer [expr expr-let expr-seq]]
             [debug :refer [current-value envs-to-list simplify-for-print]]
             [expression-manager :refer [new-expression-manager-data
                                         request compute]] 
             [expression-manager-test :refer [check-propagation]]
             entity-impl
             [store :refer [new-element-store id->content id->subject
                            current-store]]
             store-impl
             [store-utils :refer [add-entity]]
             mutable-store-impl
             [dom-utils :refer [dom-attributes]]
             [test-utils :refer [check any as-set evals-to let-mutated]])
            (cosheet.server
             [referent :refer [item-referent union-referent difference-referent
                           query-referent elements-referent
                          canonicalize-list]]
             [render2 :refer :all]
             [hierarchy :refer [items-hierarchy-by-elements]])
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

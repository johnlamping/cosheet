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
                                        {:alternate-target :some-alternate})
               {:item-referent ref
                :alternate :some-alternate}))))

(deftest transform-inherited-attributes-test
  (is (check (transform-inherited-attributes
              {:attributes [{:a 1}
                            [#{:label} {:b 2}]
                            [#{:label :element :recursive} #{:content} {:c 3}]
                            [#{:content} {:d 4}]
                            [#{:element :optional} #{:label} {:e 5}]
                            [#{:label :optional :recursive} #{:label} {:f 6}]]
               :x 9}
              :label)
             {:attributes [{:b 2}
                           [#{:label :element :recursive :optional} #{:content}
                            {:c 3}]
                           {:e 5}
                           [#{:label :optional :recursive} #{:label} {:f 6}]
                           {:f 6}]
              :x 9})))

(deftest inherited-attributes-test
  (is (check (inherited-attributes
              {:attributes [{:class "foo"}
                            {:class "bar"}
                            {:other "hi"}
                            [#{:label} {:b 2}]
                            [#{:label :optional} {:more "there"}]]
               :x 9})
             {:class "foo bar"
              :other "hi"
              :more "there"})))

(deftest content-attributes-test
  (is (check (content-attributes
              {:attributes [{:class "foo"}
                            {:class "bar"}
                            {:other "hi"}
                            [#{:label} {:b 2}]
                            [#{:content} {:class "baz"}]]
               :x 9})
             {:class "baz"})))

(deftest item-or-content-attributes-test
  (is (check (item-or-content-attributes
              {:attributes [{:class "foo"}
                            {:class "bar"}
                            {:other "hi"}
                            [#{:label} {:b 2}]
                            [#{:content} {:class "baz"}]]
               :x 9})
             {:class "foo bar baz"
              :other "hi"})))


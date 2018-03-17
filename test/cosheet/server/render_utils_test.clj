(ns cosheet.server.render-utils-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [store :refer [make-id new-element-store]]
             [entity :as entity  :refer [to-list description->entity]]
             [expression :refer [expr expr-let expr-seq]]
             [canonical :refer [canonicalize-list]]
             [store-utils :refer [add-entity]]
             [debug :refer [envs-to-list simplify-for-print]]
             [test-utils :refer [check any as-set evals-to
                                 let-mutated item->immutable]])
            (cosheet.server
             [render-utils :refer :all]
             [referent :refer [elements-referent exemplar-referent
                               union-referent]])
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

(deftest non-implied-matching-elements-R-test
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (non-implied-matching-elements-R
                               test '(nil 1) '(nil :a :c)))))
             [(canonicalize-list '(:b 1))]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (non-implied-matching-elements-R
                               test '(nil 1) '(nil :b)))))
             [(canonicalize-list '(:b 1))]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (non-implied-matching-elements-R
                               test '(nil 1) '(nil (:b 1))))))
             [])))

(deftest transform-inherited-attributes-test
  (is (check (transform-inherited-attributes
              {:attributes [{:a 1}
                            [#{:label} {:b 2}]
                            [#{:label :element :recursive} #{:content} {:c 3}]
                            [#{:content} {:d 4}]
                            [#{:element :optional} #{:label} {:e 5}]
                            [#{:label :optional :recursive} #{:label} {:f 6}]
                            [(make-id "a") {:item "yup"}]]
               :x 9}
              :label)
             {:attributes [{:b 2}
                           [#{:label :element :recursive :optional} #{:content}
                            {:c 3}]
                           {:e 5}
                           [#{:label :optional :recursive} #{:label} {:f 6}]
                           {:f 6}
                           [(make-id "a") {:item "yup"}]]
              :x 9})))

(deftest remove-inherited-for-item-test
  (is (check (remove-inherited-for-item
              {:attributes [{:a 1}
                            [#{:label} {:b 2}]
                            [(make-id "a") {:item "yup"}]
                            [(make-id "b") {:item "nope"}]]
               :x 9}
              (description->entity (make-id "a") (new-element-store)))
             {:attributes [{:a 1}
                            [#{:label} {:b 2}]
                            [(make-id "b") {:item "nope"}]]
              :x 9})))

(deftest remove-inherited-attribute=test
  (is (check (remove-inherited-attribute
              {:attributes [{:a 1 :b 2}
                            [#{:label} {:b 5}]
                            [#{:label :element :recursive} #{:content} {:a 3}]]
               :x 9}
              :a)
             {:attributes [{:b 2}
                            [#{:label} {:b 5}]]
              :x 9})))

(deftest split-descriptors-by-currency-test
  (is (check (split-descriptors-by-currency
              [{:a 1 :b 2}
               [#{:label :optional} {:b 5}]
               [#{:label :element :recursive} #{:content} {:a 3}]
               {:c 3}])
             [{:a 1 :b 2 :c 3}
              [[#{:label :optional} {:b 5}]
               [#{:label :element :recursive} #{:content} {:a 3}]]])))

(deftest inherited-attributes-test
  (is (check (inherited-attributes
              {:attributes [{:class "foo"}
                            {:class "bar"}
                            {:other "hi"}
                            [#{:label} {:b 2}]
                            [#{:label :optional} {:more "there"}]
                            [(make-id "a") {:item "yup"}]
                            [(make-id "a") #{:label :optional} {:item "nope"}]
                            [(make-id "b") {:item "nope"}]]
               :x 9}
              (description->entity (make-id "a") (new-element-store)))
             {:class "foo bar"
              :other "hi"
              :more "there"
              :item "yup"})))

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

(deftest item-referent-given-inherited-test
  (let [s0 (new-element-store)
        [s1 joe-id] (add-entity s0 nil "joe")
        [store joe-age-id] (add-entity s1 joe-id '(30 ("age" :tag)))
        joe (description->entity joe-id store)
        joe-age (description->entity joe-age-id store)]
    (is (check (item-referent-given-inherited joe-age {})
               joe-age-id))
    (is (check (item-referent-given-inherited joe-age {:match-all true})
               joe-age-id))
    (is (check (item-referent-given-inherited
                joe-age {:match-all true :subject-referent joe-id})
               (elements-referent joe-age-id joe-id)))
    (is (check (item-referent-given-inherited
                joe-age  {:subject-referent (union-referent [joe-id])})
               (exemplar-referent joe-age-id (union-referent [joe-id])))))) 


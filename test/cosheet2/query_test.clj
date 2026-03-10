(ns cosheet2.query-test
  (:require [clojure.test :refer [deftest is]]
            clojure.pprint
            (cosheet2 [store :refer [new-element-store make-item-id
                                     get-new-object-id ->ItemId]]
                      store-impl
                      [store-utils :refer [add-element]]
                      [entity :refer [to-list id->element id->object content
                                      elements label->elements mutable-entity?
                                      primitive? entity-key
                                      make-object-list make-element-list
                                      immutable-object-to-list
                                      add-elements-to-entity]]
                      entity-impl
                      [query :as query :refer :all]
                      [query-impl :refer [closest-template minimal-label?]]
                      [test-utils :refer [check as-set]])
            ; :reload
            ))

(defn listify-map-values
  "Given a map whose values are entities, run to-list on them."
  [map]
  (into {} (for [[k v] map] [k (to-list v)])))

(deftest extended-by-test
  (let [element0 '(3 "Foo")
        element1 '(3 ("foo" :label))
        itemx `(nil ~element0 ~element1)
        object-foo (id->object (make-item-id "foo") (new-element-store))
        object-bare-foo (id->object (make-item-id "foo") nil)]
    (is (extended-by? 1 1) true)
    (is (not (extended-by? 1 2)))
    (is (extended-by? "1" "1"))
    (is (not (extended-by? 1 "1")))
    (is (extended-by? :foo :foo))
    (is (not (extended-by? :foo :bar)))
    (is (extended-by? element0 element0))
    (is (extended-by? element1 element1))
    (is (extended-by? element0 element1))
    (is (not (extended-by? '(nil (nil :label)) element0)))
    (is (extended-by? '(nil (nil :label)) element1))
    (is (not (extended-by? element1 element0)))
    (is (extended-by? itemx itemx))
    (is (extended-by? '(3 ("foo" false))
                      '(3 ("foo" false))))
    (is (extended-by? '(nil ("foo" false))
                      '(3 ("Foo" false))))
    (is (not (extended-by? '(4 ("Foo" false))
                           '(3 ("foo" false)))))
    (is (extended-by? '(3 "foo")
                      '(3 ("Foo" false))))
    (is (not (extended-by? '(3 ("foo" false))
                           '(3 ("foo")))))
    (is (extended-by? '(3 ("Foo" (4 false)))
                      '(3 ("Foo" (4 false)))))
    (is (not (extended-by? '(3 ("foo" (4 false)))
                           '(3 ("foo" (4 true))))))
    (is (not (extended-by? '(3 2 2)
                           '(3 2))))
    (is (extended-by? (make-object-list
                     [`(1 (~(id->object (make-item-id "name") nil)))])
                      (make-object-list
                       [`(1 (~(id->object (make-item-id "name") nil)))
                        2])))
    (is (extended-by? (make-object-list
                       [`(1 (~(id->object (make-item-id "name") nil)))
                        2])
                      (make-object-list
                       [`(1 (~(id->object (make-item-id "name") nil)))
                        2])))
    (is (extended-by? `(~(id->object (make-item-id "name") nil))
                      `(~(id->object (make-item-id "name") nil))))
    (is (not (extended-by? (make-element-list :source 1 '(2))
                           (make-element-list :target 1 '((2 3))))))
    (is (not (extended-by? (make-element-list :target 1 '(2))
                           (make-element-list :source 1 '((2 3))))))
    (is (extended-by? (make-element-list :target 1 '(2))
                      (make-element-list :target 1 '((2 3)))))
    (is (extended-by? 3 element0))
    (is (extended-by? 3 element1))
    (is (not (extended-by? element1 3)))
    (is (extended-by? `(1 ~(not-query :x))
                      1))
    (is (extended-by? `(1 :a :b ~(not-query :x) ~(not-query :u))
                      '(1 :a (:b :c))))
    (is (not (extended-by? `(1 :a :b :c ~(not-query :x) ~(not-query :u))
                           '(1 :a (:b :c)))))
    (is (not (extended-by?
              `(1 :a ~(not-query :b))
              '(1 :a (:b :c)))))
    (is (not (extended-by?
              `(1 :a :b ~(not-query :x) ~(not-query :u) ~(not-query :a))
              '(1 :a (:b :c)))))
    (is (extended-by?
         `(1 :a (:b ~(not-query :x)) ~(not-query `(:b ~(not-query :c))))
         '(1 :a (:b :c))))
    (is (not (extended-by? `(1 :a (:b ~(not-query :c)))
                           '(1 :a (:b :c)))))
    (is (extended-by? `(1 :a (:b ~(not-query :d)))
                      '(1 :a (:b :c))))
    (is (not (extended-by? `(1 :a :b ~(not-query `(:b ~(not-query :d))))
                           '(1 :a (:b :c)))))
    (is (extended-by? (make-object-list '(1 2))
                      (make-object-list '(1 2 3))))
    (is (extended-by? (make-object-list '(1 2))
                      (make-object-list '(1 2))))
    (is (extended-by? (make-object-list
                       [`(1 (~(id->object (make-item-id "name") nil)))])
                      (make-object-list
                       [`(1 (~(id->object (make-item-id "name") nil)))
                        2])))
    (is (not (extended-by? (make-object-list '(1 2 3))
                           (make-object-list '(1 2)))))
    (is (not (extended-by? (make-element-list :source 1 '(1 2))
                           (make-object-list '(1 2)))))
    (is (not (extended-by? (make-object-list '(1 2))
                           (make-element-list :source 1 '(1 2)))))
    (is (not (extended-by? (make-element-list :source 1 '(2))
                           (make-object-list '(1 2)))))
    (is (not (extended-by? (make-object-list '(1 2))
                           (make-element-list :source 1 '(2)))))
    (is (not (extended-by? (make-object-list
                            [`(1 (~(id->object (make-item-id "name") nil)))
                             2])
                           (make-object-list
                            [`(1 (~(id->object (make-item-id "name") nil)))]))))
    (is (extended-by? object-foo object-foo))
    (is (extended-by? object-foo object-bare-foo))
    (is (extended-by? object-bare-foo object-foo))
    (is (extended-by? (make-object-list nil) object-bare-foo))
    (is (not (extended-by? object-foo
                           (make-object-list nil))))
    ;; Objects by themselves can't stand in for entities with that content.
    (is (not (extended-by? object-foo
                           (make-element-list :source object-foo nil))))
    (is (extended-by? (make-element-list :source object-foo nil)
                      (make-element-list :source object-foo nil)))
    (is (extended-by? (make-element-list :source object-foo nil)
                      (make-element-list
                       :source object-foo `((~object-bare-foo)))))
    (is (extended-by? (make-element-list
                       :source object-foo `((~object-foo)))
                      (make-element-list
                       :source object-foo `((~object-bare-foo) 5))))
    (is (not (extended-by? (make-element-list
                            :source object-foo `((~object-bare-foo)))
                           (make-element-list :source object-foo nil))))))

(defn variable
  ([name] (variable-query name))
  ([name qualifier] (variable-query name :qualifier qualifier))
  ([name qualifier reference]
   (variable-query name
                   :qualifier qualifier
                   :reference reference)))

(deftest special-forms-test
  (let [query (variable-query "foo" :qualifier '(1 2) :reference true)]
    (is (special-form? query))
    (is (variable-query? query))
    (is (= (variable-name query) "foo"))
    (is (= (variable-qualifier query) '(1 2)))
    (is (= (variable-reference query) true)))
  (let [query (variable-query
             "foo" :qualifier (make-element-list :target 2 '(1)))]
    (is (special-form? query))
    (is (variable-query? query))
    (is (= (variable-name query) "foo"))
    (is (= (variable-qualifier query) (make-element-list :target 2 '(1))))
    (is (= (variable-reference query) nil)))
  (let [query (not-query 1)]
    (is (special-form? query))
    (is (= (special-form-type query) :not))
    (is (= (sub-query query) 1)))
  (let [query (not-query (make-element-list :target 2 '(1)))]
    (is (special-form? query))
    (is (= (special-form-type query) :not))
    (is (= (sub-query query) (make-element-list :target 2 '(1)))))
  (let [query (and-query 1 (make-element-list :target 2 '(1)))]
    (is (special-form? query))
    (is (= (special-form-type query) :and))
    (is (= (sub-queries query) [1 (make-element-list :target 2 '(1))])))
  (let [query (forall-query "foo" 1 (make-element-list :target 2 '(1)))]
    (is (special-form? query))
    (is (= (special-form-type query) :forall))
    (is (= (quantifier-variable query)
           (add-elements-to-entity (variable-query "foo" :qualifier 1)
                                   '(::query/variable))))
    (is (= (sub-query query) (make-element-list :target 2 '(1)))))
  (let [query (exists-query "foo" 1 (make-element-list :target 2 '(1)))]
    (is (special-form? query))
    (is (= (special-form-type query) :exists))
    (is (= (quantifier-variable query)
           (add-elements-to-entity (variable-query "foo" :qualifier 1)
                                   '(::query/variable))))
    (is (= (sub-query query) (make-element-list :target 2 '(1))))))

(deftest closest-template-test
  (is (= (closest-template '(1 2 (3 4))
                           {"bar" 7})
         ['(1 2 (3 4)) true]))
  (is (= (closest-template '((:target 1) 2 (3 4))
                           {"bar" 7})
         ['((:target 1) 2 (3 4)) true]))
  (is (= (closest-template `(nil (2 3 ~(not-query 5)))
                           {"bar" '(7 6)})
         ['(nil (2 3)) false]))
  (is (= (closest-template `(1 2 (3 ~(variable "foo" 5)))
                           {"bar" 7})
         ['(1 2 (3 5)) #{"foo"}]))
  (is (= (closest-template `(1 2 (3 ~(variable "bar" 7)))
                           {"bar" '(7 6)})
         ['(1 2 (3 (7 6))) true]))
  (let [reversed (fn [content elements]
                   (make-element-list :target content elements))]
    (is (= (closest-template (make-object-list
                              `(2 ~(reversed (variable "bar") '(3))))
                             {"bar" 7})
           [(make-object-list `(2 ~(reversed 7 '(3)))) true]))
    (is (= (closest-template (make-object-list
                              `(2 (3 (~(make-object-list [(variable "bar")
                                                          (reversed 7 nil)])))))
                             {})
           [(make-object-list `(2 (3 (~(make-object-list [nil
                                                          (reversed 7 nil)])))))
            #{"bar"}]))
    (is (= (closest-template `(1 2 (3 ~(variable "bar" 7 true)))
                             {"bar" '(7 6)})
           ['(1 2 (3 (7 6))) false])))
  (let [named-object (id->object
                      (make-item-id "test")
                      (new-element-store))
        [_ anonymous-object-id] (get-new-object-id (new-element-store))
        anonymous-object (id->object anonymous-object-id nil)]
    (is (= (closest-template `(~(variable "foo" 5)
                               (~anonymous-object)
                               (:foo ~(variable "baz" (variable "bar")))
                               ~(not-query 8))
                             {"bar" 7})
           [`(5 ([:object]) (:foo 7)) false]))
    (is (= (closest-template `(~(variable "foo" 5)
                               (~anonymous-object)
                               (:foo ~(variable "baz")))
                             {"bar" 7})
           [`(5 ([:object]) (:foo nil)) #{"foo" "baz"}]))
    (is (= (closest-template `(~(variable "foo" 5)
                               (~anonymous-object)
                               (:foo ~(variable "foo")))
                             {"bar" 7})
           [`(5 ([:object]) (:foo nil)) false]))
    (is (= (closest-template `(~(variable "foo" 5)
                               (~named-object)
                               (:foo ~(variable "baz" (variable "bar")))
                               ~(not-query 8))
                             {"bar" 7})
           [`(5 (~named-object) (:foo 7)) false]))
    (is (= (closest-template `(~(variable "foo" 5)
                               (~named-object)
                               (:foo ~(variable "baz")))
                             {"bar" 7})
           [`(5 (~named-object) (:foo nil)) #{"foo" "baz"}]))
    (is (= (closest-template `(~(variable "foo" 5)
                               (~named-object)
                               (:foo ~(variable "foo")))
                             {"bar" 7})
           [`(5 (~named-object) (:foo nil)) false])))
  (is (thrown? java.lang.AssertionError
               (closest-template `(~(and-query (variable "foo" 5)
                                               (variable "bar" 6)))
                                 {"bar" 7}))))

(deftest minimal-label?-test
  (is (minimal-label? :foo))
  (is (not (minimal-label? '(:foo "foo"))))
  (is (minimal-label? '("foo" :label)))
  (is (not (minimal-label? '("foo" :label "bar"))))
  (is (minimal-label? `(~(id->object (->ItemId -2) nil))))
  (is (not (minimal-label? `(~(id->object (->ItemId -2) nil) "bar"))))
  (is (not (minimal-label? `(~(make-object-list '()))))))

(deftest matching-extensions-test
  (is (= (matching-extensions 1 {} 1) [{}]))
  (is (= (matching-extensions "a" {} "A") [{}]))
  (is (= (matching-extensions "A" {} "a") [{}]))
  (is (= (matching-extensions 1 {} 2) nil))
  (is (= (matching-extensions 1 {:a :b} 1)
         [{:a :b}]))
  (is (= (matching-extensions '(1) {:a :b} 1)
         [{:a :b}]))
  (is (= (matching-extensions 1 {:a :b} '(1))
         [{:a :b}]))
  (is (= (matching-extensions '(1) {:a :b} '(1))
         [{:a :b}]))
  (is (= (matching-extensions 1 {:a :b} '(1 2))
         [{:a :b}]))
  (is (= (matching-extensions '(1) {:a :b} '(1 2))
         [{:a :b}]))
  (is (= (matching-extensions '(1 2) {:a :b} '(1 2))
         [{:a :b}]))
  (is (= (matching-extensions '(1 2 3) {:a :b} '(1 (2 3)))
         nil))
  (is (= (matching-extensions '(1 (2 3)) {:a :b} '(1 2 3))
         nil))
  ;; The next two test a special case optimization.
  (is (= (matching-extensions '(nil (nil 3)) {:a :b} '(1 (2 3 4)))
         [{:a :b}]))
  (is (= (matching-extensions '(nil (nil 2)) {:a :b} '(1 (2 3 4)))
         nil))
  (is (= (matching-extensions '(1 (4 6)) {:a :b} '(1 (4 6)))
         [{:a :b}]))
  (is (= (matching-extensions '((1 2) 3 (4 (5 6))) {:a :b}
                              '((1 2) 3 (4 (5 6))))
         [{:a :b}]))

  ;; Objects
  (is (= (matching-extensions (make-object-list '(1 2)) {:a :b}
                              (make-object-list '(1 2)))
         [{:a :b}]))
  (is (= (matching-extensions (make-object-list '(1 2)) {:a :b}
                              (make-object-list '(1 (2 3) 4)))
         [{:a :b}]))
  (is (= (matching-extensions (make-object-list '(1 2)) {:a :b}
                              (make-object-list '(1)))
         nil))
  (is (= (matching-extensions '(1 2) {:a :b}
                              (make-object-list '(0 1 2)))
         nil))
  (is (= (matching-extensions (make-object-list '(1 2)) {:a :b}
                              '(0 1 2))
         nil))

  ;; Orientation
  (is (= (matching-extensions (make-element-list :target 1 '(2 3)) {:a :b}
                              (make-element-list :target 1 '(2 3)))
         [{:a :b}]))
  (is (= (matching-extensions (make-element-list :target 1 '(2 3)) {:a :b}
                              (make-element-list :source 1 '(2 3)))
         nil))
  (is (= (matching-extensions (make-element-list :target 1 '(2 3)) {:a :b}
                              1)
         nil))
  (is (= (matching-extensions (make-element-list :source 1 '(2 3)) {:a :b}
                              (make-element-list :target 1 '(2 3)))
         nil))
  (is (= (matching-extensions (make-object-list
                               `(~(make-element-list :target 1 '(2 3)))) {:a :b}
                              (make-object-list
                               `(~(make-element-list :target 1 '(2 3)))))
         [{:a :b}]))
  (is (= (matching-extensions (make-object-list
                               `(~(make-element-list :target 1 '(2 3)))) {:a :b}
                              (make-object-list
                               `(~(make-element-list :source 1 '(2 3)))))
         nil))
  
  ;; Duplicates in term
  (let [s (new-element-store)
        [s1 id1] (add-element s nil '(1 2))]
    (is (empty? (matching-extensions '(1 2 2) {:a :b}
                                     (id->element id1 s1)))))
  (let [s (new-element-store)
        [s1 id1] (add-element s nil '(1 2 2))]
    (is (= (matching-extensions '(1 2 2) {:a :b}
                                (id->element id1 s1))
           [{:a :b}])))
  (let [s (new-element-store)
        [s1 id1] (add-element s nil '(1 2 2 2))]
    (is (= (matching-extensions '(1 2 2) {:a :b}
                                (id->element id1 s1))
           [{:a :b}])))
  
  ;; Variables
  (is (= (matching-extensions (variable "foo") {:a :b}
                              2)
         [{:a :b, "foo" 2}]))
  (is (= (matching-extensions (variable "foo") {:a :b}
                              '(1 (2 3)))
         [{:a :b, "foo" '(1 (2 3))}]))
  (is (= (matching-extensions (variable "foo") {:a :b}
                              (make-element-list :target 1 '(2 3)))
         [{:a :b, "foo" (make-element-list :target 1 '(2 3))}]))
  (is (= (matching-extensions (variable "foo") {:a :b}
                              (make-object-list '(2 3)))
         [{:a :b, "foo" (make-object-list '(2 3))}]))
  (is (= (matching-extensions `(1 ~(variable "foo")) {:a :b}
                              '(1 2))
         [{:a :b, "foo" 2}]))
  (is (= (matching-extensions `(1 (~(variable "foo") :foo)) {:a :b}
                              '(1 (2 :foo)))
         [{:a :b, "foo" 2}]))
  (is (= (matching-extensions `(1 (~(variable "foo") :foo)) {:a :b}
                              '(1 (2 :bar :foo)))
         [{:a :b, "foo" 2}]))
  (is (= (matching-extensions `(1 (~(variable "foo") :foo)) {:a :b}
                              '(1 (2 :bar)))
         nil))
  (is (= (matching-extensions `(1 (~(variable "foo") :foo)) {:a :b}
                              '(1 2))
         nil))
  (is (= (matching-extensions `(1 (~(variable "foo") false)) {:a :b}
                              '(1 (2 false)))
         [{:a :b, "foo" 2}]))
  (is (= (matching-extensions `(1 (~(variable "foo") false))  {:a :b}
                              '(1 (2 true)))
           nil))
  (is (= (set (matching-extensions `(1 (~(variable "foo") :foo)) {:a :b}
                                   '(1 (2 :foo) (3 :foo) (2 :foo) (4 :bar))))
         #{{:a :b, "foo" 2} {:a :b, "foo" 3}}))
  (is (= (matching-extensions `(1 (~(variable "foo") :foo)
                                  (~(variable "foo") :bar)) {:a :b}
                              '(1 (2 :foo) (2 :bar)))
         [{:a :b, "foo" 2}]))
  (is (= (matching-extensions `(nil (~(variable "foo") :foo)
                                    (~(variable "foo") :bar))
                              {:a :b}
                              '(1 (2 :foo) (2 :bar)))
         [{:a :b, "foo" 2}]))
  (is (= (matching-extensions `(2 (~(variable "foo") :foo)
                                  (~(variable "foo") :bar)) {:a :b}
                              '(1 (2 :foo) (2 :bar)))
         nil))
  (is (= (matching-extensions `(~(variable "foo")) {:a :b}
                              '(1))
         [{:a :b, "foo" 1}]))
  (is (= (matching-extensions `(~(variable "foo") ~(variable "foo")) {:a :b}
                              '(1 1))
         [{:a :b, "foo" 1}]))
  (is (= (matching-extensions `(~(variable "foo") ~(variable "foo")) {:a :b}
                              '(1 2))
         nil))
  (is (= (matching-extensions `(~(variable "foo") ~(variable "foo")) {:a :b}
                              '(1 2 3 1))
         [{:a :b, "foo" 1}]))
  (is (= (matching-extensions `(1 (~(variable "foo") :foo)
                                  (~(variable "foo") :bar)) {:a :b}
                              '(1 (2 :foo) (3 :bar)))
         nil))
  (is (= (set (matching-extensions `(1 (~(variable "foo") :foo)
                                       (~(variable "foo") :bar)) {:a :b}
                                   '(1 (1 :foo) (2 :foo)
                                       (1 :bar) (2 :bar) (3 :bar))))
         #{{:a :b, "foo" 1} {:a :b, "foo" 2}}))
  (is (= (matching-extensions `(1 (~(variable "foo") :foo)
                                  (~(variable "bar") :bar)) {:a :b}
                              '(1 (2 :foo) (3 :bar)))
         [{:a :b, "foo" 2, "bar" 3}]))
  (is (= (matching-extensions `(1 (~(variable "foo") :foo)
                                  (~(variable nil) :bar)) {:a :b}
                              '(1 (2 :foo) (3 :bar)))
         [{:a :b, "foo" 2}]))
  (is (= (matching-extensions `(1 (~(variable nil) :foo)
                                  (~(variable nil) :bar)) {:a :b}
                              '(1 (2 :foo) (3 :bar)))
         [{:a :b}]))
  (is (= (matching-extensions `(1 (~(variable "foo" 2) :foo)) {:a :b}
                              '(1 (2 :foo)))
         [{:a :b, "foo" 2}]))
  (is (= (matching-extensions
          (make-object-list `(~(variable
                                "foo" (make-element-list :source 2 nil))))
          {:a :b}
          (make-object-list `(~(make-element-list :source 2 '(:foo)))))
         [{:a :b, "foo" (make-element-list :source 2 '(:foo))}]))
  (is (= (matching-extensions
          (make-object-list `(~(variable
                                "foo" (make-element-list :target 2 nil))))
          {:a :b}
          (make-object-list `(~(make-element-list :target 2 '(:foo)))))
         [{:a :b, "foo" (make-element-list :target 2 '(:foo))}]))
  (is (= (matching-extensions
          (make-object-list `(~(variable
                                "foo" (make-element-list :target 2 nil))))
          {:a :b}
          (make-object-list `(~(make-element-list :source 2 '(:foo)))))
         nil))
  (is (= (matching-extensions
          (make-object-list `(~(variable
                                "foo" (make-element-list :source 2 nil))))
          {:a :b}
          (make-object-list `(~(make-element-list :target 2 '(:foo)))))
         nil))
  (is (= (matching-extensions `(1 (~(variable "foo" 2) :foo)) {:a :b}
                              '(1 (3 :foo)))
         nil))
  (is (= (matching-extensions `(~(variable "foo" 1) ~(variable "foo")) {:a :b}
                              '(1 (1 :foo)))
         nil))
  (is (= (matching-extensions `(1 ~(variable "foo" `(nil ~(variable "bar"))))
                              {:a :b}
                              '(1 (2 (3 :foo))))
         [{:a :b, "foo" '(2 (3 :foo)) "bar" '(3 :foo)}]))
  (is (check (matching-extensions
              `(1 ~(variable "foo") ~(variable "bar")) {:a :b}
              '(1 2 3 4))
             (as-set [{:a :b "foo" 2 "bar" 3}
                      {:a :b "foo" 2 "bar" 4}
                      {:a :b "foo" 3 "bar" 2}
                      {:a :b "foo" 3 "bar" 4}
                      {:a :b "foo" 4 "bar" 2}
                      {:a :b "foo" 4 "bar" 3}])))
  ;; not in elements
  (is (= (matching-extensions `(1 ~(not-query :x)) 1)
         [{}]))
  (is (empty? (matching-extensions `(1 ~(not-query :a)) '(1 :a))))
  (is (= (matching-extensions `(1 :a :b ~(not-query :x) ~(not-query :u))
                              '(1 :a :b))
         [{}]))
  (is (empty? (matching-extensions `(1 :a :b :c ~(not-query :x)
                                       ~(not-query :u))
                                   '(1 :a :b))))
  (is (empty? (matching-extensions
               `(1 :a :b ~(not-query :x) ~(not-query :u) ~(not-query :a))
               '(1 :a :b))))
  (is (= (matching-extensions `(1 :a (:b ~(not-query :x))
                                  ~(not-query `(:b ~(not-query :c))))
                              '(1 :a (:b :c)))
         [{}]))
  (is (empty? (matching-extensions `(1 :a (:b ~(not-query :c)))
                                   '(1 :a (:b :c)))))
  (is (empty? (matching-extensions `(1 :a :b ~(not-query `(:b ~(not-query :d))))
                                   '(1 :a (:b :c)))))
  (is (= (matching-extensions `(1 (:a ~(variable "foo" nil))
                                  ~(not-query `(:c ~(variable "foo" nil))))
                              '(1 (:a :b) (:c :d)))
         [{"foo" :b}]))
  (is (empty? (matching-extensions `(1 (:a ~(variable "foo" nil))
                                       ~(not-query `(:c ~(variable "foo" nil))))
                                   '(1 (:a :b) (:c :b))))))

(deftest matching-elements-test
  (is (= (matching-elements '(nil ("a")) '(nil (1 ("A" 3)) (3 (4 5))))
         ['(1 ("A" 3))]))
  (let [[sa ia] (get-new-object-id (new-element-store))
        ib (make-item-id "ib") ;; a named object
        [sc ic] (get-new-object-id sa)
        [s1 id1] (add-element sa ia '(1 ("a" 3)))
        [s2 id2] (add-element s1 ia '(3 (4 5)))
        [s3 id3] (add-element s2 ib '(1 ("a" 4)))
        [s4 id4] (add-element s3 ic '(2 ("C" :label) ("C" :label)))
        [s5 id5] (add-element s4 ia (make-element-list
                                     :target (id->object ib s4) '("reversed")))
        [s id6] (add-element s5 ia `(~(make-object-list '(1)) 3))]
    (let [matches (matching-elements '(nil ("A"))
                                     (id->object ia s))]
      (is (= (map to-list matches)
             ['(1 ("a" 3))])))
    (let [matches (matching-elements nil
                                     (id->object ia s))]
      (is (check (map to-list matches)
                 (as-set ['(1 ("a" 3))
                          '(3 (4 5))
                          `(~(make-object-list '(1)) 3)
                          (make-element-list
                           :target (id->object ib s) '("reversed"))]))))
    ;; Test elements of an element.
    (let [matches (matching-elements "A" (id->element id1 s))]
      (is (= (map to-list matches)
             ['("a" 3)])))
    ;; Test matching an element with an object as content.
    (let [matches (matching-elements `(~(make-object-list '(1)))
                                     (id->object ia s))]
      (is (= matches
             [(id->element id6 s)])))
    ;; Test matching the link that was put in backwards.
    (let [term (make-element-list :source (id->object ia s) nil)]
      (is (= (matching-elements term (id->object ib s))
             [(id->element id5 s)])))
    (let [term (make-element-list :source (id->object ib s) nil)]
      (is (= (matching-elements term (id->object ia s))
             nil)))
    ;; Test matching elements in reversed orientation
    (let [term (make-element-list :target (id->object ib s) nil)]
      (is (= (matching-elements term (id->object ia s))
             [(id->element id5 :target s)])))
    (let [term (make-element-list :target (id->object ia s) nil)]
      (is (= (matching-elements term (id->object ib s))
             nil)))
    ;; Test a complex term that can match the element more than one
    ;; way.  (There had been a bug where this would return the same
    ;; element multiple times.)
    (let [matches (matching-elements '(nil ("C" :label))
                                     (id->object ic s))]
      (is (= (map to-list matches)
             ['(2 ("C" :label) ("C" :label))])))))

(deftest query-matches-test
  (let [s0 (new-element-store)
        [s1 id1] (add-element s0 (make-item-id "a") '(:a (1 (2 3)) (3 (4 5))))
        [s2 id2] (add-element s1 (make-item-id "b") '(:b (1 (2 4))))
        [s-more ids] (add-element s2 (make-item-id "c") '(:c 1 (1 2) (1 3) 2))
        [s-relationship idr] (add-element
                              s2 (make-item-id "a")
                              `(~(id->object (make-item-id "d") nil)))]
    ;; primitives
    (is (= (query-matches :a s2)
           [{}]))
    (is (empty? (query-matches :x s2)))
    ;; elements
    (is (= (query-matches '(1 (2)) s2) [{}]))
    (is (= (query-matches '(1 (3)) s2)) nil)
    (is (= (query-matches '(nil (2)) s2) [{}]))
    (is (= (query-matches '(nil (1)) s2) [{}]))
    (is (= (query-matches '(:a) s2) [{}]))
    (is (= (query-matches '(:x) s2) nil))
    ;; reversed elements
    (let [item-a (id->object (make-item-id "a") s-relationship)
          item-d (id->object (make-item-id "d") s-relationship)]
      (is (= (query-matches (make-element-list :target item-a nil)
                            s-relationship)
             [{}]))
      (is (= (query-matches (make-element-list :target item-d nil)
                            s-relationship)
             nil))
      (is (= (query-matches (make-element-list :source item-a nil)
                            s-relationship)
             nil))
      (is (= (query-matches (make-element-list :source item-d nil)
                            s-relationship)
             [{}])))
    ;; objects
    (is (= (query-matches (make-object-list '(nil)) s2) [{}]))
    (is (= (query-matches (make-object-list '((:a))) s2) [{}]))
    (is (= (query-matches (make-object-list '((:a 1))) s2) [{}]))
    (is (= (query-matches (make-object-list '((:a 2))) s2) nil))
    (is (= (query-matches (make-object-list '((:d))) s2) nil))
    ;; variables as top level entities
    (is (check (set (envs-to-list (query-matches (variable "v") s2)))
               #{{"v" (id->object (make-item-id "a") s2)}
                 {"v" (id->object (make-item-id "b") s2)}
                 {"v" (as-set '(:a (3 (4 5)) (1 (2 3))))}
                 {"v" '(:b (1 (2 4)))}
                 {"v" '(3 (4 5))}
                 {"v" '(1 (2 3))}
                 {"v" '(1 (2 4))}
                 {"v" '(2 3)}
                 {"v" '(4 5)}
                 {"v" '(2 4)}
                 {"v" 5}
                 {"v" 4}
                 {"v" 3}}))
    (is (= (set (envs-to-list
                 (query-matches (variable "v" (make-object-list '((:a)))) s2)))
           #{{"v" (id->object (make-item-id "a") s2)}}))
    (is (= (set (envs-to-list
                 (query-matches
                  (variable "v" (make-object-list '((:a 1)))) s2)))
           #{{"v" (id->object (make-item-id "a") s2)}}))
    (is (= (set (envs-to-list
                 (query-matches
                  (variable "v" (make-object-list '((:a 2)))) s2)))
           #{}))
    (is (= (set (envs-to-list
                 (query-matches
                  (variable "v" (make-object-list '((:d)))) s2)))
           #{}))
    (is (= (set (envs-to-list
                 (query-matches (and-query `(1 ~(variable "v"))
                                           (variable "v"))
                                s2)))
           #{{"v" '(2 3)} {"v" '(2 4)}}))
    ;; variables inside items
    (is (= (set
            (envs-to-list
             (query-matches `(nil (1 ~(variable "v"))) s2)))
           #{{"v" '(2 4)} {"v" '(2 3)}}))
    (is (= (set (envs-to-list
                 (query-matches `(nil (1 (2 ~(variable "v")))) s2)))
           #{{"v" 4} {"v" 3}}))
    (is (= (envs-to-list
            (query-matches `(nil (1 ~(variable "v"))
                                 (3 ~(variable "v")))
                           s2))
           nil)) 
    (is (= (envs-to-list
            (query-matches `(nil (1 (2 ~(variable "v")))
                                 (~(variable "v")))
                           s2))
           [{"v" 3}]))
    (is (= (envs-to-list
            (query-matches `(nil (~(variable "v"))
                                 (1 (2 ~(variable "v"))))
                           s2))
           [{"v" 3}]))
    (let [matches (query-matches
                   `(1 (~(variable "v" nil true) 3))
                   s2)]
      (is (= (count matches) 0)))
    (let [matches (query-matches
                   `(1 ~(variable "v" '(2 3) true))
                   s2)]
      (is (= (count matches) 1)))
    (is (= (envs-to-list
            (query-matches (and-query `(nil (~(variable "v") 4))
                                      `(1 (~(variable "v" nil true) 3)))
                           s2))
           nil))
    ;; and
    (is (= (envs-to-list
            (query-matches (and-query `(1 (~(variable "v") 3))
                                      `(nil (~(variable "v") 4)))
                           s2))
           [{"v" 2}]))
    (is (= (query-matches (and-query `(1 (~(variable "v" nil true) 3))
                                     `(nil (~(variable "v" nil true) 4)))
                          s2)
           nil))
    (is (= (query-matches (and-query `(1 (~(variable "v") 3))
                                     `(nil (~(variable "v") 5)))
                          s2)
           nil))
    (is (= (envs-to-list
            (query-matches (and-query `(~(variable "c") (~(variable "v") 3))
                                      `(~(variable "c") (~(variable "v") 4)))
                           s2))
           [{"v" 2, "c" 1}]))
    (is (= (query-matches (and-query `(~(variable "v") (~(variable "c") 3))
                                     `(~(variable "c") (~(variable "v") 4)))
                          s2)
           nil))
    (is (= (set (envs-to-list
                 (query-matches (and-query `(nil ~(variable "v"))
                                           `(nil (nil ~(variable "v"))))
                                s2)))
           #{{"v" '(2 3)} {"v" '(2 4)} {"v" 3} {"v" 4} {"v" '(4 5)} {"v" 5}}))
    (is (= (set (envs-to-list
                 (query-matches (and-query `(nil (~(variable "v")))
                                           `(nil (nil ~(variable "v"))))
                                s2)))
           #{{"v" 3} {"v" 4} {"v" 5}}))
    ;; exists
    (is (= (query-matches (exists-query "v" nil
                                        `(nil (1 ~(variable "v"))
                                              (3 ~(variable "v"))))
                          s2)
           nil)) 
    (is (= (query-matches (exists-query "v" nil
                                        `(nil (1 (2 ~(variable "v")))
                                              (~(variable "v"))))
                          s2)
           [{}]))
    (is (= (envs-to-list
            (query-matches (exists-query "c" nil
                                         `(nil (1 (2 ~(variable "v")))
                                               (~(variable "v"))))
                           s2))
           [{"v" 3}]))
    (is (= (query-matches (exists-query  "v"  `(1 (~(variable "v") 3))
                                         `(nil (~(variable "v") 4)))
                          s2)
           [{}]))
    (is (= (query-matches (exists-query  "v" `(1 ~(variable "v"))
                                         `(3 ~(variable "v")))
                          s2)
           nil))
    ;; forall
    (is (= (query-matches (forall-query "v" `(1 (~(variable "v") 3))
                                        `(1 (~(variable "v") 4)))
                          s2)
           [{}]))
    (is (= (query-matches (forall-query "v" `(1 (~(variable "v") 3))
                                        `(1 (2 ~(variable "v"))))
                          s2)
           nil))
    (is (= (query-matches (forall-query "v"  `(1 ~(variable "v"))
                                        (variable "v"))
                          s2)
           [{}]))
    ;; not inside
    (is (= (query-matches `(2 ~(not-query 7))
                          s2)
           [{}]))
    (is (= (map listify-map-values
                (query-matches (variable "v" `(2 ~(not-query 3)))
                               s2))
           
           [{"v" '(2 4)}]))
    (is (empty? (query-matches `(1 ~(not-query 2))
                               s2)))
    (is (check (map listify-map-values
                    (query-matches `(nil (1 ~(variable "v"))
                                         ~(not-query (variable "v")))
                                   s-more))
               ;; Shouldn't have {"v" '(1 2)}, because of the not.
               (as-set [{"v" '(2 3)} {"v" '(2 4)} {"v" 3}])))
    ;; Shouldn't match because we require variables to be bound to
    ;; exact entities, not to be bound to something that can be
    ;; extended to match.
    (is (empty? (query-matches `(nil ~(variable "v")  ~(variable "v"))
                               s-more)))))

(deftest matching-items-test
  (let [ia (make-item-id "A")
        ib (make-item-id "B")
        s0 (new-element-store)
        [s1 id1] (add-element s0 ia '(1 (2 3)))
        [s2 id2] (add-element s1 ia '(3 (4 5)))
        [s3 id3] (add-element s2 ib '(1 (2 4)))]
    ;; elements
    (let [matches (matching-items '(nil (2)) s3)]
      (is (= (set (map :item-id matches)) #{id1 id3})))
    (let [matches (matching-items '(nil (3)) s3)]
      (is (= matches [(first (elements (id->element id1 s3)))])))
    (let [matches (matching-items '(nil (2 3)) s3)]
      (is (= (map :item-id matches) [id1])))
    (let [matches (matching-items `(nil (2 ~(not-query 3))) s3)]
      (is (= (map :item-id matches) [id3])))
    ;; objects
    (let [matches (matching-items (make-object-list ['(nil (2))]) s3)]
      (is (= (set (map :item-id matches)) #{ia ib})))
    (let [matches (matching-items (make-object-list ['(nil (3))]) s3)]
      (is (= matches [])))
    (let [matches (matching-items (make-object-list ['(nil (nil (3)))]) s3)]
      (is (= (map :item-id matches) [ia])))
    (let [matches (matching-items
                   (make-object-list [`(nil (2 ~(not-query 3)))]) s3)]
      (is (= (map :item-id matches) [ib])))))

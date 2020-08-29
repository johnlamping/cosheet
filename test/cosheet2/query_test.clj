(ns cosheet2.query-test
  (:require [clojure.test :refer [deftest is]]
            clojure.pprint
            (cosheet2 [store :refer [new-element-store make-id]]
                      [store-impl :refer [->ItemId]]
                      [store-utils :refer [add-entity]]
                      [entity :refer [to-list description->entity content
                                      elements label->elements mutable-entity?
                                      has-keyword? atom?]]
                      entity-impl
                      [query :refer :all]
                      [query-impl :refer [bind-entity closest-template]]
                      [test-utils :refer [check as-set]]
                     )
            ; :reload
            ))

(deftest extended-by-test
  (let [element0 '(3 "Foo")
        element1 '(3 ("foo" :label))
        element2 '((3))
        itemx `(nil ~element0 ~element1)
        itemy `(nil ~element2)]
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
    (is (extended-by? itemy itemx))
    (is (not (extended-by? itemx itemy)))
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
    (is (extended-by? 3 element0))
    (is (extended-by? 3 element2))
    (is (extended-by? element2 3))
    (is (not (extended-by? element2 4)))
    (is (extended-by? 3 element1))
    (is (not (extended-by? element1 3)))
    (is (extended-by? `(1 ~(not-query :x))
                      1))
    (is (not (extended-by? `(1 ~(not-query :a))
                           '(1 :a))))
    (is (extended-by? `(1 :a :b ~(not-query :x) ~(not-query :u))
                      '(1 :a (:b :c))))
    (is (not (extended-by? `(1 :a :b :c ~(not-query :x) ~(not-query :u))
                           '(1 :a (:b :c)))))
    (is (not (extended-by?
              `(1 :a :b ~(not-query :x) ~(not-query :u) ~(not-query :a))
              '(1 :a (:b :c)))))
    (is (extended-by?
         `(1 :a (:b ~(not-query :x))  ~(not-query `(:b ~(not-query :c))))
         '(1 :a (:b :c))))
    (is (not (extended-by? `(1 :a (:b ~(not-query :c)))
                           '(1 :a (:b :c)))))
    (is (not (extended-by? `(1 :a :b ~(not-query `(:b ~(not-query :d))))
                           '(1 :a (:b :c)))))))

(defn variable
  ([name] (variable-query name))
  ([name qualifier] (variable-query name :qualifier qualifier))
  ([name qualifier reference]
   (variable-query name
                   :qualifier qualifier
                   :reference reference)))

(deftest closest-template-test
  (is (= (closest-template `(~(variable "foo" 5)
                               ~(description->entity
                                 (make-id "test")
                                 (new-element-store))
                               (:foo ~(variable "baz" (variable "bar")))
                               ~(not-query 8))
                             {"bar" 7})
         ['(5 (:foo 7)) false]))
  (is (= (closest-template '(1 2 (3 4))
                             {"bar" 7})
         ['(1 2 (3 4)) true]))
  (is (= (closest-template `(1 2 (3 ~(variable "foo" 5)))
                             {"bar" 7})
         ['(1 2 (3 5)) false]))
  (is (thrown? java.lang.AssertionError
               (closest-template `(~(and-query (variable "foo" 5)
                                               (variable "bar" 6)))
                                   {"bar" 7}))))

(deftest bound-entity-test
  (let [entity `(~(variable-query "foo")
                 (4 ~(variable-query "bar")))
        partially-bound (bind-entity entity {"foo" :a})
        bound (bind-entity entity {"foo" :b, "bar" :c})
        alternate-bound (bind-entity entity {"foo" :b, "bar" 9})]
    (is (= (content bound) :b))
    (is (= (mutable-entity? bound) false))
    (is (= (atom? bound) false))
    (is (= (to-list partially-bound)
           `(:a (4 ~(variable-query "bar")))))
    (is (= (to-list bound)
           '(:b (4 :c))))
    (is (= (map to-list (label->elements bound :b))
           ()))
    (is (= (map to-list (label->elements bound :c))
           '((4 :c))))
    (is (has-keyword? (first (label->elements bound :c)) :c))
    (is (not (has-keyword? (first (label->elements bound :c)) :b)))
    (is (= (map to-list (label->elements alternate-bound 9))
           ()))
    (is (= (map to-list (label->elements partially-bound 9))
           ()))))

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
  ;; Duplicates in term
 
  (let [s (new-element-store)
        [s1 id1] (add-entity s nil '(1 2))]
    (is (empty? (matching-extensions '(1 2 2) {:a :b}
                                     (description->entity id1 s1)))))
  (let [s (new-element-store)
        [s1 id1] (add-entity s nil '(1 2 2))]
    (is (= (matching-extensions '(1 2 2) {:a :b}
                                (description->entity id1 s1))
           [{:a :b}])))
  (let [s (new-element-store)
        [s1 id1] (add-entity s nil '(1 2 2 2))]
    (is (= (matching-extensions '(1 2 2) {:a :b}
                                (description->entity id1 s1))
           [{:a :b}])))
  ;; Variables
  (is (= (matching-extensions (variable "foo") {:a :b}
                              2)
         [{:a :b, "foo" 2}]))
  (is (= (matching-extensions (variable "foo") {:a :b}
                              '(1 (2 3)))
         [{:a :b, "foo" '(1 (2 3))}]))
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
  (is (= (matching-extensions `(1 (~(variable "foo" 2) :foo)) {:a :b}
                              '(1 (3 :foo)))
         nil))
  (is (= (matching-extensions `(~(variable "foo" 1) ~(variable "foo")) {:a :b}
                              '(1 (1 :foo)))
         nil))
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

(deftest best-matching-term-test
  (is (check (best-matching-term ['(1) '(1 2) '(1 2 5)] {:a :b} '(1 2 3 4))
             '(1 2)))
  (is (check (best-matching-term ['(1 2 5) '(1 2) '(1)] {:a :b} '(1 2 3 4))
             '(1 2)))
  (is (check (best-matching-term ['(1 2 5)] {:a :b} '(1 2 3 4))
             nil)))

(deftest matching-elements-test
  (is (= (matching-elements '(nil ("a")) '(nil (1 ("A" 3)) (3 (4 5))))
         ['(1 ("A" 3))]))
  (let [ia (->ItemId "A")
        ib (->ItemId "B")
        s0 (new-element-store)
        [s1 id1] (add-entity s0 ia '(1 ("a" 3)))
        [s2 id2] (add-entity s1 ia '(3 (4 5)))
        [s3 id3] (add-entity s2 ib '(1 ("a" 4)))]
    (let [matches (matching-elements '(nil ("A"))
                                     (description->entity ia s3))]
      (is (= (map #(to-list %) matches)
             ['(1 ("a" 3))])))
    (let [matches (matching-elements nil
                                     (description->entity ia s3))]
      (is (= (set (map #(to-list %) matches))
             (set ['(1 ("a" 3)) '(3 (4 5))]))))))

(deftest query-matches-test
  (let [s0 (new-element-store)
        [s1 id1] (add-entity s0 nil '(:a (1 (2 3)) (3 (4 5))))
        [s2 id2] (add-entity s1 nil '(:b (1 (2 4))))]
    ;; atoms
    (is (= (query-matches :a s2)
           [{}]))
    (is (empty? (query-matches :x s2)))
    ;; elements
    (is (= (query-matches '(1 (2)) s2) [{}]))
    (is (= (query-matches '(1 (3)) s2)) nil)
    (is (= (query-matches '(nil (2)) s2) [{}]))
    (is (= (query-matches '(nil (1)) s2) [{}]))
    ;; variables as top level entities
    (is (check (set (envs-to-list (query-matches (variable "v") s2)))
               #{{"v" (as-set '(:a (3 (4 5)) (1 (2 3))))}
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
    (is (= (query-matches (variable "v" `(2 ~(not-query 3)))
                          s2)
           [{"v" '(2 4)}]))
    (is (empty? (query-matches `(1 ~(not-query 2))
                               s2)))))

(deftest matching-items-test
  (let [ia (->ItemId "A")
        ib (->ItemId "B")
        s0 (new-element-store)
        [s1 id1] (add-entity s0 ia '(1 (2 3)))
        [s2 id2] (add-entity s1 ia '(3 (4 5)))
        [s3 id3] (add-entity s2 ib '(1 (2 4)))]
    (let [matches (matching-items '(nil (2)) s3)]
      (= (map :item-id matches) [ia ib]))
    (let [matches (matching-items '(nil (3)) s3)]
      (= (map :item-id matches) [ia]))
    (let [matches (matching-items '(nil (2 3)) s3)]
      (= (map :item-id matches) [ia]))
    (let [matches (matching-items '(nil (2 (not-query 3))) s3)]
      (= (map :item-id matches) [ib]))))

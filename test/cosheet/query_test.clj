(ns cosheet.query-test
  (:require [clojure.test :refer [deftest is]]
            clojure.pprint
            (cosheet [compute :as compute]
                     [store-utils :as store-utils]
                     [entity :as entity]
                     [store :as store]
                     [query :refer :all]
                     [query-impl :refer :all]
                     compute-impl
                     [store-impl :refer [->ItemId]]
                     mutable-store-impl
                     entity-impl
                     [debug :refer [current-value let-propagated envs-to-list]]
                     )
            ; :reload
            ))

(deftest extended-by-test
  (let [element0 '(3 "foo")
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
    (is (not (extended-by? element1 element0)))
    (is (extended-by? itemx itemx))
    (is (extended-by? itemy itemx))
    (is (not (extended-by? itemx itemy)))
    (is (let-propagated [a '(3 ("foo" false)) b '(3 ("foo" false))]
                        (extended-by?9 a b)))
    (is (let-propagated [b '(3 ("foo" false))]
                        (extended-by?9 '(nil ("foo" false)) b)))
    (is (not (let-propagated [a '(4 ("foo" false)) b '(3 ("foo" false))]
                             (extended-by?9 a b))))
    (is (let-propagated [a '(3 "foo") b '(3 ("foo" false))]
                        (extended-by?9 a b)))
    (is (not (let-propagated [a '(3 ("foo" false)) b '(3 ("foo"))]
                             (extended-by?9 a b))))
    (is (let-propagated [a element0] (extended-by?9 3 a)))
    (is (extended-by? 3 element2))
    (is (extended-by? element2 3))
    (is (not (extended-by? element2 4)))
    (is (extended-by? 3 element1))
    (is (not (extended-by? element1 3)))
    (is (extended-by? 3 (entity/content-reference element0)))))

(defn variable
  ([name] (variable name nil nil))
  ([name condition] (variable name condition nil nil))
  ([name condition value-may-extend]
     (variable name condition value-may-extend nil))
  ([name condition value-may-extend reference]
     `(:variable
       ~@(if name `((~name :name)))
       ~@(if condition `((~condition :condition)))
       ~@(if value-may-extend `((true :value-may-extend)))
       ~@(if reference `((true :reference))))))

(deftest bound-entity-test
  (let [entity `(~(variable "foo")
                 (4 ~(variable "bar")))
        partially-bound (bind-entity entity {"foo" 7})
        bound (bind-entity entity {"foo" 7, "bar" 9})]
    (is (= (entity/to-list partially-bound)
           '(7 (4 (:variable ("bar" :name))))))
    (is (= (entity/to-list bound)
           '(7 (4 9))))
    (is (= (map entity/to-list (entity/label->elements bound 9))
           '((4 9))))
    (is (= (map entity/to-list (entity/label->elements bound 4))
           ()))
    (is (= (map entity/to-list (entity/label->elements partially-bound 9))
           ()))))

(deftest template-matches-test
  (is (= (template-matches 1 {}  1) [{}]))
  (is (= (template-matches 1 {} 1) [{}]))
  (is (= (template-matches 1 {} 2) nil))
  (is (= (template-matches 1 {:a :b} 1) [{:a :b}]))
  (is (= (template-matches '(1) {:a :b} 1) [{:a :b}]))
  (is (= (let-propagated [x '(1)] (template-matches9 1 {:a :b} x)) [{:a :b}]))
  (is (= (let-propagated [x '(1)] (template-matches9 x {:a :b} x)) [{:a :b}]))
  (is (= (let-propagated [x '(1 2)] (template-matches9 1 {:a :b} x)) [{:a :b}]))
  (is (= (let-propagated [x '(1) y '(1 2)] (template-matches9 x {:a :b} y))
         [{:a :b}]))
  (is (= (let-propagated [x '(1 2)] (template-matches9 x {:a :b} x)) [{:a :b}]))
  (is (= (let-propagated [x '(1 2 3) y '(1 (2 3))]
                         (template-matches9 x {:a :b} y))
         nil))
  (is (= (let-propagated [x '(1 (2 3)) y '(1 2 3)]
                         (template-matches9 x {:a :b} y))
         nil))
  (is (= (let-propagated [x '(1 (4 6))]
                         (template-matches9 x {:a :b} x))
         [{:a :b}]))
  (is (= (let-propagated [x '((1 2) 3 (4 (5 6)))]
                         (template-matches9 x {:a :b} x))
         [{:a :b}]))
  (is (= (let-propagated [x 2]
                         (template-matches9 (variable "foo") {:a :b} x))
         [{:a :b, "foo" 2}]))
  (is (= (let-propagated [x '((1 2) 3)]
                         (template-matches9 (variable "foo") {:a :b} x))
         [{:a :b, "foo" '((1 2) 3)}]))
  (is (= (let-propagated [v `(1 ~(variable "foo"))
                          x '(1 2)]
                         (template-matches9 v {:a :b} x))
         [{:a :b, "foo" 2}]))
  (is (= (let-propagated [v `(1 (~(variable "foo") :foo))
                          x '(1 (2 :foo))]
                         (template-matches9 v {:a :b} x))
         [{:a :b, "foo" 2}]))
  (is (= (let-propagated [v `(1 (~(variable "foo") :foo))
                          x '(1 (2 :bar :foo))]
                         (template-matches9 v {:a :b} x))
         [{:a :b, "foo" 2}]))
  (is (= (let-propagated [v `(1 (~(variable "foo") :foo))
                          x '(1 (2 :bar))]
                         (template-matches9 v {:a :b} x))
         nil))
  (is (= (let-propagated [v `(1 (~(variable "foo") :foo))
                          x '(1 2)]
                         (template-matches9 v {:a :b} x))
         nil))
  (is (= (let-propagated [v `(1 (~(variable "foo") false))
                          x '(1 (2 false))]
                         (template-matches9 v {:a :b} x))
         [{:a :b, "foo" 2}]))
  (is (= (let-propagated [v `(1 (~(variable "foo") false))
                          x '(1 (2 true))]
                         (template-matches9 v  {:a :b} x))
           nil))
  (is (= (set (let-propagated [v `(1 (~(variable "foo") :foo))
                               x '(1 (2 :foo) (3 :foo) (2 :foo) (4 :bar))]
                              (template-matches9 v {:a :b} x)))
         #{{:a :b, "foo" 2} {:a :b, "foo" 3}}))
  (is (= (let-propagated [v `(1 (~(variable "foo") :foo)
                                (~(variable "foo") :bar))
                          x '(1 (2 :foo) (2 :bar))]
                         (template-matches9 v {:a :b} x))
         [{:a :b, "foo" 2}]))
  (is (= (let-propagated [x '(1 (2 :foo) (2 :bar))]
                         (template-matches9 `(nil (~(variable "foo") :foo)
                                                  (~(variable "foo") :bar))
                                            {:a :b}
                                            x))
         [{:a :b, "foo" 2}]))
  (is (= (let-propagated [v `(2 (~(variable "foo") :foo)
                                (~(variable "foo") :bar))
                          x '(1 (2 :foo) (2 :bar))]
                         (template-matches9 v {:a :b} x))
         nil))
  (is (= (let-propagated [v `(~(variable "foo") ~(variable "foo"))
                          x '(1 1)]
                         (template-matches9 v {:a :b} x))
         [{:a :b, "foo" 1}]))
  (is (= (let-propagated [v `(~(variable "foo") ~(variable "foo"))
                          x '(1 2)]
                         (template-matches9 v {:a :b} x))
         nil))
  (is (= (let-propagated [v `(~(variable "foo") ~(variable "foo"))
                          x '(1 2 3 1)]
                         (template-matches9 v {:a :b} x))
         [{:a :b, "foo" 1}]))
  (is (= (let-propagated [v `(1 (~(variable "foo") :foo)
                                (~(variable "foo") :bar))
                          x '(1 (2 :foo) (3 :bar))]
                         (template-matches9 v {:a :b} x))
         nil))
  (is (= (set (let-propagated [v `(1 (~(variable "foo") :foo)
                                     (~(variable "foo") :bar))
                               x '(1 (1 :foo) (2 :foo)
                                     (1 :bar) (2 :bar) (3 :bar))]
                              (template-matches9 v {:a :b} x)))
         #{{:a :b, "foo" 1} {:a :b, "foo" 2}}))
  (is (= (let-propagated [v `(1 (~(variable "foo") :foo)
                                (~(variable "bar") :bar))
                          x '(1 (2 :foo) (3 :bar))]
                         (template-matches9 v {:a :b} x))
         [{:a :b, "foo" 2, "bar" 3}]))
  (is (= (let-propagated [v `(1 (~(variable "foo") :foo)
                                (~(variable nil) :bar))
                          x '(1 (2 :foo) (3 :bar))]
                         (template-matches9 v {:a :b} x))
         [{:a :b, "foo" 2}]))
  (is (= (let-propagated [v `(1 (~(variable nil) :foo)
                                (~(variable nil) :bar))
                          x '(1 (2 :foo) (3 :bar))]
                         (template-matches9 v {:a :b} x))
         [{:a :b}]))
  (is (= (let-propagated [v `(1 (~(variable "foo" 2) :foo))
                          x '(1 (2 :foo))]
                         (template-matches9 v {:a :b} x))
         [{:a :b, "foo" 2}]))
  (is (= (let-propagated [v `(1 (~(variable "foo" 2) :foo))
                          x '(1 (3 :foo))]
                         (template-matches9 v {:a :b} x))
         nil))
  (is (= (let-propagated [v `(~(variable "foo" 1) ~(variable "foo"))
                          x '(1 (1 :foo))]
                         (template-matches9 v {:a :b} x))
         nil))
  (is (= (let-propagated [v `(~(variable "foo" 1)
                              ~(variable "foo" nil true))
                          x '(1 (1 :foo))]
                         (template-matches9 v {:a :b} x))
         [{:a :b, "foo" 1}])))

(deftest query-matches-test
  (let [ia (->ItemId "A")
        ib (->ItemId "B")
        s1 (first (store-utils/add-entity
                   (store/new-element-store) ia '(1 (2 3))))
        s2 (first (store-utils/add-entity s1 ia '(3 (4 5))))
        store (first (store-utils/add-entity s2 ib '(1 (2 4))))]
    ;; elements
    (is (= (query-matches '(1 (2)) store) [{}]))
    (is (= (query-matches '(nil (2)) store) [{}]))
    (is (= (query-matches '(nil (1)) store) [{}]))
    ;; variables as top level entities
    (is (= (set (map (fn [entry] {"v" (entity/to-list (entry "v"))})
                     (query-matches (variable "v") store)))
           #{{"v" '(nil (3 (4 5)) (1 (2 3)))}
             {"v" '(nil (1 (2 4)))}
             {"v" '(3 (4 5))}
             {"v" '(1 (2 3))}
             {"v" '(1 (2 4))}
             {"v" '(2 3)}
             {"v" '(4 5)}
             {"v" '(2 4)}}))
    (is (= (set (query-matches `(:and ((1 ~(variable "v")) :first)
                                      (~(variable "v") :second))
                               store))
           #{{"v" '(2 3)} {"v" '(2 4)}}))
    ;; variables inside items
    (is (= (set
            (query-matches `(nil (1 ~(variable "v"))) store))
           #{{"v" '(2 4)} {"v" '(2 3)}}))
    (is (= (set (query-matches `(nil (1 (2 ~(variable "v")))) store))
           #{{"v" 4} {"v" 3}}))
    (is (= (query-matches `(nil (1 ~(variable "v"))
                                (3 ~(variable "v")))
                          store)
           nil)) 
    (is (= (query-matches `(nil (1 (2 ~(variable "v")))
                                (~(variable "v")))
                          store)
           [{"v" 3}]))
    (is (= (query-matches `(nil (~(variable "v"))
                                (1 (2 ~(variable "v"))))
                          store)
           [{"v" 3}]))
    (let [matches (query-matches `(:and ((1 (~(variable "v" nil nil true) 3))
                                         :first)
                                        ((nil (~(variable "v") 4))
                                         :second))
                                 store)]
      (is (not= matches [{"v" 2}]))
      (is (= (envs-to-list matches) [{"v" 2}]))
      (is (not= ((first matches) "v") 2))
      (is (not (nil? (:item-id ((first matches) "v") 2)))))
    (is (= (envs-to-list
            (query-matches `(:and ((nil (~(variable "v") 4))
                                   :first)
                                  ((1 (~(variable "v" nil nil true) 3))
                                   :second))
                           store))
           nil))
    ;; and
    (is (= (query-matches `(:and ((1 (~(variable "v") 3)) :first)
                                 ((nil (~(variable "v") 4)) :second))
                          store)
           [{"v" 2}]))
    (is (= (query-matches `(:and ((1 (~(variable "v" nil nil true) 3))
                                  :first)
                                 ((nil (~(variable "v" nil nil true) 4))
                                  :second))
                          store)
           nil))
    (is (= (query-matches `(:and ((1 (~(variable "v") 3)) :first)
                                 ((nil (~(variable "v") 5)) :second))
                          store)
           nil))
    (is (= (query-matches `(:and ((~(variable "c") (~(variable "v") 3))
                                  :first)
                                 ((~(variable "c") (~(variable "v") 4))
                                  :second))
                          store)
           [{"v" 2, "c" 1}]))
    (is (= (query-matches `(:and ((~(variable "v") (~(variable "c") 3))
                                  :first)
                                 ((~(variable "c") (~(variable "v") 4))
                                  :second))
                          store)
           nil))
    (is (= (set (query-matches `(:and ((nil (~(variable "v"))) :first)
                                      ((nil (nil ~(variable "v"))) :second))
                               store))
           #{{"v" 3} {"v" 4} {"v" 5}}))
    ;; exists
    (is (= (query-matches `(:exists ("v" :variable-name)
                                    ((nil (1 ~(variable "v"))
                                          (3 ~(variable "v")))
                                     :body))
                          store)
           nil)) 
    (is (= (query-matches `(:exists ("v" :variable-name)
                                    ((nil (1 (2 ~(variable "v")))
                                          (~(variable "v")))
                                     :body))
                          store)
           [{}]))
    (is (= (query-matches `(:exists ("c" :variable-name)
                                    ((nil (1 (2 ~(variable "v")))
                                          (~(variable "v")))
                                     :body))
                          store)
           [{"v" 3}]))
    (is (= (query-matches `(:exists  ("v" :variable-name)
                                     ((1 (~(variable "v") 3)) :qualifier)
                                     ((nil (~(variable "v") 4)) :body))
                          store)
           [{}]))
    (is (= (query-matches `(:exists  ("v" :variable-name)
                                     ((1 ~(variable "v")) :qualifier)
                                     ((3 ~(variable "v")) :body))
                          store)
           nil))
    ;; forall
    (is (= (query-matches `(:forall ("v" :variable-name)
                                    ((1 (~(variable "v") 3)) :qualifier)
                                    ((1 (~(variable "v") 4)) :body))
                          store)
           [{}]))
    (is (= (query-matches `(:forall ("v" :variable-name)
                                    ((1 (~(variable "v") 3)) :qualifier)
                                    ((1 (2 ~(variable "v"))) :body))
                          store)
           nil))
    (is (= (query-matches `(:forall ("v" :variable-name)
                                    ((1 ~(variable "v")) :qualifier)
                                    (~(variable "v") :body))
                          store)
           [{}]))
))

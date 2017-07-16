(ns cosheet.query-test
  (:require [clojure.test :refer [deftest is]]
            clojure.pprint
            (cosheet [store :as store]
                     [store-impl :refer [->ItemId]]
                     [store-utils :as store-utils]
                     [entity :as entity]
                     [query :refer :all]
                     [query-impl :refer [bind-entity]]
                     [expression-manager :refer [current-value]]
                     [debug :refer [envs-to-list]]
                     [test-utils :refer [check as-set
                                         let-mutated let-mutated-store]]
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
    (is (let [a '(3 ("foo" false))]
          (let-mutated [b '(3 ("foo" false))]
            (extended-by? a b))))
    (is (let-mutated [b '(3 ("Foo" false))]
          (extended-by? '(nil ("foo" false)) b)))
    (is (not (let [a '(4 ("Foo" false))]
               (let-mutated [b '(3 ("foo" false))]
                 (extended-by? a b)))))
    (is (let [a '(3 "foo")]
          (let-mutated [b '(3 ("Foo" false))]
            (extended-by? a b))))
    (is (not (let [a '(3 ("foo" false))]
               (let-mutated [b '(3 ("foo"))]
                 (extended-by? a b)))))
    (is (let [a '(3 ("Foo" (4 false)))]
          (let-mutated [b '(3 ("foo" (4 false)))]
            (extended-by? a b))))
    (is (not (let [a '(3 ("foo" (4 false)))]
               (let-mutated [b '(3 ("foo" (4 true)))]
                 (extended-by? a b)))))
    (is (not (let [a '(3 2 2)]
               (let-mutated [b '(3 2)]
                 (extended-by? a b)))))
    (is (let-mutated [a element0] (extended-by? 3 a)))
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
    (is (= (entity/content bound) 7))
    (is (= (entity/mutable-entity? bound) false))
    (is (= (entity/atom? bound) false))
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
  (is (= (current-value (template-matches 1 {} 1)) [{}]))
  (is (= (current-value (template-matches "a" {} "A")) [{}]))
  (is (= (current-value (template-matches "A" {} "a")) [{}]))
  (is (= (current-value (template-matches 1 {} 2)) nil))
  (is (= (current-value (template-matches 1 {:a :b} 1)) [{:a :b}]))
  (is (= (current-value (template-matches '(1) {:a :b} 1)) [{:a :b}]))
  (is (= (let-mutated [x '(1)] (template-matches 1 {:a :b} x)) [{:a :b}]))
  (is (= (let-mutated [x '(1)] (template-matches '(1) {:a :b} x)) [{:a :b}]))
  (is (= (let-mutated [x '(1 2)] (template-matches 1 {:a :b} x)) [{:a :b}]))
  (is (= (let-mutated [y '(1 2)] (template-matches '(1) {:a :b} y))
         [{:a :b}]))
  (is (= (let [x '(1 2)]
           (let-mutated [y x] (template-matches x {:a :b} y))) [{:a :b}]))
  (is (= (let [x '(1 2 3)]
           (let-mutated [ y '(1 (2 3))]
             (template-matches x {:a :b} y)))
         nil))
  (is (= (let [x '(1 (2 3))]
           (let-mutated [y '(1 2 3)]
             (template-matches x {:a :b} y)))
         nil))
  ;; The next two test a special case optimization.
  (is (= (let-mutated [y '(1 (2 3 4))]
                      (template-matches '(nil (nil 3)) {:a :b} y))
         [{:a :b}]))
  (is (= (let-mutated [y '(1 (2 3 4))]
           (template-matches '(nil (nil 2)) {:a :b} y))
         nil))
  (is (= (let [x '(1 (4 6))]
           (let-mutated [y x]
             (template-matches x {:a :b} y)))
         [{:a :b}]))
  (is (= (let [x '((1 2) 3 (4 (5 6)))]
           (let-mutated [y x]
             (template-matches x {:a :b} y)))
         [{:a :b}]))
  ;; Duplicates in template
  (is (empty? (let [x '(1 2 2)]
           (let-mutated [y '(1 2)]
             (template-matches x {:a :b} y)))))
  (is (= (let [x '(1 2 2)]
           (let-mutated [y '(1 2 2 2)]
             (template-matches x {:a :b} y)))
         [{:a :b}]))
  (is (= (let [x '(1 2 2)]
           (let-mutated [y '(1 2 2)]
             (template-matches x {:a :b} y)))
         [{:a :b}]))
  ;; Variables
  (is (= (let-mutated [x 2]
                      (template-matches (variable "foo") {:a :b} x))
         [{:a :b, "foo" 2}]))
  (is (= (let-mutated [x '(1 (2 3))]
                      (template-matches (variable "foo") {:a :b} x))
         [{:a :b, "foo" '(1 (2 3))}]))
  (is (= (let [v `(1 ~(variable "foo"))]
           (let-mutated [x '(1 2)]
             (template-matches v {:a :b} x)))
         [{:a :b, "foo" 2}]))
  (is (= (let [v `(1 (~(variable "foo") :foo))]
           (let-mutated [x '(1 (2 :foo))]
             (template-matches v {:a :b} x)))
         [{:a :b, "foo" 2}]))
  (is (= (let [v `(1 (~(variable "foo") :foo))]
           (let-mutated [x '(1 (2 :bar :foo))]
             (template-matches v {:a :b} x)))
         [{:a :b, "foo" 2}]))
  (is (= (let [v `(1 (~(variable "foo") :foo))]
           (let-mutated [x '(1 (2 :bar))]
             (template-matches v {:a :b} x)))
         nil))
  (is (= (let [v `(1 (~(variable "foo") :foo))]
           (let-mutated [x '(1 2)]
             (template-matches v {:a :b} x)))
         nil))
  (is (= (let [v `(1 (~(variable "foo") false))]
           (let-mutated [x '(1 (2 false))]
             (template-matches v {:a :b} x)))
         [{:a :b, "foo" 2}]))
  (is (= (let [v `(1 (~(variable "foo") false))]
           (let-mutated [x '(1 (2 true))]
             (template-matches v  {:a :b} x)))
           nil))
  (is (= (set (let [v `(1 (~(variable "foo") :foo))]
                (let-mutated [x '(1 (2 :foo) (3 :foo) (2 :foo) (4 :bar))]
                  (template-matches v {:a :b} x))))
         #{{:a :b, "foo" 2} {:a :b, "foo" 3}}))
  (is (= (let [v `(1 (~(variable "foo") :foo)
                     (~(variable "foo") :bar))]
           (let-mutated [x '(1 (2 :foo) (2 :bar))]
             (template-matches v {:a :b} x)))
         [{:a :b, "foo" 2}]))
  (is (= (let-mutated [x '(1 (2 :foo) (2 :bar))]
                      (template-matches `(nil (~(variable "foo") :foo)
                                              (~(variable "foo") :bar))
                                        {:a :b}
                                        x))
         [{:a :b, "foo" 2}]))
  (is (= (let [v `(2 (~(variable "foo") :foo)
                     (~(variable "foo") :bar))]
           (let-mutated [x '(1 (2 :foo) (2 :bar))]
             (template-matches v {:a :b} x)))
         nil))
  (println "start")
  (is (= (let [v `(~(variable "foo"))]
           (let-mutated [x '(1)]
             (template-matches v {:a :b} x)))
         [{:a :b, "foo" 1}]))
  (println "done")
  (is (= (let [v `(~(variable "foo") ~(variable "foo"))]
           (let-mutated [x '(1 1)]
             (template-matches v {:a :b} x)))
         [{:a :b, "foo" 1}]))
  (is (= (let [v `(~(variable "foo") ~(variable "foo"))]
           (let-mutated [x '(1 2)]
             (template-matches v {:a :b} x)))
         nil))
  (is (= (let [v `(~(variable "foo") ~(variable "foo"))]
           (let-mutated [x '(1 2 3 1)]
             (template-matches v {:a :b} x)))
         [{:a :b, "foo" 1}]))
  (is (= (let [v `(1 (~(variable "foo") :foo)
                     (~(variable "foo") :bar))]
           (let-mutated [x '(1 (2 :foo) (3 :bar))]
             (template-matches v {:a :b} x)))
         nil))
  (is (= (set (let [v `(1 (~(variable "foo") :foo)
                          (~(variable "foo") :bar))]
                (let-mutated [x '(1 (1 :foo) (2 :foo)
                                    (1 :bar) (2 :bar) (3 :bar))]
                  (template-matches v {:a :b} x))))
         #{{:a :b, "foo" 1} {:a :b, "foo" 2}}))
  (is (= (let [v `(1 (~(variable "foo") :foo)
                     (~(variable "bar") :bar))]
           (let-mutated [x '(1 (2 :foo) (3 :bar))]
             (template-matches v {:a :b} x)))
         [{:a :b, "foo" 2, "bar" 3}]))
  (is (= (let [v `(1 (~(variable "foo") :foo)
                     (~(variable nil) :bar))]
           (let-mutated [x '(1 (2 :foo) (3 :bar))]
             (template-matches v {:a :b} x)))
         [{:a :b, "foo" 2}]))
  (is (= (let [v `(1 (~(variable nil) :foo)
                     (~(variable nil) :bar))]
           (let-mutated [x '(1 (2 :foo) (3 :bar))]
             (template-matches v {:a :b} x)))
         [{:a :b}]))
  (is (= (let [v `(1 (~(variable "foo" 2) :foo))]
           (let-mutated [x '(1 (2 :foo))]
             (template-matches v {:a :b} x)))
         [{:a :b, "foo" 2}]))
  (is (= (let [v `(1 (~(variable "foo" 2) :foo))]
           (let-mutated [x '(1 (3 :foo))]
             (template-matches v {:a :b} x)))
         nil))
  (is (= (let [v `(~(variable "foo" 1) ~(variable "foo"))]
           (let-mutated [x '(1 (1 :foo))]
             (template-matches v {:a :b} x)))
         nil))
  (is (= (let [v `(~(variable "foo" 1)
                   ~(variable "foo" nil true))]
           (let-mutated [x '(1 (1 :foo))]
             (template-matches v {:a :b} x)))
         [{:a :b, "foo" 1}]))
  (is (check (let [x `(1 ~(variable "foo") ~(variable "bar"))]
               (let-mutated [y '(1 2 3 4)]
                 (template-matches x {:a :b} y)))
             (as-set [{:a :b "foo" 2 "bar" 3}
                      {:a :b "foo" 2 "bar" 4}
                      {:a :b "foo" 3 "bar" 2}
                      {:a :b "foo" 3 "bar" 4}
                      {:a :b "foo" 4 "bar" 2}
                      {:a :b "foo" 4 "bar" 3}]))))

(deftest matching-elements-test
  (is (= (matching-elements '(nil ("a")) '(nil (1 ("A" 3)) (3 (4 5))))
         ['(1 ("A" 3))]))
  (let [ia (->ItemId "A")
        ib (->ItemId "B")
        s0 (store/new-element-store)
        mutator (fn [s]
                  (store-utils/add-entity! s ia '(1 ("a" 3)))
                  (store-utils/add-entity! s ia '(3 (4 5)))
                  (store-utils/add-entity! s ib '(1 ("a" 4))))]
    (let [matches (let-mutated-store
                   [store s0 mutator]
                   (matching-elements '(nil ("A"))
                                      (entity/description->entity ia store)))]
      (is (= (map #(current-value (entity/to-list %)) matches)
             ['(1 ("a" 3))])))
    (let [matches (let-mutated-store
                   [store s0 mutator]
                   (matching-elements nil
                                      (entity/description->entity ia store)))]
      (is (= (map #(current-value (entity/to-list %)) matches)
             ['(1 ("a" 3)) '(3 (4 5))])))))

(deftest query-matches-test
  (let [s0 (store/new-element-store)
        mutator (fn [s]
                  (store-utils/add-entity! s nil '(:a (1 (2 3)) (3 (4 5))))
                  (store-utils/add-entity! s nil '(:b (1 (2 4)))))]
    ;; elements
    (is (= (let-mutated-store [store s0 mutator]
                              (query-matches '(1 (2)) store)) [{}]))
    (is (= (let-mutated-store [store s0 mutator]
                              (query-matches '(1 (3)) store))) nil)
    (is (= (let-mutated-store [store s0 mutator]
                              (query-matches '(nil (2)) store)) [{}]))
    (is (= (let-mutated-store [store s0 mutator]
                              (query-matches '(nil (1)) store)) [{}]))
    ;; variables as top level entities
    (is (check (set (envs-to-list (let-mutated-store [store s0 mutator]
                                    (query-matches (variable "v") store))))
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
                 (let-mutated-store [store s0 mutator]
                   (query-matches `(:and ((1 ~(variable "v")) :first)
                                          (~(variable "v") :second))
                                   store))))
           #{{"v" '(2 3)} {"v" '(2 4)}}))
    ;; variables inside items
    (is (= (set
            (envs-to-list
             (let-mutated-store [store s0 mutator]
               (query-matches `(nil (1 ~(variable "v"))) store))))
           #{{"v" '(2 4)} {"v" '(2 3)}}))
    (is (= (set (envs-to-list
                 (let-mutated-store [store s0 mutator]
                   (query-matches `(nil (1 (2 ~(variable "v")))) store))))
           #{{"v" 4} {"v" 3}}))
    (is (= (envs-to-list
            (let-mutated-store [store s0 mutator]
              (query-matches `(nil (1 ~(variable "v"))
                                    (3 ~(variable "v")))
                              store)))
           nil)) 
    (is (= (envs-to-list
            (let-mutated-store [store s0 mutator]
              (query-matches `(nil (1 (2 ~(variable "v")))
                                    (~(variable "v")))
                              store)))
           [{"v" 3}]))
    (is (= (envs-to-list
            (let-mutated-store [store s0 mutator]
              (query-matches `(nil (~(variable "v"))
                                    (1 (2 ~(variable "v"))))
                              store)))
           [{"v" 3}]))
    (let [matches (let-mutated-store [store s0 mutator]
                    (query-matches
                     `(:and ((1 (~(variable "v" nil nil true) 3)) :first)
                            ((nil (~(variable "v") 4)) :second))
                     store))]
      (is (= (count matches) 1))
      (is (= (keys (first matches)) ["v"]))
      (is (not= (current-value ((first matches) "v")) 2))
      (is (= (current-value (entity/to-list ((first matches) "v")))) 2))
    (is (= (envs-to-list
            (let-mutated-store [store s0 mutator]
              (query-matches `(:and ((nil (~(variable "v") 4))
                                      :first)
                                     ((1 (~(variable "v" nil nil true) 3))
                                      :second))
                              store)))
           nil))
    ;; and
    (is (= (envs-to-list
            (let-mutated-store [store s0 mutator]
              (query-matches `(:and ((1 (~(variable "v") 3)) :first)
                                     ((nil (~(variable "v") 4)) :second))
                              store)))
           [{"v" 2}]))
    (is (= (let-mutated-store [store s0 mutator]
             (query-matches `(:and ((1 (~(variable "v" nil nil true) 3))
                                     :first)
                                    ((nil (~(variable "v" nil nil true) 4))
                                     :second))
                             store))
           nil))
    (is (= (let-mutated-store [store s0 mutator]
             (query-matches `(:and ((1 (~(variable "v") 3)) :first)
                                    ((nil (~(variable "v") 5)) :second))
                             store))
           nil))
    (is (= (envs-to-list
            (let-mutated-store [store s0 mutator]
              (query-matches `(:and ((~(variable "c") (~(variable "v") 3))
                                      :first)
                                     ((~(variable "c") (~(variable "v") 4))
                                      :second))
                              store)))
           [{"v" 2, "c" 1}]))
    (is (= (let-mutated-store [store s0 mutator]
             (query-matches `(:and ((~(variable "v") (~(variable "c") 3))
                                     :first)
                                    ((~(variable "c") (~(variable "v") 4))
                                     :second))
                             store))
           nil))
    (is (= (set (envs-to-list
                 (let-mutated-store [store s0 mutator]
                   (query-matches `(:and ((nil (~(variable "v"))) :first)
                                          ((nil (nil ~(variable "v"))) :second))
                                   store))))
           #{{"v" 3} {"v" 4} {"v" 5}}))
    ;; exists
    (is (= (let-mutated-store [store s0 mutator]
             (query-matches `(:exists ("v" :variable-name)
                                       ((nil (1 ~(variable "v"))
                                             (3 ~(variable "v")))
                                        :body))
                             store))
           nil)) 
    (is (= (let-mutated-store [store s0 mutator]
             (query-matches `(:exists ("v" :variable-name)
                                       ((nil (1 (2 ~(variable "v")))
                                             (~(variable "v")))
                                        :body))
                             store))
           [{}]))
    (is (= (envs-to-list
            (let-mutated-store [store s0 mutator]
              (query-matches `(:exists ("c" :variable-name)
                                        ((nil (1 (2 ~(variable "v")))
                                              (~(variable "v")))
                                         :body))
                              store)))
           [{"v" 3}]))
    (is (= (let-mutated-store [store s0 mutator]
             (query-matches `(:exists  ("v" :variable-name)
                                        ((1 (~(variable "v") 3)) :qualifier)
                                        ((nil (~(variable "v") 4)) :body))
                             store))
           [{}]))
    (is (= (let-mutated-store [store s0 mutator]
             (query-matches `(:exists  ("v" :variable-name)
                                        ((1 ~(variable "v")) :qualifier)
                                        ((3 ~(variable "v")) :body))
                             store))
           nil))
    ;; forall
    (is (= (let-mutated-store [store s0 mutator]
             (query-matches `(:forall ("v" :variable-name)
                                       ((1 (~(variable "v") 3)) :qualifier)
                                       ((1 (~(variable "v") 4)) :body))
                             store))
           [{}]))
    (is (= (let-mutated-store [store s0 mutator]
             (query-matches `(:forall ("v" :variable-name)
                                       ((1 (~(variable "v") 3)) :qualifier)
                                       ((1 (2 ~(variable "v"))) :body))
                             store))
           nil))
    (is (= (let-mutated-store [store s0 mutator]
             (query-matches `(:forall ("v" :variable-name)
                                       ((1 ~(variable "v")) :qualifier)
                                       (~(variable "v") :body))
                             store))
           [{}]))))

(deftest matching-items-test
  (let [ia (->ItemId "A")
        ib (->ItemId "B")
        s0 (store/new-element-store)
        mutator (fn [s]
                  (store-utils/add-entity! s ia '(1 (2 3)))
                  (store-utils/add-entity! s ia '(3 (4 5)))
                  (store-utils/add-entity! s ib '(1 (2 4))))]
    (let [matches (let-mutated-store
                   [store s0 mutator]
                   (matching-items '(nil (2)) store))]
      (= (map :item-id matches) [ia ib]))
    (let [matches (let-mutated-store
                   [store s0 mutator]
                   (matching-items '(nil (3)) store))]
      (= (map :item-id matches) [ia]))
    (let [matches (let-mutated-store
                   [store s0 mutator]
                   (matching-items '(nil (2 3)) store))]
      (= (map :item-id matches) [ia]))))

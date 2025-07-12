(ns cosheet2.store-utils-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            (cosheet2
             [store :refer :all]
             [store-utils :refer :all]
             [entity :refer [to-list description->entity]]
             entity-impl
             [store-impl :refer :all]
             [task-queue :refer [new-priority-task-queue]]
             [test-utils :refer [check as-set]])
            ; :reload
            ))

(deftest add-entity-test
  (let [[s1 id]
        (add-entity (new-element-store)
                    (make-item-id "0") '(77 ("test" :label)))
        [s2 element-id]
        (add-entity s1 id '("Fred" ("by" :label)))]
    (is (= (id->target s1 id)) (make-item-id "0"))
    (is (= (to-list (description->entity element-id s2))
           '("Fred" ("by" :label))))))

(deftest remove-entity-by-id-test
  (let [[added-store e1]
        (add-entity (new-element-store) (make-item-id "0")
                    '("foo" ("test" :label)))
        [added-store2 e2]
        (add-entity added-store e1 '("Fred" ("by" :label)))
        removed-store (remove-entity-by-id added-store2 e2)]
    (is (check (to-list (description->entity e1 added-store2))
               (as-set '("foo"
                         ("test" :label)
                         ("Fred" ("by" :label))))))
    (is (= (to-list (description->entity e1 removed-store))
           '("foo" ("test" :label))))
    (is (= (assoc removed-store :next-number (:next-number added-store))
           added-store))))


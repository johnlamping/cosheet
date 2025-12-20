(ns cosheet2.store-utils-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            (cosheet2
             [store :refer :all]
             [store-utils :refer :all]
             [entity :refer [to-list id->object id->element
                             make-element-list make-object-list]]
             entity-impl
             [store-impl :refer :all]
             [task-queue :refer [new-priority-task-queue]]
             [test-utils :refer [check as-set]])
            ; :reload
            ))

(deftest add-test
  (let [s (new-element-store)
        [s1 id] (add-element s (make-item-id "0") '(77 ("test" :label)))
        [s2 id1] (add-object s1 (make-object-list '("Hello")))
        [s3 id2] (add-element s2 "Fred" (make-element-list
                                         :target
                                         (id->object id1 s2)
                                         '(("by" :label))))
        [s4 id3] (add-element s3 id `(~(make-object-list '(1)) 3))
        [s5 id4] (add-element s4 id1 `(~(id->object (make-item-id "a") nil)))
        [s id6] (add-element s5 (make-item-id "George")
                             `(1 (~(id->object (make-item-id "name") nil))))]
    (is (= (id->target s id)) (make-item-id "0"))
    (is (= (id->target s id2)) id1)
    (is (= (id->source s id2)) "Fred")
    (is (check (to-list (id->element id s))
               (as-set `(77
                         ("test" :label)
                         (~(make-object-list '(1)) 3)))))
    (is (= (to-list (id->element id2 s))
           '("Fred" ("by" :label))))
    (is (check (to-list (id->object id1 s))
               (as-set (make-object-list
                        `("Hello"
                          ("Fred" ("by" :label))
                          (~(id->object (make-item-id "a") s)))))))
    (is (check (to-list (id->element id3 s))
               `(~(make-object-list '(1)) 3)))
    (is (check (to-list (id->element id6 s))
               `(1 (~(id->object (make-item-id "name") s)))))))

(deftest remove-entity-by-id-test
  (let [[added-store e1]
        (add-element (new-element-store) (make-item-id "0")
                    '("foo" ("test" :label)))
        [added-store2 e2]
        (add-element added-store e1 '("Fred" ("by" :label)))
        removed-store (remove-entity-by-id added-store2 e2)]
    (is (check (to-list (id->element e1 added-store2))
               (as-set '("foo"
                         ("test" :label)
                         ("Fred" ("by" :label))))))
    (is (= (to-list (id->element e1 removed-store))
           '("foo" ("test" :label))))
    (is (= (assoc removed-store :next-number (:next-number added-store))
           added-store))))


(ns cosheet2.store-utils-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            (cosheet2
             [store :refer :all]
             [store-utils :refer :all]
             [entity :refer [to-list id->entity make-object-list]]
             entity-impl
             [store-impl :refer :all]
             [task-queue :refer [new-priority-task-queue]]
             [test-utils :refer [check as-set]])
            ; :reload
            ))

(deftest add-test
  (let [s (new-element-store)
        [s1 id] (add-entity s (make-item-id "0") '(77 ("test" :label)))
        [s2 id1] (add-object s1 (make-object-list '("Hello")))
        [s3 id2] (add-entity s2 "Fred" `((:target ~(id->entity id1 s2))
                                         ("by" :label)))
        [s id3] (add-element s3 id `(~(make-object-list '(1)) 3))]
    (is (= (id->target s id)) (make-item-id "0"))
    (is (= (id->target s id2)) id1)
    (is (= (id->source s id2)) "Fred")
    (is (check (to-list (id->entity id s))
               (as-set `(77
                         ("test" :label)
                         (~(make-object-list '(1)) 3)))))
    (is (= (to-list (id->entity id2 s))
           '("Fred" ("by" :label))))
    (is (check (to-list (id->entity id1 s))
               (as-set (make-object-list
                        '("Hello" ("Fred" ("by" :label)))))))
    (is (check (to-list (id->entity id3 s))
               `(~(make-object-list '(1)) 3)))))

(deftest remove-entity-by-id-test
  (let [[added-store e1]
        (add-entity (new-element-store) (make-item-id "0")
                    '("foo" ("test" :label)))
        [added-store2 e2]
        (add-entity added-store e1 '("Fred" ("by" :label)))
        removed-store (remove-entity-by-id added-store2 e2)]
    (is (check (to-list (id->entity e1 added-store2))
               (as-set '("foo"
                         ("test" :label)
                         ("Fred" ("by" :label))))))
    (is (= (to-list (id->entity e1 removed-store))
           '("foo" ("test" :label))))
    (is (= (assoc removed-store :next-number (:next-number added-store))
           added-store))))


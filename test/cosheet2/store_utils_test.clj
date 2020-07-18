(ns cosheet2.store-utils-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            (cosheet2
             [store :refer :all]
             [store-utils :refer :all]
             entity-impl
             [store-impl :refer :all]
             [task-queue :refer [new-priority-task-queue]]
             [test-utils :refer [check]])
            ; :reload
            ))

(deftest add-entity-test
  (let [[s1 id]
        (add-entity (new-element-store)
                    (make-id "0") '((77 88) ("test" :label)))
        [s2 element-id]
        (add-entity s1 id '("Fred" ("by" :label)))]
    (is (= (id->subject s1 id)) (make-id "0"))
    (comment (is (#{'((77 88) ("test" :label) ("Fred" ("by" :label)))
                    '((77 88) ("Fred" ("by" :label)) ("test" :label))}
                  (to-list (description->entity element added-store2)))))))

(deftest remove-entity-by-id-test
  (let [[added-store e1]
        (add-entity (new-element-store) (make-id "0")
                    '(("foo") ("test" :label)))
        [added-store2 e2]
        (add-entity added-store e1 '("Fred" ("by" :label)))
        removed-store (remove-entity-by-id added-store2 e1)]
    (is (= (assoc removed-store :next-id (:next-id (new-element-store)))
           (new-element-store)))))


(ns cosheet.store-utils-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [cosheet.store :refer :all]
            [cosheet.store-utils :refer :all]
            [cosheet.entity :refer [deep-to-list description->entity]]
            cosheet.entity-impl
            [cosheet.store-impl :refer :all]
            [cosheet.test-utils :refer [check]]
            ; :reload
            ))

(deftest add-entity-test
  (let [[added-store element]
        (add-entity (new-element-store)
                    (make-id "0") '((77 88) ("test" :label)))
        [added-store2 _]
        (add-entity added-store element '("Fred" ("by" :label)))]
    (is (#{'((77 88) ("test" :label) ("Fred" ("by" :label)))
           '((77 88) ("Fred" ("by" :label)) ("test" :label))}
         (deep-to-list (description->entity element added-store2))))))

(deftest remove-entity-by-id-test
  (let [[added-store e1]
        (add-entity (new-element-store) (make-id "0")
                    '(("foo") ("test" :label)))
        [added-store2 e2]
        (add-entity added-store e1 '("Fred" ("by" :label)))
        removed-store (remove-entity-by-id added-store2 e1)]
    (is (= (assoc removed-store :next-id (:next-id (new-element-store)))
           (new-element-store)))))

(deftest add-entity!-test
  (let [[added-store element]
        (add-entity (new-element-store)
                    (make-id "0") '((77 88) ("test" :label)))
        [added-store2 _]
        (add-entity added-store element '("Fred" ("by" :label)))
        mutable (new-mutable-store (new-element-store))
        m-element (add-entity! mutable
                               (make-id "0") '((77 88) ("test" :label)))
        _ (add-entity! mutable m-element '("Fred" ("by" :label)))]
    (is (= (track-modified-ids added-store2) (current-store mutable)))))

(deftest remove-entity-by-id!-test
  (let [[added-store e1]
        (add-entity (new-element-store) (make-id "0")
                    '(("foo") ("test" :label)))
        [added-store2 e2]
        (add-entity added-store e1 '("Fred" ("by" :label)))
        mutable (new-mutable-store added-store2)]
    (remove-entity-by-id! mutable e1)
    (is (= (assoc (current-store mutable) :next-id
                  (:next-id (new-element-store)))
           (track-modified-ids (new-element-store))))))

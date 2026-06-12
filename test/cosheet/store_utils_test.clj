(ns cosheet.store-utils-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            (cosheet
             [store :refer :all]
             [store-utils :refer :all]
             [entity :refer [to-list id->object id->element
                             make-element-list make-object-list
                             elements forward-elements in-different-store
                             name-label link-type object-type]]
             entity-impl
             query-impl
             [store-impl :refer :all]
             [task-queue :refer [make-priority-task-queue]]
             [canonical :refer [canonicalize]]
             [test-utils :refer [check as-set]])
            ; :reload
            ))

(deftest add-test
  (let [s (add-universal-objects (new-element-store))
        [s1 id] (add-element s (make-item-id "0")
                             `(77 (~(link-type-object "test"))))
        [s2 id1] (add-object s1 (make-object-list '("Hello")))
        ;; A reversed link. "Fred" is the source.
        [s3 id2] (add-element s2 "Fred" (make-element-list
                                         :target
                                         (id->object id1 s2)
                                         `((~(link-type-object "by")))))
        [s4 id3] (add-element s3 id `(~(make-object-list '(1)) 3))
        [s5 id4] (add-element s4 id1 `(~(id->object (make-item-id "a") nil)))
        [s6 id6] (add-object s5
                             (make-object-list
                              [`(1 (~(id->object (make-item-id "name") nil)))
                               2]))
        [s id7] (add-object s6
                            (make-object-list
                             [`(1
                                (~(id->object (make-item-id "name") nil)))]))
        test-label (find-object-by-name s "test" (link-type-object ""))
        by-label (find-object-by-name s "by" (link-type-object ""))]
    (is (= (id->target s id)) (make-item-id "0"))
    (is (= (id->target s id2)) id1)
    (is (= (id->source s id2)) "Fred")
    (is (= id6 id7)) ; check that we found the existing object.
    (is (check (to-list (id->element id s))
               (as-set `(77
                         (~test-label)
                         (~(make-object-list '(1)) 3)))))
    (is (= (to-list (id->element id2 s))
           `("Fred" (~by-label))))
    (is (check (to-list (id->object id1 s))
               (as-set (make-object-list
                        `("Hello"
                          ("Fred" (~by-label))
                          (~(id->object (make-item-id "a") s)))))))
    (is (check (to-list (id->element id3 s))
               `(~(make-object-list '(1)) 3)))
    (is (= id7 id6))
    (is (check (map to-list (elements (id->object id6 s)))
               (as-set [`(1 (~(id->object (make-item-id "name") s)))
                         2])))))

(deftest remove-entity-by-id-test
  (let [;; Pre-store the link-type-objects so they exist in added-store
        ;; as well as added-store2, which lets the final equality
        ;; check between added-store and removed-store hold.
        s0 (-> (new-element-store)
               add-universal-objects
               (add-link-type-object "test") first
               (add-link-type-object "by") first)
        test-label (find-object-by-name s0 "test" (link-type-object ""))
        by-label (find-object-by-name s0 "by" (link-type-object ""))
        [added-store e1]
        (add-element s0 (make-item-id "0")
                    `("foo" (~test-label)))
        [added-store2 e2]
        (add-element added-store e1 `(~(make-object-list '("Fred" 1))
                                      (~by-label)))
        removed-store (remove-entity-by-id added-store2 e2)]
    (println (canonicalize (id->element e1 added-store2)))
    (is (check (canonicalize (id->element e1 added-store2))
               (canonicalize `("foo"
                               (~test-label)
                               (~(make-object-list '("Fred" 1))
                                (~by-label))))))
    (is (= (canonicalize (id->element e1 removed-store))
           (canonicalize `("foo" (~test-label)))))
    (is (= (assoc removed-store :next-number (:next-number added-store))
           added-store))))

(deftest add-universal-objects-test
  (let [s (add-universal-objects (new-element-store))
        name-label-in-s (in-different-store name-label s)
        link-type-in-s (in-different-store link-type s)
        object-type-in-s (in-different-store object-type s)]
    (is (check
         (map to-list (forward-elements name-label-in-s))
         (as-set [`(~link-type-in-s)
                  `("name" (~name-label-in-s))])))
    (is (check
         (map to-list (forward-elements link-type-in-s))
         [`(~object-type-in-s)]))
    (is (check
         (map to-list (forward-elements object-type-in-s))
         [`(~object-type-in-s)]))))

(deftest add-object-type-object-test
  (let [s0 (add-universal-objects (new-element-store))
        [store oid] (add-object-type-object s0 "foo")
        obj (in-different-store (id->object oid store) store)
        name-label-in-s (in-different-store name-label store)
        object-type-in-s (in-different-store object-type store)]
    (is (check
         (map to-list (forward-elements obj))
         (as-set [`("foo" (~name-label-in-s))
                  `(~object-type-in-s)])))))

(deftest add-link-type-object-test
  (let [s0 (add-universal-objects (new-element-store))
        [store oid] (add-link-type-object s0 "foo")
        obj (in-different-store (id->object oid store) store)
        name-label-in-s (in-different-store name-label store)
        link-type-in-s (in-different-store link-type store)]
    (is (check
         (map to-list (forward-elements obj))
         (as-set [`("foo" (~name-label-in-s))
                  `(~link-type-in-s)])))))

(deftest add-object-with-id-only-template-test
  (let [s0 (add-universal-objects (new-element-store))
        [store foo-oid] (add-link-type-object s0 "foo")
        id-only-template (id->object foo-oid nil)
        [new-store new-id] (add-object store id-only-template)]
    ;; The store is unchanged and the id matches.
    (is (= new-store store))
    (is (= new-id foo-oid))
    ;; If the store doesn't have the object, the assertion fails.
    (is (thrown? AssertionError
                 (add-object s0 (id->object foo-oid nil))))))


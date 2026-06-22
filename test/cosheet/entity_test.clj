(ns cosheet.entity-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet [orderable :as orderable]
                      [reporter :refer [reporter-valid? set-attendee!
                                        reporter-value-or-invalid]]
                      [reporter-macros :refer [let-R]]
                      [store :refer [add-link make-item-id get-new-object-id
                                     new-element-store new-mutable-store
                                     track-modified-ids
                                     current-store
                                     update-source store-update!
                                     name-label-id link-type-id]]
                      store-impl
                      mutable-store-impl
                      [entity :refer :all]
                      [entity-impl :as entity-impl]
                      [calculator :refer [current-value make-calculator-data
                                          propagate-calculator-data!]]
                      [canonical :refer [canonicalize]]
                      [task-queue :refer [make-priority-task-queue
                                          run-all-pending-tasks]]
                      [test-utils :refer [check any as-set differences]])
            ; :reload
            ))

;;; This tests the internal function of entity-impl
(deftest entity<->endpoint-test
  (let [[s id] (add-link (new-element-store) (make-item-id "a") "B")
        item (id->element id s)
        reversed-item (id->element id :target s)]
    (is (= (entity-impl/endpoint->entity 2 s) 2))
    (is (= (entity-impl/endpoint->entity :foo s) :foo))
    (is (= (entity-impl/endpoint->entity 'foo s) 'foo))
    (is (= (entity-impl/endpoint->entity orderable/initial s)
           orderable/initial))
    (is (= (entity-impl/endpoint->entity "1" s) "1"))
    (is (= (entity-impl/endpoint->entity id s) item))
    (is (= (entity-impl/endpoint->entity id s :target) reversed-item))))

(deftest id-only-item-test
  (let [[sa id0] (get-new-object-id (new-element-store))
        [_ id1] (get-new-object-id sa)
        id99 (make-item-id "99")
        item0 (id->object id0 nil)
        item1 (id->object id1 nil)
        item99 (id->object id99 nil)]
    (is (= (:item-id  item0) id0))
    (is (= (:item-id  item99) id99))
    (is (not (primitive? item0)))
    (is (object? item0))
    (is (not (element? item0)))
    (is (not (id-identified-object? item0)))
    (is (id-identified-object? item99))
    (is (not (interned-object? item0)))
    (is (= (orientation item0)) nil)
    (is (= (target-entity item0) nil))
    (is (= (originating-entity item0) nil))
    (is (= (containing-elements item0) nil))
    (is (= (label->elements item99 "foo") nil))
    (is (= (elements item99) nil))
    (is (= (forward-elements item99) nil))
    (is (= (content item99) nil))
    (is (= (content->elements item99 4) nil))
    (is (= (label->elements item99 "foo") nil))
    (is (= (entity-key item99) id99))
    (is (= (to-tree item0) item0))
    (is (= (to-tree item99) item99))
    (is (universal-object? (id->object (make-item-id "name") nil)))
    (is (universal-object? (id->object (make-item-id "link-type") nil)))
    (is (universal-object? (id->object (make-item-id "object-type") nil)))
    (is (not (universal-object?
              (id->object (make-item-id "something else") nil))))))

(deftest storeditem-test
  (let [[sa id0] (get-new-object-id (new-element-store))
        id1 (make-item-id "1")
        [sc id99] (get-new-object-id sa)
        ;; Create foo-oid and rel-oid as objects with names. The
        ;; link-type markers that make them label-objects are added
        ;; later.
        [sd foo-oid] (get-new-object-id sc)
        [sd1 foo-name-link] (add-link sd foo-oid "foo")
        [sd2 _] (add-link sd1 foo-name-link name-label-id)
        [sd3 rel-oid] (get-new-object-id sd2)
        [sd4 rel-name-link] (add-link sd3 rel-oid "relationship")
        [sd5 _] (add-link sd4 rel-name-link name-label-id)
        ;; Continue with the main structure, using foo-oid and rel-oid
        ;; as the sources where labels are wanted.
        [s1 ida] (add-link sd5 id99 3)
        [s2 idb] (add-link s1 ida foo-oid)
        [s3 idc] (add-link s2 id99 4)
        [s4 idd] (add-link s3 idc "bar")
        [s5 ide] (add-link s4 id99 "baz")
        [s6 idg] (add-link s5 ide "bletch")
        ;; Add link-type marker to foo-oid; this makes it a label-object
        ;; and therefore makes idb a label-link.
        [s7 idh] (add-link s6 foo-oid link-type-id)
        [s8 idi] (add-link s7 id99 "Joe")
        [s9 idj] (add-link s8 idi name-label-id)
        [s11 idl] (add-link s9 id0 "irrelevant")
        [s12 idm] (add-link s11 id0 id1)
        [s13 idn] (add-link s12 idm rel-oid)
        ;; Add link-type marker to rel-oid; this makes idn a label-link.
        [s ido] (add-link s13 rel-oid link-type-id)
        item0 (id->object id0 s)
        item1 (id->object id1 s)
        item99 (id->object id99 s)
        foo-label (id->object foo-oid s)
        rel-label (id->object rel-oid s)
        item-a (id->element ida s)
        item-b (id->element idb s)
        item-m (id->element idm s)
        item-a-reversed (id->element ida :target s)
        item-b-reversed (id->element idb :target s)]
    (is (= (:item-id  item0) id0))
    (is (= (:item-id  item1) id1))
    (is (not (primitive? item0)))
    (is (object? item0))
    (is (not (object? item-b)))
    (is (not (id-identified-object? item0)))
    (is (id-identified-object? item1))
    (is (interned-object? item99))
    (is (interned-object? (id->object (make-item-id "foo") s)))
    (is (not (interned-object? item-b)))
    (is (not (interned-object? item0)))
    (is (not (interned-object? "foo")))
    (is (= (orientation item0)) nil)
    (is (= (orientation item-b)) :source)
    (is (= (orientation item-a-reversed)) :target)
    (is (= (target-entity (id->element ida s)) item99))
    (is (= (target-entity item-a-reversed) item99))
    (is (= (target-entity item-b-reversed) (id->element ida s)))
    ;; For an element with :source orientation, originating-entity
    ;; matches target-entity (the opposite of content is the target).
    (is (= (originating-entity (id->element ida s)) item99))
    ;; For an element with :target orientation, originating-entity
    ;; returns the source endpoint instead. ida's source is the
    ;; primitive 3.
    (is (= (originating-entity item-a-reversed) 3))
    ;; idb's source is foo-oid, so its reversed element's
    ;; originating-entity is foo-label.
    (is (= (originating-entity item-b-reversed) foo-label))
    (is (= (label->elements item99 foo-label) [(id->element ida s)]))
    (is (= (containing-elements item1) [(id->element idm s)]))
    (is (= (containing-elements item0) []))
    ;; Check that the link-type marker on foo-oid is required: at s5
    ;; that marker has not been added, so idb is not yet a label.
    (is (= (label->elements (id->object id99 s5) foo-label) nil))
    ;; Check that we pick up relationships both forwards and backwards.
    (is (= (label->elements item0 rel-label)
           [(id->element idm s)]))
    (is (= (label->elements item1 rel-label)
           [(id->element idm :target s)]))
    (is (= (set (elements item99))
           #{(id->element ida s)
             (id->element idc s)
             (id->element ide s)
             (id->element idi s)}))
    (is (= (set (forward-elements item99))
           #{(id->element ida s)
             (id->element idc s)
             (id->element ide s)
             (id->element idi s)}))
    (is (= (elements (id->element idd s))
           nil))
    (is (= (forward-elements (id->element idd s))
           nil))
    (is (= (content item99) nil))
    (is (label-element? item-b))
    (is (not (label-element? item99)))
    (is (= (content (id->element ida s)) 3))
    (is (= (content item-b) foo-label))
    (is (= (content item-a-reversed) item99))
    (is (= (content->elements item99 4) [(id->element idc s)]))
    (is (= (content->elements item-a foo-label) [(id->element idb s)]))
    (is (= (content->elements item0 item1) [(id->element idm s)]))
    (is (= (content->elements item1 item0) [(id->element idm :target s)]))
    (is (= (label->element item99 foo-label) (id->element ida s)))
    (is (= (label->element item99 "bletch") nil))
    (is (= (label->content item99 foo-label) 3))
    (is (= (label->content item99 "bletch") nil))
    (let [[sx idx] (add-link s id99 7)
          [sy idy] (add-link sx idx foo-oid)]
      (is (thrown? java.lang.AssertionError
                   (label->element (id->object id99 sy) foo-label)))
      (is (thrown? java.lang.AssertionError
                   (label->content (id->object id99 sy) foo-label))))
    (is (= (content (id->element idc s)) 4))
    (is (= (content item-a-reversed) item99))
    (is (= (content item-b-reversed) item-a-reversed))
    (is (= (entity-key item-a) ida))
    (is (= (entity-key item-a-reversed) ida))
    (is (= (to-tree item99) item99))
    (is (check (elements item0)
               (as-set [(id->element idl s)
                        (id->element idm s)])))
    (is (check (forward-elements item0)
               (as-set [(id->element idl s)
                        (id->element idm s)])))
    (is (check (elements item1)
               (as-set [(id->element idm :target s)])))
    (is (= (forward-elements item1)
           nil))
    (is (check (to-tree item1) item1))
    (is (= (to-tree item-a-reversed) `((:target ~item99) (~foo-label))))
    (is (= (to-tree item-m) (make-tree-element :source item1
                                               `((~rel-label)))))
    (is (check (to-tree item0)
               (as-set (make-tree-object
                        `(~(make-tree-element :source item1
                                              `((~rel-label)))
                          "irrelevant")))))))

(deftest mutable-storeditem-test
  (let [[sa id0] (get-new-object-id (new-element-store))
        id1 (make-item-id "1")
        [sc id99] (get-new-object-id sa)
        ;; Create foo-oid and rel-oid as objects with names. The
        ;; link-type markers that make them label-objects are added
        ;; later.
        [sd foo-oid] (get-new-object-id sc)
        [sd1 foo-name-link] (add-link sd foo-oid "foo")
        [sd2 _] (add-link sd1 foo-name-link name-label-id)
        [sd3 rel-oid] (get-new-object-id sd2)
        [sd4 rel-name-link] (add-link sd3 rel-oid "relationship")
        [sd5 _] (add-link sd4 rel-name-link name-label-id)
        [s1 ida] (add-link sd5 id99 3)
        [s2 idb] (add-link s1 ida foo-oid)
        [s3 idc] (add-link s2 id99 4)
        [s4 idd] (add-link s3 idc "bar")
        [s5 ide] (add-link s4 id99 "baz")
        [s6 idg] (add-link s5 ide "bletch")
        [s7 idh] (add-link s6 foo-oid link-type-id)
        [s8 idi] (add-link s7 id99 "Joe")
        [s9 idj] (add-link s8 idi name-label-id)
        [s11 idl] (add-link s9 id0 "irrelevant")
        [s12 idm] (add-link s11 id0 id1)
        [s13 idn] (add-link s12 idm rel-oid)
        [s ido] (add-link s13 rel-oid link-type-id)
        queue (make-priority-task-queue 0)
        cd (make-calculator-data queue)
        ms (new-mutable-store s)
        item0 (id->object id0 ms)
        item1 (id->object id1 ms)
        item99 (id->object id99 ms)
        foo-label (id->object foo-oid ms)
        rel-label (id->object rel-oid ms)
        item-a (id->element ida ms)
        item-b (id->element idb ms)
        item-a-reversed (id->element ida :target ms)
        item-b-reversed (id->element idb :target ms)]
    (is (= (:item-id  item0) id0))
    (is (= (:item-id  item1) id1))
    (is (not (element? item0)))
    (is (element? item-b))
    (is (object? item0))
    (is (not (object? item-b)))
    (is (= (orientation item0)) nil)
    (is (= (orientation item-b)) :source)
    (is (= (orientation item-a-reversed)) :target)
    (is (= (current-value (target-entity (id->element ida ms))) item99))
    (is (= (current-value (target-entity item-a-reversed)) item99))
    (is (= (current-value (target-entity item-b-reversed))
           (id->element ida ms)))
    (is (= (current-value (originating-entity (id->element ida ms)))
           item99))
    (is (= (current-value (originating-entity item-a-reversed)) 3))
    (is (= (current-value (originating-entity item-b-reversed))
           (id->object foo-oid ms)))
    (is (= (current-value (containing-elements item1)) [(id->element idm ms)]))
    (is (= (current-value (containing-elements item0)) []))
    (is (= (primitive? item0) false))
    (is (= (current-value (label->elements item99 foo-label))
           [(id->element ida ms)]))
    (is (= (current-value (label->elements item99 "bar")) nil))
    ;; Check that we pick up relationships both forwards and backwards.
    (is (= (current-value (label->elements item0 rel-label))
           [(id->element idm ms)]))
    (is (= (current-value (label->elements item1 rel-label))
           [(id->element idm :target ms)]))
    (is (= (set (current-value (elements item99)))
           #{(id->element ida ms)
             (id->element idc ms)
             (id->element ide ms)
             (id->element idi ms)}))
    (is (= (set (current-value (forward-elements item99)))
           #{(id->element ida ms)
             (id->element idc ms)
             (id->element ide ms)
             (id->element idi ms)}))
    (is (= (current-value (elements (id->element idd ms)))
           nil))
    (is (= (current-value (forward-elements (id->element idd ms)))
           nil))
    (is (= (current-value (content item99)) nil))
    (is (= (current-value (content item-a-reversed)) item99))
    (is (= (current-value (content item-b-reversed)) item-a-reversed))
    (is (= (current-value (content->elements item99 4))
           [(id->element idc ms)]))
    (is (= (current-value (content->elements item-a foo-label))
           [(id->element idb ms)]))
    (is (= (current-value (content->elements item0 item1))
           [(id->element idm ms)]))
    (is (= (current-value (content->elements item1 item0))
           [(id->element idm :target ms)]))
    (is (= (current-value (content (id->element ida ms))) 3))
    (is (= (current-value (content item-a-reversed)) item99))
    (is (current-value
         [content (current-value (content (id->element idc ms)))])
        4)
    (is (= (current-value (content->elements item99 4))
           [(id->element idc ms)]))
    (is (= (current-value (label->content item99 foo-label)) 3))
    (is (= (current-value (label->content item99 "bletch")) nil))
    (is (= (current-value (content (id->element idc ms))) 4))
    (is (= (entity-key item-a) ida))
    (is (= (entity-key item-a-reversed) ida))
    (is (check (current-value (elements item0))
               (as-set [(id->element idl ms)
                        (id->element idm ms)])))
    (is (check (current-value (forward-elements item0))
               (as-set [(id->element idl ms)
                        (id->element idm ms)])))
    (is (check (current-value (elements item1))
               (as-set [(id->element idm :target ms)])))
    (is (= (current-value (forward-elements item1))
           nil))
    ;; Now make sure updating-immutable tracks right.
    (let [record-of-updates (atom [])
          updating-immutable-result (let-R [current-item (updating-immutable
                                                        item99)] 
                                      (is (not (mutable-entity? current-item)))
                                      (let [value (to-tree current-item)]
                                        (swap! record-of-updates
                                               #(conj % value))
                                        value))
          reporter-99 (id->updating-entity-R id99 ms)]
      (is (= @record-of-updates []))
      ;; See if it gets computed when demand is added.
      (propagate-calculator-data! updating-immutable-result cd)
      (propagate-calculator-data! reporter-99 cd)
      (run-all-pending-tasks queue)
      (is (not (reporter-valid? updating-immutable-result)))
      (set-attendee! updating-immutable-result :a 0 (fn [& _] nil))
      (set-attendee! reporter-99 :a 0 (fn [& _] nil))
      (is (not (reporter-valid? updating-immutable-result)))
      (run-all-pending-tasks queue)
      (let [orig-99 (to-tree (in-different-store item99 (current-store ms)))]
        (is (check (canonicalize
                    (reporter-value-or-invalid updating-immutable-result))
                   (canonicalize orig-99)))
        (is (check (map canonicalize @record-of-updates)
                   [(canonicalize orig-99)]))
        ;; Make sure it is not recomputed when an irrelevant change is made.
        (store-update! ms (fn [s] (update-source s idj 44)))
        (is (check (map canonicalize @record-of-updates)
                   [(canonicalize orig-99)]))
        ;; Make sure it is recomputed when a deep, but relevant, change is made.
        (store-update! ms (fn [s] (update-source s idd "bletch")))
        (run-all-pending-tasks queue)
        (is (check (canonicalize
                    (reporter-value-or-invalid updating-immutable-result))
                   (canonicalize
                    (to-tree (in-different-store item99 (current-store ms))))))
        (is (check (canonicalize
                    (to-tree (reporter-value-or-invalid reporter-99)))
                   (canonicalize
                    (to-tree (in-different-store item99 (current-store ms))))))
        (is (check (map canonicalize @record-of-updates)
                   [(canonicalize orig-99)
                    (canonicalize
                     (to-tree (in-different-store item99
                                                  (current-store ms))))]))))))

(deftest list-test
  (is (not (primitive? '(1 2))))
  (is (element? '(1 2)))
  (is (not (object? '(1 2))))
  (is (not (id-identified-object? '(1 2))))
  (is (not (interned-object? '(1 2))))
  (is (= (orientation '(1 2)) :source))
  (is (= (orientation '((:target 1) 2)) :target))
  (is (= (elements '(1 2)) [2]))
  (is (= (forward-elements '(1 2)) [2]))
  (is (= (elements '(1 (2 3) (4 5))) '[(2 3) (4 5)]))
  (is (= (forward-elements '(1 (2 3) (4 5))) '[(2 3) (4 5)]))
  (is (= (label->elements '(1 (2 :foo) (4 3)) :foo)
         '[(2 :foo)]))
  (is (= (content '(1 (2 3) (4 5))) 1))
  (is (= (content '((:source 1) (2 3) (4 5))) 1))
  (is (= (content '((:target 1) (2 3) (4 5))) 1))
  (is (= (content->elements '(1 (2 3) (4 5)) 4) ['(4 5)]))
  (is (=(entity-key '(1 2)) '(1 2)))
  (is (= (to-tree '(nil (1 nil))) '(nil (1 nil)))))

(deftest vector-test
  (is (not (primitive? [:object 1 2])))
  (is (not (element? [:object 1 2])))
  (is (object? [:object 1 2]))
  (is (not (object? [1 2])))
  (is (= (elements [:object 1 2]) [1 2]))
  (is (= (forward-elements [:object 1 2]) [1 2]))
  (is (= (elements '[:object (2 3) (4 5)]) '[(2 3) (4 5)]))
  (is (= (forward-elements '[:object (2 3) (4 5)]) '[(2 3) (4 5)]))
  (is (= (label->elements '[:object (2 :foo) (4 3)] :foo)
         '[(2 :foo)]))
  (is (= (content [:object 1 2]) nil))
  (is (= (content->elements '[:object (2 3) (4 5)] 4) '[(4 5)]))
  (is (= (entity-key [:object 1 2]) [:object 1 2]))
  (is (= (to-tree [:object 1 2]) [:object 1 2]))
  ;; More extensive tests of uniquely-identified-object?
  (let [name-label `(~(id->object (make-item-id "name") nil))]
    (is (not (uniquely-identified-object? [:object])))
    (is (not (uniquely-identified-object? [:object `(nil ~name-label)])))
    (is (not (uniquely-identified-object? [:object `("" ~name-label)])))
    (is (not (uniquely-identified-object? [:object `(~'anything ~name-label)])))
    (is (uniquely-identified-object? [:object `("Joe" ~name-label)]))))

(deftest shareable-vector-test
  ;; A shareable tree-object behaves like an object for the Entity
  ;; protocol, but its entity-key is just [:shareable-object id] (the
  ;; elements don't participate), so two references with the same id
  ;; but different elements are recognized as the same object.
  (let [tid1 (make-tree-id 1)
        tid2 (make-tree-id 2)
        obj (make-shareable-tree-object tid1 [1 2])]
    (is (= obj [:shareable-object tid1 1 2]))
    (is (shareable-tree-object? obj))
    (is (not (shareable-tree-object? [:object 1 2])))
    (is (not (primitive? obj)))
    (is (not (element? obj)))
    (is (object? obj))
    (is (= (elements obj) [1 2]))
    (is (= (forward-elements obj) [1 2]))
    (is (= (content obj) nil))
    (is (= (orientation obj) nil))
    (is (= (entity-key obj) tid1))
    (is (= (entity-key (make-shareable-tree-object tid1 [3 4]))
           (entity-key obj)))
    (is (not= (entity-key (make-shareable-tree-object tid2 [1 2]))
              (entity-key obj))))
  (let [obj (make-shareable-tree-object (make-tree-id 1)
                                        '[(2 :foo) (4 3)])]
    (is (= (label->elements obj :foo) '[(2 :foo)]))
    (is (= (content->elements obj 4) '[(4 3)]))))

(deftest shareable-uninterned-object?-test
  ;; Shareable tree-objects are uninterned.
  (is (shareable-uninterned-object?
       (make-shareable-tree-object (make-tree-id 1) [])))
  ;; Plain primitives and non-shareable tree-objects are not.
  (is (not (shareable-uninterned-object? 1)))
  (is (not (shareable-uninterned-object? "foo")))
  (is (not (shareable-uninterned-object? [:object 1 2])))
  ;; Stored anonymous objects (have a store and no name) are uninterned.
  (let [[s id] (get-new-object-id (new-element-store))
        stored-anon (id->object id s)
        stored-no-store (id->object id nil)]
    (is (shareable-uninterned-object? stored-anon))
    ;; A stored entity with no :store is presumed-interned and thus
    ;; not shareable-uninterned.
    (is (not (shareable-uninterned-object? stored-no-store)))
    ;; An interned object (e.g., the name-label one) is not
    ;; shareable-uninterned either.
    (is (not (shareable-uninterned-object?
              (id->object (make-item-id "name") s))))))

(deftest constant-test
  (is (primitive? 1))
  (is (primitive? true))
  (is (primitive? "foo"))
  (is (primitive? :foo))
  (is (primitive? 'foo))
  (is (primitive? nil))
  (is (primitive? orderable/initial))
  (is (not (element? 1)))
  (is (not (element? true)))
  (is (not (element? "foo")))
  (is (not (element? :foo)))
  (is (not (element? 'foo)))
  (is (not (element? nil)))
  (is (not (element? orderable/initial)))
  (is (not (object? 1)))
  (is (not (object? true)))
  (is (not (object? "foo")))
  (is (not (object? :foo)))
  (is (not (object? 'foo)))
  (is (not (object? nil)))
  (is (not (object? orderable/initial)))
  (is (= (orientation 1) :source))
  (is (= (orientation true) :source))
  (is (= (orientation "foo") :source))
  (is (= (orientation :foo) :source))
  (is (= (orientation 'foo) :source))
  (is (= (orientation nil) :source))
  (is (= (orientation orderable/initial) :source))
  (is (= (content 1) 1))
  (is (= (content true) true))
  (is (= (content "foo") "foo"))
  (is (= (content :foo) :foo))
  (is (= (content 'foo) 'foo))
  (is (= (content nil) nil))
  (is (= (content orderable/initial) orderable/initial))
  (is (= (elements 1) nil))
  (is (= (elements true) nil))
  (is (= (elements "foo") nil))
  (is (= (elements :foo) nil))
  (is (= (elements 'foo) nil))
  (is (= (elements nil) nil))
  (is (= (elements orderable/initial) nil))
  (is (= (forward-elements 1) nil))
  (is (= (forward-elements true) nil))
  (is (= (forward-elements "foo") nil))
  (is (= (forward-elements :foo) nil))
  (is (= (forward-elements 'foo) nil))
  (is (= (forward-elements nil) nil))
  (is (= (forward-elements orderable/initial) nil))
  (is (= (content->elements 1 1) nil))
  (is (= (content->elements true 1) nil))
  (is (= (content->elements "foo" 1) nil))
  (is (= (content->elements :foo 1) nil))
  (is (= (content->elements 'foo 1) nil))
  (is (= (content->elements nil 1) nil))
  (is (= (content->elements orderable/initial 1) nil))
  (is (= (label->elements 1 1) nil))
  (is (= (label->elements true 1) nil))
  (is (= (label->elements "foo" 1) nil))
  (is (= (label->elements :foo 1) nil))
  (is (= (label->elements 'foo 1) nil))
  (is (= (label->elements nil 1) nil))
  (is (= (label->elements orderable/initial 1) nil))
  (is (= (entity-key 1) 1))
  (is (= (entity-key true) true))
  (is (= (entity-key "foo") "foo"))
  (is (= (entity-key :foo) :foo))
  (is (= (entity-key 'foo) 'foo))
  (is (= (entity-key nil) nil))
  (is (= (entity-key orderable/initial) orderable/initial))
  (is (to-tree 3) 3))

(deftest add-elements-to-entity-test
  (is (= (add-elements-to-entity '(1 2 (3 4)) '())
         '(1 2 (3 4))))
  (is (= (add-elements-to-entity '(1 2 (3 4)) '(5 (6 7)))
         '(1 2 (3 4) 5 (6 7))))
  (is (= (add-elements-to-entity (make-tree-element :target 1 '(2 (3 4)))
                                 '(5 (6 7)))
         (make-tree-element :target 1 '(2 (3 4) 5 (6 7)))))
  (is (= (add-elements-to-entity 1 '(5 (6 7)))
         '(1 5 (6 7))))
  (is (= (add-elements-to-entity (make-tree-object '(1 2 (3 4))) '(5 (6 7)))
         (make-tree-object '(1 2 (3 4) 5 (6 7))))))

(deftest map-subparts-test
  (let [incrementer #(if (number? %) (inc %) %)]
    (is (= (map-subparts incrementer '(1 5 (2 3) 4))
           '(2 6 (2 3) 5)))
    (is (= (map-subparts incrementer (id->object (make-item-id "foo") nil))
           (id->object (make-item-id "foo") nil)))
    (is (= (map-subparts incrementer (make-tree-object '(5 (2 3) 4)))
           (make-tree-object '(6 (2 3) 5))))))

(deftest post-walk-entity-test
  (is (check (post-walk-entity
              #(cond (number? %) (inc %)
                     (interned-object? %) (id->object (make-item-id "bar") nil)
                     (= % '(3 4)) nil
                     :else %)
              `(1 (2 3)
                  (~(make-tree-object
                       `(4 (5 6))))
                  (~(id->object (make-item-id "foo") nil))))
             `(2 (~(make-tree-object
                    `(5 (6 7))))
                 (~(id->object (make-item-id "bar") nil))))))

(deftest pre-walk-entity-test
  (is (check (pre-walk-entity
              #(cond (number? %) (inc %)
                     (interned-object? %) (id->object (make-item-id "bar") nil)
                     (= % '(2 3)) nil
                     :else %)
              `(1 (2 3)
                  (~(make-tree-object
                     `(4 (5 6))))
                  (~(id->object (make-item-id "foo") nil))))
             `(2 (~(make-tree-object
                    `(5 (6 7))))
                 (~(id->object (make-item-id "bar") nil))))))

(deftest threaded-traversal-test
  (let [;; Trivial pre/post: leave the entity unchanged, but use the
        ;; caller-data as a counter or accumulator.
        counter-pre (fn [_ e _ cd] [e (inc cd)])
        collector-pre (fn [_ e _ cd] [e (conj cd e)])
        ;; Run a traversal with seen-tracking pre/post wrappers and
        ;; the caller-data shape they expect. Return [result
        ;; final-user-data].
        run (fn [entity user-pre user-post initial-user]
              (let [[r cd]
                    (threaded-traversal
                     entity
                     (wrap-pre-fn-with-loop-avoidance user-pre)
                     (wrap-post-fn-with-loop-avoidance user-post)
                     (wrap-caller-data-with-loop-avoidance-data
                      entity initial-user))]
                [r (extract-caller-data-from-loop-avoidance-data cd)]))]
    ;; Primitive sub-elements are wrapped as one-element lists before
    ;; descent, so each primitive sub-element generates two pre-fn
    ;; calls: one for the wrapped element, one for its content.
    ;; Entities visited: outer, content 1, sub (2), its content 2,
    ;; sub (3 4), its content 3, its sub (4), its content 4 — 8 total.
    (let [[result count] (run '(1 2 (3 4)) counter-pre identity-post-fn 0)]
      (is (= result '(1 2 (3 4))))
      (is (= count 8)))
    ;; Pre-fn order is depth-first: outer, then content, then each
    ;; sub-element (descending into each before moving on); primitive
    ;; sub-elements appear once as the wrapped list and once as the
    ;; primitive content.
    (let [[_ visited] (run '(1 2 (3 4)) collector-pre identity-post-fn [])]
      (is (= visited ['(1 2 (3 4)) 1 '(2) 2 '(3 4) 3 '(4) 4])))
    ;; Pre-fn can transform entities (here, double every number).
    (let [doubler (fn [_ e _ cd] [(if (number? e) (* 2 e) e) cd])
          [result _] (run '(1 2 (3 4)) doubler identity-post-fn nil)]
      (is (= result '(2 4 (6 8)))))
    ;; Pre-fn returning :entity/omit for an element drops it from
    ;; the result and skips traversal into its content and elements.
    (let [drop-5 (fn [_ e _ cd]
                   [(if (and (element? e) (= (content e) 5))
                      :entity/omit
                      e)
                    cd])
          [result _] (run '(1 2 (5 6) (3 4)) drop-5 identity-post-fn nil)]
      (is (= result '(1 2 (3 4)))))
    ;; Post-fn returning :entity/omit for an element also drops it.
    (let [drop-5-post (fn [_ e _ cd]
                        [(if (and (element? e) (= (content e) 5))
                           :entity/omit
                           e)
                         cd])
          [result _] (run '(1 2 (5 6) (3 4)) identity-pre-fn drop-5-post nil)]
      (is (= result '(1 2 (3 4)))))
    ;; When the same shareable object appears as both the content of
    ;; the outer entity and the content of one of its sub-elements,
    ;; the second occurrence is dropped at the element level (because
    ;; the outer entity isn't shareable, the second appearance reaches
    ;; the object via traverse, where it short-circuits to a bare
    ;; reference). Because every first encounter of a non-presumed-
    ;; interned object now gets a tree-id, the first occurrence is
    ;; assembled as a shareable-tree-object too, sharing that id.
    (let [obj (make-shareable-tree-object (make-tree-id 1) [])
          structure `(~obj (~obj))
          [result count] (run structure counter-pre identity-post-fn 0)]
      ;; Visits: outer, obj (the content), (obj) (the element via
      ;; the elements iteration), obj (its content, which traverse
      ;; recognizes as already seen and so does not descend) = 4.
      (is (= count 4))
      (is (= result `(~obj (~obj)))))
    ;; When the skip check fires and the parent IS a
    ;; shareable-uninterned-object, the child is dropped entirely.
    (let [y-obj (make-shareable-tree-object (make-tree-id 1) [])
          x-obj (make-shareable-tree-object (make-tree-id 2) [`(~y-obj)])
          structure `(~y-obj (~x-obj))
          [result count] (run structure counter-pre identity-post-fn 0)]
      ;; Visits: outer, y-obj, (x-obj), x-obj = 4. The (y-obj) sub-
      ;; element is dropped by wrap-pre-fn-with-loop-avoidance before
      ;; user-pre-fn is consulted, because x-obj is shareable-
      ;; uninterned and y-obj is already in seen.
      (is (= count 4))
      ;; Each of y-obj and x-obj is assembled as a shareable-tree-
      ;; object carrying the tree-id assigned at its first encounter
      ;; (y-obj is constructed with no elements, x-obj's only element
      ;; was dropped so it is too).
      (is (= result `(~y-obj
                      (~(make-shareable-tree-object
                         (make-tree-id 2) []))))))
    ;; The skip check uses the seen set as it was when this entity's
    ;; elements were entered, so additions made by one sibling's
    ;; recursion do not cause the next sibling to be skipped. obj is
    ;; constructed twice: the first time it gets the tree-id assigned
    ;; by record-encounter and is produced with its elements; the
    ;; second time the key is already in seen, so a bare reference
    ;; (a shareable tree-object with no elements) is produced.
    (let [obj (make-shareable-tree-object (make-tree-id 1) [1])
          structure `(0 (~obj) (~obj))
          [result count] (run structure counter-pre identity-post-fn 0)]
      ;; Visits: outer, 0, (obj), obj, (1), 1, (obj), obj = 8.
      ;; The second time obj is reached its key is already in the seen
      ;; map, so traversal does not descend into its elements.
      (is (= count 8))
      (is (= result `(0 (~obj)
                        (~(make-shareable-tree-object
                           (make-tree-id 1) []))))))
    ;; post-fn receives the caller-data that was input to this level
    ;; (before pre-fn ran), and the caller-data threaded back up
    ;; through the children.
    (let [pre (fn [_ _ _ _] [42 :modified])
          post (fn [_ e orig cd] [e {:orig orig :back cd}])
          ;; Primitive 42, no children: orig=:start, back=:modified.
          [_ result-cd] (run 42 pre post :start)]
      (is (= result-cd {:orig :start :back :modified})))
    ;; A shareable object at the top level is traversed through its
    ;; elements and re-assembled as a shareable-tree-object using the
    ;; tree-id assigned by record-encounter on first encounter (which
    ;; matches obj's original id, since :next-number starts at 1).
    ;; The accumulator records every entity visited, in order: the
    ;; outer object, the wrapped (1), its content 1, the element
    ;; (2 3), its content 2, the wrapped (3), its content 3.
    (let [obj (make-shareable-tree-object (make-tree-id 1) [1 '(2 3)])
          [result visited] (run obj collector-pre identity-post-fn [])]
      (is (= visited [obj '(1) 1 '(2 3) 2 '(3) 3]))
      (is (= result obj)))
    ;; When the starting entity is a stored element of a non-
    ;; presumed-interned object, the traversal must not loop back
    ;; through that object via a back-link from a descendant.
    ;; wrap-caller-data-with-loop-avoidance-data invokes
    ;; originating-entity on the element and pre-populates the seen
    ;; map with that ancestor's key, so the back-link is dropped on
    ;; first encounter.
    (let [[s1 ia-id] (get-new-object-id (new-element-store))
          [s2 ib-id] (get-new-object-id s1)
          ;; add-link arguments are [target source]; the resulting
          ;; element belongs to ia with content ib.
          [s id1] (add-link s2 ia-id ib-id)
          element (id->element id1 s)
          [_ [count _]]
          (threaded-traversal
           element
           (wrap-pre-fn-with-loop-avoidance counter-pre)
           (wrap-post-fn-with-loop-avoidance identity-post-fn)
           (wrap-caller-data-with-loop-avoidance-data element 0))]
      ;; Visits: the element itself and its content ib = 2. The one
      ;; element ib has (the back-link to ia) is dropped by
      ;; wrap-pre-fn-with-loop-avoidance before user-pre-fn is
      ;; consulted, because ia was pre-seeded in seen. Without the
      ;; pre-seeding the back-link would be followed into ia and
      ;; ia's elements, yielding a higher count.
      (is (= count 2)))))

(deftest to-tree-multi-ref-test
  ;; An element has three sub-elements: one wrapping a shareable-tree-
  ;; object with id 1 (referenced once), and two wrapping a shareable-
  ;; tree-object with id 2 (referenced twice). to-tree demotes the
  ;; singly-referenced object to a plain tree-object, and keeps both
  ;; references to the multiply-referenced object as shareable-tree-
  ;; objects with the same id so their shared identity is preserved.
  (let [obj1 (make-shareable-tree-object (make-tree-id 1) [1 2])
        obj2 (make-shareable-tree-object (make-tree-id 2) [3])]
    (is (check (to-tree `(0 (~obj1) (~obj2) (~obj2)))
               `(0 (~(make-tree-object [1 2]))
                     (~(make-shareable-tree-object (make-tree-id 2) [3]))
                     (~(make-shareable-tree-object (make-tree-id 2) [])))))
    ;; If we put obj1 last, the earlier id should go to obj2.
    (is (check (to-tree `(0 (~obj2) (~obj2) (~obj1)))
               `(0 (~(make-shareable-tree-object (make-tree-id 1) [3]))
                   (~(make-shareable-tree-object (make-tree-id 1) []))
                   (~(make-tree-object [1 2]))))))
  ;; to-tree on an uninterned stored object whose elements include
  ;; another uninterned stored object. Both objects are reached only
  ;; once (the back-link from the inner object to the outer one is
  ;; dropped by loop avoidance), so each is demoted to a plain
  ;; tree-object in the result.
  (let [[s1 a-id] (get-new-object-id (new-element-store))
        [s2 b-id] (get-new-object-id s1)
        [s3 _]         (add-link s2 a-id "x")
        [s4 _]         (add-link s3 b-id "y")
        [s5 z-link-id] (add-link s4 b-id "z")
        [s _]          (add-link s5 a-id b-id)
        item-a (id->object a-id s)]
    (is (check (to-tree item-a)
               (as-set (make-tree-object
                        ["x" `(~(as-set (make-tree-object ["y" "z"])))]))))
    ;; Add a sub-element to the z-element whose content is a. The new
    ;; reference reaches a after a has already been recorded by the
    ;; top-level traversal, so a is referenced twice in the result
    ;; tree. Pass 3 leaves both occurrences as shareable-tree-objects
    ;; with the same id so the shared identity is preserved.
    (let [[s' _] (add-link s z-link-id a-id)
          item-a' (id->object a-id s')
          item-b' (id->object b-id s')]
      (is (check (to-tree item-a')
                 (as-set
                  (make-shareable-tree-object
                   (make-tree-id 1)
                   ["x"
                    `(~(as-set
                        (make-tree-object
                         ["y"
                          `("z" (~(make-shareable-tree-object
                                   (make-tree-id 1) [])))])))]))))
      ;; Starting from b, a is referenced twice regardless of the
      ;; iteration order of b's elements: once via the reverse-
      ;; direction back-link and once via z-link's sub-element.
      ;; Whichever of those is visited first carries a's elements;
      ;; the other becomes a bare reference. Both occurrences share
      ;; the same id, and b (single-ref) is demoted to a plain
      ;; tree-object.
      (let [actual (to-tree item-b')]
        ;; We can't put check inside an or, yet we want to use as-set
        ;; inside, so we use differences, instead, which can work
        ;; inside an or.
        (is (or
             ;; back-link visited first.
             (nil?
              (first (differences
                      actual
                      (as-set
                       (make-tree-object
                        [(make-tree-element
                          :target
                          (make-shareable-tree-object
                           (make-tree-id 2) ["x"])
                          [])
                         "y"
                         `("z" (~(make-shareable-tree-object
                                  (make-tree-id 2) [])))])))))
             ;; z-link visited first.
             (nil?
              (first (differences
                      actual
                      (as-set
                       (make-tree-object
                        [(make-tree-element
                          :target
                          (make-shareable-tree-object
                           (make-tree-id 2) [])
                          [])
                         "y"
                         `("z" (~(make-shareable-tree-object
                                  (make-tree-id 2) ["x"])))])))))))))))

(deftest entity-complexity-test
  (is (= (entity-complexity "a") 1.0))
  (is (= (entity-complexity nil) 0.3))
  (is (= (entity-complexity '(1 2 "" nil)) (+ 1 (* 0.75 (+ 1 0.4 0.3)))))
  (is (= (entity-complexity '(1 (2 "a"))) (+ 1 (* 0.75 (+ 1 0.75)))))
  (is (= (entity-complexity `(~(make-tree-object '(1 2)) (2 "a")))
         (+ 0.3 (* 0.75 (+ 1 1)) (* 0.75 (+ 1 0.75))))))

(deftest label-object?-test
  (let [special-object (fn [id] (id->object (make-item-id id) nil))]
    (is (label-object? (special-object "name")))
    (is (label-object? (make-tree-object
                        `((~(special-object "link-type"))))))
    (is (link-type-object? (make-tree-object
                            `((~(special-object "link-type"))))))
    (is (not (object-type-object? (make-tree-object
                                    `((~(special-object "link-type")))))))
    (is (not (non-type-object? (make-tree-object
                                    `((~(special-object "link-type")))))))
    (is (label-object? (make-tree-object
                        `((~(special-object "object-type"))))))
    (is (not (link-type-object? (make-tree-object
                                 `((~(special-object "object-type")))))))
    (is (object-type-object? (make-tree-object
                              `((~(special-object "object-type"))))))
    (is (not (non-type-object? (make-tree-object
                                `((~(special-object "object-type")))))))
    (is (label-object? (make-tree-object
                        `(("fred" ~(special-object "name"))
                          (~(special-object "link-type"))))))
    (is (not (label-object? (make-tree-object
                             `((~(special-object "name")))))))
    (is (not (label-object? (make-tree-object
                             `(("fred" ~(special-object "name")))))))
    (is (not (link-type-object? (make-tree-object
                                 `(("fred" ~(special-object "name")))))))
    (is (not (object-type-object? (make-tree-object
                                   `(("fred" ~(special-object "name")))))))
    (is (non-type-object? (make-tree-object
                           `(("fred" ~(special-object "name"))))))))

(deftest label-element?-test
  (let [special-object (fn [id] (id->object (make-item-id id) nil))]
    (is (label-element? :foo))
    (is (label-element? '(:foo "foo")))
    (is (label-element? `(~(special-object "name"))))
    (is (not (label-element? `("foo" ~(special-object "name")))))
    (is (not (label-element? "foo")))
    (is (not (label-element? `("foo" ~(special-object "name-type")))))
    (is (not (label-element? `("foo" ~(special-object "link-type")))))
    (is (label-element? `(~(make-tree-object
                    `((~(special-object "link-type")))))))
    (is (label-element? `(~(make-tree-object
                    `((~(special-object "object-type")))))))
    (is (label-element? `(~(make-tree-object
                    `(("fred" ~(special-object "name"))
                      (~(special-object "link-type")))))))
    (is (not (label-element? `(~(make-tree-object
                         `((~(special-object "name"))))))))
    (is (not (label-element? `(~(make-tree-object
                         `(("fred" ~(special-object "name"))))))))))

(deftest name-element?-test
  (let [special-object (fn [id] (id->object (make-item-id id) nil))]
    (is (name-element? `("foo" (~(special-object "name")))))
    (is (not (name-element? "foo")))
    (is (not (name-element? `("foo"))))
    (is (not (name-element? `(~(special-object "name")))))))

(deftest make-element-list-test
  (is (= (make-tree-element :source 1 nil)
         1))
  (is (= (make-tree-element :source 1 [2])
         '(1 2)))
  (is (= (make-tree-element :target 1 nil)
         '((:target 1))))
  (is (= (make-tree-element :target 1 [2])
         '((:target 1) 2))))

(deftest make-tree-object-test
  (is (= (make-tree-object [1])
         [:object 1])))


(ns cosheet.store-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            (cosheet
             [store :refer :all]
             [store-impl :refer :all]
             [entity :refer [to-tree make-tree-object make-tree-element
                             id->element id->object]]
             entity-impl
             [utils :refer [pseudo-set-seq pseudo-set-contains?]]
             [canonical :refer [canonical-primitive-form]]
             [orderable :as orderable]
             [test-utils :refer [check as-set]])
            ))

(defn make-object-id [n]
  (if (number? n)
    (assert (< n 0))
    (assert (string? n)))
  (->ItemId n))

(defn make-link-id [n]
  (assert (number? n))
  (assert (> n 0))
  (->ItemId n))

(def foo-oid (make-object-id -2))
(def bar-oid (make-object-id -3))
;;; This store holds two top level items:
;;; <id:4>5
;;; (object-1
;;;    <id:1>(44 <id:2>("Foo" <id:3>(foo-oid :foo-keyword)
;;;                           <id:5>(bar-oid)
;;;                           <id:11>(bar-oid))
;;;              <id:9>("Bar" :bar-keyword)))
;;; foo-oid is a link-type object with name "foo", and bar-oid is a
;;; link-type object with name "bar". (link-type-id and name-label-id
;;; are used directly as sources, without corresponding objects of
;;; their own.)
(def unindexed-test-store
  (map->ElementStoreImpl
   {:id->target
    {(make-link-id 1) (make-object-id "object")
     (make-link-id 2) (make-link-id 1)
     (make-link-id 3) (make-link-id 2)
     (make-link-id 5) (make-link-id 2)
     (make-link-id 6) (make-link-id 3)
     (make-link-id 7) foo-oid
     (make-link-id 71) foo-oid
     (make-link-id 72) (make-link-id 71)
     (make-link-id 9) (make-link-id 1)
     (make-link-id 10) (make-link-id 9)
     (make-link-id 11) (make-link-id 2)  ; a duplicate of id 5.
     (make-link-id 13) bar-oid
     (make-link-id 14) (make-link-id 13)
     (make-link-id 15) bar-oid}
    :id->source
    {(make-link-id 1) 44
     (make-link-id 2) "Foo"
     (make-link-id 3) foo-oid
     (make-link-id 4) 5
     (make-link-id 5) bar-oid
     (make-link-id 6) :foo-keyword
     (make-link-id 7) link-type-id
     (make-link-id 71) "foo"
     (make-link-id 72) name-label-id
     (make-link-id 9) "Bar"
     (make-link-id 10) :bar-keyword
     (make-link-id 11) bar-oid  ; a duplicate of id 5.
     (make-link-id 13) "bar"
     (make-link-id 14) name-label-id
     (make-link-id 15) link-type-id}
    :ephemeral-ids  #{}
    :ephemeral-data {}
    :next-number 1001
    :modified-ids nil
    :equivalent-undo-point false}))

(def empty-store (new-element-store))

(defn clear-store-leaving-indices
  [store]
  (assoc store
         :id->target {}
         :id->source {}))

(deftest id<->string-test
  (let [id (make-item-id "a")]
    (is (= (-> id id->string string->id) id)))
  (let [id (->ItemId 3)]
    (is (= (-> id id->string string->id) id)))
  (let [id (->ItemId -3)]
    (is (= (-> id id->string string->id) id))))

(deftest stored-item-description-name-test
  (is (= (item-id-name (make-item-id "a")) "Id:Ia"))
  (is (= (item-id-name (->ItemId 1)) "Id:1")))

(deftest index-endpoint->ids-test
  (doseq [endpoint [:target :source]] 
    (let [value-key (endpoint-value-key endpoint)
          index-key (endpoint-index-key endpoint)
          ids (keys (:id->source unindexed-test-store))
          ;; Index the unindexed test store.
          store (reduce #(index-endpoint->ids %1 empty-store endpoint %2)
                        unindexed-test-store ids)
          ;; Then unindex it, as if all the links had been removed.
          empty-indexed (clear-store-leaving-indices store)
          unindexed (reduce #(index-endpoint->ids %1 store endpoint %2)
                            empty-indexed ids)]
      (is (= (reduce (fn [accum elements]
                       (+ accum (count (pseudo-set-seq elements))))
                     0 (vals (index-key store)))
             (count (value-key unindexed-test-store))))
      (doseq [id (keys (value-key unindexed-test-store))]
        (let [endpoint-canonical (canonical-primitive-form
                                  (get-in store [value-key id]))]
          (is (pseudo-set-contains?
               (get-in store [index-key endpoint-canonical])
               id))))
      (is (empty? (index-key unindexed))))))

(deftest index-endpoint->label->label-ids-test
  (let [ids (keys (:id->source unindexed-test-store))
        targets-indexed (reduce #(index-endpoint->ids
                                  %1 empty-store :target %2)
                                unindexed-test-store ids)
        sources-indexed (reduce #(index-endpoint->ids
                                  %1 empty-store :source %2)
                                targets-indexed ids)]
    (doseq [endpoint [:target :source]]
      (let [store (reduce #(index-endpoint->label->label-ids
                            %1 empty-store endpoint %2)
                          sources-indexed ids)
            empty-indexed (clear-store-leaving-indices store)
            unindexed (reduce #(index-endpoint->label->label-ids
                                   %1 store endpoint %2)
                                 empty-indexed ids)
            index-key (endpoint-label-index-key endpoint)]
        (is (check (index-key store)
                   (case endpoint
                     :target {(make-link-id 1) {foo-oid (make-link-id 3)
                                                bar-oid #{(make-link-id 5)
                                                          (make-link-id 11)}
                                                :bar-keyword (make-link-id 10)}
                              (make-link-id 2) {:foo-keyword (make-link-id 6)}
                              foo-oid {name-label-id (make-link-id 72)}
                              bar-oid {name-label-id (make-link-id 14)}}
                     :source {"bar" {:bar-keyword (make-link-id 10)
                                     name-label-id (make-link-id 14)}
                              "foo" {foo-oid (make-link-id 3)
                                     bar-oid #{(make-link-id 5)
                                               (make-link-id 11)}
                                     name-label-id (make-link-id 72)}
                              foo-oid {:foo-keyword (make-link-id 6)}})))
        (is (empty? (index-key unindexed)))))))

(def test-store
  (reduce #(index-all %1 empty-store %2)
          unindexed-test-store
          (keys (:id->source unindexed-test-store))))

(deftest id-valid-link?-test
  (is (id-valid-link? test-store (make-link-id 1)))
  (is (not (id-valid-link? test-store (make-link-id 99))))
  (is (not (id-valid-link? test-store (make-object-id "object")))))

(deftest id-known-object?-test
  (is (not (id-known-object? test-store (make-link-id 1))))
  (is (id-known-object? test-store (make-object-id "object")))
  (is (not (id-known-object? test-store (make-object-id "foo")))))

(deftest id-known?-test
  (is (id-known? test-store (make-link-id 1)))
  (is (not (id-known? test-store (make-link-id 99))))
  (is (id-known? test-store (make-object-id "object")))
  (is (not (id-known-object? test-store (make-object-id "foo")))))

(deftest id->target-test
  (is (= (id->target test-store (make-link-id 2)) (make-link-id 1)))
  (is (= (id->target test-store 2) nil)))

(deftest id->source-test
  (is (= (id->source test-store (make-link-id 999)) nil))
  (is (= (id->source test-store (make-link-id 1)) 44))
  (is (= (id->source test-store (make-link-id 2)) "Foo"))
  (is (= (id->source test-store (make-link-id 6)) :foo-keyword)))

(deftest target->ids-test
  (is (= (target->ids test-store (make-object-id "object"))
         [(make-link-id 1)]))
  (is (= (set (target->ids test-store (make-link-id 1)))
         (set [(make-link-id 2) (make-link-id 9)])))
  (is (= (target->ids test-store (make-link-id 999)) nil)))

(deftest source->ids-test
  (is (check (source->ids test-store "Foo")
         (as-set [(make-link-id 2) (make-link-id 71)])))
  (is (check (source->ids test-store bar-oid)
             (as-set [(make-link-id 5) (make-link-id 11)])))
  (is (= (source->ids test-store 123) nil)))

(deftest target-source->ids-test
  ;; Both target and source -ids are sets
  (is (= (target-source->ids test-store (make-link-id 3) :foo-keyword)
         [(make-link-id 6)]))
  ;; Only target-ids is a set
  (is (= (target-source->ids test-store (make-link-id 2) foo-oid)
         [(make-link-id 3)]))
  ;; Only source-ids is a set
  (is (check (target-source->ids test-store (make-link-id 2) bar-oid)
             (as-set [(make-link-id 5) (make-link-id 11)])))
  ;; Neither target nor source -ids are sets
  (is (= (target-source->ids test-store (make-link-id 9) :bar-keyword)
         [(make-link-id 10)])))

(deftest target-label->ids-test
  (is (= (target-label->ids test-store (make-link-id 1) bar-oid)
         [(make-link-id 2)]))
  (is (= (target-label->ids test-store (make-link-id 1) foo-oid)
         [(make-link-id 2)]))
  (is (= (target-label->ids test-store (make-link-id 0.5) "bar") nil))
  (is (= (target-label->ids test-store (make-link-id 999) "bar") nil))
  (is (= (target-label->ids test-store (make-link-id 1) :bar-keyword)
         [(make-link-id 9)]))
  (is (= (target-label->ids test-store (make-link-id 0.5) :bar-keyword)
         nil)))

(deftest target-label-index-on-label-object-transition-test
  ;; When an object O transitions to becoming a label-object (by having
  ;; a link added with target=O and source=link-type-id), existing links
  ;; whose source is O become labels. The index target->label->label-ids
  ;; needs to reflect this transition.
  (let [t-id (make-object-id "test-T")
        o-id (make-object-id "test-O")
        s0 (new-element-store)
        [s1 x-id] (add-link s0 t-id 44)
        ;; Add a link from O to X. It will be a label iff O is a
        ;; label-object.
        [s2 _] (add-link s1 x-id o-id)]
    ;; At this point, O is not yet a label-object, so the link from O
    ;; to X is not a label. The index reflects that.
    (is (= (target-label->ids s2 t-id o-id) nil))
    ;; Now add a link making O a label-object.
    (let [[s3 _] (add-link s2 o-id link-type-id)]
      ;; X should now be findable as an element of T labeled by O.
      (is (= (target-label->ids s3 t-id o-id) [x-id])))))

(deftest source-label->ids-test
  (is (= (source-label->ids test-store "Foo" bar-oid)
         [(make-link-id 2)]))
  (is (= (source-label->ids test-store "foo" bar-oid)
         [(make-link-id 2)]))
  (is (= (source-label->ids test-store "foo" foo-oid)
         [(make-link-id 2)]))
  (is (= (source-label->ids test-store "foo" :bar-keyword) nil))
  (is (= (source-label->ids test-store (make-link-id 1) "bar") nil))
  (is (= (source-label->ids test-store "Bar" :bar-keyword)
         [(make-link-id 9)]))
  (is (= (source-label->ids test-store (make-link-id 0.5) :bar-keyword)
         nil)))

(deftest get-new-object-id-test
  (let [[store id] (get-new-object-id test-store)]
    (is (= (:id id) (- (:next-number test-store))))
    (is (= (:next-number store) (+ 1 (:next-number test-store))))))

(def unindexed-object-store
  (map->ElementStoreImpl
   {:id->target
    {(make-link-id 1) (make-object-id -1)
     (make-link-id 2) (make-object-id -3)
     (make-link-id 3) (make-object-id -3)
     (make-link-id 4) (make-link-id 3)
     (make-link-id 6) (make-object-id -5)
     (make-link-id 7) (make-object-id -7)}
    :id->source
    {(make-link-id 1) (make-object-id -2)
     (make-link-id 2) (make-object-id -4)
     (make-link-id 3) "Joe"
     (make-link-id 4) (make-item-id "name")
     (make-link-id 6) (make-object-id "object")
     (make-link-id 7) (make-object-id -8)}
    :ephemeral-ids  #{}
    :ephemeral-data {}
    :next-number 1001
    :modified-ids nil
    :equivalent-undo-point false}))

(def object-store
  (reduce #(index-all %1 empty-store %2)
          unindexed-object-store
          (keys (:id->source unindexed-object-store))))

(deftest interned-object-id?-test
  (is (not (interned-object-id? object-store (make-object-id -1))))
  (is (interned-object-id? object-store (make-object-id "special")))
  (is (interned-object-id? object-store (make-object-id -3)))
  (is (not (interned-object-id? object-store (make-link-id 2)))))

(deftest has-link-to-non-interned-object?-test
  (is (has-link-to-non-interned-object? object-store (make-object-id -1)))
  (is (has-link-to-non-interned-object? object-store (make-object-id -2)))
  (is (has-link-to-non-interned-object? object-store (make-object-id -3)))
  (is (has-link-to-non-interned-object?
       object-store (make-object-id "object")))
  (is (not (has-link-to-non-interned-object?
            object-store (make-object-id -4))))
  (is (not (has-link-to-non-interned-object?
            object-store (make-object-id -5)))))

(deftest add-link-test
  (let [[added-store id]
        (add-link test-store (make-link-id 1) "test")]
    (is (= (:id id) (:next-number test-store)))
    (is (= (id->source added-store id) "test"))
    (is (= (id->target added-store id) (make-link-id 1))))
  ;; Test that adding nil source fails.
  (is (thrown? java.lang.AssertionError
               (add-link test-store (make-link-id 1) nil)))
  (let [[added-store id]
        (add-link
         (track-modified-ids test-store) (make-link-id 1) "test")]
    (is (= (:modified-ids added-store) #{id})))
  ;; Test illegal links.
  (is (thrown? java.lang.AssertionError
               (add-link object-store (make-object-id 99) (make-object-id 99))))
  (is (thrown? java.lang.AssertionError
               (add-link object-store (make-object-id -2) (make-object-id -7))))
  ;; OK because one side is not non-interned.
  (add-link object-store (make-object-id -2) (make-object-id -3))
  (add-link object-store (make-object-id -3) (make-object-id -2))
  ;; OK because one side's linked to objects are not non-interned.
  (add-link object-store (make-object-id -2) (make-object-id -4))
  (add-link object-store (make-object-id -4) (make-object-id -2))
  (add-link object-store (make-object-id -2) (make-object-id -5))
  (add-link object-store (make-object-id -5) (make-object-id -2)))

(deftest remove-link-test
  (let [[added-store id]
        (add-link test-store (make-link-id 1) 22)]
    (is (= (assoc (remove-link added-store id)
                  :next-number (:next-number test-store))
           test-store))
    (let [removed-store
          (remove-link (track-modified-ids added-store) id)]
      (is (= (:modified-ids removed-store) #{id}))
      (is (= (-> removed-store
                 (assoc :next-number (:next-number test-store))
                 (assoc :modified-ids nil))
             test-store)))))

(deftest update-target-test
  (let [[different-store id]
        (add-link test-store (make-object-id "object") 22)
        changed-store
        (update-target (track-modified-ids different-store)
                        id (make-object-id -2))]
    (is (= (:modified-ids changed-store) #{id}))
    (is (= (id->target changed-store id) (make-object-id -2)))
    ;; Test that setting nil as a target fails.
    (is (thrown? java.lang.AssertionError
                 (update-target test-store id nil)))
    ;; Test that setting a link as a target fails.
    (is (thrown? java.lang.AssertionError
                 (update-target test-store id (make-link-id 1))))
    ;; Test that changing an existing target that is a link fails
    (is (thrown? java.lang.AssertionError
                 (update-target test-store
                                (make-link-id 2) (make-object-id -2))))))

(deftest update-source-test
  (let [[different-store id]
        (add-link test-store (make-link-id 1) 22)
        changed-store
        (update-source (track-modified-ids different-store)
                        id "changed")]
    (is (= (:modified-ids changed-store) #{id}))
    (is (= (id->source changed-store id) "changed"))
    ;; Test that setting nil as a source fails.
    (is (thrown? java.lang.AssertionError
                 (update-source test-store id nil)))
    ;; Test that setting a link as a source fails.
    (is (thrown? java.lang.AssertionError
                 (update-source test-store id (make-link-id 1))))))

(defn check-endpoint->ids
  "Check that the derived index <endpoint>->ids is right"
  [store endpoint]
  (let [primary-key (endpoint-value-key endpoint)
        index-key (endpoint-index-key endpoint)]
    ;; Everything in :endpoint->ids is true.
    (doseq [[id links] (index-key store)]
      (doseq [link (pseudo-set-seq links)]
        (is (= (canonical-primitive-form (get-in store [primary-key link]))
               id))))
    ;; Everything that should be in :endpoint->ids is.
    (doseq [[id endpoint] (primary-key store)]
      (is (pseudo-set-contains?
           (get-in store [index-key (canonical-primitive-form endpoint)])
           id)))))

(defn check-endpoint->label->label-ids
  "Check that the derived index <endpoint>->label->label-ids is right.
  Assumes that the endpoint->ids indices are correct."
  [store endpoint]
  (let [primary-key (endpoint-value-key endpoint)
        reverse-primary-key (endpoint-index-key endpoint)
        index-key (endpoint-label-index-key endpoint)]
    ;; Everything in :endpoint->label->label-ids is true
    (doseq [[endpoint-value map] (index-key store)]
      (doseq [[label ids] map]
        (doseq [label-id (pseudo-set-seq ids)]
          ;; The label-id is a grandchild of the endpoint-value.
          (is (= (canonical-primitive-form
                  (get-in store [primary-key (id->target store label-id)]))
                 endpoint-value))
          ;; The label-id has the right source.
          (is (= (canonical-primitive-form (id->source store label-id)) label))
          ;; The label-id is a label.
          (is (id-is-label? store label-id)))))
    ;; Everything that should be in :endpoint->label->label-ids is.
    (doseq [[id source] (:id->source store)]
      (when (id-is-label? store id)
        (let [label-key (canonical-primitive-form (id->source store id))
              label-target (id->target store id)]
          (when-let [endpoint-value (canonical-primitive-form
                                     (get-in store [primary-key label-target]))]
            (is (pseudo-set-contains?
                 (get-in store [index-key endpoint-value label-key])
                 id))))))))

(defn check-derived-indices
  "Check that each of the derived indices of the store matches the data."
  [store]
  (check-endpoint->ids store :target)
  (check-endpoint->ids store :source)
  (check-endpoint->label->label-ids store :target)
  (check-endpoint->label->label-ids store :source))

(deftest all-indices-test
  (check-derived-indices test-store))

(require '[clojure.data.generators :as gen])
(deftest lots-of-changes-indices-test
  ;; We repeatedly add a bunch of elements and remove a bunch of
  ;; elements and check that the derived indices are correct. To make
  ;; sure that the removals are legal, an element only references
  ;; elements with at most 1/2 its id number, and we only remove at
  ;; most the last 1/2 of the elements while removing. (We remove them
  ;; in a random order, so this guarantees that we won't remove a
  ;; link while another link references it.
  (binding [gen/*rnd* (java.util.Random. 437)])
  (let [iterations 100  ;; 100000
        earlier-number (fn [n] (gen/uniform 1 (+ 1 (int (/ n 2)))))
        random-object (fn [] (make-object-id
                              (- (+ 1 (int (/ 200 (gen/uniform 1 100)))))))
        random-source (fn [target]
                        (case (gen/uniform 0 4)
                          0 (if (= (gen/uniform 0 2) 0)
                              (str "N" (int (/ 200 (gen/uniform 1 100))))
                              (int (/ 200 (gen/uniform 1 100))))
                          1 (random-object)
                          2 (if (link-id? target)
                              link-type-id
                              object-type-id)
                          3 :bar-keyword))]
    (loop [iteration 0
           store (let [target (random-object)
                       inner-target (random-object)]
                   (first (add-link
                           (first (add-link
                                   (new-element-store)
                                   inner-target (random-source inner-target)))
                           target (random-source target))))
           items 2]
      (let [;; Number of items to end up with after adding (has a long tail)
            n (max (+ items 10) (int (/ 1000 (gen/uniform 1 20))))
            ;; Number of items to keep after removing
            m (gen/uniform (int (/ n 2)) n)]
        (let [added-store
              (reduce (fn [store i]
                        (let [[new-store id]
                              (let [target (when (not= 0 (gen/uniform 0 10))
                                             (make-link-id (earlier-number i)))]
                                (add-link store target (random-source target)))]
                          (assert (= (:id id) i))
                          new-store))
                      store (range (+ items 1) (+ n 1)))
              mutated-store
              (reduce (fn [store i]
                        (let [id (make-link-id i)]
                          (cond-> store
                            (= 0 (gen/uniform 0 4))
                            (update-source id (random-source
                                               (id->target store id)))
                            (and (= 0 (gen/uniform 0 4))
                                 (object-id? (id->target store id)))
                            (update-target id (random-object)))))
                      added-store (range 1 (+ n 1)))
              removed-store
              (reduce (fn [store i]
                        (remove-link store (make-link-id i)))
                      mutated-store (gen/shuffle (range (+ m 1) (+ n 1))))]
          (check-derived-indices added-store)
          (check-derived-indices removed-store)
          (when (< iteration iterations)
            (recur (+ iteration 1)
                   (assoc removed-store :next-number (+ m 1))
                   m)))))))

(deftest candidate-matching-ids-test
  (let [obj-2 (id->object foo-oid nil)
        bar-object (id->object bar-oid nil)
        foo-object (id->object foo-oid test-store)]
    (is (check (candidate-matching-ids-and-estimate test-store 5)
               [1 [(make-link-id 4)] true]))
    (is (check (candidate-matching-ids-and-estimate test-store '(5))
               [1 [(make-link-id 4)] true]))
    (is (check (candidate-matching-ids-and-estimate test-store '(nil "Foo"))
               [2 [(make-link-id 1)] true]))
    (is (check (candidate-matching-ids-and-estimate test-store `(~obj-2 :foo-keyword))
               [1 [(make-link-id 3)] true]))
    (is (check (candidate-matching-ids-and-estimate test-store '(0 "Foo"))
               [0 [] false]))
    (is (check (candidate-matching-ids-and-estimate
                test-store `(nil (~obj-2) (~bar-object)))
               [1 [(make-link-id 2)] true]))
    (is (check (candidate-matching-ids-and-estimate
                test-store `(nil (~bar-object) (~bar-object)))
               [2 [(make-link-id 2)] false]))
    (is (check (candidate-matching-ids-and-estimate
                test-store
                (make-tree-element
                 :target (id->object (make-object-id "object") nil) nil))
               [1 [(make-link-id 1)] true]))
    (is (check (candidate-matching-ids-and-estimate
                test-store
                (make-tree-element
                 :target (id->object (make-object-id "object") nil) '("Foo")))
               [1 [(make-link-id 1)] true]))
    (is (check (candidate-matching-ids-and-estimate
                test-store
                (make-tree-element
                 :target foo-object nil))
               [2 (as-set [(make-link-id 7) (make-link-id 71)]) true]))
    (is (check (candidate-matching-ids-and-estimate
                test-store
                (make-tree-element
                 :target foo-object `((~(id->object name-label-id nil)))))
               [2 [(make-link-id 71)] true]))
    (is (check (candidate-matching-ids-and-estimate
                test-store
                (make-tree-element
                 :target foo-object '(5)))
               [1 () true]))
    (is (check (candidate-matching-ids-and-estimate
                test-store
                (make-tree-element
                 :target foo-object nil))
               [2 (as-set [(make-link-id 7) (make-link-id 71)]) true]))
    (is (check (candidate-matching-ids-and-estimate
                test-store (make-tree-element
                            :target (id->object (make-object-id -999) nil) nil))
               [0 () true]))
    (is (nil? (candidate-matching-ids-and-estimate test-store '(nil))))
    (is (check (candidate-matching-ids test-store nil)
               [(as-set [(make-object-id "object")
                         link-type-id
                         name-label-id
                         foo-oid
                         bar-oid
                         (make-link-id 1)
                         (make-link-id 2) (make-link-id 3)
                         (make-link-id 4) (make-link-id 5)
                         (make-link-id 6) (make-link-id 7)
                         (make-link-id 71) (make-link-id 72)
                         (make-link-id 9)
                         (make-link-id 10) (make-link-id 11)
                         (make-link-id 13) (make-link-id 14)
                         (make-link-id 15)])
                false]))
    (is (check (candidate-matching-ids test-store '(nil))
               [(as-set [(make-link-id 1)
                         (make-link-id 2) (make-link-id 3)
                         (make-link-id 4) (make-link-id 5)
                         (make-link-id 6) (make-link-id 7)
                         (make-link-id 71) (make-link-id 72)
                         (make-link-id 9)
                         (make-link-id 10) (make-link-id 11)
                         (make-link-id 13) (make-link-id 14)
                         (make-link-id 15)])
                false]))
    (is (check (candidate-matching-ids test-store (make-tree-object nil))
               [(as-set  [(make-object-id "object")
                          link-type-id
                          name-label-id
                          foo-oid
                          bar-oid])
                false]))
    (is (check (candidate-matching-ids test-store '(nil nil))
               [(as-set  [(make-link-id 1)
                          (make-link-id 2) (make-link-id 3)
                          (make-link-id 71)
                          (make-link-id 9)
                          (make-link-id 13)])
                false]))
    (is (check (candidate-matching-ids test-store '("Foo"))
               [(as-set [(make-link-id 71) (make-link-id 2)]) true]))
    (is (check (candidate-matching-ids test-store 5)
               [[(make-link-id 4)] true]))
    (is (check (candidate-matching-ids test-store '(nil "Foo" nil))
               [[(make-link-id 1)] false]))
    (is (check (candidate-matching-ids test-store '(5 nil))
               [[(make-link-id 4)] false]))
    (is (check (candidate-matching-ids test-store (make-tree-object '((44))))
               [[(make-object-id "object")] true]))
    (is (check (candidate-matching-ids test-store (make-tree-object '((nil))))
               [(as-set [(make-object-id "object") foo-oid bar-oid])
                false]))
    (is (check (candidate-matching-ids
                test-store (make-tree-object
                            `(~(make-tree-element :target nil nil))))
               [(as-set [(make-object-id "name")
                         (make-object-id "link-type")
                         foo-oid
                         bar-oid])
                false]))
    (is (check (candidate-matching-ids
                test-store (make-tree-object
                            `((nil) ~(make-tree-element :target nil nil))))
               [(as-set [foo-oid bar-oid])
                false]))
    (is (check (candidate-matching-ids
                test-store (make-tree-object '((44) (44))))
               [[(make-object-id "object")] false]))
    (is (check (candidate-matching-ids test-store (make-tree-object '(("Foo"))))
               [[foo-oid] true]))))

(deftest declare-ephemeral-id-test
  (is (= (:ephemeral-ids test-store) #{}))
  (let [ephemeral-store (-> test-store
                            (declare-ephemeral-id (make-link-id 3))
                            (add-link (make-link-id 1) "hi")
                            first
                            (declare-ephemeral-id (make-link-id 14)))]
    (is (= (:ephemeral-ids ephemeral-store)
           #{(make-link-id 3) (make-link-id 14)}))
    (is (= (all-ephemeral-ids ephemeral-store)
           #{(make-link-id 3) (make-link-id 6) (make-link-id 14)}))))

(deftest ephemeral-data-test
  (is (= (:ephemeral-data test-store) {}))
  (let [store (assoc test-store :ephemeral-data {:foo-keyword 1})]
    (is (= (:ephemeral-data store) {:foo-keyword 1}))))

(deftest new-element-store-test
  (let [store (new-element-store)]
    (is (= (candidate-matching-ids store nil) [nil false]))))

(deftest store-to-data-to-store-test
  (is (check (into {} test-store)
             (into {} (data-to-store (new-element-store)
                                     (store-to-data test-store)))))
  ;; Now try it with some items not serialized
  (let [ephemeral-store (-> test-store
                            (declare-ephemeral-id (make-link-id 3))
                            (declare-ephemeral-id (make-link-id 14)))
        smaller-store (-> test-store
                          (remove-link (make-link-id 14))
                          (remove-link (make-link-id 6)) ; Points to id 3
                          (remove-link (make-link-id 3)))]
    (is (check (into {} smaller-store)
               (into {} (data-to-store (new-element-store)
                                       (store-to-data ephemeral-store))))))
  ;; Try with obj-2 being non-interned.
  (let [ephemeral-store (-> test-store
                            (declare-ephemeral-id (make-link-id 3))
                            (declare-ephemeral-id (make-link-id 14))
                            ;; Make id -2 non-interned
                            (remove-link (make-link-id 72)))
        smaller-store (-> test-store
                          (remove-link (make-link-id 14))
                          (remove-link (make-link-id 6)) ; Points to id 3
                          (remove-link (make-link-id 3))
                          (remove-link (make-link-id 72))
                          (remove-link (make-link-id 71)) ; Points to id -2
                          (remove-link (make-link-id 7)) ; Points to id -2
                          )]
    (is (check (into {} smaller-store)
               (into {} (data-to-store (new-element-store)
                                       (store-to-data ephemeral-store)))))))

(deftest write-read-test
  (let [store (first
               ;; Add an Orderable to the store to check its serialization.
               (add-link test-store
                         (make-object-id "object")
                         (first (orderable/split orderable/initial))))
        outstr (java.io.ByteArrayOutputStream.)]
    (write-store store outstr)
    (with-open [instr (java.io.ByteArrayInputStream.
                       (.toByteArray outstr))]
      (let [s (read-store (new-element-store) instr)]
        (is (check (into {} (seq s)) (into {} (seq store))))))))

(deftest valid-undo-point-test
  (is (not (equivalent-undo-point? test-store)))
  (is (equivalent-undo-point?
       (update-equivalent-undo-point test-store true)))
  (is (not (equivalent-undo-point?
            (update-equivalent-undo-point test-store false)))))


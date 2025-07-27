(ns cosheet2.store-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            (cosheet2
             [store :refer :all]
             [store-impl :refer :all]
             [entity :refer [to-list description->entity]]
             entity-impl
             [utils :refer [pseudo-set-seq pseudo-set-contains?]]
             [canonical :refer [canonical-primitive-form]]
             [orderable :as orderable]
             [test-utils :refer [check as-set]])
            ))

(defn make-object-id [n]
  (assert (number? n))
  (assert (< n 0))
  (->ItemId n))

(defn make-link-id [n]
  (assert (number? n))
  (assert (> n 0))
  (->ItemId n))

;;; TODO: once the store accepts objects, put some of them in here.
(def unindexed-test-store
  (map->ElementStoreImpl
   {:id->target
    {(make-link-id 1) (make-object-id -1)
     (make-link-id 2) (make-link-id 1)
     (make-link-id 3) (make-link-id 2)
     (make-link-id 5) (make-link-id 2)
     (make-link-id 6) (make-link-id 3)
     (make-link-id 7) (make-link-id 3)
     (make-link-id 8) (make-link-id 5)
     (make-link-id 9) (make-link-id 1)
     (make-link-id 10) (make-link-id 9)}
    :id->source
    {(make-link-id 1) 44
     (make-link-id 2) "Foo"
     (make-link-id 3) "Baz"
     (make-link-id 4) 5
     (make-link-id 5) "bar"
     (make-link-id 6) :baz
     (make-link-id 7) :label
     (make-link-id 8) :label
     (make-link-id 9) "Bar"
     (make-link-id 10) :order}
    :temporary-ids  #{}
    :marked-as-type #{}
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

(deftest index-marked-as-type-test
  (let [ids (keys (:id->source unindexed-test-store))
        elements-indexed (reduce #(index-endpoint->ids
                                   %1 empty-store :target %2)
                                 unindexed-test-store ids)
        store (reduce #(index-marked-as-type %1 empty-store %2)
                      elements-indexed ids)
        empty-indexed (clear-store-leaving-indices store)
        unindexed (reduce #(index-marked-as-type %1 store %2)
                          empty-indexed ids)]
    (is (check (:marked-as-type store)
               #{(make-link-id 3) (make-link-id 5)}))
    (is (empty? (:marked-as-type unindexed)))))

(deftest index-endpoint->label->label-ids-test
  (doseq [endpoint [:target :source]]
    (let [ids (keys (:id->source unindexed-test-store))
          targets-indexed (reduce #(index-endpoint->ids
                                    %1 empty-store :target %2)
                                  unindexed-test-store ids)
          marks-indexed (reduce #(index-marked-as-type %1 empty-store %2)
                                targets-indexed ids)
          store (reduce #(index-endpoint->label->label-ids
                          %1 empty-store endpoint %2)
                        marks-indexed ids)
          empty-indexed (clear-store-leaving-indices store)
          unindexed (reduce #(index-endpoint->label->label-ids
                              %1 store endpoint %2)
                            empty-indexed ids)
          index-key (endpoint-label-index-key endpoint)]
      (is (check (index-key store)
                 (case endpoint
                   :target {(make-link-id 1) {"baz" (make-link-id 3)
                                              "bar" (make-link-id 5)
                                              :order (make-link-id 10)}
                            (make-link-id 2) {:baz (make-link-id 6)}}
                   :source {"bar" {:order (make-link-id 10)}
                            "foo" {"baz" (make-link-id 3)
                                   "bar" (make-link-id 5)}
                            "baz" {:baz (make-link-id 6)}})))
      (is (empty? (index-key unindexed))))))

(def test-store
  (reduce #(index-all %1 empty-store %2)
          unindexed-test-store
          (keys (:id->source unindexed-test-store))))

(deftest id-valid-link?-test
  (is (id-valid-link? test-store (make-link-id 1)))
  (is (not (id-valid-link? test-store (make-link-id 99)))))

(deftest id->target-test
  (is (= (id->target test-store (make-link-id 2)) (make-link-id 1)))
  (is (= (id->target test-store 2) nil)))

(deftest id->source-test
  (is (= (id->source test-store (make-link-id 999)) nil))
  (is (= (id->source test-store (make-link-id 1)) 44))
  (is (= (id->source test-store (make-link-id 2)) "Foo"))
  (is (= (id->source test-store (make-link-id 6)) :baz)))

(deftest target->ids-test
  (is (= (target->ids test-store (make-object-id -1)) [(make-link-id 1)]))
  (is (= (set (target->ids test-store (make-link-id 1)))
         (set [(make-link-id 2) (make-link-id 9)])))
  (is (= (target->ids test-store (make-link-id 999)) nil)))

(deftest source->ids-test
  (is (= (source->ids test-store "Foo") [(make-link-id 2)]))
  (is (check (source->ids test-store :label)
             (as-set [(make-link-id 7) (make-link-id 8)])))
  (is (= (source->ids test-store 123) nil)))

(deftest target-source->ids-test
  ;; Both target and source -ids are sets
  (is (= (target-source->ids test-store (make-link-id 3) :label)
         [(make-link-id 7)]))
  ;; Only target-ids is a set
  (is (= (target-source->ids test-store (make-link-id 2) "baz")
         [(make-link-id 3)]))
  ;; Only source-ids is a set
  (is (= (target-source->ids test-store (make-link-id 5) :label)
         [(make-link-id 8)]))
  ;; Neither target nor source -ids are sets
  (is (= (target-source->ids test-store (make-link-id 9) :order)
         [(make-link-id 10)])))

(deftest target-label->ids-test
  (is (= (target-label->ids test-store (make-link-id 1) "Bar")
         [(make-link-id 2)]))
  (is (= (target-label->ids test-store (make-link-id 1) "Baz")
         [(make-link-id 2)]))
  (is (= (target-label->ids test-store (make-link-id 0.5) "bar") nil))
  (is (= (target-label->ids test-store (make-link-id 999) "bar") nil))
  (is (= (target-label->ids test-store (make-link-id 1) :order)
         [(make-link-id 9)]))
  (is (= (target-label->ids test-store (make-link-id 0.5) :order)
         nil)))

(deftest source-label->ids-test
  (is (= (source-label->ids test-store "Foo" "Bar")
         [(make-link-id 2)]))
  (is (= (source-label->ids test-store "foo" "bar")
         [(make-link-id 2)]))
  (is (= (source-label->ids test-store "foo" "Baz")
         [(make-link-id 2)]))
  (is (= (source-label->ids test-store "foo" :order) nil))
  (is (= (source-label->ids test-store (make-link-id 1) "bar") nil))
  (is (= (source-label->ids test-store "Bar" :order)
         [(make-link-id 9)]))
  (is (= (source-label->ids test-store (make-link-id 0.5) :order)
         nil)))

(deftest id->marked-as-type?-test
  (is (id->marked-as-type? test-store (make-link-id 3)))
  (is (id->marked-as-type? test-store (make-link-id 5)))
  (is (not (id->marked-as-type? test-store (make-link-id 7))))
  (is (not (id->marked-as-type? test-store (make-link-id 1)))))

(deftest get-new-object-id-test
  (let [[id store] (get-new-object-id test-store)]
    (is (= (:id id) (- (:next-number test-store))))
    (is (= (:next-number store) (+ 1 (:next-number test-store))))))

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
    (is (= (:modified-ids added-store) #{id}))))

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
        (add-link test-store (make-object-id -1) 22)
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

(defn check-marked-as-type
  "Check that the derived set labels is right."
  [store]
  (let [marked-as-type (:marked-as-type store)]
    ;; Everything in :marked-as-type has a mark.
    (doseq [id marked-as-type]
      (is (seq (target-source->ids store id :label))))
    ;; Everything that should be in :marked-as-type is.
    (doseq [[id source] (:id->source store)]
      (when (seq (target-source->ids store id :label))
        (is (contains? marked-as-type id))))))

(defn check-endpoint->label->label-ids
  "Check that the derived index <endpoint>->label->label-ids is right.
  Assumes that the endpoint->ids and the marked-as-type indices are correct."
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
      ;; Note: must be kept in synch with entity/label?
      (when-let [label-id (cond (= source :label) (id->target store id)
                                (= source :order) id)]
        (let [label (canonical-primitive-form (id->source store label-id))
              label-target (id->target store label-id)]
          (when-let [endpoint-value (canonical-primitive-form
                                     (get-in store [primary-key label-target]))]
            (is (pseudo-set-contains?
                 (get-in store [index-key endpoint-value label])
                 label-id))))))))

(defn check-derived-indices
  "Check that each of the derived indices of the store matches the data."
  [store]
  (check-endpoint->ids store :target)
  (check-endpoint->ids store :source)
  (check-marked-as-type store)
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
  ;; link while another link reverences it.
  (binding [gen/*rnd* (java.util.Random. 437)])
  (let [earlier-number (fn [n] (gen/uniform 1 (+ 1 (int (/ n 2)))))
        random-object (fn [] (make-object-id
                              (- (+ 1 (int (/ 200 (gen/uniform 1 100)))))))
        random-source (fn [] (case (gen/uniform 0 4)
                               0 (if (= (gen/uniform 0 2) 0)
                                   (str "N" (int (/ 200 (gen/uniform 1 100))))
                                   (int (/ 200 (gen/uniform 1 100))))
                               1 (random-object)
                               2 :label
                               3 :order))
        random-target (fn [i] (case (gen/uniform 0 2)
                                0 (make-link-id (earlier-number i))
                                1 (random-object)))]
    (loop [iteration 0
           store (first (add-link
                         (first (add-link
                                 (new-element-store)
                                 (random-object) (random-source)))
                         (random-object) (random-source)))
           items 2]
      (let [;; Number of items to end up with after adding (has a long tail)
            n (max (+ items 10) (int (/ 1000 (gen/uniform 1 100))))
            ;; Number of items to keep after removing
            m (gen/uniform (int (/ n 2)) n)]
        (let [added-store
              (reduce (fn [store i]
                        (let [[new-store id]
                              (add-link
                               store
                               (when (not= 0 (gen/uniform 0 10))
                                 (make-link-id (earlier-number i)))
                               (random-source))]
                          (assert (= (:id id) i))
                          new-store))
                      store (range (+ items 1) (+ n 1)))
              mutated-store
              (reduce (fn [store i]
                        (let [id (make-link-id i)]
                          (cond-> store
                            (= 0 (gen/uniform 0 4))
                            (update-source id (random-source))
                            (and (= 0 (gen/uniform 0 4))
                                 (is-object-id? (id->target store id)))
                            (update-target id (random-object)))))
                      added-store (range 1 (+ n 1)))
              removed-store
              (reduce (fn [store i]
                        (remove-link store (make-link-id i)))
                      mutated-store (gen/shuffle (range (+ m 1) (+ n 1))))]
          (check-derived-indices added-store)
          (check-derived-indices removed-store)
          (when (< iteration 100)
            (recur (+ iteration 1)
                   (assoc removed-store :next-number (+ m 1))
                   m)))))))

(deftest candidate-matching-ids-test
  (is (check (candidate-matching-ids-and-estimate test-store 5)
             [1 [(make-link-id 4)] true]))
  (is (check (candidate-matching-ids-and-estimate test-store '(5))
             [1 [(make-link-id 4)] true]))
  (is (check (candidate-matching-ids-and-estimate test-store '(nil "Foo"))
             [1 [(make-link-id 1)] true]))
  (is (check (candidate-matching-ids-and-estimate test-store '("Baz" :baz))
             [1 [(make-link-id 3)] true]))
  (is (check (candidate-matching-ids-and-estimate test-store '(0 "Foo"))
             [0 [] false]))
  (is (check (candidate-matching-ids-and-estimate test-store '(nil "baz" "bar"))
             [1 [(make-link-id 2)] true]))
  (is (check (candidate-matching-ids-and-estimate test-store '(nil "bar" "bar"))
             [2 [(make-link-id 2) (make-link-id 1)] false]))
  (is (nil? (candidate-matching-ids-and-estimate test-store '(nil))))
  (is (check (candidate-matching-ids test-store nil)
             [(as-set [(make-link-id 1)
                       (make-link-id 2) (make-link-id 3)
                       (make-link-id 4) (make-link-id 5)
                       (make-link-id 6) (make-link-id 7)
                       (make-link-id 8) (make-link-id 9)
                       (make-link-id 10)])
              false]))
  (is (check (candidate-matching-ids test-store '(nil nil))
             [(as-set  [(make-object-id -1) (make-link-id 1)
                        (make-link-id 2) (make-link-id 3)
                        (make-link-id 5) (make-link-id 9)])
              false]))
  (is (check (candidate-matching-ids test-store '("Foo"))
             [[(make-link-id 2)] true]))
  (is (check (candidate-matching-ids test-store 5)
             [[(make-link-id 4)] true]))
    (is (check (candidate-matching-ids test-store '(nil "Foo" nil))
             [[(make-link-id 1)] false]))
  (is (check (candidate-matching-ids test-store '(5 nil))
             [[(make-link-id 4)] false])))

(deftest declare-temporary-id-test
  (is (= (:temporary-ids test-store) #{}))
  (let [temporary-store (-> test-store
                            (declare-temporary-id (make-link-id 3))
                            (add-link (make-link-id 1) "hi")
                            first
                            (declare-temporary-id (make-link-id 8)))]
    (is (= (:temporary-ids temporary-store)
           #{(make-link-id 3) (make-link-id 8)}))
    (is (= (all-temporary-ids temporary-store)
           #{(make-link-id 3) (make-link-id 6) (make-link-id 7)
             (make-link-id 8)}))))

(deftest new-element-store-test
  (let [store (new-element-store)]
    (is (= (candidate-matching-ids store nil) [nil false]))))

(deftest store-to-data-to-store-test
  (is (check (into {} test-store)
             (into {} (data-to-store (new-element-store)
                                     (store-to-data test-store)))))
  ;; Now try it with some items not serialized
  (let [temporary-store (-> test-store
                            (declare-temporary-id (make-link-id 3))
                            (declare-temporary-id (make-link-id 8)))
        smaller-store (-> test-store
                          (remove-link (make-link-id 8))
                          (remove-link (make-link-id 7)) ; Points to id 3
                          (remove-link (make-link-id 6)) ; Points to id 3
                          (remove-link (make-link-id 3)))]
    (is (check (into {} smaller-store)
               (into {} (data-to-store (new-element-store)
                                       (store-to-data temporary-store)))))))

(deftest write-read-test
  (let [store (first
               ;; Add an Orderable to the store to check its serialization.
               (add-link test-store
                         (make-object-id -1)
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


(ns cosheet2.store-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            (cosheet2
             [store :refer :all]
             [store-impl :refer :all]
             entity-impl
             [utils :refer [pseudo-set-seq pseudo-set-contains?]]
             [canonical :refer [canonical-primitive-form]]
             [orderable :as orderable]
             [test-utils :refer [check as-set]])
            ))

(defn make-link-id [n]
  (assert (number? n))
  (assert (> n 0))
  (->ItemId n))

(def unindexed-test-store
  (map->ElementStoreImpl
   {:id->target
    ;; We use 0.5 as an id because we were using 0, but can no longer
    ;; have 0 ids for links. And we need an id less than 1.
    {(make-link-id 1) (make-link-id 0.5)
     (make-link-id 2) (make-link-id 1)
     (make-link-id 3) (make-link-id 2)
     (make-link-id 5) (make-link-id 2)
     (make-link-id 6) (make-link-id 3)
     (make-link-id 7) (make-link-id 3)
     (make-link-id 8) (make-link-id 5)
     (make-link-id 9) (make-link-id 1)
     (make-link-id 10) (make-link-id 9)}
    :id->source
    {(make-link-id 0.5) 0
     (make-link-id 1) 44
     (make-link-id 2) "Foo"
     (make-link-id 3) "Baz"
     (make-link-id 4) 5
     (make-link-id 5) "bar"
     (make-link-id 6) :baz
     (make-link-id 7) :label
     (make-link-id 8) :label
     (make-link-id 9) "Bar"
     (make-link-id 10) :order}
    :temporary-ids
    #{}
    :next-id
    1001
    :modified-ids
    nil
    :equivalent-undo-point
    false}))

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
    (is (= (-> id id->string string->id) id))))

(deftest stored-item-description-name-test
  (is (= (item-id-name (make-item-id "a")) "Id:Ia"))
  (is (= (item-id-name (->ItemId 1)) "Id:1")))

(deftest index-endpoint->ids-test
  (doseq [endpoint [:target :source]] 
    (let [value-key (case endpoint :target :id->target :source :id->source)
          index-key (case endpoint :target :target->ids :source :source->ids)
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
          (is (pseudo-set-contains? (get-in store [index-key
                                                   endpoint-canonical])
                                    id))))
      (is (empty? (index-key unindexed))))))

(deftest index-id->keywords-test
  (let [ids (keys (:id->source unindexed-test-store))
        elements-indexed (reduce #(index-endpoint->ids
                                   %1 empty-store :target %2)
                                 unindexed-test-store ids)
        store (reduce #(index-id->keywords %1 empty-store %2)
                      elements-indexed ids)
        empty-indexed (clear-store-leaving-indices store)
        unindexed (reduce #(index-id->keywords %1 store %2) empty-indexed ids)]
    (is (check (:id->keywords store)
               {(make-link-id 3) #{:label :baz}
                (make-link-id 5) :label
                (make-link-id 9) :order}))
    (is (empty? (:id->keywords unindexed)))))

(deftest index-target->label->label-ids-test
  (let [ids (keys (:id->source unindexed-test-store))
        targets-indexed (reduce #(index-endpoint->ids
                                  %1 empty-store :target %2)
                                 unindexed-test-store ids)
        keywords-indexed (reduce #(index-id->keywords %1 empty-store %2)
                                 targets-indexed ids)
        store (reduce #(index-target->label->label-ids %1 empty-store %2)
                      keywords-indexed ids)
        empty-indexed (clear-store-leaving-indices store)
        unindexed (reduce #(index-target->label->label-ids %1 store %2)
                          empty-indexed ids)]
    (is (check (:target->label->label-ids store)
               {(make-link-id 1) {"baz" (make-link-id 3)
                                  "bar" (make-link-id 5)
                                  :order (make-link-id 10)}
                (make-link-id 2) {:baz (make-link-id 6)}}))
    (is (empty? (:source->ids unindexed)))))

(def test-store
  (let [ids (keys (:id->source unindexed-test-store))]
    (as-> unindexed-test-store store
      (reduce #(index-endpoint->ids %1 empty-store :target %2) store ids)
      (reduce #(index-endpoint->ids %1 empty-store :source %2) store ids)
      (reduce #(index-id->keywords %1 empty-store %2) store ids)
      (reduce #(index-target->label->label-ids %1 empty-store %2) store ids))))

(deftest all-X-test
  (is (= (set (all-ids-eventually-holding-source test-store 5))
         #{(make-link-id 4)}))
  (is (= (set (all-ids-eventually-holding-id test-store (make-link-id 4)))
         #{(make-link-id 4)}))
  (is (= (set (all-forward-reachable-ids test-store (make-link-id 1)))
          #{(make-link-id 0.5) (make-link-id 1)})))

(deftest id-valid-link?-test
  (is (id-valid-link? test-store (make-link-id 1)))
  (is (not (id-valid-link? test-store (make-link-id 99)))))

(deftest id->source-test
  (is (= (id->source test-store (make-link-id 999)) nil))
  (is (= (id->source test-store (make-link-id 1)) 44))
  (is (= (id->source test-store (make-link-id 2)) "Foo"))
  (is (= (id->source test-store (make-link-id 6)) :baz)))

(deftest target-id->ids-test
  (is (= (target-id->ids test-store (make-link-id 0.5)) [(make-link-id 1)]))
  (is (= (set (target-id->ids test-store (make-link-id 1)))
         (set [(make-link-id 2) (make-link-id 9)])))
  (is (= (target-id->ids test-store (make-link-id 999)) nil)))

(deftest target-id-label->ids-test
  (is (= (target-id-label->ids test-store (make-link-id 1) "Bar")
         [(make-link-id 2)]))
  (is (= (target-id-label->ids test-store (make-link-id 1) "Baz")
         [(make-link-id 2)]))
  (is (= (target-id-label->ids test-store (make-link-id 0.5) "bar") nil))
  (is (= (target-id-label->ids test-store (make-link-id 999) "bar") nil))
  (is (= (target-id-label->ids test-store (make-link-id 1) :order)
         [(make-link-id 9)]))
  (is (= (target-id-label->ids test-store (make-link-id 0.5) :order)
         nil)))

(deftest id->has-keyword?-test
  (is (id->has-keyword? test-store (make-link-id 3) :baz))
  (is (id->has-keyword? test-store (make-link-id 3) :label))
  (is (not (id->has-keyword? test-store (make-link-id 3) :bar)))
  (is (not (id->has-keyword? test-store (make-link-id 2) :baz))))

(deftest source-id->ids-test
  ;; TODO: !!! Once objects can be sources, revise this to use them.
  (is (= (vec (source-id->ids test-store (make-link-id 4)))
         []))
  (is (= (source-id->ids test-store (make-link-id 1)) nil))
  (is (thrown? java.lang.AssertionError
               (source-id->ids test-store "Foo"))))

(deftest id->target-test
   (is (= (id->target test-store (make-link-id 2)) (make-link-id 1)))
   (is (= (id->target test-store 2) nil)))

(deftest add-lin-test
  (let [[added-store id]
        (add-link test-store (make-link-id 1) "test")]
    (is (= (:id id) (:next-id test-store)))
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
                  :next-id (:next-id test-store))
           test-store))
    (let [removed-store
          (remove-link (track-modified-ids added-store) id)]
      (is (= (:modified-ids removed-store) #{id}))
      (is (= (-> removed-store
                 (assoc :next-id (:next-id test-store))
                 (assoc :modified-ids nil))
             test-store)))))

(deftest change-source-test
  (let [[added-store _]
        (add-link test-store (make-link-id 1) 22)
        [different-store id]
        (add-link test-store (make-link-id 1) 22)
        changed-store
        (update-source (track-modified-ids different-store)
                        id "changed")]
    (is (= (:modified-ids changed-store) #{id}))
    (is (= (id->source changed-store id) "changed"))
    ;; Test that adding nil source fails.
    (is (thrown? java.lang.AssertionError
                 (update-source test-store (make-link-id 1) nil)))))

(defn check-derived-indices
  "Check that each of the derived indices of the store matches the data."
  [store]
  ;; Everything in :target->ids is true.
  (doseq [[id elements] (:target->ids store)]
    (doseq [element (pseudo-set-seq elements)]
      (is (= (id->target store element) id))))
  ;; Everything that should be in :target->ids is.
  (doseq [[id target] (:id->target store)]
    (is (some #{id} (target-id->ids store target))))
  
  ;; Everything in :source->ids is true.
  (doseq [[source ids] (:source->ids store)]
    (doseq [id (pseudo-set-seq ids)]
      (is (= (canonical-primitive-form (id->source store id)) source))))
  ;; Everything that should be in :source->ids is.
  (doseq [[id source] (:id->source store)]
    (is (some #{id}
              (pseudo-set-seq
               (get-in store [:source->ids (canonical-primitive-form
                                             source)])))))

  ;; Everything in :id->keywords is true.
  (doseq [[id keywords] (:id->keywords store)]
    (doseq [keyword (pseudo-set-seq keywords)]
      (is (keyword? keyword))
      (is (some #(= (id->source store %) keyword)
                (target-id->ids store id)))))
  ;; Everything that should be in :id->keywords is.
  (doseq [[id source] (:id->source store)]
    (when-let [target (id->target store id)]
      (when (keyword? source)
            (is (some #{source}
                      (pseudo-set-seq
                       (get-in store [:id->keywords target])))))))

  ;; Everything in :target->label->label-ids is true
  (doseq [[id map] (:target->label->label-ids store)]
    (doseq [[label ids] map]
      (doseq [label-id (pseudo-set-seq ids)]
        (and
         (is (some (fn [element]
                     (some #{label-id} (target-id->ids store element))) 
                   (target-id->ids store id)))
         (= (canonical-primitive-form (id->source store label-id)) label)
         (is (or (some #(= (id->source store %) :label)
                       (target-id->ids store label-id))
                 (let [source (id->source store label-id)]
                   (and (keyword? source) (not= source :label)))))))))
  ;; Everything that should be :target->label->label-ids is.
  (doseq [[id source] (:id->source store)]
    (when-let [label-id (cond (= source :label) (id->target store id)
                              (= source :order) id)]
      (let [label (canonical-primitive-form (id->source store label-id))]
        (when-let [two-up (id->target store (id->target store label-id))]
          (is (some #{label-id}
                    (pseudo-set-seq
                     (get-in store [:target->label->label-ids two-up label]))))
          )))))

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
  (let [earlier-num (fn [n] (gen/uniform 1 (+ 1 (int (/ n 2)))))]
    (loop [iteration 0
           store (first (add-link
                         (first (add-link (new-element-store) nil 0))
                         nil 1))
           items 2]
      (let [;; Number of items to end up with (has a long tail)
            n (max (+ items 10) (int (/ 1000 (gen/uniform 1 100))))
            ;; Number of items to keep
            m (gen/uniform (int (/ n 2)) n)]
        (let [added-store
              (reduce (fn [store i]
                        (let [[new-store id]
                              (add-link
                               store
                               (when (not= 0 (gen/uniform 0 10))
                                 (->ItemId (earlier-num i)))
                               (case (gen/uniform 0 3)
                                 0 (int (/ 100 (gen/uniform 1 100)))
                                 1 :label
                                 2 :order))]
                          (assert (= (:id id) i))
                          new-store))
                      store (range (+ items 1) (+ n 1)))
              removed-store
              (reduce (fn [store i]
                        (remove-link store (->ItemId i)))
                      added-store (gen/shuffle (range (+ m 1) (+ n 1))))]
          (check-derived-indices added-store)
          (check-derived-indices removed-store)
          (if (< iteration 20)
            (recur (+ iteration 1)
                   (assoc removed-store :next-id (+ m 1))
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
             [1 [] true]))
  (is (check (candidate-matching-ids-and-estimate test-store '(nil "baz" "bar"))
             [1 [(make-link-id 2)] true]))
  (is (check (candidate-matching-ids-and-estimate test-store '(nil "bar" "bar"))
             [2 [(make-link-id 2) (make-link-id 1)] false]))
  (is (nil? (candidate-matching-ids-and-estimate test-store '(nil))))
  (is (check (candidate-matching-ids test-store nil)
             [(as-set [(make-link-id 0.5) (make-link-id 1)
                       (make-link-id 2) (make-link-id 3)
                       (make-link-id 4) (make-link-id 5)
                       (make-link-id 6) (make-link-id 7)
                       (make-link-id 8) (make-link-id 9)
                       (make-link-id 10)])
              false]))
  (is (check (candidate-matching-ids test-store '(nil nil))
             [(as-set  [(make-link-id 0.5) (make-link-id 1)
                        (make-link-id 2) (make-link-id 3)
                        (make-link-id 5) (make-link-id 9)])
              false]))
  (is (check (candidate-matching-ids test-store '(0))
             [[(make-link-id 0.5)] true]))
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
  (is (= test-store (data-to-store (new-element-store)
                                   (store-to-data test-store))))
  ;; Now try it with some items not serialized
  (let [temporary-store (-> test-store
                            (declare-temporary-id (make-link-id 3))
                            (declare-temporary-id (make-link-id 8)))
        smaller-store (-> test-store
                          (remove-link (make-link-id 8))
                          (remove-link (make-link-id 7))
                          (remove-link (make-link-id 6))
                          (remove-link (make-link-id 3)))]
    (is (= smaller-store
           (data-to-store (new-element-store)
                          (store-to-data temporary-store))))))

(deftest write-read-test
  (let [store (first
               ;; Add an Orderable to the store to check its serialization.
               (add-link test-store
                                (make-link-id 0.5)
                                (first (orderable/split orderable/initial))))
        outstr (java.io.ByteArrayOutputStream.)]
    (write-store store outstr)
    (with-open [instr (java.io.ByteArrayInputStream.
                       (.toByteArray outstr))]
      (let [s (read-store (new-element-store) instr)]
        (is (check (into {} (seq s)) (into {} (seq store))))))))

(deftest get-unique-number-test
  (let [s0 (new-element-store)
        [id1 s1] (get-unique-number s0)
        [id2 s2] (get-unique-number s1)]
    (is (number? id1))
    (is (number? id2))
    (is (not (= id1 id2)))))

(deftest valid-undo-point-test
  (is (not (equivalent-undo-point? test-store)))
  (is (equivalent-undo-point?
       (update-equivalent-undo-point test-store true)))
  (is (not (equivalent-undo-point?
            (update-equivalent-undo-point test-store false)))))


(ns cosheet2.store-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            (cosheet2
             [store :refer :all]
             [store-impl :refer :all]
             [utils :refer [pseudo-set-seq pseudo-set-contains?
                            canonical-atom-form]]
             ;; [entity :refer [to-list description->entity]]
             ;; entity-impl
             [orderable :as orderable]
             [test-utils :refer [check any as-set]])
            ; :reload
            ))

(def unindexed-test-store
  (map->ElementStoreImpl
   {:id->subject
    {(make-id "1") (make-id "0")
     (make-id "2") (make-id "1")
     (make-id "3") (make-id "2")
     (make-id "5") (make-id "2")
     (make-id "6") (make-id "3")
     (make-id "7") (make-id "3")
     (make-id "8") (make-id "5")
     (make-id "9") (make-id "1")
     (make-id "10") (make-id "9")}
    :id->content-data
    {(make-id "0") 0
     (make-id "1") (make-id "4")
     (make-id "2") "Foo"
     (make-id "3") "Baz"
     (make-id "4") 5
     (make-id "5") "bar"
     (make-id "6") :baz
     (make-id "7") :label
     (make-id "8") :label
     (make-id "9") "Bar"
     (make-id "10") :order}
    :temporary-ids
    #{}
    :next-id
    8
    :modified-ids
    nil
    :equivalent-undo-point
    false}))

(def empty-store (new-element-store))

(defn clear-store-leaving-indices
  [store]
  (assoc store
         :id->subject {}
         :id->content-data {}))

(deftest id<->string-test
  (let [id (make-id "a")]
    (is (= (-> id id->string string->id) id)))
  (let [id (->ItemId 3)]
    (is (= (-> id id->string string->id) id))))

(deftest stored-item-description-name-test
  (is (= (stored-item-description-name (make-id "a")) "Id-Ia"))
  (is (= (stored-item-description-name (->ItemId 1)) "Id-1")))

(deftest index-id->elements-test
  (let [ids (keys (:id->content-data unindexed-test-store))
        store (reduce #(index-id->elements %1 empty-store %2)
                      unindexed-test-store ids)
        empty-indexed (clear-store-leaving-indices store)
        unindexed (reduce #(index-id->elements %1 store %2) empty-indexed ids)]
    (is (= (reduce (fn [accum elements]
                     (+ accum (count (pseudo-set-seq elements))))
                   0 (vals (:id->elements store)))
           (count (:id->subject unindexed-test-store))))
    (doseq [id (keys (:id->subject unindexed-test-store))]
      (let [subject (get-in store [:id->subject id])]
        (is (pseudo-set-contains? (get-in store [:id->elements subject])
                                  id))))
    (is (empty? (:id->elements unindexed)))))

(deftest index-content->ids-test
  (let [ids (keys (:id->content-data unindexed-test-store))
        store (reduce #(index-content->ids %1 empty-store %2)
                      unindexed-test-store ids)
        empty-indexed (clear-store-leaving-indices store)
        unindexed (reduce #(index-content->ids %1 store %2) empty-indexed ids)]
    (is (= (reduce (fn [accum elements]
                     (+ accum (count (pseudo-set-seq elements))))
                   0 (vals (:content->ids store)))
           (count ids) ))
    (doseq [id ids]
      (let [content (get-in store [:id->content-data id])]
        (when (not (nil? content))
          (is (pseudo-set-contains?
               (get-in store [:content->ids (canonical-atom-form content)])
               id)))))
    (is (empty? (:content->ids unindexed)))))

(deftest index-id->keywords-test
  (let [ids (keys (:id->content-data unindexed-test-store))
        elements-indexed (reduce #(index-id->elements %1 empty-store %2)
                                 unindexed-test-store ids)
        store (reduce #(index-id->keywords %1 empty-store %2)
                      elements-indexed ids)
        empty-indexed (clear-store-leaving-indices store)
        unindexed (reduce #(index-id->keywords %1 store %2) empty-indexed ids)]
    (is (check (:id->keywords store)
               {(make-id "3") #{:label :baz}
                (make-id "5") :label
                (make-id "9") :order}))
    (is (empty? (:id->keywords unindexed)))))

(deftest index-id->label->ids-test
  (let [ids (keys (:id->content-data unindexed-test-store))
        elements-indexed (reduce #(index-id->elements %1 empty-store %2)
                                 unindexed-test-store ids)
        keywords-indexed (reduce #(index-id->keywords %1 empty-store %2)
                      elements-indexed ids)
        store (reduce #(index-id->label->ids %1 empty-store %2)
                      keywords-indexed ids)
        empty-indexed (clear-store-leaving-indices store)
        unindexed (reduce #(index-id->label->ids %1 store %2)
                          empty-indexed ids)]
    (is (check (:id->label->ids store)
               {(make-id "1") {"baz" (make-id "3")
                               "bar" (make-id "5")
                               :order (make-id "10")}
                (make-id "2") {:baz (make-id "6")}}))
    (is (empty? (:content->ids unindexed)))))

(def test-store
  (let [ids (keys (:id->content-data unindexed-test-store))]
    (as-> unindexed-test-store store
      (reduce #(index-id->elements %1 empty-store %2) store ids)
      (reduce #(index-content->ids %1 empty-store %2) store ids)
      (reduce #(index-id->keywords %1 empty-store %2) store ids)
      (reduce #(index-id->label->ids %1 empty-store %2) store ids))))

(deftest all-X-test
   (is (= (set (all-ids-eventually-holding-content test-store 5))
          #{(make-id "1") (make-id "4")}))
  (is (= (set (all-ids-eventually-holding-id test-store (make-id "4")))
         #{(make-id "1") (make-id "4")}))
  (is (= (set (all-forward-reachable-ids test-store (make-id "1")))
          #{(make-id "0") (make-id "1") (make-id "4")})))

(deftest id-valid?-test
  (is (id-valid? test-store (make-id "1")))
  (is (not (id-valid? test-store (make-id "wrong")))))

(deftest id->content-test
  (is (= (id->content test-store (make-id "???")) nil))
  (is (= (id->content test-store (make-id "1")) (make-id "4")))
  (is (= (id->content test-store (make-id "2")) "Foo"))
  (is (= (id->content test-store (make-id "6")) :baz)))

(deftest id->element-ids-test
  (is (= (id->element-ids test-store (make-id "0")) [(make-id "1")]))
  (is (= (set (id->element-ids test-store (make-id "1")))
         (set [(make-id "2") (make-id "9")])))
  (is (= (id->element-ids test-store (make-id "wrong")) nil)))

(deftest id-label->element-ids-test
  (is (= (id-label->element-ids test-store (make-id "1") "Bar")
         [(make-id "2")]))
  (is (= (id-label->element-ids test-store (make-id "1") "Baz")
         [(make-id "2")]))
  (is (= (id-label->element-ids test-store (make-id "0") "bar") nil))
  (is (= (id-label->element-ids test-store (make-id "wrong") "bar") nil))
  (is (= (id-label->element-ids test-store (make-id "1") :order)
         [(make-id "9")]))
  (is (= (id-label->element-ids test-store (make-id "0") :order)
         nil)))

(deftest id->has-keyword?-test
  (is (id->has-keyword? test-store (make-id "3") :baz))
  (is (id->has-keyword? test-store (make-id "3") :label))
  (is (not (id->has-keyword? test-store (make-id "3") :bar)))
  (is (not (id->has-keyword? test-store (make-id "2") :baz))))

(deftest id->containing-ids-test
  (is (= (id->containing-ids test-store (make-id "4")) #{(make-id "1")}))
  (is (= (id->containing-ids test-store (make-id "1")) #{}))
  (is (thrown? java.lang.AssertionError
               (id->containing-ids test-store "Foo"))))

(deftest id->subject-test
   (is (= (id->subject test-store (make-id "2")) (make-id "1")))
   (is (= (id->subject test-store 2) nil)))

(deftest add-simple-item-test
  (let [[added-store id]
        (add-simple-item test-store (make-id "1") "test")]
    (is (= (:id id) (:next-id test-store)))
    (is (= (id->content added-store id) "test"))
    (is (= (id->subject added-store id) (make-id "1"))))
  ;; Test that adding nil content fails.
  (is (thrown? java.lang.AssertionError
               (add-simple-item test-store (make-id "1") nil)))
  (let [[added-store id]
        (add-simple-item
         (track-modified-ids test-store) (make-id "1") "test")]
    (is (= (:modified-ids added-store) #{id}))))

(deftest remove-simple-item-test
  (let [[added-store id]
        (add-simple-item test-store (make-id "1") (make-id "2"))]
    (is (= (assoc (remove-simple-item added-store id)
                  :next-id (:next-id test-store))
           test-store))
    (let [removed-store
          (remove-simple-item (track-modified-ids added-store) id)]
      (is (= (:modified-ids removed-store) #{id}))
      (is (= (-> removed-store
                 (assoc :next-id (:next-id test-store))
                 (assoc :modified-ids nil))
             test-store)))))

(deftest change-content-test
  (let [[added-store _]
        (add-simple-item test-store (make-id "1") (make-id "2"))
        [different-store id]
        (add-simple-item test-store (make-id "1") (make-id "3"))
        changed-store
        (update-content (track-modified-ids different-store)
                        id (make-id "2"))]
    (is (= changed-store (assoc added-store :modified-ids #{id})))
    ;; Test that adding nil content fails.
    (is (thrown? java.lang.AssertionError
                 (update-content test-store (make-id "1") nil)))
    ;; Test that content that would create forward cycles fails.
    (is (thrown? java.lang.AssertionError
                 (update-content test-store (make-id "4") (make-id "9"))))
    (is (thrown? java.lang.AssertionError
                 (update-content test-store (make-id "0") (make-id "8"))))
    ;; Test that the non-cycle content doesn't fail.
    (update-content test-store (make-id "9") (make-id "4"))
    (update-content test-store (make-id "8") (make-id "0"))))

(defn check-derived-indices
  "Check that each of the derived indices of the store matches the data."
  [store]
  ;; Everything in :id->elements is true.
  (doseq [[id elements] (:id->elements store)]
    (doseq [element (pseudo-set-seq elements)]
      (is (= (id->subject store element) id))))
  ;; Everything that should be in :id->element is.
  (doseq [[id subject] (:id->subject store)]
    (is (some #{id} (id->element-ids store subject))))
  
  ;; Everything in :content->ids is true.
  (doseq [[content ids] (:content->ids store)]
    (doseq [id (pseudo-set-seq ids)]
      (is (= (canonical-atom-form (id->content store id)) content))))
  ;; Everything that should be in :content->ids is.
  (doseq [[id content] (:id->content-data store)]
    (is (some #{id}
              (pseudo-set-seq
               (get-in store [:content->ids (canonical-atom-form content)])))))

  ;; Everything in :id->keywords is true.
  (doseq [[id keywords] (:id->keywords store)]
    (doseq [keyword (pseudo-set-seq keywords)]
      (is (keyword? keyword))
      (is (some #(= (id->content store %) keyword)
                (id->element-ids store id)))))
  ;; Everything that should be in :id->keywords is.
  (doseq [[id content] (:id->content-data store)]
    (when-let [subject (id->subject store id)]
      (when (keyword? content)
            (is (some #{content}
                      (pseudo-set-seq
                       (get-in store [:id->keywords subject])))))))

  ;; Everything in :id->label->ids is true
  (doseq [[id map] (:id->label->ids store)]
    (doseq [[label ids] map]
      (doseq [label-id (pseudo-set-seq ids)]
        (and
         (is (some (fn [element]
                     (some #{label-id} (id->element-ids store element))) 
                   (id->element-ids store id)))
         (= (canonical-atom-form (id->content store label-id)) label)
         (is (or (some #(= (id->content store %) :label)
                       (id->element-ids store label-id))
                 (let [content (id->content store label-id)]
                   (and (keyword? content) (not= content :label)))))))))
  ;; Everything that should be :id->label->ids is.
  (doseq [[id content] (:id->content-data store)]
    (when-let [label-id (cond (= content :label) (id->subject store id)
                              (= content :order) id)]
      (let [label (canonical-atom-form (id->content store label-id))]
        (when-let [two-up (id->subject store (id->subject store label-id))]
          (is (some #{label-id}
                    (pseudo-set-seq
                     (get-in store [:id->label->ids two-up label])))))))))

(deftest all-indices-test
  (check-derived-indices test-store))

(require '[clojure.data.generators :as gen])

(deftest lots-of-changes-indices-test
  ;; We repeatedly add a bunch of elements and remove a bunch of
  ;; elements and check that the derived indices are correct. To make
  ;; sure that the removals are legal, an element only references
  ;; elements with at most 1/2 its id number, and we only remove at
  ;; most the last 1/2 of the elements while removing. (We remove them
  ;; in a random order.
  (binding [gen/*rnd* (java.util.Random. 437)]
    (let [earlier-num (fn [n] (gen/uniform 0 (+ 1 (int (/ n 2)))))]
      (loop [iteration 0
             store (new-element-store)
             items 0]
        (let [n (max (+ items 10) (int (/ 1000 (gen/uniform 1 100))))
              m (min (gen/uniform (int (/ n 2)) n) (- n 1))]
          (let [added-store
                (reduce (fn [store i]
                          (first (add-simple-item
                                  store
                                  (when (not= 0 (earlier-num i))
                                    (->ItemId (earlier-num i)))
                                  (case (gen/uniform 0 4)
                                    0 (->ItemId (earlier-num i))
                                    1 (int (/ 100 (gen/uniform 1 100)))
                                    2 :label
                                    3 :order))))
                        store (range items n))
                removed-store
                (reduce (fn [store i]
                          (remove-simple-item store (->ItemId i)))
                        added-store (gen/shuffle (range m n)))]
            (check-derived-indices added-store)
            (check-derived-indices removed-store)
            (if (< iteration 20)
              (recur (+ iteration 1)
                     (assoc removed-store :next-id m)
                     m))
            ))))))

(deftest candidate-matching-ids-test
  (is (check (candidate-matching-ids-and-estimate test-store 5)
             [2 (as-set [(make-id "1") (make-id "4")]) true]))
  (is (check (candidate-matching-ids-and-estimate test-store '(5))
             [2 (as-set [(make-id "1") (make-id "4")]) true]))
  (is (check (candidate-matching-ids-and-estimate test-store '(nil "Foo"))
             [1 [(make-id "1")] true]))
  (is (check (candidate-matching-ids-and-estimate test-store '(5 "foo"))
             [1 [(make-id "1")] true]))
  (is (check (candidate-matching-ids-and-estimate test-store '(0 "Foo"))
             [1 [] true]))
  (is (check (candidate-matching-ids-and-estimate test-store '(nil "baz" "bar"))
             [1 [(make-id "2")] true]))
  (is (check (candidate-matching-ids-and-estimate test-store '(nil "bar" "bar"))
             [2 [(make-id "2") (make-id "1")] false]))
  (is (nil? (candidate-matching-ids-and-estimate test-store '(nil))))
  (is (check (candidate-matching-ids test-store nil)
             [(as-set [(make-id "0") (make-id "1") (make-id "2") (make-id "3")
                        (make-id "4") (make-id "5") (make-id "6") (make-id "7")
                       (make-id "8") (make-id "9") (make-id "10")])
              false]))
  (is (check (candidate-matching-ids test-store '(nil nil))
             [(as-set  [(make-id "0") (make-id "1") (make-id "2") (make-id "3")
                        (make-id "5") (make-id "9")])
              false]))
  (is (check (candidate-matching-ids test-store '(0))
             [[(make-id "0")] true]))
  (is (check (candidate-matching-ids test-store 5)
             [(as-set [(make-id "1") (make-id "4")]) true]))
    (is (check (candidate-matching-ids test-store '(nil "Foo" nil))
             [[(make-id "1")] false]))
  (is (check (candidate-matching-ids test-store '(5 nil))
             [(as-set [(make-id "1") (make-id "4")]) false])))

(deftest declare-temporary-id-test
  (is (= (:temporary-ids test-store) #{}))
  (let [temporary-store (-> test-store
                            (declare-temporary-id (make-id "3"))
                            (add-simple-item (make-id "1") "hi")
                            first
                            (declare-temporary-id (make-id "8")))]
    (is (= (:temporary-ids temporary-store)
           #{(make-id "3") (make-id "8")}))
    (is (= (all-temporary-ids temporary-store)
           #{(make-id "3") (make-id "6") (make-id "7")
             (make-id "8")}))))

(deftest new-element-store-test
  (let [store (new-element-store)]
    (is (= (candidate-matching-ids store nil) [nil false]))))

(deftest store-to-data-to-store-test
  (is (= test-store (data-to-store (new-element-store)
                                   (store-to-data test-store))))
  ;; Now try it with some items not serialized
  (let [temporary-store (-> test-store
                            (declare-temporary-id (make-id "3"))
                            (declare-temporary-id (make-id "8")))
        smaller-store (-> test-store
                          (remove-simple-item (make-id "8"))
                          (remove-simple-item (make-id "7"))
                          (remove-simple-item (make-id "6"))
                          (remove-simple-item (make-id "3")))]
    (is (= smaller-store
           (data-to-store (new-element-store)
                          (store-to-data temporary-store))))))

(deftest write-read-test
  (let [store (first
               ;; Add an Orderable to the store to check its serialization.
               (add-simple-item test-store
                                (make-id "0")
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


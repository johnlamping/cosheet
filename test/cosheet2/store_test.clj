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
     (make-id "9") (make-id "1")}
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
     (make-id "9") "Bar"}
    :temporary-ids
    #{}
    :next-id
    8
    :modified-ids
    nil
    :equivalent-undo-point
    false}))

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
         (is (some #(= (id->content store %) :label)
                   (id->element-ids store label-id)))))))
  ;; Everything that should be :id->label->ids is.
  (doseq [[id content] (:id->content-data store)]
    (when (= content :label)
      (when-let [subject (id->subject store id)]
        (let [label (canonical-atom-form (id->content store subject))]
          (when-let [two-up (id->subject store (id->subject store subject))]
            (is (some #{subject}
                      (pseudo-set-seq
                       (get-in store [:id->label->ids two-up label]))))))))))

(deftest id<->string-test
  (let [id (make-id "a")]
    (is (= (-> id id->string string->id) id)))
  (let [id (->ItemId 3)]
    (is (= (-> id id->string string->id) id))))

(deftest stored-item-description-name-test
  (is (= (stored-item-description-name (make-id "a")) "Id-Ia"))
  (is (= (stored-item-description-name (->ItemId 1)) "Id-1")))

(deftest id->elements-test
  (let [ids (keys (:id->subject unindexed-test-store))
        store (reduce #(index-id->elements %1 %2 true) unindexed-test-store ids)
        unindexed (reduce #(index-id->elements %1 %2 false) store ids)]
    (is (= (reduce (fn [accum elements]
                     (+ accum (count (pseudo-set-seq elements))))
                   0 (vals (:id->elements store)))
           (count ids)))
    (doseq [id ids]
      (let [subject (get-in store [:id->subject id])]
        (is (pseudo-set-contains? (get-in store [:id->elements subject])
                                  id))))
    (is (empty? (:id->elements unindexed)))))

(deftest content->ids-test
  (let [ids (keys (:id->content-data unindexed-test-store))
        store (reduce #(index-content->ids %1 %2 true) unindexed-test-store ids)
        unindexed (reduce #(index-content->ids %1 %2 false) store ids)]
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
        store (reduce #(index-id->keywords %1 %2 true) unindexed-test-store ids)
        unindexed (reduce #(index-id->keywords %1 %2 false) store ids)]
    (is (check (:id->keywords store)
               {(make-id "3") #{:label :baz}
                (make-id "5") :label}))
    (is (empty? (:content->ids unindexed)))))

(deftest index-id->label->ids-test
  (let [ids (keys (:id->content-data unindexed-test-store))
        keywords-indexed (reduce #(index-id->keywords %1 %2 true)
                                 unindexed-test-store ids)
        store (reduce #(index-id->label->ids %1 %2 true) keywords-indexed ids)
        unindexed (reduce #(index-id->label->ids %1 %2 false) store ids)]
    (is (check (:id->label->ids store)
               {(make-id "1") {"baz" (make-id "3")
                               "bar" (make-id "5")}}))
    (is (empty? (:content->ids unindexed)))))

(def test-store
  (let [ids (keys (:id->content-data unindexed-test-store))]
    (as-> unindexed-test-store store
         (reduce #(index-id->elements %1 %2 true) store ids)
         (reduce #(index-content->ids %1 %2 true) store ids)
         (reduce #(index-id->keywords %1 %2 true) store ids)
         (reduce #(index-id->label->ids %1 %2 true) store ids))))

(deftest all-indices-check
  (check-derived-indices test-store))

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

(deftest id-label->element-ids-test
  (is (= (id-label->element-ids test-store (make-id "1") "Bar")
         [(make-id "2")]))
  (is (= (id-label->element-ids test-store (make-id "1") "Baz")
         [(make-id "2")]))
  (is (= (id-label->element-ids test-store (make-id "0") "bar") nil))
  (is (= (id-label->element-ids test-store (make-id "wrong") "bar") nil)))

(deftest id->element-ids-test
  (is (= (id->element-ids test-store (make-id "0")) [(make-id "1")]))
  (is (= (set (id->element-ids test-store (make-id "1")))
         (set [(make-id "2") (make-id "9")])))
  (is (= (id->element-ids test-store (make-id "wrong")) nil)))

(deftest id->content-test
  (is (= (id->content test-store (make-id "???")) nil))
  (is (= (id->content test-store (make-id "1")) (make-id "4")))
  (is (= (id->content test-store (make-id "2")) "Foo"))
  (is (= (id->content test-store (make-id "6")) :baz)))

(deftest id->subject-test
   (is (= (id->subject test-store (make-id "2")) (make-id "1")))
   (is (= (id->subject test-store 2) nil)))

(deftest atomic-value-test
  (is (= (atomic-value test-store (make-id "???")) nil))
  (is (= (atomic-value test-store (make-id "6")) :baz))
  (is (= (atomic-value test-store (make-id "1")) 5)))

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

(deftest candidate-matching-ids-test
  (is (check (candidate-matching-ids-and-estimate test-store 5)
             [2 (as-set [(make-id "1") (make-id "4")])]))
  (is (check (candidate-matching-ids-and-estimate test-store '(5))
             [2 (as-set [(make-id "1") (make-id "4")])]))
  (is (check (candidate-matching-ids-and-estimate test-store '(nil "Foo"))
             [1 [(make-id "1")]]))
  (is (check (candidate-matching-ids-and-estimate test-store '(5 "foo"))
             [1 [(make-id "1")]]))
  (is (check (candidate-matching-ids-and-estimate test-store '(0 "Foo"))
             [1 []]))
  (is (check (candidate-matching-ids-and-estimate
              test-store '(nil "baz" "bar"))
             [1 [(make-id "2")]]))
  (is (nil? (candidate-matching-ids-and-estimate test-store '(nil))))
  (is (check (candidate-matching-ids test-store nil)
             (as-set [(make-id "0") (make-id "1") (make-id "2") (make-id "3")
                      (make-id "4") (make-id "5") (make-id "6") (make-id "7")
                      (make-id "8") (make-id "9")])))
  (is (check (candidate-matching-ids test-store '(nil nil))
             (as-set  [(make-id "0") (make-id "1") (make-id "2") (make-id "3")
                       (make-id "5")])))
  (is (check (candidate-matching-ids test-store '(0))
             [(make-id "0")]))
  (is (check (candidate-matching-ids test-store 5)
             (as-set [(make-id "1") (make-id "4")]))))

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
    (is (= (candidate-matching-ids store nil) nil))))

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


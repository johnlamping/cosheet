(ns cosheet.store-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            (cosheet
             [store :refer :all]
             [store-impl :refer :all]
             [entity :refer [to-list description->entity]]
             entity-impl
             [orderable :as orderable]
             [test-utils :refer [check any as-set]])
            ; :reload
            ))

(def test-store
  (->ElementStoreImpl
   ;; id->subject
   {(make-id "1") (make-id "0")
    (make-id "2") (make-id "1")
    (make-id "3") (make-id "2")
    (make-id "5") (make-id "4")
    (make-id "7") (make-id "1")
    (make-id "8") (make-id "3")}
   ;; id->content
   {(make-id "0") 0
    (make-id "1") (make-id "4")
    (make-id "2") "Foo"
    (make-id "3") :label
    (make-id "4") (make-id "6")
    (make-id "5") "bar"
    (make-id "6") 5
    (make-id "7") "second"
    (make-id "8") "Bar"}
   ;; content->ids
   {0 (make-id "0")
    (make-id "4") (make-id "1")
    "foo" (make-id "2")
    :label (make-id "3")
    (make-id "6") (make-id "4")
    "bar" #{(make-id "5") (make-id "8")}
    5 (make-id "6")
    "second" (make-id "7")}
   ;; temporary-ids
   #{}
   ;; subject->label->ids
   {(make-id "0") {"foo" [(make-id "1")] "second" [(make-id "1")]}
    (make-id "1") {:label [(make-id "2")] nil [(make-id "7")]}
    (make-id "2") {"bar" [(make-id "3")]}
    (make-id "3") {nil [(make-id "8")]}
    (make-id "4") {nil [(make-id "5")]}}
   ;; next-id
   8
   ;; modified-ids
   nil))

(def test-implicit-content (->ImplicitContentId (make-id "2")))

(deftest id-string-test
  (is (= (stored-item-id-string (make-id "a")) "a"))
  (is (= (stored-item-description-name (make-id "a")) "Id-a"))
  (is (= (stored-item-id-string test-implicit-content) "content-2"))
  (is (= (stored-item-description-name test-implicit-content) "content-Id-2")))

(deftest id<->string-test
  (let [id (make-id "a")]
    (is (= (-> id id->string string->id) id)))
  (let [id (->ItemId 3)]
    (is (= (-> id id->string string->id) id))))

(deftest id-valid?-test
  (is (id-valid? test-store (make-id "1")))
  (is (not (id-valid? test-store (make-id "wrong")))))

(deftest id-label->element-ids-test
  (is (= (id-label->element-ids test-store (make-id "0") "foo")
         [(make-id "1")]))
    (is (= (id-label->element-ids test-store (make-id "0") "Foo")
         [(make-id "1")]))
  (is (= (id-label->element-ids test-store (make-id "1") nil)
         [(make-id "7")]))
  (is (= (id-label->element-ids test-store (make-id "0") "bar") nil))
  (is (= (id-label->element-ids test-store (make-id "wrong") "bar") nil)))

(deftest id->element-ids-test
  (is (= (id->element-ids test-store (make-id "0")) [(make-id "1")]))
  (is (= (set (id->element-ids test-store (make-id "1")))
         (set [(make-id "2") (make-id "7")])))
  (is (= (id->element-ids test-store (make-id "wrong")) nil)))

(deftest id->content-test
  (is (= (id->content test-store (make-id "???")) nil))
  (is (= (id->content test-store (make-id "1")) (make-id "4")))
  (is (= (id->content test-store (make-id "2")) "Foo"))
  (is (= (id->content test-store (make-id "6")) 5))
  (is (= (id->content test-store (->ImplicitContentId (make-id "2"))) "Foo"))
  (is (= (id->content test-store (->ImplicitContentId (make-id "1")))
         (make-id "6")))
  (is (= (id->content test-store (->ImplicitContentId
                                  (->ImplicitContentId (make-id "1"))))
         5)))

(deftest id->content-reference-test
  (is (= (id->content-reference test-store (make-id "1")) (make-id "4")))
  (is (= (id->content-reference test-store (make-id "2"))
         (->ImplicitContentId (make-id "2"))))
  (is (= (id->content-reference test-store
                                (->ImplicitContentId (make-id "2")))
         (->ImplicitContentId (->ImplicitContentId (make-id "2"))))))

(deftest id->subject-test
  (is (= (id->subject test-store (make-id "2")) (make-id "1")))
  (is (= (id->subject test-store 2) nil)))

(deftest ensure-in-vector-test
  (is (= (ensure-in-vector nil 1) [1]))
  (is (= (ensure-in-vector [1] 2) [1 2]))
  (is (= (ensure-in-vector [1 2] 2) [1 2])))

(deftest remove-from-vector-test
  (is (= (remove-from-vector [1 2] 1) [2]))
  (is (= (remove-from-vector [1 2] 3) [1 2]))
  (is (= (remove-from-vector [1] 1) nil))
  (is (= (remove-from-vector nil 1) nil)))

(deftest psuedo-set-test
  (let [ps (loop [ps nil in [] out [1 2 3 4 5]]
             (is (= (set (psuedo-set-seq ps)) (set in)))
             (doseq [x in] (is (psuedo-set-contains? ps x)))
             (doseq [x out] (is (not (psuedo-set-contains? ps x))))
             (cond (empty? in)
                   (is (nil? ps))
                   (= (count in) 1)
                   (is (= ps (first in))))
             (if (not (empty? out))
               (recur (psuedo-set-conj ps (first out))
                      (conj in (first out))
                      (rest out))
               ps))]
    (loop [ps ps in [1 2 3 4 5] out []]
      (is (= (set (psuedo-set-seq ps)) (set in)))
      (doseq [x in] (is (psuedo-set-contains? ps x)))
      (doseq [x out] (is (not (psuedo-set-contains? ps x))))
      (cond (empty? in)
            (is (nil? ps))
            (= (count in) 1)
            (is (= ps (first in))))
      (when (not (empty? in))
        (recur (psuedo-set-disj ps (first in))
               (rest in)
               (conj out (first in)))))))

(deftest id-is-content?-test
  (is (id-is-content? test-store (make-id "4") nil))
  (is (not (id-is-content? test-store (make-id "4") [(make-id "1")])))
  (is (not (id-is-content? test-store (make-id "3") nil))))

(deftest atomic-value-test
  (is (= (atomic-value test-store (make-id "???")) nil))
  (is (= (atomic-value test-store (make-id "6")) 5))
  (is (= (atomic-value test-store (make-id "1")) 5)))

(deftest index-subject-in-subject-test
  (let [missing-store
        (assoc test-store :subject->label->ids {})
        indexed-store
        (reduce (fn [store id]
                  (index-in-subject (index-subject store id) id))
                missing-store
                (sort-by :id (keys (:id->content-map missing-store))))]
    (is (= indexed-store test-store))))

(deftest index-content-test
  (let [missing-store (assoc test-store :content->ids {})
        indexed-store
        (reduce (fn [store id]
                  (index-content store id))
                missing-store (keys (:id->content-map test-store)))]
    (is (= indexed-store test-store))))

(deftest deindex-subject-test
  (let [id (make-id "999")
        s1 (-> test-store
               (assoc-in [:id->content-map id] :test)
               (assoc-in [:id->subject id] (make-id "2")))
        s2 (-> test-store
               (assoc-in [:id->content-map id] :foo)
               (assoc-in [:id->subject id] (make-id "2")))
        s3 (assoc-in test-store [:id->content id] :test)]
    (is (= (deindex-subject (index-subject s1 id) id) s1))
    (is (= (deindex-subject (index-subject s2 id) id) s2))
    (is (= (deindex-subject (index-subject s3 id) id) s3))))

(deftest deindex-content-test
  (let [id (make-id "999")
        s1 (-> test-store
               (assoc-in [:id->content-map id] :test)
               (assoc-in [:id->subject id] (make-id "2")))
        s2 (-> test-store
               (assoc-in [:id->content-map id] (make-id "4"))
               (assoc-in [:id->subject id] (make-id "2")))
        s3 (-> test-store
               (assoc-in [:id->content-map id] (make-id "5"))
               (assoc-in [:id->subject id] (make-id "2")))]
    (is (= (deindex-content (index-content s1 id) id) s1))
    (is (= (deindex-content (index-content s2 id) id) s2))
    (is (= (deindex-content (index-content s3 id) id) s3))))

(deftest promote-implicit-item-test
  (is (= (promote-implicit-item test-store (make-id "1"))
         [test-store (make-id "1")]))
  (is (= (promote-implicit-item
          test-store (->ImplicitContentId (make-id "1")))
         [test-store (make-id "4")]))
  (let [[promoted-store promoted-id]
        (promote-implicit-item
         test-store (->ImplicitContentId (make-id "2")))]
    (is (= (:next-id promoted-store) (+ 1 (:next-id test-store))))
    (is (= (:subject->label->ids promoted-store)
           (:subject->label->ids test-store)))
    (is (= (get-in promoted-store [:id->content-map (make-id "2")])
           promoted-id))
    (is (= (get-in promoted-store [:id->content-map promoted-id])
           (get-in test-store [:id->content-map (make-id "2")])))
    (is (= (get-in promoted-store [:id->subject promoted-id])
           nil))
    (is (= (get-in promoted-store [:content->ids promoted-id])
           (make-id "2")))
    (is (= (get-in promoted-store [:id->subject (make-id "2")])
           (get-in test-store [:id->subject (make-id "2")]))))
  (let [[promoted-store promoted-id]
        (promote-implicit-item
         (track-modified-ids test-store) (->ImplicitContentId (make-id "2")))]
    (is (= (:modified-ids promoted-store) #{promoted-id (make-id "2")}))
    (is (= (fetch-and-clear-modified-ids promoted-store)
           [(assoc promoted-store :modified-ids #{})
            #{promoted-id (make-id "2")}]))))

(deftest add-simple-element-test
  (let [[added-store element]
        (add-simple-element test-store (make-id "1") "test")]
    (is (= (:id element) (:next-id test-store)))
    (is (= (set (get-in added-store [:subject->label->ids (make-id "1") nil]))
           #{(make-id "7") element}))
    (is (= (get-in added-store [:id->content-map element]) "test"))
    (is (= (get-in added-store [:id->subject element]) (make-id "1"))))
  (let [[added-store element]
        (add-simple-element
         (track-modified-ids test-store) (make-id "1") "test")]
    (is (= (:modified-ids added-store) #{element}))))

(deftest remove-simple-id-test
  (let [[added-store element]
        (add-simple-element test-store (make-id "1") (make-id "2"))]
    (is (= (assoc (remove-simple-id added-store element)
                  :next-id (:next-id test-store))
           test-store))
    (let [removed-store
          (remove-simple-id (track-modified-ids added-store) element)]
      (is (= (:modified-ids removed-store) #{element}))
      (is (= (-> removed-store
                 (assoc :next-id (:next-id test-store))
                 (assoc :modified-ids nil))
           test-store)))))

(deftest change-content-test
  (let [[added-store _]
        (add-simple-element test-store (make-id "1") (make-id "2"))
        [different-store element]
        (add-simple-element test-store (make-id "1") (make-id "3"))
        changed-store
        (update-content (track-modified-ids different-store)
                        element (make-id "2"))]
    (is (= changed-store (assoc added-store :modified-ids #{element})))))

(deftest eventually-containing-items-test
  (is (= (set (eventually-containing-items test-store 5))
         #{(make-id "1") (make-id "4") (make-id "6")})))

(deftest candidate-matching-ids-test
  (is (check (candidate-matching-ids-and-estimate test-store 5)
             [3 (as-set [(make-id "1") (make-id "4") (make-id "6")])]))
  (is (check (candidate-matching-ids-and-estimate test-store '(5))
             [3 (as-set [(make-id "1") (make-id "4") (make-id "6")])]))
  (is (check (candidate-matching-ids-and-estimate test-store '(nil "foo"))
             [1 [(make-id "1")]]))
  (is (check (candidate-matching-ids-and-estimate test-store '(5 "foo"))
             [1 [(make-id "1")]]))
  (is (check (candidate-matching-ids-and-estimate test-store '(0 "foo"))
             [1 []]))
  (is (check (candidate-matching-ids-and-estimate
              test-store '(nil "foo" "second"))
             [1 [(make-id "1")]]))
  (is (nil? (candidate-matching-ids-and-estimate test-store '(nil))))
  (is (check (candidate-matching-ids test-store nil)
             (as-set [(make-id "0") (make-id "1") (make-id "2") (make-id "3")
                      (make-id "4") (make-id "5") (make-id "6") (make-id "7")
                      (make-id "8")])))
  (is (check (candidate-matching-ids test-store '(nil nil))
             (as-set  [(make-id "0") (make-id "1") (make-id "2") (make-id "3")
                       (make-id "4")])))
  (is (check (candidate-matching-ids test-store '(0))
             [(make-id "0")]))
  (is (check (candidate-matching-ids test-store 5)
             (as-set [(make-id "1") (make-id "4") (make-id "6")]))))

(deftest declare-temporary-id-test
  (is (= (:temporary-ids test-store) #{}))
  (let [temporary-store (-> test-store
                            (declare-temporary-id (make-id "2"))
                            (add-simple-element (make-id "1") "hi")
                            first
                            (declare-temporary-id (make-id "7")))]
    (is (= (:temporary-ids temporary-store)
           #{(make-id "2") (make-id "7")}))
    (is (= (all-temporary-ids temporary-store)
           #{(make-id "2") (make-id "7")
             (make-id "3") (make-id "8")}))))

(deftest new-element-store-test
  (let [store (new-element-store)]
    (is (= (candidate-matching-ids store nil) nil))))

(deftest serialization-test
  (is (= test-store (data-to-store (new-element-store)
                                   (store-to-data test-store))))
  ;; Now try it with some items not serialized
  (let [temporary-store (-> test-store
                            (declare-temporary-id (make-id "2"))
                            (declare-temporary-id (make-id "7")))
        smaller-store (-> test-store
                          (remove-simple-id (make-id "8"))
                          (remove-simple-id (make-id "3"))
                          (remove-simple-id (make-id "2"))
                          (remove-simple-id (make-id "7")))]
    (is (= smaller-store
           (data-to-store (new-element-store)
                          (store-to-data temporary-store))))))

(deftest get-unique-id-number-test
  (let [s0 (new-element-store)
        [id1 s1] (get-unique-id-number s0)
        [id2 s2] (get-unique-id-number s1)]
    (is (number? id1))
    (is (number? id2))
    (is (not (= id1 id2)))))

(deftest write-read-test
  (let [store (first
               ;; Add an Orderable to the store to check its serialization.
               (add-simple-element test-store
                                   (make-id "0")
                                   (first (orderable/split orderable/initial))))
        outstr (java.io.ByteArrayOutputStream.)]
    (write-store store outstr)
    (with-open [instr (java.io.ByteArrayInputStream.
                       (.toByteArray outstr))]
      (let [s (read-store (new-element-store) instr)]
        (is (check (into {} (seq s)) (into {} (seq store))))))))



(ns cosheet.store-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [cosheet.store :refer :all]
            [cosheet.entity :refer [to-list description->entity]]
            cosheet.entity-impl
            [cosheet.store-impl :refer :all]
            ; :reload
            ))

(def test-store
  (->ElementStoreImpl
   {(make-id "1") {:subject (make-id "0") :content (make-id "4")}
    (make-id "2") {:subject (make-id "1") :content "foo"}
    (make-id "3") {:subject (make-id "2") :content :label}
    (make-id "4") {:containers #{(make-id "1")} :content (make-id "6")}
    (make-id "5") {:subject (make-id "4") :content "bar"}
    (make-id "6") {:containers #{(make-id "4")} :content 5}
    (make-id "7") {:subject (make-id "1") :content "second"}}
   {(make-id "0") {"foo" [(make-id "1")] "second" [(make-id "1")]}
    (make-id "1") {:label [(make-id "2")] nil [(make-id "7")]}
    (make-id "2") {nil [(make-id "3")]}
    (make-id "4") {nil [(make-id "5")]}}
   8
   nil))

(def test-implicit-content (->ImplicitContentId (make-id "2")))

(deftest id-string-test
  (is (= (stored-item-id-string (make-id "a")) "a"))
  (is (= (stored-item-description-name (make-id "a")) "Id-a"))
  (is (= (stored-item-id-string test-implicit-content) "content-2"))
  (is (= (stored-item-description-name test-implicit-content) "content-Id-2")))

(deftest id-label->element-ids-test
  (is (= (id-label->element-ids test-store (make-id "0") "foo")
         [(make-id "1")]))
  (is (= (id-label->element-ids test-store (make-id "1") nil)
         [(make-id "7")]))
  (is (= (id-label->element-ids test-store (make-id "0") "bar") nil))
  (is (= (id-label->element-ids test-store (make-id "77") "bar") nil)))

(deftest id->element-ids-test
  (is (= (id->element-ids test-store (make-id "0")) [(make-id "1")]))
  (is (= (set (id->element-ids test-store (make-id "1")))
         (set [(make-id "2") (make-id "7")])))
  (is (= (id->element-ids test-store (make-id "77")) nil)))

(deftest id->content-test
  (is (= (id->content test-store (make-id "0")) nil))
  (is (= (id->content test-store (make-id "1")) (make-id "4")))
  (is (= (id->content test-store (make-id "2")) "foo"))
  (is (= (id->content test-store (make-id "6")) 5))
  (is (= (id->content test-store (->ImplicitContentId (make-id "2"))) "foo"))
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

(deftest id-is-content?-test
  (is (id-is-content? test-store (make-id "4") nil))
  (is (not (id-is-content? test-store (make-id "4") [(make-id "1")])))
  (is (not (id-is-content? test-store (make-id "3") nil))))

(deftest atomic-value-test
  (is (= (atomic-value test-store (make-id "0")) nil))
  (is (= (atomic-value test-store (make-id "6")) 5))
  (is (= (atomic-value test-store (make-id "1")) 5)))

(deftest index-subject-in-subject-test
  (let [missing-store
        (assoc test-store :subject->label->ids {})
        indexed-store
        (reduce (fn [store id]
                  (index-in-subject (index-subject store id) id))
                missing-store
                (keys (:id->data missing-store)))]
    (is (= indexed-store test-store))))

(deftest index-content-test
  (let [data-map (:id->data test-store)
        keys (keys data-map)
        missing-store
        (assoc test-store :id->data
               (zipmap keys
                       (for [key keys] (dissoc (data-map key) :containers))))
        indexed-store
        (reduce (fn [store id]
                  (index-content store id))
                missing-store keys)]
    (is (= indexed-store test-store))))

(deftest deindex-subject-test
  (let [id (make-id "999")
        s1 (assoc-in test-store [:id->data id]
                      {:subject (make-id "2") :content :test})
        s2 (assoc-in test-store [:id->data id]
                      {:subject (make-id "2") :content :foo})
        s3 (assoc-in test-store [:id->data id]
                      {:subject (make-id "2")})
        s4 (assoc-in test-store [:id->data id]
                     { :content :test})]
    (is (= (deindex-subject (index-subject s1 id) id) s1))
    (is (= (deindex-subject (index-subject s2 id) id) s2))
    (is (= (deindex-subject (index-subject s3 id) id) s3))
    (is (= (deindex-subject (index-subject s4 id) id) s4))))

(deftest deindex-content-test
  (let [id (make-id "999")
        s1 (assoc-in test-store [:id->data id]
                      {:subject (make-id "2") :content :test})
        s2 (assoc-in test-store [:id->data id]
                      {:subject (make-id "2") :content (make-id "4")})
        s3 (assoc-in test-store [:id->data id]
                      {:subject (make-id "2")  :content (make-id "5")})
        s4 (assoc-in test-store [:id->data id]
                     {:subject (make-id "2")})]
    (is (= (deindex-content (index-content s1 id) id) s1))
    (is (= (deindex-content (index-content s2 id) id) s2))
    (is (= (deindex-content (index-content s3 id) id) s3))
    (is (= (deindex-content (index-content s4 id) id) s4))))

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
    (is (= (get-in promoted-store [:id->data (make-id "2") :content])
           promoted-id))
    (is (= (get-in promoted-store [:id->data promoted-id])
           {:content (get-in test-store [:id->data (make-id "2") :content])
            :containers #{(make-id "2")}}))
    (is (= (get-in promoted-store [:id->data (make-id "2") :subject])
           (get-in test-store [:id->data (make-id "2") :subject]))))
  (let [[promoted-store promoted-id]
        (promote-implicit-item
         (track-modified-ids test-store) (->ImplicitContentId (make-id "2")))]
    (is (= (:modified-ids promoted-store) #{promoted-id}))
    (is (= (fetch-and-clear-modified-ids promoted-store)
           [(assoc promoted-store :modified-ids #{}), #{promoted-id}]))))

(deftest add-simple-element-test
  (let [[added-store element]
        (add-simple-element test-store (make-id "1") "test")]
    (is (= (:id element) (:next-id test-store)))
    (is (= (set (get-in added-store [:subject->label->ids (make-id "1") nil]))
           #{(make-id "7") element}))
    (is (= (get-in added-store [:id->data element])
           {:content "test"
            :subject (make-id "1")})))
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

(deftest candidate-matching-ids-test
  (is (= (set (candidate-matching-ids test-store nil))
         #{(make-id "0") (make-id "1") (make-id "2") (make-id "4") })))

(deftest new-element-store-test
  (let [store (new-element-store)]
    (is (= (candidate-matching-ids store nil) nil))))




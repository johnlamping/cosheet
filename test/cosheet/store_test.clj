(ns cosheet.store-test
  (:require [clojure.test :refer [deftest is]]
            [cosheet.store :refer :all]
            [cosheet.item-store :refer :all]
            [cosheet.entity :refer [to-list description->entity]]
            cosheet.entity-impl
            [cosheet.store-impl :refer :all]
            ; :reload
            ))

(def test-store
  (->ElementStore
   {(make-id "1") {:subject (make-id "0") :content (make-id "4")}
    (make-id "2") {:subject (make-id "1") :content "foo"}
    (make-id "3") {:subject (make-id "2") :content :label}
    (make-id "4") {:container (make-id "1") :content (make-id "6")}
    (make-id "5") {:subject (make-id "4") :content "bar"}
    (make-id "6") {:container (make-id "4") :content 5}
    (make-id "7") {:subject (make-id "1") :content "second"}}
   {(make-id "0") {"foo" [(make-id "1")] "second" [(make-id "1")]}
    (make-id "1") {:label [(make-id "2")] nil [(make-id "7")]}
    (make-id "2") {nil [(make-id "3")]}
    (make-id "4") {nil [(make-id "5")]}}
   8))

(def test-implicit-content (->ImplicitContentId (make-id "2")))

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
         (make-id "4")))
  (is (= (id->content test-store (->ImplicitContentId
                                  (->ImplicitContentId (make-id "1"))))
         (make-id "4"))))

(deftest id->content-reference-test
  (is (= (id->content-reference test-store (make-id "1")) (make-id "4")))
  (is (= (id->content-reference test-store (make-id "2"))
         (->ImplicitContentId (make-id "2"))))
  (is (= (id->content-reference test-store
                                (->ImplicitContentId (make-id "2")))
         (->ImplicitContentId (->ImplicitContentId (make-id "2"))))))

(deftest ensure-in-vector-test
  (is (= (ensure-in-vector nil 1) [1]))
  (is (= (ensure-in-vector [1] 2) [1 2]))
  (is (= (ensure-in-vector [1 2] 2) [1 2])))

(deftest remove-from-key-vector-test
  (is (= (remove-from-key-vector {1 [2 3] 5 [6]} 1 3) {1 [2] 5 [6]}))
  (is (= (remove-from-key-vector {1 [2 3] 5 [6]} 1 6) {1 [2 3] 5 [6]}))
  (is (= (remove-from-key-vector {1 [2] 5 [6]} 1 2) {5 [6]})))

(deftest atomic-value-test
  (is (= (atomic-value test-store (make-id "0")) nil))
  (is (= (atomic-value test-store (make-id "6")) 5))
  (is (= (atomic-value test-store (make-id "1")) 5)))

(deftest index-new-element-test
  (let [missing-store
        (assoc test-store :subject->label->ids {})
        indexed-store
        (reduce (fn [store id]
                  (index-new-element store id))
                missing-store
                (keys (:id->data missing-store)))]
    (is (= indexed-store test-store))))

(deftest promote-implicit-item-test
  (is (= (promote-implicit-item test-store (make-id "1"))
         [test-store (make-id "1")]))
  (is (= (promote-implicit-item
          test-store (->ImplicitContentId (make-id "1")))
         [test-store (make-id "4")]))
  (let [[promoted-store promoted-id]
        (promote-implicit-item test-store
                               (->ImplicitContentId (make-id "2")))]
    (is (= (:next-id promoted-store) (+ 1 (:next-id test-store))))
    (is (= (:subject->label->ids promoted-store)
           (:subject->label->ids test-store)))
    (is (= (get-in promoted-store [:id->data (make-id "2") :content])
           promoted-id))
    (is (= (get-in promoted-store [:id->data promoted-id])
           {:content (get-in test-store [:id->data (make-id "2") :content])
            :container (make-id "2")}))
    (is (= (get-in promoted-store [:id->data (make-id "2") :subject])
           (get-in test-store [:id->data (make-id "2") :subject])))))

(deftest add-simple-element-test
  (let [[added-store element]
        (add-simple-element test-store (make-id "1") "test")]
    (= (:id element) (:next-id test-store))
    (is (= (set (get-in added-store [:subject->label->ids (make-id "1") nil]))
           #{(make-id "7") element}))
    (is (= (get-in added-store [:id->data element])
           {:content "test"
            :subject (make-id "1")}))))

(deftest candidate-ids-test
  (is (= (set (candidate-ids test-store nil))
         #{(make-id "0") (make-id "1") (make-id "2") (make-id "4") })))

(deftest new-element-store-test
  (let [store (new-element-store)]
    (is (= (candidate-ids store nil) nil))))

(deftest add-entity-test
  (let [[added-store element]
        (add-entity test-store (make-id "0") '((77 88) ("test" :label)))]
    (let [[added2-store _]
          (add-entity added-store element '("Fred" ("by" :label)))]
      (is (#{'((77 88) ("test" :label) ("Fred" ("by" :label)))
             '((77 88) ("Fred" ("by" :label)) ("test" :label))}
           (to-list (description->entity element added2-store))
           )))))



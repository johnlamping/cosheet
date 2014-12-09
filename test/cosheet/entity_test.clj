(ns cosheet.entity-test
  (:require [clojure.test :refer [deftest is]]
            [cosheet.entity :refer :all]
            [cosheet.store
             :refer [add-simple-element new-element-store make-id]]
            [cosheet.entity-impl]
            cosheet.store-impl
            :reload))

(deftest storeditem-test
  (let [id0 (make-id "0")
        id1 (make-id "1")
        id99 (make-id "99")
        [s a b c d e]
        (let [[s1 a] (add-simple-element (new-element-store) id99 3)]
          (let [[s2 b] (add-simple-element s1 a "foo")]
            (let [[s3 c] (add-simple-element s2 id99 4)]
              (let [[s4 d] (add-simple-element s3 c "bar")]
                (let [[s5 e] (add-simple-element
                              s4
                              (:item-id
                               (content-reference
                                (description->entity c s4))) "baz")]
                  [s5 a b c d e])))))
        item0 (description->entity id0 s)
        item1 (description->entity id1 s)
        item99 (description->entity id99 s)]
    (is (= (:item-id  item0) id0))
    (is (= (:item-id  item1) id1))
    (is (not (atom? item0)))
    (is (= (label->elements item99 "foo") [(description->entity a s)]))
    (is (= (label->elements (description->entity a s) nil)
           [(description->entity b s)]))
    (is (= (set (elements item99))
           #{(description->entity a s) (description->entity c s)}))
    (is (= (elements (description->entity e s))
           nil))
    (is (= (content item99) nil))
    (is (= (content (description->entity a s)) 3))
    (is (not (atom? (content (description->entity c s)))))
    (is (content (content (description->entity c s))) 4)
    (is (= (elements (content (description->entity c s)))
           [(description->entity e s)]))
    (is (= (content-reference (description->entity c s))
           (content (description->entity c s))))
    (is (not= (content-reference (description->entity b s))
              (content (description->entity b s))))
    (is (= (content (content-reference (description->entity b s)))
           "foo"))
    (is (atom? (content-reference (description->entity b s))))
    (is (= (label->content item99 "foo") 3))
    (is (= (label->content item99 "bletch") nil))
    (is (= (atomic-value (description->entity c s)) 4))
    (is (label-has-atomic-value? item99 "foo" 3))
    (is (not (label-has-atomic-value? item99 "foo" 4)))
    (is (= (label->atomic-values item99 "bar")) [4])))

(deftest list-test
  (is (not (atom? '(1 2))))
  (is (= (elements '(1 2)) [2]))
  (is (= (elements '(1 (2 3) (4 5))) '[(2 3) (4 5)]))
  (is (= (label->elements '(1 (2 3) (4 5)) 3) '[(2 3)]))
  (is (= (content '(1 (2 3) (4 5))) 1))
  (is (= (content '((1 7) (2 3) (4 5))) '(1 7)))
  (is (= (content-reference '(1 (2 3) (4 5))) 1))
  (is (= (content-reference '((1 7) (2 3) (4 5))) '(1 7)))
  (is (= (to-list '(((1) 1 2) (2 (3 (4))))) '((1 1 2) (2 (3 4)))))
  (is (= (to-list '(nil (1 nil))) '(nil (1 nil)))))

(deftest constant-test
  (is (atom? 1))
  (is (atom? true))
  (is (atom? "foo"))
  (is (atom? :foo))
  (is (= (elements 1) nil))
  (is (= (elements true) nil))
  (is (= (elements "foo") nil))
  (is (= (elements :foo) nil))
  (is (= (label->elements 1 1) nil))
  (is (= (label->elements true 1) nil))
  (is (= (label->elements "foo:foo" 1) nil))
  (is (= (label->elements :foo 1) nil))
  (is (= (content 1) 1))
  (is (= (content true) true))
  (is (= (content "foo") "foo"))
  (is (= (content :foo) :foo))
  (is (= (content-reference 1) 1))
  (is (= (content-reference true) true))
  (is (= (content-reference "foo") "foo"))
  (is (= (content-reference :foo) :foo)))

(deftest atomic-value-test
  (is (= (atomic-value 2) 2))
  (is (= (atomic-value '(((1 2) 3) 4)) 1))
  (is (= (atomic-value '(((nil 2) 3) 4)) nil))
    (is (= (atomic-value nil) nil)))

(deftest entity<->description-test
  (let [s (new-element-store)
        id (make-id "1")
        item (description->entity id s)]
    (is (= (description->entity 2 s) 2))
    (is (= (description->entity :foo s) :foo))
    (is (= (description->entity "1" s) "1"))
    (is (= (description->entity id s) item))))





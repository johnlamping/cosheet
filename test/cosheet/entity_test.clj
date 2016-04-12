(ns cosheet.entity-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet [orderable :as orderable]
                     [reporters :refer [set-attendee! value]]
                     [store :refer [add-simple-element make-id
                                    new-element-store new-mutable-store
                                    track-modified-ids
                                    update-content!]]
                     [entity :refer :all]
                     store-impl
                     mutable-store-impl
                     entity-impl
                     [debug :refer [current-value]]
                     [test-utils :refer [check any as-set]])
            ; :reload
            ))

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
    (is (= (stored-entity-id-string item0) "0"))
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
    (is (= (label->atomic-values item99 "bar")) [4])
    (is (= (to-list item0) nil))))

(deftest mutable-storeditem-test
  (let [id0 (make-id "0")
        id1 (make-id "1")
        id99 (make-id "99")
        [s ida idb idc idd ide]
        (let [[s1 ida] (add-simple-element (new-element-store) id99 3)]
          (let [[s2 idb] (add-simple-element s1 ida "foo")]
            (let [[s3 idc] (add-simple-element s2 id99 4)]
              (let [[s4 idd] (add-simple-element s3 idc "bar")]
                (let [[s5 ide] (add-simple-element
                                s4
                                (:item-id
                                 (content-reference
                                  (description->entity idc s4))) "baz")]
                  [s5 ida idb idc idd ide])))))
        ms (new-mutable-store s)
        item0 (description->entity id0 ms)
        item1 (description->entity id1 ms)
        item99 (description->entity id99 ms)]    
    (is (= (:item-id  item0) id0))
    (is (= (:item-id  item1) id1))
    (is (= (stored-entity-id-string item0) "0"))
    (is (not (current-value (atom? item0))))
    (is (= (current-value (label->elements item99 "foo"))
           [(description->entity ida ms)]))
    (is (= (current-value (label->elements (description->entity ida ms) nil)) 
           [(description->entity idb ms)]))
    (is (= (set (current-value (elements item99)))
           #{(description->entity ida ms) (description->entity idc ms)}))
    (is (= (current-value (elements (description->entity ide ms)))
           nil))
    (is (= (current-value (content item99)) nil))
    (is (= (current-value (content (description->entity ida ms))) 3))
    (is (not (current-value
              (atom? (current-value (content (description->entity idc ms)))))))
    (is (current-value
         [content (current-value (content (description->entity idc ms)))])
        4)
    (is (= (current-value
            (elements (current-value (content (description->entity idc ms)))))
           [(description->entity ide ms)]))
    (is (= (current-value (content-reference (description->entity idc ms)))
           (current-value (content (description->entity idc ms)))))
    (is (not= (current-value (content-reference (description->entity idb ms)))
              (current-value (content (description->entity idb ms)))))
    (is (= (current-value
            (content (current-value
                      (content-reference (description->entity idb ms)))))
           "foo"))
    (is (current-value
         [atom? (current-value
                 (content-reference (description->entity idb ms)))]))
    (is (= (current-value (label->content item99 "foo")) 3))
    (is (= (current-value (label->content item99 "bletch")) nil))
    (is (= (current-value (atomic-value (description->entity idc ms))) 4))
    (is (current-value (label-has-atomic-value? item99 "foo" 3)))
    (is (not (current-value (label-has-atomic-value? item99 "foo" 4))))
    (is (= (current-value (label->atomic-values item99 "bar"))) [4])
    (is (= (current-value (deep-to-list item99))
           (deep-to-list (description->entity id99 s))))
    ;; Now  make sure call-with-immutable tracks right.
    (let [record (atom [])
          fun (fn [current-item]
                (is (not (mutable-entity? current-item)))
                (let [value (deep-to-list current-item)]
                  (swap! record #(conj % value))
                  value))
          call-with-immutable-result (call-with-immutable item99 fun)]
      (is (= @record []))
      ;; See if it gets computed when demand is added.
      (set-attendee! call-with-immutable-result :a (fn [id reporter] nil))
      (is (check (value call-with-immutable-result)
                 (as-set '(nil ((4 "baz") "bar") (3 "foo")))))
      (is (check @record [(as-set '(nil ((4 "baz") "bar") (3 "foo")))]))
      ;; Make sure it is not recomputed when an irrelevant change is made.
      (update-content! ms id0 44)
      (is (check @record [(as-set '(nil ((4 "baz") "bar") (3 "foo")))]))
      ;; Make sure it is recomputed when a deep, but relevant, change is made.
      (update-content! ms idd "bletch")
      (println (current-value (to-list item99)))
      (is (check (value call-with-immutable-result)
                 (as-set '(nil ((4 "baz") "bletch") (3 "foo")))))
      (is (check @record [(as-set '(nil ((4 "baz") "bar") (3 "foo")))
                          (as-set '(nil ((4 "baz") "bletch") (3 "foo")))])))))

(deftest list-test
  (is (not (atom? '(1 2))))
  (is (= (elements '(1 2)) [2]))
  (is (= (elements '(1 (2 3) (4 5))) '[(2 3) (4 5)]))
  (is (= (label->elements '(1 (2 3) (4 5)) 3) '[(2 3)]))
  (is (= (content '(1 (2 3) (4 5))) 1))
  (is (= (content '((1 7) (2 3) (4 5))) '(1 7)))
  (is (= (content-reference '(1 (2 3) (4 5))) 1))
  (is (= (content-reference '((1 7) (2 3) (4 5))) '(1 7)))
  (is (= (to-list '(((1) 1 2) (2 (3 (4))))) '(((1) 1 2) (2 (3 4)))))
  (is (= (deep-to-list '(((1) 1 2) (2 (3 (4))))) '((1 1 2) (2 (3 4)))))
  (is (= (to-list '(nil (1 nil))) '(nil (1 nil)))))

(deftest constant-test
  (is (atom? 1))
  (is (atom? true))
  (is (atom? "foo"))
  (is (atom? :foo))
  (is (atom? 'foo))
  (is (atom? nil))
  (is (atom? orderable/initial))
  (is (= (elements 1) nil))
  (is (= (elements true) nil))
  (is (= (elements "foo") nil))
  (is (= (elements :foo) nil))
  (is (= (elements 'foo) nil))
  (is (= (elements nil) nil))
  (is (= (elements orderable/initial) nil))
  (is (= (label->elements 1 1) nil))
  (is (= (label->elements true 1) nil))
  (is (= (label->elements "foo:foo" 1) nil))
  (is (= (label->elements :foo 1) nil))
  (is (= (label->elements 'foo 1) nil))
  (is (= (label->elements nil 1) nil))
  (is (= (label->elements orderable/initial 1) nil))
  (is (= (content 1) 1))
  (is (= (content true) true))
  (is (= (content "foo") "foo"))
  (is (= (content :foo) :foo))
  (is (= (content 'foo) 'foo))
  (is (= (content nil) nil))
  (is (= (content orderable/initial) orderable/initial))
  (is (= (content-reference 1) 1))
  (is (= (content-reference true) true))
  (is (= (content-reference "foo") "foo"))
  (is (= (content-reference :foo) :foo))
  (is (= (content-reference nil) nil))
  (is (= (content-reference orderable/initial) orderable/initial)))

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
    (is (= (description->entity 'foo s) 'foo))
    (is (= (description->entity orderable/initial s) orderable/initial))
    (is (= (description->entity "1" s) "1"))
    (is (= (description->entity id s) item))))


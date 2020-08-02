(ns cosheet2.entity-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet2 [orderable :as orderable]
                      [reporter :refer [valid? set-attendee!
                                        reporter-value]]
                      [expression :refer [expr-let]]
                      [store :refer [add-simple-item make-id
                                     new-element-store new-mutable-store
                                     track-modified-ids
                                     current-store
                                     update-content store-update!]]
                      store-impl
                      mutable-store-impl
                      [entity :refer :all]
                      entity-impl
                      [calculator :refer [current-value new-calculator-data
                                          propagate-calculator-data!]]
                      [canonical :refer [canonicalize-list]]
                      [task-queue :refer [new-priority-task-queue
                                          run-all-pending-tasks]]
                      [test-utils :refer [check any as-set]])
            ; :reload
            ))

(deftest storeditem-test
  (let [id0 (make-id "0")
        id1 (make-id "1")
        id99 (make-id "99")
        [s1 ida] (add-simple-item (new-element-store) id99 3)
        [s2 idb] (add-simple-item s1 ida "foo")
        [s3 idc] (add-simple-item s2 id99 4)
        [s4 idd] (add-simple-item s3 idc "bar")
        [s5 ide] (add-simple-item s4 id99 "baz")
        [s6 idf] (add-simple-item s5 idc ide)
        [s7 idg] (add-simple-item s6 ide "bletch")
        [s _] (add-simple-item s7 idb :label)
        item0 (description->entity id0 s)
        item1 (description->entity id1 s)
        item99 (description->entity id99 s)]
    (is (= (:item-id  item0) id0))
    (is (= (:item-id  item1) id1))
    (is (not (atom? item0)))
    (is (= (subject (description->entity ida s)) item99))
    (is (= (label->elements item99 "foo") [(description->entity ida s)]))
    ;; Check that the :label is required.
    (is (= (label->elements (description->entity id99 s5) "foo") nil))
    (is (= (set (elements item99))
           #{(description->entity ida s)
             (description->entity idc s)
             (description->entity ide s)}))
    (is (= (elements (description->entity idd s))
           nil))
    (is (= (content item99) nil))
    (is (= (content (description->entity ida s)) 3))
    (is (not (atom? (content (description->entity idf s)))))
    (is (content (content (description->entity idf s))) "baz")
    (is (= (elements (content (description->entity idf s)))
           [(description->entity idg s)]))
    (is (= (label->element item99 "foo") (description->entity ida s)))
    (is (= (label->element item99 "bletch") nil))
    (is (= (label->content item99 "foo") 3))
    (is (= (label->content item99 "bletch") nil))
    (let [[sx idx] (add-simple-item s id99 7)
          [sy idy] (add-simple-item sx idx "foo")
          [sz idz] (add-simple-item sy idy :label)]
      (is (thrown? java.lang.AssertionError
                   (label->element (description->entity id99 sz) "foo")))
      (is (thrown? java.lang.AssertionError
                   (label->content (description->entity id99 sz) "foo"))))
    (is (= (ultimate-content (description->entity idc s)) 4))
    (is (= (to-list item0) nil))
    (is (= (current-version item0) item0))))

(deftest mutable-storeditem-test
  (let [id0 (make-id "0")
        id1 (make-id "1")
        id99 (make-id "99")
        [s1 ida] (add-simple-item (new-element-store) id99 3)
        [s2 idb] (add-simple-item s1 ida "foo")
        [s3 idc] (add-simple-item s2 id99 4)
        [s4 idd] (add-simple-item s3 idc "bar")
        [s5 ide] (add-simple-item s4 id99 "baz")
        [s6 idf] (add-simple-item s5 idc ide)
        [s7 idg] (add-simple-item s6 ide "bletch")
        [s _] (add-simple-item s7 idb :label)
        queue (new-priority-task-queue 0)
        cd (new-calculator-data queue)
        ms (new-mutable-store s)
        item0 (description->entity id0 ms)
        item1 (description->entity id1 ms)
        item99 (description->entity id99 ms)
        list-99 `(nil ("baz" "bletch")
                      (4 "bar" ~(description->entity ide ms))
                      (3 ("foo" :label)))]
    (is (= (:item-id  item0) id0))
    (is (= (:item-id  item1) id1))
    (is (= (subject (description->entity ida ms)) item99))
    (is (not (current-value (atom? item0))))
    (is (= (current-value (label->elements item99 "foo"))
           [(description->entity ida ms)]))
    (is (= (current-value (label->elements item99 "bar")) nil))
    (is (= (set (current-value (elements item99)))
           #{(description->entity ida ms)
             (description->entity idc ms)
             (description->entity ide ms)}))
    (is (= (current-value (elements (description->entity idd ms)))
           nil))
    (is (= (current-value (content item99)) nil))
    (is (= (current-value (content (description->entity ida ms))) 3))
    (is (not (current-value
              (atom? (current-value (content (description->entity idf ms)))))))
    (is (current-value
         [content (current-value (content (description->entity idc ms)))])
        4)
    (is (= (current-value
            (elements (current-value (content (description->entity idf ms)))))
           [(description->entity idg ms)]))
    (is (= (current-value (label->content item99 "foo")) 3))
    (is (= (current-value (label->content item99 "bletch")) nil))
    (is (= (current-value (ultimate-content (description->entity idc ms))) 4))
    (let [as-list (current-value (to-list item99))]
      (is (check (canonicalize-list as-list)
                 (canonicalize-list list-99))))
    ;; Now make sure updating-immutable tracks right.
    (let [record (atom [])
          updating-immutable-result
          (expr-let [current-item (updating-immutable item99)] 
           (is (not (mutable-entity? current-item)))
           (let [value (to-list current-item)]
             (swap! record #(conj % value))
             value))]
      (is (= @record []))
      ;; See if it gets computed when demand is added.
      (propagate-calculator-data! updating-immutable-result cd)
      (run-all-pending-tasks queue)
      (is (not (valid? updating-immutable-result)))
      (set-attendee! updating-immutable-result :a 0
                     (fn [& _] nil))
      (is (not (valid? updating-immutable-result)))
      (run-all-pending-tasks queue)
      (let [orig-99 (to-list (in-different-store item99 (current-store ms)))]
        (is (check (canonicalize-list
                    (reporter-value updating-immutable-result))
                   (canonicalize-list orig-99)))
        (is (check (map canonicalize-list @record)
                   [(canonicalize-list orig-99)]))
        ;; Make sure it is not recomputed when an irrelevant change is made.
        (store-update! ms (fn [s] (update-content s id0 44)))
        (is (check (map canonicalize-list @record)
                   [(canonicalize-list orig-99)]))
        ;; Make sure it is recomputed when a deep, but relevant, change is made.
        (store-update! ms (fn [s] (update-content s idd "bletch")))
        (run-all-pending-tasks queue)
        (is (check (canonicalize-list
                    (reporter-value updating-immutable-result))
                   (canonicalize-list
                    (to-list (in-different-store item99 (current-store ms))))))
        (is (check (map canonicalize-list @record)
                   [(canonicalize-list orig-99)
                    (canonicalize-list
                     (to-list (in-different-store item99
                                                  (current-store ms))))]))))
    ;; Finally, check current-version
    (is (= (current-version item0)
           (description->entity id0 (current-store ms))))))

(deftest list-test
  (is (not (atom? '(1 2))))
  (is (= (elements '(1 2)) [2]))
  (is (= (elements '(1 (2 3) (4 5))) '[(2 3) (4 5)]))
  (is (= (label->elements '(1 (2 (3 :label)) (4 3)) 3)
         '[(2 (3 :label))]))
  (is (= (content '(1 (2 3) (4 5))) 1))
  (is (= (content '((1 7) (2 3) (4 5))) '(1 7)))
  (is (= (to-list '(((1) 1 2) (2 (3 (4))))) '(((1) 1 2) (2 (3 4)))))
  (is (= (to-list '(nil (1 nil))) '(nil (1 nil))))
  (is (= (to-deep-list '(((1) 1 2) (2 (3 (4))))) '((1 1 2) (2 (3 4)))))
  (is (= (to-deep-list '(nil (1 nil))) '(nil (1 nil)))))

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
  (is (= (content orderable/initial) orderable/initial)))

(deftest ultimate-content-test
  (is (= (ultimate-content 2) 2))
  (is (= (ultimate-content '(((1 2) 3) 4)) 1))
  (is (= (ultimate-content '(((nil 2) 3) 4)) nil))
  (is (= (ultimate-content nil) nil)))

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

(deftest label?-test
  (is (label? :foo))
  (is (label? '(:foo "foo")))
  (is (label? '("foo" :label)))
  (is (label? '("foo" :label "bar")))
  (is (not (label? :label)))
  (is (not (label? "foo")))
  (is (minimal-label? :foo))
  (is (not (minimal-label? '(:foo "foo"))))
  (is (minimal-label? '("foo" :label)))
  (is (not (minimal-label? '("foo" :label "bar")))))


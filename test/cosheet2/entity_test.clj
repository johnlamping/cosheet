(ns cosheet2.entity-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet2 [orderable :as orderable]
                      [reporter :refer [valid? set-attendee!
                                        reporter-value]]
                      [expression :refer [expr-let]]
                      [store :refer [add-link make-item-id
                                     new-element-store new-mutable-store
                                     track-modified-ids
                                     current-store
                                     update-source store-update!]]
                      store-impl
                      mutable-store-impl
                      [entity :refer :all]
                      entity-impl
                      [calculator :refer [current-value new-calculator-data
                                          propagate-calculator-data!]]
                      [canonical :refer [canonicalize]]
                      [task-queue :refer [new-priority-task-queue
                                          run-all-pending-tasks]]
                      [test-utils :refer [check any as-set]])
            ; :reload
            ))

(deftest storeditem-test
  (let [id0 (make-item-id "0")
        id1 (make-item-id "1")
        id99 (make-item-id "99")
        [s1 ida] (add-link (new-element-store) id99 3)
        [s2 idb] (add-link s1 ida "foo")
        [s3 idc] (add-link s2 id99 4)
        [s4 idd] (add-link s3 idc "bar")
        [s5 ide] (add-link s4 id99 "baz")
        [s7 idg] (add-link s5 ide "bletch")
        [s _] (add-link s7 idb :label)
        item0 (id->entity id0 s)
        item1 (id->entity id1 s)
        item99 (id->entity id99 s)
        item-b (id->entity idb s)
        item-a-reversed (id->entity ida s :target)]
    (is (= (:item-id  item0) id0))
    (is (= (:item-id  item1) id1))
    (is (not (primitive? item0)))
    (is (= (entity-type item0)) :object)
    (is (= (entity-type item-b)) :element)
    (is (= (orientation item0)) nil)
    (is (= (orientation item-b)) :source)
    (is (= (orientation item-a-reversed)) :target)
    (is (= (container (id->entity ida s)) item99))
    (is (= (container item-a-reversed) 3))
    (is (= (label->elements item99 "foo") [(id->entity ida s)]))
    ;; Check that the :label is required.
    (is (= (label->elements (id->entity id99 s5) "foo") nil))
    (is (= (set (elements item99))
           #{(id->entity ida s)
             (id->entity idc s)
             (id->entity ide s)}))
    (is (= (elements (id->entity idd s))
           nil))
    (is (= (content item99) nil))
    (is (marked-as-type? item-b))
    (is (not(marked-as-type? item99)))
    (is (= (content (id->entity ida s)) 3))
    (is (= (content item-b) "foo"))
    (is (= (content item-a-reversed) item99))
    (is (= (content->elements item99 4) [(id->entity idc s)]))
    (is (= (label->element item99 "foo") (id->entity ida s)))
    (is (= (label->element item99 "bletch") nil))
    (is (= (label->content item99 "foo") 3))
    (is (= (label->content item99 "bletch") nil))
    (let [[sx idx] (add-link s id99 7)
          [sy idy] (add-link sx idx "foo")
          [sz idz] (add-link sy idy :label)]
      (is (thrown? java.lang.AssertionError
                   (label->element (id->entity id99 sz) "foo")))
      (is (thrown? java.lang.AssertionError
                   (label->content (id->entity id99 sz) "foo"))))
    (is (= (content (id->entity idc s)) 4))
    (is (= (to-list item0) item0))))

(deftest mutable-storeditem-test
  (let [id0 (make-item-id "0")
        id1 (make-item-id "1")
        id99 (make-item-id "99")
        [s1 ida] (add-link (new-element-store) id99 3)
        [s2 idb] (add-link s1 ida "foo")
        [s3 idc] (add-link s2 id99 4)
        [s4 idd] (add-link s3 idc "bar")
        [s5 ide] (add-link s4 id99 "baz")
        [s6 idg] (add-link s5 ide "bletch")
        [s7 idh] (add-link s6 idb :label)
        [s idj] (add-link s7 id0 "irrelevant")
        queue (new-priority-task-queue 0)
        cd (new-calculator-data queue)
        ms (new-mutable-store s)
        item0 (id->entity id0 ms)
        item1 (id->entity id1 ms)
        item99 (id->entity id99 ms)
        item-b (id->entity idb ms)
        item-a-reversed (id->entity ida ms :target)
        list-99 `(nil ("baz" "bletch")
                      (4 "bar")
                      (3 ("foo" :label)))]
    (is (= (:item-id  item0) id0))
    (is (= (:item-id  item1) id1))
    (is (= (entity-type item0)) :object)
    (is (= (entity-type item-b)) :element)
    (is (= (orientation item0)) nil)
    (is (= (orientation item-b)) :source)
    (is (= (orientation item-a-reversed)) :target)
    (is (= (current-value (container (id->entity ida ms))) item99))
    (is (= (current-value (container item-a-reversed)) 3))
    (is (= (primitive? item0) false))
    (is (= (current-value (label->elements item99 "foo"))
           [(id->entity ida ms)]))
    (is (= (current-value (label->elements item99 "bar")) nil))
    (is (= (set (current-value (elements item99)))
           #{(id->entity ida ms)
             (id->entity idc ms)
             (id->entity ide ms)}))
    (is (= (current-value (elements (id->entity idd ms)))
           nil))
    (is (= (current-value (content item99)) nil))
    (is (current-value (marked-as-type? item-b)))
    (is (not (current-value (marked-as-type? item99))))
    (is (= (current-value (content (id->entity ida ms))) 3))
    (is (= (current-value (content item-a-reversed)) item99))
    (is (current-value
         [content (current-value (content (id->entity idc ms)))])
        4)
    (is (= (current-value (content->elements item99 4))
           [(id->entity idc ms)]))
    (is (= (current-value (label->content item99 "foo")) 3))
    (is (= (current-value (label->content item99 "bletch")) nil))
    (is (= (current-value (content (id->entity idc ms))) 4))
    (let [as-list (current-value (to-list item99))]
      (is (check (canonicalize as-list)
                 (canonicalize list-99))))
    ;; Now make sure updating-immutable tracks right.
    (let [record-of-updates (atom [])
          updating-immutable-result (expr-let [current-item (updating-immutable
                                                             item99)] 
                                      (is (not (mutable-entity? current-item)))
                                      (let [value (to-list current-item)]
                                        (swap! record-of-updates
                                               #(conj % value))
                                        value))
          reporter-99 (id->updating-entity-R id99 ms)]
      (is (= @record-of-updates []))
      ;; See if it gets computed when demand is added.
      (propagate-calculator-data! updating-immutable-result cd)
      (propagate-calculator-data! reporter-99 cd)
      (run-all-pending-tasks queue)
      (is (not (valid? updating-immutable-result)))
      (set-attendee! updating-immutable-result :a 0 (fn [& _] nil))
      (set-attendee! reporter-99 :a 0 (fn [& _] nil))
      (is (not (valid? updating-immutable-result)))
      (run-all-pending-tasks queue)
      (let [orig-99 (to-list (in-different-store item99 (current-store ms)))]
        (is (check (canonicalize
                    (reporter-value updating-immutable-result))
                   (canonicalize orig-99)))
        (is (check (map canonicalize @record-of-updates)
                   [(canonicalize orig-99)]))
        ;; Make sure it is not recomputed when an irrelevant change is made.
        (store-update! ms (fn [s] (update-source s idj 44)))
        (is (check (map canonicalize @record-of-updates)
                   [(canonicalize orig-99)]))
        ;; Make sure it is recomputed when a deep, but relevant, change is made.
        (store-update! ms (fn [s] (update-source s idd "bletch")))
        (run-all-pending-tasks queue)
        (is (check (canonicalize
                    (reporter-value updating-immutable-result))
                   (canonicalize
                    (to-list (in-different-store item99 (current-store ms))))))
        (is (check (canonicalize
                    (to-list (reporter-value reporter-99)))
                   (canonicalize
                    (to-list (in-different-store item99 (current-store ms))))))
        (is (check (map canonicalize @record-of-updates)
                   [(canonicalize orig-99)
                    (canonicalize
                     (to-list (in-different-store item99
                                                  (current-store ms))))]))))))

(deftest list-test
  (is (not (primitive? '(1 2))))
  (is (= (orientation '(1 2)) :source))
  (is (= (orientation '((:target 1) 2)) :target))
  (is (= (elements '(1 2)) [2]))
  (is (= (elements '(1 (2 3) (4 5))) '[(2 3) (4 5)]))
  (is (= (label->elements '(1 (2 (3 :label)) (4 3)) 3)
         '[(2 (3 :label))]))
  (is (= (content '(1 (2 3) (4 5))) 1))
  (is (= (content '((:source 1) (2 3) (4 5))) 1))
  (is (= (content '((:target 1) (2 3) (4 5))) 1))
  (is (= (content->elements '(1 (2 3) (4 5)) 4) ['(4 5)]))
  (is (marked-as-type? '("foo" :label)))
  (is (not (marked-as-type? '("foo" :foo))))
  (is (= (to-list '(((1) 1 2) (2 (3 (4))))) '(((1) 1 2) (2 (3 4)))))
  (is (= (to-list '(nil (1 nil))) '(nil (1 nil)))))

(deftest vector-test
  (is (not (primitive? [:object 1 2])))
  (is (= (elements [:object 1 2]) [1 2]))
  (is (= (elements '[:object (2 3) (4 5)]) '[(2 3) (4 5)]))
  (is (= (label->elements '[:object (2 (3 :label)) (4 3)] 3)
         '[(2 (3 :label))]))
  (is (= (content [:object 1 2]) nil))
  (is (= (content->elements '[:object (2 3) (4 5)] 4) '[(4 5)]))
  (is (not (marked-as-type? [:object 1 2])))
  (is (= (to-list [:object 1 2]) [:object 1 2])))

(deftest constant-test
  (is (primitive? 1))
  (is (primitive? true))
  (is (primitive? "foo"))
  (is (primitive? :foo))
  (is (primitive? 'foo))
  (is (primitive? nil))
  (is (primitive? orderable/initial))
  (is (= (content 1) 1))
  (is (= (content true) true))
  (is (= (content "foo") "foo"))
  (is (= (content :foo) :foo))
  (is (= (content 'foo) 'foo))
  (is (= (content nil) nil))
  (is (= (content orderable/initial) orderable/initial))
  (is (= (elements 1) nil))
  (is (= (elements true) nil))
  (is (= (elements "foo") nil))
  (is (= (elements :foo) nil))
  (is (= (elements 'foo) nil))
  (is (= (elements nil) nil))
  (is (= (elements orderable/initial) nil))
  (is (= (content->elements 1 1) nil))
  (is (= (content->elements true 1) nil))
  (is (= (content->elements "foo" 1) nil))
  (is (= (content->elements :foo 1) nil))
  (is (= (content->elements 'foo 1) nil))
  (is (= (content->elements nil 1) nil))
  (is (= (content->elements orderable/initial 1) nil))
  (is (= (label->elements 1 1) nil))
  (is (= (label->elements true 1) nil))
  (is (= (label->elements "foo" 1) nil))
  (is (= (label->elements :foo 1) nil))
  (is (= (label->elements 'foo 1) nil))
  (is (= (label->elements nil 1) nil))
  (is (= (label->elements orderable/initial 1) nil))
  (is (not (marked-as-type? 1)))
  (is (not (marked-as-type? true)))
  (is (not (marked-as-type? "foo")))
  (is (not (marked-as-type? :foo)))
  (is (not (marked-as-type? 'foo)))
  (is (not (marked-as-type? nil)))
  (is (not (marked-as-type? orderable/initial)))
  (is (to-list 3) 3))

(deftest entity<->description-test
  (let [s (new-element-store)
        id (make-item-id "1")
        item (id->entity id s)]
    (is (= (description->entity 2 s) 2))
    (is (= (description->entity :foo s) :foo))
    (is (= (description->entity 'foo s) 'foo))
    (is (= (description->entity orderable/initial s) orderable/initial))
    (is (= (description->entity "1" s) "1"))
    (is (= (description->entity id s) item))))

(deftest marked-as-type?-test
  (is (marked-as-type? '("foo" :label)))
  (is (marked-as-type? '("foo" :label "bar")))
  (is (marked-as-type? '("foo" (:label "bar"))))
  (is (not (marked-as-type? '(:label "foo"))))
  (is (not (marked-as-type? :label)))
  (is (not (marked-as-type? "foo")))
  (is (not (marked-as-type? '("foo"))))
  (is (not (marked-as-type? '("foo" "bar"))))
  (is (not (marked-as-type? '(("foo" ("bar" :label))))))
  )

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


(ns cosheet.mutable-store-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [store :refer :all]
             [store-utils :refer :all]
             [entity :refer [to-list description->entity]]
             [reporters :refer [set-attendee! value invalid]]
             entity-impl
             store-impl
             [mutable-store-impl :refer :all]
             [test-utils :refer [check any as-set evals-to let-mutated]])
            ; :reload
            ))

(defn- request [reporter]
  (set-attendee! reporter :demand (fn [key reporter] nil))
  reporter)

(defn- get-value [reporter]  
  (value (request reporter)))

(deftest test-store
  (let [[store element]
        (add-entity (new-element-store) nil '(77 ("test" :label)
                                                 ("Fred" ("by" :label))))
        mutable-store (new-mutable-store store)]
    ;; Test the accessors
    (is (get-value (id-valid? mutable-store element)))
    (is (not (get-value (id-valid? mutable-store (make-id "wrong")))))
    (is (= (get-value (id-label->element-ids mutable-store element :label))
           (id-label->element-ids store element :label)))
    (is (= (get-value (id->element-ids mutable-store element))
           (id->element-ids store element)))
    (is (= (get-value (id->content mutable-store element))
           (id->content store element)))
    (is (= (get-value (id->content-reference mutable-store element))
           (id->content-reference store element)))
    (is (= (get-value (call-dependent-on-id mutable-store element identity))
           (track-modified-ids store)))
    (is (= (get-value (candidate-matching-ids mutable-store 77))
           (candidate-matching-ids store 77)))
    (is (mutable-store? mutable-store))
    ;; Test that subscriptions track
    (let [content (id->content mutable-store element)
          content-ref (id->content-reference store element)
          implicit-content (id->content mutable-store content-ref)
          element-ids (id->element-ids mutable-store element)
          label-ids (id-label->element-ids mutable-store element :label)
          candidate-ids (candidate-matching-ids mutable-store nil)
          tracking-store (call-dependent-on-id mutable-store element identity)
          callback (fn [id reporter arg]
                     (is (= arg "arg")))]
      (set-attendee! content :a callback "arg")
      (set-attendee! implicit-content :a callback "arg")
      (set-attendee! element-ids :a callback "arg")
      (set-attendee! label-ids :a callback "arg")
      (set-attendee! candidate-ids :a callback "arg")
      (set-attendee! tracking-store :a callback "arg")
      (let [[store1 e] (add-simple-element store element "foo")
            [store2 _] (add-simple-element store1 e :label)
            revised-store  (update-content store2 element 88)
            me (add-simple-element! mutable-store element "foo")
            me1 (add-simple-element! mutable-store me :label)
            _ (update-content! mutable-store element 88)]
        (is (= (value content) (id->content revised-store element)))
        (is (= (value implicit-content)
               (id->content revised-store content-ref)))
        (is (= (set (value element-ids))
               (set (id->element-ids revised-store element))))
        (is (= (set (value label-ids))
               (set (id-label->element-ids revised-store element :label))))
        (is (= (set (value candidate-ids))
               (set (candidate-matching-ids revised-store nil))))
        (is (= (value tracking-store)
               (track-modified-ids revised-store)))
        ;; Test that unsubscribe removes tracking by unsubscribing one
        ;; of the reporters, and then changing back to the original store.
        ;; Test undo in the process.
        (set-attendee! label-ids :a)
        (set-attendee! label-ids :demand)
        (undo! mutable-store)
        (undo! mutable-store)
        (remove-simple-id! mutable-store me)
        ;; Still tracked, so should be equal to the original store
        (is (= (set (value element-ids))
               (set (id->element-ids store element))))
        (is (= (set (value candidate-ids))
               (set (candidate-matching-ids store nil))))
        ;; Not tracked, so should be invalid
        (is (= (value label-ids) invalid))
        ;; Subscribing should return the current value, and update the
        ;; reporter.
        (set-attendee! label-ids :a callback "arg")
        (is (= (set (value label-ids))
               (set (id-label->element-ids store element :label)))))
      ;; TODO: Put a version of the test
      ;; here that builds the store from scratch using add-entity!.
)))

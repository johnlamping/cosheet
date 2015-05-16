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
             [mutable-store-impl :refer :all])
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
    (is (= (get-value (id-label->element-ids mutable-store element :label))
           (id-label->element-ids store element :label)))
    (is (= (get-value (id->element-ids mutable-store element))
           (id->element-ids store element)))
    (is (= (get-value (id->content mutable-store element))
           (id->content store element)))
    (is (= (get-value (id->content-reference mutable-store element))
           (id->content-reference store element)))
    (is (= (get-value (candidate-matching-ids mutable-store 77))
           (candidate-matching-ids store 77)))
    (is (mutable-store? mutable-store))
    ;; Test that subscriptions track
    (let [element-ids (id->element-ids mutable-store element)
          label-ids (id-label->element-ids mutable-store element :label)
          candidate-ids (candidate-matching-ids mutable-store nil)
          [store1 e] (add-simple-element store element "foo")
          [revised-store e1] (add-simple-element store1 e :label)
          callback (fn [id reporter arg]
                     (is (= arg "arg")))]
      (set-attendee! element-ids :a callback "arg")
      (set-attendee! label-ids :a callback "arg")
      (set-attendee! candidate-ids :a callback "arg")
      (let [e (add-simple-element! mutable-store element "foo")
            e1 (add-simple-element! mutable-store e :foo)
            _ (update-content! mutable-store e1 :label)]
        (is (= (set (value element-ids))
               (set (id->element-ids revised-store element))))
        (is (= (set (value label-ids))
               (set (id-label->element-ids revised-store element :label))))
        (is (= (set (value candidate-ids))
               (set (candidate-matching-ids revised-store nil))))
        ;; Test that unsubscribe removes tracking by unsubscribing one
        ;; of the reporters, and then changing back to the original store.
        (set-attendee! label-ids :a)
        (set-attendee! label-ids :demand)
        (remove-simple-id! mutable-store e1)
        (remove-simple-id! mutable-store e)
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
      ;; TODO: Write add-entity!, and then put a version of the test
      ;; here that builds the store from scratch.
)))

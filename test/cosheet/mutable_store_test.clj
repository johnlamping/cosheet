(ns cosheet.mutable-store-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [store :refer :all]
             [entity :refer [to-list description->entity]]
             [state :refer :all]
             entity-impl
             store-impl
             [mutable-store-impl :refer :all])
            ; :reload
            ))

(deftest test-store
  (let [[store element]
        (add-entity (new-element-store) nil '(77 ("test" :label)
                                                 ("Fred" ("by" :label))))
        mutable-store (new-mutable-store store)]
    ;; Test the accessors
    (is (= (state-value (id-label->element-ids mutable-store element :label))
           (id-label->element-ids store element :label)))
    (is (= (state-value (id->element-ids mutable-store element))
           (id->element-ids store element)))
    (is (= (state-value (id->content mutable-store element))
           (id->content store element)))
    (is (= (state-value (id->content-reference mutable-store element))
           (id->content-reference store element)))
    (is (= (state-value (candidate-matching-ids mutable-store 77))
           (candidate-matching-ids store 77)))
    (is (mutable-store? mutable-store))
    ;; Test that subscriptions track
    (let [element-ids (id->element-ids mutable-store element)
          label-ids (id-label->element-ids mutable-store element :label)
          candidate-ids (candidate-matching-ids mutable-store nil)
          [store1 e] (add-simple-element store element "foo")
          [revised-store e1] (add-simple-element store1 e :label)
          callback (fn [value state arg]
                     (is (= arg "arg")))]
      (subscribe element-ids callback "arg")
      (subscribe label-ids callback "arg")
      (subscribe candidate-ids callback "arg")
      (let [e (add-simple-element! mutable-store element "foo")
            e1 (add-simple-element! mutable-store e :foo)
            _ (update-content! mutable-store e1 :label)]
        (is (= (set (state-value element-ids))
               (set (id->element-ids revised-store element))))
        (is (= (set (state-value label-ids))
               (set (id-label->element-ids revised-store element :label))))
        (is (= (set (state-value candidate-ids))
               (set (candidate-matching-ids revised-store nil))))
        ;; Test that unsubscribe removes tracking by unsubscribing one
        ;; of the states, and then changing back to the original store.
        (unsubscribe label-ids callback "arg")
        (remove-simple-id! mutable-store e1)
        (remove-simple-id! mutable-store e)
        ;; Still tracked, so should be equal to the original store
        (is (= (set (state-value element-ids))
               (set (id->element-ids store element))))
        (is (= (set (state-value candidate-ids))
               (set (candidate-matching-ids store nil))))
        ;; Not tracked, so should be equal to the revised store
        (is (= (set (state-value label-ids))
               (set (id-label->element-ids revised-store element :label))))
        ;; Subscribing should return the current value, and update the
        ;; state.
        (is (= (set (subscribe label-ids callback "arg" ))
               (set (id-label->element-ids store element :label))))
        (is (= (set (state-value label-ids))
               (set (id-label->element-ids store element :label)))))
      ;; TODO: Write add-entity!, and then put a version of the test
      ;; here that builds the store from scratch.
)))
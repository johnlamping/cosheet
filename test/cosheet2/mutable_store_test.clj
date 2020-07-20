(ns cosheet2.mutable-store-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet2
             [store :refer :all]
             [store-utils :refer :all]
             [reporter :refer [set-attendee! reporter-value invalid
                               ;; TODO: Remove this
                               reporter-data]]
             [calculator :refer [new-calculator-data propagate-calculator-data!
                                 request compute]]
             [expression :refer [category-change]]
             store-impl
             [mutable-store-impl :refer :all]
             [canonical :refer [canonicalize-list]]
             [task-queue :refer [new-priority-task-queue
                                 run-all-pending-tasks]]
             [test-utils :refer [check any as-set]])
            ; :reload
            ))

(def cd (new-calculator-data (new-priority-task-queue 0)))

(defn- get-value [reporter]
  (request reporter cd)
  (compute cd)
  (reporter-value reporter))

(deftest test-store
  (let [[store element]
        (add-entity (new-element-store)
                    nil '(77 ("test" :label)
                             ("Fred" ("by" :label))))
        initial-store (track-modified-ids store)
        queue (new-priority-task-queue 0)
        calculator-data (new-calculator-data queue) 
        mutable-store (new-mutable-store store)
        modified-store (update-content store element 99)]
    ;; Test the accessors
    (is (get-value (id-valid? mutable-store element)))
    (is (not (get-value (id-valid? mutable-store (make-id "wrong")))))
    (is (= (get-value (id-label->element-ids mutable-store element :label))
           (id-label->element-ids store element :label)))
    (is (= (get-value (id->element-ids mutable-store element))
           (id->element-ids store element)))
    (is (= (get-value (id->content mutable-store element))
           (id->content store element)))
    (is (= (get-value (candidate-matching-ids mutable-store 77))
           (candidate-matching-ids store 77)))
    (is (mutable-store? mutable-store))
    ;; Test that subscriptions track.
    (let [content (id->content mutable-store element)
          element-ids (id->element-ids mutable-store element)
          label-ids (id-label->element-ids mutable-store element :label)
          candidate-ids (candidate-matching-ids mutable-store nil)
          tracking-store (category-change [element] (:reporter mutable-store))
          callback (fn [& {:keys [key reporter description categories]}]
                     nil)]
      (propagate-calculator-data! content calculator-data)
      (propagate-calculator-data! element-ids calculator-data)
      (propagate-calculator-data! label-ids calculator-data)
      (propagate-calculator-data! candidate-ids calculator-data)
      (propagate-calculator-data! tracking-store calculator-data)
      (set-attendee! content :a 0 callback)
      (set-attendee! element-ids :a 0 callback)
      (set-attendee! label-ids :a 0 callback)
      (set-attendee! candidate-ids :a 0 callback)
      (set-attendee! tracking-store :a 0 callback)
      (run-all-pending-tasks queue)
      (is (= (reporter-value tracking-store) initial-store))
      (store-reset! mutable-store modified-store)
      (run-all-pending-tasks queue)
      (is (= (reporter-value tracking-store)
             (track-modified-ids modified-store)))
      (is (= (reporter-value content) 99))
      (let [[store1 e] (add-simple-item store element "foo")
            [store2 _] (add-simple-item store1 e :label)
            store3 (declare-temporary-id store2 e)
            revised-store (update-content store3 element 77)
            me (store-update-control-return!
                mutable-store #(add-simple-item % element "foo"))
            me1 (store-update-control-return!
                 mutable-store #(add-simple-item % me :label))
            _ (store-update! mutable-store
                             #(update-equivalent-undo-point % true))
            _ (store-update! mutable-store #(declare-temporary-id % me))
            _ (store-update! mutable-store #(update-content % element 88))
            _ (store-update! mutable-store
                             #(update-equivalent-undo-point % false))
            _ (store-update! mutable-store #(update-content % element 99))
            _ (store-update! mutable-store #(update-content % element 77))]
        (run-all-pending-tasks queue)
        (is (= (reporter-value content) (id->content revised-store element)))
        (is (= (set (reporter-value element-ids))
               (set (id->element-ids revised-store element))))
        (is (= (set (reporter-value label-ids))
               (set (id-label->element-ids revised-store element :label))))
        (is (= (set (reporter-value candidate-ids))
               (set (candidate-matching-ids revised-store nil))))
        (is (check (reporter-value tracking-store)
               (track-modified-ids revised-store)))
        
        ;; Test undo and redo.
        
        (is (can-undo? mutable-store))
        (undo! mutable-store)
        (is (can-undo? mutable-store))
        (undo! mutable-store)
        (is (can-undo? mutable-store))
        (undo! mutable-store)
        (is (can-undo? mutable-store))
        (undo! mutable-store)
        (is (not (can-undo? mutable-store)))
        (run-all-pending-tasks queue)
        (is (= (reporter-value content) (id->content modified-store element)))
        (is (= (set (reporter-value element-ids))
               (set (id->element-ids store element))))
        (is (= (set (reporter-value label-ids))
               (set (id-label->element-ids store element :label))))
        (is (= (set (reporter-value candidate-ids))
               (set (candidate-matching-ids store nil))))
        (is (= (reporter-value tracking-store)
               (track-modified-ids modified-store)))
        (is (can-redo? mutable-store))
        (redo! mutable-store)
        (is (can-redo? mutable-store))
        (redo! mutable-store)
        (is (can-redo? mutable-store))
        (redo! mutable-store)
        (is (can-redo? mutable-store))
        (redo! mutable-store)
        (is (not (can-redo? mutable-store)))
        (run-all-pending-tasks queue)
        (is (= (reporter-value content) (id->content revised-store element)))
        (is (= (set (reporter-value element-ids))
               (set (id->element-ids revised-store element))))
        (is (= (set (reporter-value label-ids))
               (set (id-label->element-ids revised-store element :label))))
        (is (= (set (reporter-value candidate-ids))
               (set (candidate-matching-ids revised-store nil))))
        (is (= (reporter-value tracking-store)
               (track-modified-ids revised-store)))
        
        ;; Test that unsubscribe removes tracking by unsubscribing one
        ;; of the reporters, and then changing back to the original store.
        (set-attendee! label-ids :a)
        (set-attendee! label-ids :demand)
        (undo! mutable-store)
        (undo! mutable-store)
        (undo! mutable-store)
        (is (can-redo? mutable-store))
        (store-update! mutable-store #(remove-simple-item % me))
        (is (not (can-redo? mutable-store)))
        (run-all-pending-tasks queue)
        ;; Still tracked, so should be equal to the original store
        (is (= (set (reporter-value element-ids))
               (set (id->element-ids store element))))
        (is (= (set (reporter-value candidate-ids))
               (set (candidate-matching-ids store nil))))
        ;; Not tracked, so should be invalid
        (is (= (reporter-value label-ids) invalid))
        ;; Subscribing should return the current value, and update the
        ;; reporter.
        (set-attendee! label-ids :a 0 callback)
        (is (= (set (reporter-value label-ids))
               (set (id-label->element-ids store element :label))))))))

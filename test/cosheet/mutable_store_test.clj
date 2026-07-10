(ns cosheet.mutable-store-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [store :refer :all]
             [store-utils :refer :all]
             entity-impl
             [reporter :refer [set-attendee! reporter-value-or-invalid invalid
                               ;; TODO: Remove this
                               reporter-data]]
             [calculator :refer [make-calculator-data propagate-calculator-data!
                                 request compute computation-value]]
             [category-change-calculator :refer [category-change-R]]
             store-impl
             [mutable-store-impl :refer :all]
             [task-queue :refer [make-priority-task-queue
                                 run-all-pending-tasks]]
             [test-utils :refer [check any as-set]]
             [debug :refer [store-as-list]])
            ; :reload
            ))

(defn mutable-store-as-list [mutable-store]
  (let [data (reporter-data mutable-store)
        current (:value data)
        history (:history data)
        future (:future data)]
    [(vec (map store-as-list (reverse (map second history))))
     (store-as-list current)
     (vec (map store-as-list (map second future)))]))

(def cd (make-calculator-data (make-priority-task-queue 0)))

(deftest test-store
  (let [s0 (add-universal-objects (new-element-store))
        [s0a by-label-id] (add-link-type-object s0 "by")
        [store element]
        (add-element s0a
                     nil `(77 (~(link-type-object "test"))
                              ("Fred" (~(link-type-object "by")))))
        initial-store (track-modified-ids store)
        queue (make-priority-task-queue 0)
        calculator-data (make-calculator-data queue)
        mutable-store (new-mutable-store store)
        modified-store (update-source store element 99)]
    ;; Test the accessors.
    (is (computation-value (id-valid-link? mutable-store element) cd))
    (is (not (computation-value
              (id-valid-link? mutable-store (make-item-id "wrong"))
              cd)))
    (is (= (computation-value (id->target mutable-store element) cd)
           (id->target store element)))
    (is (= (computation-value (id->source mutable-store element) cd)
           (id->source store element)))
    (is (= (computation-value (target->ids mutable-store element) cd)
           (target->ids store element)))
    (is (= (computation-value (source->ids mutable-store 77) cd)
           (source->ids store 77)))
    (is (= (computation-value (target-source->ids mutable-store element 77) cd)
           (target-source->ids store element 77)))
    (is (= (computation-value
            (target-label->ids mutable-store element by-label-id)
            cd)
           (target-label->ids store element by-label-id)))
    (is (= (computation-value
            (source-label->ids mutable-store 77 by-label-id)
            cd)
           (target-label->ids store 77 by-label-id)))
    (is (= (computation-value (candidate-matching-ids mutable-store 77) cd)
           (candidate-matching-ids store 77)))
    (is (mutable-store? mutable-store))
    ;; Test that subscriptions track.
    (let [source (id->source mutable-store element)
          element-ids (target->ids mutable-store element)
          label-ids (target-label->ids mutable-store element by-label-id)
          candidate-ids (candidate-matching-ids mutable-store nil)
          tracking-store (category-change-R [element] mutable-store)
          callback (fn [& {:keys [key reporter description categories]}]
                     nil)]
      (propagate-calculator-data! source calculator-data)
      (propagate-calculator-data! element-ids calculator-data)
      (propagate-calculator-data! label-ids calculator-data)
      (propagate-calculator-data! candidate-ids calculator-data)
      (propagate-calculator-data! tracking-store calculator-data)
      (set-attendee! source :a 0 callback)
      (set-attendee! element-ids :a 0 callback)
      (set-attendee! label-ids :a 0 callback)
      (set-attendee! candidate-ids :a 0 callback)
      (set-attendee! tracking-store :a 0 callback)
      (run-all-pending-tasks queue)
      (is (= (reporter-value-or-invalid tracking-store) initial-store))
      (store-reset! mutable-store modified-store)
      (run-all-pending-tasks queue)
      (is (= (reporter-value-or-invalid tracking-store)
             (track-modified-ids modified-store)))
      (is (= (reporter-value-or-invalid source) 99))
      (let [[store1 e] (add-link store element "foo")
            ;; Add a label-link as a sub-element of e, using by-label-id
            ;; (a link-type object) as the source so it counts as a label.
            [store2 _] (add-link store1 e by-label-id)
            store3 (declare-ephemeral-id store2 e)
            revised-store (update-source store3 element "S3")
            me (store-update-control-return!
                mutable-store #(add-link % element "foo"))
            s0 (current-store mutable-store)
            _ (store-update-control-return!
               mutable-store #(add-link % me by-label-id))
            s1 (current-store mutable-store)
            _ (store-update! mutable-store
                             #(-> %
                                  (declare-ephemeral-id me)
                                  (update-source element "S1a")
                                  (update-equivalent-undo-point true)))
            s1a (current-store mutable-store)
            _ (store-update! mutable-store #(update-source % element "S1b"))
            s1b (current-store mutable-store)
            _ (store-update! mutable-store
                             #(-> %
                                  (update-source element "S2")
                                  (update-equivalent-undo-point false)))
            s2 (current-store mutable-store)
            _ (store-update! mutable-store #(update-source % element "S3"))
            s3 (current-store mutable-store)]
        (run-all-pending-tasks queue)
        (is (= (reporter-value-or-invalid source) "S3"))
        (is (= (set (reporter-value-or-invalid element-ids))
               (set (target->ids revised-store element))))
        (is (= (set (reporter-value-or-invalid label-ids))
               (set (target-label->ids revised-store element by-label-id))))
        (is (= (set (reporter-value-or-invalid candidate-ids))
               (set (candidate-matching-ids revised-store nil))))
        (is (check (reporter-value-or-invalid tracking-store)
               (track-modified-ids revised-store)))
        
        ;; Test undo and redo.

        (is (can-undo? mutable-store))
        (undo! mutable-store)
        (is (check (current-store mutable-store) s2))
        (run-all-pending-tasks queue)
        (is (= (reporter-value-or-invalid source) "S2"))
        (is (can-undo? mutable-store))
        (undo! mutable-store)
        ;; Undo lands at s1 (equiv stores s1a, s1b are not in history).
        (is (check (current-store mutable-store) s1))
        (run-all-pending-tasks queue)
        (is (check (reporter-value-or-invalid source) (id->source s1 element)))
        (is (can-undo? mutable-store))
        (undo! mutable-store)
        (is (check (current-store mutable-store) s0))
        (run-all-pending-tasks queue)
        (is (= (reporter-value-or-invalid source) 99))
        (is (can-undo? mutable-store))
        (undo! mutable-store)
        (is (not (can-undo? mutable-store)))
        (is (check (current-store mutable-store)
                   (track-modified-ids modified-store)))
        (run-all-pending-tasks queue)
        (is (= (reporter-value-or-invalid source) (id->source modified-store element)))
        (is (= (set (reporter-value-or-invalid element-ids))
               (set (target->ids store element))))
        (is (= (set (reporter-value-or-invalid label-ids))
               (set (target-label->ids store element by-label-id))))
        (is (= (set (reporter-value-or-invalid candidate-ids))
               (set (candidate-matching-ids store nil))))
        (is (= (reporter-value-or-invalid tracking-store)
               (track-modified-ids modified-store)))
        (is (can-redo? mutable-store))
        (redo! mutable-store)
        (is (check (current-store mutable-store) s0))
        (is (can-redo? mutable-store))
        (redo! mutable-store)
        ;; s1 is restored in the future; redo goes to s1.
        (is (check (current-store mutable-store) s1))
        (is (can-redo? mutable-store))
        ;; Test that an equivalent update doesn't take out the future.
        (store-update! mutable-store
                       #(-> %
                            (update-source element 33)
                            (update-equivalent-undo-point true)))
        (run-all-pending-tasks queue)
        (is (= (reporter-value-or-invalid source) 33))
        (is (can-redo? mutable-store))
        (redo! mutable-store)
        ;; Redo from equiv current pushes s1 (cne) to history; lands at s2.
        (is (check (current-store mutable-store) s2))
        (run-all-pending-tasks queue)
        (is (= (reporter-value-or-invalid source) "S2"))
        ;; Test that undo from s2 goes to s1 (cne, not the equiv state).
        (is (can-undo? mutable-store))
        (undo! mutable-store)
        (is (check (current-store mutable-store) s1))
        (run-all-pending-tasks queue)
        (is (check (reporter-value-or-invalid source) (id->source s1 element)))
        (is (can-redo? mutable-store))
        (redo! mutable-store)
        (is (check (current-store mutable-store) s2))
        (run-all-pending-tasks queue)
        (is (= (reporter-value-or-invalid source) "S2"))
        (is (can-redo? mutable-store))
        (redo! mutable-store)
        (is (check (current-store mutable-store) s3))
        (run-all-pending-tasks queue)
        (is (= (reporter-value-or-invalid source) "S3"))
        (is (not (can-redo? mutable-store)))
        (run-all-pending-tasks queue)
        (is (= (reporter-value-or-invalid source) (id->source revised-store element)))
        (is (= (set (reporter-value-or-invalid element-ids))
               (set (target->ids revised-store element))))
        (is (= (set (reporter-value-or-invalid label-ids))
               (set (target-label->ids revised-store element by-label-id))))
        (is (= (set (reporter-value-or-invalid candidate-ids))
               (set (candidate-matching-ids revised-store nil))))
        (is (= (reporter-value-or-invalid tracking-store)
               (track-modified-ids revised-store)))

        ;; Test that current-significant is updated when store equals
        ;; current-significant and modified-ids is empty.
        (let [data-before (reporter-data mutable-store)
              _ (is (= (:current-significant data-before)
                       (current-store mutable-store)))
              _ (store-update! mutable-store
                               #(assoc % :ephemeral-data {:test "value"}))
              s3e (current-store mutable-store)
              data-after (reporter-data mutable-store)]
          (is (= (:current-significant data-after) s3e)))

        ;; Test that unsubscribe removes tracking by unsubscribing one
        ;; of the reporters, and then changing back to the original store.
        (set-attendee! label-ids :a)
        (set-attendee! label-ids :demand)
        (undo! mutable-store)
        (undo! mutable-store)
        (undo! mutable-store)
        (is (can-redo? mutable-store))
        (store-update! mutable-store #(remove-link % me))
        (is (not (can-redo? mutable-store)))
        (run-all-pending-tasks queue)
        ;; Still tracked, so should be equal to the original store
        (is (= (set (reporter-value-or-invalid element-ids))
               (set (target->ids store element))))
        (is (= (set (reporter-value-or-invalid candidate-ids))
               (set (candidate-matching-ids store nil))))
        ;; Not tracked, so should be invalid
        (is (= (reporter-value-or-invalid label-ids) invalid))
        ;; Subscribing should return the current value, and update the
        ;; reporter.
        (set-attendee! label-ids :a 0 callback)
        (run-all-pending-tasks queue)
        (is (= (set (reporter-value-or-invalid label-ids))
               (set (target-label->ids store element by-label-id))))))))

(deftest categories-affected-by-ids-test
  ;; No modified ids.
  (is (nil? (categories-affected-by-ids
             [] (new-element-store) (new-element-store))))
  ;; An object with no links stores adds nothing.
  (let [iobj (make-item-id "I")]
    (is (= (categories-affected-by-ids [iobj] (new-element-store)
                                       (new-element-store))
           #{iobj})))
  ;; A link/element id: only its target is added.
  (let [iobj (make-item-id "I")
        [s1 obj] (get-new-object-id (new-element-store))
        [store link] (add-link s1 iobj obj)]
    (is (= (categories-affected-by-ids [link] store store)
           #{link iobj})))
  ;; An object that is not interned adds the links for which it is the
  ;; source, but not the ones for which it is the target.
  (let [iobj (make-item-id "I")
        [s1 obj] (get-new-object-id (new-element-store))
        [s2 src-link] (add-link s1 iobj obj)   ;; obj is the source
        [store tgt-link] (add-link s2 obj "s")] ;; obj is the target
    (is (not (interned-object-id? store obj)))
    (is (= (categories-affected-by-ids [obj] store store)
           #{obj src-link iobj})))
  ;; An object whose interned status differs between the two stores
  ;; adds the links for which it is the source, and the links for
  ;; which it is the target whose source is an object. Here obj is
  ;; unnamed in the before store and named in the after store.
  (let [iobj (make-item-id "I")
        jobj (make-item-id "J")
        [s1 obj] (get-new-object-id (new-element-store))
        [s2 src-link] (add-link s1 iobj obj)
        [s3 obj-tgt-link] (add-link s2 obj jobj)
        [s4 prim-tgt-link] (add-link s3 obj "u")
        before-store s4
        [s5 name-link] (add-link s4 obj "Fred")
        [after-store label-link] (add-link s5 name-link name-label-id)]
    (is (not (interned-object-id? before-store obj)))
    (is (interned-object-id? after-store obj))
    (is (= (categories-affected-by-ids [obj] before-store after-store)
           #{obj src-link obj-tgt-link iobj}))
    ;; Nothing extra if the interned status didn't change.
    (is (= (categories-affected-by-ids [obj] after-store after-store)
           #{obj}))))

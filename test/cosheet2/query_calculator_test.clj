(ns cosheet2.query-calculator-test
  (:require [clojure.test :refer [deftest is]]
            clojure.pprint
            (cosheet2 [store :refer [new-element-store new-mutable-store
                                     store-reset! store-update!
                                     store-update-control-return!
                                     current-store]]
                      store-impl
                      mutable-store-impl
                      [store-utils :refer [add-element remove-entity-by-id]]
                      [task-queue :refer [make-priority-task-queue]]
                      [reporter :refer [set-calculator-data!
                                        set-attendee-and-call!
                                        reporter-value valid?]]
                      [calculator :refer [make-calculator-data compute]]
                      [query :refer [variable-query]]
                      [query-calculator :refer [matching-item-ids-R]]
                      [test-utils :refer [check]]
                     )
            ; :reload
            ))

(defn variable
  ([name] (variable-query name))
  ([name qualifier] (variable-query name :qualifier qualifier))
  ([name qualifier reference]
   (variable-query name
                   :qualifier qualifier
                   :reference reference)))

(deftest query-calculator-test
  (let [cd (make-calculator-data (make-priority-task-queue 0))
        s0 (new-element-store)
        [s1 id1] (add-element s0 nil '(:a (3 (4 5))))
        [s2 id2] (add-element s1 id1 '(1 (2 3)))
        [s3 id3] (add-element s2 nil '(:b 3))
        [s4 id4] (add-element s3 id3 1)
        [s5 id5] (add-element s4 id4 2)
        ms (new-mutable-store s5)
        term '(1 2)
        answer (matching-item-ids-R term ms)
        history (atom [])
        callback (fn [& {:as args}]
                   (swap! history #(conj % (dissoc args :reporter))))]
    (set-calculator-data! answer cd)
    (set-attendee-and-call! answer :foo 1 callback)
    (is (check @history
               [{:key :foo :description #{} :categories #{}}
                {:key :foo :description nil :categories nil}]))
    (is (not (valid? answer)))
    (compute cd)
    (is (check @history
               [{:key :foo :description #{} :categories #{}}
                {:key :foo :description nil :categories nil}
                {:key :foo :description nil :categories nil}]))
    (is (check (reporter-value answer)
               #{id2 id4}))
    (is (check (matching-item-ids-R term (current-store ms))
               (reporter-value answer)))
    (let [id6
          (store-update-control-return!
           ms #(add-element % nil '(1 2 3 4)))]
      (compute cd)
      (is (check (reporter-value answer)
                 #{id2 id4 id6}))
      (is (check @history
               [{:key :foo :description #{} :categories #{}}
                {:key :foo :description nil :categories nil}
                {:key :foo :description nil :categories nil}
                {:key :foo :description #{} :categories #{}}
                {:key :foo :description #{id6}, :categories #{id6}}]))
      (store-update! ms #(-> %
                             (remove-entity-by-id id6)
                             (remove-entity-by-id id5)))
      (compute cd)
      (is (check (reporter-value answer)
                 #{id2}))
      (is (check @history
               [{:key :foo :description #{} :categories #{}}
                {:key :foo :description nil :categories nil}
                {:key :foo :description nil :categories nil}
                {:key :foo :description #{} :categories #{}}
                {:key :foo :description #{id6}, :categories #{id6}}
                {:key :foo :description #{} :categories #{}}
                {:key :foo :description #{id4 id6} :categories #{id4 id6}}]))
      (store-reset! ms s5)
      (compute cd)
      (is (check (reporter-value answer)
                 #{id2 id4}))
      (is (check @history
               [{:key :foo :description #{} :categories #{}}
                {:key :foo :description nil :categories nil}
                {:key :foo :description nil :categories nil}
                {:key :foo :description #{} :categories #{}}
                {:key :foo :description #{id6}, :categories #{id6}}
                {:key :foo :description #{} :categories #{}}
                {:key :foo :description #{id4 id6} :categories #{id4 id6}}
                                {:key :foo :description #{} :categories #{}}
                {:key :foo :description nil :categories nil}])))))



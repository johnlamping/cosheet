(ns cosheet.server.views-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet
             [store :refer [new-element-store new-mutable-store current-store
                             make-item-id store-update!]]
             [test-utils :refer [check]]
             [calculator :refer [make-calculator-data compute]]
             [task-queue :refer [make-priority-task-queue]]
             [map-reporter :refer [make-map-reporter]]
             mutable-store-impl)
            (cosheet.server
             [dom-manager :refer [make-dom-manager add-root-dom
                                  activate-component client-id->component]]
             [views :refer [ajax-response]])))

(defn make-test-setup
  "Return a map with a mutable store, dom-manager, and client-state set up
  for ajax-response tests. The store has :following-selection-store-ids set
  to following-ids in ephemeral-data, and optionally :following-selection
  set to initial-following-selection. The dom-manager has a root component
  with a sub-component whose relative-id is item-id."
  [item-id following-ids select-ids & [initial-following-selection]]
  (let [base-store (assoc (new-element-store)
                          :ephemeral-data
                          (cond-> {:following-selection-store-ids following-ids}
                            initial-following-selection
                            (assoc :following-selection
                                   initial-following-selection)))
        ms (new-mutable-store base-store)
        cd (make-calculator-data (make-priority-task-queue 0))
        manager (make-dom-manager ms cd)
        client-state (make-map-reporter {:in-sync true
                                         :select-store-ids select-ids
                                         :if-selected nil})]
    (add-root-dom manager
                  {:relative-id :root
                   :render-dom (fn [spec store]
                                 [:div [:component
                                        {:relative-id item-id
                                         :render-dom (fn [spec store]
                                                       [:div "item"])}]])})
    (activate-component (client-id->component @manager "root"))
    (compute cd)
    {:ms ms :manager manager :client-state client-state}))

(deftest ajax-response-following-selection-test
  (let [item-id (make-item-id "bar")
        item-client-id "root_Ibar"]

    ;; When :following-selection-store-ids matches the select-store-ids used,
    ;; ajax-response should set :following-selection to the resolved client-id.
    (let [{:keys [ms manager client-state]}
          (make-test-setup item-id [item-id] [item-id])]
      (ajax-response manager ms client-state nil {})
      (is (check (:ephemeral-data (current-store ms))
                 {:following-selection item-client-id})))
    
    ;; When the store already has a :following-selection but also has
    ;; :following-selection-store-ids matching the select-store-ids,
    ;; ajax-response should replace the old :following-selection with the
    ;; newly resolved one.
    (let [{:keys [ms manager client-state]}
          (make-test-setup item-id [item-id] [item-id] "old-selection")]
      (ajax-response manager ms client-state {} {})
      (is (check (:ephemeral-data (current-store ms))
                 {:following-selection item-client-id})))

    ;; When :following-selection-store-ids does not match the select-store-ids
    ;; used, ajax-response should leave :following-selection unset.
    (let [other-id (make-item-id "other")
          {:keys [ms manager client-state]}
          (make-test-setup item-id [other-id] [item-id])]
      (ajax-response manager ms client-state nil {})
      (is (check (:ephemeral-data (current-store ms))
                 {:following-selection-store-ids [other-id]})))))


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
  to following-ids in ephemeral-data. The dom-manager has a root component
  with a sub-component whose relative-id is item-id."
  [item-id following-ids select-ids]
  (let [base-store (assoc (new-element-store)
                          :ephemeral-data
                          {:following-selection-store-ids following-ids})
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
      (let [ephemeral-data (:ephemeral-data (current-store ms))]
        (is (= (:following-selection ephemeral-data) item-client-id))
        (is (nil? (:following-selection-store-ids ephemeral-data)))))

    ;; When :following-selection-store-ids does not match the select-store-ids
    ;; used, ajax-response should leave :following-selection unset.
    (let [other-id (make-item-id "other")
          {:keys [ms manager client-state]}
          (make-test-setup item-id [other-id] [item-id])]
      (ajax-response manager ms client-state nil {})
      (is (nil? (:following-selection (:ephemeral-data (current-store ms))))))))

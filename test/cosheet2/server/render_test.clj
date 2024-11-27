(ns cosheet2.server.render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet2
             [debug :refer [simplify-for-print]]
             [test-utils :refer [check any as-set]]
             [store :refer [new-mutable-store]]
             store-impl
             [reporter :refer [reporter-data reporter-value]]
             [calculator :refer [new-calculator-data computation-value]]
             [task-queue :refer [new-priority-task-queue]])
            (cosheet2.server
             [render :refer :all]
             [model-utils :refer [starting-store]]
             [session-state :refer [update-add-session-temporary-element
                                    create-client-state]])
            ; :reload
            ))

(deftest initial-top-level-item-DOM-R-test
  (let [store (starting-store "Tab")
        [store temporary-id] (update-add-session-temporary-element store)
        mutable-store (new-mutable-store store)
        client-state (create-client-state mutable-store nil)
        top-level-id (top-level-id-R mutable-store client-state)
        queue (new-priority-task-queue 0)
        cd (new-calculator-data queue)
        DOM-R (top-level-DOM-R mutable-store temporary-id
                               client-state top-level-id)
        dom (computation-value DOM-R cd)]
    (println dom)))

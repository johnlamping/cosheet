(ns cosheet.core-test
  (require (cosheet
            core
            debug-test
            client-utils-test dom-utils-test
            utils-test orderable-test
            reporters-test mutable-manager-test
            entity-test store-test query-test
            mutable-map-test task-queue-test computation-manager-test
            mutable-store-test store-utils-test)
           (cosheet.server
            render-test dom-tracker-test actions-test)
           :reload))

(ns cosheet.core-test
  (require (cosheet
            core
            debug-test test-utils-test
            client-utils-test dom-utils-test
            utils-test orderable-test
            reporters-test expression-test
            canonical-test
            mutable-manager-test state-map-test mutable-map-test
            entity-test store-test query-test
            task-queue-test expression-manager-test
            mutable-store-test store-utils-test)
           (cosheet.server
            referent hierarchy render dom_tracker actions
            referent-test hierarchy-test
            render-utils-test render-test item-render-test table-render-test
            dom-tracker-test actions-test)
           :reload))

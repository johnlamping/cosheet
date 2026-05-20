(ns cosheet.map-reporter-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet [map-reporter :refer :all]
                      [task-queue :refer [make-priority-task-queue]]
                      [calculator :refer [make-calculator-data
                                          compute
                                          propagate-calculator-data!]]
                      [reporter  :refer [make-reporter reporter-valid? invalid
                                         reporter-value-or-invalid set-value!
                                         reporter-data
                                         set-attendee!]]
                      [test-utils :refer [check any as-set]])
            ; :reload
            ))

(deftest map-reporter-test
  (let [cd (make-calculator-data (make-priority-task-queue 0))
        r1 (make-reporter :value 1)
        ms (make-map-reporter {:a r1 :b 2})
        ra (map-reporter-get ms :a)
        rb (map-reporter-get ms :b)
        rc (map-reporter-get ms :c)
        history (atom [])
        callback (fn [& {:keys [key reporter]}]
                   (swap! history #(conj % [key (reporter-value-or-invalid reporter)])))]
    (is (= (map-reporter-get-current ms :a) 1))
    (propagate-calculator-data! ra cd)
    (propagate-calculator-data! rb cd)
    (propagate-calculator-data! rc cd)
    (is (not (reporter-valid? ra)))
    (set-attendee! ra :ra 1 callback)
    (is (= @history []))
    (compute cd)
    (is (= (reporter-value-or-invalid ra) 1))
    (is (not (reporter-valid? rb)))
    (is (check @history
               [[:ra 1]]))
    (set-attendee! rb :rb 10 callback)
    (set-attendee! rc :rc 100 callback)
    (compute cd)
    (is (check @history
               [[:ra 1]
                [:rb 2]
                [:rc nil]]))
    (set-value! r1 2)
    (compute cd)
    (is (= (reporter-value-or-invalid ra) 2))
    (is (check @history
               [(any) (any) (any)
                [:ra 2]]))
    (map-reporter-change-value! ms :b (fn [x] (+ x 9)))
    (compute cd)
    (is (check @history
               [(any) (any) (any)
                [:ra 2]
                [:rb invalid]
                [:rb 11]]))
    (map-reporter-reset! ms {:a 3 :c 5})
    (is (= (map-reporter-get-current ms :c) 5))
    (compute cd)
    (is (check (nthrest @history 6)
               (as-set [[:ra invalid]
                        [:rc invalid]
                        [:ra 3]
                        [:rc 5]])))
    (is (= (map-reporter-change-value-control-return!
            ms :c (fn [x] [[x "hi"] "there"]))
           "there"))
    (compute cd)
    (is (check (nthrest @history 10)
               [[:rc invalid]
                [:rc [5 "hi"]]]))))

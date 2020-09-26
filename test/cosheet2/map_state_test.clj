(ns cosheet2.map-state-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet2 [map-state :refer :all]
                      [task-queue :refer [new-priority-task-queue]]
                      [calculator :refer [new-calculator-data
                                          compute
                                          propagate-calculator-data!]]
                      [reporter  :refer [new-reporter valid? invalid
                                         reporter-value set-value!
                                         reporter-data
                                         set-attendee!]]
                      [test-utils :refer [check any]])
            ; :reload
            ))

(deftest map-state-test
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        r1 (new-reporter :value 1)
        ms (new-map-state {:a r1 :b 2})
        ra (map-state-get ms :a)
        rb (map-state-get ms :b)
        rc (map-state-get ms :c)
        history (atom [])
        callback (fn [& {:keys [key reporter]}]
                   (swap! history #(conj % [key (reporter-value reporter)])))]
    (is (= (map-state-get-current ms :a) 1))
    (propagate-calculator-data! ra cd)
    (propagate-calculator-data! rb cd)
    (propagate-calculator-data! rc cd)
    (is (not (valid? ra)))
    (set-attendee! ra :ra 1 callback)
    (is (= @history []))
    (compute cd)
    (is (= (reporter-value ra) 1))
    (is (not (valid? rb)))
    (is (check @history
               [[:ra invalid]
                [:ra 1]]))
    (set-attendee! rb :rb 10 callback)
    (set-attendee! rc :rc 100 callback)
    (compute cd)
    (is (check @history
               [[:ra invalid]
                [:ra 1]
                [:rb invalid]
                [:rb 2]
                [:rc invalid]
                [:rc nil]]))
    (set-value! r1 2)
    (compute cd)
    (is (= (reporter-value ra) 2))
    (is (check @history
               [(any) (any) (any) (any) (any) (any)
                [:ra 2]]))
    (map-state-change-value! ms :b (fn [x] (+ x 9)))
    (compute cd)
    (is (check @history
               [(any) (any) (any) (any) (any) (any)
                [:ra 2]
                [:rb invalid]
                [:rb 11]]))
    (map-state-reset! ms :c 5)
    (is (= (map-state-get-current ms :c) 5))
    (compute cd)
    (is (check @history
               [(any) (any) (any) (any) (any) (any)
                (any) (any) (any)
                [:rc invalid]
                [:rc 5]]))
    (is (= (map-state-change-value-control-return!
            ms :c (fn [x] [[x "hi"] "there"]))
           "there"))
    (compute cd)
    (is (check @history
               [(any) (any) (any) (any) (any) (any)
                (any) (any) (any) (any) (any) 
                [:rc invalid]
                [:rc [5 "hi"]]]))))

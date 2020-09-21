(ns cosheet2.map-state-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet2 [map-state :refer :all]
                      [task-queue :refer [new-priority-task-queue]]
                      [calculator :refer [new-calculator-data
                                          compute
                                          propagate-calculator-data!]]
                      [reporter  :refer [new-reporter reporter-value 
                                         reporter-data
                                         valid? invalid
                                         set-attendee!]]
                      [test-utils :refer [check any]])
            ; :reload
            ))

(deftest map-state-test
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        ms (new-map-state {:a 1 :b 2})
        ra (map-state-get ms :a)
        rb (map-state-get ms :b)
        rc (map-state-get ms :c)
        history (atom [])
        callback (fn [& {:keys [key reporter]}]
                   (swap! history #(conj % [key (reporter-value reporter)])))]
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
    (set-attendee! rc :rc 100 callback)
    (compute cd)
    (is (check @history
               [[:ra invalid]
                [:ra 1]
                [:rc invalid]
                [:rc nil]]))
    (map-state-change-value! ms :a (fn [x] (+ x 9)))
    (compute cd)
    (is (check @history
               [[:ra invalid]
                [:ra 1]
                [:rc invalid]
                [:rc nil]
                [:ra invalid]
                [:ra 10]]))
    (map-state-reset! ms :c 5)
    (compute cd)
    (is (check @history
               [[:ra invalid]
                [:ra 1]
                [:rc invalid]
                [:rc nil]
                [:ra invalid]
                [:ra 10]
                [:rc invalid]
                [:rc 5]]))
    (is (= (map-state-change-value-control-return!
            ms :c (fn [x] [[x "hi"] "there"]))
           "there"))
    (compute cd)
    (is (check @history
               [[:ra invalid]
                [:ra 1]
                [:rc invalid]
                [:rc nil]
                [:ra invalid]
                [:ra 10]
                [:rc invalid]
                [:rc 5]
                [:rc invalid]
                [:rc [5 "hi"]]]))))

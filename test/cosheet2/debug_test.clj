(ns cosheet2.debug-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            (cosheet2
             [debug :refer :all]
             [task-queue :refer [current-tasks new-priority-task-queue]]
             [reporter :refer [new-reporter reporter-atom reporter-data
                               reporter-value set-value!
                               data-value 
                               valid? reporter?]]
             [calculator :refer [new-calculator-data current-value
                                 propagate-calculator-data!
                                 compute request]]
             [expression :refer [app-R cache-R]]
             [test-utils :refer [check any as-set]])
            ; :reload
            ))

(deftest reporters-profile-test
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        r0 (new-reporter :name :r0 :value 3)
        indirect (fn indirect [arg] (app-R max arg r0))
        r1 (app-R indirect r0)
        r-inc (app-R inc (cache-R min r1))
        r-dec (app-R dec (cache-R min r1))
        rs (app-R + r-inc r-dec)]
    (propagate-calculator-data! rs cd)
    (let [profile (reporters-profile [rs])
          expected{nil {'_PLUS_ 1 'inc 1 'dec 1 'min 1 'max 1
                        'cosheet2.debug-test/fn/indirect 1}
                   'cosheet2.debug-test/fn/indirect {'max 1}}]
      (is (= profile expected)))))

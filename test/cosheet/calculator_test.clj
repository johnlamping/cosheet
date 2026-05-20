(ns cosheet.calculator-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet [reporter :refer [make-reporter reporter-atom reporter-data
                                        reporter-value-or-invalid set-value!
                                        data-value-or-invalid 
                                        reporter-valid? reporter?
                                        set-calculator-data-if-needed!
                                        set-attendee! change-data! invalid
                                        validity-category]]
                      [task-queue :refer [make-priority-task-queue]]
                      [calculator :refer :all]
                      [utils :refer [update-new-further-action
                                     update-new-further-actions]])
            ; :reload
            ))

(defn fib [n s]
  (if (<= n 1)
    s   
    (make-reporter :application [+ (fib (- n 1) s) (fib (- n 2) s)]
                  :calculator (fn [& _] nil))))

(deftest make-calculator-data-test
  (let [queue (make-priority-task-queue 0)
        cd (make-calculator-data queue)]
    (is (= (:queue cd) queue))
    (is (not (nil? (:cache cd))))))

(deftest modify-and-act!-test
  (let [r (make-reporter :test 10)
        a (atom 1)]
    (modify-and-act! r (fn [data]
                        (-> data
                            (update-in [:test] inc)
                            (update-new-further-action swap! a inc)
                            (update-new-further-actions [[swap! a #(* % 3)]]))))
    (is (= (:test (reporter-data r)) 11))
    (is (= @a 6))))

(defn activated? [r]
  (if (reporter? r)
    (let [data (reporter-data r)]
      (and (or (= (:calculator-data data) :cd)
               (reporter-valid? (data-value-or-invalid data)))
           (every? activated? (:application data))))
    true))

(deftest propagate-calculator-data!-test
  (let [state (make-reporter :value 0)
        f6 (fib 6 state)]
    (is (not (activated? f6)))
    (propagate-calculator-data! f6 :cd)
    (is (activated? f6))))

(deftest update-value-and-dependent-depth-test
  (let [cd (make-calculator-data (make-priority-task-queue 0))
        r (make-reporter :value :v :dependent-depth 2)
        history (atom [])
        callback (fn [&{:keys [categories]}]
                   (swap! history #(conj % categories)))
        data-keys [:value :valid :dependent-depth]
        data (reporter-data r)]
    (set-calculator-data-if-needed! r cd)
    (set-attendee! r :foo 1 callback)
    ; Make the reporter go invalid.
    (modify-and-act! r (fn [data]
                         (update-value-and-dependent-depth data r invalid 4)))
    ;; The value and depth shouldn't change.
    (is (= (select-keys (reporter-data r) data-keys)
           {:valid false
            :value :v                    
            :dependent-depth 2}))
    (compute cd)
    (is (= @history [[validity-category]]))
    ;; Now make it valid, with the original value, but a different depth.
    (modify-and-act! r (fn [data]
                         (update-value-and-dependent-depth data r :v 4)))
    (is (= (select-keys (reporter-data r) data-keys)
           {:valid true
            :value :v                    
            :dependent-depth 4}))
    (compute cd)
    ;; This should show up as only a validity change.
    (is (= @history [[validity-category]
                     [validity-category]]))
    ;; Now change the the depth, but leave the value alone
    (modify-and-act! r (fn [data]
                         (update-value-and-dependent-depth data r :v 6)))
    (is (= (select-keys (reporter-data r) data-keys)
           {:valid true
            :value :v                    
            :dependent-depth 6}))
    (compute cd)
    ;; This should be a change with no categories
    (is (= @history [[validity-category]
                     [validity-category]
                     []]))
    ;; Now change the value
    (modify-and-act! r (fn [data]
                         (update-value-and-dependent-depth data r :x 6)))
    (is (= (select-keys (reporter-data r) data-keys)
           {:valid true
            :value :x                    
            :dependent-depth 6}))
    (compute cd)
    ;; This should be an unspecified change
    (is (= @history [[validity-category]
                     [validity-category]
                     []
                     nil]))))

(deftest copy-value-test
  (let [cd (make-calculator-data (make-priority-task-queue 0))
        r1 (make-reporter :value :v)
        r2 (make-reporter :value-source r1
                         :value-source-priority-delta 1
                         :calculator-data cd)]
    (register-for-value-source r2 r1 copy-value-callback)
    (compute cd)
    (is (= (reporter-value-or-invalid r2) :v))
    (set-value! r1 :w)
    (compute cd)
    (is (= (reporter-value-or-invalid r2) :w))
    (swap! (reporter-atom r2) dissoc :value-source)
    (register-for-value-source r2 r1 copy-value-callback)
    (compute cd)
    (set-value! r1 :x)
    (compute cd)
    (is (= (reporter-value-or-invalid r2) :w))))

(deftest current-value-test
  (let [state (make-reporter :value 0)
        fib6 (fib 6 state)]
    (is (= (current-value fib6) 0))
    (set-value! state 1)
    (is (= (current-value fib6) 13))))

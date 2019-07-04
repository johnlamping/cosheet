(ns cosheet.mutable-manager-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet
             [reporter :refer [set-attendee! value invalid]]
             [mutable-manager :refer :all]
             [task-queue :refer [new-priority-task-queue
                                 run-all-pending-tasks]])
            ; :reload
            ))

(defn- request [reporter]
  (set-attendee! reporter :demand 0 (fn [key reporter] nil)))

(defn- unrequest [reporter]
  (set-attendee! reporter :demand))

(deftest mutable-manager-test
  (let [execution-count (atom 0)
        queue (new-priority-task-queue 0)
        mm (new-mutable-manager-data {:a 1 :b 2} queue)
        fa (fn [state]
             (swap! execution-count (fn [n] (+ n 1)))
             (:a state))
        ra (get-or-make-reporter [:a] fa mm)]
    (is (= (value ra) invalid))
    (is (= @execution-count 0))
    (is (= (:value @mm {:a 1 :b 2})))
    (is (= (:subscriptions @mm) {}))
    (is (= (:application->attended-reporter @mm) nil))
    (request ra)
    (run-all-pending-tasks queue)
    (is (= (value ra) 1))
    (is (= @execution-count 1))
    (is (= (:subscriptions @mm) {:a #{ra}}))
    (is (= (:application->reporter @mm) {[fa] ra}))
    (is (= (get-or-make-reporter [:a] fa mm) ra))
    (describe-and-swap! mm (fn [v] [(assoc v :a 3) [:a]]))
    (run-all-pending-tasks queue)
    (is (= (value ra) 3))
    (is (= @execution-count 2))
    (describe-and-swap! mm (fn [v] [(assoc v :a 5) [:b :c]]))
    (run-all-pending-tasks queue)
    (is (= (value ra) 3))
    (reset-manager! mm {:a 6 :f 2})
    (run-all-pending-tasks queue)
    (is (= (value ra) 6))
    (is (= @execution-count 3))
    (unrequest ra)
    (is (= (value ra) 6))
    (request ra)
    (is (= @execution-count 3)) ;; Should have kept old version
    (unrequest ra)
    (is (= (value ra) 6))
    (describe-and-swap! mm (fn [v] [(assoc v :a 3) [:a]]))
    (is (= (value ra) invalid))
    (request ra)
    (is (= (value ra) 3))
    (unrequest ra)
    (is (not= (:subscriptions @mm) {}))
    (dotimes [n 10]
      (describe-and-swap! mm (fn [v] [(assoc v :b n) [:b]])))
    (is (= (:subscriptions @mm) {}))
    (request ra)
    (unrequest ra)
    (stop-tracking-suspended mm)
    (is (= (:subscriptions @mm) {}))
    (is (= (:application->attended-reporter @mm) nil))))


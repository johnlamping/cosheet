(ns cosheet.task-queue-test
  (:require [clojure.test :refer [deftest is]]
            [cosheet.task-queue :refer :all]
            :reload))

(deftest tasks-test
  (let [queue (new-priority-task-queue)
        history (atom [])
        task-factory (fn [& expected]
                       (fn [& args]
                         (is (= args expected))
                         (swap! history #(conj % args))))]
    (add-task queue (task-factory :a1 :a2) :a1 :a2)
    (add-task-with-priority queue -1 (task-factory :a3 :a4) :a4)
    (add-task-with-priority queue 1 (task-factory :a5) :a5)
    (is (run-pending-task queue :a3))
    (is (run-pending-task queue))
    (is (not (finished-all-tasks? queue)))
    (is (run-pending-task queue))
    (is (= @history [[:a3 :a4] [:a1 :a2] [:a5]]))
    (is (finished-all-tasks? queue))
    (wait-until-finished queue)
    (is (not (run-pending-task queue)))
    (is (= @history [[:a3 :a4] [:a1 :a2] [:a5]]))
    (add-task queue (task-factory :a6 :a2) :a2)
    (add-task-with-priority queue -1 (task-factory :a6 :a4) :a4)
    (run-all-pending-tasks queue :a6)
    (is (= @history [[:a3 :a4] [:a1 :a2] [:a5] [:a6 :a4] [:a6 :a2]]))))

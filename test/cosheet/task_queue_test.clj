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
    (add-task-with-priority queue -1 (task-factory :a4) :a4)
    (add-task-with-priority queue 1 (task-factory :a5) :a5)
    (is (run-pending-task queue))
    (is (run-pending-task queue))
    (is (not (finished-all-tasks? queue)))
    (is (run-pending-task queue))
    (is (= @history [[:a4] [:a1 :a2] [:a5]]))
    (is (finished-all-tasks? queue))
    (wait-until-finished queue)
    (is (not (run-pending-task queue)))
    (is (= @history [[:a4] [:a1 :a2] [:a5]]))
    (add-task queue (task-factory :a2) :a2)
    (add-task-with-priority queue -1 (task-factory :a4) :a4)
    (run-all-pending-tasks queue)
    (is (= @history [[:a4] [:a1 :a2] [:a5] [:a4] [:a2]]))))

(deftest tasks-workers-test
  (let [queue (new-priority-task-queue 3)
        history (atom [])
        task-factory (fn [& expected]
                       (fn [& args]
                         (is (= args expected))
                         (swap! history #(conj % args))))]
    (add-task queue (task-factory :a1 :a2) :a1 :a2)
    (add-task-with-priority queue -1 (task-factory :a4) :a4)
    (add-task-with-priority queue 1 (task-factory :a5) :a5)
    (wait-until-finished queue)
    (is (not (run-pending-task queue)))
    (is (= @history [[:a4] [:a1 :a2] [:a5]]))
    (add-task queue (task-factory :a2) :a2)
    (add-task-with-priority queue -1 (task-factory :a4) :a4)
    (wait-until-finished queue)
    (is (= @history [[:a4] [:a1 :a2] [:a5] [:a4] [:a2]]))
    (is (= (:num-workers @queue) 0))))

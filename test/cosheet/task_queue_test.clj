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
    (is (run-pending-task queue false))
    (is (run-pending-task queue false))
    (is (not (finished-all-tasks? queue)))
    (is (run-pending-task queue false))
    (is (= @history [[:a4] [:a1 :a2] [:a5]]))
    (is (finished-all-tasks? queue))
    (wait-until-finished queue)
    (is (not (run-pending-task queue false)))
    (is (= @history [[:a4] [:a1 :a2] [:a5]]))
    (add-task queue (task-factory :a2) :a2)
    (add-task-with-priority queue -1 (task-factory :a4) :a4)
    (run-all-pending-tasks queue)
    (is (= @history [[:a4] [:a1 :a2] [:a5] [:a4] [:a2]]))
    (add-task queue (task-factory :a7) :a7)
    (add-task-with-priority queue -1 (task-factory :a8) :a8)
    (add-task-with-priority queue 1 (task-factory :a9) :a9)
    (run-some-pending-tasks queue 2)
    (is (= @history [[:a4] [:a1 :a2] [:a5] [:a4] [:a2] [:a8] [:a7]]))))

(deftest tasks-workers-test
  (dotimes [_ 500]
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
      (is (not (run-pending-task queue false)))
      (is (= (set @history)  #{[:a4] [:a1 :a2] [:a5]}))
      (add-task queue (task-factory :a3) :a3)
      (add-task-with-priority queue -1 (task-factory :a6) :a6)
      (wait-until-finished queue)
      (is (= (set @history) #{[:a4] [:a1 :a2] [:a5] [:a3] [:a6]}))
      (is (= (:num-running @queue) 0))
      (add-task queue (task-factory :a7) :a7)
      (add-task-with-priority queue -1 (task-factory :a8) :a8)
      (add-task-with-priority queue 1 (task-factory :a9) :a9)
      (run-some-pending-tasks queue 2)
      (when (not (finished-all-tasks? queue))
        (Thread/sleep 10)) ;; Let the worker threads finish.
      (is (= (:num-running @queue) 0)))))

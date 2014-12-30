(ns cosheet.synchronize-test
  (:require [clojure.test :refer [deftest is]]
            [cosheet.synchronize :refer :all]
            :reload))

(deftest tasks-test
  (let [queue (new-priority-task-queue)
        history (atom [])
        task-factory (fn [& expected]
                       (fn [& args]
                         (is (= args expected))
                         (swap! history #(conj % args))))]
    (add-task queue (task-factory :a1 :a2) :a1 :a2)
    (add-task-with-priority queue -1 (task-factory :a3 :a4) :a3 :a4)
    (add-task-with-priority queue 1 (task-factory :a5) :a5)
    (is (run-pending-task queue))
    (is (run-pending-task queue))
    (is (run-pending-task queue))
    (is (= @history [[:a3 :a4] [:a1 :a2] [:a5]]))
    (is (not (run-pending-task queue)))
    (is (= @history [[:a3 :a4] [:a1 :a2] [:a5]]))))

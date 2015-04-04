(ns cosheet.task-queue
  (:require [clojure.data.priority-map :as priority-map]
            [cosheet.utils :refer [swap-returning-both!]]))

;;; Methods for maintaining a priority queue of tasks

(defn new-priority-task-queue []
  ;; Records a priority queue of undone tasks and a count of running,
  ;; but not finished tasks.
  (atom [(priority-map/priority-map) 0]))

(defn add-task-with-priority
  "Add a task to the queue of pending tasks (lower priority goes
  first). The task is a seq of a function and arguments."
  [task-queue priority & task]
  (swap! task-queue (fn [[queue count]] [(assoc queue task priority) count])))

(defn add-task
  "Add a task to the queue of pending tasks with priority 0.
   The task is a seq of a function and arguments."
  [task-queue & task]
  (apply add-task-with-priority task-queue 0 task))

(defn run-pending-task
  "Execute the topmost task in the queue, if any, and pop the queue.
  Return true if there was a task."
  [task-queue & prefix-args]
  (let [[task priority]
        (peek (first
               (first (swap-returning-both!
                       task-queue
                       (fn [[queue count]] (if (empty? queue)
                                             [queue count]
                                             [(pop queue) (inc count)]))))))]
    (when task
      (apply (first task) (concat prefix-args (rest task)))
      (swap! task-queue (fn [[queue count]] [queue (dec count)]))
      true)))

(defn finished-all-tasks? [task-queue]
  (let [[queue count] @task-queue]
    (and (empty? queue) (zero? count))))

(defn run-all-pending-tasks
  "Execute tasks in the queue, in priority order,
   until there are no more tasks and all tasks have terminated."
  [task-queue & prefix-args]
  (loop []
    (apply run-pending-task task-queue prefix-args)
    (when (not(finished-all-tasks? task-queue))
      (recur))))

(defn wait-until-finished [task-queue]
  (loop []
    (if (not (finished-all-tasks? task-queue))
      (recur))))



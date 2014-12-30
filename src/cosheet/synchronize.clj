(ns cosheet.synchronize
  (:require [clojure.data.priority-map :as priority-map]
            [cosheet.mutable-map :refer [swap-returning-both!]]))

;;; Methods for maintaining a priority queue of tasks

(defn new-priority-task-queue []
  (atom (priority-map/priority-map)))

(defn add-task-with-priority
  "Add a task to the queue of pending tasks (lower priority goes
  first). The task is a seq of a function and arguments."
  [task-queue priority & task]
  (swap! task-queue #(assoc % task priority)))

(defn add-task
  "Add a task to the queue of pending tasks with priority 0.
   The task is a seq of a function and arguments."
  [task-queue & task]
  (apply add-task-with-priority task-queue 0 task))

(defn run-pending-task [task-queue]
  "Execute the topmost task in the queue, if any, and pop the queue.
   Return true if there was a task."
  (let [[task priority]
        (peek (first (swap-returning-both!
                      task-queue
                      (fn [queue] (if (empty? queue) queue (pop queue))))))]
    (when task
      (apply (first task) (rest task))
      true)))


;;; TODO: write functions for a trivial store; probably can't
;;; find one online, because this is not about events but change
;;; notifications.

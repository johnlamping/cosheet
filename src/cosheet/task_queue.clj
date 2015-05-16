(ns cosheet.task-queue
  (:require [clojure.data.priority-map :as priority-map]
            [cosheet.utils :refer [swap-returning-both! swap-control-return!]]))

;;; Methods for maintaining a priority queue of tasks

(defn new-priority-task-queue
  ;; Records a priority queue of unstarted tasks, a count of running,
  ;; but not finished tasks, a maximum number of worker threads, and a
  ;; number of current worker threads. (Note: tasks may be run under
  ;; threads other than the worker threads we can provide.)
  ([] (new-priority-task-queue 0))
  ([max-workers]
   (atom {:tasks (priority-map/priority-map)
          :num-running 0
          :max-workers max-workers
          :num-workers 0})))

(def do-work)

(defn add-task-with-priority
  "Add a task to the queue of pending tasks (lower priority goes
  first). The task is a seq of a function and its arguments.
  In addition, if we are not at our maximum number of worker tasks,
  start up a new worker."
  [task-queue priority & task]
  (when (swap-control-return!
         task-queue
         (fn [data]
           (let [added (update-in data [:tasks] #(assoc % task priority))]
             (if (< (:num-workers data) (:max-workers data))
               [(update-in added [:num-workers] inc) true]
               [added false]))))
    (future (do-work task-queue))))

(defn add-task
  "Add a task to the queue of pending tasks with priority 0.
   The task is a seq of a function and arguments."
  [task-queue & task]
  (apply add-task-with-priority task-queue 0 task))

(defn run-pending-task
  "Execute the topmost task in the queue, if any, and pop the queue.
   Return true if there was a task. The second argument is true
   if we are a worker thread, in which case, we decrease the worker
   count if we find no work."
  ([task-queue]
   (run-pending-task task-queue false))
  ([task-queue is-worker]
   (let [[task priority]
         (peek (:tasks
                (first (swap-returning-both!
                        task-queue
                        (fn [data] (if (empty? (:tasks data))
                                     (if is-worker
                                       (update-in data [:num-workers] dec)
                                       data)
                                     (-> data
                                         (update-in [:tasks] pop)
                                         (update-in [:num-running] inc))))))))]
     (when task
       (apply (first task) (rest task))
       (swap! task-queue (fn [data] (update-in data [:num-running] dec)))
       true))))

(defn do-work
  "The function that a worker runs."
  [task-queue]
  (loop []
    (when (run-pending-task task-queue true)
      (recur))))

(defn finished-all-tasks? [task-queue]
  (let [data @task-queue]
    (and (empty? (:tasks data)) (zero? (:num-running data)))))

(defn run-all-pending-tasks
  "Execute tasks in the queue, in priority order,
   until there are no more tasks and all tasks have terminated."
  [task-queue]
  (loop []
    (run-pending-task task-queue)
    (when (not (finished-all-tasks? task-queue))
      (recur))))

(defn wait-until-finished [task-queue]
  (loop []
    (if (not (finished-all-tasks? task-queue))
      (recur))))

(defn current-tasks
  "For debugging: return a seq of all the current tasks."
  [task-queue]
  (map first (seq (:tasks @task-queue))))

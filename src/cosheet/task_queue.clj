(ns cosheet.task-queue
  (:require [clojure.data.priority-map :as priority-map]
            [cosheet.utils :refer [swap-returning-both! swap-control-return!]]))

;;; Methods for maintaining a priority queue of tasks

(defn new-priority-task-queue
  ;; Records a priority queue of unstarted tasks, a count of running,
  ;; but not finished tasks, a maximum number of worker threads, and a
  ;; number of current worker threads. (Note: tasks may be run under
  ;; threads other than the worker threads we can provide.)
  [max-workers]
  (atom {:tasks (priority-map/priority-map)
         :num-running 0
         :max-workers max-workers
         :num-workers 0}))

(def do-work)

(defn add-task-with-priority
  "Add a task to the queue of pending tasks (lower priority goes
  first). The task is a seq of a function and its arguments.
  In addition, if we are not at our maximum number of worker tasks,
  start up a new worker."
  [task-queue priority & task]
  (assert task)
  (when (swap-control-return!
         task-queue
         (fn [data]
           (let [added (update-in data [:tasks] #(assoc % task priority))]
             ;; Don't add a thread if we already have a thread for
             ;; every ten tasks.
             (if (let [num-workers (:num-workers data)]
                   (and (< num-workers (:max-workers data))
                        (> (count (:tasks added)) (* 10 num-workers))))
               [(update-in added [:num-workers] inc) true]
               [added false]))))
    (future (do-work task-queue))))

(defn add-tasks-with-priorities
  "Add tasks to the queue of pending tasks (lower priority goes
  first). priorities-and-tasks is a seq of seqs, each consisting of a priority,
  a function, and then the remainder being the arguments to the function.
  In addition, if we are not at our maximum number of worker tasks,
  start up a new worker."
  [task-queue priorities-and-tasks]
  (when (swap-control-return!
         task-queue
         (fn [data]
           (let [added (reduce (fn [data [priority & task]]
                                 (update-in data [:tasks]
                                            #(assoc % task priority)))
                               data priorities-and-tasks)]
             ;; Don't add a thread if we already have a thread for
             ;; every ten tasks.
             (if (let [num-workers (:num-workers data)]
                   (and (< num-workers (:max-workers data))
                        (> (count (:tasks added)) (* 10 num-workers))))
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
   Return true if there was a task.
   If the second argument is true, decrement the number of workers
   if there was no task."
  [task-queue decrement-workers-if-no-task]
  (let [task (swap-control-return!
              task-queue
              (fn [data] 
                (if (empty? (:tasks data))
                  [(if decrement-workers-if-no-task
                     (update-in data [:num-workers] dec)
                     data)
                   nil]
                  [(-> data
                       (update-in [:tasks] pop)
                       (update-in [:num-running] inc))
                   (first (peek (:tasks data)))])))]
    (when task
      ;; Keep a failure in the task from killing our thread, which
      ;; would prevent us from realizing when we are done.
      ;; TODO: Also check for a task that runs too long?
      ;;       Note: To do that, make a future to run the task, do a
      ;;             (deref f 1000 ::timeout) to give it one second to
      ;;             run before returning ::timeout, and wrap the
      ;;             deref in a try, because a failure in the task
      ;;             will turn into a failure of the deref.
      (try
        (apply (first task) (rest task))
        (catch Exception e
          (println (str "Caught exception: " (.getMessage e)))
          (clojure.stacktrace/print-stack-trace e))
        (catch java.lang.Error e
          (println (str "Caught error: " (.getMessage e)))
          (clojure.stacktrace/print-stack-trace e))
        (finally
          (swap! task-queue (fn [data] (update-in data [:num-running] dec)))))
      true)))

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
    (if (run-pending-task task-queue false)
      (recur)
      (when (not (finished-all-tasks? task-queue))
        (recur)))))

(defn run-some-pending-tasks
  "Execute tasks in the queue, in priority order, until there are no
  more tasks, or at least max-tasks have been done."
  [task-queue max-tasks]
  (loop [num-done 0]
    (when (< num-done max-tasks)
      (if (run-pending-task task-queue false)
        (recur (+ num-done 1))
        (when (not (finished-all-tasks? task-queue))
          (recur num-done))))))

(defn wait-until-finished [task-queue]
  (loop []
    (if (not (finished-all-tasks? task-queue))
      (recur))))

(defn current-tasks
  "For debugging: return a seq of all the current tasks."
  [task-queue]
  (map first (seq (:tasks @task-queue))))

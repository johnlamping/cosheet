(ns cosheet.synchronize
  (:require [clojure.data.priority-map :as priority-map]))

;;; Utilities for making clean maps. Maybe don't belong here.

(defn dissoc-in
  "Remove (get-in map keys), and if that creates an empty map one
  level up, keep removing."
  [map keys]
  (if (empty? keys)
    nil
    (let [key (first keys)
          lower (dissoc-in (get map key) (rest keys))]
      (if (empty? lower)
        (dissoc map key)
        (assoc map key lower)))))

(defn update-in-clean-up
  "Like update-in, but removes empty collections."
  [map keys fn]
  (let [result (fn (get-in map keys))]
      (if (or (nil? result) (and (coll? result) (empty? result)))
        (dissoc-in map keys)
        (assoc-in map keys result))))

;;; Functions that implement mutable maps.
;;; By hiding mutable maps behind this abstraction, we can switch the
;;; implementation if we want.
;;; This implementation is a vector containing atoms of maps. A key is
;;; stored in the map at the index corresponding to its hash.

(defn swap-returning-old! [atom f & args]
  (loop []
    (let [old @atom
          new (apply f old args)]
      (if (compare-and-set! atom old new)
        old
        (recur)))))

(defn new-mutable-map [] (vec (map (fn [i] (atom {})) (range 10))))

(defn- mm-ref [mm key]
  (mm (mod (hash key) (count mm))))

(defn- mm! [fun mm key & args]
  (apply fun @(mm-ref mm key) key args))

(defn- mm-in! [fun mm keys & args]
  (apply fun @(mm-ref mm (first keys)) keys args))

(def mm-get! (partial mm! get))
(def mm-get-in! (partial mm-in! get-in))

(defn- mm-swap! [fun mm key & args]
  (swap! (mm-ref mm key) #(apply fun % key args)))

(defn- mm-swap-in! [fun mm keys & args]
  (swap! (mm-ref mm (first keys)) #(apply fun % keys args)))

(defn- mm-swap-in-returning-old! [fun mm keys & args]
  (get-in (swap-returning-old! (mm-ref mm (first keys))
                               #(apply fun % keys args))
          keys))

(def mm-update! (partial mm-swap! (fn [map key f] (update-in map [key] f))))
(def mm-update-in! (partial mm-swap-in! update-in))
(def mm-update-in-returning-old! (partial mm-swap-in-returning-old! update-in))
(def mm-assoc-in! (partial mm-swap-in! assoc-in))
(def mm-dissoc-in! (partial mm-swap-in! dissoc-in))
(def mm-update-in-clean-up! (partial mm-swap-in! update-in-clean-up))

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
        (peek (swap-returning-old!
               task-queue (fn [queue] (if (empty? queue) queue (pop queue)))))]
    (when task
      (apply (first task) (rest task))
      true)))

;;; Methods for reflecting the state of part of a mutable map
;;; somewhere else, either elsewhere in the map, or elsewhere
;;; entirely.

;;; This is used for propagating updated information to users of the
;;; information without using transactions, just atomic operations.
;;; The principal is that anyplace that uses information makes a
;;; record of what it used. Then, it can be notified of changes at any
;;; time, possibly with some changes skipped, as long as it is
;;; guaranteed that it will eventually be notified of the latest
;;; information.

;;; In typical use, after a use of information is recorded, a
;;; task is set up to make a subscription for that information. When
;;; that task runs, it makes the state of there being a subscription
;;; agree with whether a subscription is still wanted, and if the
;;; information had changed before the subscription was created, it
;;; does a notification as if it had come from the subscription.

(defprotocol State
  "The methods of something that holds a single value that can change."
  (state-value [this]
    "The current value of the state")
  (subscribe [this callback & args]
    "Returns the current value, or nil if it is currently unknown.
     Will eventually call the callback if the current value changes,
     passing it the new value and the args specified in the subscribe.
     May not call it for every change.")
  (unsubscribe [this callback & args]
    "Removes the specified subscription."))

(defn call-with-latest-value
  "Call the function with the path, the current value of the path,
   and the other arguments, until the value of the path after the call
   matches the value used in the call. This can be used to make
   some external state correspond with the state on the path."
  [mm path f & args]
  (loop [initial (mm-get-in! mm path)]
    (apply f path initial args)
    (let [now (mm-get-in! mm path)]
      (when (not= initial now) (recur now)))))

(defn do-propagations-on-path
  "Given a path to a collection of pending propagation operations,
  each described by a path to data that needs to be reflected
  elsewhere, remove the collection from the map, do
  call-with-latest-value for each path. Return true if there were any
  operations to do."
  [mm pending-path f & args]
  (let [pending (mm-update-in-returning-old! mm pending-path (fn [tasks] nil))]
    (when (seq pending)
      (doseq [path pending]
        (apply call-with-latest-value mm path f args))
      true)))

;;; TODO: write functions for a trivial store; probably can't
;;; find one online, because this is not about events but change
;;; notifications.

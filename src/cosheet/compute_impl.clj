(ns cosheet.compute-impl
  (:require [clojure.data.priority-map :as priority-map]
            [cosheet.compute :refer :all]))

(defn empty-to-nil [item]
  (if (seq item) item))

(defn dissoc-in [map keys]
  "Remove (get-in map keys),
   and if that creates an empty map one level up, keep removing."
  (if (empty? keys)
    nil
    (let [key (first keys)
          lower (dissoc-in (get map key) (rest keys))]
      (if (empty? lower)
        (dissoc map key)
        (assoc map key lower)))))

(defn update-in-collapse-empty [map keys fn]
  "Like update-in, but removes empty  maps."
  (let [result (fn (get-in map keys))]
      (if (or (nil? result) (and (coll? result) (empty? result)))
        (dissoc-in map keys)
        (assoc-in map keys result))))

;;; Functions that implement mutable maps.
;;; By hiding mutable maps behind this abstraction, we can switch the
;;; implementation if we want.
;;; This implementation is a vector containing refs of maps. A key is
;;; stored in the map at the index corresponding to its hash.

(defn new-mutable-map [] (vec (map (fn [i] (ref {})) (range 10))))

(defn mm-get! [mm key]
  (@(mm (mod (hash key) (count mm))) key))

(defn mm-get-in! [mm keys]
  (get-in (mm-get! mm (first keys)) (rest keys)))

;;; The next three must be called inside a transaction.
(defn mm-update! [mm key f]
  (alter (mm (mod (hash key) (count mm))) #(update-in % [key] f)))

(defn mm-update-in! [mm keys f]
  (mm-update! mm (first keys) (fn [v] (update-in v (rest keys) f))))

(defn mm-assoc-in! [mm keys value] (mm-update-in! mm keys (constantly value)))

;;; TODO: write functions for a subscription manager; probably can't
;;; find one, because this is not about events but change notifications.

;;; SUGGESTION: The following code uses STL transactions to coordinate
;;; propagation of information among expressions. It would be possible
;;; to change it to not use transactions, because the task of the code
;;; is to propagate information around, so that everything is
;;; eventually caught up. If it has version numbers so that it can
;;; avoid never replacing newer information with older information,
;;; then changes to information for each expression can be done
;;; independently from changes to others. But doing that is tricky,
;;; such as managing subscriptions to make sure nothing gets lost. For
;;; now, we just use transactions, even though they are slower.

;;; SUGGESTION: if it become important to pass around deltas, the way
;;; to do that is to have value information contain a promise of the
;;; next version of that value and the delta between those versions.
;;; Then, anything with a handle to an old value can chase the change
;;; path, while GC will get rid of change information that no longer
;;; matters to anybody.

;;; TODO: approximations need to be implemented.

;;; TODO: priorities need to be implemented

;;; Methods for ApproximatingScheduler

(defn add-task [scheduler & task]
  "Add a task to the queue of pending tasks. The task is a seq of a
   function and arguments. When run, the scheduler will be threaded in
   as the first argument"
  (dosync
   (alter (:pending scheduler) #(assoc % task 0))))

(defn run-pending-task [scheduler]
  "Execute the topmost task in the queue, if any."
  (let [task
        (dosync (let [queue (:pending scheduler)
                      [task priority] (peek @queue)]
                  (if task
                    (ref-set queue (pop @queue)))
                  task))]
    (if task
      (apply (first task) scheduler (rest task)))))

;;; TODO: write a description of how approximate iterations work, how
;;; they logically unfold the dependency graph, with only the last
;;; unfolding being kept around, but all unfoldings being invalidated
;;; when a non-monotonic input changes or a monotonic input changes in
;;; a non-monotonic way.

;;; In several places, we keep track of dependency information,
;;; reflecting what information we used, and tracking how that differs
;;; from the current version of that information. This is stored in a
;;; map with the keys:
;;;   A :used-info map from expressions that our value depends on to
;;;     a copy of the version of information for that expression that
;;;     is reflected in our value.
;;;   A map of :approximations whose keys are expressions that our
;;;     value depends on where where only a lower bound on their value
;;;     was used in computing our value. The values in the map are a
;;;     pair of which iteration of that value was used, and which
;;;     iteration of the expression's value we need as input if our
;;;     value is to be used in the next iteration. The former can be
;;;     computed from :used-info, but is aggregated here to make
;;;     computation easier. The latter is reset to 0 whenever a
;;;     non-monotonic input changes, because that change invalidates
;;;     all iterations that went through this expression.
;;;   A set :changed-inputs of expressions in
;;;     used-info whose current value does not agree with the value
;;;     in used-info.
;;;   A set of :uncertain-inputs of expressions in used-info whose
;;;     current value agrees with the value in used-info, but which
;;;     might be out of date.

(defn valid? [info]
  "True if all information we know about has been propagated to the value."
  (and (empty? (:changed-inputs info))
       (empty? (:uncertain-inputs info))))

(defn updated-dependency-info [info depends-on-info]
  "Given the current information about an expression, and the latest
   information about an expression that it depends on, update the
   dependency information"
  (let [depends-on (:expression depends-on-info)
        used-info (-> info :used-info depends-on)]
    (if (nil? used-info)
      info ; Our subscription lapsed after the update was scheduled.
      (let [value-changed (not= (:value used-info) (:value depends-on-info))]
        (-> info
            (update-in-collapse-empty [:changed-inputs]
               #(if value-changed
                  ((fnil conj #{}) % depends-on)
                  (empty-to-nil (disj % depends-on))))
            (update-in-collapse-empty [:uncertain-inputs]
               #(if (or value-changed (valid? depends-on-info))
                  (empty-to-nil (disj % depends-on))
                  ((fnil conj #{}) % depends-on))))))))

(defn propagate-change [scheduler expression]
  "The value or currency of the expression has changed.
   Notify everything that depends on it.")

(defn calculate [scheduler expression]
  "We may have the information to compute the current continuation of
   the expression. Check, and calulate if we have the information.")

(defn expression-depends-on-changed [scheduler, expression, depends-on]
  (dosync
   (let [orig-info (mm-get! (:expressions scheduler) expression)
         depends-on-info (mm-get! (:expressions scheduler) depends-on)
         new-info (mm-update! (:expressions scheduler) expression
                              #(updated-dependency-info % depends-on-info))]
     (if (not= (valid? new-info) (valid? orig-info))
       (add-task scheduler propagate-change expression))
     (if (and (not (empty? (:changed-inputs new-info)))
              (empty? (:uncertain-inputs new-info)))
       (calculate scheduler expression)))))

(defrecord
    ^{:doc
      "A Scheduler that can also handle approximations."}
    ApproximatingScheduler

  [;; This holds a mutable map that records expressions and their
   ;; values. The expressions are specified by a form and its
   ;; arguments, all of which are themselves expressions or constants.
   ;; The map is keyed by the expression. Its values are maps that
   ;; record:
   ;;   The :expression, which is also the key to the map.
   ;;   The :value of the expression.
   ;;   A set of :using-expressions for this value.
   ;;   A set of :using-continuations for this value.
   ;;   All the dependency information keys.
   ;;   For internal computations the :continuation to call if we are
   ;;     waiting for arguments.
   ;;   For external computations, the :unsubscribe thunk to call to
   ;;     unsubscribe to changes in the value
   expressions
   
   ;; This holds a mutable map that records computations of functions
   ;; that need to be applied to the values of their arguments. It is
   ;; keyed by the list giving the function and the expressions that
   ;; need to be evaluated. Its values are maps that record:
   ;;   The :continuation, which is also the key to the map
   ;;   The :expression whose value this continuation is calculating
   ;;   A :needed set giving expressions we need to evaluate whose
   ;;     values are not currently valid.
   ;;   All the dependency information keys, reflecting not the expressions
   ;;     needed by this computation, but any that were used in
   ;;     earlier continuations that produced this continuation.
   continuations

   ;; This is holds a list of pending work, mostly updates that need
   ;; to be propagated. Each entry is a sequence of a function and its
   ;; arguments.
   pending
   ]

  Notifier

  (current-value [this expression]
    (apply request this expression)
    ;(finish-computation this)
    (:value (mm-get! (:expressions this) expression)))

  Scheduler

  (ready? [this expression]
    (let [info (mm-get! (:expressions this) expression)]
      (and (= (:currency info) :valid)
           (= (:approximations info) nil))))
)

(defmethod new-approximating-scheduler true []
  (->ApproximatingScheduler (new-mutable-map)
                            (new-mutable-map)
                            (ref (priority-map/priority-map))))

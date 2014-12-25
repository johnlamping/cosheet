(ns cosheet.compute-impl
  (:require [clojure.data.priority-map :as priority-map]
            [clojure.set :as set]
            [cosheet.compute :refer :all]))

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

(defn update-in-clean-up [map keys fn]
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

(defn mm-ref [mm key]
  (mm (mod (hash key) (count mm))))

(defn mm! [fun mm key & args]
  (apply fun @(mm-ref mm key) key args))

(defn mm-in! [fun mm keys & args]
  (apply fun @(mm-ref mm (first keys)) keys args))

(def mm-get! (partial mm! get))
(def mm-get-in! (partial mm-in! get-in))

;;; The next batch must be called inside a transaction.
(defn mm-alter! [fun mm key & args]
  (alter (mm-ref mm key) #(apply fun % key args)))

(defn mm-alter-in! [fun mm keys & args]
  (alter (mm-ref mm (first keys)) #(apply fun % keys args)))

(def mm-update! (partial mm-alter! (fn [map key f] (update-in map [key] f))))
(def mm-update-in! (partial mm-alter-in! update-in))
(def mm-assoc-in! (partial mm-alter-in! assoc-in))
(def mm-dissoc-in! (partial mm-alter-in! dissoc-in))
(def mm-update-in-clean-up! (partial mm-alter-in! update-in-clean-up))

;;; TODO: write functions for a subscription manager; probably can't
;;; find one online, because this is not about events but change
;;; notifications.

;;; SUGGESTION: The following code uses STL transactions to coordinate
;;; propagation of information among expressions. It would be possible
;;; to change it to not use transactions, because the task of the code
;;; is to propagate information, not events, so exact coordination
;;; between subscriber and subscribed is not required. A plausible way
;;; to do this is with a combination of the current system of
;;; remembering a copy of the data that you depended on, together with
;;; a two stage implementation of subscriptions, where first the
;;; subscribing object records that it is interested, and gets put in
;;; a queue of unregistered subscriptions. That queue is gone through,
;;; the subscription entered on the subscribed object, and a check
;;; done at that time to see if a change notification is necessary.

;;; SUGGESTION: if it become important to pass around deltas, the way
;;; to do that is to have value information contain a promise of the
;;; next version of that value and the delta between those versions.
;;; Then, anything with a handle to an old value can chase the change
;;; path, while GC will get rid of change information for which there
;;; are no longer handles.

;;; TODO: approximations need to be implemented.

;;; TODO: priorities need to be implemented

;;; TODO: garbage collection needs to be implemented.

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
                  (when task
                    (ref-set queue (pop @queue)))
                  task))]
    (when task
      (apply (first task) scheduler (rest task)))))

;;; TODO: write a description of how approximate iterations work, how
;;; they logically unfold the dependency graph, with only the last
;;; unfolding being kept around, but all unfoldings being invalidated
;;; when a non-monotonic input changes or a monotonic input changes in
;;; a non-monotonic way.

(defn valid? [info]
  "True if all information we know about has been propagated to the value."
  (and (not (:value-inputs-changed info))
       (empty? (:uncertain-inputs info))))

(defn handle-input-value-changed [scheduler expression]
  "A the value of a dependency has changed. Record that, stop tracking
   dependencies, and cancel any pending computation."
  (dosync
   (let [e-mm (:expressions scheduler)]
     (doseq [depends-on (keys (mm-get-in! e-mm [expression :used-info]))]
       (mm-update-in-clean-up! e-mm [depends-on :using-expressions]
          #(disj % expression)))
     (mm-update-in! e-mm [expression]
       (fn [info] (-> info
                      (assoc :value-inputs-changed true)
                      (dissoc :used-info :uncertain-inputs :computation)))))))

(defn update-dependency-info [scheduler expression depends-on]
  "Given an expression, and an expression that it depends on that may
   have changed, update the dependency information."
  (dosync
   (let [e-mm (:expessions scheduler)
         depends-on-info (mm-get! e-mm depends-on)
         used-info (mm-get-in! e-mm [expression :used-info depends-on])]
     (when (not (nil? used-info))
        ; Our subscription is still present.
       (if (not= (:value used-info) (:value depends-on-info))
         (handle-input-value-changed scheduler expression)
         (mm-update-in-clean-up! e-mm [expression :uncertain-inputs]
                                 #(if (valid? depends-on-info)
                                    (disj % depends-on)
                                    ((fnil conj #{}) % depends-on))))))))

(def expression-depends-on-changed)
(def run-computation)
(def start-evaluation)

(defn expression-changed [scheduler expression]
  (doseq [using (mm-get-in! (:expressions scheduler)
                            [expression :using-expressions])]
    (add-task scheduler expression-depends-on-changed expression))
  ;; TODO: also tell anything that listens to us.
)

(defn set-new-value [scheduler expression value]
  (let [e-mm (:expressions scheduler)
        old-value (mm-get-in! e-mm [expression :value])]
    (mm-update-in! e-mm [expression :value] value)
    (mm-dissoc-in! e-mm [expression :value-inputs-changed])
    (when (not= value old-value)
      (expression-changed scheduler expression))))

(defn state-change-callback [scheduler expression]
  (when-let [state (mm-get-in! (:expressions scheduler) [expression :state])]
    (set-new-value scheduler expression (state-value state))))

(defn process-state-result [scheduler expression result]
  (let [e-mm (:expressions scheduler)
        callback (partial state-change-callback scheduler expression)
        [value unsubscriber] (subscription result callback)]
    (mm-assoc-in! e-mm [expression :state] state)
    (mm-assoc-in! e-mm [expression :unsubsciber] unsubscriber)
    (set-new-value scheduler expression value)))

(defn ensure-expression [scheduler expression]
  (mm-update-in! (:expressions scheduler) [expression]
                 (fn [info] (or info {:value-inputs-changed true})))
  (add-task scheduler start-evaluation expression))

(defn process-continuation-result [scheduler expression result]
  (let [e-mm (:expressions scheduler)
        function (second result)
        arguments (nnext result)
        new-arguments (set/difference
                       (set arguments)
                       (set (keys (mm-get-in! e-mm [expression :used-info]))))]
    (doseq [expression new-arguments]
      (ensure-expression scheduler expression))
    (mm-update-in!
     e-mm [expression :used-info]
     (fn [used] ((fnil into {}) used (map (fn [exp] [exp nil]) new-arguments))))
    (mm-assoc-in! e-mm [expression :computation] (cons function arguments))
    (doseq [depends-on new-arguments]
      (mm-update-in! e-mm [depends-on :using-expressions]
                     #((fnil conj #{}) % expression)))
    (if-let [uncertain (seq (filter #(not (valid? (mm-get! e-mm %)))
                                    arguments))]
      (mm-assoc-in! e-mm [expression :uncertain-inputs] uncertain)
      (run-computation scheduler expression))))

(defn process-result [scheduler expression result]
  "Do the appropriate processing for the result of a computation,
   based on the type of result. Must be called inside a transaction."
  (let [e-mm (:expressions scheduler)
        old-value (mm-get-in! e-mm [expression :value])]
    (cond
      (satisfies? State result)
      (process-state-result scheduler expression result)
      (and (list? result) (= (first result) :continuation))
      (process-continuation-result scheduler expression result)
      :else
      (set-new-value scheduler expression result))))

(defn start-evaluation [scheduler expression]
  "Start the computation that the information calls for.
   There must be no pending computation"
  (dosync
   (mm-update-in!
    (:expressions scheduler) [expression]
    (fn [info](-> info
                  (update-in [:unsubcriber]
                             (fn [thunk] (when thunk (thunk)) nil))
                  (dissoc :state))))
   (process-result scheduler expression
                   (apply (first expression) (rest expression)))))

;;; TODO: This doesn't track dependencies right.
(defn run-computation [scheduler expression]
  "Call when everything the computation depends on is ready. (This
   means both values that it needs, and values used to derive this
   computation.)"
  (dosync
   (let [e-mm (:expressions scheduler)
         computation (mm-get-in! e-mm [expression :computation])
         used-info (or (mm-get-in! e-mm [expression :used-info]) #{})
         function (first computation)
         arguments (rest computation)
         values (map (fn [argument]
                       (let [used (used-info argument)]
                         (if (not (nil? used))
                           used
                           (mm-get-in! e-mm [argument :value]))))
                     arguments)
         values-map (zipmap arguments values)]
     (doseq [argument arguments]
       (mm-update-in! e-mm [expression :used_info argument]
                      (fn [info] (values-map argument))))
     (process-result scheduler expression (apply function values)))))

(defn update-computation [scheduler expression]
  "If we need to start up a new computation for the expession, do it.
   If we have one, run it if it is now ready."
  (dosync
   (let [e-mm (:expressions scheduler)
         info (mm-get! e-mm expression)]
     (if (nil? (:computation info))
       (start-evaluation scheduler expression)
       (when (empty? (:uncertain-inputs info))
         (run-computation scheduler expression))))))

(defn expression-depends-on-changed [scheduler expression depends-on]
  "Note that there has been a change in the information of one of
   the expressions this expression depends on.
   If that might change our information, schedule propagation."
  (dosync
   (let [e-mm (:expressions scheduler)
         orig-info (mm-get! e-mm expression)]
     (update-dependency-info e-mm expression depends-on)
     (update-computation scheduler expression)
     (let [new-info (mm-get! e-mm expression)]
       (when (or (not= (valid? new-info) (valid? orig-info))
                 (not= (:value new-info) (:value orig-info)))
         (expression-changed scheduler expression))))))

(defrecord
    ^{:doc
      "A Scheduler that can also handle approximations."}
    ApproximatingScheduler

  [;; This holds a mutable map that records expressions and their
   ;; values. The expressions are specified by a form and its
   ;; arguments, all of which are themselves expressions or constants.
   ;; The map is keyed by the expression. Its values are maps that
   ;; record:
   ;;   The :value of the expression.
   ;;   A set of :using-expressions that depend on the value.
   ;;   A boolean :value-inputs-changed if some of the dependencies of
   ;;     the value have changed. In that case, the next three fields
   ;;     refer to the continuation.
   ;;   A :used-info map from expressions that are depended on by our
   ;;     lastest computation (continuation if present, otherwise
   ;;     value) to a copy of the version of information for that
   ;;     expression that is reflected in the computation.
   ;;   A set of :uncertain-inputs of expressions in used-info whose
   ;;     current value agrees with the value in used-info, but which
   ;;     might be out of date.
   ;;   A map of :approximations whose keys are expressions that our
   ;;     value depends on where where only a lower bound on their value
   ;;     was used in computing our value. The values in the map are a
   ;;     pair of which iteration of that value was used, and which
   ;;     iteration of the expression's value we need as input if our
   ;;     value is to be used in the next iteration. The former can be
   ;;     computed from :used-info, but is aggregated here to make
   ;;     computation easier. The latter is reset to 0 whenever a
   ;;     non-monotonic input changes, because that change invalidates
   ;;     all iterations that went through this expression.
   ;;   For internal computations the :computation to call if we are
   ;;     waiting for arguments, in the form (function argument ...),
   ;;     where the function is a Clojure function, while the
   ;;     arguments are expressions
   ;;   For external computations, the :state object that holds our value.
   ;;   For external computations, the :unsubscriber thunk to call to
   ;;     unsubscribe to changes in the value
   expressions

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

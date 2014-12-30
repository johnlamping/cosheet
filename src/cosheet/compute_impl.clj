(ns cosheet.compute-impl
  (:require [clojure.set :as set]
            [cosheet.compute :refer :all]
            [cosheet.synchronize :refer :all]
            [cosheet.mutable-map :as mm :refer [dissoc-in update-in-clean-up]]))

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

;;; TODO: write a description of how approximate iterations work, how
;;; they logically unfold the dependency graph, with only the last
;;; unfolding being kept around, but all unfoldings being invalidated
;;; when a non-monotonic input changes or a monotonic input changes in
;;; a non-monotonic way.

(defn valid?
  "True if all information we know about has been propagated to the value."
  [info]
  (and (not (:value-inputs-changed info))
       (empty? (:uncertain-inputs info))))

(defn equal-propagated-info?
  "True if the information that would be propagated from info1 and info2
   is the same."
  [info1 info2]
   (and (= (valid? info1) (valid? info2))
        (= (:value info1) (:value info2))))

;;; The next functions compute revised expression info to reflect new
;;; information. They have no side effects.

(def register-added-depends)
(def register-removed-depends)
(def register-added-state)
(def register-removed-state)

(defn update-add-registration [info path f & args]
  (update-in info [:pending-registrations] #(conj % `[~path ~f ~@args])))

(defn update-remove-state [info]
  (if-let [state (:state info)]
    (-> info
        (dissoc :state)
        (update-add-registration [:state-info] register-removed-state state))
    info))

(defn update-value [info value]
  (assert (not (nil? value)))
  (assoc info :value value))

(defn update-value-from-state [info]
  (if-let [state (:state info)]
    (update-value info (state-value state))
    info))

;;; The next functions handle newly computed results. When they are
;;; called, all used-info must be valid, as far as we know, because we
;;; will consider the result to be valid.

(defn update-state-result [info state]
  (-> (update-remove-state)
      (assoc info :state state)
      (update-add-registration [:state-info] register-added-state state)
      (update-value-from-state)
      (dissoc :value-inputs-changed)))

(defn update-application-result [info application]
  (let [args (rest application)
        needed-args (set/difference
                     (set args) (set (keys (:depends-info info))))]
    (-> info
        (assoc [:needed] needed-args)
        (update-in [:depends-info]
                   #((fnil into {}) % (zipmap needed-args (repeat nil))))
        (update-in [:uncertain-info] #((fnil into #{}) % needed-args))
        (assoc :application application)
        (update-add-registration
         [:depends-info] register-added-depends needed-args))))

(defn update-value-result [info value]
  (-> (update-remove-state)
      (update-value value)
      (dissoc :value-inputs-changed)))

(defn update-result [info result]
  "Do the appropriate update given the result of a computation,
   based on the type of result."
  (cond
    (satisfies? State result)
    (update-state-result info result)
    (and (list? result) (= (first result) :application))
    (update-application-result info (rest result))
    :else
    (update-value-result info result)))

(defn update-start-evaluation
  "Start the computation that the information calls for.
   There must be no pending application and no current state."
  [info [form & args]]
  (update-result info (apply form args)))

(defn update-run-application
  "All the information we depend on is valid, as far as we have heard,
   and we have a waiting application. Run it."
  [info]
  (let [[f & args] (:application info)
        arg-info (:depends-info info)]
    (-> info
        (dissoc :needed)
        (update-result (apply f (map #(-> % arg-info :value) args))))))

(defn update-run-application-while-ready [info]
  (loop [latest-info info]
    (if (and (not (nil? (:application latest-info)))
             (empty? (:uncertain-info latest-info))
             (empty? (:pending-registrations latest-info)))
      (recur (update-run-application latest-info))
      latest-info)))

(defn update-used-value-changed [info]
  "A value that we used in our computation has changed, which means
   that we need to start computation all over again. Record that the
   current value is not valid via :value-inputs-changed, stop tracking
   dependencies, cancel our state and any pending application, and
   start the computation again."
  (-> info
      (assoc :value-inputs-changed true)
      (update-remove-state)
      (update-add-registration [:depends-info] register-removed-depends
                               (keys (:depends-info info)))
      (dissoc :depends-info :needed :uncertain-inputs :application)
      (update-start-evaluation)))

(defn update-depends
  "The propagated information for an expression that we may depend on
  may have changed, update what we know about it."
  [info depends-on depends-on-info]
  (if (contains? (:depends-info info) depends-on)
    (let [valid (valid? depends-on-info)
          revised-info
          (update-in-clean-up
           info [:uncertain-inputs]
           (if valid #(disj % depends-on) #((fnil conj #{}) % depends-on)))]
      (if (contains? (:needed info) depends-on)
        (assoc-in revised-info [:depends-info depends-on]
                  (:value depends-on-info))
        (if (not= (:value (-> info :depends-info depends-on))
                  (:value depends-on-info))
          (update-used-value-changed info))))
    info))

(def handle-subscribed-expression-changed)
(def change-and-propagate)

;;; The next functions handle :pending-registrations

(defn ensure-expression
  "If we have no information about the expression, start evaluating it."
  [scheduler expression]
  (let [e-mm (:expressions scheduler)]
    (when (nil? (mm/get! e-mm expression))
      (mm/update-in!
       e-mm [expression]
       (fn [info] (or info (update-start-evaluation
                            {:value-inputs-changed true} expression)))))))

(defn register-added-depends
  "Given the current value of :depends-info of an expression and a
  collection of expressions that have been added to it, make sure the
  expression :using-expressions field in each of the added expressions
  that are still used to accord with whether the expression is in
  :depends-info, and schedule a change propagation if we used it and
  the current value doesn't agree with what we used."
  [current-depends-info scheduler expression added-expressions]
  (let [e-mm (:expressions scheduler)]
    (doseq [added-expression added-expressions]
      (when (contains? current-depends-info added-expression)
        (ensure-expression scheduler added-expression)
        (mm/update-in! e-mm [added-expression :using-expressions]
                       #((fnil conj #{}) % expression))
        (when (not (equal-propagated-info?
                    (current-depends-info added-expression)
                    (mm/get! e-mm added-expression)))
          (add-task scheduler
                    handle-subscribed-expression-changed
                    scheduler expression added-expression))))))

(defn register-removed-depends
  "Given the current value of :used-expressions of an expression and a
   collection of expressions that have been removed from it, update the
   :using-expressions field in each of those expressions to accord with
   whether the expression is not it :used-expressions."
  [current-depends-info scheduler expression removed-expressions]
  (let [e-mm (:expressions scheduler)]
    (doseq [removed-expression removed-expressions]
      (when (not (contains? current-depends-info removed-expression))
        (mm/update-in-clean-up! e-mm [removed-expression :using-expressions]
                                #(disj % expression))))))

(defn state-change-callback [scheduler expression]
  (change-and-propagate scheduler expression update-value-from-state))

(defn register-added-state
  "Given the current state object, and an object that may have become
   the state, if they are the same, subscribe to the state, and if the
   state's value is not the same as the currently recorded one,
   schedule a change propagation."
  [current-state scheduler expression added-state]
  (let [e-mm (:expressions scheduler)]
    (when (= (current-state added-state))
      (let [value (subscribe current-state state-change-callback
                             scheduler expression)]
        (if (not= value (mm/get-in! e-mm [expression :value]))
          (change-and-propagate
           scheduler expression update-value-from-state))))))

(defn register-removed-state
  "Given the current state object, and an object that may have been
   the state, if they are different, unsubscribe from the removed state."
  [current-state scheduler expression removed-state]
  (let [e-mm (:expressions scheduler)]
    (if (not= (current-state removed-state))
      (unsubscribe removed-state
                   state-change-callback scheduler expression))))

(defn propagate-expression-info-changed
  "The information for the expression has changed. Tell anyone else
   who cares."
  [scheduler expression]
  ;; TODO: also tell anything that listens to us.
  (doseq [using (mm/get-in! (:expressions scheduler)
                            [expression :using-expressions])]
    (add-task scheduler handle-subscribed-expression-changed expression)))

(defn execute-registrations-and-application
  "Run a collection of registrations."
  [registrations scheduler expression]
  (let [e-mm (:expressions scheduler)]
    (doseq [[path f & args] registrations]
      (apply mm/call-with-latest-value-in!
             e-mm path f scheduler expression args))
    (mm/update! e-mm expression update-run-application-while-ready)))

(defn change-and-propagate
  "Run the function on the information for the expression, and replace
   the information with the result of the function. If the value or
   validity of the expression has changed, propagate that."
  [scheduler expression f & args]
  (let [e-mm (:expressions scheduler)
        [old-info new-info] (apply mm/update-in-returning-both!
                                   e-mm [expression] f args)]
    (mm/call-and-clear-in! e-mm [expression :pending-registrations]
                           execute-registrations-and-application
                           scheduler expression)
    (when (not (equal-propagated-info? (mm/get! e-mm expression new-info)
                                       old-info))
      (propagate-expression-info-changed scheduler expression))))

(defn handle-subscribed-expression-changed [scheduler expression depends-on]
  "Note that there has been a change in the information of one of
   the expressions this expression depends on.
   If that might change our information, schedule propagation."
  (change-and-propagate scheduler expression update-depends
                        depends-on
                        (mm/get! (:expressions scheduler) depends-on)))

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
   ;;     the value have changed. In that case, there will be a
   ;;     pending application, and the next three fields pertain to
   ;;     it. Unlike the later fields, which pertain to the pending
   ;;     application, if there is one, this field always pertains to
   ;;     the current value.
   ;;   A :depends-info map from expressions that were used or are
   ;;      needed by our computations so far (application if present,
   ;;      otherwise value) to a copy of the version of information
   ;;      for that expression that is reflected in the computation.
   ;;   A :needed subset of the keys of :depends-info that have not
   ;;     been used yet.
   ;;   A :pending-registrations set of registrations of information
   ;;     from this expression that may need to be reflected
   ;;     elsewhere, and whose registration might cause our
   ;;     information to change. They are of the form [path function &
   ;;     args] where the path navigates from the information for this
   ;;     expression to the information that needs to be reflected. We
   ;;     will mm/call-with-latest-value-in! arranging for the
   ;;     function to be called with the scheduler, this expression,
   ;;     the value of the information, and the additional args.
   ;;   A set of :uncertain-inputs of expressions in :depends-info
   ;;     whose current value agrees with the recorded value, but
   ;;     which might be out of date.
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
   ;;   For internal computations the :application to call if we are
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
    (:value (mm/get! (:expressions this) expression)))

  Scheduler

  (ready? [this expression]
    (let [info (mm/get! (:expressions this) expression)]
      (and (= (:currency info) :valid)
           (= (:approximations info) nil))))
)

(defmethod new-approximating-scheduler true []
  (->ApproximatingScheduler (mm/new-mutable-map)
                            (mm/new-mutable-map)
                            (new-priority-task-queue)))

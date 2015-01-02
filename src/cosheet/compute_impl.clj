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
  (and (:value-depends-unchanged info)
       (empty? (:uncertain-depends info))))

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
  (update-in info [:pending-registrations]
             #((fnil conj []) % `[~path ~f ~@args])))

(defn update-remove-state [info]
  (if-let [state (:state info)]
    (-> info
        (dissoc :state)
        (update-add-registration [:state] register-removed-state state))
    info))

(defn update-value [info value]
  (assert (not (nil? value)))
  (assoc info :value value))

(defn update-value-from-state [info]
  (if-let [state (:state info)]
    (update-value info (state-value state))
    info))

(letfn
    [(update-state-result [info state]
       (-> info
           (update-remove-state)
           (assoc :state state)
           (update-add-registration [:state] register-added-state state)
           (update-value-from-state)
           (assoc :value-depends-unchanged true)))
     
     (update-application-result [info application]
       (let [args (rest application)
             needed-args (set/difference
                          (set args) (set (keys (:depends-info info))))]
         (->
          (if (empty? needed-args)
            info
            (-> info
                (update-in
                 [:depends-info]
                 #((fnil into {}) % (zipmap needed-args (repeat nil))))
                (assoc :unused-depends needed-args)
                (assoc :uncertain-depends needed-args)
                (update-add-registration
                 [:depends-info] register-added-depends needed-args)))
          (assoc :application application))))
     
     (update-value-result [info value]
       (-> info
           (update-remove-state)
           (update-value value)
           (assoc :value-depends-unchanged true)))]
  
  (defn update-result [info result]
    "Do the appropriate update given the result of a computation,
     based on the type of result."
    (assert (empty? (:uncertain-depends info)))
    (cond
      (satisfies? State result)
      (update-state-result info result)
      (and (list? result) (= (first result) :application))
      (update-application-result info (rest result))
      :else
      (update-value-result info result))))

(letfn
    [(update-run-application [info]
       (let [[f & args] (:application info)
             arg-info (:depends-info info)]
         (-> info
             (dissoc :unused-depends :application)
             (update-result (apply f (map #(-> % arg-info :value) args))))))]
  
  (defn update-run-application-while-ready [info]
    (loop [latest-info info]
      (if (and (not (nil? (:application latest-info)))
               (empty? (:uncertain-depends latest-info))
               (empty? (:pending-registrations latest-info)))
        (recur (update-run-application latest-info))
        latest-info))))

(defn update-start-evaluation
  "Start the computation that the information calls for.
   There must be no pending application and no current state."
  [info [form & args]]
  (update-result info (apply form args)))

(defn update-initialize-if-needed
  "If info is nil, return an initialized info."
  [info expression]
  (or info (update-start-evaluation {} expression)))

(letfn
    ;; A value that we used in our computation has changed, which
    ;; means that we need to start computation all over again. Record
    ;; that the current value is not valid via :value-depends-unchanged,
    ;; stop tracking dependencies, cancel our state and any pending
    ;; application, and start the computation again.
    [(update-used-value-changed [info expression]
       (-> info
           (dissoc :value-depends-unchanged)
           (update-remove-state)
           (update-add-registration [:depends-info] register-removed-depends
                                    (keys (:depends-info info)))
           (dissoc :depends-info :unused-depends :uncertain-depends
                   :application)
           (update-start-evaluation expression)))

     ;; update :uncertain-depends to reflect the validity of the
     ;; depended on information
     (update-uncertain [info depends-on depends-on-info]
       (if (valid? depends-on-info)
         (update-in-clean-up info [:uncertain-depends]
                             #(disj % depends-on))
         (update-in info [:uncertain-depends]
                    #((fnil conj #{}) % depends-on))))]

  (defn update-depends-on-info-changed
    "The propagated information for an expression that we may depend on
     may have changed, update what we know about it."
    [info expression depends-on depends-on-info]
    (if (contains? (:depends-info info) depends-on)
      (let [revised-info (update-uncertain info depends-on depends-on-info)]
        (if (contains? (:unused-depends revised-info) depends-on)
          (assoc-in revised-info [:depends-info depends-on] depends-on-info)
          (if (= (:value (-> revised-info :depends-info depends-on))
                 (:value depends-on-info))
            revised-info
            (update-used-value-changed revised-info expression))))
      info)))

(def handle-depends-on-info-changed)

(letfn
    ;; Run a collection of registrations
    [(run-registrations [registrations scheduler expression]
       (let [e-mm (:expressions scheduler)]
         (doseq [[path f & args] registrations]
           (apply mm/call-with-latest-value-in!
                  e-mm (concat [expression] path)
                  f scheduler expression args))))

      ;; The information for the expression has changed. Tell anyone else
      ;; who cares.
     (schedule-propagations [scheduler expression]
       ;; TODO: also tell anything that listens to us.
       (doseq [using (mm/get-in! (:expressions scheduler)
                                 [expression :using-expressions])]
         (add-task (:pending scheduler)
                   handle-depends-on-info-changed using expression)))]

  (defn change-and-schedule-propagation
    "Run the function on the information for the expression, and replace
    the information with the result of the function. If the value or
    validity of the expression has changed, propagate that."
    [scheduler expression f & args]
    (let [e-mm (:expressions scheduler)
          [old-info new-info] (apply mm/update-in-returning-both!
                                     e-mm [expression] f args)]
      (loop []
        (mm/call-and-clear-in! e-mm [expression :pending-registrations]
                               run-registrations scheduler expression)
        (let [[old new] (mm/update-in-returning-both!
                         e-mm [expression] update-run-application-while-ready)]
          (if (not= old new)
            (recur))))
      (when (not (equal-propagated-info? (mm/get! e-mm expression) old-info))
        (schedule-propagations scheduler expression)))))

(defn handle-depends-on-info-changed [scheduler expression depends-on]
  "Record in this expression the latest information for of one of the
   expressions this expression depends on. If that changes its
   information, schedule propagation."
  (change-and-schedule-propagation
   scheduler expression
   update-depends-on-info-changed
   expression depends-on (mm/get! (:expressions scheduler) depends-on)))

(defn handle-state-changed [scheduler expression]
  "Record in this expression the latest information for its state. If
   that changes its information, schedule propagation."
  (change-and-schedule-propagation
   scheduler expression update-value-from-state))

;;; The next functions handle :pending-registrations. They are
;;; designed to be idempotent, so they can be called as long as there
;;; is any indication that things may be out of date. But they do not
;;; perform their updates atomically, so their effects could already
;;; be out of date by the time they return. To address that, they
;;; should always be called from a call-with-latest-value-in!

(defn register-added-depends
  "Given the current value of :depends-info of an expression and a
   collection of expressions that have been added to it, make sure the
   :using-expressions field in each of the added expressions that are
   still used contains the expression, and schedule a change
   propagation if we used it and the current information doesn't agree
   with what we used."
  [current-depends-info scheduler expression added-expressions]
  (let [e-mm (:expressions scheduler)]
    (doseq [added-expression added-expressions]
      (when (contains? current-depends-info added-expression)
        (mm/update-in! e-mm [added-expression]
                       (fn [info]
                         (-> info
                             (update-initialize-if-needed added-expression)
                             (update-in [:using-expressions] 
                                     #((fnil conj #{}) % expression)))))
        ;; If there is already information about the value,
        ;; schedule a propagation.
        (when (not (equal-propagated-info?
                    (current-depends-info added-expression)
                    (mm/get! e-mm added-expression)))
          (add-task (:pending scheduler)
                    handle-depends-on-info-changed
                    expression added-expression))))))

(defn register-removed-depends
  "Given the current value of :depends-info of an expression and a
   collection of expressions that have been removed from it, make sure
   the :using-expressions field in each of those expressions doesn't
   list the expression if they are not depended on."
  [current-depends-info scheduler expression removed-expressions]
  (let [e-mm (:expressions scheduler)]
    (doseq [removed-expression removed-expressions]
      (when (not (contains? current-depends-info removed-expression))
        (mm/update-in-clean-up! e-mm [removed-expression :using-expressions]
                                #(disj % expression))))))

(letfn
    [(state-change-callback [scheduler expression]
       (add-task (:pending scheduler) handle-state-changed expression))]

  (defn register-added-state
    "Given the current state object, and an object that may have become
    the state, if they are the same, subscribe to the state, and if the
    state's value is not the same as the currently recorded one,
    schedule change propagation."
    [current-state scheduler expression added-state]
    (let [e-mm (:expressions scheduler)]
      (when (= current-state added-state)
        (let [value (subscribe current-state
                               [state-change-callback scheduler expression])]
          (if (not= value (mm/get-in! e-mm [expression :value]))
            (add-task (:pending scheduler) handle-state-changed expression))))))

  (defn register-removed-state
    "Given the current state object, and an object that may have been
    the state, if they are different, unsubscribe from the removed state."
    [current-state scheduler expression removed-state]
    (let [e-mm (:expressions scheduler)]
      (if (not= current-state removed-state)
        (unsubscribe removed-state
                     [state-change-callback scheduler expression])))))

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
   ;;   A boolean :value-depends-unchanged if none of the dependencies of
   ;;     the value have changed. Otherwise, there will be a
   ;;     pending application, and the next three fields pertain to
   ;;     it. Unlike the later fields, which pertain to the pending
   ;;     application, if there is one, this field always pertains to
   ;;     the current value.
   ;;   A :depends-info map from expressions that were used or are
   ;;      needed by our computations so far (application if present,
   ;;      otherwise value) to a copy of the version of information
   ;;      for that expression that is reflected in the computation.
   ;;   A :unused-depends subset of the keys of :depends-info that have not
   ;;     been used yet but are needed by the current application.
   ;;   A set of :uncertain-depends of expressions in :depends-info
   ;;     whose current value agrees with the recorded value, but
   ;;     which might be out of date.
   ;;   A :pending-registrations set of registrations of information
   ;;     from this expression that may need to be reflected
   ;;     elsewhere, and whose registration might cause our
   ;;     information to change. They are of the form [path function &
   ;;     args] where the path navigates from the information for this
   ;;     expression to the information that needs to be reflected. We
   ;;     will mm/call-with-latest-value-in! arranging for the
   ;;     function to be called with the scheduler, this expression,
   ;;     the value of the information, and the additional args.
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
   ;; arguments. When the function is called, the scheduler will be
   ;; prepended to the arguments.
   pending
   ]

  Notifier

  (current-value [this expression]
    (apply request this expression)
    ;(finish-computation this)
    (:value (mm/get! [:expressions this] expression)))

  Scheduler

  (ready? [this expression]
    (valid? (mm/get! [:expressions this] expression)))
  )

(defmethod clojure.core/print-method ApproximatingScheduler

  [scheduler writer]
  (.write writer "<ApproximatingScheduler>"))

(defmethod new-approximating-scheduler true []
  (->ApproximatingScheduler (mm/new-mutable-map)
                            (new-priority-task-queue)))

(ns cosheet.compute-impl
  (:require [clojure.set :as set]
            (cosheet [compute :refer :all]
                     [state :refer :all]
                     [task-queue :refer :all]
                     [utils :refer [dissoc-in update-in-clean-up]]
                     [mutable-map :as mm])))

;;; This is the definition of an approximating scheduler (although the
;;; implementation doesn't yet do approximation). Most of the code is
;;; functions named update-* that compute new versions of immutable
;;; information about expressions. The rest of the code manages a
;;; mutable map that stores this information for each expression. It
;;; is mostly concerned with propagating information for one
;;; expression to another.

;;; The propagation is multi-threaded, but can avoid using locks and
;;; TSM because it just needs eventual consistency; it is just copying
;;; information. The danger is that in between one thread reading and
;;; then writing, an update to the read information will happen in
;;; another thread, and the updated information copied, only to be
;;; overwritten by old information by the first thread. There are two
;;; ways to avoid this. One is to read the information inside the
;;; atomic operation in a thread, so that another write will cause a
;;; conflict and get redone. The other is to check, after doing a
;;; copy, that the information that was copied still matches the
;;; latest information, and redo the copy if it doesn't. This code
;;; uses both techniques, the former for copying external state, and
;;; the latter for copying information between expressions.

;;; TODO: Make the code use just the former technique, by having the
;;; updated that copy information take a thunk that will read the
;;; information inside the atomic operation.

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

(defn eval-and-call? [expr]
  (and (list? expr) (= (first expr) :eval-and-call)))

(defn eval-and-call-tracer [expr]
  (second expr))

(defn eval-and-call-fn [expr]
  (nth expr 2))

(defn eval-and-call-args [expr]
  (seq (nthnext expr 3)))

(defn current-value-impl
  [[fn & args]]
  (let [result (apply fn args)]
    (cond
      (state? result)
      (state-value result)
      (eval-and-call? result)
      ((eval-and-call-tracer result)
       #(current-value-impl (cons (eval-and-call-fn result)
                                  (map current-value-impl
                                       (eval-and-call-args result)))))
      :else
      result)))

(defmethod current-value true [expression]
  (current-value-impl expression))

;;; The next functions compute revised expression info to reflect new
;;; information. They have no side effects.

(def register-different-depends)
(def register-different-state)

(defn update-add-action [info f & args]
  (update-in info [:pending-actions]
             #((fnil conj []) % `[~f ~@args])))

(defn update-remove-state [info]
  (if-let [state (:state info)]
    (-> info
        (dissoc :state)
        (update-add-action register-different-state state))
    info))

(defn update-valid [info]
  (let [valid (and (not (:value-depends-changed info))
                   (empty? (:uncertain-depends info)))]
    (update-in-clean-up info [:visible :valid]
                        (constantly (if valid true nil)))))

(defn update-value [info value]
  (assert (not (nil? value)))
  (assoc-in info [:visible :value] value))

(defn update-value-from-state [info]
  (if-let [state (:state info)]
    (update-value info (state-value state))
    info))

(letfn
    [(update-state-result [info state]
       (-> info
           (update-remove-state)
           (assoc :state state)
           (update-add-action register-different-state state)
           (update-value-from-state)
           (dissoc :value-depends-changed)
           (update-valid)))
     
     (update-eval-and-call-result [info fn args]
       (let [needed-args (set/difference
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
                (update-add-action register-different-depends needed-args)))
          (assoc :eval-and-call (cons fn args)))))
     
     (update-value-result [info value]
       (-> info
           (update-remove-state)
           (update-value value)
           (dissoc :value-depends-changed)
           (update-valid)))]
  
  (defn update-result [info result]
    "Do the appropriate update given the result of a computation,
     based on the type of result."
    (assert (empty? (:uncertain-depends info)))
    (assert (empty? (:unused-depends info)))
    (cond
      (state? result)
      (update-state-result info result)
      (eval-and-call? result)
      (update-eval-and-call-result
       info (eval-and-call-fn result) (eval-and-call-args result))
      :else
      (update-value-result info result))))

(letfn
    [(update-run-eval-and-call [info]
       (let [[f & args] (:eval-and-call info)
             arg-info (:depends-info info)]
         (-> info
             (dissoc :unused-depends :eval-and-call)
             (update-result (apply f (map #(-> % arg-info :value) args))))))]
  
  (defn update-run-eval-and-call-while-ready [info]
    (loop [latest-info info]
      (if (and (not (nil? (:eval-and-call latest-info)))
               (empty? (:uncertain-depends latest-info)))
        (recur (update-run-eval-and-call latest-info))
        latest-info))))

(defn update-start-evaluation
  "Start the computation that the information calls for.
   There must be no pending eval-and-call and no current state."
  [info [form & args]]
  (-> info
      (assoc :value-depends-changed true)
      (update-valid)
      (update-result (apply form args))))

(defn update-initialize-if-needed
  "If info is nil, return an initialized info."
  [info expression]
  (or info (update-start-evaluation {:visible {}} expression)))

(letfn
    ;; A value that we used in our computation has changed, which
    ;; means that we need to start computation all over again. Record
    ;; that the current value is not valid via :value-depends-changed,
    ;; stop tracking dependencies, cancel our state and any pending
    ;; eval-and-call, and start the computation again.
    [(update-used-value-changed [info expression]
       (-> info
           (assoc :value-depends-changed true)
           (update-valid)
           (update-remove-state)
           (update-add-action register-different-depends
                                    (keys (:depends-info info)))
           (dissoc :depends-info :unused-depends :uncertain-depends
                   :eval-and-call)
           (update-start-evaluation expression)))

     ;; update :uncertain-depends to reflect the validity of the
     ;; depended on information
     (update-uncertain [info depends-on depends-on-visible]
       (update-valid (if (:valid depends-on-visible)
                       (update-in-clean-up info [:uncertain-depends]
                                           #(disj % depends-on))
                       (update-in info [:uncertain-depends]
                                  #((fnil conj #{}) % depends-on)))))]

  (defn update-depends-on-visible
    "The visible information for an expression that we may depend on
     may have changed, update what we know about it, given a function
     that fetches it. Using a function allows the fetch do be done as part
     of an atomic update of our info, so that we won't be saving
     old information."
    [info expression depends-on depends-on-visible-getter]
    (if (contains? (:depends-info info) depends-on)
      (let [depends-on-visible (depends-on-visible-getter)
            revised-info (update-uncertain info depends-on depends-on-visible)]
        (if (or (contains? (:unused-depends revised-info) depends-on)
                (= (:value ((or (:depends-info revised-info) {}) depends-on))
                   (:value depends-on-visible)))
          (assoc-in revised-info [:depends-info depends-on] depends-on-visible)
          (update-used-value-changed revised-info expression)))
      info)))

(def handle-depends-on-info-changed)

(letfn
    ;; Run a collection of actions
    [(run-actions [actions scheduler expression]
       (doseq [[f & args] actions]
         (apply f scheduler expression args)))

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
          [old new] (mm/update-in-returning-both!
                     e-mm [expression]
                     (fn [info] (update-run-eval-and-call-while-ready
                                 (apply f info args))))]
      (mm/call-and-clear-in! e-mm [expression :pending-actions]
                             run-actions scheduler expression)

      (when (not= (:visible old) (:visible new))
        (schedule-propagations scheduler expression)))))

(defn handle-depends-on-info-changed [scheduler expression depends-on]
  "Record in this expression the latest information for of one of the
   expressions this expression depends on. If that changes its
   information, schedule propagation."
  ;; Since we are passing in information from another expression, we
  ;; have to keep running until we have passed in the latest
  ;; information, because an earlier run of ours may have stomped on
  ;; more recent information.
  (change-and-schedule-propagation
   scheduler expression
   update-depends-on-visible expression depends-on
   #(mm/get-in! (:expressions scheduler) [depends-on :visible])))

(defn handle-state-changed [scheduler expression]
  "Record in this expression the latest information for its state. If
   that changes its information, schedule propagation."
  (change-and-schedule-propagation
   scheduler expression update-value-from-state))

;;; The next functions handle :pending-actions. They are
;;; designed to be idempotent, so they can be called as long as there
;;; is any indication that things may be out of date.

;;; Notice that both additions and removals must be handled by the
;;; same pending registration. Otherwise, an add might occur in the
;;; middle of the execution of a removal, and the removal would remove
;;; it and when finished, wouldn't notice that an add was now necessary.

(letfn
    ;; Given the current value of :depends-info of an expression and
    ;; an expressions that is in it and may have been added to it,
    ;; make sure the :using-expressions field in the added expression
    ;; contains the expression, and schedule a change propagation if
    ;; we used it and the current information doesn't agree with what
    ;; we used.
    [(register-added-depends
       [current-depends-info scheduler expression added-expression]
       (let [e-mm (:expressions scheduler)]
         (change-and-schedule-propagation
          scheduler added-expression
          (fn [info]
            (-> info
                (update-initialize-if-needed added-expression)
                (update-in [:using-expressions] 
                           #((fnil conj #{}) % expression)))))
         ;; If there is already information about the value,
         ;; schedule a propagation.
         (when (not (= (current-depends-info added-expression)
                       (mm/get-in! e-mm [added-expression :visible])))
           (add-task (:pending scheduler)
                     handle-depends-on-info-changed
                     expression added-expression))))

     ;; Given the current value of :depends-info of an expression and
     ;; an expression that is not in it and may have been removed from
     ;; it, make sure the :using-expressions field in the removed
     ;; expressions doesn't list the expression. 
     (register-removed-depends
       [scheduler expression removed-expression]
       (let [e-mm (:expressions scheduler)]
         (mm/update-in-clean-up! e-mm [removed-expression :using-expressions]
                                 #(disj % expression))))]

  (defn register-different-depends
    "Given the current value of :depends-info of an expression and a
     collection of expressions that have been added to it or removed from
     it, make sure the :using-expressions field in each of those
     expressions reflects the depends-info, and schedule change
     propagation is appropriate."
    [scheduler expression changed-expressions]
     (mm/call-with-latest-value-in!
      (:expressions scheduler) [expression :depends-info]
      (fn [current-depends-info]
        (doseq [changed-expression changed-expressions]
          (if (contains? current-depends-info changed-expression)
            (register-added-depends
             current-depends-info scheduler expression changed-expression)
            (register-removed-depends
             scheduler expression changed-expression)))))))

(letfn
    [(state-change-callback [value state scheduler expression]
       (add-task (:pending scheduler) handle-state-changed expression))

     (register-added-state
       [current-state scheduler expression]
       (let [e-mm (:expressions scheduler)]
         (let [value (subscribe current-state
                                state-change-callback scheduler expression)]
           (if (not= value (mm/get-in! e-mm [expression :visible :value]))
             (add-task (:pending scheduler) handle-state-changed expression)))))

     (register-removed-state
       [scheduler expression removed-state]
       (unsubscribe removed-state
                    state-change-callback scheduler expression))]

  (defn register-different-state
   "Given the current state object, and an object that may be or may
    have been the state, unsubscribe from the state of interest if it is
    not the current state, and subscribe to it if it is. For a
    subscription, if the state's value is not the same as the currently
    recorded one, schedule change propagation."
   [scheduler expression state-of-interest]
    (mm/call-with-latest-value-in!
     (:expressions scheduler) [expression :state]
     (fn [current-state]
       (if (= current-state state-of-interest)
         (when (not (nil? current-state))
           (register-added-state current-state scheduler expression))
         (when (not (nil? state-of-interest))
           (register-removed-state scheduler expression state-of-interest)))))))

(defn run-all-pending [scheduler]
  (loop []
    (when (run-pending-task (:pending scheduler) scheduler)
      (recur))))

(defn scheduler-summary
  "Return a map that describes the content of a scheduler
   but doesn't have deep nesting."
  [scheduler]
  (let [e-mm (:expressions scheduler)
        expressions (keys (mm/current-contents e-mm))]
    {:expressions
     (zipmap expressions
             (for [expression expressions]
               (let [info (mm/get! e-mm expression)]
                 (let [tags (keys info)]
                   (zipmap tags
                           (for [tag tags]
                             (let [info (info tag)]
                               (if (= tag :state)
                                 `("state" ~(state-value info))
                                 info))))))))
     :pending @(:pending scheduler)}))

(defrecord
    ^{:doc
      "A Scheduler that can also handle approximations."}
    ApproximatingScheduler

  [;; This holds a mutable map that records expressions and their
   ;; values. The expressions are specified by a form, which must be
   ;; a function and its arguments, which can be anything.
   ;; The map is keyed by the expression. Its values are maps that
   ;; record:
   ;;   A :visible map of what other expressions see about this
   ;;     expression, consisting of
   ;;     The :value of the expression.
   ;;     A :valid tag if the value is valid
   ;;   A set of :using-expressions that depend on the value.
   ;;   A boolean :value-depends-changed if some of the dependencies of
   ;;     the value have changed. In this case, there will be a
   ;;     pending eval-and-call, and the next three fields pertain to
   ;;     it. Unlike the later fields, which pertain to the pending
   ;;     eval-and-call, if there is one, this field always pertains to
   ;;     the current value.
   ;;   A :depends-info map from expressions that were used or are
   ;;      needed by our computations so far (eval-and-call if present,
   ;;      otherwise value) to a copy of the version of information
   ;;      for that expression that is reflected in the computation.
   ;;   A :unused-depends subset of the keys of :depends-info that have not
   ;;     been used yet but are needed by the current eval-and-call.
   ;;   A set of :uncertain-depends of expressions in :depends-info
   ;;     whose current value agrees with the recorded value, but
   ;;     which might be out of date.
   ;;   A :pending-actions set of actions to take based on new information
   ;;     about this expression. This is a way for a purely functional
   ;;     update to the information for one expression to request
   ;;     changes to other information. The actions are of the form
   ;;     [function & args]. The functionn will be called with the scheduler,
   ;;     this expression, and the additional args.
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
   ;;   For internal computations the :eval-and-call to call if we are
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

  (notifier-value [this expression]
    (request this expression)
    (run-all-pending this)
    (mm/get-in! (:expressions this) [expression :visible :value]))

  Scheduler

  (request [this expression]
    (change-and-schedule-propagation this expression
                                     update-initialize-if-needed expression))

  (ready? [this expression]
    (mm/get! (:expressions this) [expression :visible :valid]))
  )

(defmethod clojure.core/print-method ApproximatingScheduler
  [scheduler writer]
  (.write writer "<ApproximatingScheduler>"))

(defmethod new-approximating-scheduler true []
  (->ApproximatingScheduler (mm/new-mutable-map)
                            (new-priority-task-queue)))

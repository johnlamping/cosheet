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
;;; information about references. The rest of the code manages a
;;; mutable map that stores this information for each reference. It
;;; is mostly concerned with propagating information for one
;;; reference to another.

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
;;; uses the former technique as much as possible, but currently needs
;;; to use the latter for subscribing to state, because State objects
;;; don't have a conditional subscription method that consults a thunk
;;; to decide whether to subscribe.

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

(defn expression? [expr]
  (and (list? expr) (= (first expr) :expression)))

(defn expression-tracer [expr]
  (second expr))

(defn expression-fn [expr]
  (nth expr 2))

(defn expression-args [expr]
  (seq (nthnext expr 3)))

(defn current-value-impl
  [[fn & args]]
  (let [result (apply fn args)]
    (cond
      (state? result)
      (state-value result)
      (expression? result)
      ((expression-tracer result)
       #(current-value-impl (cons (expression-fn result)
                                  (map current-value-impl
                                       (expression-args result)))))
      :else
      result)))

(defmethod current-value true [reference]
  (current-value-impl reference))

;;; The next functions compute revised reference info to reflect new
;;; information. They have no side effects.

(def register-different-depends)
(def register-different-state)
(def copy-visible-to-user)

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
     
     (update-expression-result [info fn args]
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
                ;; We don't get the values yet, as we have to copy
                ;; them anyway, once updates are set up.
                (update-add-action register-different-depends needed-args)))
          (assoc :expression (cons fn args)))))
     
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
      (expression? result)
      (update-expression-result
       info (expression-fn result) (expression-args result))
      :else
      (update-value-result info result))))

(letfn
    [(update-run-expression [info]
       (let [[f & args] (:expression info)
             arg-info (:depends-info info)]
         (-> info
             (dissoc :unused-depends :expression)
             (update-result (apply f (map #(-> % arg-info :value) args))))))]
  
  (defn update-run-expression-while-ready [info]
    (loop [latest-info info]
      (if (and (not (nil? (:expression latest-info)))
               (empty? (:uncertain-depends latest-info)))
        (recur (update-run-expression latest-info))
        latest-info))))

(defn update-start-evaluation
  "Start the computation that the information calls for.
   There must be no pending expression and no current state."
  [info [form & args]]
  (-> info
      (assoc :value-depends-changed true)
      (update-valid)
      (update-result (apply form args))))

(defn update-initialize-if-needed
  "If info is nil, return an initialized info."
  [info reference]
  (or info (update-start-evaluation {:visible {}} reference)))

(defn update-adjust-using
  "Update the :using-references set for this reference to reflect
   whether the specified possibly using reference does, in fact, use us.
   The depends-on-getter must return the current value of the depends-on
   field of the possibly using reference. By calling the getter inside
   the update, inside an atomic operation, we make sure we are not
   overwriting new information with old."
  [info reference possibly-using depends-on-getter]
  (let [usings-depends-info (depends-on-getter)]
    (if (contains? usings-depends-info reference)
      ;; It uses us.
      (as-> info info
          (update-initialize-if-needed info reference)
          (update-in info [:using-references] 
                     #((fnil conj #{}) % possibly-using))
          ;; If we have different information, set up to copy it.
          (if (= (usings-depends-info reference)
                 (:visible info))
            info
            (update-add-action info copy-visible-to-user possibly-using)))
      ;; It doesn't use us.
      (update-in-clean-up info [:using-references]
                          #(disj % possibly-using)))))

(letfn
    ;; A value that we used in our computation has changed, which
    ;; means that we need to start computation all over again. Record
    ;; that the current value is not valid via :value-depends-changed,
    ;; stop tracking dependencies, cancel our state and any pending
    ;; expression, and start the computation again.
    [(update-used-value-changed [info reference]
       (-> info
           (assoc :value-depends-changed true)
           (update-valid)
           (update-remove-state)
           (update-add-action register-different-depends
                                    (keys (:depends-info info)))
           (dissoc :depends-info :unused-depends :uncertain-depends
                   :expression)
           (update-start-evaluation reference)))

     ;; update :uncertain-depends to reflect the validity of the
     ;; depended on information
     (update-uncertain [info depends-on depends-on-visible]
       (update-valid (if (:valid depends-on-visible)
                       (update-in-clean-up info [:uncertain-depends]
                                           #(disj % depends-on))
                       (update-in info [:uncertain-depends]
                                  #((fnil conj #{}) % depends-on)))))]

  (defn update-depends-on-visible
    "The visible information for a reference that we may depend on
     may have changed, update what we know about it, given a function
     that fetches it. Using a function allows the fetch do be done as part
     of an atomic update of our info, so that we won't be saving
     old information."
    [info reference depends-on depends-on-visible-getter]
    (if (contains? (:depends-info info) depends-on)
      (let [depends-on-visible (depends-on-visible-getter)
            revised-info (update-uncertain info depends-on depends-on-visible)]
        (if (or (contains? (:unused-depends revised-info) depends-on)
                (= (:value ((or (:depends-info revised-info) {}) depends-on))
                   (:value depends-on-visible)))
          (assoc-in revised-info [:depends-info depends-on] depends-on-visible)
          (update-used-value-changed revised-info reference)))
      info)))

(letfn
    ;; Run a collection of actions
    [(run-actions [actions scheduler reference]
       (doseq [[f & args] actions]
         (apply f scheduler reference args)))

      ;; The information for the reference has changed. Tell anyone else
      ;; who cares.
     (schedule-propagations [scheduler reference]
       ;; TODO: also tell anything that listens to us.
       (doseq [using (mm/get-in! (:references scheduler)
                                 [reference :using-references])]
         (add-task (:pending scheduler)
                   copy-visible-to-user reference using)))]

  (defn change-and-schedule-propagation
    "Run the function on the information for the reference, and replace
    the information with the result of the function. If the value or
    validity of the reference has changed, propagate that."
    [scheduler reference f & args]
    (let [r-mm (:references scheduler)
          [old new] (mm/update-in-returning-both!
                     r-mm [reference]
                     (fn [info] (update-run-expression-while-ready
                                 (apply f info args))))]
      (mm/call-and-clear-in! r-mm [reference :pending-actions]
                             run-actions scheduler reference)

      (when (not= (:visible old) (:visible new))
        (schedule-propagations scheduler reference)))))

(defn copy-visible-to-user [scheduler depends-on reference]
  "Record in this reference the latest information for of one of the
   references this reference depends on. If that changes its
   information, schedule propagation."
  (change-and-schedule-propagation
   scheduler reference
   update-depends-on-visible reference depends-on
   #(mm/get-in! (:references scheduler) [depends-on :visible])))

(defn copy-state-value [scheduler reference]
  "Record in this reference the latest information for its state. If
   that changes its information, schedule propagation."
  (change-and-schedule-propagation
   scheduler reference update-value-from-state))

;;; The next functions handle :pending-actions. They are
;;; designed to be idempotent, so they can be called as long as there
;;; is any indication that things may be out of date.

;;; Notice that both additions and removals must be handled by the
;;; same pending registration. Otherwise, an add might occur in the
;;; middle of the execution of a removal, and the removal would remove
;;; it and when finished, wouldn't notice that an add was now necessary.

(defn register-different-depends
  "Given a reference and a collection of references that have been
  added to it or removed from it, make sure the :using-references
  field in each of those references reflects the depends-info,
  and propagate values from those references that are used."
  [scheduler reference changed-references]
  (let [r-mm (:references scheduler)
        getter #(mm/get-in! r-mm [reference :depends-info])]
    (doseq [changed-reference changed-references]
      (change-and-schedule-propagation
       scheduler changed-reference
       update-adjust-using changed-reference reference getter))))

(letfn
    [(state-change-callback [value state scheduler reference]
       (add-task (:pending scheduler) copy-state-value reference))

     (register-added-state
       [current-state scheduler reference]
       (let [r-mm (:references scheduler)]
         (let [value (subscribe current-state
                                state-change-callback scheduler reference)]
           (if (not= value (mm/get-in! r-mm [reference :visible :value]))
             (add-task (:pending scheduler) copy-state-value reference)))))

     (register-removed-state
       [scheduler reference removed-state]
       (unsubscribe removed-state
                    state-change-callback scheduler reference))]

  (defn register-different-state
   "Given the current state object, and an object that may be or may
    have been the state, unsubscribe from the state of interest if it is
    not the current state, and subscribe to it if it is. For a
    subscription, if the state's value is not the same as the currently
    recorded one, schedule change propagation."
   ;; TODO: make State objects have a conditional subscription method
   ;; that consults a thunk to decide whether to subscribe. Then we
   ;; could get rid of the last call-with-latest-value! in this code.
   [scheduler reference state-of-interest]
    (mm/call-with-latest-value-in!
     (:references scheduler) [reference :state]
     (fn [current-state]
       (if (= current-state state-of-interest)
         (when (not (nil? current-state))
           (register-added-state current-state scheduler reference))
         (when (not (nil? state-of-interest))
           (register-removed-state scheduler reference state-of-interest)))))))

(defn run-all-pending [scheduler]
  (loop []
    (when (run-pending-task (:pending scheduler) scheduler)
      (recur))))

(defn scheduler-summary
  "Return a map that describes the content of a scheduler
   but doesn't have deep nesting."
  [scheduler]
  (let [r-mm (:references scheduler)
        references (keys (mm/current-contents r-mm))]
    {:references
     (zipmap references
             (for [reference references]
               (let [info (mm/get! r-mm reference)]
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

  [;; This holds a mutable map that records references and their
   ;; values. The references are specified by a form, which must be
   ;; a function and its arguments, which can be anything.
   ;; The map is keyed by the reference. Its values are maps that
   ;; record:
   ;;   A :visible map of what other references see about this
   ;;     reference, consisting of
   ;;     The :value of the reference.
   ;;     A :valid tag if the value is valid
   ;;   A set of :using-references that depend on the visible information.
   ;;   A boolean :value-depends-changed if some of the dependencies of
   ;;     the value have changed.
   ;;   A :depends-info map from references that were used or are
   ;;     needed by our computations so far to a copy of the version
   ;;     of the information for that reference that is reflected
   ;;     in the computation.
   ;;   A :depends-used map from references that are needed in our
   ;;     computations so far to the expressions that need them.
   ;;   A :pending-actions set of actions to take based on new information
   ;;     about this reference. This is a way for a purely functional
   ;;     update to the information for one reference to request
   ;;     changes to other information. The actions are of the form
   ;;     [function & args]. The functionn will be called with the scheduler,
   ;;     this reference, and the additional args.
   ;;   For internal computations, a :expressionss map from expressions in
   ;;     the form (function arg arg ...) to maps about them. Those
   ;;     maps contain a subset of:
   ;;     A :visible map that is a copy of the latest information we
   ;;       know about the value of the expression
   ;;     A :reference that the expression evaluates to, if evaluation has
   ;;       been done.
   ;;     A set :uncertain-depends of expressions directly inside this one
   ;;       whose value is not yet valid.
   ;;     A set of :using-expressions of expressions that use the value of
   ;;       this one. The set can also contain :top-level if this expression
   ;;       is the top level expression of the reference.
   ;;   For external computations, the :state object that holds our value.
   ;;   For external computations, the :unsubscriber thunk to call to
   ;;     unsubscribe to changes in the value
   ;;   A map of :approximations whose keys are references that our
   ;;     value depends on where where only a lower bound on their value
   ;;     was used in computing our value. The values in the map are a
   ;;     pair of which iteration of that value was used, and which
   ;;     iteration of the reference's value we need as input if our
   ;;     value is to be used in the next iteration. The former can be
   ;;     computed from :used-info, but is aggregated here to make
   ;;     computation easier. The latter is reset to 0 whenever a
   ;;     non-monotonic input changes, because that change invalidates
   ;;     all iterations that went through this reference.
   references

   ;; This is holds a list of pending work, mostly updates that need
   ;; to be propagated. Each entry is a sequence of a function and its
   ;; arguments. When the function is called, the scheduler will be
   ;; prepended to the arguments.
   pending
   ]

  Notifier

  (notifier-value [this reference]
    (request this reference)
    (run-all-pending this)
    (mm/get-in! (:references this) [reference :visible :value]))

  Scheduler

  (request [this reference]
    (change-and-schedule-propagation this reference
                                     update-initialize-if-needed reference))

  (ready? [this reference]
    (mm/get! (:references this) [reference :visible :valid]))
  )

(defmethod clojure.core/print-method ApproximatingScheduler
  [scheduler writer]
  (.write writer "<ApproximatingScheduler>"))

(defmethod new-approximating-scheduler true []
  (->ApproximatingScheduler (mm/new-mutable-map)
                            (new-priority-task-queue)))

(ns cosheet.compute-impl
  (:require [clojure.set :as set]
            (cosheet [compute :refer :all]
                     [state :refer :all]
                     [task-queue :refer :all]
                     [utils :refer [dissoc-in update-in-clean-up
                                    call-with-latest-value]]
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
;;; overwritten by old information by the first thread. Even reading
;;; the information inside the atomic operation in a thread doesn't
;;; work, because the correct value might have been set back to the
;;; state at the beginining of the atomic operation, and the atomic
;;; operation would overwrite it. Instead, we check, after doing a
;;; copy, that the information that was copied still matches the
;;; latest information, and redo the copy if it doesn't. This code
;;; uses the former technique as much as possible, but currently needs
;;; to use the latter for subscribing to state, because State objects
;;; don't have a conditional subscription method that consults a thunk
;;; to decide whether to subscribe.

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


;;; Trivial scheduler that just runs everything and returns the
;;; current value.

(defn expression? [expr]
  (and (list? expr) (= (first expr) :expression)))

(defn expression-tracer [expr]
  (second expr))

(defn expression-fn [expr]
  (nth expr 2))

(defn expression-args [expr]
  (seq (nthnext expr 3)))

(def reference-current-value)

(defn expression-current-value [expr]
  (if (expression? expr)
    ((expression-tracer expr)
     #(reference-current-value
       (cons (expression-current-value (expression-fn expr))
             (map expression-current-value (expression-args expr)))))
    expr))

(defn reference-current-value
  [[fn & args]]
  (let [result (apply fn args)]
    (cond (state? result) (state-value result)
          (expression? result) (expression-current-value result)
          :else result)))

(defmethod current-value true [item]
  (cond (state? item) (state-value item)
        (expression? item) (expression-current-value item)
        (sequential? item) (reference-current-value item)
        :else item))

;;; The next functions compute revised reference info to reflect new
;;; information. They have no side effects, but they can set pending
;;; actions in the info to request side effects that propagate
;;; information. They are not responsible for requesting propagation
;;; of a new value, though, as that will be checked by the function
;;; that calls them.

(def register-different-depends)
(def register-different-state)
(def schedule-copy-visible-to-user)
(def schedule-copy-visible-to-all-users)

(defn update-add-action [info f & args]
  (update-in info [:requested-actions]
             #((fnil conj []) % `[~f ~@args])))

(defn update-value
  "Update the value, which must be valid."
  [info value]
  (let [old-visible (:visible info)
        new-visible (if (nil? value) nil  {:value value :valid true})]
    (cond-> (update-in-clean-up info [:visible] (constantly new-visible))
      (not= old-visible new-visible)
      (update-add-action schedule-copy-visible-to-all-users))))

(defn update-invalid [info]
  (let [was-valid (get-in info [:visible :valid])]
    (cond-> (dissoc-in info [:visible :valid])
      was-valid
      (update-add-action schedule-copy-visible-to-all-users))))

(letfn
    [(update-remove-reference [info expr]
       (let [reference (get-in info [:expressions expr :reference])]
         (if (nil? reference)
           info
           (as-> info info
             (dissoc-in info [:expressions expr :reference])
             (update-in-clean-up info [:using-expressions reference]
                                 #(disj % expr))
             (if (nil? (get-in info [:using-expressions reference]))
               (-> info
                   (dissoc-in [:depends-info reference])
                   (update-add-action register-different-depends [reference]))
               info)))))

     (update-add-reference [info expr reference]
       (if (nil? reference)
         info
         (-> (if (nil? (get-in info [:using-expressions reference]))
               (-> info
                   (assoc-in [:depends-info reference] nil)
                   (update-add-action register-different-depends [reference]))
               info)
             (update-in [:using-expressions reference]
                        #((fnil conj #{}) % expr))
             (assoc-in [:expressions expr :reference] reference))))]
  
  (defn update-reference
    "Update the reference of the expression.
     Don't worry about the effects of that on validity."
    [info expr reference]
    (if (= reference (get-in info [:expressions expr :reference]))
      info
      (-> info
          (update-remove-reference expr)
          (update-add-reference expr reference)))))

(letfn
    [(update-value-result [info value]
       (update-value info value))
     
     (update-state-result [info state]
       (-> info
           (assoc :state state)
           (update-value (state-value state))
           (update-add-action register-different-state state)))
     
     (update-add-expression [info expr using-expression]
       (let [expr-parts (cons (expression-fn expr) (expression-args expr))
             subsidiary-exprs (filter expression? expr-parts)]
         (as-> info info
           (if (empty? subsidiary-exprs)
                     (update-reference info expr expr-parts)
                     (assoc-in info [:expressions expr :uncertain-depends]
                               (set subsidiary-exprs)))
           (if (nil? using-expression)
             info
             (update-in info [:expressions expr :using-expressions]
                        #((fnil conj #{}) % using-expression)))
           (reduce (fn [info subsidiary]
                     (update-add-expression info subsidiary expr))
                   info
                   subsidiary-exprs))))

     (remove-traces [expr]
       (if (expression? expr)
         (apply make-expression nil (remove-traces (expression-fn expr))
                (map remove-traces (expression-args expr)))
         expr))

     (update-expression-result [info expr]
       (let [expr (remove-traces expr)]
         (-> info
             (assoc :expression expr)
             (update-add-expression expr nil))))]

  (defn update-initialize
    "Run the reference and set up the information in accordance
     with what comes back.."
    [info [f & args]]
    (let [result (apply f args)]
      (cond (state? result) (update-state-result info result)
            (expression? result) (update-expression-result info result)
      :else (update-value-result info result)))))

(defn update-initialize-if-needed
  "If info is nil, return an initialized info."
  [info reference]
  (or info (update-initialize {} reference)))

(defn update-using-reference
  "Update the :using-references set for this reference to reflect
   whether the specified possibly using reference does, in fact, use us."
  [info reference possibly-using usings-depends-info]
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
        (update-add-action info
                           schedule-copy-visible-to-user possibly-using)))
    ;; It doesn't use us.
    (update-in-clean-up info [:using-references]
                        #(disj % possibly-using))))

(letfn
    [;; Record that the given subexpression does not have a valid
     ;; value, and propagate if we had been valid before.
     (update-subexpression-not-valid [info expr subexpression]
       (let [was-valid (let [reference
                             (get-in info [:expressions expr :reference])]
                         (and (not (nil? reference))
                              (get-in info [:depends-info reference :valid])))]
         (as-> info info
           (update-in info [:expressions expr :uncertain-depends]
                      #((fnil conj #{}) % subexpression))
           (if was-valid
             (-> info
                 (update-reference expr nil)
                 (update-expression-not-valid expr))
             info))))

     ;; The value of the expression is no longer valid. Record the
     ;; local consequences of that, and propagate up the use chain.
     (update-expression-not-valid [info expr]
       (reduce
        (fn [info using-expr]
          (update-subexpression-not-valid info using-expr expr))
        (-> (if (= expr (:expression info))
              (update-invalid info)
              info))          
        (get-in info [:expressions expr :using-expressions])))

     ;; Given an expression, all of whose sub-expressions have valid
     ;; values, return the resulting reference.
     (expression-reference [info expression]
       (map (fn [element]
              (if (expression? element)
                (let [reference (get-in info [:expressions element :reference])]
                  (get-in info [:depends-info reference :value]))
                element))
            (cons (expression-fn expression)
                  (expression-args expression))))

     (update-subexpression-valid [info expr subexpression]
       (as-> info info
         (update-in-clean-up info [:expressions expr :uncertain-depends]
                             #(disj % subexpression))
         (if (empty? (get-in info [:expressions expr :uncertain-depends]))
           (let [reference (expression-reference info expr)]
             (as-> info info
               (update-reference info expr reference)
               (let [visible-info (get-in info [:depends-info reference])]
                 (if (:valid visible-info)
                   (update-expression-reference-visible info expr visible-info)
                   (update-expression-not-valid info expr)))))
           info)))

     ;; Given an expression and the visible info for its reference,
     ;; update users of the expression.
     (update-expression-reference-visible [info expr visible-info]
       (if (:valid visible-info)
         (reduce
          (fn [info using-expr]
            (update-subexpression-valid info using-expr expr))
          (if (= expr (:expression info))
            (update-value info (:value visible-info))
            info)
          (get-in info [:expressions expr :using-expressions]))         
         (update-expression-not-valid info expr)))]

  (defn update-depends-on-visible
    "The visible information for a reference that we may depend on
     may have changed, update what we know about it, given a function
     that fetches it. Using a function allows the fetch do be done as part
     of an atomic update of our info, so that we won't be saving
     old information."
    [info depends-on depends-on-visible]
    (if (contains? (:depends-info info) depends-on)
      (if (= depends-on-visible (get-in info [:depends-info depends-on]))
        info
        (reduce (fn [info expr]
                  (update-expression-reference-visible
                   info expr depends-on-visible))
                (assoc-in info [:depends-info depends-on] depends-on-visible)
                (get-in info [:using-expressions depends-on])))
      info)))

(defn change-and-run-requested-actions
  "Run the function on the information for the reference, and replace
   the information with the result of the function. If the value or
   validity of the reference has changed, propagate that."
  [scheduler reference f & args]
  (let [r-mm (:references scheduler)]
    (mm/update-in! r-mm [reference]
                   (fn [info] (apply f info args)))
    (mm/call-and-clear-in! r-mm [reference :requested-actions]
                           (fn [actions]
                             (doseq [[f & args] actions]
                               (apply f scheduler reference args))))))

;;; The next functions copy information. They are
;;; idempotent, so they can be called even if there is only a possibility
;;; that things may be out of date.

(defn copy-visible-to-user [scheduler depends-on reference]
  "Record in this reference the latest information for of one of the
   references this reference depends on. If that changes its
   information, schedule propagation."
  (mm/call-with-latest-value-in!
   (:references scheduler) [depends-on :visible]
   (fn [depends-on-visible]
     (change-and-run-requested-actions
      scheduler reference
      update-depends-on-visible depends-on depends-on-visible))))

(defn schedule-copy-visible-to-user [scheduler depends-on reference]
  (add-task (:pending scheduler)
            copy-visible-to-user depends-on reference))

(defn schedule-copy-visible-to-all-users
  "The visible information for the reference has changed. Tell anyone else
  who cares."
  [scheduler reference]
  ;; TODO: also tell anything that listens to us.
  (doseq [using (mm/get-in! (:references scheduler)
                            [reference :using-references])]
    (schedule-copy-visible-to-user scheduler reference using)))

(defn copy-state-value [scheduler reference]
  "Record in this reference the latest information for its state. If
   that changes its information, schedule propagation."
  (call-with-latest-value
   #(when-let [state (mm/get-in! (:references scheduler) [reference :state])]
      (state-value state))
   (fn [value] (change-and-run-requested-actions
                scheduler reference update-value value))))

;;; Notice that both additions and removals must be handled by the
;;; same pending registration. Otherwise, an add might occur in the
;;; middle of the execution of a removal, and the removal would remove
;;; it and when finished, wouldn't notice that an add was now necessary.
(defn register-different-depends
  "Given a reference and a collection of references that have been
  added to its dependencies or removed from them, make sure the
  :using-references field in each of those references reflects
  the :depends-info, and propagate values from those references
  that are used."
  [scheduler reference changed-references]
  (let [r-mm (:references scheduler)]
    (doseq [changed-reference changed-references]
      (mm/call-with-latest-value-in!
       r-mm [reference :depends-info]
       (fn [reference-depends-on-info]
         (change-and-run-requested-actions
          scheduler changed-reference
          update-using-reference
          changed-reference reference reference-depends-on-info))))))

(letfn
    [(state-change-callback [state scheduler reference]
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

  ;; TODO: in the new regime, the state of a value can never change,
  ;; so make this just register-state, but move the removed stuff to
  ;; garbage collection.
  (defn register-different-state
   "Given a reference, and an object that may be or may
    have been its state, unsubscribe from the state of interest if it is
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

(defn simplify-for-print [item]
  (cond (state? item)
        `("state"  ~(state-value item))
        (expression? item)
        `(:expr ~(simplify-for-print
                  (cons (expression-fn item)(expression-args item))))
        (set? item)
        (set (map simplify-for-print item))
        (map? item)
        (let [keys (keys item)]
          (zipmap (map simplify-for-print keys)
                  (for [key keys] (simplify-for-print (item key)))))
        (sequential? item)
        (map simplify-for-print item)
        :else
        item))

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
   ;;   A set of :using-references that depend on our visible
   ;;     information.
   ;;   A :requested-actions set of actions to take based on new information
   ;;     about this reference. This is a way for a purely functional
   ;;     update to the information for one reference to request
   ;;     changes to other information. The actions are of the form
   ;;     [function & args]. The functionn will be called with the scheduler,
   ;;     this reference, and the additional args.
   ;;   For internal computations:
   ;;     An :expression whose value is our value
   ;;     An :expressions map from expressions in
   ;;       the form (function arg arg ...) to maps about them. Those
   ;;       maps contain a subset of:
   ;;       A :reference that the expression simplifies to, if all
   ;;         sub-expression evaluation have been done. The value
   ;;         of the expression is the cached value of the reference.
   ;;       A set :uncertain-depends of expressions directly inside this one
   ;;         for which we don't have a valid value.
   ;;       A set of :using-expressions of expressions that use the value of
   ;;         this one. This has to be a set, because the same
   ;;         expression might be a subexpression in several places.
   ;;     A :depends-info map from references of our expressions
   ;;       to a copy of the version of the information for that
   ;;       reference.
   ;;     A :using-expressions map from references of our
   ;;       expressions to the expressions they are the reference of.
   ;;   For external computations:
   ;;     The :state object that holds our value.
   ;;     The :unsubscriber thunk to call to unsubscribe to changes
   ;;       in the value
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
    (change-and-run-requested-actions this reference
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

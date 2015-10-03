(ns cosheet.computation-manager
  (:require (cosheet [task-queue :refer :all]
                     [utils :refer [dissoc-in update-in-clean-up
                                    swap-returning-both!
                                    swap-control-return!
                                    call-with-latest-value]]
                     [reporters :as reporter]
                     [mutable-map :as mm])))

;;; Manage the (re)computation of reporters using a priority queue.

;;; The following fields are used in reporters, in addition to the
;;; standard reporter fields.
;;; Provided by the original creator of the reporter:
;;;         :expression The expression describing the computation that
;;;                     gives the value of this reporter 
;;;       :manager-type A keyword describing what kind of manager the
;;;                     reporter should have.
;;; Added by this manager:
;;;       :value-source A reporter whose value should be the value of this
;;;                     reporter if we have no needed-values.
;;;      :needed-values A set of reporters whose values this reporter needs
;;;                     to evaluate its expression and that it doesn't have a
;;;                     valid value for.
;;; :subordinate-values A map from reporters whose values this reporter needs
;;;                     to evaluate its expression to the last
;;;                     value it saw for them, even if they have gone
;;;                     invalid subsequently.
;;;    :further-actions A list of [function arg arg ...] calls that
;;;                     need to be performed. (These will never actually
;;;                     be stored in a reporter, but are added to the
;;;                     map before it is stored.)

;;; The computation is multi-threaded, but can avoid using locks and
;;; TSM because it just needs eventual consistency; it is just copying
;;; information. The danger is that in between a read and a copy in
;;; one thread, the data that was read will be changed, and another
;;; thread will complete a read and copy of the new information, only to
;;; have the first thread overwrite it with the stale information.
;;;
;;; Doing the read inside an atomic update operation for copying
;;; doesn't work. Consider the copied data starting out at A, and
;;; source value changing from A to B, and then back to A. An atomic
;;; swap! to the copy reads its current value as A, then the function
;;; provided to the swap! reads the intermediate source value B and
;;; returns it. But before the swap! finishes, another thread sets the
;;; copie's value back to the final A. Now, when the original swap!
;;; goes to finish, it will see that the copy's value is still A, like
;;; it initially read, so the swap! succeeds, and sets the copy to the
;;; stale B.
;;;
;;; Instead, we check, after doing a copy, that the information that
;;; was copied still matches the latest information, and redo the copy
;;; if it doesn't.

;;; SUGGESTION: if it become important to pass around deltas, the way
;;; to do that is to have value information contain a promise of the
;;; next version of that value and the delta between those versions.
;;; Then, anything with a handle to an old value can chase the change
;;; path, while GC will get rid of change information for which there
;;; are no longer handles.

;;; TODO: Priorities need to be implemented

;;; TODO: Approximations need to be implemented.
;;; They logically unfold the dependency graph, with only the last
;;; unfolding being kept around, but all unfoldings being invalidated
;;; when a non-monotonic input changes or a monotonic input changes in
;;; a non-monotonic way.
;;; It uses a map, :approximations
;;;     whose keys are reporters that our
;;;     value depends on, where only a lower bound on their value
;;;     was used in computing our value. The values in the map are a
;;;     pair of [which iteration of that value was used, and which
;;;     iteration of the reporter's value we need as input if our
;;;     value is to be used in the next iteration]. The latter is reset
;;;     to 0 whenever there is a non-monotonic change to an input,
;;;     because that invalidates all iterations that went through
;;;     this reporter.


;;; The management record contains all the information that the
;;; various managers and propagators need.
;;; By making it a record, we can define our own print-method, to
;;; avoid infinite loops when it is printed out. (The queue will
;;; contain references back to the management record.)
(defrecord ManagementImpl
    [queue  ; A task-queue of pending tasks.
     manager-map ; A map from :manager-type of a reporter to a
                 ; manager function.
     cache ; A mutable map from expression to reporter.
           ; (present if cache reporters are supported.)
     ])

(defmethod print-method ManagementImpl [s ^java.io.Writer w]
  ;; Avoid huge print-outs.
  (.write w "<ManagementImpl>"))

(defn update-new-further-action
  "Given a map, add an an action to the further actions."
  [data & action]
  (update-in data [:further-actions] (fnil conj []) (vec action)))

(defn modify-and-act
  "Atomicly call the function on the reporter's data.
   The function should return the new data for the reporter,
   which may also contain a temporary field, :further-actions with
   a list of actions that should be performed."
  [reporter f]
  (let [actions (swap-control-return!
                 (reporter/data-atom reporter)
                 (fn [data] (let [new-data (f data)]
                              [(dissoc new-data :further-actions)
                               (:further-actions new-data)])))]
    (doseq [action actions]
      (apply (first action) (rest action)))))

(def eval-expression-if-ready)
(def manage)

(defn update-value
  "Given the data from a reporter, and the reporter, set the value in the data,
  and request the appropriate registrations."
  [data reporter value]
  (if (= value (:value data))
    data
    (-> data
        (assoc :value value)
        (update-new-further-action reporter/inform-attendees reporter))))

(defn copy-value-callback
  [[_ to] from]
  (call-with-latest-value
   #(reporter/value from)
   (fn [value] (modify-and-act
                to
                (fn [data] (if (= (:value-source data) from)
                             (update-value data to
                                           (if (empty? (:needed-values data))
                                             value
                                             reporter/invalid))
                             data))))))

(defn register-copy-value
  "Register the need to copy (or not copy) the value from
   the first reporter to become the value of the second."
  [from to]
  (call-with-latest-value
   #(when (= (:value-source (reporter/data to)) from) [copy-value-callback])
   (fn [callback]
     (apply reporter/set-attendee!
            from (list :copy-value to) callback))))

(defn update-value-source
  "Given the data from a reporter, and the reporter, set the value-source
  to the given source, and request the appropriate registrations."
  [data reporter source]
  (let [old-source (:value-source data)]
    (if (= source old-source)
      data
      (reduce
       (fn [data src]
         (update-new-further-action data register-copy-value src reporter))
       (if source
         (assoc data :value-source source)
         (dissoc data :value-source))
       ;; Add the new source before removing the old one, so that any
       ;; shared subsidiary reporters will never lose demand.
       (filter identity [source old-source])))))

(defn copy-subordinate-callback
  [to from management]
  (call-with-latest-value
   #(reporter/value from)
   (fn [value]
     (modify-and-act
      to
      (fn [data]
        (if (if (contains? (:needed-values data) from)
              (= value reporter/invalid)
              (or (not (contains? (:subordinate-values data) from))
                  (= value (get-in data [:subordinate-values from]))))
          data
          (if (reporter/valid? value)
            (let [new-data
                  (cond-> (update-in data [:needed-values] disj from)
                    (not= value
                          (get-in data [:subordinate-values from] ::not-found))
                    (#(-> %
                          (assoc-in [:subordinate-values from] value)
                          (update-value-source to nil))))]
              (if (empty? (:needed-values new-data))
                (apply update-new-further-action new-data
                       add-task (:queue management)
                       (if (nil? (:value-source new-data))
                         [eval-expression-if-ready to management]
                         ;; We have re-confirmed all old values for the source.
                         ;; Get the value back.
                         [copy-value-callback
                          `(:copy-value ~to) (:value-source new-data)]))
                new-data))
            (-> data
                (update-in [:needed-values] conj from)
                (update-value to reporter/invalid)))))))))

(defn register-copy-subordinate
  "Register the need to copy (or not copy) the value from the first reporter
  for use as an argument of the second."
  [from to management]
  (call-with-latest-value
   #(let [data (reporter/data to)]
      (when (or (contains? (:needed-values data) from)
                (contains? (:subordinate-values data) from))
        [copy-subordinate-callback management]))
   (fn [callback] (apply reporter/set-attendee! from to callback))))

(defn reuse-parts
  "Given an old reporter and a new reporter that is not yet managed,
   and does not have an attendee,
   if any of the parts of the expression of the reporter
   are themselves reporters, and they have the same expression
   as the expression of reporters that are parts of the old reporter,
   return a new reporter with the matched reporters replaced 
   by the matching ones from the old reporter."
  [old new]
  (let [expr #(:expression (reporter/data %))
        reusable-map (let [reusable (filter #(and (reporter/reporter? %)
                                                  (expr %))
                                            (expr old))]
                       (zipmap (map expr reusable) reusable))
        new-expr (expr new)
        reused-expr (map #(or (when (reporter/reporter? %)
                                (reusable-map (expr %))) %)
                         new-expr)]
    (if (= new-expr reused-expr)
      new
      (apply reporter/new-reporter
             (mapcat identity
                     (assoc (reporter/data new) :expression reused-expr))))))

(defn eval-expression-if-ready
  "If all the arguments for an eval reporter are ready, evaluate it."
  [reporter management]
  (modify-and-act
   reporter
   (fn [data]
     (if (empty? (:needed-values data))
       (let [application (map (fn [term] (if (reporter/reporter? term)
                                           ((:subordinate-values data) term)
                                           term))
                              (:expression data))
             value (apply (first application) (rest application))]
         (if (reporter/reporter? value)
           (-> data
               ;; Manage the new value-source, before swapping it for
               ;; the old one, so reporters that are used by both will
               ;; always have some demand, and not be taken out of the
               ;; cache.
               (update-new-further-action manage value management)
               (update-value-source reporter value))
           (-> data
               (update-value reporter value)
               (update-value-source reporter nil))))
       data))))

(defn eval-manager
  "Manager for an eval reporter."
  [reporter management]
  (modify-and-act
   reporter
   (fn [data]
     (let [subordinates (set (filter reporter/reporter? (:expression data)))
           new-data (reduce
                     (fn [data subordinate]
                       (update-new-further-action
                        data
                        register-copy-subordinate
                        subordinate reporter management))
                     data subordinates)]
       (if (reporter/data-attended? new-data)
         (-> new-data
             (assoc :needed-values subordinates)
             (assoc :subordinate-values {})
             (update-new-further-action
              add-task (:queue management)
              eval-expression-if-ready reporter management))
         (-> new-data
             (dissoc :needed-values)
             (dissoc :subordinate-values)
             (update-value-source reporter nil)
              ;;; Note, we are not attended, so it is OK to change the
              ;;; value without notifying the callbacks.
             (assoc :value reporter/invalid)))))))

(defn cached-eval-manager
  "Manager for an eval reporter that is also stored in the cache."
  [reporter management]
  (when (not (reporter/attended? reporter))
    (mm/dissoc-in! (:cache management)
                   [(:expression (reporter/data reporter))]))
  (eval-manager reporter management))

(defn ensure-in-cache
  "Get or make an cached-eval reporter for the given expression in the cache.
   It may not be managed."
  [expression original-name management]
  (mm/update-in!
   (:cache management) [expression]
   #(or % (apply reporter/new-reporter
                     :expression expression
                     :manager-type :cached-eval
                     (when original-name [:name ["cached" original-name]])))))

(defn cache-manager
  "Manager that looks up the value of a reporter's expression in a cache of
  reporters."
  [reporter management]
  (let [data (reporter/data reporter)
        expression (:expression data)
        cache (:cache management)]
    ;;; We do side-effects, which can't run inside a swap!, so we have
    ;;; to use call-with-latest-value.
    (call-with-latest-value
     #(reporter/attended? reporter)
     (fn [attended]
       (let [value-source (when attended
                            (ensure-in-cache
                             expression (:name data) management))]
         (modify-and-act reporter
                         #(update-value-source % reporter value-source))
         ;; We can't do management until the registration is done,
         ;; or the source's manager will take it out of the cache.
         (when value-source (manage value-source management)))))))

(defn new-management
  "Create the manager data for a caching evaluator."
  ([] (new-management 0))
  ([worker-threads]
   (map->ManagementImpl {:cache (mm/new-mutable-map)
                         :queue (new-priority-task-queue worker-threads)
                         :manager-map {:cache cache-manager
                                       :eval eval-manager
                                       :cached-eval cached-eval-manager}})))

(defn manage
  "Assign the manager to the reporter
  and to any reporters referenced by its expression."
  [reporter management]
  (when (reporter/reporter? reporter)
    (let [data (reporter/data reporter)
          manager-type (:manager-type data)]
      (when (and manager-type (not (:manager data)))
        (let [manager-fn (manager-type (:manager-map management))]
          (assert manager-fn (format "Unknown manager type: %s" manager-type))
          (reporter/set-manager! reporter manager-fn management))
        (doseq [term (:expression data)]
          (manage term management))))))

(defn request
  "Request computation of a reference."
  [x management]
  (reporter/set-attendee! x :computation-request (fn [key value] nil))
  (manage x management)
  x)

(defn compute
  "Do all pending computations, or if the second argument is provided,
   all or that many, which ever comes first."
  ([management]
   (run-all-pending-tasks (:queue management)))
  ([management max-tasks]
   (run-some-pending-tasks (:queue management) max-tasks)))

(defn computation-value
  "Given a possible reporter, compute its value and return it."
  [x management]
  (if (reporter/reporter? x)
    (do
      (request x management)
      (compute management)
      (reporter/value x))
    x))


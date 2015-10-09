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
;;;   :reusable-reporter An optional reporter that is only present
;;;                      before the reporter is managed, which is
;;;                      already in use, and which can be scavenged to
;;;                      replace arguments of the expression with
;;;                      reporters that are already in use.
;;;        :value-source A reporter whose value should be the value of this
;;;                      reporter if we have no needed-values.
;;;    :old-value-source We may suspend the value source if some of the
;;;                      values we used to compute it went invalid, but
;;;                      we haven't seen a valid contradiction to the
;;;                      values we used. In that case, the value source
;;;                      is put here, and we stay subscribed to it, but
;;;                      don't copy its value.
;;; :arguments-unchanged Present, and equal to true, is we have not
;;;                      seen a valid value different from the ones
;;;                      used to compute old-value-source.
;;;       :needed-values A set of reporters whose values this reporter needs
;;;                      to evaluate its expression and that it doesn't have a
;;;                      valid value for. Not present if nothing is
;;;                      attending to the reporter.
;;;  :subordinate-values A map from reporters whose values this reporter needs
;;;                      to evaluate its expression to the last
;;;                      valid value it saw for them, even if they have gone
;;;                      invalid subsequently. Not present if nothing
;;;                      is attending to the reporter.
;;;     :further-actions A list of [function arg arg ...] calls that
;;;                      need to be performed. (These will never actually
;;;                      be stored in a reporter, but are added to the
;;;                      map before it is stored.)

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
     manage-map ; A map from :manager-type of a reporter to a
                ; function that takes a reporter and a management,
                ; does any needed preparation putting the reporter
                ; under management, and sets its manager.
     cache ; A mutable map from expression to reporter.
           ; (present if cache reporters are supported.)
     ])

(defmethod print-method ManagementImpl [s ^java.io.Writer w]
  ;; Avoid huge print-outs.
  (.write w "<ManagementImpl>"))

(defn update-new-further-action
  "Given a map, add an an action to the further actions.
   This is used to request actions that affect state elsewhere than
   in the map. They will be done immediately after the map is stored back,
   in the order in which they were requested."
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
  and request the appropriate propagation."
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
                             (update-value data to value)
                             data))))))

(defn null-callback
  [key reporter]
  nil)

(defn register-copy-value
  "Register the need to copy (or not copy) the value from
   the first reporter to become the value of the second."
  [from to]
  (call-with-latest-value
   #(let [data (reporter/data to)]
      (cond (= (:value-source data) from) [copy-value-callback]
            (= (:old-value-source data) from) [null-callback]))
   (fn [callback]
     (apply reporter/set-attendee!
            from (list :copy-value to) callback))))

(defn update-value-source
  "Given the data from a reporter, and the reporter, set the value-source
   to the given source, and request the appropriate registrations.
   If the optional keyword :old true is passed,
   update the old-value-source, rather than value-source."
  [data reporter source & {:keys [old]}]
  ;; We must only set to non-nil if there are attendees for our value,
  ;; otherwise, we will create demand when we have none ourselves.
  (assert (or (nil? source) (reporter/data-attended? data)))
  (let [source-key (if old :old-value-source :value-source)
        original-source (source-key data)]
    (if (= source original-source)
      data
      (reduce
       (fn [data src]
         (update-new-further-action data register-copy-value src reporter))
       (if source
         (assoc data source-key source)
         (dissoc data source-key))
       ;; Add the new source before removing any old one, so that any
       ;; subsidiary reporters common to both will always have demand.
       (filter identity [source original-source])))))

(defn update-old-value-source
  [data reporter source]
  (update-value-source data reporter source :old true))

(defn copy-subordinate-callback
  [to from management]
  (call-with-latest-value
   #(reporter/value from)
   (fn [value]
     (modify-and-act
      to
      (fn [data]
        (let [same-value
              (= value (get-in data [:subordinate-values from] ::not-found))]
          (if (or (not (contains? data :needed-values))
                  (if (contains? (:needed-values data) from)
                    (= value reporter/invalid)
                    same-value))
            data
            ;; A value that we care about changed.
            ;; We are invalid until the recomputation runs,
            ;; which may not be for a while.
            (let [current-source (:value-source data)
                  newer-data (cond-> (update-value data to reporter/invalid)
                               current-source
                               ;; We must do the copy from source to
                               ;; old-source before clearing source,
                               ;; so the old source always has attendees.
                               (#(-> %
                                     (update-old-value-source to current-source)
                                     (assoc :arguments-unchanged true)
                                     (update-value-source to nil))))]
              (if (reporter/valid? value)
                (let [new-data
                      (cond-> (update-in newer-data [:needed-values] disj from)
                        (not same-value)
                        (#(-> %
                              (assoc-in [:subordinate-values from] value)
                              (dissoc :arguments-unchanged))))]
                  (if (empty? (:needed-values new-data))
                    (if (:arguments-unchanged new-data)
                      ;; We have re-confirmed all old values for
                      ;; the old source. Make it current again.
                      (-> new-data
                          (update-value-source to (:old-value-source new-data))
                          (update-old-value-source to nil)
                          (dissoc :arguments-unchanged))
                      ;; Some value changed. Schedule recomputation.
                      (apply update-new-further-action new-data 
                             add-task (:queue management)
                             [eval-expression-if-ready to management]))
                    new-data))
                (update-in newer-data [:needed-values] conj from))))))))))

(defn register-copy-subordinate
  "Register the need to copy (or not copy) the value from the first reporter
  for use as an argument of the second."
  [from to management]
  (call-with-latest-value
   #(let [data (reporter/data to)]
      (when (or (contains? (get data :needed-values #{}) from)
                (contains? (:subordinate-values data) from))
        [copy-subordinate-callback management]))
   (fn [callback] (apply reporter/set-attendee! from to callback))))

(defn- get-expression [reporter]
  (when (reporter/reporter? reporter)
    (:expression (reporter/data reporter))))

(defn update-reuse-arguments
  "Given data for a expression reporter and an old expression
  reporter, if any of the arguments of the expression of the new
  reporter are themselves reporters, and have identical expressions to
  reporters that are arguments of the old reporter, update the data
  with the matched reporters replaced by the matching ones from the
  old reporter."
  [data old-reporter]
   (let [new-expression (:expression data)
         old-data (reporter/data old-reporter)
         old-expression (:expression old-data)
         reuse-map (into {} (mapcat #(let [expression (get-expression %)]
                                       (when expression [[expression %]]))
                                    old-expression))]
       (let [num-reused (count (filter #(reuse-map (get-expression %))
                                       new-expression))]
         (when (not= num-reused 0)
           (println "reusing" num-reused "in" (first new-expression))))
       (assoc data :expression
              (map #(or (reuse-map (get-expression %)) %) new-expression))))

(defn provide-reusable-reporter
  "Set the :reusable-reporter, provided both arguments are expression
  reporters, there isn't already a :reusable-reporter, and the
  reporter is not managed."
  [reporter reusable-reporter]
  (when (and (get-expression reporter) (get-expression reusable-reporter))
    (swap! (reporter/data-atom reporter)
           (fn [data]
             (cond-> data
               (not (or (:reusable-reporter data) (:manager data)))
               (assoc :reusable-reporter reusable-reporter))))))

(defn set-reusable-reporters
  "If the number of arguments of both expressions is the same, then
  when corresponding positions are both expression reporters, and the
  reporter in the new position is unmanaged, set its
  :reusable-reporter to the reporter in the old expression."
  [new-expression old-expression]
  (when (= (count new-expression) (count old-expression))
    (doseq [[new old] (map list new-expression old-expression)]
      (provide-reusable-reporter new old))))

(defn eval-expression-if-ready
  "If all the arguments for an eval reporter are ready, and we don't have
  a value, evaluate the expression."
  [reporter management]
  (modify-and-act
   reporter
   (fn [data]
     ;; It is possible to get several eval-expression-if-ready queued
     ;; up for the same reporter. In that case, the first one to run
     ;; will do the evaluations, and the later ones will notice that
     ;; the value is valid, and not bother to re-evaluate.
     (if (and (not (reporter/valid? (:value data)))
              ;; We don't run if we still need values, or if
              ;; :needed-values is nil, which means
              ;; nobody is attending to the reporter.
              (= (:needed-values data) #{}))
       (let [value-map (:subordinate-values data)
             application (map #(get value-map % %) (:expression data))
             value (apply (first application) (rest application))]
         (-> (if (reporter/reporter? value)
               (let [old-value-source (:old-value-source data)]
                 ;; We have to set our value source first, so we
                 ;; generate demand for the new value, then set the
                 ;; :reusable-reporter before we manage the new value,
                 ;; so the manager can use it.
                 (-> (cond-> (update-value-source data reporter value)
                       old-value-source
                       (update-new-further-action
                        provide-reusable-reporter value old-value-source))
                     (update-new-further-action manage value management)))
               (-> data
                   (update-value reporter value)
                   (update-value-source reporter nil)))
             (update-old-value-source reporter nil)
             (dissoc :arguments-unchanged)))
       data))))

(defn eval-manager
  "Manager for an eval reporter."
  [reporter management]
  (modify-and-act
   reporter
   (fn [data]
     (if (= (reporter/data-attended? data) (contains? data :needed-values))
       data
       (let [subordinates (set (filter reporter/reporter? (:expression data)))
             new-data (reduce
                       (fn [data subordinate]
                         (update-new-further-action
                          data
                          register-copy-subordinate
                          subordinate reporter management))
                       (update-value data reporter reporter/invalid)
                       subordinates)]
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
               (update-old-value-source reporter nil)
               (dissoc :arguments-unchanged))))))))

(defn manage-eval
  "Optimize the reporter, and have it be managed by the eval-manager."
  [reporter management]
  (let [old-reporter (:reusable-reporter (reporter/data reporter))]
    ;; We do any expression adjusting before setting the manager,
    ;; which uses the expression.
    (when old-reporter
      (let [old-value-source (let [old-data (reporter/data old-reporter)]
                               (or (:value-source old-data)
                                   (:old-value-source old-data)))]
        (modify-and-act
           reporter
           (fn [data] (cond-> (-> data
                                  (dissoc :reusable-reporter)
                                  (update-reuse-arguments old-reporter))
                        (and old-value-source
                             ;; Setting the value source propagates
                             ;; demand, so it is only allowed if we
                             ;; have demand.
                             (not (empty? (:attendees data))))
                        (update-old-value-source reporter old-value-source)))))
      (set-reusable-reporters (:expression (reporter/data reporter))
                              (:expression (reporter/data old-reporter)))))
  (reporter/set-manager! reporter eval-manager management)
  ;; We manage the subexpressions after we set our manager. It gives
  ;; them demand, so they will have demand they need before they are
  ;; managed.
  (doseq [term (:expression (reporter/data reporter))]
    (manage term management)))

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
   (map->ManagementImpl
    {:cache (mm/new-mutable-map)
     :queue (new-priority-task-queue worker-threads)
     :manage-map {:cache (fn [r m] (reporter/set-manager!
                                    r cache-manager m))
                  :eval manage-eval
                  :cached-eval (fn [r m] (reporter/set-manager!
                                          r cached-eval-manager m))}})))

(defn manage
  "Assign the manager to the reporter
  and to any reporters referenced by its expression."
  [reporter management]
  (when (reporter/reporter? reporter)
    (let [data (reporter/data reporter)
          manager-type (:manager-type data)]
      (when (and manager-type (not (:manager data)))
        (let [manage-fn (manager-type (:manage-map management))]
          (assert manage-fn (format "Unknown manager type: %s" manager-type))
          (manage-fn reporter management))))))

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


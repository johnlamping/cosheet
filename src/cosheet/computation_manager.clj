(ns cosheet.computation-manager
  (:require (cosheet [reporter :as reporter]
                     [task-queue :refer :all]
                     [utils :refer [dissoc-in update-in-clean-up
                                    swap-returning-both!
                                    swap-control-return!
                                    call-with-latest-value]]
                     [mutable-map :as mm])))

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

;;; The following fields are used in reporters, in addition to the
;;; standard reporter fields:
;;;         :expression The expression describing the computation that
;;;                     gives the value of this reporter 
;;;       :manager-type A keyword describing what kind of manager the
;;;                     reporter should have.
;;;       :value-source A reporter whose value should be the value of this
;;;                     reporter if we have no needed-values.
;;;      :needed-values A set of reporters whose values this reporter needs
;;;                     to evaluate its expression and that it doesn't have a
;;;                     valid value for.
;;; :subordinate-values A map from reporters whose values this reporter needs
;;;                     to evaluate its expression to the last value
;;;                     value it saw for them, even if they have gone
;;;                     invalid subsequently.
;;;    :pending-actions A list of [function arg arg ...] calls that
;;;                     need to be performed. (These will never actualy
;;;                     be stored in a reporter, but are added to the
;;;                     data before it is stored.)

(defn modify-and-act
  "Atomicly call the function on the reporter's data.
   The function should return the new data for the reporter,
   which may also contain a temporary field, :pending-actions with
   a list of actions that should be performed."
  [reporter f]
  (let [actions (swap-control-return!
                 (reporter/data-atom reporter)
                 (fn [data] (let [new-data (f data)]
                              [(dissoc new-data :pending-actions)
                               (:pending-actions new-data)])))]
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
        (update-in [:pending-actions]
                   conj [reporter/inform-attendees reporter]))))

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
       (fn [data src] (update-in data [:pending-actions]
                                 conj [register-copy-value src reporter]))
       (if source
         (assoc data :value-source source)
         (dissoc data :value-source))
       (filter identity [old-source source])))))

(defn copy-subordinate-callback
  [to from management]
  (call-with-latest-value
   #(reporter/value from)
   (fn [value]
     (modify-and-act
      to
      (fn [data]
        (if (= value (if (contains? (:needed-values data) from)
                       reporter/invalid
                       (get-in data [:subordinate-values from]) ))
          data
          (if (reporter/valid? value)
            (let [new-data
                  (cond-> (update-in data [:needed-values] disj from)
                    (not= value (get-in data [:subordinate-values from]))
                    (#(-> %
                          (assoc-in [:subordinate-values from] value)
                          (update-value-source to nil))))]
              (if (empty? (:needed-values new-data))
                (if (nil? (:value-source new-data))
                  (update-in new-data [:pending-actions]
                             conj [add-task (:queue management)
                                   eval-expression-if-ready to management])
                  ;; We have re-confirmed all old values for the source.
                  ;; Get the value back.
                  (update-in new-data [:pending-actions]
                             conj[add-task (:queue management)
                                  copy-value-callback
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

(defn eval-expression-if-ready
  "If all the arguments for an eval reporter are ready, evaluate it."
  [reporter management]
  (modify-and-act
   reporter
   (fn [data]
     (if (= (:needed-values data) #{})
       (let [application (map (fn [term] (if (reporter/reporter? term)
                                           ((:subordinate-values data) term)
                                           term))
                              (:expression data))
             value (apply (first application) (rest application))]
         (if (reporter/reporter? value)
           (-> data
               (update-value-source reporter value)
               (update-in [:pending-actions]
                          conj [manage value management]))
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
     (let [subordinates (set (filter reporter/reporter?
                                              (:expression data)))
           new-data (assoc data :pending-actions
                           (map (fn [subordinate]
                                  [register-copy-subordinate
                                   subordinate reporter management])
                                subordinates))]
       (if (reporter/data-attended? new-data)
         (-> new-data
             (assoc :needed-values subordinates)
             (assoc :subordinate-values {})
             (update-in [:pending-actions]
                        conj [add-task (:queue management)
                              eval-expression-if-ready reporter management]))
         (-> new-data
             (dissoc :needed-values)
             (dissoc :subordinate-values)
             (update-value-source reporter nil)
              ;;; Note, we are not attended, so it is OK to change the
              ;;; value without notifying the callbacks.
             (assoc :by :manager)
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
  [expression management]
  (mm/update-in!
   (:cache management) [expression]
   #(or % (reporter/new-reporter :expression expression
                                 :manager-type :cached-eval))))

(defn cache-manager
  "Manager that looks up the value of a reporter's expression in a cache of
  reporters."
  [reporter management]
  (let [expression (:expression (reporter/data reporter))
        cache (:cache management)]
    ;;; We do side-effects, which can't run inside a swap!, so we have
    ;;; to use call-with-latest-value.
    (call-with-latest-value
     #(reporter/attended? reporter)
     (fn [attended]
       (let [value-source (when attended
                            (ensure-in-cache expression management))]
         (modify-and-act reporter
                         #(update-value-source % reporter value-source))
         ;; We can't do management until the registration is done,
         ;; or the source's manager will take it out of the cache.
         (when value-source (manage value-source management)))))))

(defn new-management
  "Create the manager data for a caching evaluator."
  []
  (map->ManagementImpl {:cache (mm/new-mutable-map)
                        :queue (new-priority-task-queue)
                        :manager-map {:cache cache-manager
                                      :eval eval-manager
                                      :cached-eval cached-eval-manager}}))

(defn manage
  "Assign the manager to the reporter
  and to any reporters referenced by its expression."
  [reporter management]
  (let [data (reporter/data reporter)
        manager-type (:manager-type data)]
    (when (and manager-type (not (:manager data)))
      (let [manager-fn (manager-type (:manager-map management))]
        (assert manager-fn (format "Unknown manager type: %s" manager-type))
        (reporter/set-manager! reporter manager-fn management))
      (doseq [term (:expression data)]
        (when (reporter/reporter? term)
          (manage term management))))))

(defn request
  "Request computation of a reference."
  [x management]
  (reporter/set-attendee! x :computation-request (fn [key value] nil))
  (manage x management)
  x)

(defn compute
  "Do all pending computations."
  [management]
  (run-all-pending-tasks (:queue management)))

(defn computation-value
  "Given a possible reporter, compute its value and return it."
  [x management]
  (if (reporter/reporter? x)
    (do
      (request x management)
      (compute management)
      (reporter/value x))
    x))


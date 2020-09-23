(ns cosheet2.calculator
  (:require (cosheet2 [reporter :refer [reporter? reporter-data reporter-value
                                        reporter-atom data-value
                                        valid? attended?
                                        set-calculator-data-if-needed!
                                        set-attendee! set-attendee-and-call!
                                        remove-attendee!
                                        inform-attendees]]
                      [mutable-map :as mm]
                      [task-queue :refer [run-all-pending-tasks
                                          run-some-pending-tasks
                                          add-task-with-priority]]
                      [utils :refer [swap-control-return!
                                     swap-and-act!
                                     update-new-further-action
                                     with-latest-value]])))

;;; The manager data record contains all the information that the
;;; expression calculators and cache calculators need.
;;; By making it a record, we can define our own print-method, to
;;; avoid infinite loops when it is printed out. (The queue will
;;; contain references back to the manager data record.)
(defrecord CalculatorData
    [queue      ; A task-queue of pending tasks,
                ; used by expression calculators.
     cache      ; A mutable map from expression to reporter,
                ; used by cache calculators.
     ])

(defmethod print-method CalculatorData [s ^java.io.Writer w]
  ;; Avoid huge print-outs.
  (.write w "<CalculatorData>"))

(defn new-calculator-data
  "Create a calculator data to support both application and cache calculators."
  [queue]
  (assert (instance? clojure.lang.Atom queue))
  (map->CalculatorData
   {:cache (mm/new-mutable-map)
    :queue queue}))

(defn propagate-calculator-data!
  "If this reporter hadn't already been activated, activate it
   and all reporters it depends on."
  [reporter cd]
  (when (reporter? reporter)
    (let [data (reporter-data reporter)]
      (when (not (:calculator-data data))
        (set-calculator-data-if-needed! reporter cd)
        (when-let [source (:value-source data)]
          (propagate-calculator-data! source cd))
        (doseq [term (:application data)]
          (propagate-calculator-data! term cd))))))

;;; Many kinds of calculators can get their value from another reporter.
;;; The copy value code provides support for that.
;;; It assumes the following two fields have been filled in appropriately:
;;;
;;;         :value-source A reporter whose value should be the
;;;                       value of this reporter.
;;; :value-source-is-canonical True if our value source is as good a
;;;                            representation of our computation as we are.
;;;                            This is used to find the canonical reporter when
;;;                            the cache calculator determines the cache key.
;;; :value-source-priority-delta The amount that we add to our priority
;;;                              to get the priority we give to our value
;;;                              source. Only needs to valid when there is
;;;                              a value source.
;;;     :dependent-depth The difference between the worst priority we bestow
;;;                      on any computation we depend on, and our priority.
;;;                      This is only valid when our value is valid.
;;;                      The purpose of this is to allow reporters that depend
;;;                      on several reporters know what priorities they
;;;                      should bestow on their dependents to fully prioritize
;;;                      one, and all its dependents, over another and all
;;;                      its dependents.
;;;     :further-actions A list of [function arg arg ...] calls that
;;;                      need to be performed. (These will never actually
;;;                      be stored in a reporter, but are added to the
;;;                      map before it is stored.)

(defn modify-and-act!
  "Atomically call the function on the reporter's data.
   The function should return the new data for the reporter,
   which may also contain a temporary field, :further-actions with
   a list of actions that should be performed."
  [reporter f]
  (swap-and-act! (reporter-atom reporter) f))

(defn update-value-and-dependent-depth
  "Given the data from a reporter, and the reporter, set the value
   and dependent-depth in the data, and request the appropriate propagation."
  [data reporter value dependent-depth]
  (if (and (= value (:value data)) (= dependent-depth (:dependent-depth data)))
    data
    (-> data
        (assoc :value value
               :dependent-depth dependent-depth)
        (update-new-further-action inform-attendees reporter))))

(defn copy-value
  "Copy the value in from to be the reporter's value, and update the
   reporter's dependent depth. Additionally, if anything changed, also
   run data-finalizer on the reporter's data, also giving it the reporter
   and calculator data."
  [reporter from data-finalizer]
  (with-latest-value [[value value-dependent-depth]
                      (let [data (reporter-data from)]
                        [(:value data) (or (:dependent-depth data) 0)])]
    (modify-and-act!
     reporter
     (fn [data]
       (let [cd (:calculator-data data)]
         (if (= (:value-source data) from)
           (let [our-depth (when (valid? value)
                             (+ value-dependent-depth
                                (:value-source-priority-delta data)))]
             (cond-> (update-value-and-dependent-depth
                      data reporter value our-depth)
               data-finalizer
               (data-finalizer reporter cd)))
           data))))))

(defn add-propagate-task
  "Add a task at the propagate priority for the reporter."
  [reporter & task]
  (let [data (reporter-data reporter)
        cd (:calculator-data data)]
      (apply add-task-with-priority
       ;; Propagating has to be prioritized before computing, as an early
       ;; priority computation may depend on a lower priority value, and needs
       ;; to be informed if that value changes.
             (:queue cd) (- (:priority data) 1e6) task)))

(defn copy-value-callback
  [& {[_ reporter] :key from :reporter}]
  (add-propagate-task reporter  copy-value reporter from nil))

(defn register-for-value-source
  "Register the callback for the value of the second
   reporter, when it is the value-source of our reporter."
  [reporter from callback cd]
  (with-latest-value
    [priority 
     (let [data (reporter-data reporter)]
       (when (= (:value-source data) from)
         (+ (:priority data) (:value-source-priority-delta data))))]
    ;; It is possible, with caching, for the same reporter to be used
    ;; as a dependent in several ways. We need to have a different key
    ;; for each case, or the source reporter will only record one of
    ;; them.
    (let [key (list :copy-value reporter)]
      (set-attendee-and-call!
       from key priority (when priority callback)))))


;;; The following functions are utilities to ask for computation of
;;; of a reporter and get its value.

(defn request
  "Request computation of a reporter, returning the reporter."
  [r cd]
  (propagate-calculator-data! r cd)
  (set-attendee! r ::computation-request 0 (fn [& _] nil))
  r)

(defn unrequest
  "Remove request for computation of a reporter."
  [r cd]
  (remove-attendee! r ::computation-request))

(defn compute
  "Do all pending computations, or if the second argument is provided,
   all or that many, which ever comes first."
  ([cd]
   (run-all-pending-tasks (:queue cd)))
  ([cd max-tasks]
   (run-some-pending-tasks (:queue cd) max-tasks)))

(defn computation-value
  "Given a possible reporter, compute its value and return it."
  [r cd]
  (if (reporter? r)
    (do (request r cd)
        (compute cd)
        (reporter-value r))
    r))

(defn current-value
  "Return the current value of the reporter, if it is valid. Otherwise,
   try to chase the value-source or run its application."
  [reporter]
  (if (reporter? reporter)
    (let [data (reporter-data reporter)
          value (data-value data)
          application (:application data)
          value-source (:value-source data)]
      (cond
        (valid? value)
        value
        (not (nil? value-source))
        (current-value value-source)
        application
        ;; There is an application for the reporter, apply it directly.
        ((or (:trace data) (fn [thunk] (thunk)))
         #(current-value (apply (fn [f & args] (apply f args))
                                (map current-value application))))
        (and (:calculator-data data) (not (attended? reporter)))
        ;; Maybe the calculator knows a different way to get the value.
        ;; Add an attendee, get the value, then take the attendee away.
        (do
          (set-attendee! reporter :request 0 (fn [& _] nil))
          (let [result (reporter-value reporter)]
            (set-attendee! reporter :request)
            result))
        true
        value))
    reporter))

(ns cosheet2.calculator
  (:require (cosheet2 [reporter :refer [reporter? data data-atom data-value
                                        value valid? attended?
                                        set-calculator-data-if-needed!
                                        set-attendee! remove-attendee!
                                        inform-attendees]]
                      [mutable-map :as mm]
                      [task-queue :refer [run-all-pending-tasks
                                          run-some-pending-tasks]]
                      [utils :refer [swap-control-return!]])))

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

(defn new-calculator-data
  "Create a calculator data to support both application and cache calculators."
  [queue]
  (assert (instance? clojure.lang.Atom queue))
  (map->CalculatorData
   {:cache (mm/new-mutable-map)
    :queue queue}))

(defn propagate-calculator-data!
  "Activate this reporter and the terms of its application, if any."
  [reporter cd]
  (when (reporter? reporter)
    (set-calculator-data-if-needed! reporter cd)
    (doseq [term (:application (data reporter))]
      (propagate-calculator-data! term cd))))

;;; Many kinds of calculators can get their value from another reporter.
;;; The copy value code provides support for that.
;;; It assumes the following two fields have been filled in appropriately:
;;;
;;;         :value-source A reporter whose value should be the
;;;                       value of this reporter.
;;; :value-source-priority-delta The amount that we add to our priority
;;;                              to get the priority we give to our value
;;;                              source. Only needs to valid when there is
;;;                              a value source.
;;;     :dependent-depth The difference between the worst priority we bestow
;;;                      on any computation we depend on, and our priority.
;;;                      This is only valid when our value is valid.
;;;                      The purpose of this is to allow reporters that depend
;;;                      on several reporters be able what priorities they
;;;                      should bestow on their dependents to fully prioritize
;;;                      one, and all its dependents over another and all
;;;                      its dependents.
;;;     :further-actions A list of [function arg arg ...] calls that
;;;                      need to be performed. (These will never actually
;;;                      be stored in a reporter, but are added to the
;;;                      map before it is stored.)

(defn update-new-further-action
  "Given a map, add an action to the further actions.
   This is used to request actions that affect state elsewhere than
   in the map. They will be done immediately after the map is stored back,
   in the order in which they were requested."
  [data & action]
  (update-in data [:further-actions] (fnil conj []) (vec action)))

(defn modify-and-act
  "Atomically call the function on the reporter's data.
   The function should return the new data for the reporter,
   which may also contain a temporary field, :further-actions with
   a list of actions that should be performed."
  [reporter f]
  (let [actions (swap-control-return!
                 (data-atom reporter)
                 (fn [data] (let [new-data (f data)]
                              [(dissoc new-data :further-actions)
                               (:further-actions new-data)])))]
    (doseq [action actions]
      (apply (first action) (rest action)))))

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

;;; The following functions are utilities to ask for computation of
;;; of a reporter and get its value.

(defn request
  "Request computation of a reporter, returning the reporter."
  [r cd]
  (set-attendee! r ::computation-request 0 (fn [& _] nil))
  (propagate-calculator-data! r cd)
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
        (value r))
    r))

(defn current-value
  "Run computation on the reporter, returning the current value,
   rather than tracking dependencies."
  [reporter]
  (if (reporter? reporter)
    (let [data (data reporter)
          value (data-value data)
          application (:application data)]
      (cond
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
          (let [result (value reporter)]
            (set-attendee! reporter :request)
            result))
        true
        value))
    reporter))

(defmethod print-method CalculatorData [s ^java.io.Writer w]
  ;; Avoid huge print-outs.
  (.write w "<ExpressionManagerData>"))

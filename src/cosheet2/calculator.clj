(ns cosheet2.calculator
  (:require (cosheet2 [reporter :refer [reporter? data value data-value
                                        valid? attended?
                                        set-calculator-data-if-needed!
                                        set-attendee! remove-attendee!]]
                      [mutable-map :as mm]
                      [task-queue :refer [run-all-pending-tasks
                                          run-some-pending-tasks]])))

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

(ns cosheet.computation-manager
  (:require (cosheet [reporter :as reporter]
                     [task-queue :refer :all]
                     [utils :refer [dissoc-in update-in-clean-up
                                    swap-returning-both!
                                    swap-control-return!
                                    call-with-latest-value]]
                     [mutable-map :as mm])))

;;; The management data structure is a map that contains all the
;;; information that the various managers and propagators need. It
;;; includes:
;;;       :queue A task-queue of pending tasks.
;;; :manager-map A map from :manager-type of a reporter to a
;;;              manager function.
;;;       :cache A mutable map from expression to reporter.
;;;              (present if cache reporters are supported.)

;;; The following fields are used in reporters, in addition to the
;;; standard reporter fields:
;;;         :expression The expression describing the computation that
;;;                     gives the value of this reporter 
;;;       :manager-type A keyword describing what kind of manager the
;;;                     reporter should have.
;;;       :value-source A reporter whose value should be the value of this
;;;                     reporter.
;;;      :needed-values A set of reporters whose values this reporter needs
;;;                     to evaluate its expression and doesn't know.
;;; :subordinate-values A map from reporters whose values this reporter needs
;;;                     to evaluate its expression to their values.

(defn modify-and-act
  "Atomiclly call the function on the reporter's data.
   The function should return the new data for the reporter
   and a list of actions that should be performed."
  [reporter f]
  (let [actions (swap-control-return! (reporter/data-atom reporter) f)]
    (doseq [action actions]
      (apply (first action) (rest action)))))

(def eval-expression-if-ready)

(defn copy-value-callback
  [[_ to] from]
  (call-with-latest-value
   #(reporter/value from)
   (fn [value] (reporter/set-value! to value))))

(defn register-copy-value
  "Register the need to copy (or not copy) the value from
   the first reporter to become the value of the second."
  [from to]
  (call-with-latest-value
   #(when (= (:value-source to) from) [copy-value-callback])
   (fn [callback] (apply reporter/set-attendee!
                         from '(:copy-value to) callback))))

;;; TODO: implement the next two
(defn copy-subordinate-callback
  [to from management]
  (call-with-latest-value
   #(reporter/value from)
   (fn [value]
     (modify-and-act
      to
      (fn [data]
        (if (and (or (contains? (:needed-values data) from)
                     (contains? (:subordinate-values data) from))
                 (not= value
                       (get (:subordinate-values data) from reporter/invalid)))
          (if (reporter/valid? value)
            (let [new-data (-> data
                               (update-in [:needed-values] disj from)
                               (assoc-in [:subordinate-values from] value))]
              [new-data
               (when (empty? (:needed-values new-data))
                 [[add-task (:queue management)
                   eval-expression-if-ready to management]])])
            (let [new-data (-> data
                               (update-in [:needed-values] conj from)
                               (update-in [:subordinate-values] dissoc from)
                               (assoc :value reporter/invalid)
                               (dissoc :value-source))]
              [new-data
               (concat
                (when (not= (:value new-data) (:value data))
                  [reporter/inform-attendees to])
                (when (not= (:value-source new-data) (:value-source data))
                  [register-copy-value (:value-source data) to]))]))
          [data nil]))))))

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
             value (apply (first application) (rest application))
             actions (let [old-value-source (:value-source data)]
                       (when (and old-value-source
                                  (not= value old-value-source))
                         [[register-copy-value old-value-source reporter]]))]
         (if (reporter/reporter? value)
           [(assoc data :value-source value)
            (concat actions
                    [[register-copy-value value reporter]
                     [manage value management]])]
           [(-> data
                (assoc :value value)
                (dissoc :value-source))
            (concat actions
                    (when (not= value (:value data))
                      [[reporter/inform-attendees reporter]]))]))
       [data nil]))))

(defn eval-manager
  "Manager for an eval reporter."
  [reporter management]
  (modify-and-act
   reporter
   (fn [data]
     (let [expr (:expression data)
           subordinate-reporters (set (filter reporter/reporter? expr))
           copy-requests (map (fn [subordinate]
                                [register-copy-subordinate
                                 subordinate reporter management])
                              subordinate-reporters)]
       (if (reporter/data-attended? data)
         [(-> data
              (assoc :needed-values subordinate-reporters)
              (assoc :subsidary-values {}))
          (conj copy-requests
                [add-task (:queue management)
                 eval-expression-if-ready reporter management])]
         [(-> data
              (dissoc :needed-values)
              (dissoc :subsidary-values)
              (dissoc :value-source)
              ;;; Note, we are not attended, so it is OK to change the
              ;;; value without notifying the callbacks.              
              (assoc :value reporter/invalid))
          copy-requests])))))

(defn cached-eval-manager
  "Manager for an eval reporter that is also stored in the cache."
  [reporter management]
  (when (not (reporter/attended? reporter))
       (dissoc (:cache management) (:expression (reporter/data reporter))))
  (eval-manager reporter management))

(defn ensure-in-cache
  "Get or make an cached-eval reporter for the given expression in the cache."
  [expression management]
  (let [cache (:cache management)]
    (mm/update-in!
     cache [expression]
     #(or % (reporter/new-reporter :expression expression
                                   :manager-type :cached-eval)))
    (manage (mm/get! cache expression) management)))

(defn cached-manager
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
       (if attended
         (let [value-source (ensure-in-cache expression management)]
           (swap-returning-both!
            (reporter/data-atom reporter) assoc :value-source value-source)
            ;;; The register-copy-value has to be done before the
            ;;; value-source is managed, or the manager will take it
            ;;; out of the cache.
           (register-copy-value value-source reporter)
           (manage value-source management))
         (let [value-source
               (:value-source
                (first (swap-returning-both!
                        (reporter/data-atom reporter) dissoc :value-source)))]
           (register-copy-value value-source reporter)))))))

(defn new-management
  "Create the manager data for a caching evaluator."
  []
  {:cache (mm/new-mutable-map)
   :queue (new-priority-task-queue)
   :manager-map {:cached cached-manager
                 :eval eval-manager
                 :cached-eval cached-eval-manager}})

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

(defn new-manager
  "Return an object that will set up management of a reporter"
  []
  (let [management (new-management)]
    (fn [reporter] (manage reporter management))))

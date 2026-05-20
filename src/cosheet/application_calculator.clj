(ns cosheet.application-calculator
  (:require (cosheet [reporter :refer [make-reporter
                                        reporter? reporter-valid? data-valid? invalid
                                        reporter-data data-value-or-invalid
                                        set-attendee! set-attendee-and-call!
                                        remove-attendee!
                                        data-attended?]]
                      [calculator :refer [propagate-calculator-data!
                                          modify-and-act! update-to-invalid
                                          update-value-and-dependent-depth
                                          copy-value
                                          register-for-value-source]]
                      [task-queue :refer [add-task-with-priority]]
                      [utils :refer [with-latest-value
                                     update-new-further-action
                                     assoc-if-non-empty]])))

;;; Manage the (re)computation of application reporters, using a
;;; priority queue.

;;; What makes applications interesting is that an application might
;;; return another application as a value. Here are a couple of
;;; examples of when that is necessary:
;;;    * A conditional evaluates its condition, and then chooses a branch,
;;;      depending on the result of the condition.
;;;    * A funtion does a database call to get list of items matching a
;;;      query, and then maps over the items in the list.
;;; In these cases, the information that a computation requires can't
;;; be fixed ahead of time; it depends on how the computation
;;; unfolds. So it isn't possible to set up a fixed dependency graph
;;; for the computation. But applications amount to fixed dependency
;;; graphs: Their arguments are supposed to be the data that the
;;; function depends on, so they can be re-run if the arguments
;;; change.
;;;
;;; Either the applications need to be given possibly unnecessary
;;; arguments that will only needed sometimes, or we need some other
;;; mechanism. We go with the mechanism of letting an application
;;; reporter return a reporter for another application. In the
;;; examples above, the arguments of the initial application are only
;;; the data used to determine what additional computation is
;;; needed. The initial application uses that data to construct a
;;; follow up application to do the rest of the work, with arguments
;;; for the data it actually needs, The initial application then
;;; returns the reporter for the follow-up application as its answer.

;;; Here is how we use two of the fields from calculator.clj:
;;;        :value-source If our application returns a reporter, it is stored
;;;                      here, so that its value becomes our value.
;;; :value-source-priority-delta
;;;                      We want recomputation of our subordinates to
;;;                      have priority over recomputation of our
;;;                      value-source, because the recomputation of a
;;;                      subordinate may change what value source we
;;;                      need, removing the need to recompute our
;;;                      current value source. To ensure that, we set
;;;                      :value-source-priority-delta to one more than
;;;                      the max of the dependent depth of all our
;;;                      subordinates.

;;; Application reporters use these additional fields:
;;;         :application The application describing the computation that
;;;                      gives the value of this reporter
;;;  :subordinate-values A map from reporters this reporter needs to
;;;                      run its application to a pair of the last
;;;                      valid value it saw for them and their
;;;                      dependent-depth.  The pair is kept even if
;;;                      the value later goes invalid. The map is not
;;;                      present if nothing is attending to this
;;;                      reporter.
;;;       :needed-values This is only valid if anything is attending to
;;;                      the reporter. In that case, it is a (possibly
;;;                      empty) set of reporters whose values this
;;;                      reporter needs to run its application and
;;;                      that it doesn't have a valid value for.
;;;  :former-application-value
;;;                      The previous result of our application, if we
;;;                      know it, and we don't currently have a valid
;;;                      value, and we've kept demand since it was
;;;                      calculated. (If our application returned a
;;;                      reporter, this will be that reporter, rather
;;;                      than our former value, which came from that
;;;                      reporter.) This will hold invalid if we don't
;;;                      know what it is. :former-application-value
;;;                      serves two purposes.
;;;                      First, if some of our arguments have gone
;;;                      invalid, but not changed values, then if our
;;;                      arguments retake their last valid values, we
;;;                      will reuse this, instead of calling our
;;;                      application again.
;;;                      Second, if this is a reporter, and hence our
;;;                      old value-source, we maintain demand for it
;;;                      it to keep it alive. Even if we get a
;;;                      different value source, while its value being
;;;                      computed, our old value source can stay
;;;                      cached and available for reuse by upcoming
;;;                      computations of the current value
;;;                      source. Sometimes, for example, our new value
;;;                      source returns our old value source as its
;;;                      value.
;;; :arguments-unchanged Present, and equal to true, if we have a
;;;                      :former-application-value and all of the
;;;                      arguments we depend on that are currently
;;;                      valid have the same values as when we
;;;                      computed the former-application-value.
;;;  :requested-priority The priority that we have used to determine
;;;                      our requests' priorities. If our :priority
;;;                      changes from that, we have to redo our
;;;                      requests.

;;; The computation is multi-threaded. Its unit of computation is
;;; running an application, and propagating its result to everywhere
;;; it is used. That occurs in one thread. And it might enable the
;;; running of more applications. All applications that are possible
;;; ready to run are kept in a priority queue, which prioritizes the
;;; applications to run next, and lets multiple threads work on them.

;;; We can avoid using locks and TSM because we only provides eventual
;;; consistency; it is just copying information. But there is a
;;; danger:
;;;    * A data item is changed.
;;;    * Thread A is started to copy it to a place that depends on it.
;;;    * Thread A reads the data, but doesn't copy it yet.
;;;    * The data is changed again, and Thread B is started to copy it.
;;;    * Thread B reads and copies the data.
;;;    * Thread A writes its (stale) copy of the data.
;;; We now have the stale copy of the data in the places that depend
;;; on it, and no pending activity to update it.

;;; It might seem that doing the read inside an atomic update
;;; operation for copying would fix this problem. But that still
;;; leaves a problem when the source of copied data can
;;; change. Suppose there are two alternative sources, S and T for the
;;; value of an atom M. And suppose that M initially wants the data
;;; from source S. This sequence can happen:
;;;    * The desired source changes from S to T.
;;;    * Thread A starts a swap! on M to copy the latest value M should have.
;;;    * The swap! notices that the current value of M comes from S.
;;;    * It calls the function it is passed to get the value T.
;;;    * Meanwhile, the desired source changes from T back to S.
;;;    * Thread B starts a swap! to copy the latest value M should have.
;;;    * That thread copies the value from S to M.
;;;    * Thread A's swap! goes to finish, sees that the current value
;;;      is still the value from S, so it succeeds, setting the current value
;;;      to the stale one it's function got from T.

;;; Instead, we check, after doing a copy, that the information that
;;; was copied still matches the latest information, and redo the copy
;;; if it doesn't.

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

(def run-application-if-ready)
(def update-former-application-value)

(defn subordinate-depth
  "Return the max of the priorities of our subordinates relative to ours."
  [data]
  (let [depths (map second (vals (:subordinate-values data)))]
    (if (empty? depths)
      0
      (+ 1 (apply max depths)))))

(defn update-remove-unnecessary-former-application-value
  [data reporter cd]
  (cond-> data
    (data-valid? data)
    ;; We have finished computing a value, so the old application
    ;; is not holding onto anything useful.
    (update-former-application-value reporter invalid cd)))

(defn copy-value-and-cleanup-callback
  "Copy the value from our value source."
  [& {[_ reporter] :key from :reporter}]
  (copy-value reporter from update-remove-unnecessary-former-application-value))

(defn register-copy-value
  "Register to copy the value from our value source."
  [reporter from]
  (register-for-value-source reporter from copy-value-and-cleanup-callback))

(defn null-callback
  "We use this when we want to preserve demand for a reporter,
   but don't currently care about it's value."
  [& _]
  nil)

(defn register-demand-former-application-value
  "Register whether or not our former-application-value creates demand
  for the former-value argument, which must be a reporter."
  [reporter former-value cd]
  (assert (reporter? former-value))
  ;; It is possible, with caching, for the same reporter to be
  ;; both our value source and one of our subordinates. We
  ;; need to have a different key for the two cases, or that
  ;; reporter will only record one of them.
  (let [key (list :demand-former-application-value reporter)]
    (with-latest-value
        [maches-former-application-value
         (= (:former-application-value (reporter-data reporter)) former-value)]
        (if maches-former-application-value
          (set-attendee! former-value key Double/MAX_VALUE null-callback)
          (remove-attendee! former-value key)))))

(defn update-value-source
  "Given the data from a reporter, and the reporter, set the value-source
   to the given source, and request the appropriate registrations."
  [data reporter source]
  ;; We must only set to non-nil if there are attendees for our value,
  ;; otherwise, we will create demand when we have none ourselves.
  (assert (or (nil? source) (data-attended? data)))
  (let [source-key :value-source
        original-source (source-key data)]
    (if (= source original-source)
      data
      (reduce
       (fn [data src]
         (update-new-further-action
          data register-copy-value reporter src))
       (assoc-if-non-empty data source-key source)
       ;; Add the new source before removing any old one, so that any
       ;; subsidiary reporters common to both will always have demand.
       (filter identity [source original-source])))))

(defn update-former-application-value
  "Given the data from a reporter, and the reporter, set the
  former-application-value to the given value, and if the value is a
  reporter, request the appropriate registrations."
  [data reporter value cd]
  ;; We must only set to a valid value if there are attendees for our
  ;; value, otherwise, we will create demand when we have none
  ;; ourselves.
  (assert (or (not (reporter-valid? value)) (data-attended? data)))
  (let [recorded-former-value (:former-application-value data)]
    (if (= value recorded-former-value)
      data
      (let [data (-> data
                     (assoc :former-application-value value)
                     (assoc :arguments-unchanged (reporter-valid? value)))]
        (reduce
         (fn [data src]
           (update-new-further-action data
                                      register-demand-former-application-value
                                      reporter src cd))
         data
         ;; Add the new source before removing any old one, so that any
         ;; subsidiary reporters common to both will always have demand.
         (filter reporter? [value recorded-former-value]))))))

(defn copy-subordinate
  [reporter from cd]
  (with-latest-value [[value dependent-depth]
                      (let [data (reporter-data from)]
                        [(data-value-or-invalid data)
                         (or (:dependent-depth data) 0)])]
    (modify-and-act!
     reporter
     (fn [data]
       (let [same-value
             (= value (get-in data [:subordinate-values from 0] ::not-found))]
         (if (or (not (data-attended? data))
                 (not (contains? data :needed-values))
                 (if (contains? (:needed-values data) from)
                   (not (reporter-valid? value))
                   same-value))
           data
           ;; A value that we care about changed.  We are invalid
           ;; until the recomputation runs, which may not be for a
           ;; while.
           (let [last-application-value (or (:value-source data)
                                            (data-value-or-invalid data))
                 newer-data (cond-> (update-value-and-dependent-depth
                                     data reporter invalid nil)
                              (not= last-application-value invalid)
                              ;; We must do the copy to
                              ;; former-application-value before
                              ;; clearing value-source, so the former value
                              ;; always has attendees.
                              (#(-> %
                                    (update-former-application-value
                                     reporter last-application-value cd)
                                    (update-value-source reporter nil))))]
             (if (reporter-valid? value)
               (let [new-data
                     (cond-> (update-in newer-data [:needed-values] disj from)
                       (not same-value)
                       (#(-> %
                             (assoc-in [:subordinate-values from]
                                       [value dependent-depth])
                             (dissoc :arguments-unchanged))))]
                 (if (empty? (:needed-values new-data))
                   (if (:arguments-unchanged new-data)
                     ;; We have re-confirmed all old values for
                     ;; the old source. Make it current again.
                     (let [{:keys [former-application-value]} new-data]
                       (-> (if (reporter? former-application-value)
                             (update-value-source
                              new-data reporter former-application-value)
                             (update-value-and-dependent-depth
                              new-data reporter former-application-value
                              (subordinate-depth data)))
                           (update-former-application-value
                            reporter invalid cd)))
                     ;; Some value changed. Schedule recomputation.
                     (update-new-further-action
                      new-data 
                      add-task-with-priority (:queue cd) (:priority data)
                      run-application-if-ready reporter cd))
                   new-data))
               (update-in newer-data [:needed-values] conj from)))))))))

(defn copy-subordinate-callback
  [& {reporter :key from :reporter :as keys}]
  (let [data  (reporter-data reporter)
        cd (:calculator-data data)]
    (copy-subordinate reporter from cd)))

(defn register-copy-subordinate
  "Register the need to copy (or not copy) the value from the first reporter
  for use as an argument of the second."
  [reporter from cd]
  (with-latest-value
    [[priority callback]
     (let [data (reporter-data reporter)]
       (when (or (contains? (get data :needed-values #{}) from)
                 (contains? (:subordinate-values data) from))
         ;; We make the priority of calculating our subordinate
         ;; one worse. That way, shallow computations will finish before
         ;; deep ones, as their subordinates will have better priorities.
         [(+ (:priority data) 1) copy-subordinate-callback]))]
    (if (nil? callback)
      (remove-attendee! from reporter)
      (set-attendee-and-call! from reporter priority callback))))

(defn request-each-register-copy-subordinate
  "Register all the subordinateds this reporter needs."
  [data reporter subordinates cd]
  (reduce
   (fn [data subordinate]
     (update-new-further-action
      data register-copy-subordinate reporter subordinate cd))
   data subordinates))

(defn run-application-if-ready
  "If all the arguments for the reporter are ready, and we don't have
  a value, run the application."
  [reporter cd]
  (modify-and-act!
   reporter
   (fn [data]
     (if (or
          ;; It is possible that we lost demand for this reporter
          ;; since the request to run it was queued, but we
          ;; haven't been informed yet, so this fact is not
          ;; reflected in :needed-values.
          (not (data-attended? data))
          ;; It is possible to get several run-application-if-ready
          ;; queued up for the same reporter. In that case, the first
          ;; one to run will run the application, and the later ones
          ;; will notice that the value is valid, and not bother to
          ;; re-evaluate.
          (data-valid? data) ; 
          ;; We don't run if we still need values, or if
          ;; :needed-values is nil, which means that
          ;; nobody is attending to the reporter.
          (not= (:needed-values data) #{}))
       data
       (let [value-map (:subordinate-values data)
             application (map #(first (get value-map % [%]))
                              (:application data))
             value (apply (first application) (rest application))
             subordinate-depth (subordinate-depth data)]
         (if (reporter? value)
           (-> data
               ;; We have to set our value source first, so we
               ;; generate demand for the new value, before we
               ;; activate it.
               (update-value-source reporter value)
               (assoc :value-source-priority-delta (+ 1 subordinate-depth))
               (update-new-further-action propagate-calculator-data! value cd))
           (-> data
               (update-value-and-dependent-depth
                reporter value subordinate-depth)
               (update-value-source reporter nil)
               (update-former-application-value reporter invalid cd))))))))

(defn do-application-calculate
  "The calculation work for an application reporter."
  [reporter cd]
  (modify-and-act!
   reporter
   (fn [data]
     (let [same-attended (= (data-attended? data)
                            (contains? data :needed-values))
           same-priority (or (= (:priority data) (:requested-priority data))
                             (not (data-attended? data)))]
       (if (and same-attended same-priority)
         data
         (let [subordinates (set (filter reporter?
                                         (:application data)))
               new-data (-> data
                            (request-each-register-copy-subordinate
                             reporter subordinates cd)
                            (assoc :requested-priority (:priority data)))]
           (if same-attended
             ;; Only the priority changed. We need only also re-register for
             ;; the value copying, to update the priority we pass down.
             (cond-> new-data
               (:value-source new-data)
               (update-new-further-action
                register-copy-value reporter (:value-source new-data)))
             (if (data-attended? new-data)
               (-> new-data
                   (assoc :needed-values subordinates)
                   (assoc :subordinate-values {})
                   (update-new-further-action
                    add-task-with-priority (:queue cd)
                    (:priority data)
                    run-application-if-ready reporter cd))
               (-> new-data
                   (dissoc :needed-values)
                   (dissoc :subordinate-values)
                   (update-to-invalid)
                   (assoc :dependent-depth nil)
                   (update-former-application-value reporter invalid cd)
                   (update-value-source reporter nil))))))))))

(defn application-calculator
  [reporter cd]
  (add-task-with-priority
   (:queue cd) (:priority (reporter-data reporter))
   do-application-calculate reporter cd))

(defn make-application-R
  "Takes an application, and optionally a trace thunk, and a calculator,
  and additional arguments, and returns a new application reporter.
  But if no calculator is specified and none of the parts are
  reporters, then just evaluates the expression.
  The trace thunk should be a function that calls its one argument. It
  should be created at the point in the code where an application is
  generated. It will be placed on the stack by
  calculator/current-value, so that the stack backtrace will contain a
  record of where applications were created. Without the trace, stack
  will just contain a bunch of recursive calls to current-value."
  [application & {:keys [trace calculator]
                  :as args
                  :or {calculator application-calculator}}]
  ;; Catch some errors that leave no stack trace.
  (assert ((some-fn ifn? reporter?) (first application)))
  (if (and (not (some reporter? application))
           (= calculator application-calculator))
    ;; In this case, none of the arguments are reporters, and we have
    ;; an application calculator, so just run the application now.  No
    ;; need to make a reporter for it.  (Of course, the application
    ;; might return a reporter.)
    (apply (first application) (rest application))
    ;; In this case, either we can't run the application yet, or it
    ;; might have a caching calculator.  If it has a caching
    ;; calculator, we don't want to run the application now, even if
    ;; we could, because we want to cache its computation.  That way,
    ;; if the computation returns an application reporter, all calls
    ;; will return the identical reporter, from the cache, so that
    ;; reporter's computation won't be duplicated either.
    (apply make-reporter
           :application application
           :trace trace
           :calculator calculator
           (apply concat (dissoc args :trace :calculator)))))

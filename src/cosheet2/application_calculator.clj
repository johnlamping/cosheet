(ns cosheet2.application-calculator
  (:require (cosheet2 [reporter :as reporter]
                      [calculator :refer [propagate-calculator-data!]]
                      [task-queue :refer [add-task
                                          add-task-with-priority]]
                      [utils :refer [swap-control-return!
                                     with-latest-value
                                     assoc-if-non-empty]])))

;;; Split out update-value-source and update-old-value-source.
;;; Similarly for register-copy-value.

;;; Manage the (re)computation of application reporters using a priority queue.

;;; The following fields are used in reporters, in addition to the
;;; standard reporter fields.
;;; Provided by the original creator of the reporter:
;;;         :application The application describing the computation that
;;;                     gives the value of this reporter 
;;; Added by this manager:
;;;  :subordinate-values A map from reporters this reporter needs
;;;                      for run its application to a pair of the last
;;;                      valid value it saw for them and their dependent-depth.
;;;                      The pair is kept even if the value later goes
;;;                      invalid. The map is not present if
;;;                      nothing is attending to the reporter.
;;;       :needed-values A set of reporters whose values this reporter needs
;;;                      to run its application and that it doesn't have a
;;;                      valid value for. Not present if nothing is
;;;                      attending to the reporter.
;;;        :value-source A reporter whose value should be the value of this
;;;                      reporter if we have no needed-values.
;;;    :old-value-source The previous :value-source, if we know it and
;;;                      we haven't yet gotten a value from the
;;;                      current value source. This serves two
;;;                      purposes. If we don't yet have a current
;;;                      value source, and some of our arguments have
;;;                      just gone invalid, but not changed values,
;;;                      then this will become the value source again,
;;;                      if our arguments retake their last valid
;;;                      values. Second, even if we have a new value source,
;;;                      but don't have its value yet, we will keep
;;;                      this reporter and keep generating demand for it,
;;;                      causing its sub-computations to be kept active,
;;;                      so that if they were cached, they will be available
;;;                      for reuse by the subcomputations of the current
;;;                      value source, even ones it hasn't generated yet.
;;; :value-source-priority-delta The amount that we add to our priority
;;;                              to get the priority we give to our value
;;;                              source. Only needs to valid when there is
;;;                              a value source
;;; :arguments-unchanged Present, and equal to true, if we have an
;;;                      old-value-source and we have not
;;;                      seen a valid value different from the ones
;;;                      used to compute it.
;;;  :requested-priority The priority that we have used to determine
;;;                      our requests' priorities. If our :priority changes
;;;                      from that, we have to redo our requests.
;;;     :dependent-depth The difference between the worst priority we bestow
;;;                      on any computation we depend on and our priority.
;;;                      This is only valid when our value is valid.
;;;                      The purpose of this is to have the computation
;;;                      of our subordinates have an earlier (lower) priority
;;;                      that the computation of our value source. That way,
;;;                      if a change makes both a subordinate and our value
;;;                      source invalid, the subordinate will be recomputed
;;;                      first. We want that, because the recomputation of
;;;                      the subordinate may change what value source we
;;;                      need, removing the need to recompute our current
;;;                      value source.
;;;                      To do this, the priority of our demand for our
;;;                      value source is our priority plus one more than
;;;                      the max of the dependent depth of all our
;;;                      subordinates.
;;;                      That makes our dependent depth be one more than
;;;                      the dependent depth of our value source plus the
;;;                      largest dependent depth of our subordinates.
;;;     :further-actions A list of [function arg arg ...] calls that
;;;                      need to be performed. (These will never actually
;;;                      be stored in a reporter, but are added to the
;;;                      map before it is stored.)

;;; The computation is multi-threaded, but can avoid using locks and
;;; TSM because it only provides eventual consistency; it is just copying
;;; information. The danger is that in between a read and a copy in
;;; one thread, the data that was read will be changed, and another
;;; thread will complete a read and copy of the new information, only to
;;; have the first thread overwrite it with the stale information.
;;;
;;; Doing the read inside an atomic update operation for copying
;;; doesn't work. Consider the copied data starting out at A, and
;;; source value changing from A to B, and then back to A. An atomic
;;; swap! in the copy reads the copy's current value as A, then the function
;;; provided to the swap! reads the intermediate source value B and
;;; returns it. But before the swap! finishes, another thread sets the
;;; copy's value back to the final A. Now, when the original swap!
;;; goes to finish, it will see that the copy's value is still A, like
;;; it initially read, so the swap! succeeds, and sets the copy to the
;;; stale B.
;;;
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
                 (reporter/data-atom reporter)
                 (fn [data] (let [new-data (f data)]
                              [(dissoc new-data :further-actions)
                               (:further-actions new-data)])))]
    (doseq [action actions]
      (apply (first action) (rest action)))))

(def run-application-if-ready)
(def update-old-value-source)

(defn subordinate-depth
  "Return the max of the priorities of our subordinates relative to ours."
  [data]
  (let [depths (map second (vals (:subordinate-values data)))]
    (if (empty? depths)
      0
      (+ 1 (apply max depths)))))

(defn update-value-and-dependent-depth
  "Given the data from a reporter, and the reporter, set the value
   and dependent-depth in the data,
   and request the appropriate propagation."
  [data reporter value dependent-depth]
  (if (and (= value (:value data)) (= dependent-depth (:dependent-depth data)))
    data
    (-> data
        (assoc :value value
               :dependent-depth dependent-depth)
        (update-new-further-action reporter/inform-attendees reporter))))

(defn copy-value
  [reporter from cd]
  (with-latest-value [[value value-dependent-depth]
                      (let [data (reporter/data from)]
                        [(:value data) (or (:dependent-depth data) 0)])]
    (modify-and-act
     reporter
     (fn [data]
       (if (= (:value-source data) from)
         (let [our-depth (when (reporter/valid? value)
                           (+ value-dependent-depth
                              (:value-source-priority-delta data)))]
           (cond-> (update-value-and-dependent-depth
                    data reporter value our-depth)
             (reporter/valid? value)
             ;; We have finished computing a value, so the old source
             ;; is not holding onto anything useful.
             (update-old-value-source reporter nil cd)))
         data)))))

(defn copy-value-callback
  [& {[_ reporter] :key from :reporter}]
  (let [cd (:calculator-data (reporter/data reporter))]
    (add-task-with-priority
     ;; Propagating has to be prioritized before computing, as an early
     ;; priority computation may depend on a lower priority value, and needs
     ;; to be informed if that value changes.
     (:queue cd) (- (:priority (reporter/data reporter)) 1e6)
     copy-value reporter from cd)))

(defn null-callback
  "We use this when we want to preserve demand for a reporter,
   but don't currently care about it's value."
  [& _]
  nil)

(defn register-copy-value
  "Register the need to copy (or not copy) the value from the second
   reporter to become the value of our reporter."
  [reporter from cd]
  (with-latest-value
    [priority 
     (let [data (reporter/data reporter)]
       (when (= (:value-source data) from)
         (+ (:priority data) (:value-source-priority-delta data))))]
    ;; It is possible, with caching, for the same reporter to be
    ;; both our value source and one of our subordinates. We
    ;; need to have a different key for the two cases, or the
    ;; reporter will only record one of them.
    (let [key (list :copy-value reporter)]
      (reporter/set-attendee-and-call!
       from key priority (when priority copy-value-callback)))))

(defn register-demand-old-value
  "Register the need to demand (or not demand) the value from the second
   reporter."
  [reporter from cd]
  (with-latest-value
    [has-source
     (= (:old-value-source (reporter/data reporter)) from)]
    ;; It is possible, with caching, for the same reporter to be
    ;; both our value source and one of our subordinates. We
    ;; need to have a different key for the two cases, or the
    ;; reporter will only record one of them.
    (let [key (list :demand-old-value reporter)]
      (if has-source
        (reporter/set-attendee! from key Double/MAX_VALUE null-callback)
        (reporter/remove-attendee! from key)))
   ))

(defn update-value-source
  "Given the data from a reporter, and the reporter, set the value-source
   to the given source, and request the appropriate registrations.
   If the optional keyword :old true is passed,
   update the old-value-source, rather than value-source."
  [data reporter source cd & {:keys [old]}]
  ;; We must only set to non-nil if there are attendees for our value,
  ;; otherwise, we will create demand when we have none ourselves.
  (assert (or (nil? source) (reporter/data-attended? data)))
  (let [source-key (if old :old-value-source :value-source)
        original-source (source-key data)]
    (if (= source original-source)
      data
      (reduce
       (fn [data src]
         (update-new-further-action data register-copy-value reporter src cd))
       (if source
         (assoc data source-key source)
         (dissoc data source-key))
       ;; Add the new source before removing any old one, so that any
       ;; subsidiary reporters common to both will always have demand.
       (filter identity [source original-source])))))

(defn update-old-value-source
  "Given the data from a reporter, and the reporter, set the old-value-source
   to the given source, and request the appropriate registrations."
  [data reporter source cd & {:keys [old]}]
  ;; We must only set to non-nil if there are attendees for our value,
  ;; otherwise, we will create demand when we have none ourselves.
  (assert (or (nil? source) (reporter/data-attended? data)))
  (let [original-source (:old-value-source data)]
    (if (= source original-source)
      data
      (let [data (-> data
                     (assoc-if-non-empty :old-value-source source)
                     (dissoc :arguments-unchanged))]
        (reduce
         (fn [data src]
           (update-new-further-action data register-demand-old-value
                                      reporter src cd))
         data
         ;; Add the new source before removing any old one, so that any
         ;; subsidiary reporters common to both will always have demand.
         (filter identity [source original-source]))))))

(defn copy-subordinate
  [reporter from cd]
  (with-latest-value [[value dependent-depth]
                      (let [data (reporter/data from)]
                        [(:value data) (or (:dependent-depth data) 0)])]
    (modify-and-act
     reporter
     (fn [data]
       (let [same-value
             (= value (get-in data [:subordinate-values from 0] ::not-found))]
         (if (or (not (reporter/data-attended? data))
                 (not (contains? data :needed-values))
                 (if (contains? (:needed-values data) from)
                   (= value reporter/invalid)
                   same-value))
           data
           ;; A value that we care about changed.
           ;; We are invalid until the recomputation runs,
           ;; which may not be for a while.
           (let [current-source (:value-source data)
                 newer-data (cond-> (update-value-and-dependent-depth
                                     data reporter reporter/invalid 0)
                              current-source
                              ;; We must do the copy from source to
                              ;; old-source before clearing source,
                              ;; so the old source always has attendees.
                              (#(-> %
                                    (update-old-value-source
                                     reporter current-source cd)
                                    (assoc :arguments-unchanged true)
                                    (update-value-source reporter nil cd))))]
             (if (reporter/valid? value)
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
                     (-> new-data
                         (update-value-source
                          reporter (:old-value-source new-data) cd)
                         (update-old-value-source reporter nil cd))
                     ;; Some value changed. Schedule recomputation.
                     ;; TODO: Add priority here.
                     (apply update-new-further-action new-data 
                            add-task (:queue cd)
                            [run-application-if-ready reporter cd]))
                   new-data))
               (update-in newer-data [:needed-values] conj from)))))))))

(defn copy-subordinate-callback
  [& {reporter :key from :reporter :as keys}]
  (let [data  (reporter/data reporter)
        cd (:calculator-data data)]
    (add-task-with-priority
     ;; Propagating has to be prioritized before computing, as an early
     ;; priority computation may depend on a worse priority value, and needs
     ;; to be informed if that value changes.
     (:queue cd) (- (:priority data) 1e6)
     copy-subordinate reporter from cd)))

(defn register-copy-subordinate
  "Register the need to copy (or not copy) the value from the first reporter
  for use as an argument of the second."
  [reporter from cd]
  (with-latest-value
    [[priority callback]
     (let [data (reporter/data reporter)]
       (when (or (contains? (get data :needed-values #{}) from)
                 (contains? (:subordinate-values data) from))
         ;; We make the priority of calculating our subordinate
         ;; one worse. That way, shallow computations will finish before
         ;; deep ones, as their subordinates will have better priorities.
         [(+ (:priority data) 1) copy-subordinate-callback]))]
    (if (nil? callback)
      (reporter/remove-attendee! from reporter)
      (reporter/set-attendee-and-call! from reporter priority callback))))

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
  (modify-and-act
   reporter
   (fn [data]
     (if (or
          ;; It is possible that we lost demand for this reporter
          ;; since the request to run it was queued, but we
          ;; haven't been informed yet, so this fact is not
          ;; reflected in :needed-values.
          (not (reporter/data-attended? data))
          ;; It is possible to get several run-application-if-ready
          ;; queued up for the same reporter. In that case, the first
          ;; one to run will run the application, and the later ones
          ;; will notice that the value is valid, and not bother to
          ;; re-evaluate.
          (reporter/valid? (:value data))
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
         (if (reporter/reporter? value)
           (-> data
               ;; We have to set our value source first, so we
               ;; generate demand for the new value, before we
               ;; activate it.
               (update-value-source reporter value cd)
               (assoc :value-source-priority-delta (+ 1 subordinate-depth))
               (update-new-further-action propagate-calculator-data! value cd))
           (-> data
               (update-value-and-dependent-depth
                reporter value subordinate-depth)
               (update-value-source reporter nil cd)
               (update-old-value-source reporter nil cd))))))))

(defn do-application-calculate
  "The calculation work for an application reporter."
  [reporter cd]
  (modify-and-act
   reporter
   (fn [data]
     (let [same-attended (= (reporter/data-attended? data)
                            (contains? data :needed-values))
           same-priority (or (= (:priority data) (:requested-priority data))
                             (not (reporter/data-attended? data)))]
       (if (and same-attended same-priority)
         data
         (let [subordinates (set (filter reporter/reporter?
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
                register-copy-value reporter (:value-source new-data) cd))
             (let [new-data (update-value-and-dependent-depth
                             new-data reporter reporter/invalid 0)]
               (if (reporter/data-attended? new-data)
                 (-> new-data
                     (assoc :needed-values subordinates)
                     (assoc :subordinate-values {})
                     (update-new-further-action
                      add-task (:queue cd)
                      run-application-if-ready reporter cd))
                 (-> new-data
                     (dissoc :needed-values)
                     (dissoc :subordinate-values)
                     (update-value-source reporter nil cd)
                     (update-old-value-source reporter nil cd)))))))))))

(defn application-calculator
  [reporter cd]
  (add-task-with-priority
   (:queue cd) (:priority (reporter/data reporter))
   do-application-calculate reporter cd))


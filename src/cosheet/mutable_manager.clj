(ns cosheet.mutable-manager
  (:require (cosheet [reporter
                      :as reporter
                      :refer [valid? value set-value! set-manager!
                              attended? new-reporter invalid]]
                     [task-queue :refer [add-tasks-with-priorities]]
                     [debug :refer [simplify-for-print]]
                     [utils :refer [with-latest-value
                                    update-in-clean-up
                                    swap-control-return!]])))

;;; This is code that knows how to recompute reporters that depend on
;;; a function of a value that can change. The function, itself, must
;;; not return a reporter.

;;; Each function is associated with a list of keys that it depends on,
;;; and each update to the value is associated with a list of keys that
;;; the may have changed. When the atom is updated, all possibly affected
;;; functions are re-evaluated.

;;; It sets :application in reporters that it manages to be the
;;; function that should be called, together with any additional
;;; arguments beyond the value

;;; A reporter that we manage can be in one of three states:
;;;     Tracked and updated: We recompute the reporter as necessary. This is
;;;                          the state for reporters that have attendees.
;;;   Tracked and suspended: If something the reporter depends on changes,
;;;                          we set its value to :invalid, rather than
;;;                          recompute it. This is the state for
;;;                          reporters that don't have attendees, but
;;;                          have had them recently.  It means that if
;;;                          they gain attendees, we can avoid
;;;                          recomputation if nothing they depend on
;;;                          has changed.
;;;             Not tracked: We mark the reporter invalid, and don't recompute
;;;                          it. This is the state for reporters that
;;;                          haven't had attendees for a while. We
;;;                          hold no pointers to them, so they can be
;;;                          garbage collected if nothing else points
;;;                          to them either. But if they become
;;;                          attended to, they will always have to be
;;;                          recomputed.

(def suspended-reporter-size 10)

(defn new-mutable-manager-data
  [value queue]
  (atom {
         ;; The current value.
         :value value
         
         ;; A map from key to the set of reporters with that key that we
         ;; are tracking, and thus need to be checked on any change to that key.
         :subscriptions {}

         ;; A cache so that requests for the same computation can
         ;; share a reporter. It is a map from application to a
         ;; a reporter with that application, that we are tracking if there
         ;; is any.
         :application->reporter nil

         ;; A vector of maps whose keys are reporters that are tracked,
         ;; but not currently attended. These reporters are suspended.
         ;; When a reporter becomes unattended, we put it here, rather
         ;; than remove it immediately, so it is available if it should
         ;; be attended again. We cycle through the spots in this vector each
         ;; time the value is updated, forgetting about a reporter once
         ;; its spot is returned to.
         ;; The value is the keys of the reporter.
         :suspended-reporters->keys (vec (repeat suspended-reporter-size {}))

         ;; A map from reporter to its index in :suspended-reporters.
         :suspended-reporter->index {}

         ;; The position in :suspended-reporters where we are currently
         ;; adding reporters.
         :current-suspended-reporters-index 0

         ;; The queue of work remaining to do.
         :queue queue}))

(defn mutable-manager-queue
  [manager-data]
  (:queue @manager-data))

(defn current-mutable-value
  "Return the current value of the managed item."
  [manager-data]
  (:value @manager-data))


(defn track-reporter
  "Given the current manager data, a reporter, and its keys,
  add the information to make us track whether it needs updates."
  [data reporter keys]
  (let [application (:application (reporter/data reporter))]
    (-> data
        (update-in [:subscriptions]
                   (fn [subscriptions]
                     (reduce (fn [subscriptions key]
                               (update-in subscriptions [key]
                                          #((fnil conj #{}) % reporter)))
                             subscriptions keys)))
        (update-in [:application->reporter application]
                   ;; Since we don't always track reporters with no attendees,
                   ;; we might already have another reporter for this
                   ;; expression, in which case, leave it.
                   (fn [existing] (or existing reporter))))))

(defn untrack-reporter
  "Given the current manager data, a reporter, and its keys,
  remove the information to make us track whether it needs updates."
  [data reporter keys]
  (let [application (:application (reporter/data reporter))]
    (-> data
        (update-in [:subscriptions]
                   (fn [subscriptions]
                     (reduce (fn [subscriptions key]
                               (update-in-clean-up subscriptions [key]
                                                   #(disj % reporter)))
                             subscriptions keys)))
        (update-in-clean-up
                        [:application->reporter application]
                        ;; Since we don't always track reporters with
                        ;; no attendees, we might already have another
                        ;; reporter for this expression, in which
                        ;; case, leave it.
                        (fn [existing]
                          (if (= existing reporter) nil existing))))))

(defn suspend-reporter
  "Add the reporter to the suspension information, at the current index."
  [data reporter keys]
  (if ((:suspended-reporter->index data) reporter)
    data ; already suspended
    (let [index (:current-suspended-reporters-index data)]
      (-> data
          (update-in [:suspended-reporters->keys index]
                     (fn [reporters] (assoc reporters reporter keys)))
          (assoc-in [:suspended-reporter->index reporter] index)))))

(defn resume-reporter
  "Remove the reporter from the suspension information, at the current index."
  [data reporter]
  (let [index ((:suspended-reporter->index data) reporter)]
    (if index
      (-> data
          (update-in [:suspended-reporters->keys index]
                     (fn [reporters] (dissoc reporters reporter)))
          (update-in [:suspended-reporter->index]
                     (fn [reporters] (dissoc reporters reporter))))
      data ; wasn't suspended
      )))

(defn compute-reporter
  "Given the current mutable value, compute the value for the reporter."
  [value reporter]
  (let [[f & args] (:application (reporter/data reporter))]
    (set-value! reporter (apply f value args))))

(defn invalidate-reporter!
  "Mark the reporter as invalid, and stop tracking it if it is suspended."
  [manager-data reporter]
  (set-value! reporter reporter/invalid)
  ;; If this reporter is suspended, stop tracking it completely,
  ;; as we now know that it is invalid, so there is no point in further
  ;; tracking.
  (swap!
   manager-data
   (fn [data]
     (let [index (get-in data [:suspended-reporter->index reporter])]
       (if index
         (let [keys (get-in data [:suspended-reporters->keys index reporter])]
           (-> data
               (resume-reporter reporter)
               (untrack-reporter reporter keys)))
         data)))))

(defn reporter-data-changed!
  "Recompute the reporter using the latest value, if it is being attended to.
   Otherwise, mark it invalid, and remove it from tracking."
  [manager-data reporter]
  (with-latest-value [value (:value @manager-data)]
    (with-latest-value [attended (attended? reporter)]
      (if attended
        (compute-reporter value reporter)
        (invalidate-reporter! manager-data reporter)))))

(defn advance-suspended-reporters-index!
  "Advance the unattended reporters index. Remove all reporters at the
   new index, and mark them invalid."
  [manager-data]
  (let [affected-reporters
        (swap-control-return!
         manager-data
         (fn [data]
           (let [index (mod (+ (:current-suspended-reporters-index data) 1)
                            suspended-reporter-size)
                 keys-map (get-in data [:suspended-reporters->keys index])
                 reporters (keys keys-map)]
             [(reduce (fn [data reporter]
                        (-> data
                            ; Takes it out of suspended info
                            (resume-reporter reporter)
                            (untrack-reporter reporter (keys-map reporter))))
                      (assoc data :current-suspended-reporters-index index)
                      reporters)
              reporters])))]
    ;; Since we are not tracking them, we can no longer trust their values.
    ;; But another thread may have started tracking them, so we update
    ;; their values depending on whether they are attended to.
    (doseq [reporter affected-reporters]
      (reporter-data-changed! manager-data reporter))))

(defn queue-data-changes [manager-data reporters]
  (doseq [reporter reporters]
    (reporter/set-value! reporter reporter/invalid))
  (let [queue (:queue @manager-data)]
    (add-tasks-with-priorities
       queue
       (map (fn [reporter] [(or (:priority (reporter/data reporter)) 0)
                            reporter-data-changed!
                            manager-data reporter])
            reporters))))

(defn reporters-for-keys-data-changed
  "Update all reporters that care about the changes represented by the keys."
  [manager-data keys]
  ;; It is OK to get the subscriptions outside of
  ;; with-latest-value, since it is sufficient to inform all the
  ;; ones that were active at some time after the value changed.
  (when (not (empty? keys))
    (let [subscriptions (:subscriptions @manager-data)
          ;; Avoid calling the same reporter twice.
          reporters (set (mapcat (partial get subscriptions) keys))]
      (queue-data-changes manager-data reporters))))

(defn all-reporters-data-changed
  "Update all reporters."
  [manager-data]
  (let [;; Avoid calling the same reporter twice.
        reporters (set (apply concat (vals (:subscriptions @manager-data))))]
    (queue-data-changes manager-data reporters)))

(defn manager-callback
  "Callback when a reporter starts or stops needing updates."
  [reporter manager-data keys]
  (with-latest-value [attended (attended? reporter)]
    (let [application (:application (reporter/data reporter))]
      (if attended
        (do
          (swap!
           manager-data
           (fn [data]
             (if ((:suspended-reporter->index data) reporter)
               (resume-reporter data reporter)
               (track-reporter data reporter keys))))
          (when (not (valid? (value reporter)))
            (reporter-data-changed! manager-data reporter)))
        (swap! manager-data
               (fn [data] (suspend-reporter data reporter keys)))))))

(defn get-or-make-reporter
  "Retrieve or create a reporter that gives the value of the function
   applied to the current value and the arguments. The keys are
   the changes that will cause recomputation."
  [keys fn manager-data & args]
  (let [application (cons fn args)]
    (or (get-in @manager-data [:application->reporter application])
        (new-reporter :manager [manager-callback manager-data keys]
                      :application application))))

(defn stop-tracking-suspended
  "Stop tracking any reporters that are suspended."
  [manager-data]
  (dotimes [n suspended-reporter-size]
    (advance-suspended-reporters-index! manager-data)))

(defn reset-manager-value!
  "Set the value of the manager to a new value, informing all reporters
  of the change."
  [manager-data new-value]
  (swap! manager-data (fn [data] (assoc data :value new-value)))
  (all-reporters-data-changed manager-data))

(defn describe-and-swap-control-return!
  "Run the function on the current value. It must return a new value, 
  a list of keys that might have been affected, and a return value.
  Update the value to the new value, update all reporters that depend
  on the keys, and return the return value."
  [manager-data update-fn]
  (let [[affected-keys result]
        (swap-control-return!
         manager-data
         (fn [data]
           (let [[new-value affected-keys result] (update-fn (:value data))]
             [(assoc data :value new-value)
              [affected-keys result]])))]
    (reporters-for-keys-data-changed manager-data affected-keys)
    (advance-suspended-reporters-index! manager-data)
    result))

(defn describe-and-swap!
  "Run the function on the current value. It must return a new value 
  and a list of keys that might have been affected. Update the value
  to the new value, and update all reporters that depend on the keys."
  [manager-data update-fn]
  (describe-and-swap-control-return!
   manager-data
   (fn [value] (let [[new-value keys] (update-fn value)]
                 [new-value keys nil]))))

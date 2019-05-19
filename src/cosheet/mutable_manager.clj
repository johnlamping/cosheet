(ns cosheet.mutable-manager
  (:require (cosheet [reporter
                      :as reporter
                      :refer [set-value! set-manager!
                              attended? new-reporter invalid]]
                     [task-queue :refer [add-tasks-with-priorities]]
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

(defn new-mutable-manager-data
  [value queue]
  (atom {
         ;; The current value.
         :value value
         
         ;; A map from key to a set of reporters that need to be
         ;; checked on any change to that key.
         :subscriptions {}

         ;; A cache so that requests for the same computation can
         ;; share a reporter. It is a map from application to an
         ;; an attended reporter with that application, if there
         ;; is any.
         :application->attended-reporter nil

         ;; The queue of work remaining to do.
         :queue queue}))

(defn mutable-manager-queue
  [manager-data]
  (:queue @manager-data))

(defn current-mutable-value
  "Return the current value of the managed item."
  [manager-data]
  (:value @manager-data))

(defn compute-reporter
  "Given the current mutable value, compute the value for the reporter."
  [value reporter]
  (let [[f & args] (:application (reporter/data reporter))]
    ;; TODO!!!: Don't do this if the reporter is invalid and has no attendees.
    (set-value! reporter (apply f value args))))

(defn recompute-reporter
  "Recompute the reporter using the latest value."
  [manager-data reporter]
  (with-latest-value [value (:value @manager-data)]
    (compute-reporter value reporter)))

(defn queue-recomputations [manager-data reporters]
  (doseq [reporter reporters]
    (reporter/set-value! reporter reporter/invalid))
  (let [queue (:queue @manager-data)]
    (add-tasks-with-priorities
       queue
       (map (fn [reporter] [(or (:priority (reporter/data reporter)) 0)
                            recompute-reporter manager-data reporter])
            reporters))))

(defn recompute-reporters-for-keys
  "Update all reporters that care about the changes represented by the keys."
  [manager-data keys]
  ;; It is OK to get the subscriptions outside of
  ;; with-latest-value, since it is sufficient to inform all the
  ;; ones that were active at some time after the value changed.
  (when (not (empty? keys))
    (let [subscriptions (:subscriptions @manager-data)
          ;; Avoid calling the same reporter twice.
          reporters (set (mapcat (partial get subscriptions) keys))]
      (queue-recomputations manager-data reporters))))

(defn recompute-all-reporters
  "Update all reporters."
  [manager-data]
  (let [;; Avoid calling the same reporter twice.
        reporters (set (apply concat (vals (:subscriptions @manager-data))))]
    (queue-recomputations manager-data reporters)))

(defn add-subscriptions
  "Given the current manager data, a reporter, and its keys,
  add subscriptions for all the keys."
  [data reporter keys]
  (update-in data [:subscriptions]
             (fn [subscriptions]
               (reduce (fn [subscriptions key]
                         (update-in subscriptions [key]
                                    #((fnil conj #{}) % reporter)))
                       subscriptions keys))))

(defn remove-subscriptions
  "Given the current manager data, a reporter, and its keys,
  add subscriptions for all the keys."
  [data reporter keys]
  (update-in data [:subscriptions]
             (fn [subscriptions]
               (reduce (fn [subscriptions key]
                         (update-in-clean-up subscriptions [key]
                                             #(disj % reporter)))
                       subscriptions keys))))

(defn manager-callback
  "Callback when a reporter starts or stops needing updates."
  [reporter manager-data keys]
  (with-latest-value [attended (attended? reporter)]
    (let [application (:application (reporter/data reporter))]
      (if attended
        (do
          (swap! manager-data
                 (fn [data]
                   (-> data
                       (add-subscriptions reporter keys)
                       (update-in [:application->attended-reporter application]
                                  ;; Due to races, we might already
                                  ;; have another reporter for this
                                  ;; expression, in which case, leave it.
                                  (fn [existing] (or existing reporter))))))
          (recompute-reporter manager-data reporter))
        (do
          (set-value! reporter invalid)
          (swap! manager-data
                 (fn [data]
                   (-> data
                       (remove-subscriptions reporter keys)
                       (update-in-clean-up
                        [:application->attended-reporter application]
                        ;; Due to races, we might already have
                        ;; another reporter for this expression, in
                        ;; which case, leave it.
                        (fn [existing]
                          (if (= existing reporter) nil existing)))))))))))

(defn get-or-make-reporter
  "Retrieve or create a reporter that gives the value of the function
   applied to the current value and the arguments. The keys are
   the changes that will cause recomputation."
  [keys fn manager-data & args]
  (let [application (cons fn args)]
    (or (get-in @manager-data [:application->attended-reporter application])
        (new-reporter :manager [manager-callback manager-data keys]
                      :application application))))

(defn reset-manager!
  "Set the value of the manager to a new value, informing all reporters
  of the change."
  [manager-data new-value]
  (swap! manager-data (fn [data] (assoc data :value new-value)))
  (recompute-all-reporters manager-data))

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
    (recompute-reporters-for-keys manager-data affected-keys)
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

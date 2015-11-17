(ns cosheet.mutable-manager
  (:require (cosheet [reporters :as reporter
                      :refer [set-value! set-manager!
                              attended? new-reporter invalid]]
                     [utils :refer [with-latest-value
                                    update-in-clean-up
                                    swap-control-return!]])))

;;; This is code that knows how to recompute reporters that depend on
;;; a function of a value that can change. Each function is associated
;;; with a list of keys that it depends on, and each update to the
;;; value is associated with a list of keys that the may have changed.
;;; When the atom is updated, all possibly affected functions are
;;; re-evaluated.

;;; It sets :application in reporters that it manages to be the
;;; function that should be called, together with any additional
;;; arguments beyond the value

(defn new-mutable-management [value]
  (atom {
         ;; The current value
         :value value
         
         ;; A map from key to a set of reporters that need to be
         ;; checked on any change to that key.
         :subscriptions {}

         ;; A cache so that requests for the same computation can
         ;; share a reporter. It is a map from application to an
         ;; a set of attended reporters with that application, if there
         ;; are any.
         :application->attended-reporter nil}))

(defn current-mutable-value
  "Return the current value of the managed item."
  [management]
  (:value @management))

(defn compute-reporter
  "Given the value, compute the value for the reporter."
  [value reporter]
  (let [[f & args] (:application (reporter/data reporter))]
    (set-value! reporter (apply f value args))))

(defn recompute-reporter
  "Recompute the reporter using the latest value."
  [management reporter]
  (with-latest-value [value (:value @management)]
    (compute-reporter value reporter)))

(defn recompute-reporters-for-keys
  "Update all reporters that care about the changes represented by the keys."
  [management keys]
  ;; It is OK to get the subscriptions outside of
  ;; with-latest-value, since it is sufficient to inform all the
  ;; ones that were active at some time after the value changed.
  (when (not (empty? keys))
    (let [subscriptions (:subscriptions @management)
          ;; Avoid calling the same reporter twice.
          reporters (set (mapcat (partial get subscriptions) keys))]
      (with-latest-value [value (:value @management)]
        (doseq [reporter reporters]
          (compute-reporter value reporter))))))

(defn add-subscriptions
  "Given the management data, a reporter, and its keys,
  add subscriptions for all the keys."
  [data reporter keys]
  (update-in data [:subscriptions]
             (fn [subscriptions]
               (reduce (fn [subscriptions key]
                         (update-in subscriptions [key]
                                    #((fnil conj #{}) % reporter)))
                       subscriptions keys))))

(defn remove-subscriptions
  "Given the management data, a reporter, and its keys,
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
  [reporter management keys]
  (with-latest-value [attended (attended? reporter)]
    (let [application (:application (reporter/data reporter))]
      (if attended
        (do
          (swap! management
                 (fn [data]
                   (-> data
                       (add-subscriptions reporter keys)
                       (update-in [:application->attended-reporter application]
                                  ;; Due to races, we might already
                                  ;; have another reporter for this
                                  ;; expression, in which case, leave it.
                                  (fn [existing] (or existing reporter))))))
          (recompute-reporter management reporter))
        (do
          (set-value! reporter invalid)
          (swap! management
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
  [keys fn management & args]
  (let [application (cons fn args)]
    (or (get-in @management [:application->attended-reporter application])
        (new-reporter :manager [manager-callback management keys]
                      :application application))))

(defn describe-and-swap-control-return!
  "Run the function on the current value. It must return a new value, 
  a list of keys that might have been affected, and a return value.
  Update the value to the new value, update all reporters that depend
  on the keys, and return the return value."
  [management update-fn]
  (let [[affected-keys result]
        (swap-control-return!
         management
         (fn [data]
           (let [[new-value affected-keys result] (update-fn (:value data))]
             [(assoc data :value new-value)
              [affected-keys result]])))]
    (recompute-reporters-for-keys management affected-keys)
    result))

(defn describe-and-swap!
  "Run the function on the current value. It must return a new value 
  and a list of keys that might have been affected. Update the value
  to the new value, and update all reporters that depend on the keys."
  [management update-fn]
  (describe-and-swap-control-return!
   management
   (fn [value] (let [[new-value keys] (update-fn value)]
                 [new-value keys nil]))))

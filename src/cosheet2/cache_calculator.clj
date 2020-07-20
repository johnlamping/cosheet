(ns cosheet2.cache-calculator
  (:require (cosheet2 [reporter :refer [reporter? attended? new-reporter
                                        reporter-data data-attended?
                                        invalid]]
                      [mutable-map :as mm]
                      [calculator :refer [propagate-calculator-data!
                                          update-new-further-action
                                          register-for-value-source
                                          copy-value-callback
                                          modify-and-act!]]
                      [utils :refer [with-latest-value
                                     assoc-if-non-empty]]
                      [application-calculator
                       :refer [application-calculator]])))

;;; Calculator for a reporter that forwards to another reporter,
;;; stored in a cache, with the same application. It creates one if
;;; needed.  This avoids redoing shared, possibly expensive,
;;; computations.

(defn canonicalize-reporter
  "If the argument is a reporter, chase :value-source if that is canonical"
  [reporter]
  (if (reporter? reporter)
    (let [data (reporter-data reporter)]
      (if (:value-source-is-canonical data)
        (canonicalize-reporter (:value-source data))
        reporter))
    reporter))

(defn canonicalize-application
  "canonicalize any reporters in the application."
  [application]
  (map canonicalize-reporter application))

(defn get-or-make-reporter
  "Try to find an application reporter for the given application in the cache.
   If there isn't one, make one and propagate the calculator data to it."
  [application original-name cd]
  (when (= (second application) 41))
  (or (mm/mm-get (:cache cd) (canonicalize-application application))
      (let [reporter (apply new-reporter
                              :application application
                              :calculator application-calculator
                              (when original-name
                                [:name ["cached" original-name]]))]
        (when (= (second application) 41))
        (propagate-calculator-data! reporter cd)
        reporter)))

(defn adjust-cache-membership
  "Make sure the reporter is in the cache if and only if it is attended to.
   (Make an exception if there is another reporter already in the cache
   with the same key.)"
  [reporter cd]
  (let [application (canonicalize-application
                     (:application (reporter-data reporter)))]
    (with-latest-value [attended (attended? reporter)]
      (mm/update-in-clean-up!
       (:cache cd) [application]
       (fn [current]
         ;; If there is already a different reporter, leave it.
         (if (when current (not= current reporter))
           current
           (when attended reporter)))))))

(defn update-value-source
  "Given the data from a reporter, and the reporter, set the value-source
   to the given source, and request the appropriate registrations."
  [data reporter source cd]
  ;; We must only set to non-nil if there are attendees for our value,
  ;; otherwise, we will create demand when we have none ourselves.
  (assert (or (nil? source) (data-attended? data)))
  (let [original-source (:value-source data)]
    (if (= source original-source)
      data
      (let [s (or source original-source)]
        ;; We must be either going from no source to source, or vice versa.
        (assert (not (and source original-source)))
        (-> data
            (assoc-if-non-empty :value-source source)
            ;; We have to adjust the registration before we determine whether
            ;; it belongs in the cache.
            (update-new-further-action
             register-for-value-source reporter s copy-value-callback cd)
            (update-new-further-action adjust-cache-membership s cd))))))

(defn cache-calculator
  "Calculator that looks up the value of a reporter's application in a
   cache of reporters."
  [reporter cd]
  (let [data (reporter-data reporter)
        application (:application data)
        cache (:cache cd)]
    (modify-and-act!
     reporter
     (fn [data]
       (let [source (when (data-attended? data)
                      (or (:value-source data)
                          (get-or-make-reporter application (:name data) cd)))]
         (cond-> (-> data
                     (assoc :value-source-priority-delta 1
                            :value-source-is-canonical (not (nil? source)))
                     (update-value-source reporter source cd))
           (nil? source)
           (assoc :value invalid)))))))

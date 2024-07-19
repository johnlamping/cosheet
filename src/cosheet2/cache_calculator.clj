(ns cosheet2.cache-calculator
  (:require (cosheet2 [reporter :refer [reporter? attended? new-reporter
                                        reporter-data data-attended?
                                        invalid]]
                      [mutable-map :as mm]
                      [calculator :refer [propagate-calculator-data!
                                          register-for-value-source
                                          copy-value-callback
                                          modify-and-act!]]
                      [utils :refer [with-latest-value
                                     update-new-further-action
                                     assoc-if-non-empty]]
                      [application-calculator
                       :refer [application-calculator]])))

;;; Calculator for a forwarding reporter. These have an application,
;;; but forward to another reporter with the same application, which
;;; can be shared among several forwarding reporters with the same
;;; application.  The shared reporter does the work once, while the
;;; forwarding reporters keep track of their respective attendees.
;;; This avoids redoing shared computations that might be expensive.

;;; A cache keeps track of all the application reporters that are
;;; referenced by forwarding reporters.  When a forwarding reporter is
;;; created, and no reporter for its application is already in the
;;; cache, one is made and put in the cache.

;;; The :value-source of a forwarding reporter can change over time,
;;; because its application reporter needs to be removed from the
;;; cache when there is no longer any demand for it, to free up the
;;; memory. But later, a new one with the same application might be
;;; created when a different forwarding reporter asks for it. Other
;;; forwarding reporters need to be able to find that new one. So they
;;; clear out their :value-source when they lose demand, and then look
;;; anew when their demand reappears.

;;; A forwarding reporter's data has this additional field.
;;;    :cache-key  The key to use to do the cache look up for the
;;;                reporter to forward to.  Normally, this is the
;;;                application.  But if the application contains
;;;                forwarding reporters, they are replaced by their
;;;                key.  That way, applications will match if they
;;;                differ only by forwarding reporters with the same
;;;                application.

;;; Why can't we get rid of forwarding reporters, and just make the
;;; calls that want cached applications call get-or-make-reporter?
;;; The problem is how to remove unused reporters from the
;;; cache. Clojure doesn't have support for weak maps, so we can't
;;; rely on GC to do it. (We could use Java's weak maps, but we want
;;; to be agnostic about the underlying language.) Since we are aware
;;; of changes to demand, we could not add a reporter to the cache
;;; until it gets demand, and remove it when it has no more
;;; demand. But since reporters get created with no demand, we could
;;; end up with several reporters, all with the same application, but
;;; none with demand yet. So they wouldn't be in the cache, and they
;;; wouldn't share computation.

;;; Forwarding reporters get around this problem by addinmg a layer of
;;; indirection, so we can have it both ways. The cache keeps track of
;;; only active computations. And a forwarding reporter looks in the
;;; cache when demand changes, seeing if there is currently a reporter
;;; already calculating its value, and redirects its value to there.

(defn- cache-key
  "Return the cache key for an application."
  [application]
  (vec (map #(if (reporter? %)
               (or (:cache-key (reporter-data %))
                   %)
               %)
            application)))

(defn get-or-make-reporter
  "Try to find an application reporter in the cache for the given
  forwarding reporter.  If there isn't one, make one and propagate the
  calculator data to it."
  [data cd]
  (or (mm/mm-get (:cache cd) (:cache-key data))
      (let [reporter (apply new-reporter
                            :application (:application data)
                            :calculator application-calculator
                            (when-let [original-name (:name data)]
                              [:name ["cached" original-name]]))]
        (propagate-calculator-data! reporter cd)
        reporter)))

(defn- adjust-cache-membership
  "Make sure the reporter is in the cache if and only if it is attended to.
   (Except, if there is another reporter already in the cache
   with the same key, throw this one out.)"
  [reporter key cd]
  (with-latest-value [attended (attended? reporter)]
    (mm/update-in-clean-up!
     (:cache cd) [key]
     (fn [current]
       ;; If there is already a different reporter, leave it.
       (if (when current (not= current reporter))
         current
         (when attended reporter))))))

(defn- update-value-source
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
            ;; We have to adjust our source's registration before we
            ;; determine whether it belongs in the cache.
            (update-new-further-action
             register-for-value-source reporter s copy-value-callback cd)
            (update-new-further-action
             adjust-cache-membership s (:cache-key data) cd))))))

(defn cache-calculator
  "Calculator that looks up the value of a reporter's application in a
   cache of reporters."
  [reporter cd]
  (let [data (reporter-data reporter)
        cache (:cache cd)]
    (modify-and-act!
     reporter
     (fn [data]
       (let [source (when (data-attended? data)
                      (or (:value-source data)
                          (get-or-make-reporter data cd)))]
         (cond-> (update-value-source data reporter source cd)
           (nil? source)
           (assoc :value invalid)))))))

(defn data-for-forwarding-reporter
  "Given an application for a forwarding reporter, return a list of
   keywords and values map of the properties that make a reporter with
   that application a forwarding reporter."
  [application]
  [:value-source-priority-delta 1
   :cache-key (cache-key application)
   :calculator cache-calculator])

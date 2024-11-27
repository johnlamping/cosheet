(ns cosheet2.category-change-calculator
  (:require (cosheet2 [reporter :refer [reporter-data data-attended?
                                        set-attendee-and-call!
                                        reporter? invalid]]
                      [calculator :refer [modify-and-act!
                                          copy-value-callback]]
                      [utils :refer [update-new-further-action]])))

;;; Manage a reporter that forwards requests to another reporter,
;;; while changing the categories of interest. That means that the
;;; value of this reporter won't necessarily agree with the value of
;;; the reporter it forwards to, since this reporter will only be
;;; updated for changes to the forwarded reporter that affect the
;;; specified categories. Put another way, the value of the category
;;; change reporter will be equal to the value of the reporter it
;;; forwards to only with respect to the categories of interest.

;;; The primary use of a category change reporter is when an
;;; application uses only part of the value of some reporter. By
;;; wrapping that reporter in a category change reporter, the
;;; application will only be informed of changes to the part it
;;; depends on. For example, an application reporter that does a query
;;; over a database only cares about all changes to the database that
;;; might affect the result of its query. By interposing this
;;; calculator when building the application reporter, only the
;;; relevant demand can get passed down, so only changes that could
;;; affect the result of the query will get notified upwards.

;;; This reporter's data must include
;;;    :value-source  The reporter it forwards to
;;;      :categories  The categories of demand it should pass down.
;;; Those may not change once the category change reporter is created.

(defn category-change-calculator
  "Calculator that changes the categories of requests."
  [reporter cd]
  (let [data (reporter-data reporter)]
    (modify-and-act!
     reporter
     (fn [data]
       (let [source (:value-source data)
             attended (data-attended? data)]
         (assert (reporter? source))
         (cond-> (-> data
                     (assoc :value-source-priority-delta 1)
                     (update-new-further-action
                      set-attendee-and-call!
                      source
                      (list :copy-value reporter)
                      (+ 1 (:priority data))
                      (:categories data)
                      (when attended copy-value-callback)))
           (not attended)
           (assoc :value invalid)))))))

(ns cosheet2.category-change-calculator
  (:require (cosheet2 [reporter :refer [reporter-data data-attended?
                                        set-attendee-and-call!
                                        reporter? invalid]]
                      [calculator :refer [modify-and-act!
                                          copy-value-callback]]
                      [utils :refer [update-new-further-action]])))

;;; Manage a reporter that forwards requests to another reporter,
;;; while changing the categories of interest.
;;; This reporters data must include
;;;    :value-source  The reporter it forwards to
;;;      :catagories  The categories of demand it should pass down.
;;; Those may not change once the forwarding reporter is created.

;;; This is used to wrap a reporter when an application uses only part
;;; of its value, so that the application will only be informed of
;;; changes to that part. For example, an application reporter that
;;; does a query over a database only cares about all changes to the
;;; database that might affect the result. But the application
;;; reporter code propagates demand for all changes to its arguments,
;;; including the datavase. By interposing this calculator when
;;; building the application reporter, only the relevant demand gets
;;; passed down.

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
                     (assoc :value-source-priority-delta 1
                            :value-source-is-canonical true)
                     (update-new-further-action
                      set-attendee-and-call!
                      source
                      (list :copy-value reporter)
                      (+ 1 (:priority data))
                      (:categories data)
                      (when attended copy-value-callback)))
           (not attended)
           (assoc :value invalid)))))))

(ns cosheet2.category-change-calculator
  (:require (cosheet2 [reporter :refer [reporter-data data-attended?
                                        set-attendee-and-call!
                                        reporter? invalid]]
                      [calculator :refer [modify-and-act!
                                          update-new-further-action
                                          copy-value-callback]])))

;;; Manage a reporter that forwards requests to another reporter, while
;;; changing the categories of interest.
;;; This is to wrap a reporter when an expression uses only part of its
;;; value. The reporter it forwards to, and its categories of interest, are
;;; fixed when the reporter is created.

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

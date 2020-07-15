(ns cosheet2.category-change-calculator
  (:require (cosheet2 [reporter :refer [reporter-data data-attended?
                                        set-attendee-and-call!
                                        reporter?]]
                      [calculator :refer [modify-and-act
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
    (modify-and-act
     reporter
     (fn [data]
       (let [source (:value-source data)]
         (assert (reporter? source))
         (-> data
             (assoc :value-source-priority-delta 1)
             (update-new-further-action
              set-attendee-and-call!
              source
              (list :copy-value reporter)
              (+ 1 (:priority data))
              (:categories data)
              (when (data-attended? data)
                copy-value-callback))))))))

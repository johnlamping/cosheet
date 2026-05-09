(ns cosheet.category-change-calculator
  (:require (cosheet [reporter :refer [reporter-data data-attended?
                                        set-attendee-and-call! remove-attendee!
                                        validity-category make-reporter
                                        universal-category reporter?]]
                      [calculator :refer [modify-and-act!
                                          copy-value-callback
                                          update-to-invalid]]
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
     (fn [{:keys [value-source priority categories] :as data}]
       (assert (reporter? value-source))
       (assert (seq categories))
       (let [attended (data-attended? data)
             callback-key (list :copy-value reporter)]
         (cond-> (assoc data :value-source-priority-delta 1)
           attended
           (update-new-further-action
            set-attendee-and-call!
            value-source
            callback-key
            (+ 1 priority)
            (conj categories validity-category)
            copy-value-callback)
           (not attended)
           (update-to-invalid)
           (not attended)
           (update-new-further-action
            remove-attendee! value-source callback-key)))))))

(defn category-change-R
  "Takes a set of categories and a reporter and returns a reporter that
  tracks the input reporter's value, but only when it has a change in
  any of the given categories; the tracking reporter is only
  guaranteed to be up to date as of the last such change."
  [categories reporter]
  (assert (reporter? reporter))
  (if (or (nil? categories)
          (= categories [universal-category]))
    reporter ; The categories don't make a difference.
    (make-reporter
     :value-source reporter
     :categories categories
     :calculator category-change-calculator)))

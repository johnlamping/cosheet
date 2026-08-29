(ns cosheet.query-calculator
  (:require (cosheet [reporter :refer [reporter-data reporter-latest-value
                                        data-value-or-invalid
                                        reporter-valid?
                                        set-attendee-and-call!
                                        remove-attendee!
                                        inform-attendees
                                        data-attended?
                                        make-reporter]]
                      [store :refer (mutable-store?)]
                      [entity :refer [id->entity]]
                      entity-impl
                      [query :refer [matching-items matching-extensions]]
                      query-impl
                      [calculator :refer [modify-and-act! update-to-invalid
                                          update-value update-to-invalid]]
                      [task-queue :refer [add-task-with-priority]]
                      [utils :refer [with-latest-value
                                     update-new-further-action]])))

;;; query-calculator calculates the (re)computation of a reporter
;;; whose value is a seq of the ids of a matching-items request
;;; against a store.

;;; When the reporter's value changes to a valid value, the change's
;;; description and its changed categories are either both all the ids
;;; or all ids that either got added or removed since the last valid
;;; value.

;;; This manager adds following fields to the reporter:
;;;                :term The term whose matches we report.
;;;               :store The mutable store to run the term against.
;;;   :ids-to-reevaluate The store ids whose items may have changed since
;;;                      our value was last valid. If this is nil,
;;;                      anything may have changed since then.

(defn store-change
  "The function to call asynchronously after the store has changed."
  [reporter store]
  (with-latest-value
    [immutable (reporter-latest-value store)]
    (modify-and-act!
     reporter
     (fn [data]
       (let [{:keys [term value ids-to-reevaluate]} data
             value-or-invalid (data-value-or-invalid data)
             [new-value changed-ids]
             (cond
               (= #{} ids-to-reevaluate)
               [value #{}]
               (and (reporter-valid? value) ids-to-reevaluate)
               ;; We have an old value, and we know how the store changed.
               ;; we can do an update, rather than a whole re-query.
               (reduce (fn [[new-value changed-ids] id]
                         (let [immutable-entity
                               (id->entity id immutable)
                               matches
                               (not-empty (matching-extensions
                                           term immutable-entity))
                               currently-in (contains? new-value id)]
                           (cond (and matches (not currently-in))
                                 [(conj new-value id) (conj changed-ids id)]
                                 (and (not matches) currently-in)
                                 [(disj new-value id) (conj changed-ids id)]
                                 true
                                 [new-value changed-ids])))
                       [value #{}]
                       (seq ids-to-reevaluate))
               true
               (let [items (matching-items term immutable)]
                 [(set (map :item-id items))
                  ;; Here, nil means we don't know what changed.
                  nil]))]
         (if (= new-value value-or-invalid)
           data
           (cond-> (-> data
                       (update-new-further-action
                        inform-attendees reporter changed-ids changed-ids)
                       (update-value new-value))             
             (reporter-valid? new-value)
             (assoc :ids-to-reevaluate #{}))))))))

(defn store-change-callback
  "The function we ask the store to call when its value changes."
  [& {reporter :key store :reporter categories :categories}]
  (let [data (reporter-data reporter)
        cd (:calculator-data data)]
    (modify-and-act!
     reporter
     (fn [data]
        (-> data
           (update-to-invalid)
           (update :ids-to-reevaluate
                   (fn [old] (when (and (not (nil? old))
                                        (not (nil? categories)))
                               (disj (clojure.set/union old categories)
                                     :validity-category))))
           (update-new-further-action inform-attendees reporter #{} #{})
           (update-new-further-action
            add-task-with-priority
            (:queue cd) (:priority data)
            store-change reporter store))))))

(defn query-calculator
  [reporter cd]
  (modify-and-act!
   reporter
   (fn [data]
     (let [store (:store data)]
       (if (data-attended? data)
         (-> data
             (assoc :dependent-depth 1) 
             (update-new-further-action
              set-attendee-and-call!
              store reporter (+ (:priority data) 1) store-change-callback))
         (-> data
             (update-to-invalid)
             ;; The following line isn't strictly necessary, since
             ;; the first call we get when we are attended to again will
             ;; say that the changes aren't known. But this makes the
             ;; data correct.
             (assoc :ids-to-reevaluate nil)
             (update-new-further-action remove-attendee! store reporter)))))))

(defn matching-item-ids-R
  "Return a reporter whose value will track the set of item ids in the
  store that match the term."
  [term store]
  (if (mutable-store? store)
    (make-reporter :calculator query-calculator
                  :term term
                  :store store)
    (set (map :item-id (matching-items term store)))))

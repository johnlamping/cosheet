(ns cosheet2.query-calculator
  (:require (cosheet2 [reporter :refer [reporter-data reporter-value
                                        invalid valid?
                                        set-attendee-and-call!
                                        remove-attendee!
                                        inform-attendees
                                        data-attended?
                                        new-reporter]]
                      [entity :refer [description->entity in-different-store
                                      to-list]]
                      entity-impl
                      [query :refer [matching-items matching-extensions]]
                      query-impl
                      [calculator :refer [modify-and-act!
                                          update-new-further-action]]
                      [task-queue :refer [add-task-with-priority]]
                      [utils :refer [with-latest-value]])))

;;; Manage the (re)computation of a query against a store.  The value
;;; of its reporter is a set of ids whose items satisfy the query.
;;; The query must be a term. Both change description and changed
;;; categories are the set ids that got added or removed.

;;; This manager adds following fields to the reporter:
;;;               :query The query whose results we report.
;;;               :store The mutable store to run the query against.
;;;    :last-valid-value The last valid value we had.
;;;   :ids-to-reevaluate The store ids whose items may have changed since
;;;                      last-valid-value. If this is nil, we don't know
;;;                      what may have changed.

(defn store-change
  [reporter store]
  (with-latest-value
    [immutable (reporter-value store)]
    (modify-and-act!
     reporter
     (fn [data]
       (let [{:keys [value query last-valid-value ids-to-reevaluate]}
             data
             [new-value changed-ids]
             (cond
               (not (valid? immutable))
               [invalid #{}]
               (= #{} ids-to-reevaluate)
               [value #{}]
               (and (valid? last-valid-value) ids-to-reevaluate)
               ;; We have an old value, and we know how the store changed.
               ;; we can do an update, rather than a whole re-query.
               (reduce (fn [[value changed-ids] id]
                         (let [immutable-entity
                               (description->entity id immutable)
                               matches
                               (not-empty (matching-extensions
                                           query immutable-entity))
                               currently-in (contains? value id)]
                           (cond (and matches (not currently-in))
                                 [(conj value id) (conj changed-ids id)]
                                 (and (not matches) currently-in)
                                 [(disj value id) (conj changed-ids id)]
                                 true
                                 [value changed-ids])))
                       [last-valid-value #{}] (seq ids-to-reevaluate))
               true
               (let [items (matching-items query immutable)]
                 [(set (map :item-id items))
                  nil]))]
         (if
           (= new-value value)
           data
           (cond-> (-> data
                       (assoc :value new-value)
                       (update-new-further-action
                        inform-attendees reporter changed-ids changed-ids))
             (valid? new-value)
             (assoc :last-valid-value new-value
                    :ids-to-reevaluate #{}))))))))

(defn store-change-callback
  [& {reporter :key store :reporter categories :categories}]
  (let [data (reporter-data reporter)
        cd (:calculator-data data)]
    (modify-and-act!
     reporter
     (fn [data]
        (-> data
           (assoc :value invalid)
           (update :ids-to-reevaluate
                   (fn [old] (when (and (not (nil? old))
                                        (not (nil? categories)))
                               (clojure.set/union old categories))))
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
             (assoc :value :invalid)
             (update-new-further-action remove-attendee! store reporter)))))))

(defn query-matches-mutable
  [query mutable-store]
  (new-reporter :calculator query-calculator
                :query query
                :store mutable-store))

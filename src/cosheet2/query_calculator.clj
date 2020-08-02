(ns cosheet2.query-calculator
  (:require (cosheet2 [reporter :refer [reporter-data reporter-value
                                        valid?
                                        set-attendee-and-call!
                                        remove-attendee!
                                        inform-attendees
                                        data-attended?]]
                      [entity :refer [description->entity in-different-store]]
                      [query :refer [matching-items matching-extensions]]
                      [calculator :refer [modify-and-act!
                                          update-new-further-action]]
                      [task-queue :refer [add-task-with-priority]]
                      [utils :refer [with-latest-value]])))


;;; Manage the (re)computation of a query against a store.  The value
;;; of its reporter is a map from id to mutable entities with that id
;;; that satisfy the query.
;;; Both ids change description and changed categories are the ids that
;;; got added or removed.

;;; This manager adds following fields to the reporter:
;;;               :query The query whose results we report.
;;;               :store The mutable store to run the query against.
;;;    :last-valid-value The last valid value we had.
;;;          :last-store The immutable store that was used to compute
;;;                      :last-valid-value. This allows us to not
;;;                      rerun the query if we are re-attended to and
;;;                      the store hasn't changed.


(defn store-change-callback
  [& {reporter :key store :reporter description :description}]
  (with-latest-value
    [immutable (reporter-value store)]
    (modify-and-act!
     reporter
     (fn [data]
       (let [{:keys [value query last-valid-value last-store]} data
             data (assoc data :last-store immutable)
             [new-value changed-ids]
             (cond
               (= immutable last-store)
               [value []]
               (and (valid? data) description)
               ;; We have an old value, and we know how the store changed.
               ;; we can do an update, rather than a whole re-query.
               (reduce (fn [[value changed-ids] id]
                         (let [immutable-entity
                               (description->entity id immutable)
                               matches
                               (not-empty (matching-extensions
                                           immutable-entity query {}))]
                           [(if (= matches (contains? value id))
                              [value changed-ids]
                              [(if matches
                                 (assoc value id
                                        (description->entity id store))
                                 (dissoc value id))
                               (concat changed-ids id)])]))
                       value [])
               true
               (let [immutable-items (matching-items query immutable)]
                 [(zipmap (map :item-id immutable-items)
                          (map #(in-different-store immutable-items store)
                               immutable-items))
                  nil]))]
         (if (= new-value value)
           data
           (-> data
               (assoc :value new-value
                      :last-valid-value new-value
                      :last-store immutable)
               (update-new-further-action
                inform-attendees reporter changed-ids changed-ids))))))))

(defn do-query-calculate
  [reporter cd]
  (modify-and-act!
   reporter
   (fn [data]
     (let [store (:store data)]
       (if (data-attended? data)
         (-> data
             assoc :dependent-depth 1 
             (update-new-further-action
              set-attendee-and-call!
              store reporter (+ (:priority data) 1) store-change-callback))
         (-> data
             (assoc :value :invalid)
             (update-new-further-action remove-attendee! store reporter)))))))

(defn query-calculator
  [reporter cd]
  (add-task-with-priority
   (:queue cd) (:priority (reporter-data reporter))
   do-query-calculate reporter cd))

(ns cosheet2.query-calculator
  (:require (cosheet2 [reporter :refer [reporter-data reporter-value
                                        invalid valid?
                                        set-attendee-and-call!
                                        remove-attendee!
                                        inform-attendees
                                        data-attended?
                                        new-reporter]]
                      [store :refer (mutable-store?)]
                      [entity :refer [description->entity in-different-store
                                      to-list]]
                      entity-impl
                      [query :refer [matching-items matching-extensions]]
                      query-impl
                      [calculator :refer [modify-and-act!]]
                      [task-queue :refer [add-task-with-priority]]
                      [utils :refer [with-latest-value
                                     update-new-further-action]])))

;;; query-calculator calculates the (re)computation of a term against
;;; a store for a reporter. It makes the value of its reporter be the
;;; set of ids whose items satisfy the term.

;;; When the reporter's value changes to a valid value, the change's
;;; description and its changed categories are both the set all ids
;;; that either got added or removed since the last valid value.

;;; This manager adds following fields to the reporter:
;;;                :term The term whose matches we report.
;;;               :store The mutable store to run the term against.
;;;    :last-valid-value The last valid value we had.
;;;   :ids-to-reevaluate The store ids whose items may have changed since
;;;                      last-valid-value was valid. If this is nil,
;;;                      anything may have changed since then.

(defn store-change
  "The function to call asynchronously after the store has changed."
  [reporter store]
  (with-latest-value
    [immutable (reporter-value store)]
    (modify-and-act!
     reporter
     (fn [data]
       (let [{:keys [value term last-valid-value ids-to-reevaluate]}
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
                                           term immutable-entity))
                               currently-in (contains? value id)]
                           (cond (and matches (not currently-in))
                                 [(conj value id) (conj changed-ids id)]
                                 (and (not matches) currently-in)
                                 [(disj value id) (conj changed-ids id)]
                                 true
                                 [value changed-ids])))
                       [last-valid-value #{}] (seq ids-to-reevaluate))
               true
               (let [items (matching-items term immutable)]
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
  "The function we ask the store to call when its value changes."
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
    (new-reporter :calculator query-calculator
                  :term term
                  :store store)
    (set (map :item-id (matching-items term store)))))

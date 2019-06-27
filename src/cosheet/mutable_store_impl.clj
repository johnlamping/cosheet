(ns cosheet.mutable-store-impl
  (:require (cosheet [store :refer :all]
                     [mutable-manager
                      :refer [new-mutable-manager-data
                              mutable-manager-queue
                              current-mutable-value
                              get-or-make-reporter reset-manager!
                              describe-and-swap!
                              describe-and-swap-control-return!]]
                     [reporter
                      :refer [set-value! set-manager!]]
                     [utils :refer [call-with-latest-value
                                    update-in-clean-up
                                    swap-control-return!]]
                     [debug :refer [simplify-for-print]])))

(defn store-to-manager-data
  "Given an immutable store, create the map that we store in the manager data,
  which adds a history to the store."
  [immutable-store]
  (let [tracking-store (track-modified-ids immutable-store)]
    {:store tracking-store
     ;; The next two fields each are a list of [modified-ids, store] pairs
     ;; where store gives one store in a sequence of stores, and
     ;; modified-ids gives the ids that change between that store and
     ;; the previous one in the sequence.
     
     ;; :history is a list of [modified-ids, store] pairs going backward
     ;; in time.
     :history nil
     ;; :future is a list of [modified-ids, store] pairs going forward
     ;; in time, starting from the next one after the current store.
     :future nil
     }))

(defn item-ids-affected-by-id
  "Return a seq of item ids that might be affected by a change to the given id."
  [id old-store new-store]
  (loop [item (when id (non-implicit-id id))
         affected nil]
    (if (not (nil? item))
      (recur (or (id->subject new-store item) (id->subject old-store item))
             (conj affected item))
      affected)))

(defn keys-affected-by-ids
  "Return a seq of keys that might be affected by the set of modified ids.
   The set must be non-empty."
  [modified-ids old-store new-store]
  (reduce (fn [accum id]
            (into accum (item-ids-affected-by-id id old-store new-store)))
          #{nil} ; We have to always inform reporters keyed by nil.
          modified-ids))

(defn apply-to-store
  "Return the result of the operation on the store
  of the state. Using this lets callers avoid creating thunks as
  arguments to get-or-make-reporter, which would break caching."
  [state operation & args]
  (apply operation (:store state) args))

(defn get-or-make-reporter-adjusting-ids
  "This does a get-or-make-reporter,
   after making any implicit ids non-implicit."
  [ids & args]
  (apply get-or-make-reporter (map #(when % (non-implicit-id %)) ids) args))

(defn add-to-modified-ids-in-sequence
  "Given a history or future sequence, add the specified modified ids to
   the modified ids for the top item."
  [history modified-ids]
  (when (seq history)
    (let [[[top-modified top-store] & remainder]
          history]
      (cons
       [(distinct (concat top-modified modified-ids))
        top-store]
       remainder))))

(defrecord MutableStoreImpl
    ^{:doc
      "A store that contains an immutable store,
       supports mutation to that store, handles undo,
       and returns reporter objects for queries."}
  [manager-data ;; A mutable-manager manager-data holding the immutable
                ;; store and a history as its value. 
   ]

  ElementStore

  (id->subject [this id]
    (id->subject (:store (current-mutable-value (:manager-data this))) id))

  Store

  (id-valid? [this id]
    (get-or-make-reporter-adjusting-ids
     [id] apply-to-store (:manager-data this) id-valid? id))

  (id-label->element-ids [this id label]
    (get-or-make-reporter-adjusting-ids
     [id] apply-to-store (:manager-data this) id-label->element-ids id label))

  (id->element-ids [this id]
    (get-or-make-reporter-adjusting-ids
     [id] apply-to-store (:manager-data this) id->element-ids id))

  (id->content [this id]
    (let [base-id (when id (non-implicit-id id))]
      (get-or-make-reporter-adjusting-ids
       [base-id] apply-to-store (:manager-data this) id->content id)))

  (id->content-reference [this id]
    (get-or-make-reporter-adjusting-ids
     [id] apply-to-store (:manager-data this) id->content-reference id))

  (candidate-matching-ids [this template]
    (get-or-make-reporter-adjusting-ids
     [nil] apply-to-store (:manager-data this) candidate-matching-ids template))

  (mutable-store? [this] true)
  
  MutableStore

  (current-store [this] (:store (:value @(:manager-data this))))

  (store-queue [this] (mutable-manager-queue (:manager-data this)))

  (call-dependent-on-id [this id fun]
    (get-or-make-reporter-adjusting-ids
     [id] apply-to-store (:manager-data this) fun))

  (reset-store! [this new-store]
    (reset-manager! (:manager-data this) (store-to-manager-data new-store)))

  (do-update! [this update-fn]
    (do-update-control-return! this (fn [store] [(update-fn store) nil])))

  (do-update-control-return! [this update-fn]
    (describe-and-swap-control-return!
     (:manager-data this)
     (fn [{:keys [store history future] :as state}]
       (let [[updated-store result] (update-fn store)
             [new-store modified-ids] (fetch-and-clear-modified-ids
                                       updated-store)]
         [(if (not= new-store store)
            (if (seq modified-ids)
              (if (equivalent-undo-point? new-store)
                (if (equivalent-undo-point? store)
                  ;; The new store and the current one are both
                  ;; equivalent to the top one in the history. This
                  ;; means that undo will never go to the current
                  ;; store, so we don't need to push it onto the
                  ;; history, and we don't need to wipe out the future.
                  ;; But we do have to update the modified ids to be relative
                  ;; to the new store.
                  {:store new-store
                   :history (add-to-modified-ids-in-sequence
                             history modified-ids)
                   :future (add-to-modified-ids-in-sequence
                            future modified-ids)}
                  ;; The new store is equivalent to the current one.
                  ;; We still need to push it, as an undo followed by redo
                  ;; should land us there.
                  ;; We don't have to wipe out the future, but we do need
                  ;; to update its modified ids.
                  {:store new-store
                   :history (cons [modified-ids store] history)
                   :future (add-to-modified-ids-in-sequence
                            future modified-ids)})
                {:store new-store
                 :history (cons [modified-ids store] history)
                 :future nil})
              ;; Only some ancilliary information unrelated to the content
              ;; changed.
              (assoc state :store new-store))
            state)
          (keys-affected-by-ids modified-ids store new-store)
          result]))))

  (add-simple-element! [this subject content]
    (do-update-control-return! this #(add-simple-element % subject content)))

  (remove-simple-id! [this id]
    (do-update! this #(remove-simple-id % id)))

  (update-content! [this id content]
    (do-update! this #(update-content % id content)))

  (declare-temporary-id! [this id]
    (do-update! this #(declare-temporary-id % id)))

  (can-undo? [this]
    (let [{:keys [store history]} (:value @(:manager-data this))]
      (and (not (empty? history ))
           (or (not (equivalent-undo-point? store))
               (not (empty? (rest history)))))))

  (undo! [this]
    (describe-and-swap!
     (:manager-data this)
     (fn [{:keys [store history future] :as state}]
       (if (or (empty? history)
               (and (equivalent-undo-point? store)
                    (empty? (rest history))))
         [state []]
         (if (equivalent-undo-point? store)
           ;; We are equivalent to the top store in the history, so we
           ;; need to undo to the store beyond that.
           ;; The first if has ensured that there are at least two items
           ;; in the history.
           (let [[[modified-ids1 prev-store]
                  [modified-ids2 target-store]
                  & remaining-history] history]
             [{:store target-store
               :history remaining-history
               :future (cons [modified-ids2 prev-store]
                             (if (empty? future)
                               ;; Don't push an equivalent undo point as the
                               ;; final future store, as a redo would want
                               ;; to go beyond that, and couldn't.
                               future
                               (cons [modified-ids1 store] future)))}
              (distinct (concat (keys-affected-by-ids
                                 modified-ids1 store prev-store)
                                (keys-affected-by-ids
                                 modified-ids2 prev-store target-store)))])
           (let [[[modified-ids prev-store] & remaining-history] history]
             [{:store prev-store
               :history remaining-history
               :future (cons [modified-ids store] future)}
              (keys-affected-by-ids modified-ids prev-store store)]))))))

  (can-redo? [this]
    (let [future (:future (:value @(:manager-data this)))]
      (and (not (empty? future))
           (let [[[modified-ids next-store] & remaining-future] future]
             (or (not (equivalent-undo-point? next-store))
                 (not (empty? remaining-future)))))))

  (redo! [this]
    (describe-and-swap!
     (:manager-data this)
     (fn [{:keys [store history future] :as state}]
       (if (or (empty? future)
               (let [[[modified-ids next-store] & remaining-future] future]
                 (and (equivalent-undo-point? next-store)
                      (empty? remaining-future))))
         [state []]
         (let [[[modified-ids next-store] & remaining-future] future]
           (if (equivalent-undo-point? next-store)
             ;; We have to go beyond the top store, which is equivalent
             ;; to the current one.
             (let [[[modified-ids2 target-store] & target-future]
                   remaining-future]
               [{:store target-store
                  :history (cons [modified-ids2 next-store]
                                 (cons [modified-ids store] history))
                 :future remaining-future}
                (distinct (concat (keys-affected-by-ids
                                   modified-ids store next-store)
                                  (keys-affected-by-ids
                                   modified-ids2 next-store target-store)))])
             [{:store next-store
               :history (cons [modified-ids store] history)
               :future remaining-future}
              (keys-affected-by-ids modified-ids next-store store)])))))))

(defmethod print-method MutableStoreImpl [s ^java.io.Writer w]
  (.write w "Mutable:")
  (print-method (dissoc (:value @(:manager-data s)) :history) w))

(defmethod new-mutable-store true [immutable-store queue]
  (map->MutableStoreImpl
     {:manager-data (new-mutable-manager-data
                     (store-to-manager-data immutable-store)
                     queue)}))

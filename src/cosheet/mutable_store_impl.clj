(ns cosheet.mutable-store-impl
  (:require (cosheet [store :refer :all]
                     [mutable-manager
                      :refer [new-mutable-manager-data
                              mutable-manager-queue
                              current-mutable-value
                              get-or-make-reporter
                              stop-tracking-suspended reset-manager-value!
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
     
     ;; :history is a list of [modified-keys, store] pairs going backward
     ;; in time.
     :history nil
     ;; :future is a list of [modified-keys, store] pairs going forward
     ;; in time, starting from the next one after the current store.
     :future nil
     }))

(defn item-ids-affected-by-id
  "Return a seq of ids in the store that contain the given id, and thus
   might are affected by a change to it."
  [id store]
  (loop [id (when id (non-implicit-id id))
         affected nil]
    (if (nil? id)
      affected
      (recur (id->subject store id)
             (conj affected id)))))

(defn keys-affected-by-ids
  "Return a collection of keys that might be affected by a change to a set of
   modified ids.  We are given both the old store and the new one, as
   some modified ids might be in only one of the two stores."
  [modified-ids old-store new-store]
  (when (seq modified-ids)
    (reduce (fn [accum id]
              (-> accum
                  (into (item-ids-affected-by-id id old-store))
                  (into (item-ids-affected-by-id id new-store))))
            ;; We have to inform reporters keyed by nil if anything changed.
            #{nil} 
            modified-ids)))

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

(defn add-to-modified-keys-in-sequence
  "Given a history or future sequence, add the specified modified keys to
   the modified keys for the top item."
  [history modified-keys]
  (when (seq history)
    (let [[[top-modified top-store] & remainder] history]
      (cons [(distinct (concat top-modified modified-keys)) top-store]
            remainder))))

(defn can-undo-impl
  [store history]
  (when (not (empty? history))
    (or (not (equivalent-undo-point? store))
        (let [[[modified-keys store] & remaining-history] history]
          (can-undo-impl store remaining-history)))))

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
    (reset-manager-value! (:manager-data this)
                          (store-to-manager-data new-store)))

  (do-update! [this update-fn]
    (do-update-control-return! this (fn [store] [(update-fn store) nil])))

  (do-update-control-return! [this update-fn]
    (describe-and-swap-control-return!
     (:manager-data this)
     (fn [{:keys [store history future] :as state}]
       (let [[updated-store result] (update-fn store)
             [new-store modified-ids] (fetch-and-clear-modified-ids
                                       updated-store)
             modified-keys (keys-affected-by-ids modified-ids store new-store)]
         [(if (not= new-store store)
            (if (seq modified-keys)
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
                   :history (add-to-modified-keys-in-sequence
                             history modified-keys)
                   :future (add-to-modified-keys-in-sequence
                            future modified-keys)}
                  ;; The new store is equivalent to the current one.
                  ;; We still need to push the current one, as an undo
                  ;; followed by redo should land us there.
                  ;; We don't have to wipe out the future, but we do need
                  ;; to update its modified ids.
                  {:store new-store
                   :history (cons [modified-keys store] history)
                   :future (add-to-modified-keys-in-sequence
                            future modified-keys)})
                {:store new-store
                 :history (cons [modified-keys store] history)
                 :future nil})
              ;; Only some ancilliary information unrelated to the content
              ;; changed.
              (assoc state :store new-store))
            state)
          modified-keys
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
      (can-undo-impl store history)))

  (undo! [this]
    (describe-and-swap!
     (:manager-data this)
     (fn [{:keys [store history future] :as state}]
       (if (can-undo-impl store history)
         ;; Loop until we find a store that is not equivalent to its
         ;; predecessor. We need to undo to the store before that.
         ;; (Even though we try to avoid having several equivalent stores
         ;; in the history, that can happen if a new store is an equivalent
         ;; store and the future holds one, as well.)
         (loop [store store
                history history
                future future
                cum-modified-keys nil]
           (let [[[modified-keys prev-store] & remaining-history] history
                 cum-modified-keys (if cum-modified-keys
                                     (distinct (concat modified-keys
                                                       cum-modified-keys))
                                     modified-keys)]
             (if (equivalent-undo-point? store)
               (recur prev-store
                      remaining-history
                      (if (empty? future)
                        ;; Don't push an equivalent undo point as the
                        ;; final future store, as a redo would want
                        ;; to go beyond that, and couldn't.
                        future
                        (cons [modified-keys store] future))
                      cum-modified-keys)
               [{:store prev-store
                 :history remaining-history
                 :future (cons [cum-modified-keys store] future)}
                cum-modified-keys])))
         [state []]))))

  (can-redo? [this]
    (let [future (:future (:value @(:manager-data this)))]
      (some (fn [[modified-keys store]]
              (not (equivalent-undo-point? store)))
            future)))

  (redo! [this]
    (describe-and-swap!
     (:manager-data this)
     (fn [{:keys [store history future] :as state}]
       (if (or (empty? future)
               (let [[[modified-keys next-store] & remaining-future] future]
                 (and (equivalent-undo-point? next-store)
                      (empty? remaining-future))))
         [state []]
         (loop [store store
                history history
                future future
                cum-modified-keys nil]
           (let [[[modified-keys next-store] & remaining-future] future
                 history (cons [modified-keys store] history)
                 cum-modified-keys (if cum-modified-keys
                                     (distinct (concat modified-keys
                                                       cum-modified-keys))
                                     modified-keys)]
             (if (equivalent-undo-point? next-store)
               (recur
                next-store
                history
                remaining-future
                cum-modified-keys)
               [{:store next-store
                 :history history
                 :future remaining-future}
                cum-modified-keys]))))))))

(defmethod print-method MutableStoreImpl [s ^java.io.Writer w]
  (.write w "Mutable:")
  (print-method (dissoc (:value @(:manager-data s)) :history) w))

(defmethod new-mutable-store true [immutable-store queue]
  (map->MutableStoreImpl
     {:manager-data (new-mutable-manager-data
                     (store-to-manager-data immutable-store)
                     queue)}))

(defn stop-speculative-tracking
  "Stop speculative tracking (tracking for any reporters that currently
   have no attendees)."
  [mutable-store]
  (stop-tracking-suspended (:manager-data mutable-store)))

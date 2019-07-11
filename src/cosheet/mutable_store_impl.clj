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
                     [utils :refer [call-with-latest-value union-seqs
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
     ;; Usually, when a new action is done, future is cleared out. However,
     ;; if a new state is undo-equivalent, then the future is not changed.
     ;; If this is followed by a redo command, then it will be as if the
     ;; latest change never happened. In particular, the most recent change
     ;; will not become part of the history. This means that a redo followed
     ;; by an undo will return to the state just before the redone action
     ;; was originally taken.
     :future nil
     ;; :futures-state will only be present if equivalent-undo-point
     ;; stores have been created, causing a push onto history, but without
     ;; changing future. In that case, :futures-state holds the state
     ;; that was current when :future was created.
     :futures-state nil
     ;; :futures-modified-ids is present when :futures-state is
     ;; present, and holds the modified ids between the current :store
     ;; and the :store in :futures-modified-ids.
     :futures-modified-ids nil
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

(defn rearrange-for-undo
  "Given a state, return the new state after an undo (ignoring
  futures-state), and also return the modified ids."
  [state]
  ;; Loop until we find a store that is not equivalent to its
  ;; predecessor. We need to undo to the store before that.  (Even
  ;; though we try to avoid having several equivalent stores in the
  ;; history, that can happen if a new store is an equivalent store
  ;; and the future holds one, as well.)
  (loop [store (:store state)
         history (:history state)
         future (:future state)
         cum-modified-keys nil]
     (let [[[modified-keys prev-store] & remaining-history] history
           cum-modified-keys (union-seqs modified-keys cum-modified-keys)]
       (if (equivalent-undo-point? store)
         (recur prev-store
                remaining-history
                (if (empty? future)
                  ;; Don't push an equivalent undo point as the
                  ;; final future store, as a redo would want
                  ;; to go beyond that, and couldn't.
                  nil
                  (cons [modified-keys store] future))
                cum-modified-keys)
         [{:store prev-store
           :history remaining-history
           :future (cons [modified-keys store] future)}
          cum-modified-keys]))))

(defn rearrange-for-redo
  "Given a state, return the new state after a redo (ignoring
  futures-state), and also return the modified ids."
  [state]
  (loop [store (:store state)
         history (:history state)
         future (:future state)
         cum-modified-keys nil]
    (let [[[modified-keys next-store] & remaining-future] future
          history (cons [modified-keys store] history)
          cum-modified-keys (union-seqs modified-keys cum-modified-keys)]
      (if (equivalent-undo-point? next-store)
        (recur
         next-store
         history
         remaining-future
         cum-modified-keys)
        [{:store next-store
          :history history
          :future remaining-future}
         cum-modified-keys]))))

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
                ;; Since the new store is logically equivalent, we don't
                ;; need to wipe out the future. We just need to make some
                ;; changes to the history.
                (let [new-history
                      (if (equivalent-undo-point? store)
                        ;; The new store and the current one are both
                        ;; equivalent to the top one in the history. This
                        ;; means that undo will never go to the current
                        ;; store, so we don't need to push it onto the
                        ;; history.
                        (add-to-modified-keys-in-sequence
                         history modified-keys)
                         ;; The new store is equivalent to the current one.
                         ;; We still need to push the current one, as an undo
                         ;; followed by redo should land us there.
                        (cons [modified-keys store] history))]
                  (let [{:keys [futures-state futures-modified-ids]} state]
                    (cond-> {:store new-store
                             :history new-history
                             :future future}
                      future
                      (assoc :futures-state (or futures-state state)
                             :futures-modified-ids (union-seqs
                                                    futures-modified-ids
                                                    modified-ids)))))
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
     (fn [{:keys [store history future futures-state] :as state}]
       (if (can-undo-impl store history)
         (if (empty? futures-state)
           (rearrange-for-undo state)
           (let [[state modified-ids] (rearrange-for-undo futures-state)]
             [state
              (union-seqs modified-ids (:futures-modified-ids state))]))
         [state []]))))

  (can-redo? [this]
    (let [future (:future (:value @(:manager-data this)))]
      (some (fn [[modified-keys store]]
              (not (equivalent-undo-point? store)))
            future)))

  (redo! [this]
    (describe-and-swap!
     (:manager-data this)
     (fn [{:keys [store history future futures-state] :as state}]
       (if (or (empty? future)
               (let [[[modified-keys next-store] & remaining-future] future]
                 (and (equivalent-undo-point? next-store)
                      (empty? remaining-future))))
         [state []]
         (if (empty? futures-state)
           (rearrange-for-redo state)
           (let [[state modified-ids] (rearrange-for-redo futures-state)]
             [state
              (union-seqs modified-ids (:futures-modified-ids state))])))))))

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

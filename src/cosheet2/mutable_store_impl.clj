(ns cosheet2.mutable-store-impl
  (:require (cosheet2 [store :refer :all]
                      [store-impl :refer [all-ids-eventually-holding-content]]
                      [reporter :refer [set-value! change-data!
                                        data-value reporter-data
                                        change-data-control-return!
                                        reporter-value
                                        universal-category Reporter]]
                      [expression :refer [cache category-change]]
                      [utils :refer [call-with-latest-value union-seqs
                                     update-in-clean-up
                                     swap-control-return!]])))

(defn add-id-to-affected-ids
  "Takes a set of ids that contains all ids that might be affected by
   a change to any of them. (In concrete terms, it contains all of
   their subjects and contains all ids that contain any of them.) Add
   the given id and restore the closure property."
  [affected store id]
  (loop [pending-ids [id]
         affected affected]
    (if (empty? pending-ids)
      affected
      (let [[id & remaining-ids] pending-ids]
        (if (contains? affected id)
          (recur remaining-ids affected)
          (recur (concat pending-ids
                         (all-ids-eventually-holding-content store id)
                         (when-let [subject (id->subject store id)]
                           [subject]))
                 (conj affected id)))))))

(defn categories-affected-by-ids
  "Return a set of categories that might be affected by a change to a
  set of modified ids.  A category is any id whose elements or content
  could be affected by one of the changed ids. We are given both the
  old store and the new one, as some modified ids might be in only one
  of the two stores."
  [modified-ids old-store new-store]
  (when (seq modified-ids)
    (reduce (fn [accum id] (-> accum
                               (add-id-to-affected-ids old-store id)
                               (add-id-to-affected-ids new-store id)))
            #{} 
            modified-ids)))

(defmacro cache-and-categorize
  "Return the result of the operation on the store, caching, and using
   the given categories."
  [categories operation store & args]
  `(cache ~operation (category-change ~categories ~store) ~@args))

(defn add-to-modified-ids-in-sequence
  "Given a history or future sequence, add the specified modified keys to
   the modified keys for the top item."
  [history modified-ids]
  (when (seq history)
    (let [[[top-modified top-store] & remainder] history]
      (cons [(distinct (concat top-modified modified-ids)) top-store]
            remainder))))

(defn change-description
  "Given an old state the new state, and a list of modified ids, return a
   triple suitable for change-data!"
  [old-state new-state modified-ids]
  [new-state
   modified-ids
   (categories-affected-by-ids modified-ids
                               (:value old-state) (:value new-state))])

(defn change-and-add-to-history
  "Given a reporter state, a new store, and the modified ids between them,
  change the store and update the history."
  [state new-store modified-ids]
  (let [{:keys [value history future]} state
        old-store value]
    (if (not= new-store old-store)
      (cond-> (assoc state :value new-store)
        (seq modified-ids)
        (into (if (equivalent-undo-point? new-store)
                ;; Since the new store is logically equivalent, we don't
                ;; need to wipe out the future. We just need to make some
                ;; changes to the history.
                (let [new-history
                      (if (equivalent-undo-point? old-store)
                        ;; The new store and the current one are both
                        ;; equivalent to the top one in the history. This
                        ;; means that undo will never go to the current
                        ;; store, so we don't need to push it onto the
                        ;; history.
                        (add-to-modified-ids-in-sequence
                         history modified-ids)
                        ;; The new store is equivalent to the current one.
                        ;; We still need to push the current one, as an undo
                        ;; followed by redo should land us there.
                        (cons [modified-ids old-store] history))]
                  (let [{:keys [futures-state futures-modified-ids]} state]
                    (cond-> {:history new-history
                             :future future}
                      future
                      (assoc :futures-state (or futures-state state)
                             :futures-modified-ids (union-seqs
                                                    futures-modified-ids
                                                    modified-ids)))))
                {:history (cons [modified-ids old-store] history)
                 :future nil})))
      state)))

(defn can-undo-impl
  [store history]
  (when (not (empty? history))
    (or (not (equivalent-undo-point? store))
        (let [[[modified-keys store] & remaining-history] history]
          (can-undo-impl store remaining-history)))))

(defn rearrange-for-undo
  "Given our reporter's data, return the new state after an undo (ignoring
  futures-state), and also return the modified ids."
  [state]
  ;; Loop until we find a store that is not equivalent to its
  ;; predecessor. We need to undo to the store before that.  (Even
  ;; though we try to avoid having several equivalent stores in the
  ;; history, that can happen if a new store is an equivalent store
  ;; and the future holds one, as well.)
  (loop [store (:value state)
         history (:history state)
         future (:future state)
         cum-modified-ids nil]
     (let [[[modified-ids prev-store] & remaining-history] history
           cum-modified-ids (union-seqs modified-ids cum-modified-ids)]
       (if (equivalent-undo-point? store)
         (recur prev-store
                remaining-history
                (if (empty? future)
                  ;; Don't push an equivalent undo point as the
                  ;; final future store, as a redo would want
                  ;; to go beyond that, and couldn't.
                  nil
                  (cons [modified-ids store] future))
                cum-modified-ids)
         [(assoc state
                 :value prev-store
                 :history remaining-history
                 :future (cons [modified-ids store] future))
          cum-modified-ids]))))

(defn rearrange-for-redo
  "Given a state, return the new state after a redo (ignoring
  futures-state), and also return the modified ids."
  [state]
  (loop [store (:value state)
         history (:history state)
         future (:future state)
         cum-modified-ids nil]
    (let [[[modified-ids next-store] & remaining-future] future
          history (cons [modified-ids store] history)
          cum-modified-ids (union-seqs modified-ids cum-modified-ids)]
      (if (equivalent-undo-point? next-store)
        (recur
         next-store
         history
         remaining-future
         cum-modified-ids)
        [(assoc state
                :value next-store
                :history history
                :future remaining-future)
         cum-modified-ids]))))

(defrecord MutableStoreImpl
    ^{:doc
      "A store that contains an immutable store,
       supports mutation to that store, handles undo,
       and returns reporter objects for queries."}

    [;; We are a reporter whose value is the current store,
     ;; and that has additional fields to track history.
     data]

  Reporter

  Store

  (id-valid? [this id]
    (cache-and-categorize
     [id] id-valid? this id))

  (id->content [this id]
    (cache-and-categorize
     [id] id->content this id))

  (id->element-ids [this id]
    (cache-and-categorize
     [id] id->element-ids this id))

  (id->subject [this id]
    (id->subject (reporter-value this) id))

  (id-label->element-ids [this id label]
    (cache-and-categorize
     [id] id-label->element-ids this id label))

  (id->has-keyword? [this id keyword]
    (cache-and-categorize
     [id] id->has-keyword? this id keyword))

  (candidate-matching-ids [this template]
    (cache candidate-matching-ids this template))

  (mutable-store? [this] true)
  
  MutableStore

  (current-store [this] (reporter-value this))

  (store-reset! [this new-store]
    (set-value! this (track-modified-ids new-store)))

  (store-update! [this update-fn]
    (store-update-control-return! this (fn [store] [(update-fn store) nil])))

  (store-update-and-act! [this update-fn]
    (let [actions (store-update-control-return!
                   this
                   #(store-fetch-and-clear-further-actions (update-fn %)))]
    (doseq [action actions]
      (apply (first action) (rest action)))))

  (store-update-control-return! [this update-fn]
    (change-data-control-return!
     this
     (fn [state]
       (let [store (data-value state)
             [updated-store result] (update-fn store)
             [new-store modified-ids] (fetch-and-clear-modified-ids
                                       updated-store)
             new-state (change-and-add-to-history state new-store modified-ids)]
         (conj (change-description state new-state modified-ids)
               result)))))

  (can-undo? [this]
    (let [{:keys [value history]} (reporter-data this)]
      (can-undo-impl value history)))

  (undo! [this]
    (change-data!
     this
     (fn [{:keys [value history future futures-state] :as state}]
       (let [store value]
         (if (can-undo-impl store history)
           (if (empty? futures-state)
             (let [[new-state modified-ids] (rearrange-for-undo state)]
               (change-description state new-state modified-ids))
             (let [[new-state modified-ids] (rearrange-for-undo futures-state)
                   modified-ids (union-seqs modified-ids
                                            (:futures-modified-ids state))]
               (change-description state new-state modified-ids)))
           [state [] []])))))

  (can-redo? [this]
    (let [future (:future (reporter-data this))]
      (some (fn [[modified-ids store]]
              (not (equivalent-undo-point? store)))
            future)))

  (redo! [this]
    (change-data!
     this
     (fn [{:keys [value history future futures-state] :as state}]
       (let [store value]
         (if (or (empty? future)
                 (let [[[modified-ids next-store] & remaining-future] future]
                   (and (equivalent-undo-point? next-store)
                        (empty? remaining-future))))
           [state [] []]
           (if (empty? futures-state)
             (let [[new-state modified-ids] (rearrange-for-redo state)]
               (change-description state new-state modified-ids))
             (let [[new-state modified-ids] (rearrange-for-redo futures-state)
                   modified-ids (union-seqs modified-ids
                                            (:futures-modified-ids state))]
               (change-description state new-state modified-ids)))))))))

(defmethod print-method MutableStoreImpl [s ^java.io.Writer w]
  (.write w "MutableStore"))

(defmethod new-mutable-store true [immutable-store]
  "Given an immutable store, create the reporter whose value is the current
   store and that also holds our history information."
  (->MutableStoreImpl
   (atom
    {:value (track-modified-ids immutable-store)
     :priority Double/MAX_VALUE
     
     ;; The next two fields each are a list of [modified-ids, store] pairs
     ;; where store gives one store in a sequence of stores, and
     ;; modified-ids gives the ids that change between that store and
     ;; the previous one in the sequence.
     
     ;; :history is a list of [modified-ids, store] pairs going backward
     ;; in time.
     :history nil
     ;; :future is a list of [modified-ids, store] pairs going forward
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
     })))


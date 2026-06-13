(ns cosheet.mutable-store-impl
  (:require (cosheet [store :refer :all]
                      [reporter :refer [set-value! change-data!
                                        data-latest-value reporter-data
                                        change-data-control-return!
                                        reporter-value-or-invalid
                                        value-valid? data-valid?
                                        universal-category Reporter
                                        merge-default-reporter-data]]
                      [reporter-macros :refer [cache-R]]
                      [category-change-calculator :refer [category-change-R]]
                      [utils :refer [call-with-latest-value union-seqs
                                     update-in-clean-up
                                     swap-control-return!]])))

(defn new-mutable-store-data
  "Return the data for a new mutable store with a state that starts out
  equal to a given immutable store."
  [immutable-store]
  (let [initial (track-modified-ids immutable-store)]
    (merge-default-reporter-data
     :value initial
     ;; Undo is supported by having a list of past store states,
     ;; starting with the most recent, and going backward in
     ;; time. Each past state is recorded along with the set of ids
     ;; that differ between it and the next more recent state. Redo is
     ;; supported with a similar list of future store states.

     ;; :history is a list of [modified-ids, store] pairs going
     ;; backward in time.
     :history nil
     ;; :future is a list of [modified-ids, store] pairs going forward
     ;; in time, starting from the next one after the current store.
     :future nil

     ;; When the user makes a significant change to the store, we push
     ;; the old store onto the history. Then, when the user does an
     ;; undo, we pop the top store off the history, making it the
     ;; current store, and push the current store onto the future, to
     ;; allow for a redo. The modified-ids stored along with a store
     ;; gives all the ids that can be affected when that store is made
     ;; current.
     
     ;; Redoing undone changes becomes impossible after a new change,
     ;; because the states on the future list are no longer derived
     ;; from the new current state, and we don't support merging
     ;; several changes. So we clear out the future whenever the user
     ;; makes a significant change.

     ;; But not all user changes are significant. The new state can be
     ;; marked "undo-equivalent", which means it is essentially
     ;; equivalent to the previous state. This usually means that the
     ;; changes between it and the previous store affect only display
     ;; information that is recorded in the store, like the current
     ;; selection, but not any persistent information.

     ;; We treat these undo-equivalent stores as either uninteresting
     ;; intermediaries or as side excursions, depending on what
     ;; happens later. In any case, they are never recorded in
     ;; :history or :future, and we will never revisit them.

     ;; To handle them, we have a couple additional fields.
     :current-significant initial
     ;; :current-significant holds the non-equivalent store that is
     ;; equivalent to the current store. (It will be the current store
     ;; if the current store is non-equivalent.) When we push a store
     ;; onto history or future, it is :current-significant that we push.
     :modified-ids-to-current-significant nil
     ;; :modified-ids-to-current-significant holds the modified ids
     ;; between the current store and :current-significant.
     )))

(defn add-id-to-affected-ids
  "Takes a set of ids that contains all ids that might be affected by a
  change to any of them. (In concrete terms, it contains all of their
  targets and contains all ids that contain any of them.) Add the
  given id and restore the closure property. The store must be
  immutable."
  [affected store id]
  (loop [pending-ids [id]
         affected affected]
    (if (empty? pending-ids)
      affected
      (let [[id & remaining-ids] pending-ids]
        (if (contains? affected id)
          (recur remaining-ids affected)
          (recur (concat remaining-ids
                         (when-let [target (id->target store id)]
                           [target]))
                 (conj affected id)))))))

(defn categories-in-one-store-affected-by-ids
  "Return a set of categories that might be affected by a change to a
  set of modified ids in the given store.  A category is any id whose
  elements or source could be affected by one of the changed ids. The
  store must be immutable."
  [modified-ids store]
  (when (seq modified-ids)
    (reduce (fn [accum id] (add-id-to-affected-ids accum store id))
            #{} 
            modified-ids)))

(defn categories-affected-by-ids
  "Return a set of categories that might be affected by a change to a
  set of modified ids.  A category is any id whose elements or source
  could be affected by one of the changed ids. We are given both the
  old store and the new one, as some modified ids might be in only one
  of the two stores."
  [modified-ids before-store after-store]
  (when (seq modified-ids)
    (clojure.set/union
     (categories-in-one-store-affected-by-ids modified-ids before-store)
     (categories-in-one-store-affected-by-ids modified-ids after-store))))

(defmacro cache-and-categorize
  "Return the result of the operation on the store, caching, and using
   the given categories."
  [categories operation store & args]
  `(cache-R ~operation (category-change-R ~categories ~store) ~@args))

(defn description-of-change
  "Given an old state, the new state, and a list of modified ids, return a
   triple suitable for change-data!"
  [old-state new-state modified-ids]
  [new-state
   modified-ids
   (categories-affected-by-ids
    modified-ids (data-latest-value old-state) (data-latest-value new-state))])

(defn change-and-add-to-history
  "Given the mutable store's reporter state, the revised store from
  after a change, and the modified ids between that store and the
  store before the change, update the state's current store, history,
  and :current-significant/:modified-ids-to-current-significant
  tracking."
  [state after-store modified-ids]
  (let [{:keys [value history future
                current-significant modified-ids-to-current-significant]} state
        before-store value]
    (if (= after-store before-store)
      state
      (let [state (assoc state :value after-store)
            equivalent (equivalent-undo-point? after-store)]
        (if (empty? modified-ids)
          ;; Nothing changed.
          state
          (let [modified-ids-to-current-significant
                (union-seqs (:modified-ids-to-current-significant state)
                            modified-ids)]
            (if (equivalent-undo-point? after-store)
              ;; No significant change, just update the modified-ids.
              (assoc state
                     :modified-ids-to-current-significant
                     modified-ids-to-current-significant)
              ;; A significant change; we push it.
              (assoc state
                     :history (cons [modified-ids-to-current-significant
                                     current-significant]
                                    history)
                     :future nil
                     :current-significant after-store
                     :modified-ids-to-current-significant nil))))))))

(defn rearrange-for-undo
  "Given our reporter's data, return the new state after an undo, and
  also return the modified ids."
  [state]
  (let [{:keys [history future
                current-significant modified-ids-to-current-significant]} state
        [[hist-modified hist-store] & remaining-history] history
        all-modified (union-seqs modified-ids-to-current-significant
                                 hist-modified)]
    [(assoc state
            :value hist-store
            :history remaining-history
            :future (cons [hist-modified current-significant] future)
            :current-significant hist-store
            :modified-ids-to-current-significant nil)
     all-modified]))

(defn rearrange-for-redo
  "Given our reporter's data, return the new state after a redo, and
  also return the modified ids."
  [state]
  (let [{:keys [history future
                current-significant modified-ids-to-current-significant]} state
        [[fut-modified fut-store] & remaining-future] future
        all-modified (union-seqs modified-ids-to-current-significant fut-modified)]
    [(assoc state
            :value fut-store
            :future remaining-future
            :history (cons [fut-modified current-significant] history)
            :current-significant fut-store
            :modified-ids-to-current-significant nil)
     all-modified]))

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

  (id-valid-link? [this id]
    (cache-and-categorize
     [id] id-valid-link? this id))

  (id-known-object? [this id]
    (cache-and-categorize
     [id] id-known-object? this id))

  (id->target [this id]
    (cache-and-categorize
        [id] id->target this id))

  (id->source [this id]
    (cache-and-categorize
        [id] id->source this id))

  (target->ids [this target]
    (cache-and-categorize
        [target] target->ids this target))

  (source->ids [this source]
    (cache-and-categorize
        [source] source->ids this source))

  (target-source->ids [this target source]
    (cache-and-categorize
        [target source] target-source->ids this target source))

  (target-label->ids [this target label]
    (cache-and-categorize
        [target] target-label->ids this target label))

  (source-label->ids [this source label]
    (cache-and-categorize
        [source] source-label->ids this source label))

  (candidate-matching-ids [this template]
    (cache-R candidate-matching-ids this template))

  (mutable-store? [this] true)
  
  MutableStore

  (current-store [this] (reporter-value-or-invalid this))

  (store-reset! [this new-store]
    (assert (value-valid? new-store))
    (change-data!
     this
     (fn [state]
       (let [tracked (track-modified-ids new-store)
             new-state (assoc state
                              :value tracked
                              :history nil
                              :future nil
                              :current-significant tracked
                              :modified-ids-to-current-significant nil)]
         [new-state nil nil]))))

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
       (let [store (data-latest-value state)
             [updated-store result] (update-fn store)
             [new-store modified-ids] (fetch-and-clear-modified-ids
                                       updated-store)
             new-state (change-and-add-to-history state new-store modified-ids)
             new-state (if (and (= store (:current-significant state))
                               (empty? modified-ids))
                         (assoc new-state :current-significant new-store)
                         new-state)]
         (assert (data-valid? new-state))
         (conj (description-of-change state new-state modified-ids)
               result)))))

  (can-undo? [this]
    (some? (:history (reporter-data this))))

  (undo! [this]
    (change-data!
     this
     (fn [state]
       (if (some? (:history state))
         (let [[new-state modified-ids] (rearrange-for-undo state)]
           (description-of-change state new-state modified-ids))
         [state [] []]))))

  (can-redo? [this]
    (some? (:future (reporter-data this))))

  (redo! [this]
    (change-data!
     this
     (fn [state]
       (if (some? (:future state))
         (let [[new-state modified-ids] (rearrange-for-redo state)]
           (description-of-change state new-state modified-ids))
         [state [] []])))))

(defmethod print-method MutableStoreImpl [s ^java.io.Writer w]
  (.write w "MutableStore"))

(defmethod new-mutable-store true [immutable-store]
  "Given an immutable store, create the reporter whose value is the current
   store and that also holds our history information."
  (->MutableStoreImpl
   (atom (new-mutable-store-data immutable-store))))


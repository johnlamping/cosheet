(ns cosheet2.mutable-store-impl
  (:require (cosheet2 [store :refer :all]
                      [reporter :refer [set-value! change-data!
                                        data-value reporter-data
                                        change-data-control-return!
                                        reporter-value
                                        universal-category Reporter]]
                      [expression :refer [cache category-change]]
                      [utils :refer [call-with-latest-value union-seqs
                                     update-in-clean-up
                                     swap-control-return!]])))

(defn new-mutable-store-data
  "Return the data for a new mutable store with a state that starts out
  equal to a given immutable store."
  [immutable-store]
  {:value (track-modified-ids immutable-store)
   :priority Double/MAX_VALUE
   
   ;; Undo is supported by having a list of past store states,
   ;; starting with the most recent, and going backward in time. Each
   ;; past state is recorded along with the set of ids that differ
   ;; between it and the next more recent state. Redo is supported
   ;; with a similar list of future store states, this time with each
   ;; successive state being another step forward in time, with the
   ;; ids where it differs from the preceeding state.

   ;; So at any given time, there is a list of states going into the
   ;; past, the current state, and a list of states going into the
   ;; future. Undo and redo then just become a matter of pushing the
   ;; current state onto one of the lists, and popping it off the
   ;; other one.

   ;; :history is a list of [modified-ids, store] pairs going backward
   ;; in time.
   :history nil
   ;; :future is a list of [modified-ids, store] pairs going forward
   ;; in time, starting from the next one after the current store.
   :future nil

   ;; When the user makes a significant change (not an undo or a
   ;; redo), we push the old state onto the history, so an undo will
   ;; get back to it. And the modified state becomes the current
   ;; state. Additionally, redoing undone changes becomes impossible
   ;; after that a new change, because the states on the future list
   ;; are no longer derived from the new current state, and we don't
   ;; support merging several changes. So we clear out the future
   ;; whenever the user makes a significant change.

   ;; But not all user changes are significant. The new state might be
   ;; marked "undo-equivalent", which means it is basically equivalent
   ;; to the previous state. This usually means that the changes
   ;; between it and the previous store affect only display
   ;; information that is recorded in the store, like the current
   ;; selection, but not any persistent information.

   ;; If the user clicks around a few times, we get sequences of
   ;; states with equivalent persistent information. Stepping within
   ;; the sequence is just doing things like moving the focus
   ;; around. So an undo or redo while we are in such a sequence
   ;; should go past it, to the first state is is not undo-equivalent
   ;; to. Instead, we should go to the first non-equivalent state.
   
   ;; That leaves the question of what to do when an undo or redo
   ;; moves to such a sequence. Which of its equivalent stores should
   ;; we go to? The answer turns out to be to move to the state
   ;; closest to the state we are coming from. For example, if we are
   ;; moving backward with an undo, we want to move to the state just
   ;; before the last significant change, because that state will
   ;; record where the user focus was when they made that change,
   ;; which was probably on the changed item. Putting them back at
   ;; that focus will help them see what the undo changed. In the
   ;; other direction, when moving forward in time, with a redo, we
   ;; want to go to the state right after the next significant change,
   ;; because it will still have the user focus on the item that
   ;; changed.

   ;; So that means that we only need to record the first and last
   ;; states in any sequence of undo-equivalent states, because those
   ;; are the only ones we will ever have to return to. So the first
   ;; time we make an undo-equivalent change, we push the previous
   ;; state onto the history, as the first of the sequence of
   ;; undo-equivalent states. Then we don't push again until the next
   ;; change that is not undo-equivalent to this sequence, at which
   ;; point the push will put the last state of the sequence into the
   ;; history. We will never have more than two consequtive
   ;; undo-equivalent states in either the history or future, with the
   ;; second of the two marked undo-equivalent to its previous
   ;; sequence.

   ;; There another special case: when we are into a sequence of
   ;; equivalent stores, and the user does an undo or a redo. Here is
   ;; a sequence of steps that can get a user into the most
   ;; general situation:
   ;;   * Start in store state S0
   ;;   * Make a significant change, reaching state S1
   ;;   * Make some non-significant changes, reaching the equivalent state S1a
   ;;   * Then make a significant change, reaching state S2
   ;;   * Do an undo, returning to state S1a
   ;;   * Make some more non-significant changes, reaching state S1b
   ;;   * Do an undo or redo.
   
   ;; When the undo or redo happens from state S1b, it turns out that
   ;; we want to throw out the non-significant changes that led to
   ;; state S1b, reverting the user to state S1a before doing the undo
   ;; or redo. That is because the state just before the first state
   ;; in the future, S2, is state S1a. That is the state we will want
   ;; return to if the user ever does redos up to S2 and then does an
   ;; undo. Our current state, S1b, is never something we want to
   ;; return to.

   ;; To support this, when we do an undo, and get to a state like S1b
   ;; that is undo-equivalent to the previous state, we also push that
   ;; state onto the history. Keeping it available should we need
   ;; it. We do something special on most actions when we are in an
   ;; undo-equivalent state and there is an undo-eqivalent state on
   ;; top of the history:
   ;;   * For an undo or a redo, we revert to the state on top of the
   ;;     history.
   ;;   * For a significant change, we replace the state on top of the
   ;;     stack with our state before the change. (Since that is the
   ;;     last state before the changed state.)
   ;;   * For an undo-equivalent change, we don't do anything special;
   ;;     the other two cases will do the right thing with our new
   ;;     state.
   })

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
  `(cache ~operation (category-change ~categories ~store) ~@args))

(defn description-of-change
  "Given an old state, the new state, and a list of modified ids, return a
   triple suitable for change-data!"
  [old-state new-state modified-ids]
  [new-state
   modified-ids
   (categories-affected-by-ids
    modified-ids (data-value old-state) (data-value new-state))])

(defn change-and-add-to-history
  "Given the mutable store's reporter state, the revised store from
  after a change, and the modified ids between that store and the
  store before the change, update the state's current store and
  history."
  [state after-store modified-ids]
  (let [{:keys [value history future]} state
        before-store value]
    (if (= after-store before-store)
      state
      (let [state (assoc state :value after-store)
            equivalent (equivalent-undo-point? after-store)]
        (if (empty? modified-ids)
          state
          (let [after-history
                (if equivalent
                  (if (equivalent-undo-point? before-store)
                    ;; The after store and the before store are both
                    ;; equivalent to the top one in the history, so we
                    ;; don't need to push onto it. We just have to add
                    ;; the recently changed ids to the difference
                    ;; between the top store in the history and the
                    ;; current store.
                    (when history
                      (let [[[top-modified top-store] & remaining-history]
                            history]
                        (cons [(union-seqs top-modified modified-ids)
                               top-store]
                              remaining-history)))
                    ;; The after store is equivalent to the before
                    ;; store, but the before store isn't equivalent to
                    ;; the top of the history. So we push the before
                    ;; store, making it available for a later redo.
                    (cons [modified-ids before-store] history))
                  (if (equivalent-undo-point? before-store)
                    (if (empty? history)
                      ;; Don't push an equivalent undo point onto an
                      ;; empty history. (This case shouldn't even happen.)
                      history
                      (let [[[top-modified top-store] & remaining-history]
                            history]
                        (if (equivalent-undo-point? top-store)
                          ;; Our before store is equivalent to the top of
                          ;; the history, and that store is equivalent to
                          ;; the store before that (which must mean it got
                          ;; there following an undo). Rather than push,
                          ;; replace the top of the history with our before
                          ;; store.
                          (cons [(union-seqs top-modified modified-ids)
                                 before-store]
                                remaining-history)
                          (cons [modified-ids before-store] history))))
                    (cons [modified-ids before-store] history)))]
            (assoc state
                   :history after-history
                   :future (when equivalent future))))))))

(defn move-forward-in-time
  "Given a history, current, and future, return a similar triple, with
  the top store of the future popped to become the current store, and
  the old current store pushed onto the history. Notice that this can
  be called with the history and future arguments swapped to move
  backward in time. Also return the ids that were changed between the
  old and new current stores."
  [history current future]
  (let [[[modified-ids top-store] & remaining-future] future]
    [[(when ;; There is no point having an undo-equivalent state as
            ;; the end point of the history (or future). And having
            ;; one there messes up detection of whether we can undo
            ;; (or redo).
          (or (seq history)
              (not (equivalent-undo-point? current)))
        (cons [modified-ids current] history))
      top-store
      remaining-future]
     modified-ids]))

(defn remove-unnedded-undo-equivalent
  "We are about to do an undo or a redo. Take our reporter's data. See
  if the current store is undo-equivalent to the store on top of the
  history and that store is undo-equivalent to the previous one. If so
  make it as if we never took the steps from the top of the history to
  the current store. (If the condition is satisfied, then the store on
  top of the history must be the one that came just before the top of
  the future, while our current store is just some navigation changes
  since then. We want to discard those changes.
  Also return the ids modified by any changes to what is the current
  store."
  [state]
  (let [{:keys [value history future]} state]
    (if (empty? history)
      [state nil]
      (let [[[top-modified top-store] & remaining-history]
            history]
        (if (and (equivalent-undo-point? value)
                 (equivalent-undo-point? top-store))
          [(assoc state
                  :history remaining-history
                  :value top-store)
           top-modified]
          [state nil])))))

(defn rearrange-for-undo
  "Given our reporter's data, return the new state after an undo, and
  also return the modified ids."
  [state]
  ;; Loop until we find a store that is not equivalent to its
  ;; predecessor. We need to undo to the store before that.  (Even
  ;; though we try to avoid having several equivalent stores in the
  ;; history, that can happen if a new store is an equivalent store
  ;; and the future holds one, as well.)
  (let [[cleaned cum-modified] (remove-unnedded-undo-equivalent state)
        {:keys [value history future]} cleaned
        current value
        ;; If our current store is undo equivalent to the top of the
        ;; history, we need to go past it.
        [[future current history] modified-ids]
        (if (equivalent-undo-point? current)
          (move-forward-in-time future current history)
          [[future current history] nil])
        cum-modified (union-seqs cum-modified modified-ids)
        ;; Now do the undo to the new store. 
        [[future current history] modified-ids] (move-forward-in-time
                                                 future current history)
        cum-modified (union-seqs cum-modified modified-ids)]
    [(assoc state
            :value current
            :history (if (equivalent-undo-point? current)
                       ;; This is the case where we need to also push
                       ;; this store back onto the history stack. (See
                       ;; the comments at the top of this file.)
                       (cons [nil current] history)
                       history)
            :future future)
     cum-modified]))

(defn rearrange-for-redo
  "Given our reporter's data, return the new state after a redo, and
  also return the modified ids."
  [state]
  (let [[cleaned cum-modified] (remove-unnedded-undo-equivalent state)
        {:keys [value history future]} cleaned
        current value
        ;; If the current store is undo equivalent to the top of the
        ;; future, we need to go past it.
        [[history current future] modified-ids]
        (if (equivalent-undo-point? (let [[[_ store] & _] future] store))
          (move-forward-in-time history current future)
          [[history current future] nil])
        cum-modified (union-seqs cum-modified modified-ids)
        ;; Now do the redo to the new store.
        [[history current future] modified-ids]
        (move-forward-in-time history current future)
        cum-modified (union-seqs cum-modified modified-ids)]
    [(assoc state
            :value current
            :history history
            :future future)
     cum-modified]))

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

  (id-described-object? [this id]
    (cache-and-categorize
     [id] id-described-object? this id))

  (id->target [this id]
    (id->target (reporter-value this) id))

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

  (id->marked-as-type? [this id]
    (cache-and-categorize
        [id] id->marked-as-type? this id))

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


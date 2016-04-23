(ns cosheet.mutable-store-impl
  (:require (cosheet [store :refer :all]
                     store-impl
                     [mutable-manager
                      :refer [new-mutable-manager-data
                              get-or-make-reporter
                              describe-and-swap!
                              describe-and-swap-control-return!]]
                     [reporters :as reporter
                      :refer [set-value! set-manager!
                              attended? new-reporter]]
                     [utils :refer [call-with-latest-value
                                    update-in-clean-up
                                    swap-control-return!]])))

(defn items-affected-by-id
  "Return a seq of items that might be affected by a change to the given id."
  [id old-store new-store]
  (loop [item id
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
            (into accum (items-affected-by-id id old-store new-store)))
          #{nil} ; We have to always inform reporters keyed by nil.
          modified-ids))

(defn non-implicit-id
  "Traverse through ImplicitContentIds to get to the non-implicit id."
  [id]
  (if (instance? cosheet.store_impl.ImplicitContentId id)
    (non-implicit-id (:containing-item-id id))
    id))

(defn apply-to-store
  "Return a reporter giving the result of the operation on the store
  of the state. Using this lets callers of this avoid creating thunks as
  arguments to get-or-make-reporter, which would break caching."
  [state operation & args]
  (apply operation (:store state) args))

(defrecord MutableStoreImpl
    ^{:doc
      "A store that contains an immutable store,
       supports mutation to that store, handles undo,
       and returns reporter objects for queries."}
  [manager-data ;; A mutable-manager manager-data holding the immutable
                ;; store and a history as its value. 
   ]

  Store

  (id-valid? [this id]
    (get-or-make-reporter
     [id] apply-to-store (:manager-data this) id-valid? id))

  (id-label->element-ids [this id label]
    (get-or-make-reporter
     [id] apply-to-store (:manager-data this) id-label->element-ids id label))

  (id->element-ids [this id]
    (get-or-make-reporter
     [id] apply-to-store (:manager-data this) id->element-ids id))

  (id->content [this id]
    (let [base-id (non-implicit-id id)]
      (get-or-make-reporter
       [base-id] apply-to-store (:manager-data this) id->content id)))

  (id->content-reference [this id]
    (get-or-make-reporter
     [id] apply-to-store (:manager-data this) id->content-reference id))

  (candidate-matching-ids [this item]
    (get-or-make-reporter
     [nil] apply-to-store (:manager-data this) candidate-matching-ids item))

  (mutable-store? [this] true)
  
  MutableStore

  (current-store [this] (:store (:value @(:manager-data this))))

  (call-dependent-on-id [this id fun]
    (get-or-make-reporter
     [id] apply-to-store (:manager-data this) fun))

  (do-update! [this update-fn]
    (do-update-control-return! this (fn [store] [(update-fn store) nil])))

  (do-update-control-return! [this update-fn]
    (describe-and-swap-control-return!
     (:manager-data this)
     (fn [{:keys [store history]}]
       (let [[updated-store result] (update-fn store)
             [new-store modified-ids] (fetch-and-clear-modified-ids
                                       updated-store)]
         [{:store new-store
           :history (cons [modified-ids store] history)
           :future nil}
          (keys-affected-by-ids modified-ids store new-store)
          result]))))

  (add-simple-element! [this subject content]
    (do-update-control-return! this #(add-simple-element % subject content)))

  (remove-simple-id! [this id]
    (do-update! this #(remove-simple-id % id)))

  (update-content! [this id content]
    (do-update! this #(update-content % id content)))

  (can-undo? [this]
    (not (empty? (:history (:value @(:manager-data this))))))

  (undo! [this]
    (describe-and-swap!
     (:manager-data this)
     (fn [{:keys [store history future]}]
       (if (empty? history)
         [{:store store :history history :future future} []]
         (let [[[modified-ids prev-store] & remaining-history] history]
           [{:store prev-store
             :history remaining-history
             :future (cons [modified-ids store] future)}
            (keys-affected-by-ids modified-ids prev-store store)])))))

  (can-redo? [this]
    (not (empty? (:future (:value @(:manager-data this))))))

  (redo! [this]
    (describe-and-swap!
     (:manager-data this)
     (fn [{:keys [store history future]}]
       (if (empty? future)
         [{:store store :history history :future future} []]
         (let [[[modified-ids next-store] & remaining-future] future]
           [{:store next-store
             :history (cons [modified-ids store] history)
             :future remaining-future}
            (keys-affected-by-ids modified-ids next-store store)]))))))

(defmethod print-method MutableStoreImpl [s ^java.io.Writer w]
  (.write w "Mutable:")
  (print-method (:value @(:manager-data s)) w))

(defmethod new-mutable-store true [immutable-store]
  (map->MutableStoreImpl
   {:manager-data (new-mutable-manager-data
                   {:store (track-modified-ids immutable-store)
                    ;; :history is a list of [modified-ids, store] pairs,
                    ;; where store gives a previous state and modified-ids
                    ;; gives the ids that change from that state to the next
                    ;; one.
                    :history nil
                    ;; :future is a list of [modified-ids, store] pairs,
                    ;; where store gives a next state and modified-ids
                    ;; gives the ids that change from the current state to
                    ;; the next one.
                    :future nil})}))

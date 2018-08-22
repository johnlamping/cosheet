(ns cosheet.mutable-store-impl
  (:require (cosheet [store :refer :all]
                     [mutable-manager
                      :refer [new-mutable-manager-data
                              current-mutable-value
                              get-or-make-reporter reset-manager!
                              describe-and-swap!
                              describe-and-swap-control-return!]]
                     [reporter
                      :refer [set-value! set-manager!
                              attended? new-reporter]]
                     [utils :refer [call-with-latest-value
                                    update-in-clean-up
                                    swap-control-return!]])))

(defn store-to-manager-data
  "Given an immutable store, create the map that we store in the manager data,
  which adds a history to the store."
  [immutable-store]
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
   :future nil})

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
     (fn [{:keys [store history] :as state}]
       (let [[updated-store result] (update-fn store)
             [new-store modified-ids] (fetch-and-clear-modified-ids
                                       updated-store)]
         [(if (not= new-store store)
            {:store new-store
             :history (cons [modified-ids store] history)
             :future nil}
            state)
          (keys-affected-by-ids modified-ids store new-store)
          result]))))

  (revise-update-control-return! [this expected-current update-fn]
    (describe-and-swap-control-return!
     (:manager-data this)
     (fn [state]
       (if (not= expected-current (:store state))
         [state nil]
         (let [{:keys [store history future]} state]
           (if (empty? history)
             [state []]
             (let [[[prev-modified-ids prev-store] & prev-history] history
                   [updated-store result] (update-fn prev-store)
                   [new-store new-modified-ids] (fetch-and-clear-modified-ids
                                                 updated-store)
                   all-modified-ids (seq (clojure.set/union
                                          (set prev-modified-ids)
                                          (set new-modified-ids)))]
               [(if (not= new-store store)
                  {:store new-store
                   :history (cons [new-modified-ids prev-store] prev-history)
                   :future nil}
                  state)
                (keys-affected-by-ids all-modified-ids store new-store)
                result])))))))

  (add-simple-element! [this subject content]
    (do-update-control-return! this #(add-simple-element % subject content)))

  (remove-simple-id! [this id]
    (do-update! this #(remove-simple-id % id)))

  (update-content! [this id content]
    (do-update! this #(update-content % id content)))

  (declare-transient-id! [this id]
    (do-update! this #(declare-transient-id % id)))

  (can-undo? [this]
    (not (empty? (:history (:value @(:manager-data this))))))

  (undo! [this]
    (describe-and-swap!
     (:manager-data this)
     (fn [{:keys [store history future] :as state}]
       (if (empty? history)
         [state []]
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
     (fn [{:keys [store history future] :as state}]
       (if (empty? future)
         [state []]
         (let [[[modified-ids next-store] & remaining-future] future]
           [{:store next-store
             :history (cons [modified-ids store] history)
             :future remaining-future}
            (keys-affected-by-ids modified-ids next-store store)]))))))

(defmethod print-method MutableStoreImpl [s ^java.io.Writer w]
  (.write w "Mutable:")
  (print-method (dissoc (:value @(:manager-data s)) :history) w))

(defmethod new-mutable-store true [immutable-store]
  (map->MutableStoreImpl
   {:manager-data (new-mutable-manager-data
                   (store-to-manager-data immutable-store))}))

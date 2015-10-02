(ns cosheet.mutable-store-impl
  (:require (cosheet [store :refer :all]
                     [reporters :as reporter
                      :refer [set-value! set-manager!
                              attended? new-reporter invalid]]
                     [utils :refer [call-with-latest-value
                                    update-in-clean-up
                                    swap-control-return!]])))

(defn inform-reporter
  "Make sure the reporter has the latest value from the store."
  [immutable-store reporter]
  (let [[f & args] (:fetch (reporter/data reporter))]
    (set-value! reporter (apply f immutable-store args))))

(defn manager-callback
  "Callback when a reporter starts or stops needing updates from the store.
   If id is non-nil, then only changes to that item can affect the value."
  [reporter store id]
  (call-with-latest-value
   #(attended? reporter)
   (fn [attended]
     (let [fetch (:fetch (reporter/data reporter))]
       (if attended
         (do
           (swap! (:data store)
                  (fn [data]
                    (-> data
                        (update-in [:subscriptions id]
                                   #((fnil conj #{}) % reporter))
                        (update-in [:fetch->attended-reporter fetch]
                                   ;; Due to races, we might already
                                   ;; have another reporter for this
                                   ;; expression, in which case, leave it.
                                   (fn [existing] (or existing reporter))))))
           (inform-reporter (:state @(:data store)) reporter))
         (do
           (set-value! reporter invalid)
           (swap! (:data store)
                  (fn [data]
                    (-> data
                        (update-in-clean-up
                         [:subscriptions id] #(disj % reporter))
                        (update-in-clean-up
                         [:fetch->attended-reporter fetch]
                         ;; Due to races, we might already have
                         ;; another reporter for this expression, in
                         ;; which case, leave it.
                         (fn [existing]
                           (if (= existing reporter) nil existing))))))))))))

(defn get-or-make-reporter
  "Retrieve or create a reporter that gives the value of the function
   applied to the current state of the store and the arguments. If id
   is non-nil, the result must depend only on id and its elements."
  [id fn store & args]
  (let [fetch (cons fn args)]
    (or (get-in @(:data store) [:fetch->attended-reporter fetch])
        (new-reporter :manager [manager-callback store id]
                      :fetch fetch))))

(defn inform-reporters-for-key
  "Update all reporters that depend on the given key."
  [immutable-store subscriptions key]
  (doseq [reporter (get subscriptions key)]
    (inform-reporter immutable-store reporter)))

(defn inform-reporters
  "Update all reporters that care about the changes."
  [new-store subscriptions keys]
  (doseq [key keys]
    (inform-reporters-for-key new-store subscriptions key)))

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

(defrecord MutableStoreImpl
    ^{:doc
      "A store that contains an immutable store,
       supports mutation to that store,
       and returns reporter objects for queries."}
  [;; A map holding the reporter's data, consisting of
   data
   
   ;; An atom holding the current immutable state
   ;; :state

   ;; A mutable map from item id to a set of reporters that need
   ;; to be checked on any change to that item or its elements.
   ;; Reporters indexed under a nil id must be checked for all changes.
   ;; :subscriptions

   ;; A mutable map from fetch expression to an attended reporter with that
   ;; fetch, if there is one.
   ;; :fetch->attended-reporter
   ]

  Store

  (id-valid? [this id]
    (get-or-make-reporter id id-valid? this id))

  (id-label->element-ids [this id label]
    (get-or-make-reporter id id-label->element-ids this id label))

  (id->element-ids [this id]
    (get-or-make-reporter id id->element-ids this id))

  (id->content [this id]
    (get-or-make-reporter id id->content this id))

  (id->content-reference [this id]
    (get-or-make-reporter id id->content-reference this id))

  (candidate-matching-ids [this item]
    (get-or-make-reporter nil candidate-matching-ids this item))

  (mutable-store? [this] true)
  
  MutableStore

  (current-store [this] (:state @(:data this)))

  (do-update! [this update-fn]
    (do-update-control-return! this (fn [store] [(update-fn store) nil])))

  (do-update-control-return! [this update-fn]
    (let [[modified-ids subscriptions old-store new-store result]
          (swap-control-return!
           (:data this)
           (fn [data]
             (let [old-store (:state data)
                   subscriptions (:subscriptions data)
                   [updated-store result] (update-fn old-store)
                   [new-store modified-ids] (fetch-and-clear-modified-ids
                                             updated-store)]
               [(assoc data :state new-store)
                [modified-ids subscriptions old-store new-store result]])))]
      (when (not= (count modified-ids) 0)
        (inform-reporters
         new-store subscriptions
         (keys-affected-by-ids modified-ids old-store new-store)))
      result))

  (add-simple-element! [this subject content]
    (do-update-control-return! this #(add-simple-element % subject content)))

  (remove-simple-id! [this id]
    (do-update! this #(remove-simple-id % id)))

  (update-content! [this id content]
    (do-update! this #(update-content % id content))))

(defmethod print-method MutableStoreImpl [s ^java.io.Writer w]
  (.write w "Mutable:")
  (print-method (:state @(:data s)) w))

(defmethod new-mutable-store true [immutable-store]
  (map->MutableStoreImpl
   {:data (atom {:state (track-modified-ids immutable-store)
                 :subscriptions {}
                 :fetch->attended-reporter {}})}))

(ns cosheet.mutable-store-impl
  (:require (cosheet [store :refer :all]
                     [reporter :as reporter
                      :refer [set-value! set-manager!
                              attended? new-reporter invalid]]
                     [mutable-map :as mm]
                     [utils :refer [call-with-latest-value
                                    swap-control-return!]])))

(defn update-reporter
  "Make sure the reporter has the latest value from the store."
  [store reporter]
  (let [[f & args] (:fetch (reporter/data reporter))]
    (set-value! reporter (apply f @(:store store) args))))

(defn manager-callback
  "Callback when a reporter starts or stops needing updates from the store.
   If id is non-nil, then only changes to that item can affect the value."
  [reporter store id]
  (call-with-latest-value
   #(attended? reporter)
   (fn [attended]
     (let [subscriptions (:subscriptions store)
           fetch (:fetch (reporter/data reporter))]
       (if attended
         (do
           (mm/update! subscriptions id #((fnil conj #{}) % reporter))
           (mm/update!
            (:fetch->attended-reporter store) fetch
            ;; Due to races, we might already have another
            ;; reporter for this expression, in which case, leave it.
            (fn [existing-reporter] (or existing-reporter reporter)))
           (update-reporter store reporter))
         (do
           (set-value! reporter invalid)
           (mm/update-in-clean-up! subscriptions [id] #(disj % reporter))
           (mm/update-in-clean-up!
            (:fetch->attended-reporter store) [fetch]
            ;; Due to races, we might already have another
            ;; reporter for this expression, in which case, leave it.
            (fn [existing-reporter]
              (if (= existing-reporter reporter) nil existing-reporter)))))))))

(defn get-or-make-reporter
  "Retrieve or create a reporter that gives the value of the function
   applied to the current state of the store and the arguments. If id
   is non-nil, the result must depend only on id and its elements."
  [id fn store & args]
  (let [fetch (cons fn args)]
    (or (mm/get! (:fetch->attended-reporter store) fetch)
        (new-reporter :manager [manager-callback store id]
                      :fetch fetch))))

(defn update-reporters-for-key
  "Update all reporters that depend on the given key."
  [store key]
  (doseq [reporter (mm/get! (:subscriptions store) key)]
    (update-reporter store reporter)))

(defn update-reporters-for-id
  "Update all reporters that could be affected by the item changing."
  [store id]
  (update-reporters-for-key store nil) ; The ones we have to check always.
  (loop [item id]
    (when (not (nil? item))
      (update-reporters-for-key store item)
      (recur (id->subject @(:store store) item)))))

(defrecord MutableStoreImpl
    ^{:doc
      "A store that contains an immutable store,
       supports mutation to that store,
       and returns reporter objects for queries."}
  [
   ;; An atom holding the current immutable state
   store

   ;; A mutable map from item id to a set of reporters that need
   ;; to be checked on any change to that item or its elements.
   ;; Reporters indexed under a nil id must be checked for all changes.
   subscriptions

   ;; A mutable map from fetch expression to a attended reporter with that
   ;; fetch, if there is one.
   fetch->attended-reporter]

  Store

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

  (current-store [this] @(:store this))

  (add-simple-element! [this subject content]
    (let [element
          (swap-control-return! (:store this)
                                #(add-simple-element % subject content))]
      (update-reporters-for-id this subject)
      element))

  (remove-simple-id! [this id]
    (let [subject (id->subject @(:store this) id)]
      (swap! (:store this) #(remove-simple-id % id))
      (update-reporters-for-id this subject)))

  (update-content! [this id content]
    (swap! (:store this) #(update-content % id content))
    (update-reporters-for-id this id)))

(defmethod print-method MutableStoreImpl [s ^java.io.Writer w]
  (.write w "Mutable:")
  (print-method (:store s) w))

(defmethod new-mutable-store true [store]
  (map->MutableStoreImpl {:store (atom store)
                          :subscriptions (mm/new-mutable-map)
                          :fetch->attended-reporter (mm/new-mutable-map)}))

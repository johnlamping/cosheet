(ns cosheet.mutable-store-impl
  (:require (cosheet [store :refer :all]
                     [state :refer :all]
                     [mutable-map :as mm]
                     [utils :refer [call-with-latest-value
                                    swap-control-return!]])))

(defn update-state
  "Make sure the state has the latest value from the store."
  [store state]
  (let [[f & args] (:expression state)]
    (state-update state (fn [old] (apply f @(:store store) args)))))

(defn subscription-callback
  "Callback when a state starts or stops needing updates from the store.
   If id is non-nil, then only changes to that item can affect the state."
  [subscribed state store id]
  (let [subscriptions (:subscriptions store)]
    (if subscribed
      (do
        (mm/update! subscriptions id #((fnil conj #{}) % state))
        (mm/update!
         (:expression->subscribed-state store) (:expression state)
         ;; Due to races, we might already have another
         ;; state for this expression, in which case, leave it.
         (fn [existing-state] (or existing-state state)))
        (update-state store state))
      (do
        (mm/update-in-clean-up! subscriptions [id] #(disj % state))
        (mm/update-in-clean-up!
         (:expression->subscribed-state store) [(:expression state)]
         ;; Due to races, we might already have another
         ;; state for this expression, in which case, leave it.
         (fn [existing-state]
           (if (= existing-state state) nil existing-state)))))))

(defn get-or-make-state
  "Retrieve or create a state that gives the value of the function
   applied to the current state of the store and the arguments. If id
   is non-nil, the result must depend only on id and its elements."
  [id fn store & args]
  (let [expression (cons fn args)]
    (or (mm/get! (:expression->subscribed-state store) expression)
        ;; TODO: We update the state twice, once here, and once
        ;; when a subscription is made. Instead, don't evaluate here,
        ;; but have a small cache of temporary subscriptions that we
        ;; make, which will cause evaluation, and will keep the value
        ;; up to date until an external subscription is made.
        (let [state (new-state :value nil
                               :callback [subscription-callback store id]
                               :additional {:expression expression})]
          (update-state store state)
          state))))

(defn update-states-for-key
  "Update all states that depend on the given key."
  [store key]
  (doseq [state (mm/get! (:subscriptions store) key)]
    (update-state store state)))

(defn update-states-for-id
  "Update all states that could be affected by the item changing."
  [store id]
  (update-states-for-key store nil) ; The ones we have to check always.
  (loop [item id]
    (when (not (nil? item))
      (update-states-for-key store item)
      (recur (id->subject @(:store store) item)))))

(defrecord MutableStoreImpl
    ^{:doc
      "A store that contains an immutable store,
       supports mutation to that store, and returns state objects for queries"}
  [
   ;; An atom holding the current immutable state
   store

   ;; A mutable map from item id to a set of states that need
   ;; to be checked on any change to that item or its elements.
   ;; States under a nil id must be checked for all changes.
   subscriptions

   ;; A mutable map from expression to a subscribed state with that
   ;; expression, if there is one.
   expression->subscribed-state]

  Store

  (id-label->element-ids [this id label]
    (get-or-make-state id id-label->element-ids this id label))

  (id->element-ids [this id]
    (get-or-make-state id id->element-ids this id))

  (id->content [this id]
    (get-or-make-state id id->content this id))

  (id->content-reference [this id]
    (get-or-make-state id id->content-reference this id))

  (candidate-matching-ids [this item]
    (get-or-make-state nil candidate-matching-ids this item))

  (mutable-store? [this] true)
  
  MutableStore

  (current-store [this] @(:store this))

  (add-simple-element! [this subject content]
    (let [element
          (swap-control-return! (:store this)
                                #(add-simple-element % subject content))]
      (update-states-for-id this subject)
      element))

  (remove-simple-id! [this id]
    (let [subject (id->subject @(:store this) id)]
      (swap! (:store this) #(remove-simple-id % id))
      (update-states-for-id this subject)))

  (update-content! [this id content]
    (swap! (:store this) #(update-content % id content))
    (update-states-for-id id)))

(defmethod print-method MutableStoreImpl [s ^java.io.Writer w]
  (.write w (:store s)))

(defmethod new-mutable-store true [store]
  (map->MutableStoreImpl {:store (atom store)
                          :subscriptions (mm/new-mutable-map)
                          :expression->subscribed-state (mm/new-mutable-map)}))

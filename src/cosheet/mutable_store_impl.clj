(ns cosheet.mutable-store-impl
  (:require (cosheet [store :refer :all]
                     [mutable-manager
                      :refer [new-mutable-management
                              get-or-make-reporter
                              describe-and-swap-control-return!]]
                     [reporters :as reporter
                      :refer [set-value! set-manager!
                              attended? new-reporter invalid]]
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

(defrecord MutableStoreImpl
    ^{:doc
      "A store that contains an immutable store,
       supports mutation to that store,
       and returns reporter objects for queries."}
  [management ;; A mutable-manager managemente holding the immutable
              ;; store as its value. 
   ]

  Store

  (id-valid? [this id]
    (get-or-make-reporter [id] id-valid? (:management this) id))

  (id-label->element-ids [this id label]
    (get-or-make-reporter
     [id] id-label->element-ids (:management this) id label))

  (id->element-ids [this id]
    (get-or-make-reporter [id] id->element-ids (:management this) id))

  (id->content [this id]
    (get-or-make-reporter [id] id->content (:management this) id))

  (id->content-reference [this id]
    (get-or-make-reporter [id] id->content-reference (:management this) id))

  (candidate-matching-ids [this item]
    (get-or-make-reporter [nil] candidate-matching-ids (:management this) item))

  (mutable-store? [this] true)
  
  MutableStore

  (current-store [this] (:value @(:management this)))

  (do-update! [this update-fn]
    (do-update-control-return! this (fn [store] [(update-fn store) nil])))

  (do-update-control-return! [this update-fn]
    (describe-and-swap-control-return!
     (:management this)
     (fn [old-store]
       (let [[updated-store result] (update-fn old-store)
             [new-store modified-ids] (fetch-and-clear-modified-ids
                                       updated-store)]
         [new-store
          (keys-affected-by-ids modified-ids old-store new-store)
          result]))))

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
   {:management (new-mutable-management (track-modified-ids immutable-store))}))

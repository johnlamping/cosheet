(ns cosheet.entity-impl
  (:require (cosheet [store :refer [target-label->ids source-label->ids
                                     target->ids source->ids
                                     target-source->ids
                                     id->source id->target
                                     id-endpoint->value
                                     item-id? link-id? object-id?
                                     mutable-store?
                                     current-store
                                     Store]]
                      [canonical :refer [equivalent-primitives?]]
                      [entity :refer :all]
                      [reporter-macros :refer [seq-R let-R]]
                      [category-change-calculator :refer [category-change-R]]
                      orderable)))

(defn endpoint->entity
  "Given any value of an endpoint from a store, the store, and an
  optional direction, :forward or :reverse, return the described
  entity. The direction is ignored for anything but link values."
  ([value store]
   (endpoint->entity value store :forward))
  ([value store direction]
   (if (item-id? value)
     (if (object-id? value)
       (id->object value store)
       (id->element value direction store))
     ;; The value must be a primitive.
     value)))

(defrecord
    ^{:doc
      "An entity with an id that can be put into a store, but that has no
      store associated with it. These can be used to create list forms
      of entities that have known ids based on strings, and can still
      added to stores. That is useful for both building the initial
      store and for unit tests."}
    IdOnlyEntity

    [item-id] ; The DirectedItemId holding the item in the store.

  StoredEntity

  (target-entity [this] nil)
  (subject-entity [this] nil)
  (containing-elements [this] nil)

  (in-different-store [this store-or-entity]
    (id->entity-m item-id
                  (if (satisfies? Store store-or-entity)
                    store-or-entity
                    (:store store-or-entity))))

  Entity

  (mutable-entity? [this] false)
  (primitive? [this] false)
  (element? [this] (link-id? (store-item-id item-id)))
  (object? [this] (object-id? (store-item-id item-id)))
  (content [this] nil)
  (forward-elements [this] nil)
  (reverse-elements [this] nil)
  (all-elements [this] nil)
  (reverse? [this] (reverse-id? item-id))
  (content->elements [this content-value] nil)
  (label->elements [this label] nil)
  (entity-key [this] (store-item-id item-id))
  (updating-immutable [this] this))

(defrecord
    ^{:doc "An entity whose elements are described by a store."}
    ImmutableStoredEntity

    [store        ; The immutable store that holds the information
                  ; for this item.
     item-id]     ; The DirectedItemId holding the item in the store.

  StoredEntity

  (target-entity [this]
    (let [underlying-item-id (store-item-id item-id)]
      (when (not (object-id? underlying-item-id))
        (endpoint->entity (id->target store underlying-item-id) store))))

  (subject-entity [this]
    (let [underlying-item-id (store-item-id item-id)]
      (when (not (object-id? underlying-item-id))
        (endpoint->entity (id-endpoint->value
                           store underlying-item-id (subject-endpoint this))
                          store))))

  (containing-elements [this]
    (let [underlying-item-id (store-item-id item-id)]
      (assert (object-id? underlying-item-id) this)
      (map #(endpoint->entity % store)
           (source->ids store underlying-item-id))))
  
  (in-different-store [this store-or-entity]
    (let [new-store (if (satisfies? Store store-or-entity)
                      store-or-entity
                      (:store store-or-entity))]
      (when (and (nil? new-store) (object? this))
        (assert (uniquely-identified-object? this) this))
      (id->entity-m item-id new-store)))
  
  Entity

  (mutable-entity? [this] false)

  (primitive? [this] false)
  (element? [this] (link-id? (store-item-id item-id)))
  (object? [this] (object-id? (store-item-id item-id)))

  (content [this]
    (let [underlying-item-id (store-item-id item-id)]
      (when (not (object-id? underlying-item-id))
        (endpoint->entity (id-endpoint->value
                           store underlying-item-id (content-endpoint this))
                          store))))

  (forward-elements [this]
    (seq (->> (target->ids store (store-item-id item-id))
              (map #(id->element % store)))))

  (reverse-elements [this]
    (let [underlying-item-id (store-item-id item-id)]
      (when (object-id? underlying-item-id)
        (seq (->> (source->ids store underlying-item-id)
                  (filter #(object-id? (id->target store %)))
                  (map #(id->element % :reverse store)))))))

  (all-elements [this]
    (let [underlying-item-id (store-item-id item-id)]
      (seq (concat (forward-elements this)
                   (remove #(= (entity-key (content %)) underlying-item-id)
                           (reverse-elements this))))))
  
  (reverse? [this] (reverse-id? item-id))

  (content->elements [this content-value]
    (let [underlying-item-id (store-item-id item-id)
          content-key (entity-key content-value)]
      (seq (cond-> (->> (target-source->ids
                         store underlying-item-id content-key)
                        (map #(id->element % store)))
             (and (object-id? underlying-item-id) (object-id? content-key))
             (concat
              (->> (target-source->ids
                    store content-key underlying-item-id)
                   (map #(id->element % :reverse store))))))))

  (label->elements [this label]
    (let [underlying-item-id (store-item-id item-id)
          label-key (entity-key label)]
      (seq (cond-> (->> (target-label->ids
                         store underlying-item-id label-key)
                        (map #(id->element % store)))
             (object-id? underlying-item-id)
             (concat (->> (source-label->ids
                           store underlying-item-id label-key)
                          (filter #(object-id? (id->target store %)))
                          (map #(id->element % :reverse store))))))))

  (entity-key [this]
    (store-item-id item-id))

  (updating-immutable [this] this))

(defrecord
    ^{:doc "An item whose elements are described by a mutable store."}
    MutableStoredEntity

    [store        ; The mutable store that holds the information for this item.
     item-id]     ; The DirectedItemId holding the item in the store.


  StoredEntity

  (target-entity [this]
    (let [underlying-item-id (store-item-id item-id)]
      (when (not (object-id? underlying-item-id))
        (let-R [target-id (id->target store underlying-item-id)]
          (endpoint->entity target-id store)))))

  (subject-entity [this]
    (let [underlying-item-id (store-item-id item-id)]
      (when (not (object-id? underlying-item-id))
        (let-R [subject-id (id-endpoint->value
                            store underlying-item-id (subject-endpoint this))]
          (endpoint->entity subject-id store)))))

  (containing-elements [this]
    (let [underlying-item-id (store-item-id item-id)]
      (assert (object-id? underlying-item-id) this)
      (let-R [link-ids (source->ids store underlying-item-id)]
        (map #(endpoint->entity % store)
             link-ids))))

  (in-different-store [this store-or-entity]
    (let [new-store (if (satisfies? Store store-or-entity)
                      store-or-entity
                      (:store store-or-entity))]
      (when (and (nil? new-store) (object? this))
        (assert (uniquely-identified-object?
                 (in-different-store this (current-store store)))
                this))
      (id->entity-m item-id new-store)))

  Entity

  (mutable-entity? [this] true)

  (primitive? [this?] false)
  (element? [this] (link-id? (store-item-id item-id)))
  (object? [this] (object-id? (store-item-id item-id)))

  (content [this]
    (let [underlying-item-id (store-item-id item-id)]
      (when (not (object-id? underlying-item-id))
        (let-R [content (id-endpoint->value
                         store underlying-item-id (content-endpoint this))]
          (endpoint->entity content store (entity-direction this))))))

  (forward-elements [this]
    (let-R [element-ids (target->ids store (store-item-id item-id))]
      (seq (for [element-id element-ids]
             (id->element element-id store)))))

  (reverse-elements [this]
    (let [underlying-item-id (store-item-id item-id)]
      (when (object-id? underlying-item-id)
        (let-R [element-ids (source->ids store underlying-item-id)
                targets (seq-R (map #(id->target store %) element-ids))]
          (seq (->> (map vector element-ids targets)
                    (keep (fn [[element-id target]]
                            (when (object-id? target)
                              element-id)))
                    (map #(id->element % :reverse store))))))))

  (all-elements [this]
    (let [underlying-item-id (store-item-id item-id)]
      (let-R [forward-elements (forward-elements this)]
        (if (object-id? underlying-item-id)
          (let-R [reverse-elements (reverse-elements this)
                  contents (seq-R (map content reverse-elements))]
            (seq (concat forward-elements
                         (keep (fn [[element element-content]]
                                 (when (not= (entity-key element-content)
                                             underlying-item-id)
                                   element))
                               (map vector reverse-elements contents)))))
          forward-elements))))
  (reverse? [this] (reverse-id? item-id))

  (content->elements [this content-value]
    (let [underlying-item-id (store-item-id item-id)
          content-key (entity-key content-value)]
      (let-R [element-ids (target-source->ids
                          store underlying-item-id content-key)
              reverse-element-ids (when (and (object-id?
                                              underlying-item-id)
                                            (object-id? content-key))
                                    (target-source->ids
                                     store content-key
                                     underlying-item-id))]
        (seq (concat (for [element-id element-ids]
                       (id->element element-id store))
                     (for [element-id reverse-element-ids]
                       (id->element element-id :reverse store)))))))

  (label->elements [this label]
    (let [underlying-item-id (store-item-id item-id)
          label-key (entity-key label)]
      (let-R [element-ids (target-label->ids
                           store underlying-item-id label-key)]
        (let [forward-elements (seq (for [element-id element-ids]
                                      (id->element element-id store)))]
          (if (object-id? underlying-item-id)
            (let-R [element-ids (source-label->ids
                                 store underlying-item-id label-key)
                    targets (seq-R (map #(id->target store %) element-ids))]
              (seq (concat
                   forward-elements
                   (->> (map vector element-ids targets)
                        (keep (fn [[element-id target]]
                                (when (object-id? target)
                                  element-id)))
                        (map #(id->element % :reverse store))))))
            forward-elements)))))

  (entity-key [this]
    (store-item-id item-id))

  (updating-immutable [this]
    (let-R [immutable-store (category-change-R [(store-item-id item-id)] store)]
      (in-different-store this immutable-store))))

(defn- equivalent-entities?
  "Return whether the entities are equivalent primitives, or are objects
  with the same key."
  [e1 e2]
  (if (primitive? e1)
    (equivalent-primitives? e1 e2)
    (and (object? e1)
         (= (entity-key e1) (entity-key e2)))))

;;; Make a list work as an element. The format is either
;;;  ((direction content) element element...)
;;;  (content element element...)
;;;
;;; We extend ISeq, because, for example, while '(1 2) is a
;;; PersistentList, `(1 2) is a Cons, and ISeq subsumes both, without also
;;; picking up vectors.
(extend-type clojure.lang.ISeq
  
  Entity

  (mutable-entity? [this] false)

  (primitive? [this] false)
  (element? [this] true)
  (object? [this] false)

  (content [this]
    (let [f (first this)]
      (if (and (seq? f) (#{:forward :reverse} (first f)))
        (second f)
        f)))

  (forward-elements [this] (seq (filter #(not (reverse? %))
                                        (all-elements this))))

  (reverse-elements [this] nil)

  (all-elements [this] (seq (rest this)))

  (reverse? [this]
    (let [f (first this)]
      (and (seq? f) (= (first f) :reverse))))

  (content->elements [this content-value]
    (seq (filter #(equivalent-entities? content-value (content %))
                 (forward-elements this))))

  (label->elements [this label]
    (seq (filter (fn [element]
                   (some #(and (label-element? %)
                               (equivalent-entities? label (content %)))
                         (forward-elements element)))
                 (forward-elements this))))

  (entity-key [this]
    this)

  (updating-immutable [this] this))

;;; Make a vector work as an object or a link
;;; For an object, the format is
;;;   [:object element element ...]
;;; Or, for an object whose identity should be recognizable across
;;; multiple references:
;;;   [:conflux-object identifier element element ...]
(extend-type clojure.lang.PersistentVector

  Entity

  (mutable-entity? [this] false)

  (primitive? [this] false)
  (element? [this] false)
  (object? [this] (or (= (first this) :object)
                      (= (first this) :conflux-object)))

  (content [this] nil)

  (forward-elements [this] (seq (filter #(not (reverse? %))
                                        (all-elements this))))

  (reverse-elements [this] (when (object? this)
                             (seq (filter reverse?
                                          (all-elements this)))))

  (all-elements [this]
    (assert (object? this) this)
    (seq (case (first this)
           :object (rest this)
           :conflux-object (nthrest this 2))))
  (reverse? [this] false)

  (content->elements [this content-value]
    (seq (filter #(equivalent-entities? content-value (content %))
                 (all-elements this))))

  (label->elements [this label]
    (seq (filter (fn [element]
                   (some #(and (label-element? %)
                               (equivalent-entities? label (content %)))
                         (forward-elements element)))
                 (all-elements this))))


  (entity-key [this]
    (case (first this)
      :conflux-object (second this)
      this))

  (updating-immutable [this] this))

(extend-protocol Entity
  clojure.lang.Keyword
  (mutable-entity? [this] false)
  (primitive? [this] true)
  (element? [this] false)
  (object? [this] false)
  (content [this] this)
  (forward-elements [this] nil)
  (reverse-elements [this] nil)
  (all-elements [this] nil)
  (reverse? [this] false)
  (content->elements [this content-value] nil)
  (label->elements [this label] nil)
  (entity-key [this] this)
  (updating-immutable [this] this)
  
  clojure.lang.Symbol
  (mutable-entity? [this] false)
  (primitive? [this] true)
  (element? [this] false)
  (object? [this] false)
  (content [this] this)
  (forward-elements [this] nil)
  (reverse-elements [this] nil)
  (all-elements [this] nil)
  (reverse? [this] false)
  (content->elements [this content-value] nil)
  (label->elements [this label] nil)
  (entity-key [this] this)
  (updating-immutable [this] this)
  
  java.lang.String
  (mutable-entity? [this] false)
  (primitive? [this] true)
  (element? [this] false)
  (object? [this] false)
  (content [this] this)
  (forward-elements [this] nil)
  (reverse-elements [this] nil)
  (all-elements [this] nil)
  (reverse? [this] false)
  (content->elements [this content-value] nil)
  (label->elements [this label] nil)
  (entity-key [this] this)
  (updating-immutable [this] this)
  
  java.lang.Number
  (mutable-entity? [this] false)
  (primitive? [this] true)
  (element? [this] false)
  (object? [this] false)
  (content [this] this)
  (forward-elements [this] nil)
  (reverse-elements [this] nil)
  (all-elements [this] nil)
  (reverse? [this] false)
  (content->elements [this content-value] nil)
  (label->elements [this label] nil)
  (entity-key [this] this)
  (updating-immutable [this] this)
  
  java.lang.Boolean
  (mutable-entity? [this] false)
  (primitive? [this] true)
  (element? [this] false)
  (object? [this] false)
  (content [this] this)
  (forward-elements [this] nil)
  (reverse-elements [this] nil)
  (all-elements [this] nil)
  (reverse? [this] false)
  (content->elements [this content-value] nil)
  (label->elements [this label] nil)
  (entity-key [this] this)
  (updating-immutable [this] this)
  
  cosheet.orderable.Orderable
  (mutable-entity? [this] false)
  (primitive? [this] true)
  (element? [this] false)
  (object? [this] false)
  (content [this] this)
  (forward-elements [this] nil)
  (reverse-elements [this] nil)
  (all-elements [this] nil)
  (reverse? [this] false)
  (content->elements [this content-value] nil)
  (label->elements [this label] nil)
  (entity-key [this] this)
  (updating-immutable [this] this)

  nil ;; For convenience in null punning
  (mutable-entity? [this] false)
  (primitive? [this] true)
  (element? [this] false)
  (object? [this] false)
  (content [this] this)
  (forward-elements [this] nil)
  (reverse-elements [this] nil)
  (all-elements [this] nil)
  (reverse? [this] false)
  (content->elements [this content-value] nil)
  (label->elements [this label] nil)
  (entity-key [this] this)
  (updating-immutable [this] this))

(defn- make-stored-entity
  "Make the kind of stored entity appropriate to the store, holding the
  given directed id."
  [directed-id store]
  (cond
    (nil? store) (->IdOnlyEntity directed-id)
    (mutable-store? store) (->MutableStoredEntity store directed-id)
    true (->ImmutableStoredEntity store directed-id)))

(extend-protocol ToStoredEntity
  cosheet.store.ItemId
  (id->entity-m
    ([this store]
     (make-stored-entity this store))
    ([this direction store]
     (assert (or (nil? direction) (#{:forward :reverse} direction)))
     (make-stored-entity
      (cond-> this (= direction :reverse) flip-id-direction)
      store)))
  cosheet.entity.ReverseItemId
  (id->entity-m
    ([this store]
     (make-stored-entity this store))))

;;; Now that id->object has been implemented, we go back to the entity
;;; namespace and provide the definitions of these constants.
(in-ns 'cosheet.entity)
(def name-label (id->object name-label-id nil))
(def link-type (id->object link-type-id nil))
(def object-type (id->object object-type-id nil))

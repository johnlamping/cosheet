(ns cosheet.entity-impl
  (:require (cosheet [store :refer [target-label->ids source-label->ids
                                     target->ids source->ids
                                     target-source->ids
                                     id->source id->target
                                     item-id? link-id? object-id?
                                     mutable-store?
                                     current-store
                                     Store]]
                      [canonical :refer [equivalent-primitives?]]
                      [entity :refer :all]
                      [reporter-macros :refer [seq-R let-R]]
                      [category-change-calculator :refer [category-change-R]])))

(defn endpoint->entity
  "Given any value of an endpoint, the store, and an optional
  orientation, return the described entity.
  The orientation is ignored for anything but link values."
  ([value store]
   (endpoint->entity value store :source))
  ([value store orientation]
   (if (item-id? value)
     (if (object-id? value)
       (id->object value store)
       (id->element value orientation store))
     value)))

(defrecord
    ^{:doc
      "An entity with an id that can be put into a store, but that has no
      store associated with it. These can be used to create list forms
      of entities that have known ids based on strings, and can still
      added to stores. That is useful for both building the initial
      store and for unit tests."}
    IdOnlyEntity

    [item-id      ; The ItemId of the item in the store.
     orientation] ; The endpoint that holds an element entity's content.

  StoredEntity

  (target-entity [this] nil)
  (originating-entity [this] nil)
  (containing-elements [this] nil)

  (in-different-store [this store-or-entity]
    (id->entity-m item-id
                  orientation
                  (if (satisfies? Store store-or-entity)
                    store-or-entity
                    (:store store-or-entity))))

  Entity

  (mutable-entity? [this] false)
  (primitive? [this] false)
  (element? [this] (link-id? item-id))
  (object? [this] (object-id? item-id))
  (content [this] nil)
  (elements [this] nil)
  (forward-elements [this] nil)
  (orientation [this] orientation)
  (content->elements [this content-value] nil)
  (label->elements [this label] nil)
  (entity-key [this] item-id)
  (updating-immutable [this] this))

(defrecord
    ^{:doc "An entity whose elements are described by a store."}
    ImmutableStoredEntity

    [store        ; The immutable store that holds the information for this item.
     item-id      ; The ItemId of the item in the store.
     orientation] ; The endpoint that holds an element entity's content.

  StoredEntity

  (target-entity [this]
    (when (not (object-id? item-id))
      (endpoint->entity (id->target store item-id) store)))

  (originating-entity [this]
    (when (not (object-id? item-id))
      (endpoint->entity (if (= orientation :target)
                          (id->source store item-id)
                          (id->target store item-id))
                        store)))

  (containing-elements [this]
    (assert (object-id? item-id) this)
    (map #(endpoint->entity % store)
         (source->ids store item-id)))
  
  (in-different-store [this store-or-entity]
    (let [new-store (if (satisfies? Store store-or-entity)
                      store-or-entity
                      (:store store-or-entity))]
      (when (and (nil? new-store) (object? this))
        (assert (uniquely-identified-object? this) this))
      (id->entity-m item-id orientation new-store)))
  
  Entity

  (mutable-entity? [this] false)

  (primitive? [this] false)
  (element? [this] (link-id? item-id))
  (object? [this] (object-id? item-id))

  (content [this]
    (when (not (object-id? item-id))
      (if (= orientation :target)
        (endpoint->entity (id->target store item-id) store :target)
        (endpoint->entity (id->source store item-id) store))))

  (elements [this]
    (seq (cond->> (forward-elements this)
           (object-id? item-id)
           (concat (->> (source->ids store item-id)
                        (filter #(object-id? (id->target store %)))
                        (map #(id->element % :target store)))))))

  (forward-elements [this]
    (seq (->> (target->ids store item-id)
              (map #(id->element % store)))))

  (orientation [this]
    orientation)

  (content->elements [this content-value]
    (let [content-key (entity-key content-value)]
      (seq (cond-> (->> (target-source->ids store item-id content-key)
                        (map #(id->element % store)))
             (and (object-id? item-id) (object-id? content-key))
             (concat
              (->> (target-source->ids store content-key item-id)
                   (map #(id->element % :target store))))))))

  (label->elements [this label]
    (let [label-key (entity-key label)]
      (seq (cond-> (->> (target-label->ids store item-id label-key)
                        (map #(id->element % store)))
             (object-id? item-id)
             (concat (->> (source-label->ids store item-id label-key)
                          (filter #(object-id? (id->target store %)))
                          (map #(id->element % :target store))))))))

  (entity-key [this]
    item-id)

  (updating-immutable [this] this))

(defrecord
    ^{:doc "An item whose elements are described by a mutable store."}
    MutableStoredEntity

    [store        ; The mutable store that holds the information for this item.
     item-id      ; The ItemId of the item in the store.
     orientation] ; The endpoint that holds an element entity's content.


  StoredEntity

  (target-entity [this]
    (when (not (object-id? item-id))
      (let-R [target-id (id->target store item-id)]
        (endpoint->entity target-id store))))

  (originating-entity [this]
    (when (not (object-id? item-id))
      (if (= orientation :target)
        (let-R [source-id (id->source store item-id)]
          (endpoint->entity source-id store))
        (let-R [target-id (id->target store item-id)]
          (endpoint->entity target-id store)))))

  (containing-elements [this]
    (assert (object-id? item-id) this)
    (let-R [link-ids (source->ids store item-id)]
      (map #(endpoint->entity % store)
           link-ids)))

  (in-different-store [this store-or-entity]
    (let [new-store (if (satisfies? Store store-or-entity)
                      store-or-entity
                      (:store store-or-entity))]
      (when (and (nil? new-store) (object? this))
        (assert (uniquely-identified-object?
                 (in-different-store this (current-store store)))
                this))
      (id->entity-m item-id orientation new-store)))

  Entity

  (mutable-entity? [this] true)

  (primitive? [this?] false)
  (element? [this] (link-id? item-id))
  (object? [this] (object-id? item-id))

  (content [this]
    (when (not (object-id? item-id))
      (if (= orientation :target)
        (let-R [content (id->target store item-id)]
          (endpoint->entity content store :target))
        (let-R [content (id->source store item-id)]
          (endpoint->entity content store)))))

  (elements [this]
    (let-R [forward-elements (forward-elements this)]
      (if (object-id? item-id)
        (let-R [element-ids (source->ids store item-id)
                targets (seq-R (map #(id->target store %) element-ids))]
          (seq (concat
                forward-elements
                (->> (map vector element-ids targets)
                     (keep (fn [[element-id target]]
                             (when (object-id? target)
                               element-id)))
                     (map #(id->element % :target store))))))
        forward-elements)))

  (forward-elements [this]
    (let-R [element-ids (target->ids store item-id)]
      (seq (for [element-id element-ids]
             (id->element element-id store)))))

  (orientation [this]
    orientation)

  (content->elements [this content-value]
    (let [content-key (entity-key content-value)]
      (let-R [element-ids (target-source->ids
                          store item-id content-key)
              reverse-element-ids (when (and (object-id? item-id)
                                            (object-id? content-key))
                                    (target-source->ids
                                     store content-key item-id))]
        (seq (concat (for [element-id element-ids]
                       (id->element element-id store))
                     (for [element-id reverse-element-ids]
                       (id->element element-id :target store)))))))

  (label->elements [this label]
    (let [label-key (entity-key label)]
      (let-R [element-ids (target-label->ids store item-id label-key)]
        (let [forward-elements (seq (for [element-id element-ids]
                                      (id->element element-id store)))]
          (if (object-id? item-id)
            (let-R [element-ids (source-label->ids store item-id label-key)
                    targets (seq-R (map #(id->target store %) element-ids))]
              (seq (concat
                   forward-elements
                   (->> (map vector element-ids targets)
                        (keep (fn [[element-id target]]
                                (when (object-id? target)
                                  element-id)))
                        (map #(id->element % :target store))))))
            forward-elements)))))

  (entity-key [this]
    item-id)

  (updating-immutable [this]
    (let-R [immutable-store (category-change-R [item-id] store)]
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
;;;  ((content orientation) element element...)
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
      (if (and (seq? f) (#{:source :target :either} (first f)))
        (second f)
        f)))

  (elements [this] (seq (rest this)))

  (forward-elements [this] (seq (filter #(not= (orientation %) :target)
                                        (elements this))))

  (orientation [this]
    (let [f (first this)]
      (if (and (seq? f) (#{:source :target :either} (first f)))
        (first f)
        :source)))

  (content->elements [this content-value]
    (seq (filter #(equivalent-entities? content-value (content %))
                 (elements this))))

  (label->elements [this label]
    (seq (filter (fn [element]
                   (some #(and (label-element? %)
                               (equivalent-entities? label (content %)))
                         (elements element)))
                 (elements this))))

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

  (elements [this]
    (assert (object? this) this)
    (seq (case (first this)
           :object (rest this)
           :conflux-object (nthrest this 2))))

  (forward-elements [this] (seq (filter #(not= (orientation %) :target)
                                        (elements this))))

  (orientation [this] nil)

  (content->elements [this content-value]
    (seq (filter #(equivalent-entities? content-value (content %))
                 (elements this))))

  (label->elements [this label]
    (seq (filter (fn [element]
                   (some #(and (label-element? %)
                               (equivalent-entities? label (content %)))
                         (elements element)))
                 (elements this))))


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
  (elements [this] nil)
  (forward-elements [this] nil)
  (orientation [this] :source)
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
  (elements [this] nil)
  (forward-elements [this] nil)
  (orientation [this] :source)
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
  (elements [this] nil)
  (forward-elements [this] nil)
  (orientation [this] :source)
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
  (elements [this] nil)
  (forward-elements [this] nil)
  (orientation [this] :source)
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
  (elements [this] nil)
  (forward-elements [this] nil)
  (orientation [this] :source)
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
  (elements [this] nil)
  (forward-elements [this] nil)
  (orientation [this] :source)
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
  (elements [this] nil)
  (forward-elements [this] nil)
  (orientation [this] :source)
  (content->elements [this content-value] nil)
  (label->elements [this label] nil)
  (entity-key [this] this)
  (updating-immutable [this] this))

(extend-protocol ToStoredEntity
  cosheet.store.ItemId
  (id->entity-m [this orientation store]
    (assert (or (nil? orientation) (#{:source :target} orientation)))
    (cond
      (nil? store) (->IdOnlyEntity this orientation)
      (mutable-store? store) (->MutableStoredEntity store this orientation)
      true (->ImmutableStoredEntity store this orientation))))

;;; Now that id->object has been implemented, we go back to the entity
;;; namespace and provide the definitions of these constants.
(in-ns 'cosheet.entity)
(def name-label (id->object name-label-id nil))
(def link-type (id->object link-type-id nil))
(def object-type (id->object object-type-id nil))

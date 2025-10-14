(ns cosheet2.entity-impl
  (:require (cosheet2 [store :refer [target-label->ids
                                     target->ids
                                     target-source->ids
                                     id->source id->target
                                     is-object-id?
                                     id->marked-as-type?
                                     mutable-store?
                                     current-store
                                     Store]]
                      [canonical :refer [equivalent-primitives?]]
                      [calculator :refer [current-value]]
                      [orderable :as orderable]
                      [entity :refer :all]
                      [expression :refer [expr-seq expr-let expr
                                          category-change]])))

(defrecord
    ^{:doc "An entity whose elements are described by a store."}
    ImmutableStoredEntity

    [store        ; The immutable store that holds the information for this item.
     item-id      ; The ItemId of the item in the store.
     orientation] ; The endpoint that holds an element entity's content.

  StoredEntity

  (container [this]
    (when (not (is-object-id? item-id))
      (if (= orientation :target)
        (description->entity (id->source store item-id) store)
        (description->entity (id->target store item-id) store))))
  
  (in-different-store [this store-or-entity]
    (id->entity item-id
                (if (satisfies? Store store-or-entity)
                  store-or-entity
                  (:store store-or-entity))
                orientation))
  
  Entity

  (mutable-entity? [this] false)

  (primitive? [this] false)

  (entity-type [this]
    (if (is-object-id? item-id)
      :object
      (if orientation
        :element
        :link)))

  (content [this]
    (when (not (is-object-id? item-id))
      (if (= orientation :target)
        (description->entity (id->target store item-id) store)
        (description->entity (id->source store item-id) store))))

  (elements [this]
    (seq (for [element-id (target->ids store item-id)]
           (id->element element-id store))))

  (orientation [this]
    orientation)

  (content->elements [this content-value]
    (seq (for [element-id (target-source->ids store item-id content-value)]
           (id->element element-id store))))

  (label->elements [this label]
    (seq (for [element-id (target-label->ids store item-id label)]
           (id->element element-id store))))

  (marked-as-type? [this]
    (id->marked-as-type? store item-id))

  (updating-immutable [this] this))

(defrecord
    ^{:doc "An item whose elements are described by a mutable store."}
    MutableStoredEntity

    [store        ; The mutable store that holds the information for this item.
     item-id      ; The ItemId of the item in the store.
     orientation] ; The endpoint that holds an element entity's content.


  StoredEntity

  (container [this]
    (when (not (is-object-id? item-id))
      (if (= orientation :target)
        (expr-let [container-id (id->source store item-id)]
          (when container-id
            (description->entity container-id store)))
        (expr-let [container-id (id->target store item-id)]
          (when container-id
            (description->entity container-id store))))))

  (in-different-store [this store-or-entity]
    (id->entity item-id 
                (if (satisfies? Store store-or-entity)
                  store-or-entity
                  (:store store-or-entity))
                orientation))

  Entity

  (mutable-entity? [this] true)

  (primitive? [this?] false)

  (entity-type [this]
    (if (is-object-id? item-id)
      :object
      (if orientation
        :element
        :link)))

  (content [this]
    (when (not (is-object-id? item-id))
      (if (= orientation :target)
        (expr-let [content (id->target store item-id)]
          (description->entity content store))
        (expr-let [content (id->source store item-id)]
          (description->entity content store)))))

  (elements [this]
    (expr-let [element-ids (target->ids store item-id)]
      (seq (for [element-id element-ids]
             (id->element element-id store)))))

  (orientation [this]
    orientation)

  (content->elements [this content-value]
    (expr-let [element-ids (target-source->ids store item-id content-value)]
      (seq (for [element-id element-ids]
             (id->element element-id store)))))

  (label->elements [this label]
    (expr-let [element-ids (target-label->ids store item-id label)]
      (seq (for [element-id element-ids]
             (id->element element-id store)))))

  (marked-as-type? [this]
    (id->marked-as-type? store item-id))

  (updating-immutable [this]
    (expr-let [immutable-store (category-change [item-id] store)]
        (in-different-store this immutable-store))))

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

  (entity-type [this] :element)

  (content [this]
    (let [f (first this)]
      (if (and (seq? f) (#{:source :target :either} (first f)))
        (second f)
        f)))

  (elements [this] (seq (rest this)))

  (orientation [this]
    (let [f (first this)]
      (if (and (seq? f) (#{:source :target :either} (first f)))
        (first f)
        :source)))

  (content->elements [this content-value]
    (seq (filter #(equivalent-primitives? content-value (content %))
                 (elements this))))

  (label->elements [this label]
    (seq (filter (fn [element]
                   (some #(and (equivalent-primitives? label (content %))
                               (label? %))
                         (elements element)))
                 (elements this))))

  (marked-as-type? [this]
    (some #(= (content %) :label)
          (elements this)))

  (updating-immutable [this] this))

;;; Make a vector work as an object or a link
;;; For an object, the format is
;;;   [:object element element ...]
(extend-type clojure.lang.PersistentVector

  Entity

  (mutable-entity? [this] false)

  (primitive? [this] false)

  (entity-type [this] :object)

  (content [this] nil)
  
  (elements [this]
    (assert (= (first this) :object))
    (seq (rest this)))

  (orientation [this] nil)

  (content->elements [this content-value]
    (seq (filter #(equivalent-primitives? content-value (content %))
                 (elements this))))

  (label->elements [this label]
    (seq (filter (fn [element]
                   (some #(and (equivalent-primitives? label (content %))
                               (label? %))
                         (elements element)))
                 (elements this))))

  (marked-as-type? [this] false)
  
  (updating-immutable [this] this))

(extend-protocol Entity
  clojure.lang.Keyword
  (mutable-entity? [this] false)
  (primitive? [this] true)
  (entity-type [this] :primitive)
  (content [this] this)
  (elements [this] nil)
  (orientation [this] nil)
  (content->elements [this content-value] nil)
  (label->elements [this label] nil)
  (marked-as-type? [this] false)
  (updating-immutable [this] this)
  
  clojure.lang.Symbol
  (mutable-entity? [this] false)
  (primitive? [this] true)
  (entity-type [this] :primitive)
  (content [this] this)
  (elements [this] nil)
  (orientation [this] nil)
  (content->elements [this content-value] nil)
  (label->elements [this label] nil)
  (marked-as-type? [this] false)
  (updating-immutable [this] this)
  
  java.lang.String
  (mutable-entity? [this] false)
  (primitive? [this] true)
  (entity-type [this] :primitive)
  (content [this] this)
  (elements [this] nil)
  (orientation [this] nil)
  (content->elements [this content-value] nil)
  (label->elements [this label] nil)
  (marked-as-type? [this] false)
  (updating-immutable [this] this)
  
  java.lang.Number
  (mutable-entity? [this] false)
  (primitive? [this] true)
  (entity-type [this] :primitive)
  (content [this] this)
  (elements [this] nil)
  (orientation [this] nil)
  (content->elements [this content-value] nil)
  (label->elements [this label] nil)
  (marked-as-type? [this] false)
  (updating-immutable [this] this)
  
  java.lang.Boolean
  (mutable-entity? [this] false)
  (primitive? [this] true)
  (entity-type [this] :primitive)
  (content [this] this)
  (elements [this] nil)
  (orientation [this] nil)
  (content->elements [this content-value] nil)
  (label->elements [this label] nil)
  (marked-as-type? [this] false)
  (updating-immutable [this] this)
  
  cosheet2.orderable.Orderable
  (mutable-entity? [this] false)
  (primitive? [this] true)
  (entity-type [this] :primitive)
  (content [this] this)
  (elements [this] nil)
  (orientation [this] nil)
  (content->elements [this content-value] nil)
  (label->elements [this label] nil)
  (marked-as-type? [this] false)
  (updating-immutable [this] this)

  nil ;; For convenience in null punning
  (mutable-entity? [this] false)
  (primitive? [this] true)
  (entity-type [this] :primitive)
  (content [this] this)
  (elements [this] nil)
  (orientation [this] nil)
  (content->elements [this content-value] nil)
  (label->elements [this label] nil)
  (marked-as-type? [this] false)
  (updating-immutable [this] this))

(extend-protocol ToStoredEntity
  cosheet2.store.ItemId
  (id->entity-m [this store orientation]
    (assert (or (nil? orientation) (#{:source :target} orientation)))
    (if (mutable-store? store)
      (->MutableStoredEntity store this orientation)
      (->ImmutableStoredEntity store this orientation))))

(extend-protocol Description
  cosheet2.store.ItemId
  (description->entity [this store] (id->entity this store))
  clojure.lang.Keyword
  (description->entity [this store] this)
  clojure.lang.Symbol
  (description->entity [this store] this)
  java.lang.String
  (description->entity [this store] this)
  java.lang.Number
  (description->entity [this store] this)
  java.lang.Boolean
  (description->entity [this store] this)
  clojure.lang.PersistentVector
  (description->entity [this store] this)
  cosheet2.orderable.Orderable
  (description->entity [this store] this)
  clojure.lang.ISeq
  (description->entity [this store] this)
  nil
  (description->entity [this store] nil) ;; For convenience in null punning
  )

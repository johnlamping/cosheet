(ns cosheet2.entity-impl
  (:require (cosheet2 [store :refer [id-label->element-ids
                                     id->element-ids
                                     id->content id->subject
                                     id->has-keyword?
                                     mutable-store?
                                     current-store
                                     Store]]
                      [utils :refer [equivalent-atoms?]]
                      [calculator :refer [current-value]]
                      [orderable :as orderable]
                      [entity :refer :all]
                      [expression :refer [expr-seq expr-let expr
                                          category-change]])))

(defrecord
    ^{:doc "An item whose elements are described by a store."}
    StoredItem

  [store     ; The immutable store that holds the information for this item.
   item-id]  ; The ItemDescription of the item in the store.

  StoredEntity

  (subject [this]
    (when-let [subject-id (id->subject store item-id)]
      (description->entity subject-id store)))

  (in-different-store [this store-or-entity]
    (description->entity (:item-id this)
                         (if (satisfies? Store store-or-entity)
                           store-or-entity
                           (:store store-or-entity))))
  
  Entity

  (mutable-entity? [this] false)

  (atom? [this] false)

  (label->elements [this label]
    (seq (for [element-id (id-label->element-ids store item-id label)]
           (description->entity element-id store))))

  (elements [this]
    (seq (for [element-id (id->element-ids store item-id)]
           (description->entity element-id store))))

  (content [this]
    (description->entity (id->content store item-id) store))

  (has-keyword? [this keyword]
    (id->has-keyword? store item-id keyword))

  (updating-immutable [this] this)

  (current-version [this] this))

(defrecord
    ^{:doc "An item whose elements are described by a mutable store."}
    MutableStoredItem

    [store     ; The mutable store that holds the information for this item.
     item-id]  ; The ItemDescription of the item in the store.

  StoredEntity

  (subject [this]
    (when-let [subject-id (id->subject store item-id)]
      (description->entity subject-id store)))

  (in-different-store [this store-or-entity]
    (description->entity (:item-id this)
                         (if (satisfies? Store store-or-entity)
                           store-or-entity
                           (:store store-or-entity))))

  Entity

  (mutable-entity? [this] true)

  (atom? [this?] false)

  (label->elements [this label]
    (expr-let [element-ids (id-label->element-ids store item-id label)]
      (seq (for [element-id element-ids]
             (description->entity element-id store)))))

  (elements [this]
    (expr-let [element-ids (id->element-ids store item-id)]
      (seq (for [element-id element-ids]
             (description->entity element-id store)))))

  (content [this]
    (expr-let [content (id->content store item-id)]
      (description->entity content store)))

  (has-keyword? [this keyword]
    (id->has-keyword? store item-id keyword))

  (updating-immutable [this]
    (expr-let [immutable-store (category-change [item-id] store)]
        (in-different-store this immutable-store)))

  (current-version [this]
    (description->entity item-id (current-store store))))

;;; Make a list work as an item. The format is (content element
;;; element...) We use ISeq, because, for example, while '(1 2) is a
;;; PersistentList, `(1 2) is a Cons.
(extend-type clojure.lang.ISeq
  
  Entity

  (mutable-entity? [this] false)

  (atom? [this] false)

  
  (label->elements [this label]
    (seq (filter (fn [element]
                   (some #(and (equivalent-atoms? label (ultimate-content %))
                               (label? %))
                         (elements element)))
                 (elements this))))

  (elements [this] (seq (rest this)))

  (content [this] (first this))

  (has-keyword? [this keyword]
    (some #(= (content %) keyword) (elements this)))

  (updating-immutable [this] this)

  (current-version [this] this))

(extend-protocol Entity
  clojure.lang.Keyword
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (has-keyword? [this keyword] false)
  (updating-immutable [this] this)
  (current-version [this] this)
  clojure.lang.Symbol
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (has-keyword? [this keyword] false)
  (updating-immutable [this] this)
  (current-version [this] this)
  java.lang.String
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (has-keyword? [this keyword] false)
  (updating-immutable [this] this)
  (current-version [this] this)
  java.lang.Number
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (has-keyword? [this keyword] false)
  (updating-immutable [this] this)
  (current-version [this] this)
  java.lang.Boolean
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (has-keyword? [this keyword] false)
  (updating-immutable [this] this)
  (current-version [this] this)
  cosheet2.orderable.Orderable
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (has-keyword? [this keyword] false)
  (updating-immutable [this] this)

  nil ;; For convenience in null punning
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (has-keyword? [this keyword] false)
  (updating-immutable [this] this)
  (current-version [this] this)
)

(extend-protocol Description
  cosheet2.store.StoredItemDescription
  (description->entity [this store]
    (if (mutable-store? store)
      (->MutableStoredItem store this)
      (->StoredItem store this)))
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





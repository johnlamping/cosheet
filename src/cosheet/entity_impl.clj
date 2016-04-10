(ns cosheet.entity-impl
  (:require (cosheet [store :refer [atom-description?
                                    id-label->element-ids
                                    id->element-ids
                                    id->content
                                    id->content-reference
                                    id->list
                                    mutable-store?
                                    stored-item-id-string]]
                     [orderable :as orderable]
                     [entity :refer :all]
                     [expression :refer [expr-seq expr-let expr]])))

(defrecord
    ^{:doc "An item whose elements are described by a store."}
    StoredItem

  [store     ; The immutable store that holds the information for this item.
   item-id]  ; The ItemDescription of the item in the store.

  StoredEntity
  Entity

  (mutable-entity? [this] false)

  (atom? [this?] (atom-description? item-id))

  (label->elements [this label]
    (seq (for [element-id (id-label->element-ids store item-id label)]
           (description->entity element-id store))))

  (elements [this]
    (seq (for [element-id (id->element-ids store item-id)]
           (description->entity element-id store))))

  (content [this]
    (description->entity (id->content store item-id) store))

  (content-reference [this]
    (description->entity (id->content-reference store item-id) store))

  (to-list [this]
    (id->list store item-id)))

(defrecord
    ^{:doc "An item whose elements are described by a mutable store."}
    MutableStoredItem

    [store     ; The mutable store that holds the information for this item.
     item-id]  ; The ItemDescription of the item in the store.

  StoredEntity
  Entity

  (mutable-entity? [this] true)

  (atom? [this?] (atom-description? item-id))

  (label->elements [this label]
    (expr-let [element-ids (expr id-label->element-ids store item-id label)]
      (seq (for [element-id element-ids]
             (description->entity element-id store)))))

  (elements [this]
    (expr-let [element-ids (expr id->element-ids store item-id)]
      (seq (for [element-id element-ids]
             (description->entity element-id store)))))

  (content [this]
    (expr-let [content (expr id->content store item-id)]
      (description->entity content store)))

  (content-reference [this]
    (expr-let [reference (expr id->content-reference store item-id)]
      (description->entity reference store)))

  (to-list [this]
    (id->list store item-id)))

;;; Make a list work as an item. The format is (content element
;;; element...) We use ISeq, because, for example, while '(1 2) is a
;;; PersistentList, `(1 2) is a Cons.
(extend-type clojure.lang.ISeq
  
  Entity

  (mutable-entity? [this] false)

  (atom? [this] false)

  (label->elements [this label]
    (seq (filter #(some (partial = label)
                        (map atomic-value (elements %)))
                 (elements this))))

  (elements [this] (seq (rest this)))

  (content [this] (first this))

  (content-reference [this] (first this))

  (to-list [this] this))

(extend-protocol Entity
  clojure.lang.Keyword
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  (to-list [this] this)
  clojure.lang.Symbol
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  (to-list [this] this)
  java.lang.String
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  (to-list [this] this)
  java.lang.Number
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  (to-list [this] this)
  java.lang.Boolean
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  (to-list [this] this)
  cosheet.orderable.Orderable
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  (to-list [this] this)

  nil ;; For convenience in null punning
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  (to-list [this] this)
)

(extend-protocol Description
  cosheet.store.StoredItemDescription
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
  cosheet.orderable.Orderable
  (description->entity [this store] this)
  nil
  (description->entity [this store] nil) ;; For convenience in null punning
  )

;;; TODO: We don't need two versions of each of these any more,
;;; because expr is smart about returning a reporter or not.
(defmethod label->content false [entity label]
  (let [elements (label->elements entity label)]
    (when elements
      (assert (= (count elements) 1))
      (content (first elements)))))

(defmethod label->content true [entity label]
  (expr-let [elements (expr label->elements entity label)]
    (when elements
      (assert (= (count elements) 1))
      (content (first elements)))))

(defmethod atomic-value false [entity]
  (cond (nil? entity) nil
        (atom? entity) (content entity) ; Could be implicit content reference.
        :else (atomic-value (content entity))))

(defmethod atomic-value true [entity]
  (if (nil? entity)
    nil
    (expr-let [content (expr content entity)]
      (if (atom? content)
        content
        (atomic-value content)))))

(defmethod label->atomic-values false [entity label]
  (map atomic-value (label->elements entity label)))

(defmethod label->atomic-values true [entity label]
  (expr-let [elements (expr label->elements entity label)]
    (expr-seq map atomic-value elements)))

(defmethod label-has-atomic-value? false [entity label value]
  (some (partial = value)
        (label->atomic-values entity label)))

(defmethod label-has-atomic-value? true [entity label value]
  (expr-let [atomics (expr label->atomic-values entity label)]
            (some (partial = value) atomics)))

(defmethod stored-entity-id-string  true [entity]
  (assert (satisfies? StoredEntity entity))
  (stored-item-id-string (:item-id entity)))

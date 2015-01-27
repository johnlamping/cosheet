(ns cosheet.entity-impl
  (:require (cosheet [store :refer [atom-description?
                                    id-label->element-ids
                                    id->element-ids
                                    id->content
                                    id->content-reference
                                    mutable-store?]]
                     [entity :refer :all]
                     [compute :refer [eval-map eval-let]])))

(defrecord
    ^{:doc "An item whose elements are described by a store."}
    StoredItem

  [store     ; The immutable store that holds the information for this item.
   item-id]  ; The ItemDescription of the item in the store.

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
    (description->entity (id->content-reference store item-id) store)))

(defrecord
    ^{:doc "An item whose elements are described by a store."}
    MutableStoredItem

  [store     ; The mutable store that holds the information for this item.
   item-id]  ; The ItemDescription of the item in the store.

  Entity

  (mutable-entity? [this] true)

  (atom? [this?] (atom-description? item-id) )

  (label->elements [this label]
    (eval-let [element-ids (id-label->element-ids store item-id label)]
              (seq (for [element-id element-ids]
                     (description->entity element-id store)))))

  (elements [this]
    (eval-let [element-ids (id->element-ids store item-id)]
              (seq (for [element-id element-ids]
                     (description->entity element-id store)))))

  (content [this]
    (eval-let [content (id->content store item-id)]
              (description->entity content store)))

  (content-reference [this]
    (eval-let [reference (id->content-reference store item-id)]
              (description->entity reference store))))

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

  (content-reference [this] (first this)))

(extend-protocol Entity
  clojure.lang.Keyword
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  java.lang.String
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  java.lang.Number
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  java.lang.Boolean
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  nil
  (mutable-entity? [this] false)) ;; For convenience in null punning

(extend-protocol Description
  cosheet.store.StoredItemDescription
  (description->entity [this store]
    (if (mutable-store? store)
      (->MutableStoredItem store this)
      (->StoredItem store this)))
  clojure.lang.Keyword
  (description->entity [this store] this)
  java.lang.String
  (description->entity [this store] this)
  java.lang.Number
  (description->entity [this store] this)
  java.lang.Boolean
  (description->entity [this store] this)
  nil
  (description->entity [this store] nil) ;; For convenience in null punning
  )

(defmethod label->content false [entity label]
  (let [elements (label->elements entity label)]
    (when elements
      (assert (= (count elements) 1))
      (content (first elements)))))

(defmethod label->content true [entity label]
  (eval-let [elements [label->elements entity label]]
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
    (eval-let [content [content entity]]
      (if (atom? content)
        content
        (atomic-value content)))))

(defmethod label->atomic-values false [entity label]
  (map atomic-value (label->elements entity label)))

(defmethod label->atomic-values true [entity label]
  (eval-let [elements [label->elements entity label]]
    (eval-map atomic-value elements)))

(defmethod label-has-atomic-value? false [entity label value]
  (some (partial = value)
        (label->atomic-values entity label)))

(defmethod label-has-atomic-value? true [entity label value]
  (eval-let [atomics [label->atomic-values entity label]]
    (some (partial = value) atomics)))


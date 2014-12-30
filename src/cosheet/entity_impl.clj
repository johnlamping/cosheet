(ns cosheet.entity-impl
  (:require [cosheet.store :refer [atom-description?]]
            [cosheet.item-store :refer :all]
            [cosheet.entity :refer :all]))

(defrecord
    ^{:doc "An item whose elements are described by a store."}
    StoredItem

  [store     ; The store that holds the information for this item.
   item-id]  ; The ItemDescription of the item in the store.
  
  Entity

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

;;; Make a list work as an item. The format is (content element
;;; element...) We use ISeq, because, for example, while '(1 2) is a
;;; PersistentList, `(1 2) is a Cons.
(extend-type clojure.lang.ISeq
  
  Entity

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
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  java.lang.String
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  java.lang.Number
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  java.lang.Boolean
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this))

(extend-protocol Description
  cosheet.store.StoredItemDescription
  (description->entity [this store] (->StoredItem store this))
  clojure.lang.Keyword
  (description->entity [this store] this)
  java.lang.String
  (description->entity [this store] this)
  java.lang.Number
  (description->entity [this store] this)
  java.lang.Boolean
  (description->entity [this store] this)
  nil
  (description->entity [this store] nil)
  )

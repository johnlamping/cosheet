(ns cosheet.store
  (:require [cosheet.entity :as entity]))

;;; All the information about non-primitive items is kept in the
;;; store: the elements of the item, their elements, complex values
;;; they reference, etc. Since the elements of items are themselves
;;; items, adding or removing items from the store can change the
;;; elements of another stored item.

;;; Internally, items are referred to by ids, which are managed by the
;;; store. Ids means that stores supports circular references among
;;; items, because the store represents the references between items,
;;; rather than the items having to reference each other directly.

;;; This means that there are two ways to represent a stored item: as
;;; a full fledged entity object that includes both the store and the
;;; item id, so the entity is self-sufficient, but doesn't track
;;; changes in the store, or as just id, which is how most
;;; communication with the store works, but which requires asking the
;;; store to get information about the entity. Constants, like numbers
;;; and strings, are both entities and descriptions.

;;; Note that in some cases, we will have an item we want to add to a
;;; store. It may reference some ids in the store, but the structure
;;; of the item obviously can't be in the store yet. The store will
;;; copy the items structure, allocating ids for it as necessary.

(defprotocol StoredItemDescription
  "A description of an Item recorded by a store"
  (atom-description? [this]
    "True if this description refers to an atom. This is true only for
   content references."))

(defprotocol BasicStore
  "The basic methods that databases support,
   from which higher levels ones are built."
  (add-simple-element [this subject content]
    "Add an element to the subject with the given content,
     which must be atomic, returning the store and id of the new element.")
  (remove-id [this id]
    "Remove the item with the given id from the store.
     It must have no elements.")
  (candidate-matching-ids [this item]
    "Return the ids of all items that could potentially be extensions
     of the given item")
  (mutable-store? [this]
    "Return whether this store is mutable"))

;; Factory that creates an empty ElementStore
(defmulti new-element-store
  (constantly true))

;; Factory that makes element ids from primitives, which must not be integers.
(defmulti make-id
  (constantly true))

(defn add-entity [store subject-id target]
  "Add the target entity with the given subject, whose content is the content
   of the target and whose elements are the elements of the target."
  (if (or (satisfies? StoredItemDescription target) (entity/atom? target))
    (add-simple-element store subject-id target)
    (let [[preliminary-store content]
          (let [content (entity/content target)]
            (if (or (satisfies? StoredItemDescription target)
                    (entity/atom? target))
              [store content]
              (add-entity store nil content)))]
      (let [[added-store id]
            (add-entity preliminary-store subject-id content)]
        [(reduce (fn [store element] (first (add-entity store id element)))
                 added-store
                 (entity/elements target))
         id]))))



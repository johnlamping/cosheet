(ns cosheet.store
  (:require [cosheet.entity :as entity]))

;;; All the information about non-primitive items is kept in the
;;; store: the elements of the item, their elements, complex values
;;; they reference, etc. Internally, such items are described by ids.
;;; Changing the store can thus logically change the elements of an
;;; item. This also supports circular references among items, because
;;; the store represents the references between items, rather than the
;;; items having to reference each other directly.

;;; This means that there are two ways to represent an item: with the
;;; store explicitly recorded as part of the representation, so the
;;; item object is self-sufficient, but doesn't track changes in the
;;; store, or as just a description, which is what the store works in
;;; terms of, but which can't support item operations by itself.
;;; Constants, like numbers and strings, are both items and
;;; descriptions.

;;; Note that in some cases, we will have an item we want to add to a
;;; store. It may reference some ids in the store, but the store
;;; obviously can't have the information about the item's elements.
;;; Rather, the item will rely on a second store or will have its own
;;; information. Adding the item to the main store will transfer that
;;; information.

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
     which must be atomic, returning the store and id of the element.")
  (remove-id [this id]
    "Remove the item with the given id from the store")
  (candidate-ids [this item]
    "Return all item ids that could potentially extend the given item"))

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



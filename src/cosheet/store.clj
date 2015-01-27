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
     content references, which are only created to refer to atoms."))

(defprotocol Store
  "The methods that all stores support for accessing their data.
   Mutable stores can return State objects as their answer
   for any of these methods, except for mutable-store?"

  ;; The methods that Item and Entity rely on stores having.
  ;; These take and return descriptions.
  
  (id-label->element-ids [this id label]
    "Returns a seq the ids of all ids that have the given subject and label.")

  (id->element-ids [this id]
    "Returns a seq of all ids that have the id as a subject.")

  (id->content [this id]
    "Given the id of an element, return a description of its content.")

  (id->content-reference [this id]
    "Given an item, return a reference to its content.")

  ;; Used by queries.
  
  (candidate-matching-ids [this item]
    "Return the ids of all items that could potentially be extensions
     of the given item")

  (mutable-store? [this]
    "Return whether this store is mutable"))

(defprotocol ElementStore
  "Methods that element stores implement,
   that rely on all items being explicitly known,
  and each having just one subject."
  
  (id->subject [this id]
    "Given an item, return its subject.")

  (id-is-content? [this id exceptions]
    "Return true if the id is the content of some other id
     that not in the list of exceptions."))

(defprotocol ImmutableStore
  "The basic methods that immutable stores support to create variants,
   from which higher levels ones are built."
  (add-simple-element [this subject content]
    "Add an element to the subject with the given content,
     which must be atomic, returning the store and id of the new element.")
  
  (remove-simple-id [this id]
    "Remove the item with the given id from the store.
     It must have no elements.")

  (update-content [this id content]
    "Change the content of the item with the  given id to be the
     specified content."))

(defprotocol MutableStore
  "The basic methods that mutable stores support to change themselves,
  from which higher levels ones are built."
  (current-store [this]
    "The current state of the mutable store")
  
  (add-simple-element! [this subject content]
    "Add an element to the subject with the given content,
     which must be atomic, returning the id of the new element.")
  
  (remove-simple-id! [this id]
    "Remove the item with the given id from the store.
     It must have no elements.")

  (update-content! [this id content]
    "Change the content of the item with the  given id to be the
     specified content."))

;; Factory that creates an empty ElementStore
(defmulti new-element-store
  (constantly true))

;; Factory that creates a MutableStore initialized to a given store
(defmulti new-mutable-store
  (constantly true))

;; Factory that makes element ids from primitives, which must not be integers.
(defmulti make-id
  (constantly true))




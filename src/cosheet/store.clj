(ns cosheet.store)

;;; All the information about non-primitive items is kept in the
;;; store: the elements of the item, their elements, complex values
;;; they reference, etc. Since the elements of items are themselves
;;; items, adding or removing items from the store can change the
;;; elements of another stored item.

;;; Internally, items are referred to by ids, which are managed by the
;;; store. Ids means that stores support circular references among
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
;;; copy the item's structure, allocating ids for it as necessary.

(defprotocol StoredItemDescription
  "A description of an Item recorded by a store"

  (non-implicit-id [this]
    "Traverse through ImplicitContentIds to get to the non-implicit id.")
  
  (atom-description? [this]
    "True if this description refers to an atom. This is true only for
     content references, which are only created to refer to atoms.")

  (stored-item-id-string [this]
    "A unique string id of the item")

  (stored-item-description-name [this]
    "A printable name for the item description"))

(defprotocol Store
  "The methods that all stores support for accessing their data.
   Mutable stores can return reporter objects as their answer
   for any of these methods, except for mutable-store?"

  ;; The methods that Item and Entity rely on stores having.
  ;; These take and return descriptions.

  (id-valid? [this id]
    "Returns true if the id is a valid id for the store.")
  
  (id-label->element-ids [this id label]
    "Returns a seq of all ids that have the given subject and label.")

  (id->element-ids [this id]
    "Returns a seq of all ids that have the id as a subject.")

  (id->content [this id]
    "Given the id of an element, return a description of its content.")

  (id->content-reference [this id]
    "Given an id, return a reference to its content.")

  ;; Used by queries.
  
  (candidate-matching-ids [this template]
    "Return the ids of all items that could potentially be extensions
     of the given template, which must be the list form of an entity,
     and may not have non-atomic contents.")

  (mutable-store? [this]
    "Return whether this store is mutable"))

(defprotocol ElementStore
  "Methods that element stores implement,
   that rely on all items being explicitly known,
   and each having just one subject."
  
  (id->subject [this id]
    "Given an item, return its subject. Assumes that the subject of an entity
    never changes, so doesn't return a reporter even for a mutable store."))

(defprotocol ImmutableStore
  "The basic methods that immutable stores support to create variants,
   from which higher levels ones are built."

  (id-is-content? [this id exceptions]
    "Return true if the id is the content of some other id
     that not in the list of exceptions.")

  (add-simple-element [this subject content]
    "Add an element to the subject with the given content,
     which must be atomic, returning the store and id of the new element.")
  
  (remove-simple-id [this id]
    "Remove the item with the given id from the store.
     It must have no elements.")

  (update-content [this id content]
    "Change the content of the item with the given id to be the
     specified content.")

  (track-modified-ids [this]
    "Record the ids that have been modified.")

  (fetch-and-clear-modified-ids [this]
    "Clear the record of modified ids.
     Returns the new store, and the original set of modified ids.")

  (declare-temporary-id [this id]
    "Declare the id to be temporary. It and all its descendant elements
     will not be written. Returns the new store.")

  (store-to-data [this]
    "Convert the store to a clojure structure that can be serialized.")

  (write-store [this stream]
    "Write the store to the stream in a format that read-store expects.")

  (data-to-store [this data]
    "Converts the output of store-to-data back to a store.")

  (read-store [this stream]
    "Reades a store that was written by write-store."))

(defprotocol MutableStore
  "The basic methods that mutable stores support to change themselves,
  from which higher levels ones are built."
  (current-store [this]
   "The current store of the mutable store.")

  (call-dependent-on-id [this id fun]
   "Returns a reporter with the result of calling the function on
   the immutable store, which will update whenever anything about the
   entity with the given id changes.")

  (reset-store! [this new-store]
    "Set the store to the new store, updating all reporters.")

  (do-update! [this update-fn]
    "Run the update function on the current state of the store.
     Update the store with the result, and notify all reporters of
     changes noted by the update. This is a way to package a number
     of updates to the current store into a single transaction.")

  (do-update-control-return! [this update-fn]
    "Run the update function on the current state of the store.
     It must return a pair of the new store and a return value.
     Update the store with the result, and notify all reporters of
     changes noted by the update. This is a way to package a number
     of updates to the current store into a single transaction.")

  (revise-update-control-return! [this expected-current update-fn]
    "Run the update function on the previous state of the store,
    provided the current state matches expected-current.")
  
  (add-simple-element! [this subject content]
    "Add an element to the subject with the given content,
     which must be atomic, returning the id of the new element.")
  
  (remove-simple-id! [this id]
    "Remove the item with the given id from the store.
     It must have no elements.")

  (update-content! [this id content]
    "Change the content of the item with the given id to be the
     specified content.")

  (declare-temporary-id! [this id]
    "Declare the id to be temporary. It and all its descendant elements
     will not be written.")

  (can-undo? [this]
    "Return true if an undo step can be done on the store.")

  (undo! [this]
    "Undo the last operation.")

  (can-redo? [this]
    "Return true if a redo step can be done on the store.")

  (redo! [this]
    "Redo the last undone operation."))

;; Factory that creates an empty ElementStore
(defmulti new-element-store
  (constantly true))

;; Factory that creates a MutableStore initialized to a given store
(defmulti new-mutable-store
  (constantly true))

;; Factory that makes element ids from primitives, which must not be integers.
(defmulti make-id
  (constantly true))

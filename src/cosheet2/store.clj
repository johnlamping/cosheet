(ns cosheet2.store)

;;; A store is a set of items, each of which has an identity, a
;;; content, and optionally a subject -- an item that it modifies. The
;;; set of items that modify another are called its elements.

;;; More complicated items are build up with successive levels of
;;; modification.  An item and everything that modifies it is called
;;; an entity.  Entities can be written in the form of list whose
;;; first member is the item's content, and whose subsequent members
;;; are its elements, in the same format. So it looks like:
;;;    (<content> (<content> ...) (<content ...) ...)
;;; This form doesn't include the ids of the items, and often the ids
;;; are only the concern of the store. For example, when querying for
;;; an entity matching a template, the list form is
;;; appropriate. Similarly, it can be used to describe an entity to be
;;; added.

;;; An item's subject can never change, and the subject structure is
;;; always a DAG; there are no circular references. But the content
;;; can change, and chasing subject links and ids in content can together
;;; yield cycles.

;;; The store needs to know three things about each item:
;;;         id: An implementation of StoredItemDescription that is unique
;;;             to the item. This is the how items are referred to in the
;;;             store's API.
;;;    content: The content of the item. This can be a string, number,
;;;             orderable, atom, nil, or the id of another item.
;;;    subject: The id of the item that this item is the subject of,
;;;             or nil if this item has no subject.

;;; The store maintains indices that make some queries about it
;;; faster. It provides special queries for items modified with an
;;; item with :label as its content.

(defprotocol StoredItemDescription
  "A description of an Item recorded by a store"

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
  
  (id->content [this id]
    "Given the id of an element, return a description of its content.")

  (id->element-ids [this id]
    "Returns a seq of all ids that have the id as their subject.")

  (id->subject [this id]
    "Given an item, return its subject. Assumes that the subject of an entity
    never changes, so doesn't return a reporter even for a mutable store.")

  (id-label->element-ids [this id label]
    "Returns the ids of all elements of the item with given id that
     have the structure
        (? (<label> (:label)))."
    )

  (candidate-matching-ids [this template]
    "Takes a template, which must be the list form of an entity,
     and may not have non-atomic contents. Return a seq ids that
     includes the ids all items that could potentially be extensions
     of the given template. Also return boolean that is true if the
     list of ids is precise; if all of them represent items that are
     extensions of the template.")

  (mutable-store? [this]
    "Return whether this store is mutable"))

(defprotocol ImmutableStore
  "The basic methods that immutable stores support to create variants,
   from which higher levels ones are built."

  (add-simple-item [this subject content]
    "Add an item to the subject with the given content,
     which must be atomic, returning the store and id of the new element.")
  
  (remove-simple-item [this id]
    "Remove the item with the given id from the store.
     It must have no elements.")

  (update-content [this id content]
    "Change the content of the item with the given id to be the
     specified content.")

  (get-unique-number [this]
    "Return a number and an updated store that will never return
     that number again.")

  (track-modified-ids [this]
    "Record the ids that have been modified.")

  (fetch-and-clear-modified-ids [this]
    "Clear the record of modified ids.
     Returns the new store, and the original set of modified ids.")

  (update-equivalent-undo-point [this equivalent]
    "Set whether this store is equivalent to the previous store as
     an undo point. This state persists through other changes until
     explicitly changed.")

  (equivalent-undo-point? [this]
    "Return whether this store is equivalent to the previous store as
     an undo point. An undo/redo goes to the nearest store not equivalent to
     the current one. This means that undo followed by redo won't
     necessarily return the same store. This gives natural behavior when
     persistent changes are interleaved with display changes, like selections,
     because it means that after either an undo or a redo, the selection is
     ends up at the item changed by the undo/redo.")

  (declare-temporary-id [this id]
    "Declare the id to be temporary. It and all its descendant elements
     will not be written. Returns the new store.")

  (store-update-new-further-action [this action]
    "Add an action to the further actions of the store.")

  (store-fetch-and-clear-further-actions [this]
    "Return the store with further actions eliminated, plus the list of
     further actions that were there.")

  (store-to-data [this]
    "Convert the store to a clojure structure that can be serialized.")

  (data-to-store [this data]
    "Converts the output of store-to-data back to a store.")

  (write-store [this stream]
    "Write the store to the stream in a format that read-store expects.")

  (read-store [this stream]
    "Reades a store that was written by write-store."))

(defprotocol MutableStore
  "The basic methods that mutable stores support to change themselves,
  from which higher levels ones are built."
  (current-store [this]
    "The current store of the mutable store.")

  (store-reset! [this new-store]
    "Set the store to the new store, updating all reporters.")

  (store-update! [this update-fn]
    "Run the update function on the current state of the store.
     Update the store with the result, and notify all reporters of
     changes noted by the update. This is a way to package a number
     of updates to the current store into a single transaction.")

  (store-update-and-act! [this update-fn]
    "Run the update function on the current state of the store.
     Update the store with the result, and notify all reporters of
     changes noted by the update. If the updated store has any further
     actions, perform them, and remove them from the store.")

  (store-update-control-return! [this update-fn]
    "Run the update function on the current state of the store.
     It must return a pair of the new store and a return value.
     Update the store with the result, and notify all reporters of
     changes noted by the update. This is a way to package a number
     of updates to the current store into a single transaction.")

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

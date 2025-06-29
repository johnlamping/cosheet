(ns cosheet2.store
  (:require (cosheet2 [utils :refer [parse-string-as-number]])))

;;; A store is a set of items, each of which has an identity, a
;;; content, and optionally a subject -- an item that it modifies. The
;;; set of items that modify another are called its elements.

;;; When an entity is stored in the store, it is represented as a root
;;; item, with all of its elements represented as items whose subject
;;; is the root.

;; The method description->entity will convert a root item's id and a
;; store into the entity it represents. In the other direction, there
;; are functions to put the list form of an entity into the store, as
;; a bunch of items.

;;; An item's subject can never change, and the subject structure is
;;; always a DAG; there are no circular references. But the content
;;; can change, and chasing subject links and ids in content can together
;;; yield cycles.

;;; The store needs to know three things about each item:
;;;         id: An ItemId that is unique
;;;             to the item. This is the how items are referred to in the
;;;             store's API.
;;;    content: The content of the item. This can be a string, number,
;;;             orderable, symbol, nil, or the id of another item.
;;;    subject: The id of the item that this item is the subject of,
;;;             or nil if this item has no subject.

;;; The store maintains indices that make some queries to it
;;; faster. It provides special queries for items modified with an
;;; item with :label as its content.

;;; Stores have a few other bits of functionality.
;;; Immutable stores can:
;;;   * Track ids that have been modified.
;;;   * Note whether they are semantically different from a
;;;     previous version, for purposes of undo/redo.
;;;   * Be asked for a new integer, and updating the store to
;;;     never return that integer again.
;;;   * Record a list of pending further actions.
;;;   * Read and write its contents to a stream. And some of its
;;;     contents can be marked as transitory -- not to be written.
;;; Mutable stores can:
;;;   * Undo and Redo.

;;; Long TODO:

;;; Change the store to generalize items to be symmetrical
;;; links, so that rather than having a subject and content, for most
;;; purposes, they simply have two ends.
;;;
;;; There would also be ids that did not identify items: but would
;;; generally represent some user object, like a trip or a
;;; restaurant. These would be entities that had no content, but that
;;; still had elements. They would typically have a :name element,
;;; which would be used in the UI to identify them. Both ends of an
;;; item could hold these non-item ids, to set up a relationship
;;; between them.
;;;
;;; One item in the store would define two entities, depending on its
;;; orientation: which end is considered to be its content. A variant
;;; of ItemId, OrientedItemId would give that orientation. The
;;; entity's elements would be any links connecting to it from either
;;; end, with their orientation determined by which end they link to
;;; it from. Some labels on an entity could be designated is applying in
;;; only one orientation.
;;;
;;; It would still be the case that only one of the ends of an item
;;; would be allowed to can hold an item, making it an element of that
;;; item. That avoids the "contents" of an item being another item,
;;; which reifies items in a way that doesn't seem useful.
;;;
;;; Both ends of items would be indexed, which lets you find all
;;; elements of an object, as well as all objects with a particular
;;; value. And when a change happens and you have to find all the
;;; entities that are modified, that may include both ends, since
;;; either or both could be user objects.

;;; The current code supports notifying a tree of any changes to any
;;; subtree. That is still supported, as only links to items need to
;;; be further chased, and an item is allowed to have only one such
;;; link. So you still get a tree structure.

(defrecord
    ^{:doc
      "The id of an item in a store."}
    ItemId
    [id])

(defn make-item-id
  "Make an item id that is not one that can be created by the store."
  [id]
  ;; Integers are reserved for creation by the store
  (assert (not (integer? id)))
  (->ItemId id))

(defn is-item-id?
  "Return true if the argument is an item id."
  [x]
  (instance? ItemId x))

(defn id->string
  "Return a string representation of an id."
  [id]
  (assert (instance? ItemId id))
  (let [id (:id id)]
    (if (integer? id) (str id) (str "I" id))))

(defn string->id
  "Given the string representation of an id, return the id."
  [rep]
  (->ItemId (if (= (first rep) \I) (subs rep 1) (parse-string-as-number rep))))

(defn item-id-name [this]
  "A printable name for the item id, indicating it is an id."
  (clojure.string/join ["Id-" (id->string this)]))

(defprotocol Store
  "The methods that all stores support for accessing their data.
   Mutable stores may return reporter objects as their answer
   for any of these methods, except for mutable-store?"

  (mutable-store? [this]
    "Return whether this store is mutable")

  ;; The methods that ImmutableStoredEntity and MutableStoredEntity rely
  ;; on stores having. They typically take a store and an ItemId, and may
  ;; return ItemIds.

  (id-valid? [this id]
    "Returns true if the id is a valid id for the store, one that 
    store has information about.")
  
  (id->subject [this id]
    "Given an item, return its subject. Assumes that the subject of an entity
    never changes, so doesn't return a reporter even for a mutable store.")

  (id->content [this id]
    "Given the id of an element, return a description of its content.")

  (id->element-ids [this id]
    "Returns a seq of all ids that have the id as their subject.")

  (id-label->element-ids [this id label]
    "Returns a seq of the ids of all elements of the given id that have an
     element of their own that has the label value as its content and
     that counts as a label.")

  (id->has-keyword? [this id keyword]
    "Returns true if the item with the given id has an element whose
    content is the given keyword.")

  (id->containing-ids [this id]
    "Returns a seq of all ids that have the given id as their content.")

  (candidate-matching-ids [this template]
    "Takes a template, which must be the list form of an entity,
     and may not have non-primitive contents. Return a seq of ids that
     includes the ids all entities that could potentially be
     extensions of the given template. Also return a boolean that is
     true if the list of ids is precise; if all of them represent
     items that are extensions of the template."))

  ;; TODO: Add a candidate-matching-element-ids method that takes a
  ;; template and gives a superset of all elements that could match
  ;; it.

(defprotocol ImmutableStore
  "The basic methods that immutable stores support to create variants,
   from which higher levels functions are built."

  (add-simple-item [this subject content]
    "Add an item with the given subject and content. The subject must be
     either nil or an id that is already in the store, and the content
     must be primitive. Return the store and id of the new element.")
  
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
    "Start recording the ids of items that have been modified.")

  (fetch-and-clear-modified-ids [this]
    "Clear the record of modified ids.
     Returns the new store, and the original set of modified ids.")

  (equivalent-undo-point? [this]
    "Return whether this store is equivalent to the previous store as an
     undo point. When this is the case, is is usually because what
     changed was something like the current selection, which is not
     semantic. An undo/redo goes to the nearest store not equivalent
     to the current one. This means that undo followed by redo won't
     necessarily return the same store. This gives natural behavior
     because it means that after either an undo or a redo, the
     selection ends up at the item changed by the undo/redo.")

  (update-equivalent-undo-point [this equivalent]
    "Set whether this store is equivalent to the previous store as an undo
     point. This state persists through all updates until it is
     explicitly changed.")

  (declare-temporary-id [this id]
    "Declare the id to be temporary. It and all its descendant elements
     will not be written when the store is written out. Returns the
     new store.")

  (store-fetch-and-clear-further-actions [this]
    "Return the store with any pending further actions eliminated, plus
     the list of pending further actions that were there.")

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
  from which higher levels functions are built.
  
  In addition to these methods, a MutableStore is also a Reporter."
  
  (current-store [this]
    "The current immutable store of the mutable store.")

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
     changes noted by the update. If the updated store has any pending
     further actions, perform them, and remove them from the store.")

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


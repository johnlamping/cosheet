(ns cosheet2.store
  (:require (cosheet2 [utils :refer [parse-string-as-number]])))

;;; TODO: Once store is fully updated, check that all of the below is true.

;;; A store deals with three kinds of items:
;;;   primitives: string, number, orderable, or symbol.
;;;      objects: atomic items that come with a unique id and nothing else.
;;;        links: which have a unique id and two endpoints, their source and
;;;               target, each of which are items.

;;; A store is primarily a record of a set of links. When an endpoint
;;; of a link is another link or an object, the store records that
;;; with the id of that item. A link is often thought of as giving
;;; information about its target item. Given an item, all the links
;;; that target it are called its elements, and all their sources are
;;; said to qualify the item. And those link's qualifiers can give
;;; information about how they quality it.

;;; Ids are distinguished from primitives by being wrapped in ItemId
;;; records. Object ids are either wrapped strings or wrapped negative
;;; numbers, while link ids are wrapped positive numbers. This makes
;;; it possible to tell immediately which kind of item an id
;;; indicates.

;;; A link can never be a source of another. And a primitive can
;;; never be a target of a link.

;;; A link's target can't change if it is another link. And a link's
;;; target can never be changed to be a link. In other words, a link's
;;; target can only be a link if the link was created that way. This
;;; ensures that the target structure is always a DAG; there are no
;;; circular references in chasing targets through links. Change
;;; tracking relies on this, as it propagates change notifications
;;; through targets to links. (It doesn't propagate changes through
;;; objects, and there can be loops if targets are also chased through
;;; objects.)

;;; Objects are not saved in the store, as such. All that is known
;;; about objects comes from the links that link to them.

;;; There are two special objects, whose ids are the strings "Type"
;;; and "Name", respectively. An object counts as a label or a class
;;; if it is qualified by the "Type" object. And a link counts as a
;;; name if it is qualified by the "Name" object. And a couple of
;;; links are added to the store so that the "Name" object has the
;;; name, "Name".

;;; The store maintains a map from name to object. It uses this to
;;; return the object id for a given name, and to ensure that no two
;;; objects get the same name. This map isn't written out when the
;;; store is written, as it can be derived from the links.

;;; A link or object, plus all its qualifiers, and their qualifiers,
;;; etc, determines an entity. And the Entity code knows how to use a
;;; store to satisfy the Entity APIs. The method description->entity
;;; will convert an item's id and a store into the entity it
;;; represents. The source of links becomes the content of entities,
;;; In the other direction, there are functions to put the list form
;;; of an entity into the store, as a bunch of links.

;;; The store needs to know three things about each link:
;;;         id: An ItemId that is unique
;;;             to the link. This is the how links are referred to in the
;;;             store's API.
;;;     source: either a primitive or the id of the source object.
;;;     target: The id of the link's target.

;;; The store maintains indices that make some queries to it
;;; faster. It provides special queries for items modified with labels
;;; or classes.

;;; Stores have a few other bits of functionality.
;;; Immutable stores can:
;;;   * Track ids that have been modified.
;;;   * Note whether they are semantically different from a
;;;     previous version, for purposes of undo/redo.
;;;   * Be asked for a new integer, updating the store to
;;;     never return that integer again.
;;;   * Record a list of pending further actions.
;;;   * Read and write its contents to a stream. And some of its
;;;     contents can be marked as transitory -- not to be written.
;;;   * Return the id for the object with a given name.
;;; Mutable stores can:
;;;   * Undo and Redo.

(defrecord
    ^{:doc
      "The id of an item in a store."}
    ItemId
    [id])

(defn make-item-id
  "Make an item id that is not one that will be given out by the store."
  [id]
  ;; Integers are reserved for creation by the store
  (assert (not (integer? id)))
  (->ItemId id))

(defn is-item-id?
  "Return true if the argument is an item id."
  [x]
  (instance? ItemId x))

(defn is-link-id?
  "Return true if the argument is an item id for a link."
  [x]
  (and (instance? ItemId x)
       (let [id (:id x)]
         (and (number? id)
              (> id 0)))))

(defn is-object-id?
   "Return true if the argument is an item id for an object."
  [x]
  (and (instance? ItemId x)
       (let [id (:id x)]
         (or (not (number? id))
             (< id 0)))))

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
  (clojure.string/join ["Id:" (id->string this)]))

(defprotocol Store
  "The methods that all stores support for accessing their data.
   Mutable stores may return reporter objects as their answer
   for any of these methods, except for mutable-store?"

  (mutable-store? [this]
    "Return whether this store is mutable")

  ;; The methods that ImmutableStoredEntity and MutableStoredEntity rely
  ;; on stores having. They typically take a store and an ItemId, and may
  ;; return ItemIds.

  (id-valid-link? [this id]
    "Returns true if the id is a valid link id for the store, one that 
    store has information about.")

  (id-described-object? [this id]
    "Returns true if the id is an object id that the store has some description
    for. In other words, one that is the target of some link in the store.")
  
  (id->target [this id]
    "Given a link id, return its target. If the target is a link, it
    assumes that it never changes, so doesn't return a reporter even
    for a mutable store.")

  (id->source [this id]
    "Given the id of a link, return a description of its source.")

  (target->ids [this id]
    "Returns a seq of all ids that have the given target.")

  (source->ids [this id]
    "Returns a seq of all ids that have the given source.")

  (target-label->ids [this id label]
    "Returns a seq of the ids that have the given id as their target and
    that are themselves targeted by a link that counts as a label and
    that has the given label as its source.")

  ;; TODO: Probably get rid of this.
  (id->has-keyword? [this id keyword]
    "Returns true if the link with the given id has an element whose
    source is the given keyword.")

  (candidate-matching-ids [this template]
    "Takes a template, which must be the list form of an entity. Return a
     seq of ids that includes the ids all entities that could
     potentially be extensions of the given template. Also return a
     boolean that is true if the list of ids is precise; if all of
     them represent items that are extensions of the template."))

  ;; TODO: Add a candidate-matching-element-ids method that takes a
  ;; template and gives a superset of all elements that could match
  ;; it.

(defprotocol ImmutableStore
  "The basic methods that immutable stores support to create variants,
   from which higher levels functions are built."

  (add-link [this target source]
    "Add an item with the given target and source. The target must be
     either nil or an object id or a link id that is already in the
     store, and the source must be primitive or an object id. Return
     the modified store and the id of the new element.")
  
  (remove-link [this id]
    "Remove the link with the given id from the store.
     It must not be the target of any other links.")

  (update-source [this id source]
    "Change the source of the link with the given id to be the
     specified source.")

  ;; TODO: replace this with get-unique-atom-id
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


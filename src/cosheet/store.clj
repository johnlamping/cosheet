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

  (id-is-content? [this id]
    "Return true if the id is the content of some other id.")

  ;; Used by queries.
  
  (candidate-matching-ids [this item]
    "Return the ids of all items that could potentially be extensions
     of the given item")

  (mutable-store? [this]
    "Return whether this store is mutable"))

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

;; Factory that creates an empty ElementStore
(defmulti new-element-store
  (constantly true))

;; Factory that makes element ids from primitives, which must not be integers.
(defmulti make-id
  (constantly true))

(defn add-entity
  "Add an entity to the store, with content and elements equal to the target,
   and with the given subject.
   Return the new store and the id of the element."
  [store subject-id target]
  (if (or (satisfies? StoredItemDescription target) (entity/atom? target))
    (let [[s id] (add-simple-element store subject-id target)]
      [s id])
    (let [[preliminary-store content]
          (let [content (entity/content target)]
            (if (or (satisfies? StoredItemDescription content)
                    (entity/atom? content))
              [store content]
              (add-entity store nil content)))]
      (let [[added-store id]
            (add-entity preliminary-store subject-id content)]
        [(reduce (fn [store element] (first (add-entity store id element)))
                 added-store
                 (entity/elements target))
         id]))))

(defn remove-entity-by-id
  "Remove the entity with the given id, and all its elements and content."
  [store id]
  (let [content (id->content store id)
        no-elements (reduce (fn [store element]
                              (remove-entity-by-id store element))
                            store
                            (id->element-ids store id))
        no-entity (remove-simple-id no-elements id)]
    (if (and (satisfies? StoredItemDescription content)
             (not (id-is-content? no-entity content)))
      (remove-entity-by-id no-entity content)
      no-entity)))



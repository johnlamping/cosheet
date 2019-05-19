(ns cosheet.entity)

;;; An entity is either a constant or an item. An item may have a
;;; content, which is another entity, and may have elements, which are
;;; other items. There is no ordering among the elements of an item.

;;; In list form, an item is written as (content element element ...)
;;; For example (5 "value" (3 "x") (4 "y")) describes an item with
;;; content 5, and three elements, ("value"), (3 "x") and (4 "y"). As
;;; illustrated here, elements that have just a content and no items
;;; can have their parentheses dropped in the list form.

;;; There are functions to get all elements of an entity, or just
;;; those elements with a specific label. A label is, for example, the
;;; "x" in the element(3 "x"). In general, the content of any element
;;; of an element is a label for that element.

(defprotocol Description
  "A description of an item or constant."
  (description->entity [this store]
    "Return an item or other entity, given the store the description
     depends on."))

(defprotocol StoredEntity
  "A tag for stored entities. They must have unique item-ids."

  (subject [this]
    "Return the subject of this stored entity, if any.")

  (in-different-store [this store-or-entity]
    "Replace the entity with an entity with the same id,
    but with the specified store or the store the second entity."))

(defprotocol Entity
  "An item or constant. For constants, the entity methods behave as if
  it had been reified into an item with the constant as its content,
  and nothing else."

  (mutable-entity? [this]
    "True if this entity might change. In that case, most of the methods
     on it assume they are runing under a compute-manager, and may return
     reporters.")

  (atom? [this]
    "True if this entity is atomic: either a primitive or a reference to
     a primitive.
     Note: A reference to the content of a mutable store can start
     out atomic, and then become non-atomic if the content gets
     promoted to a full item. It will still be reported as an atom,
     though, because references are only valid in the context of a
     query. If the store changes, the query will be re-run, and the
     non-atomicness noticed at that point.")

  ;; The results of the following methods can change if the entity is
  ;; mutable. In that case, they return reporters.

  ;; TODO: Rather than taking an atomic label, allow this to take an
  ;;       arbitrary description of the elements in question.
  (label->elements [this label]
    "Return a seq of items for all our elements with an elaboration with
     the given atomic label.")

  (elements [this]
    "Return a seq of items for all our elements.")

  (content [this]
    "Return the content of the entity.")

  (content-reference [this]
    "Return a reference to the content of the entity if the entity is stored,
     otherwise just return its content.")

  (updating-call-with-immutable [this fun]
  "Call the function with an immutable version of the entity.
  If the entity is mutable, a reporter is returned whose value is the result
  of calling the function on the immutable version of the entity, getting
  re-called whenever the entity changes.
  This is good if you want to do a computation on the entity, and
  not have to track every sub-dependency.")

  (current-version [this]
    "Return an immutable entity that is the current value of the entity."))

(defmacro updating-with-immutable
  "Run with an immutable version of the entity.
  If the entity is mutable, a reporter is returned whose value is the result
  of running the body on the immutable version of the entity, getting
  re-called whenever the entity changes."
  [[var entity] & body]
  `(updating-call-with-immutable
    ~entity
    (fn [~var] ~@body)))

;;; Utility functions that work on entities

(defn- mutable?-dispatch [entity & rest]
  (mutable-entity? entity))

(defmulti label->content
  "Return the content of the element with the given label.
   There must be at most one such element." 
  mutable?-dispatch)

(defmulti atomic-value
  "Return the atomic value reached by chasing contents"
  mutable?-dispatch)

(defmulti label->atomic-values
  "Return all atomic values for the label, reflecting multiplicity."
  mutable?-dispatch)

(defmulti label-has-atomic-value?
  "Whether the entity has the given value
   among its atomic values for the given label."
  mutable?-dispatch)

(defmulti to-list
  "Return a list form of the entity. If a content is itself an entity,
  include the entity in the list, rather than its content.
  That way, the value of to-list will only change if the entity or something
  that pertains to it changes."
  (constantly true))

(defmulti deep-to-list
  "Return a list form of the entity. If a content is itself an entity,
  include the list form of its content."
  (constantly true))

(defmulti stored-entity-id-string
  "Return a unique id string for a stored entity"
  (constantly true))


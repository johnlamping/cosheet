(ns cosheet2.entity
  (:require (cosheet2 [calculator :refer [current-value]]
                      [expression :refer [expr-let]])))

;;; An entity is either
;;;    a constant
;;;    an object
;;;    a link
;;;    an element

;;; Constants, objects, and links have the same meaning as they do in
;;; stores. But elements are a little different.  An element is a
;;; property, qualifier, or relation of another entity, as seen from
;;; that entity. There is no ordering among the elements of an entity.

;;; An element's description consists of a content, which is an
;;; entity, plus any elements of the element. So each element
;;; determines a tree of its elements, their elements, etc.

;;; In a store, an element is represented by a link. But an element
;;; additionally picks a direction for that link, thus determing which
;;; endpoint of the link the element is considered to be about. The
;;; other endpoint is the entity's content. The endpoint an entity is
;;; about is normally not considered to be part of the element; it's
;;; not included in the element's description.

;;; Elements are normally not accessed in terms of source and target,
;;; but in terms of content. Elements that are associated with stores
;;; also support a container method, which returns the entity they
;;; qualify. But the container is not considered to be part of the
;;; entity.

;;; Elements essentially give a way of parsing the information in a
;;; store into entities that have more structure than just a bunch of
;;; links. They give a convenient way to describe
;;;    * What should be displayed in a cell
;;;    * Information that should be added to entities in the store.
;;;    * Queries for searches over the store (to find entities matching
;;;      an element or containing matches to an element).
;;; These uses of entities typically require creating an entities that
;;; doesn't exactly match any entity in the store. To this end, there
;;; is a representation of entities that is largely independent of
;;; stores, called the list form.

;;; The list form of constants is just constants, since they are
;;; already independent of stores. The list form of specific objects
;;; is a wrapper of their id with the store, because the id is the
;;; only way to identify specific objects. But the list form of an
;;; element is
;;;   ((content orientation) element element ...)
;;; where orientation is either :source or :target, to indicate which
;;; endpoint holds the content. That indicates how an entity should be
;;; turned into a link. The orientation can also be nil if the element
;;; indicates a query that can match links going in either direction.

;;; Alternatively, in the common case when the orientation is :source,
;;; the list form of an element may be
;;;   (content element element ...)
;;; For example (5 "value" (3 "x") (4 "y")) describes an item with
;;; content 5, and three elements, ("value"), (3 "x") and (4 "y"). As
;;; illustrated here, elements that have just a content, orientation
;;; :source, and no items can have their parentheses dropped in the
;;; list form.

;;; When a objects is used in a query as a generic object, there also
;;; needs to be a list form for it. Its list form is
;;;    [:object element element ...]
;;; The code that converts items in the store to their query forms
;;; generates this representation for generic objects, which have the
;;; tag, :generic, as a property in the store.

;;; If it turns out that list forms of links are also necessary, they
;;; should be
;;;    [:link source target element element ...]

;;; TODO: Add an id->element method, and replace most used of
;;; description->entity with it. Maybe there should also be
;;; id->entity. And link entities should not be implemented until they
;;; are needed somewhere.

;;; There are functions to get all elements of an entity, or just
;;; those elements with a specific label. For the entity
;;; ("Joe" "person" (44 ("age" :label) "uncertain")) "age" is the label
;;; for the element (44 ("age" :label) "uncertain"). In general, a label
;;; of an element is one of its elements that itself has the element :label.

(defprotocol ToStoredEntity
  "A description of an item."
  (id->entity-m [this store orientation]
    "Return an entity corresponding to an item id."))

(defprotocol Description
  "A description of an item or constant."
  (description->entity [this store]
    "Return an item or other entity, given the store the description
     depends on."))

(defprotocol Entity
  "An item or constant. For constants, the entity methods behave as if
  it is an entity with the constant as its content, and no elements."

  (mutable-entity? [this]
    "True if this entity might change. In that case, most of the methods
     on it assume they are runing under a compute-manager, and may return
     reporters.")

  (primitive? [this]
    "True if this entity is a primitive, like a string or a number.")

  ;; The results of the following methods can change if the entity is
  ;; mutable. In that case, they return reporters.

  (content [this]
    "Return the content of the entity.")

  (elements [this]
    "Return a seq of items for all our elements.")

  (content->elements [this content-value]
    "Return the elements with the given content")

  (label->elements [this label]
    "Return a seq of items for all our elements with an elaboration with
     the given atomic label.")

  (marked-as-type? [this]
    "Return whether the entity is marked as being a type. (Has an element
     whose content is :label)"
    (some #(= (content %) :label)
          (elements this)))

  (updating-immutable [this]
    "If the entity is immutable, return it. Otherwise, return a
     reporter whose value is an immutable entity matching the current
     value of the entity. This is good if you want to do a computation
     on the entity, and not have to track every sub-dependency."))

(defprotocol StoredEntity
  "A tag for stored entities. They must have unique item-ids."

  (container [this]
    "Return the container of this stored entity (its target), if any.")

  (in-different-store [this store-or-entity]
    "Replace the entity with an entity with the same id,
    but with the specified store or the store of the second entity."))

;;; Utility functions that work on entities

;; NOTE: This definition must be kept in synch with store-impl/id-is-label?
(defn label? [entity]
  "Return whether the entity counts as a label (either has content that
  is a keyword and is not :label, or has an element whose content
  is :label)."
  (or (let [content (content entity)]
        (and (keyword? content) (not= content :label)))
      (marked-as-type? entity)))

(defn minimal-label?
  "Given a label, Return true if it is as small as it can be
   while still being a label."
  [entity]
  (if (keyword? (content entity))
    (empty? (elements entity))
    (empty? (rest (elements entity)))))

(defn content-transformed-immutable-to-list [content-transformer]
  "Internal function that takes a transformer on contents
  and returns a function that converts an immutable entity to a list,
  running the content transformer on contents."
  ;; Note: We tried using a letfn here, so we didn't have to recursively
  ;; call content-transformed-immutable-to-list. But that resulted in
  ;; a compile error, where the letfn definition was not available deep
  ;; inside.
  (fn [entity]
    (let [content (content-transformer (content entity))
          elements (elements entity)]
      (if (empty? elements)
        content
        (cons content
              (map (content-transformed-immutable-to-list content-transformer)
                   elements))))))

(defn to-list [entity]
  "Return a list form of the entity. If a content is itself an entity,
  include the entity in the list, rather than its content.
  That way, the value of to-list will only change if the entity or something
  that pertains to it changes."
  (if (mutable-entity? entity)
    ;; We want to run with updating-immutable, but if a content is an
    ;; entity, we want the resulting entity to reference the mutable
    ;; store.
    (expr-let [immutable (updating-immutable entity)]
      ((content-transformed-immutable-to-list
        (fn [content] (if (satisfies? StoredEntity content)
                         (in-different-store content entity)
                         content)))
       immutable))
    ((content-transformed-immutable-to-list identity) entity)))

(defn to-deep-list [entity]
  "Like to-list, but expand out content that is entities."
  (if (primitive? entity)
    entity
    (expr-let [immutable (updating-immutable entity)]
      (let [content-as-list (to-deep-list (content entity))
            elements (elements entity)]
        (if (empty? elements)
          content-as-list
          (cons content-as-list (map to-deep-list elements)))))))

(defn label->element
  "Return the element with the given label.
  There must be at most one such element."
  [entity label]
  (expr-let [elements (label->elements entity label)]
    (when elements
      (assert (= (count elements) 1)
              (apply str "entity "  (:id (:item-id entity))
                     " has " (count elements) " elements for label " label
                     " entity contents: " (current-value (to-list entity))
                     " element contents: "
                     (interleave (repeat " ")
                                 (map #(current-value (content %)) elements))))
      (first elements))))

(defn label->content
  "Return the content of the element with the given label.
   There must be at most one such element."
  [entity label]
  (expr-let [element (label->element entity label)]
    (content element)))

(defn id->entity
  ([id store]
   (id->entity-m id store :source))
  ([id store orientation]
   (id->entity-m id store orientation)))

(defn id->updating-entity-R
  ([id store]
   (id->updating-entity-R id store :source))
  ([id store orientation]
   (let [entity (id->entity id store orientation)]
     (updating-immutable entity))))


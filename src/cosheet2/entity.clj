(ns cosheet2.entity
  (:require (cosheet2 [calculator :refer [current-value]]
                      [expression :refer [expr-let]])))

;;; An entity is either a constant or an item. An item may have a
;;; content, which is another entity, and may have elements, which are
;;; other items. There is no ordering among the elements of an item.

;;; In list form, an item is written as (content element element ...)
;;; For example (5 "value" (3 "x") (4 "y")) describes an item with
;;; content 5, and three elements, ("value"), (3 "x") and (4 "y"). As
;;; illustrated here, elements that have just a content and no items
;;; can have their parentheses dropped in the list form.

;;; There are functions to get all elements of an entity, or just
;;; those elements with a specific label. For the entity
;;; ("Joe" "person" (44 ("age" :label) "uncertain")) "age" is the label
;;; for the element (44 ("age" :label) "uncertain"). In general, a label
;;; of an element is one of its elements that itself has the element :label.

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

  (atom? [this]
    "True if this entity is atomic: a primitive.")

  ;; The results of the following methods can change if the entity is
  ;; mutable. In that case, they return reporters.

  (label->elements [this label]
    "Return a seq of items for all our elements with an elaboration with
     the given atomic label.")

  (elements [this]
    "Return a seq of items for all our elements.")

  (content [this]
    "Return the content of the entity.")

  (updating-immutable [this]
    "If the entity is immutable, return it. Otherwise, return a
     reporter whose value is an immutable entity matching the current
     value of the entity. This is good if you want to do a computation
     on the entity, and not have to track every sub-dependency.")

  (current-version [this]
    "Return an immutable entity that is the current value of the entity."))

(defprotocol StoredEntity
  "A tag for stored entities. They must have unique item-ids."

  (subject [this]
    "Return the subject of this stored entity, if any.")

  (in-different-store [this store-or-entity]
    "Replace the entity with an entity with the same id,
    but with the specified store or the store of the second entity."))

;;; Utility functions that work on entities

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
  (if (atom? entity)
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

(defn ultimate-content
  "Chase content until an atomic content is reached."
  [entity]
  (if (nil? entity)
    nil
    (expr-let [content (content entity)]
      (if (atom? content)
        content
        (ultimate-content content)))))


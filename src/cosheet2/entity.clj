(ns cosheet2.entity
  (:require (cosheet2 [calculator :refer [current-value]]
                      [expression :refer [expr-let]]
                      [store :refer [is-item-id?
                                     is-link-id? is-object-id?]])))

;;; An entity is either
;;;    a primitive
;;;    an object
;;;    a link
;;;    an element

;;; Primitives, objects, and links have the same meaning as they do in
;;; stores. But elements are a little different.  An element is a
;;; property, qualifier, or relation of another entity, as seen from
;;; that entity. There is no ordering among the elements of an entity.

;;; Elements give a way of overlaying a structure on information in a
;;; store, or on information that might be stored there. That makes
;;; them more convenient than just a bunch of links. They give a
;;; convenient way to describe:
;;;    * What should be displayed in a cell.
;;;    * Information that should be added to entities in the store.
;;;    * Queries for searches over the store (to find entities matching
;;;      an element or containing matches to an element).

;;; An element's description consists of
;;;   * a content, which is an entity.
;;;   * an orientation, either :source or :target, which indicates how the
;;;     content is related to the element.
;;;   * any sub-elements of the element.
;;;     So each element determines a tree of its elements, their
;;;     elements, etc.

;;; In a store, an element is represented by a link, and the element's
;;; orientation says which endpoint of the link holds the element's
;;; content. The other endpoint is the entity that the element is
;;; about, that is, where the element is seen from. That endpoint of
;;; the link is the context of the element, and not considered to be
;;; part of the element. For example, it's not included in the
;;; element's description.

;;; Elements are normally accessed in terms of content, orientation,
;;; and sub-elements. But elements that are associated with stores
;;; also support a target-entity method, which returns the entity
;;; corresponding to their link's target. Depending on the element's
;;; orientation, that could either be its content or the entity the
;;; element is about.

;;; Since some uses of entities require creating an entity that
;;; doesn't exactly match any entity in the store. There is a
;;; representation of entities that is largely independent of stores,
;;; called the list form.

;;; The list form of primitives is just the primitive, since they are
;;; already independent of stores. The list form of specific objects
;;; is a wrapper of their id with the store, because the id is the
;;; only way to identify specific objects.
;;; Elements are more complicated. The most general list form of an
;;; element is
;;;   ((orientation content) element element ...)
;;; where orientation is either :source or :target, to indicate which
;;; endpoint holds the content.

;;; But the most general form isn't always necessary. If an element
;;; has orientation :source, and its content is either an object or is
;;; a primitive that is not a list, then one of two simplified forms
;;; is possible.
;;;    * If it also has no sub-elements of its own, then its list form is
;;;      just the list form of its content. This means that the list
;;;      form can't distinguish between a simple element and
;;;      primitives or objects. But that is OK, because all uses of
;;;      the list form either expect elements or non-elements.
;;;    * If it does have sub-elements of its own, then its list form is
;;;      (content element element ...)
;;; Combinding these simplified forms yield compact list
;;; representations of the most common kinds of elements, like
;;;   (5 "value" (3 "x") (4 "y"))

;;; When an object is used in a query as a generic object, there also
;;; needs to be a list form for it. Its list form is
;;;    [:object element element ...]
;;; The code that converts items in the store to their query forms
;;; generates this representation for generic objects, which have the
;;; tag, :generic, as a property in the store.

;;; If it turns out that list forms of links are also necessary, they
;;; should be
;;;    [:link source target element element ...]

;;; TODO: !!! Get rid of this once the definition of label changes
;;; There are functions to get all elements of an entity, or just
;;; those elements with a specific label. For the entity
;;; ("Joe" "person" (44 ("age" :label) "uncertain")) "age" is the label
;;; for the element (44 ("age" :label) "uncertain"). In general, a label
;;; of an element is one of its elements that itself has the element :label.

(defprotocol ToStoredEntity
  "A description of an item."
  (id->entity-m [this store orientation]
    "Return an entity corresponding to an item id."))

(defprotocol Entity
  "An store item or a primitive. For primitives, the entity methods
  behaves as if it were an entity with the primitive as its content,
  and no elements."

  (mutable-entity? [this]
    "True if this entity might change. In that case, most of the methods
     on it assume they are runing under a compute-manager, and may return
     reporters.")

  (primitive? [this]
    "True if this entity is a primitive, like a string or a number.")

  (object? [this]
    "True if this entity is an object (neither an element or a primitive)")

  (entity-type [this]
    "Return the type of the entity, one of :primitive, :object,
    or :element, (or :link if it gets implemented.)")

  ;; The results of the following methods can change if the entity is
  ;; mutable. In that case, they return reporters.

  (content [this]
    "Return the content of the entity.")

  (elements [this]
    "Return a seq of items for all our elements.")

  (orientation [this]
    "The orientation of an element entity.")

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

  (target-entity [this]
    "If the entity represents a link, return the entity corresponding to
    its target. This is independent of the orientation of the
    entity. If the resulting entity is also a link, it will have
    orientation :source, thus this entity will be its content.")

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

(defn make-element-list
  "Make the list representation of the described entity."
  [orientation content elements]
  ;; We leave off the orientation if we can.
  (let [needs-orientation (or (seq? content)
                              (not= orientation :source))]
    (if (and (empty? elements) (not needs-orientation))
      content
      (cons (if needs-orientation
              (list orientation content)
              content)
            elements))))

(defn content-transformed-immutable-to-list [content-transformer]
  "Internal function that takes a transformer on contents
  and returns a function that converts an immutable entity to a list,
  running the content transformer on contents."
  ;; Note: We tried using a letfn here, so we didn't have to recursively
  ;; call content-transformed-immutable-to-list. But that resulted in
  ;; a compile error, where the letfn definition was not available deep
  ;; inside.
  (fn [entity]
    (if (or (primitive? entity)
            (and (= (entity-type entity) :object)
                 (not (seq (content->elements entity :generic)))))
      (content-transformer entity)
      (let [elements (elements entity)
            mapped-elements (map (content-transformed-immutable-to-list
                                  content-transformer)
                                 elements)]
        (if (= (entity-type entity) :object)
          (apply vector :object mapped-elements)
          (let [content (content-transformer (content entity))]
            (make-element-list (orientation entity) content mapped-elements)))))))

(defn to-list [entity]
  "Return a list form of the entity. If a content is a non-generic object,
  include the object in the list, rather than its list form.
  That way, the value of to-list will only change if the entity or one
  of its elements that pertains to it changes."
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

(defn id->element [id store]
  (assert (is-link-id? id))
  (id->entity-m id store :source))

(defn id->object [id store]
  (assert (is-object-id? id))
  (id->entity-m id store nil))

(defn id->entity
  ([id store]
   (if (is-object-id? id)
     (id->entity-m id store nil)
     (id->entity-m id store :source)))
  ([id store orientation]
   (assert (is-item-id? id))
   (when (is-object-id? id)
     (assert (not orientation)))
   (id->entity-m id store orientation)))

(defn id->updating-entity-R
  ([id store]
   (id->updating-entity-R id store (when (not (is-object-id? id)) :source)))
  ([id store orientation]
   (let [entity (id->entity id store orientation)]
     (updating-immutable entity))))


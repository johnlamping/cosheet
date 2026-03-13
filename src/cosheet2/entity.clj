(ns cosheet2.entity
  (:require (cosheet2 [calculator :refer [current-value]]
                      [expression :refer [expr-let]]
                      [store :refer [make-item-id item-id?
                                     link-id? object-id?
                                     generic-name?
                                     ;; These are used by entity_impl.clj
                                     ;; when it is working in our namespace. 
                                     name-label-id
                                     link-type-id object-type-id]])))

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

;;; The store's restriction that the target can't be a primitive and
;;; the source can't be a link restricts the possible elements. In
;;; addition, for now, a link can only be reversed if both its source
;;; and target are objects. In other words, only relationships between
;;; objects are reversible.

;;; The case where the target can be a link may be supported
;;; later. That is a pretty big change because it means that content
;;; of an element could be another element, which is viewed from
;;; neither its link's source or target, but from one of the links to
;;; it.

;;; This, in turn, means that the elements of an object correspond to
;;; all links that have it as a target, and all links that have it as
;;; a target and that have an object as their subject.

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
;;; already independent of stores. The list form of mutable objects
;;; and named objects is their normal element form. That provides the
;;; support for the object methods, while also including their id. And
;;; one of these objects in a query matches a subject object iff the
;;; two have the same id (independent of which store they are in).

;;; But anonymous objects don't need an id in list form, as they match
;;; based on their elements, not their id. So they do have a list form
;;; that includes just their elements:
;;;    [:object element element ...]

;;; Elements are more complicated. The most general list form of an
;;; element is
;;;   ((orientation content) element element ...)
;;; where orientation is either :source or :target, to indicate which
;;; endpoint holds the content.

;;; But the most general form isn't always necessary. If an element
;;; has orientation :source, and its content is not a list, then one
;;; of two simplified forms is possible.
;;;    * If its content is a primitive and it has no sub-elements of
;;;      its own, then its list form is just its content.
;;;    * If it does have sub-elements of its own, then its list form is
;;;      (content element element ...)
;;; Combinding these simplified forms yield compact list
;;; representations of the most common kinds of elements, like
;;;   (5 "value" (3 "x") (4 "y"))

;;; This means that the list form of an element that consists of
;;; nothing but a primitive content is just that content. So
;;; converting to list form can change an entity from an element to a
;;; primitive.  To reduce the problems from that, primitives return
;;; the same answers to content, orientation, and elememts as would an
;;; element consisting of just that primitive as its content. That is,
;;; they return themselves for their content, :souce for their
;;; orientation, nil for their elements.

;;; We can't use the same trick for the list form of an element
;;; consisting of nothing but an object, because objects are defined
;;; to not have any content.

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
  (id->entity-m [this orientation store]
    "Return an entity corresponding to an item id.
    Should only be called by Entity and its implementation"))

(defprotocol Entity
  "An store item or a primitive. For primitives, the entity methods
  behaves as if it were an entity with the primitive as its content,
  and no elements."

  (mutable-entity? [this]
    "True if this entity might change. In that case, most of the methods
     on it assume they are runing under a compute-manager, and may return
     reporters.")

  ;; Every element is exactly one of these three:

  (primitive? [this]
    "True if this entity is a primitive, like a string or a number.")

  (element? [this]
    "True if the entity is an element.")

  (object? [this]
    "True if this entity is an object.")

  ;; The results of the following methods can change if the entity is
  ;; mutable. In that case, they return reporters.

  (content [this]
    "Return the content of the entity.")

  (elements [this]
    "Return a seq of items for all our elements, including reversed links
    between objects.")

  (forward-elements [this]
    "Return a seq of items for all our elements, not including reversed
    links between objects.")

  (orientation [this]
    "The orientation of an element entity.")

  (content->elements [this content-value]
    "Return the elements with the given content")

  (label->elements [this label]
    "Return a seq of items for all our elements with an elaboration with
     the given label.")

  (marked-as-type? [this]
    "Return whether the entity is marked as being a type. (Has an element
     whose content is :label)")

  (entity-key [this]
    "Return the key of this entity. For stored entities, it is their
    item-id. For all other entities it is the entity, itself.
    Matching of named objects is based on their key, so that it
    will be independent of any particular store.

    Note that the key ignores the orientation of elements. Both query
    matching and to-list rely on that to avoid infinite loops. The
    only other place the key is used is to compare objects, which have
    no orientation.")

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
    entity. If the target is a link, the resulting entity will be given
    orientation :source.")

  (in-different-store [this store-or-entity]
    "Replace the entity with an entity with the same id,
    but with the specified store or the store of the second entity."))

(defn id->element 
  ([id store]
   (id->element id :source store))
  ([id orientation store]
   (assert (link-id? id) id)
   (id->entity-m id orientation store)))

(defn id->object [id store]
  (assert (object-id? id) id)
  (id->entity-m id nil store))

(defn id->entity
  [id store]
  (if (object-id? id)
    (id->entity-m id nil store)
    (id->entity-m id :source store)))

(defn id->updating-entity-R
  ([id store]
   (id->updating-entity-R id store (when (not (object-id? id)) :source)))
  ([id store orientation]
   (let [entity (id->entity-m id orientation store)]
     (updating-immutable entity))))

;;; Utility functions that work on entities

;;; These are entities for the corresponding object ids. We can't give
;;; their definitions yet, because that requires id->object, which
;;; isn't implemented until entity_impl.clj. We declare them here, and
;;; entity_impl.clj will give them bindings.
(def name-label)
(def link-type)
(def object-type)

(defn stored-entity?
  [entity]
  (satisfies? StoredEntity entity))

(defn universal-object?
  "Return true if the object is one of the universal objects:
  name-label, link-type, and object-type. The object can be in any
  store."
  [entity]
  (and (stored-entity? entity)
       (#{name-label-id link-type-id object-type-id} (:item-id entity))))

(defn id-identified-object?
  "Return true if the entity is an object that is identified by its id."
  [entity]
  (and (stored-entity? entity)
       (object? entity)
       (string? (:id (:item-id entity)))))

(defn uniquely-identified-object?
  "Return true if the entity, which must be immutable, is an object that
  is uniquely identified, either by its name or by its id."
  [entity]
  (and (object? entity)
       (or (id-identified-object? entity)
           (when-let [names (label->elements entity name-label)]
             (some #(not (generic-name? %))
                   (map content names))))))

(defn interned-object?
  "Return true if the entity, which must be immutable, is a uniquely
  identified object, and is stored. (That implies that it has been
  interned."
  [entity]
  (and (stored-entity? entity)
       (uniquely-identified-object? entity)))

(defn link-type-object?
  "Return true if the entity is an object that is a link type."
  [entity]
  (and (object? entity)
       (seq (content->elements entity link-type))))

(defn object-type-object?
  "Return true if the entity is an object that is an object type."
  [entity]
  (and (object? entity)
       (seq (content->elements entity object-type))))

(defn non-type-object?
  "Return true if the entity is an object, but not link-type or object-type."
  [entity]
  (and (object? entity)
       (not (or (link-type-object? entity)
                (object-type-object? entity)))))

;;; NOTE: The next two definitions must be kept in synch with
;;; store-impl/id-is-label?

(defn label-object? [entity]
  "Return whether the entity is an object that makes an element that
   has the entity as content be a label."
  (and (object? entity)
       (or (= (entity-key entity) name-label-id)
           (link-type-object? entity)
           (object-type-object? entity))))

(defn label-element? [entity]
  "Return whether the entity counts as a label. A label is a link under
  which its target should be indexed, starting from either of the
  target's endpoints.
  A link is a label if:
     * It's source is either
        * a keyword that is not :label
        * the object with item-id 'name'.
        * an object that has an element whose content has an item-id of
         'link-type' or 'object-type'.
     * Has an element whose content is :label (obsolete)"
  (or (let [content (content entity)]
        (cond (object? content) (label-object? content)
              (keyword? content) (not= content :label)))
      ;; TODO: !!! Get rid of this marked-as-type? condition once
      ;; :label no longer marks labels.
      (marked-as-type? entity)))

(defn make-element-list
  "Make the list representation of the described entity, simplifying it
  as much as possible without leaving ambiguities. This can turn
  elements into primitives."
  [element-orientation content elements]
  ;; Validate the constraints on our object and elements
  (assert (or (not (element? content))
              ;; TODO: !!! For now, special forms are elements.
              ;;           When they become objects, remove this case.
              (= (keyword? (first content)))))
  (assert (not-any? #(or (object? %)
                         (not= (orientation %) :source))
                    elements))
  (assert (or (nil? element-orientation)
              (#{:source :target} element-orientation)))
  ;; We leave off the orientation if we can.
  (if (or (seq? content)
          (not= element-orientation :source))
    (cons (list element-orientation content) elements)
    (if (and (primitive? content)
             (empty? elements))
      content
      (cons content elements))))

(defn make-object-list
  "Make the list representation of the described object."
  [elements]
  ;; Make sure the elements we are given respect the list form.
  (assert (not-any? object? elements))
  (into [:object] elements))

(defn add-elements-to-entity
  "Add elements an entity, using a list form for its top level if
  anything changed."
  [entity elements-to-add]
  (if (empty? elements-to-add)
    entity
    (cond (element? entity)
          (make-element-list (orientation entity)
                             (content entity)
                             (concat (elements entity) elements-to-add))
          (object? entity)
          (make-object-list (concat (elements entity) elements-to-add))
          :else (make-element-list :source entity elements-to-add))))

(defn map-elements
  "Run the function, which must return an element, on each of the
  elements of the entity, if any, to get new elements. Then reassemble
  the entity from the resulting elements. Don't map the elements of
  interned objects."
  [f entity]
  (cond (element? entity)
        (make-element-list (orientation entity)
                             ;; This handles contents that are objects.
                             (content entity)
                             (map f (elements entity)))
        (and (object? entity) (not (interned-object? entity)))
        (make-object-list (map f (elements entity)))
        :else
        entity))

;;; TODO: Get rid of the next two if they are not used.

(defn coerce-primitive-to-element
  "If the entity is a primitive, make it into an element. This is useful
  when iterating over elements, to make sure that everything that
  comes back is an element."
  [entity]
  (if (primitive? entity)
    `(~entity)
    entity))

(defn pre-walk-entity
  "Recursively run the function on all the elements of the entity, from
  the top down up, going through non-interned objects. The function
  must return the same kid of entity as it gets. If the function turns
  an element into nil, that element will be removed."
  [f entity]
  (let [entity (f entity)]
    (cond (element? entity)
          (make-element-list (orientation entity)
                             (pre-walk-entity f (content entity))
                             (keep #(pre-walk-entity f %) (elements entity)))
          (and (object? entity) (not (interned-object? entity)))
          (make-object-list (keep #(pre-walk-entity f %) (elements entity)))
          :else
          entity)))

(defn post-walk-entity
  "Recursively run the function on all the elements of the entity, from
  the leaves up, going through non-interned objects. The function must
  return the same kid of entity as it gets. If the function turns
  an element into nil, that element will be removed."
  [f entity]
  (f (cond (element? entity)
           (make-element-list (orientation entity)
                              (post-walk-entity f (content entity))
                              (keep #(post-walk-entity f %)
                                    (elements entity)))
           (and (object? entity) (not (interned-object? entity)))
           (make-object-list (keep #(post-walk-entity f %) (elements entity)))
           :else
           entity)))

(defn recursively-in-different-store
  "Recursively put all stored entities in the entity into a different store."
  [entity store]
  (post-walk-entity #(if (stored-entity? %)
                             (in-different-store % store)
                             %)
                          entity))

(defn immutable-to-list-generator [object-to-list]
  "Internal function that takes an object to list function and returns a
  function from immutable entity and element to skip to a list,
  handling objects with the object to list function. The
  object-to-list function must also take an object and an element to
  skip.
  The element to skip only has an effect when converting an non-interned
  object. In that case, an element of the object with the same key
  will not be shown. This avoids an infinite loop when an non-interned
  object has a relation to another non-interned object, and showing all
  elements of both objects would bounce back and forth between them
  forever."
  ;; Note: We tried using a letfn here, so we didn't have to pass in
  ;; the object-transformer each time we called ourselves
  ;; recursively. But that resulted in a compile error, where the
  ;; letfn definition was not available deep inside.
  (fn [entity skipped-element]
    (let [recurse (immutable-to-list-generator object-to-list)]
      (cond
        (primitive? entity) entity
        (object? entity) (object-to-list entity skipped-element)
        true (make-element-list (orientation entity)
                                (recurse (content entity) entity)
                                (map #(recurse % nil) (elements entity)))))))

(defn immutable-object-to-list [object skipped-element]
  (if (not (interned-object? object))
    (let [recurse (immutable-to-list-generator immutable-object-to-list)]
      (make-object-list (map #(recurse % nil)
                             ;; We rely on entity-key ignoring
                             ;; orientation, so that the two
                             ;; orientations of a relation will match.
                             (remove #(= (entity-key %)
                                         (entity-key skipped-element))
                                     (elements object)))))
    object))

(defn to-list [entity]
  "Return a list form of the entity."
  (if (mutable-entity? entity)
    ;; We want to run with updating-immutable, relative to an
    ;; immutable store, but for objects, we want to return the
    ;; corresponding object from the mutable store.
    (expr-let [immutable (updating-immutable entity)]
      ((immutable-to-list-generator
        (fn [object skipped-element] (if (stored-entity? object)
                                       (in-different-store object entity)
                                       object)))
       immutable nil))
    ((immutable-to-list-generator immutable-object-to-list)
     entity nil)))

(defn entity-complexity
  "Return the complexity of the element, which is the total number of
   elements, sub-elements, etc, with sub-elements counting less."
  [item]
  (let [content (content item)
        content-is-interned (interned-object? content)
        all-elements (cond-> (elements item)
                       (and (object? content) (not content-is-interned))
                       (concat (elements content)))]
    ;; We count generic content less, but enough that a generic
    ;; content plus an element is considered higher complexity than a
    ;; primitive.
    (+ (if (object? content)
         ;; A non-interned object counts like a primitive, while a
         ;; non-interned object get its elements counted, plus a bit.
         (if content-is-interned 1.0 0.3)
         (get {nil 0.3   'anything 0.3   "" 0.4}
              content 1.0))
       (* 0.75 (apply + (map entity-complexity all-elements))))))

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



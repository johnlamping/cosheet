(ns cosheet.entity
  (:require (cosheet [calculator :refer [current-value]]
                      [reporter-macros :refer [let-R]]
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

;;; Elements give a way of overlaying a hierarchical structure on
;;; information in a store, or on information that might be stored
;;; there. That makes them more convenient than just a bunch of
;;; links. They give a convenient way to describe:
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
;;; addition, a link can only be an element in both directions if both
;;; its source and target are objects. In other words, only
;;; relationships between objects are reversible. All other links only
;;; represent elements where their source is the content.

;;; This, in turn, means that the elements of an object correspond to
;;; all links that have it as a target, plus all links that have it as
;;; a source and that have an object as their target.

;;; That still allows for circularity in the element
;;; structure. Trivially, if two objects have a link between them,
;;; there an element going each way. And cycles can be formed without
;;; reversing links; there can be a link from object A to object B,
;;; and another one from B to A. And there can be larger cycles, as
;;; well. And the linkages can be more indirect, where object A has an
;;; element with a sub-element with content object B, etc.

;;; This means that any traversal of an entity structure has to avoid
;;; looping forever through circularities. And it should also avoid
;;; re-processing objects that can be reached via multiple paths. The
;;; repetition-avoiding-threaded-traverse function handles these cases in
;;; a general enough way to meet most needs. It is built on the more
;;; basic threaded-traverse function which threads user data through a
;;; depth first traversal; that enables repeat avoidance, without
;;; committing to a particular approach.

;;; Elements are normally accessed in terms of content, orientation,
;;; and sub-elements. But elements that are associated with stores
;;; also support three other methods. The target-entity method takes
;;; an element and returns the entity corresponding to its target,
;;; independent of its orientation. The originating-entity method
;;; returns the entity at the opposite endpoint from the one holding
;;; the content. And the containing-elements method takes an entity
;;; and returns the elements that have it as its their content and
;;; have orientation :source.

;;; Since some uses of entities require making an entity that is not
;;; in the store. For example, it could be a pattern to query for in
;;; the store, or it could be a description of an entity that should
;;; be added to the store. For these uses, there is a representation
;;; of entities, called the tree form, that is largely independent of
;;; stores.

;;; The tree form of primitives is just the primitive, since they are
;;; already independent of stores. The tree form of mutable objects
;;; and named objects is their normal element form. That provides the
;;; support for the object methods, while also including their id. And
;;; one of these objects in a query matches a subject object iff the
;;; two have the same id (independent of which store they are in).

;;; The tree form of anonymous objects needs to be able to express
;;; circularity and sharing across multiple paths. So, even though a
;;; tree form is a tree, a location in that tree needs a way to
;;; reference objects elsewhere in the tree.

;;; Anonymous objects that don't need to be referenced more than once
;;; don't have to worry about this, and have a tree form that includes
;;; just their elements:
;;;    [:object element element ...]

;;; But anonymous objects that can be referenced more than once are
;;; represented by a a conflux-tree-object, which has an id that can
;;; be used to reference it:
;;;   [:conflux-object identifier element element ...]
;;; For each id, only the first conflux-tree-object with that id in
;;; the tree form (in depth first order) will include the elements of
;;; the object; all other occurrences will have only the id, acting as
;;; a reference to the first occurrence.

;;; The other nuance to handle circularity is that links between
;;; objects are recorded in only one direction in the tree form, from
;;; the first encountered object (in depth first order) to the latter
;;; encountered object.

;;; While a tree form can describe a circular structure, or one with
;;; objects shared across multiple paths, navigating down the tree
;;; only keeps the information that's in the subtree, so it may no
;;; longer be able to construct the full structure. On the other hand,
;;; a tree form built afresh from the new node would have all the
;;; necessary information.

;;; Elements need to indicate which direction they are traversing a
;;; link. The most general tree form of an element is
;;;   ((orientation content) element element ...)
;;; where orientation is either :source or :target, to indicate which
;;; endpoint holds the content.

;;; But the most general form isn't always necessary. If an element
;;; has orientation :source, and its content is not a list, then one
;;; of two simplified forms is possible.
;;;    * If its content is a primitive and it has no sub-elements of
;;;      its own, then its tree form is just its content.
;;;    * If it does have sub-elements of its own, then its tree form is
;;;      (content element element ...)
;;; Combinding these simplified forms yield compact tree
;;; representations of the most common kinds of elements, like
;;;   (5 "value" (3 "x") (4 "y"))

;;; This means that the tree form of an element that consists of
;;; nothing but a primitive content is just that content. So
;;; converting to tree form can change an entity from an element to a
;;; primitive.  To reduce the problems from that, primitives return
;;; the same answers to content, orientation, and elememts as would an
;;; element consisting of just that primitive as its content. That is,
;;; they return themselves for their content, :souce for their
;;; orientation, nil for their elements.

;;; We can't use the same trick for the tree form of an element
;;; consisting of nothing but an object, because objects are defined
;;; to not have any content.

;;; As mentioned earlier, if a conflux tree object with a given id
;;; appears in a tree form, only its first occurrence in depth first
;;; order records the elements of the object. This still gives depth
;;; first traversals all the information they need the first time they
;;; encounter a given conflux id. But that depth first order depends
;;; on the order of elements at each node. So to make the order
;;; repeatable, the tree form fixes the order of elements to be the
;;; order they appear in the representation. That fixes the depth
;;; first order for entities defined by a tree form.

;;; In contrast, the order of elements defined by a store is
;;; undefined, so there is no defined depth first order for entities
;;; defined by a store; it might change from traversal to
;;; traversal. That means that there is no unique tree form
;;; corresponding to a particular entity defined by a store. The
;;; canonical namespace has functions to help with this.

;;; If it turns out that tree forms of links are also necessary, they
;;; should be
;;;    [:link source target element element ...]

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

  (entity-key [this]
    "Return the key of this entity. For stored entities, it is their
    item-id. For conflux-tree-objects, it is their id. For all other
    entities it is the entity, itself.

    Since matching of store objects is independent of the store, named
    objects can be matched even if they are associated with different
    versions of the store, from after they were interned.

    Note that the key ignores the orientation of elements. Query
    matching relies on that to avoid infinite loops. The only other
    place the key is used is to compare objects, which have no
    orientation.")

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

  (originating-entity [this]
    "If the entity represents a link, return the entity corresponding
    to the endpoint of the link opposite the entity's content. That is,
    when the orientation is :source it is the target endpoint, and when
    the orientation is :target it is the source endpoint. If the
    opposite endpoint is a link, the resulting entity will be given
    orientation :source.")

  (containing-elements [this]
    "If the entity is an object, return all the elements that contain it
    and have orientation :source.")

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

(defn name-element?
  "Return true if the entity is an element that counts as a name."
  [entity]
  (and (element? entity)
       (seq (content->elements entity name-label))))

(defn universal-object?
  "Return true if the object is one of the universal objects:
  name-label, link-type, and object-type. The object can be in any
  store."
  [entity]
  (and (stored-entity? entity)
       (#{name-label-id link-type-id object-type-id} (:item-id entity))))

;;; There are several ways that objects might be identified.
;;; First, there are objects can be located in a store, based on their
;;; properties:
;;;          id-identified: The object's item-id is a know string,
;;;                         like "link-type"
;;;    uniquely-idenfified: The object is either id-identified or it
;;;                         has a name that isn't just the empty string.
;;;                         If it is not in the store, then stands for
;;;                         a similarly named object in the store.
;;;               interned: The object is uniquely identified, and stored.
;;;                         In tree form, these objects are
;;;                         represented by themselves.
;;;      presumed-interned: The object is uniquely identified, or it
;;;                         consists of an object it, but no store.
;;;                         In the second case, the object is presumed
;;;                         to have originated from an interned object,
;;;                         and then gotten its store dropped. When put
;;;                         in the context of a store, that store should
;;;                         have an interned object with the same id.
;;; Second, there are objects that might have several elements that
;;; reference them, but that don't have any notable identifying
;;; characteristics. They can only be referenced in terms of a numeric
;;; id.
;;;    conflux-uninterned-object: This is an object that is not
;;;                               presumed interned, but that supports
;;;                               a way of being identified for
;;;                               purposes of attaching multiple
;;;                               links. Any stored object that is nor
;;;                               presumed-interned qualifies, as does
;;;                               a conflux-tree-object.

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

(defn presumed-interned-object?
  "Return true if the entity, which must be immutable, is a uniquely
  identified object, and is stored, or is a stored object entity, but
  with no store given."
  [entity]
  (or (interned-object? entity)
      (and (stored-entity? entity)
           (object? entity)
           (nil? (:store entity)))))

(defn tree-entity?
  "Return true if the entity is safe to recurse through without a
  repetition-avoiding traversal: either it is not stored (so it is
  already in tree form), or it is a presumed-interned object (which
  is treated as atomic)."
  [entity]
  (or (not (stored-entity? entity))
      (presumed-interned-object? entity)))

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
   has the entity as its content be a label.
   An object is a label object if either:
     * it's id is name-label-id
     * it has an element whose content has an item-id of
      link-type-id or object-type-id
  (The first cast might look redundant, since the name-label object
  has a link-type element once a store gets its universal
  elements. But sometimes, a template uses a name-label object with a
  store of nil. So it won't have any elements yet.)"
  (and (object? entity)
       (or (= (entity-key entity) name-label-id)
           (link-type-object? entity)
           (object-type-object? entity))))

(defn label-element? [entity]
  "Return whether the entity counts as a label. A label is a link under
  which its target should be indexed, starting from either of the
  target's endpoints.
  A link is a label if its source is either
     * a keyword
     * a label object"
  (let [content (content entity)]
    (or (keyword? content)
        (label-object? content))))

(defn make-tree-element
  "Make the tree representation of the described entity, simplifying it
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

(defn make-tree-object
  "Make the tree representation of the described object."
  [elements]
  ;; Make sure the elements we are given respect the tree form.
  (assert (not-any? object? elements))
  (into [:object] elements))

(defn non-conflux-tree-object?
  "Return true if entity is the output of make-tree-object."
  [entity]
  (and (vector? entity)
       (= (first entity) :object)))

(defrecord
    ^{:doc
      "The id for conflux-tree-objects."}
    TreeId
    [number])

(defn make-tree-id
  "Turn an integer into a tree id."
  [n]
  ;; Integers are reserved for creation by the store
  (assert (integer? n))
  (->TreeId n))

(defn tree-id?
  "Return true if the argument is a tree id."
  [x]
  (instance? TreeId x))

(defn make-conflux-tree-object
  "Make a tree representation of an object that carries an id so that
   multiple references to the same object can be recognized as sharing
   identity (via entity-key)."
  [id elements]
  (assert (tree-id? id))
  (assert (not-any? object? elements))
  (into [:conflux-object id] elements))

(defn conflux-tree-object?
  "Return true if entity is the output of make-conflux-tree-object."
  [entity]
  (and (vector? entity)
       (= (first entity) :conflux-object)))

(defn tree-object?
  "Return trun if the entity is the output of make-tree-object or
  make-conflux-tree-object."
  [entity]
  (and (vector? entity)
       (#{:object :conflux-object} (first entity))))

(defn conflux-uninterned-object?
  "Return true if the entity is an object that might be the content of
  multiple elementts, even though it's not interned or even presumed
  interned."
  [entity]
  (or (conflux-tree-object? entity)
      (and (stored-entity? entity)
           (object? entity)
           (not (presumed-interned-object? entity)))))

(defn conflux-tree-object-id
  "Return the id of a conflux-tree-object."
  [entity]
  (assert conflux-tree-object? entity)
  (second entity))

(defn make-tree-object-copying-id
  "Given a template object and a seq of elements, Make a tree object
  with the given elements. It the template is a conflux-tree-object,
  then make the object a conflux-tree-object with the same
  identity. Otherwise, make it a plain tree object."
  [template elements]
  (if (conflux-tree-object? template)
    (make-conflux-tree-object (conflux-tree-object-id template) elements)
    (make-tree-object elements)))

(defn add-elements-to-entity
  "Add elements an entity, using a tree form for its top level if
  anything changed."
  [entity elements-to-add]
  (assert (tree-entity? entity))
  (if (empty? elements-to-add)
    entity
    (cond (element? entity)
          (make-tree-element (orientation entity)
                             (content entity)
                             (concat (elements entity) elements-to-add))
          (object? entity)
          (make-tree-object-copying-id
           entity (concat (elements entity) elements-to-add))
          :else (make-tree-element :source entity elements-to-add))))

(defn map-subparts
  "Run the function on each subpart of the entity, and reassemble the
  entity from the results. For an element entity, the subparts are its
  content and its elements; for a non presumed interned object, they
  are its elements. Interned objects and primitives are returned
  unchanged. The function must return the same kid of entity as it
  gets. If the function turns an element into nil, that element will
  be removed."
  [f entity]
  (cond (element? entity)
        (make-tree-element (orientation entity)
                           (f (content entity))
                           (keep f (elements entity)))
        (and (object? entity) (not (presumed-interned-object? entity)))
        (let [new-elements (keep f (elements entity))]
          (make-tree-object-copying-id entity new-elements))
        :else
        entity))

(defn pre-traverse-entity
  "Recursively run the function on all the elements of the entity, from
  the top down up, going through objects that are not presumed
  interned. The function must return the same kid of entity as it
  gets. If the function turns an element into nil, that element will
  be removed."
  [f entity]
  (assert (tree-entity? entity))
  (letfn [(recurse [e] (map-subparts recurse (f e)))]
    (recurse entity)))

(defn post-traverse-entity
  "Recursively run the function on all the elements of the entity and
  their content, from the leaves up, going through objects that are
  not presumed interned. The function must return an element or a
  primitive. If the function turns an element into nil, that element
  will be removed."
  [f entity]
  (assert (tree-entity? entity))
  (letfn [(recurse [e] (f (map-subparts recurse e)))]
    (recurse entity)))

(defn threaded-traverse-helper
  "Build the recursive worker for threaded-traverse. Given pre-fn
  and post-fn, return a function traverse [parent-entity
  original-entity caller-data] that performs the traversal and
  returns [entity caller-data]. The caller-data is opaque to
  traverse; pre-fn and post-fn determine its shape and meaning.
  post-fn may be nil, in which case the forward walk still happens
  and threads caller-data, but no tree form is assembled and every
  traverse call returns a nil entity."
  [pre-fn post-fn]
  (letfn [(traverse [parent-entity parent-caller-data
                     original-entity original-caller-data]
            (let [[entity caller-data] (pre-fn parent-entity original-entity
                                               parent-caller-data
                                               original-caller-data)]
              (cond
                (and (= entity :entity/omit)
                     (element? original-entity))
                ;; pre-fn dropped this element; don't descend or assemble.
                [:entity/omit caller-data]

                (or (presumed-interned-object? entity)
                    (primitive? entity))
                ;; Treat it as atomic.
                (if post-fn
                  (post-fn original-entity entity
                           original-caller-data caller-data)
                  [nil caller-data])

                (element? entity)
                (let [[new-content caller-data]
                      (traverse original-entity original-caller-data
                                (content entity) caller-data)
                      [new-elements caller-data]
                      (traverse-elements entity original-caller-data
                                         caller-data)]
                  (if post-fn
                    (let [assembled (make-tree-element (orientation entity)
                                                       new-content
                                                       new-elements)] 
                      (post-fn original-entity assembled
                               original-caller-data caller-data))
                    [nil caller-data]))

                :else
                ;; Object: Traverse the elements. Then if the entity
                ;; was a conflux-tree-object, put them in a
                ;; conflux-tree-object with the same identity,
                ;; otherwise put them in a tree-object.
                (let [[new-elements caller-data]
                      (traverse-elements entity original-caller-data
                                         caller-data)]
                  (if post-fn
                    (let [assembled (make-tree-object-copying-id
                                     entity new-elements)]
                      (post-fn original-entity assembled
                               original-caller-data caller-data))
                    [nil caller-data])))))

          (traverse-elements [entity original-caller-data caller-data]
            (reduce
             (fn [[results caller-data] child]
               (let [child (if (element? child) child (list child))
                     [r caller-data] (traverse entity original-caller-data
                                               child caller-data)]
                 (if (= r :entity/omit)
                   [results caller-data]
                   ;; Undo the primitive-wrap above when the
                   ;; traversal returned the wrapped element
                   ;; unchanged, so a primitive sub-element stays
                   ;; a primitive in the result.
                   (let [r (if (and (sequential? r)
                                    (= (count r) 1)
                                    (primitive? (first r)))
                             (first r)
                             r)]
                     [(conj results r) caller-data]))))
             [[] caller-data]
             (elements entity)))]
    traverse))

(defn threaded-traverse
  "Treats an entity as a graph, with elements corresponding to edges,
  while objects and primitives correspond to nodes. Does a depth
  first traversal of the graph, which logically overlays a tree on
  it. Returns a pair [tree-entity final-caller-data]: tree-entity is
  the assembled tree form, and final-caller-data is the caller-data
  that was threaded through the traversal.

  (An aside: the edges of the graph can have their own edges, which
  means that the traversal has to traverse edges of edges. This is
  not a standard structure, although RDF-star comes close.)

  The caller provides two functions, pre-fn and post-fn, which can
  modify the traversal or its resulting tree entity. In addition,
  caller data is threaded through all calls to those functions,
  which lets them accumulate information across the traversal.

  Presumed-interned objects are treated as atomic and are not
  descended into. Their item-id identifies them across references,
  and the purpose of the tree form is to represent entities that may
  not be in a store.

  Everything else is traversed. And threaded-traverse, itself, does
  not detect cycles or make repeated references to the same
  non-presumed-interned object reference the same result: an input
  that contains either will cause it to loop or produce a tree with
  duplicated subtrees. pre-fn and post-fn are responsible for handling
  these cycles and sharing, since different callers may want to handle
  them in different ways.

  One way is provided here: wrap pre-fn with
  wrap-pre-fn-with-repetition-avoidance and post-fn with
  wrap-post-fn-with-repetition-avoidance, and supply caller-data wrapped
  with wrap-caller-data-with-repetition-avoidance-data.  Together those
  wrappers will emit a conflux-tree-object the first time a repeated
  object is reached and bare references thereafter.

  The pre-fn is called just before an entity is traversed, passing in
  the parent entity, the entity, the parent caller data, and the
  caller data; it must return a pair of a revised entity and revised
  caller data. The parent entity is nil for the top-level call and the
  original entity of the enclosing traversal for nested calls.  The
  parent caller data is the caller data as it was when the parent
  entity was passed to traverse (and is nil for the top-level call).
  The traversal will then proceed from the revised entity, rather than
  on the original one, and the revised caller data will be passed on
  to the next invocation of a caller provided function. If the entity
  is an element, pre-fn may return :entity/omit for the revised
  entity, which tells the traversal to behave as if the entity weren't
  there: don't traverse it or include it in the resulting tree
  entity. In the case where an element would ordinarily be represented
  by a primitive, pre-fn will be passed a full element, not the
  primitive, so it will know it is working on an element, and may
  return :entity/omit.

  The post-fn is called after the traversal has finished for an
  entity. It gets passed the original entity, a tree entity the
  traversal assembles from the traversals of the entity's parts, and
  both the original caller data for the entity being traversed and the
  caller data after the traversal of the entity. The post-fn must
  returns a pair of the revised tree entity and the revised caller
  data. As a rule, its revised caller data will be based on the latest
  caller data, but it might want to use the original caller data in
  deciding what to do. If the post-fn is called with a tree element,
  it also has the option to return :entity/omit as the revised
  element. In that case the tree element will be thrown away. Also,
  just as for pre-fun, in the case where an element would ordinarily
  be represented by a primitive, post-fn will be passed a full
  element.

  post-fn may be nil. In that case the forward walk still happens and
  caller-data is still threaded through, but no tree form is assembled
  and only the final caller-data is returned. This supports code that
  traverses purely for their effect on caller-data."
  [entity pre-fn post-fn caller-data]
  (let [result ((threaded-traverse-helper pre-fn post-fn)
                nil nil entity caller-data)]
    (if post-fn
      result
      (second result))))

(defn identity-pre-fn
  "An identity pre-fn for threaded-traverse: returns its entity and
  caller-data unchanged."
  [_ entity _ caller-data]
  [entity caller-data])

(defn identity-post-fn
  "An identity post-fn for threaded-traverse: returns its entity and
  caller-data unchanged."
  [_ entity _ caller-data]
  [entity caller-data])

(defn record-encounter
  "Update a seen map for entering an entity: the first time a
  conflux-uninterned-object is encountered, assign a fresh tree-id (drawn from
  :next-number) so that each time the entity is encountered, the
  traversal will make a conflux-tree-object with that id."
  [seen entity]
  (if-not (conflux-uninterned-object? entity)
    seen
    (let [k (entity-key entity)]
      (if (contains? seen k)
        seen
        (-> seen
            (assoc k (make-tree-id (:next-number seen)))
            (update :next-number inc))))))

(defn call-user-fn-adding-seen
  "Call user-fn with the remaining arguments, expect it to return
  [user-entity new-user-data], and assemble the final result
  [user-entity [new-user-data seen-out]] expected by callers of the
  repetition-avoidance wrappers."
  [seen-out user-fn & args]
  (let [[user-entity new-user-data] (apply user-fn args)]
    [user-entity [new-user-data seen-out]]))

(defn wrap-caller-data-with-repetition-avoidance-data
  "Wrap user caller-data with the bookkeeping state that
  wrap-pre-fn-with-repetition-avoidance and
  wrap-post-fn-with-repetition-avoidance expect.

  If starting-entity is a stored element, the endpoint opposite its
  content is examined: when that endpoint is a conflux-uninterned
  object, its entity-key is pre-populated into the initial seen map
  (with value nil). This prevents the traversal from going through
  the object that starting-entity is an element of -- any back-link
  from a freshly entered descendant object to that ancestor will
  have its content already in seen and so will be dropped."
  [starting-entity user-data]
  (let [originating (when (stored-entity? starting-entity)
                      (originating-entity starting-entity))
        seen (record-encounter {:next-number 1} originating)]
    [user-data seen]))

(defn extract-caller-data-from-repetition-avoidance-data
  "Return the user caller-data from a repetition-avoidance caller-data
  pair (as produced by wrap-caller-data-with-repetition-avoidance-data)."
  [repetition-avoidance-data]
  (first repetition-avoidance-data))

(defn wrap-pre-fn-with-repetition-avoidance
  "Use together with wrap-post-fn-with-repetition-avoidance.
  Wrap a user pre-fn so that the resulting pre-fn prevents
  threaded-traverse from descending into cycles or repeated references
  to the same non-presumed-interned object. The wrapped function
  expects caller-data of the form [user-data seen] and threads
  user-data through the user pre-fn. seen is a map that records every
  non-presumed-interned object that has been entered
  (see record-encounter). When the wrapped pre-fn would otherwise
  cause traversal to revisit such an object, it intervenes before
  calling user-pre-fn:
    - For an element of a conflux parent whose content has already
      been seen, it returns :entity/omit so the element is dropped;
      the element will be traversed in the other direction.
    - For a non-presumed-interned object, it looks to see if its key
      has already been assigned a conflux id. If not, it assigns a key
      and makes a conflux-tree-object with that key and all the
      elements of the original object. If there already was an id, it
      makes a conflux-tree-object that id, but no elements, so there
      will be nothing for the traversal to descend further into.
  It intervenes before calling user-pre-fn, so that that function will
  never be called with something that should be avoided."
  [user-pre-fn]
  (fn [parent-entity entity [parent-user-data parent-seen] [user-data seen]]
    (cond
      (and (element? entity)
           (conflux-uninterned-object? parent-entity)
           (conflux-uninterned-object? (content entity))
           (contains? parent-seen (entity-key (content entity))))
      ;; This link has or will be handled from the other direction.
      ;; Drop the element without consulting user-pre-fn.
      [:entity/omit [user-data seen]]

      (conflux-uninterned-object? entity)
      ;; Record the encounter and substitute a conflux-tree-object
      ;; whose id is the value record-encounter assigned, carrying the
      ;; original entity's elements on first encounter and no elements
      ;; on subsequent encounters (so the traversal will not descend
      ;; into the object's elements again).
      (let [orig-had-key? (contains? seen (entity-key entity))
            new-seen (record-encounter seen entity)
            new-tree (make-conflux-tree-object
                      (get new-seen (entity-key entity))
                      (if orig-had-key? [] (elements entity)))]
        (call-user-fn-adding-seen
         new-seen user-pre-fn parent-entity new-tree
         parent-user-data user-data))

      :else
      (call-user-fn-adding-seen
       seen user-pre-fn parent-entity entity
       parent-user-data user-data))))

(defn wrap-post-fn-with-repetition-avoidance
  "Wrap a user post-fn so that the resulting post-fn links all multiple
  visits to a non-presumed-interned object together by giving them a
  shared id. The wrapped function expects caller-data of the
  form [user-data seen] and threads user-data through the user
  post-fn. seen is the same map maintained by
  wrap-pre-fn-with-repetition-avoidance. When the user post-fn's result is
  a plain tree-object, and a key was recorded for the original object
  before that object was reached, the wrapper returns a
  conflux-tree-object carrying that id.  The user-post-fn is called
  after the substitution.  If the user-post-fn is nil, just return
  nil; don't wrap."
  [user-post-fn]
  (when user-post-fn
    (fn [original-entity assembled [orig-user _] [new-user new-seen]]
      (let [result (if-let
                       [id (when (non-conflux-tree-object? assembled)
                             (get new-seen (entity-key original-entity)))]
                     (make-conflux-tree-object id (elements assembled))
                     assembled)]
        (call-user-fn-adding-seen
         new-seen user-post-fn original-entity result orig-user new-user)))))

(defn repetition-avoiding-threaded-traverse
  "Do a threaded traversal that avoids descending into cycles or into
  the elements of a non-presumed-interned object more than once. Each
  link will be traversed exactly once. Each node will be traversed
  exactly once with its elements, while the other traversals will have
  no objects."
  [entity pre-fn post-fn caller-data]
  (let [[tree caller-data]
        (threaded-traverse
         entity
         (wrap-pre-fn-with-repetition-avoidance pre-fn)
         (wrap-post-fn-with-repetition-avoidance post-fn)
         (wrap-caller-data-with-repetition-avoidance-data entity caller-data))]
    [tree (extract-caller-data-from-repetition-avoidance-data caller-data)]))

(defn recursively-in-different-store
  "Recursively traverse the entity, changing the store of all
  presumed-interned objects to the given store."
  [entity store]
  (let [post-fn (fn [original assembled _ caller-data]
                  [(if (presumed-interned-object? original)
                     (in-different-store original store)
                     assembled)
                   caller-data])
        [tree _] (repetition-avoiding-threaded-traverse
                  entity identity-pre-fn post-fn nil)]
    tree))

(defn convert-unneeded-conflux-tree-objects
  "Threaded traverse tree, counting occurrences of each
  conflux-tree-object id, then post walk it, demoting any
  conflux-tree-object whose id appears only once to a plain
  tree-object."
  [tree]
  (assert (tree-entity? tree))
  (let [count-conflux-pre-fn (fn [_ e _ cd]
                               [e (cond-> cd
                                    (conflux-tree-object? e)
                                    (update (conflux-tree-object-id e)
                                            (fnil inc 0)))])
        counts (threaded-traverse tree count-conflux-pre-fn nil {})
        convert-unshared (fn [e] (if (and (conflux-tree-object? e)
                                          (= (get counts
                                                  (conflux-tree-object-id e))
                                             1))
                                   (make-tree-object (elements e))
                                   e))] 
    (post-traverse-entity convert-unshared tree)))

(defn to-tree
  "Return a tree form of the entity. Cycles and repeated references
  to the same non-presumed-interned object are coalesced via
  conflux-tree-objects; objects that are referenced only once
  collapse back to plain tree-objects."
  [entity]
  ;; A repetition-avoiding traversal builds the tree, producing a
  ;; conflux-tree-object for every non-presumed-interned object. The
  ;; post-fn flips user-data to true if any conflux-tree-object is
  ;; produced.  If none was, no further work is needed; otherwise hand
  ;; off to convert-unneeded-conflux-tree-objects to demote any that
  ;; turn out to be referenced only once.
  (let [conflux-post-fn (fn [_ e _ cd]
                          [e (or cd (conflux-tree-object? e))])
        [assembled needs-cleanup? ]
        (repetition-avoiding-threaded-traverse
         entity identity-pre-fn conflux-post-fn false)]
    (cond-> assembled
      needs-cleanup? convert-unneeded-conflux-tree-objects)))

(defn entity-complexity
  "Return the complexity of the entity, a measure of the total number
   of elements, sub-elements, etc, with sub-elements counting less.
   Repeated references to a non-presumed-interned object only count
   its elements once."
  [entity]
  (letfn [(post-fn [original assembled original-cd cd-after-children]
            (if (element? original)
              ;; The element's contribution to its parent's
              ;; children-sum is the self-cost of its content plus a
              ;; discounted sum of its own children's contributions.
              (let [c (content original)
                    content-cost (if (object? c)
                                   (if (presumed-interned-object? c) 1.0 0.3)
                                   (get {nil 0.3 'anything 0.3 "" 0.4} c 1.0))]
                [:entity/omit ; We don't need this element any more. 
                 (+ original-cd
                    content-cost
                    (* 0.75 (- cd-after-children original-cd)))])
              ;; Everything besides entities pass their children's sum
              ;; through unchanged: the element that has them as
              ;; content uses that sum directly.
              [assembled cd-after-children]))]
    ;; Since only elements add to the complexity, we need to wrap a
    ;; non-element in an element.
    (let [wrapped (if (element? entity) entity (list entity))
          [_ complexity] (repetition-avoiding-threaded-traverse
                          wrapped identity-pre-fn post-fn 0)]
      complexity)))

(defn label->element
  "Return the element with the given label.
  There must be at most one such element."
  [entity label]
  (let-R [elements (label->elements entity label)]
    (when elements
      (assert (= (count elements) 1)
              (apply str "entity "  (:id (:item-id entity))
                     " has " (count elements) " elements for label " label
                     " entity contents: " (current-value (to-tree entity))
                     " element contents: "
                     (interleave (repeat " ")
                                 (map #(current-value (content %)) elements))))
      (first elements))))

(defn label->content
  "Return the content of the element with the given label.
   There must be at most one such element."
  [entity label]
  (let-R [element (label->element entity label)]
    (content element)))


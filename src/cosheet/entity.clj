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
;;; addition, at least for now, a link can only be an element in both
;;; directions if both its source and target are objects. In other
;;; words, only relationships between objects are reversible. All
;;; other links only represent elements where their source is the
;;; content.

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

;;; This means that any traversal of an entity structure has to handle
;;; circularity. The threaded-traversal function handles cyclic
;;; structure in a general enough way to meet most needs.

;;; The case where the source is a link may be supported later. That
;;; is a pretty big change because it means that content of an element
;;; could be another element, which is viewed from neither its link's
;;; source or target, but from one of the links to it.

;;; Elements are normally accessed in terms of content, orientation,
;;; and sub-elements. But elements that are associated with stores
;;; also support two other methods. The target-entity method takes an
;;; element and returns the entity corresponding to its target,
;;; independent of its orientation. And the containing-elements method
;;; takes an entity and returns the elements that have it as its their
;;; content and have orientation :source.

;;; Since some uses of entities require creating an entity that
;;; doesn't exactly match any entity in the store, there is a
;;; representation of entities that is largely independent of stores,
;;; called the tree form.

;;; The tree form of primitives is just the primitive, since they are
;;; already independent of stores. The tree form of mutable objects
;;; and named objects is their normal element form. That provides the
;;; support for the object methods, while also including their id. And
;;; one of these objects in a query matches a subject object iff the
;;; two have the same id (independent of which store they are in).

;;; The tree form of anonymous objects needs to deal with need to
;;; represent circularity. That is handled by giving anonymous objects
;;; ids if necessary. If an anonymous object needs to be referenced
;;; more than once, a shareable-tree-object is used for it. This has
;;; the form
;;;   [:sharaeble-object identifier element element ...]
;;; only its first occurrence in the tree form (in depth first order)
;;; will include the elements of the object, while; all other
;;; occurrences will have only the id.

;;; Anonymous objects that don't need to be referenced more than once
;;; have a tree form that includes just their elements:
;;;    [:object element element ...]

;;; The other nuance to handle circularity is that links between
;;; objects are recorded in only one direction in the tree form, from
;;; the first encountered object (in depth first order) to the latter
;;; encountered object.

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

;;; Note that the definition of depth first order requires an ordering
;;; on the elements of entities. While the ordering of elements is
;;; undefined, in general, for the purpose of traversing tree
;;; entities, the order is defined to be the order the elements appear
;;; in the entities. Only the traversal code relies on that.

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
    item-id. For shareable-tree-objects, it is their id. For all other
    entities it is the entity, itself.

    Since matching of store objects is independent of the store, named
    objects can be matched even if they are associated with different
    versions of the store, from after they were interned.

    Note that the key ignores the orientation of elements. Both query
    matching and to-tree rely on that to avoid infinite loops. The
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

(defrecord
    ^{:doc
      "The id for shareable-tree-objects."}
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

(defn make-shareable-tree-object
  "Make a tree representation of an object that carries an id so that
   multiple references to the same object can be recognized as sharing
   identity (via entity-key)."
  [id elements]
  (assert (tree-id? id))
  (assert (not-any? object? elements))
  (into [:shareable-object id] elements))

(defn shareable-tree-object?
  "Return true if entity is the output of make-shareable-tree-object."
  [entity]
  (and (vector? entity)
       (= (first entity) :shareable-object)))

(defn shareable-uninterned-object?
  "Return true if the entity is an object whose identity can be
  recognized across multiple references without being interned: either
  a shareable tree-object (which carries an explicit id) or a stored
  object that is not presumed-interned."
  [entity]
  (or (shareable-tree-object? entity)
      (and (stored-entity? entity)
           (object? entity)
           (not (presumed-interned-object? entity)))))

(defn add-elements-to-entity
  "Add elements an entity, using a tree form for its top level if
  anything changed."
  [entity elements-to-add]
  (if (empty? elements-to-add)
    entity
    (cond (element? entity)
          (make-tree-element (orientation entity)
                             (content entity)
                             (concat (elements entity) elements-to-add))
          (object? entity)
          (make-tree-object (concat (elements entity) elements-to-add))
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
        (make-tree-object (keep f (elements entity)))
        :else
        entity))

(defn pre-walk-entity
  "Recursively run the function on all the elements of the entity, from
  the top down up, going through objects that are not presumed
  interned. The function must return the same kid of entity as it
  gets. If the function turns an element into nil, that element will
  be removed."
  [f entity]
  (map-subparts #(pre-walk-entity f %) (f entity)))

(defn post-walk-entity
  "Recursively run the function on all the elements of the entity and
  their content, from the leaves up, going through objects that are
  not presumed interned. The function must return an element or a
  primitive. If the function turns an element into nil, that element
  will be removed."
  [f entity]
  (f (map-subparts #(post-walk-entity f %) entity)))

(defn record-encounter
  "Update a seen map for entering an entity: the first time a
  shareable-uninterned-object is encountered, add its key with value
  nil; on a later encounter, assign a fresh tree-id (drawn from
  :next-number) so the construction step knows to produce a shareable
  form with that id."
  [seen entity]
  (if-not (shareable-uninterned-object? entity)
    seen
    (let [k (entity-key entity)]
      (cond
        (not (contains? seen k)) (assoc seen k nil)
        (nil? (get seen k)) (-> seen
                                (assoc k (make-tree-id
                                          (:next-number seen)))
                                (update :next-number inc))
        :else seen))))

(defn wrap-caller-data-with-loop-avoidance-data
  "Wrap user caller-data with the bookkeeping state that
  wrap-pre-fn-with-loop-avoidance and
  wrap-post-fn-with-loop-avoidance expect."
  [user-data]
  [user-data {:next-number 1}])

(defn wrap-pre-fn-with-loop-avoidance
  "Wrap a user pre-fn so that the resulting pre-fn prevents
  threaded-traversal from descending into cycles or repeated
  references to the same non-presumed-interned object. The wrapped
  function expects caller-data of the form [user-data seen] and
  threads user-data through the user pre-fn. seen is a map that
  records every non-presumed-interned object that has been entered
  (see `record-encounter`). When the wrapped pre-fn would otherwise
  cause traversal to revisit such an object, it short-circuits:
    - for an element of a shareable parent whose content has already
      been seen, it returns a nil entity so the element is dropped;
    - for a non-presumed-interned object whose key has already been
      seen, it returns a plain tree-object with no elements so the
      traversal does not descend into it.
  Use together with wrap-post-fn-with-loop-avoidance."
  [user-pre-fn]
  (fn [parent-entity original-entity [user-data seen]]
    (let [[user-entity new-user-data] (user-pre-fn parent-entity
                                                   original-entity user-data)
          drop? (and (element? original-entity)
                     (shareable-uninterned-object? parent-entity)
                     (shareable-uninterned-object? (content original-entity))
                     (contains? seen
                                (entity-key (content original-entity))))
          object-non-interned (and (object? original-entity)
                                   (not (presumed-interned-object?
                                         original-entity)))
          was-in (and object-non-interned
                      (contains? seen (entity-key original-entity)))
          new-seen (if object-non-interned
                     (record-encounter seen original-entity)
                     seen)
          entity (cond
                   drop? nil
                   was-in (make-tree-object [])
                   :else user-entity)]
      [entity [new-user-data new-seen]])))

(defn wrap-post-fn-with-loop-avoidance
  "Wrap a user post-fn so that the resulting post-fn links the
  multiple occurrences of a non-presumed-interned object together by
  giving them a shared id. The wrapped function expects caller-data
  of the form [user-data seen] and threads user-data through the
  user post-fn. seen is the same map maintained by
  wrap-pre-fn-with-loop-avoidance. When the user post-fn's
  result is a plain tree-object and seen has a non-nil tree-id for
  the original non-presumed-interned object's key (assigned at the
  point of the repeat reference), the wrapped function returns a
  shareable-tree-object carrying that id."
  [user-post-fn]
  (fn [original-entity assembled [orig-user _] [new-user new-seen]]
    (let [[user-result new-user-2] (user-post-fn original-entity assembled
                                                  orig-user new-user)
          result (if (and (vector? user-result)
                          (= :object (first user-result))
                          (object? original-entity)
                          (not (presumed-interned-object? original-entity))
                          (some? (get new-seen
                                       (entity-key original-entity))))
                   (make-shareable-tree-object
                    (get new-seen (entity-key original-entity))
                    (rest user-result))
                   user-result)]
      [result [new-user-2 new-seen]])))

(defn threaded-traversal-helper
  "Build the recursive worker for threaded-traversal. Given pre-fn
  and post-fn, return a function traverse [parent-entity
  original-entity caller-data] that performs the traversal and
  returns [entity caller-data]. The caller-data is opaque to
  traverse; pre-fn and post-fn determine its shape and meaning.
  post-fn may be nil, in which case the forward walk still happens
  and threads caller-data, but no tree form is assembled and every
  traverse call returns a nil entity."
  [pre-fn post-fn]
  (letfn [(traverse [parent-entity original-entity original-caller-data]
            (let [[entity caller-data] (pre-fn parent-entity original-entity
                                               original-caller-data)]
              (cond
                (and (nil? entity)
                     (element? original-entity))
                ;; pre-fn dropped this element; don't descend or assemble.
                [nil caller-data]

                (or (presumed-interned-object? entity)
                    (primitive? entity))
                ;; Treat it as atomic.
                (if post-fn
                  (post-fn original-entity entity
                           original-caller-data caller-data)
                  [nil caller-data])

                (element? entity)
                (let [[new-content caller-data]
                      (traverse original-entity (content entity) caller-data)
                      [new-elements caller-data]
                      (traverse-elements entity caller-data)]
                  (if post-fn
                    (post-fn original-entity
                             (make-tree-element (orientation entity)
                                                new-content
                                                new-elements)
                             original-caller-data caller-data)
                    [nil caller-data]))

                :else ;; Object: always assemble a plain tree-object.
                (let [[new-elements caller-data]
                      (traverse-elements entity caller-data)]
                  (if post-fn
                    (post-fn original-entity
                             (make-tree-object new-elements)
                             original-caller-data caller-data)
                    [nil caller-data])))))

          (traverse-elements [entity caller-data]
            (reduce
             (fn [[results caller-data] child]
               (let [child (if (element? child) child (list child))
                     [r caller-data] (traverse entity child caller-data)]
                 [(if (nil? r) results (conj results r)) caller-data]))
             [[] caller-data]
             (elements entity)))]
    traverse))

(defn threaded-traversal
  "Treats an entity as a graph, with elements corresponding to edges,
  while objects and primitives correspond to nodes. Does a depth
  first traversal of the graph, which logically overlays a tree on
  it. Returns a pair [tree-entity final-caller-data]: tree-entity is
  the assembled tree form, and final-caller-data is the caller-data
  that was threaded through the traversal.

  Presumed-interned objects are treated as atomic and are not
  descended into. Their item-id identifies them across references,
  and the purpose of the tree form is to represent entities that may
  not be in a store.

  threaded-traversal itself does not detect cycles or coalesce
  repeated references to the same non-presumed-interned object: an
  input that contains either will cause it to loop or produce a tree
  with duplicated subtrees. To traverse such an input, wrap pre-fn
  with wrap-pre-fn-with-loop-avoidance and post-fn with
  wrap-post-fn-with-loop-avoidance, and supply caller-data
  wrapped with wrap-caller-data-with-loop-avoidance-data.
  Together those wrappers will emit a shareable-tree-object the
  first time a repeated object is reached and bare references
  thereafter.

  (An aside: the edges of the graph can have their own edges, which
  means that the traversal has to traverse edges of edges. This is
  not a standard structure, although RDF-star comes close.)

  The caller provides two functions, pre-fn and post-fn, which can
  modify the traversal or its resulting tree entity. In addition,
  caller data is threaded through all calls to those functions,
  which lets them accumulate information across the traversal.

  The pre-fn is called just before an entity is traversed, passing in
  the parent entity, the entity, and the caller data, and must return
  a pair of a revised entity and revised caller data. The parent
  entity is nil for the top-level call and the original entity of the
  enclosing traversal for nested calls. The traversal will then
  proceed on the revised entity, rather than on the original one, and
  the revised caller data will be passed on to the next invocation of
  a caller provided function. If the entity is an element, pre-fn may
  return nil for the revised entity, which tells the traversal to
  behave as if the entity weren't there: don't traverse it or include
  it in the resulting tree entity. In the case where an element would
  ordinarily be represented by a primitive, pre-fn will be passed a
  full element, not the primitive, so it will know it can return nil.

  The post-fn is called after the traversal has finished for an
  entity. It gets passed the original entity, the tree entity the
  traversal has assembled, and both the original caller data for the
  entity being traversed and the caller data after the traversal of
  the entity. The post-fn must returns a pair of the revised tree
  entity and the revised caller data. As a rule, its revised caller
  data will be based on the latest caller data, but it might want to
  use the original caller data in deciding what to do. If the post-fn
  is called with a tree element, is also has the option to return a
  nil revised element. In that case the tree element will be thrown
  away. And just as for pre-fun, in the case where an element would
  ordinarily be represented by a primitive, post-fn will be passed a
  full element.

  post-fn may be nil. In that case the forward walk still happens
  and caller-data is still threaded through, but no tree form is
  assembled and the returned tree-entity is nil. This supports code
  that traverses purely for the side effect on caller-data."
  [entity pre-fn post-fn caller-data]
  ((threaded-traversal-helper pre-fn post-fn) nil entity caller-data))

(defn recursively-in-different-store
  "Recursively put all stored entities in the entity into a different store."
  [entity store]
  (post-walk-entity #(if (stored-entity? %)
                             (in-different-store % store)
                             %)
                          entity))

(defn internal-to-tree
  "Internal function to-tree that takes an element to skip, which only
  has an effect when converting an non-interned object. In that case,
  an element of the object with the same key will not be shown. This
  avoids an infinite loop when a non-interned object has a relation to
  another non-interned object, and showing all elements of both
  objects would bounce back and forth between them forever."
  [entity skipped-element]
  (cond
    (primitive? entity) entity
    (object? entity) (if (presumed-interned-object? entity)
                       entity
                       (make-tree-object
                        (map #(internal-to-tree % nil)
                             ;; We rely on entity-key ignoring
                             ;; orientation, so that the two
                             ;; orientations of a relation will match.
                             (remove #(= (entity-key %)
                                         (entity-key skipped-element))
                                     (elements entity)))))
    true (make-tree-element (orientation entity)
                            (internal-to-tree (content entity) entity)
                            (map #(internal-to-tree % nil) (elements entity)))))

(defn to-tree
  "Return a tree form of the entity."
  [entity]
  (internal-to-tree entity nil))

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


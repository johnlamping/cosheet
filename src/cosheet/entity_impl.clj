(ns cosheet.entity-impl
  (:require (cosheet [store :refer [atom-description?
                                    id-label->element-ids
                                    id->element-ids
                                    id->content id->subject
                                    id->content-reference
                                    call-dependent-on-id
                                    mutable-store?
                                    stored-item-id-string]]
                     [orderable :as orderable]
                     [entity :refer :all]
                     [expression :refer [expr-seq expr-let expr]])))

(defrecord
    ^{:doc "An item whose elements are described by a store."}
    StoredItem

  [store     ; The immutable store that holds the information for this item.
   item-id]  ; The ItemDescription of the item in the store.

  StoredEntity

  (subject [this]
    (when-let [subject-id (id->subject store item-id)]
      (description->entity subject-id store)))

  (in-different-store [this entity-with-store]
    (description->entity (:item-id this) (:store entity-with-store)))
  
  Entity

  (mutable-entity? [this] false)

  (atom? [this] (atom-description? item-id))

  (label->elements [this label]
    (seq (for [element-id (id-label->element-ids store item-id label)]
           (description->entity element-id store))))

  (elements [this]
    (seq (for [element-id (id->element-ids store item-id)]
           (description->entity element-id store))))

  (content [this]
    (description->entity (id->content store item-id) store))

  (content-reference [this]
    (description->entity (id->content-reference store item-id) store))

  (call-with-immutable [this fun] (fun this)))

(defrecord
    ^{:doc "An item whose elements are described by a mutable store."}
    MutableStoredItem

    [store     ; The mutable store that holds the information for this item.
     item-id]  ; The ItemDescription of the item in the store.

  StoredEntity

  (subject [this]
    (when-let [subject-id (id->subject store item-id)]
      (description->entity subject-id store)))

  (in-different-store [this entity-with-store]
    (description->entity (:item-id this) (:store entity-with-store)))

  Entity

  (mutable-entity? [this] true)

  (atom? [this?] (atom-description? item-id))

  (label->elements [this label]
    (expr-let [element-ids (id-label->element-ids store item-id label)]
      (seq (for [element-id element-ids]
             (description->entity element-id store)))))

  (elements [this]
    (expr-let [element-ids (id->element-ids store item-id)]
      (seq (for [element-id element-ids]
             (description->entity element-id store)))))

  (content [this]
    (expr-let [content (id->content store item-id)]
      (description->entity content store)))

  (content-reference [this]
    (expr-let [reference (id->content-reference store item-id)]
      (description->entity reference store)))

  (call-with-immutable [this fun]
    (call-dependent-on-id
     store item-id  #(fun (description->entity item-id %)))))

;;; Make a list work as an item. The format is (content element
;;; element...) We use ISeq, because, for example, while '(1 2) is a
;;; PersistentList, `(1 2) is a Cons.
(extend-type clojure.lang.ISeq
  
  Entity

  (mutable-entity? [this] false)

  (atom? [this] false)

  (label->elements [this label]
    (seq (filter #(some (partial = label)
                        (map atomic-value (elements %)))
                 (elements this))))

  (elements [this] (seq (rest this)))

  (content [this] (first this))

  (content-reference [this] (first this))

  (call-with-immutable [this fun] (fun this)))

(extend-protocol Entity
  clojure.lang.Keyword
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  (call-with-immutable [this fun] (fun this))
  clojure.lang.Symbol
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  (call-with-immutable [this fun] (fun this))
  java.lang.String
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  (call-with-immutable [this fun] (fun this))
  java.lang.Number
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  (call-with-immutable [this fun] (fun this))
  java.lang.Boolean
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  (call-with-immutable [this fun] (fun this))
  cosheet.orderable.Orderable
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  (call-with-immutable [this fun] (fun this))

  nil ;; For convenience in null punning
  (mutable-entity? [this] false)
  (atom? [this] true)
  (label->elements [this label] nil)
  (elements [this] nil)
  (content [this] this)
  (content-reference [this] this)
  (call-with-immutable [this fun] (fun this))
)

(extend-protocol Description
  cosheet.store.StoredItemDescription
  (description->entity [this store]
    (if (mutable-store? store)
      (->MutableStoredItem store this)
      (->StoredItem store this)))
  clojure.lang.Keyword
  (description->entity [this store] this)
  clojure.lang.Symbol
  (description->entity [this store] this)
  java.lang.String
  (description->entity [this store] this)
  java.lang.Number
  (description->entity [this store] this)
  java.lang.Boolean
  (description->entity [this store] this)
  cosheet.orderable.Orderable
  (description->entity [this store] this)
  nil
  (description->entity [this store] nil) ;; For convenience in null punning
)

;;; TODO: We don't need two versions of each of these any more,
;;; because expr is smart about returning a reporter or not.
(defmethod label->content false [entity label]
  (let [elements (label->elements entity label)]
    (when elements
      (assert (= (count elements) 1)
              (apply str "entity "  (:id (:item-id entity))
                     " has " (count elements) " elements for label " label
                     " contents: "
                     (interleave (repeat " ") (map content elements))))
      (content (first elements)))))

(defmethod label->content true [entity label]
  (expr-let [elements (expr label->elements entity label)]
    (when elements
      (assert (= (count elements) 1)
              (str "entity "  (:id (:item-id entity))
                   " has " (count elements) " elements for label " label))
      (content (first elements)))))

(defmethod atomic-value false [entity]
  (cond (nil? entity) nil
        (atom? entity) (content entity) ; Could be implicit content reference.
        :else (atomic-value (content entity))))

(defmethod atomic-value true [entity]
  (if (nil? entity)
    nil
    (expr-let [content (expr content entity)]
      (if (atom? content)
        content
        (atomic-value content)))))

(defmethod label->atomic-values false [entity label]
  (map atomic-value (label->elements entity label)))

(defmethod label->atomic-values true [entity label]
  (expr-let [elements (expr label->elements entity label)]
    (expr-seq map atomic-value elements)))

(defmethod label-has-atomic-value? false [entity label value]
  (some (partial = value)
        (label->atomic-values entity label)))

(defmethod label-has-atomic-value? true [entity label value]
  (expr-let [atomics (expr label->atomic-values entity label)]
    (some (partial = value) atomics)))

(defmethod to-list true [entity]
  (if (atom? entity)
    (atomic-value entity)
    (expr-let [content (content entity)
               elements (elements entity)]
      (if (empty? elements)
        content
        (expr-let [element-lists (expr-seq map to-list elements)]
          (cons content element-lists))))))

(defmethod deep-to-list true [entity]
  (if (atom? entity)
    (atomic-value entity)
    (expr-let [content (content entity)
               content-as-list (deep-to-list content)
               elements (elements entity)]
      (if (empty? elements)
        content-as-list
        (expr-let [element-lists (expr-seq map deep-to-list elements)]
          (cons content-as-list element-lists))))))

(defmethod stored-entity-id-string  true [entity]
  (assert (satisfies? StoredEntity entity))
  (stored-item-id-string (:item-id entity)))

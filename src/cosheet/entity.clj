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

(defprotocol Entity
  "An item or constant. For constants, the entity methods behave as if
  it had been reified into an item with the constant as its content,
  and nothing else."

  (atom? [this]
    "True if this entity is atomic: either a primitive or a reference to
     a primitive.")

  ;; TODO: Rather than taking an atomic label, allow this to take an
  ;;       arbitrary description of the elements in question.
  (label->elements [this label]
    "Return a seq of items for all our elements with with an elaboration with
     the given atomic label.")

  (elements [this]
    "Return a seq of items for all our elements.")

  (content [this]
    "Return the content of the entity.")

  (content-reference [this]
    "Return a reference to the content of the entity if the entity is stored,
     otherwise just return its content."))

;;; Utility functions that work on entities

(defn label->content [entity label]
  "Return the content of the element with the given label.
   There must be at most one such element." 
  (let [elements (label->elements entity label)]
    (when elements
      (assert (= (count elements) 1))
      (content (first elements)))))

(defn atomic-value [entity]
  "Return the atomic value reached by chasing contents"
  (cond (nil? entity) nil
        (atom? entity) (content entity) ; Could be implicit content reference.
        :else (atomic-value (content entity))))

(defn label->atomic-values [entity label]
  "Return all atomic values for the label, reflecting multiplicity."
  (map atomic-value (label->elements entity label)))

(defn label-has-atomic-value? [entity label value]
  "Whether the entity has the given value
   among its atomic values for the given label."
  (some (partial = value)
        (label->atomic-values entity label)))

(defn to-list [entity]
  "Return the list representation of the entity"
  (if (or (nil? entity) (atom? entity))
    (atomic-value entity)
    (let [entity-content (to-list (content entity))
          entity-elements (seq (map to-list (elements entity)))]
      (if entity-elements
        (cons entity-content entity-elements)
        entity-content))))

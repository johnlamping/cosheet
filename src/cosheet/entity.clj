(ns cosheet.entity)

(defprotocol Description
  "A description of an item or constant."
  (description->entity [this store]
    "Return an item or other entity, given the store the description
     depends on."))

(defprotocol Entity
  "An item or constant. For constants, the methods behave as if it had
   been reified into an item with the constant as content, and nothing else."

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
  "Return the content of the given label.
   There must be at most one such label." 
  (let [elements (label->elements entity label)]
    (when elements
      (assert (= (count elements) 1))
      (content (first elements)))))

(defn atomic-value [entity]
  "Return the atomic value reached by chasing contents"
  (if (or (nil? entity) (atom? entity))
    entity
    (atomic-value (content entity))))

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
    entity
    (let [entity-content (to-list (content entity))
          entity-elements (seq (map to-list (elements entity)))]
      (if entity-elements
        (cons entity-content entity-elements)
        entity-content))))

(ns cosheet.server.semantic
  (:require
   (cosheet
    [canonical :refer [canonicalize]]
    [entity :refer [object? element? label-element? reverse? primitive?
                    non-type-object? presumed-interned-object? tree-entity?
                    uniquely-identified-object? interned-object? id->entity
                    content all-elements forward-elements entity-direction
                    content->elements label->content link-type object-type
                    make-tree-element make-tree-object make-tree-id
                    make-tree-object-copying-id add-elements-to-entity
                    make-conflux-tree-object conflux-tree-object-id
                    conflux-tree-object? non-conflux-tree-object?
                    convert-unneeded-conflux-tree-objects
                    identity-post-fn threaded-traverse
                    repetition-avoiding-threaded-traverse]]
    [query :refer [make-not-query make-variable-query special-form?
                   variable-query? variable-name variable-qualifier
                   variable-reference]])))

;;; This file addrsses whether entities are semantic and/or selectors.

;;; For purposes of comparing two entities, not all of their elements
;;; matter. In particular, order information, or other information
;;; about how to display the elements, is considered irrelevant for
;;; matching a condition. We call the elements that do matter the
;;; semantic elements.

;;; Specifically, a semantic element is one whose content is something
;;; the user can enter, and is not an object marked :non-semantic.
;;; Note that this test depends on direction: the content of a reverse
;;; element is the link's target. So a link whose source is a
;;; non-semantic object is non-semantic seen from its target, but seen
;;; from its source it is a reverse element whose content is the
;;; target, and so is semantic if the target is a semantic object.

;;; Once an element is put in the store, if will have an :order
;;; element if it is semantic. But is possible for a store object to
;;; have an :order element even if it is non-semantic. (This happens
;;; with tabs.)

;;; Note: transform-pattern-toward-fixed-term's handling of
;;; :require-semantic needs to be kept in synch with the definition of
;;; a semantic element.

(defn semantic-element?
  "Return true if an item counts as semantic information. An element
  whose content is an object marked :non-semantic does not."
  [immutable-entity]
   (let [cont (content immutable-entity)]
     (or (string? cont)
         (number? cont)
         (= cont 'anything)
         (and (object? cont)
              (empty? (content->elements cont :non-semantic))))))

(defn semantic-elements
  "Return the elements of an entity that are semantic."
  [immutable-entity]
  (filter semantic-element? (all-elements immutable-entity)))

(defn semantic-label-elements
  "Return the semantic elements of an entity that are labels."
  [entity]
  (filter #(and (label-element? %) (semantic-element? %)) (all-elements entity)))

(defn semantic-non-label-elements
  "Return the semantic elements of an entity that are not labels."
  [entity]
  (filter #(and (not (label-element? %)) (semantic-element? %))
          (all-elements entity)))

(defn internal-semantic-to-tree
  "Make a tree form of the entity that includes only its semantic
  information. Interned objects encountered during the traversal are
  not expanded; they appear as references. elements-fn is called on
  each entity to obtain its semantic elements and their order."
  [entity elements-fn]
  (letfn [(pre-fn [_ e _ conflux-seen]
            (cond (conflux-tree-object? e)
                  ;; We rely on the the non-repetition wrapper of our
                  ;; pre-fun to turn non-interned objects into
                  ;; tree-objects.
                  [(make-conflux-tree-object
                    (conflux-tree-object-id e) (elements-fn e))
                   true]
                  
                  (non-conflux-tree-object? e)
                  [(make-tree-object (elements-fn e))
                   conflux-seen]
                  
                  (element? e)
                  [(make-tree-element
                    (content e) (elements-fn e) (entity-direction e))
                   conflux-seen]
                  
                  :else
                  [e conflux-seen]))]
    (let [[tree conflux-seen] (repetition-avoiding-threaded-traverse
                               entity pre-fn identity-post-fn false)]
      (cond-> tree
        conflux-seen convert-unneeded-conflux-tree-objects))))

(defn semantic-to-tree
  "Given an immutable entity, make a list representation of the
  semantic information of the entity."
  [immutable-entity]
  (internal-semantic-to-tree immutable-entity semantic-elements))

(defn semantic-to-tree-excluding-elements
  "Like semantic-to-tree, but omits any semantic elements whose ids are
  in element-ids-to-exclude."
  [immutable-entity element-ids-to-exclude]
  (internal-semantic-to-tree
   immutable-entity
   (fn [entity]
     (remove #(element-ids-to-exclude (:item-id %))
             (semantic-elements entity)))))

(defn entity->canonical-semantic
  "Return the canonical form of the semantic information for the entity.
  Only works on immutable entities."
  [entity]
  (canonicalize (semantic-to-tree entity)))

(defn selector-marked-element?
  "Return whether the element is marked with a :selector whose :reverse
  information matches the orientation of the element."
  [element]
  (when-let [selector (first (content->elements element :selector))]
    (= (reverse? element)
       (boolean (seq (content->elements selector :reverse))))))

(defn selector-semantic-elements
  "Return the semantic elements of an entity that are marked with a
  :selector whose orientation matches theirs."
  [entity]
  (filter selector-marked-element? (semantic-elements entity)))

(defn selector-semantic-to-tree
  "Like semantic-to-tree, but only follows elements that are marked with
  a :selector whose orientation matches theirs."
  [immutable-entity]
  (internal-semantic-to-tree immutable-entity selector-semantic-elements))

(defn selector?
  "Return whether the entity is a selector."
  [entity]
  (seq (content->elements entity :selector)))

(defn transform-pattern-toward-fixed-term
  "Given a pattern, alter it in accordance with the options. Specifically:
    * Replace 'anything by nil.
    * Replace conflux-tree-objects with reference variables. The first
      occurrence of a conflux gets a fresh name and a qualifier whose
      elements are the original conflux's elements; later occurrences
      use just the name. The caller-data is a map from conflux-id to
      variable name.
    * If require-not-type is true and an object is not a type, then
      require it not to have a link-type or object-type element, so it
      won't match those.
    * If require-semantic is true, make the pattern only match semantic
      items. There are two cases where a match could be non-semantic.
      Here they are, and how they are resolved.
         * An element with nil content: add a '(nil :order).
         * A non-interned object: add a (not :non-semantic). This also
           handles elements with content of a non-interned object.
      In addition, in this case, we don't want to match the universal
      objects. We add a '(nil :order) to avoid matching them."
  [pattern {:keys [require-not-type require-semantic] :as options}]
  (assert (tree-entity? pattern))
  (letfn [(pre-fn [_ entity _ conflux-map]
            (cond
              (= entity 'anything)
              [nil conflux-map]

              (conflux-tree-object? entity)
              (let [id (conflux-tree-object-id entity)]
                (if-let [cached-var (get conflux-map id)]
                  ;; Already encountered: substitute with the cached
                  ;; reference variable, which needs no further
                  ;; processing.
                  [[:entity/do-not-process cached-var] conflux-map]
                  ;; First encounter: store a qualifier-less reference
                  ;; variable in the map, to use when recursing into
                  ;; the conflux's elements, which might mention the
                  ;; same conflux id. They will turn into the
                  ;; qualifier for the variable, and that is the one
                  ;; place where variables with the same name don't
                  ;; have to repeat the qualifier.
                  [entity
                   (assoc conflux-map id
                          (make-variable-query (gensym "v") :reference true
                                               :form :object))]))

              :else
              [entity conflux-map]))
          (post-fn [original assembled _ conflux-map]
            (cond
              (element? original)
              (let [c (content assembled)
                    new-elements
                    (cond-> (or (forward-elements assembled) [])
                      (and (nil? c) require-semantic)
                      (concat ['(nil :order)]))]
                [(make-tree-element
                  c new-elements (entity-direction original))
                 conflux-map])

              (presumed-interned-object? original)
              [original conflux-map]

              (conflux-tree-object? original)
              ;; First encounter (back-references don't reach here):
              ;; assembled is the rebuilt conflux with post-recursion
              ;; elements. Build the qualifier from them, produce a
              ;; reference variable carrying that qualifier, and
              ;; record it in the map, to use for all occurrences of
              ;; the same conflux-id elsewhere.
              (let [id (conflux-tree-object-id original)
                    v-name (label->content (get conflux-map id)
                                           :cosheet.query/name)
                    qualifier (make-tree-object (all-elements assembled))
                    qualified-var (make-variable-query v-name
                                                  :qualifier qualifier
                                                  :reference true)]
                [qualified-var (assoc conflux-map id qualified-var)])

              (object? original)
              [(make-tree-object-copying-id
                original
                (cond-> (or (all-elements assembled) [])
                  (and require-not-type (non-type-object? original))
                  (concat [(make-not-query `(~link-type))
                           (make-not-query `(~object-type))])
                  require-semantic
                  (concat ['(nil :order)
                           (make-not-query '(:non-semantic))])))
               conflux-map]

              :else  ; primitive
              [assembled conflux-map]))]
    (let [[tree _] (repetition-avoiding-threaded-traverse
                    pattern pre-fn post-fn {})]
      tree)))

(defn selector->fixed-term
  "Convert the selector entity to a list, and change 'anything to nil."
  [selector-entity]
  (assert (selector? selector-entity) selector-entity)
  (-> selector-entity
      selector-semantic-to-tree
      (transform-pattern-toward-fixed-term {})))

(defn selector->fixed-term-with-negations
  "Given a selector entity, alter it to work as a query that assumes
  everything it is querying over is semantic. Specifically:
    * Replace 'anything by nil.
    * If an element is not a label, then require it to not match labels."
  [selector-entity]
  (assert (selector? selector-entity) selector-entity)
  (-> selector-entity
      selector-semantic-to-tree
      (transform-pattern-toward-fixed-term {:require-not-type true})))

(defn pattern->fixed-term-query
  "Given a pattern, alter it to work as a fixed-term query. Specifically:
    * Replace 'anything by nil.
    * If an element is not a label, require it not to not match labels.
    * Make it only match semantic items: add a '(nil :order) element to
      an element with nil content and to an object, and require a
      non-interned object not to have a :non-semantic element."
  [pattern]
  (transform-pattern-toward-fixed-term
   pattern {:require-not-type true :require-semantic true}))

(defn selector->fixed-term-query
  "Given an selector entity, turn it into a fixed-term query."
  [selector-entity]
  (assert (selector? selector-entity) selector-entity)
  (pattern->fixed-term-query (selector-semantic-to-tree selector-entity)))

(defn mark-template-as-selector
  "Return the template with a :selector element added to it and,
  recursively, to each of its semantic elements and non-identified,
  non-interned objects. When adding a :selector element to a reverse
  link, add a :reverse sub-element. That allows recreating the
  direction in which the template was traversed."
  [template]
  (if (primitive? template)
    ;; Our recursion doesn't touch primitives, because it will have
    ;; marked their holders as selectors. So if we start with a bare
    ;; primitive, we have to handle it specially.
    (make-tree-element template [:selector])
    (convert-unneeded-conflux-tree-objects
     (first
      (repetition-avoiding-threaded-traverse
       template
       (fn [_ entity _ caller-data]
         (if (or (and (element? entity) (semantic-element? entity))
                 (and (object? entity)
                      (not (uniquely-identified-object? entity))
                      (not (presumed-interned-object? entity))))
           [entity caller-data]
           [[:entity/do-not-process entity] caller-data]))
       (fn [_ assembled _ caller-data]
         [(add-elements-to-entity
           assembled [(if (and (element? assembled)
                               (reverse? assembled))
                        '(:selector :reverse)
                        :selector)])
          caller-data])
       nil)))))

(defn replace-anythings-with-empty
  "Return the template with every 'anything value replaced by an empty
  value: an empty object when it is the content of a reversed element,
  otherwise the empty string. Cycles and objects reached by multiple
  paths are handled by repetition-avoiding-threaded-traverse."
  [template]
  (convert-unneeded-conflux-tree-objects
   (first
    (repetition-avoiding-threaded-traverse
     template
     (fn [parent entity _ caller-data]
       [(if (= entity 'anything)
          (if (and (element? parent) (reverse? parent))
            (make-tree-object [])
            "")
          entity)
        caller-data])
     identity-post-fn
     nil))))

(defn selector-marked-entities
  "Returns the entities, starting from the one with the given id, that
  carry the :selector markings that mark-template-as-selector
  added. Does not recurse into interned objects or into unmarked
  entities. Doesn't include an element if it's not marked with
  :selector or the :reverse information on the :selector doesn't match
  the orientation of the element.
  The orientation restriction is for handling objects. Without it, it
  could be possible recurse from an object to one that had referenced
  it, by following the reference link in reverse. With the orientation
  information, we can restrict the traversal to what was added to a
  selector while it was a selector."
  [store id]
  (repetition-avoiding-threaded-traverse
   (let [entity (id->entity id store)]
     ;; The traversal doesn't descend into interned objects. But the
     ;; starting object may have just become interned, so start from
     ;; a tree copy of its elements.
     (if (interned-object? entity)
       (make-tree-object (all-elements entity))
       entity))
   (fn [_ entity _ entities]
     (let [selector (first (content->elements entity :selector))]
       (cond
         (nil? selector) [:entity/omit entities]
         (or (object? entity)
             (= (reverse? entity)
                (boolean (seq (content->elements selector :reverse)))))
         [entity (conj entities entity)]
         :else [:entity/omit entities])))
   nil
   []))

(defn add-non-selector-to-fixed-term
  "Given a fixed-term pattern, return a pattern that additionally
  requires that the matched item not have a (:selector) element. This
  doesn't guarantee that no sub-entities of a selector are found. So
  callers to this should generally filter the results to make sure
  they are not selectors."
  [pattern]
  (add-elements-to-entity pattern [(make-not-query '(:selector))]))

;;; We have various list forms of entities for different purposes:
;;;      query: a form suitable for use as a query. It can have nils,
;;;             which means it can't be saved in the store. It may
;;;             include non-semantic information, like :order to
;;;             restrict what it matches.
;;;    pattern: a form of a query that can be saved in the store. It
;;;             uses 'anything for wildcards, where a query would have
;;;             nil. We need to distiguish wildcards from an empty
;;;             value, because the user might want to search for an
;;;             empty value, and the only natural way to express that
;;;             is with an empty value, distinct from a wildcard.
;;;  template:  the list form for the semantic content of a new item.
;;;             It may have 'anything as they are allowed in items.
;;;    generic: a pattern or template that has '??? to indicate values
;;;             that need to be filled in with unique strings.

(defn fixed-term-to-template
  "Given a fixed-term, turn it into a template by removing any (nil :order),
  removing any negations, replacing any nil by the specified
  replacement (which defaults to the empty string) and turning
  reference variables into conflux tree objects.
  The first occurrence of a reference variable becomes a conflux tree
  object with a fresh identity and with elements matching the
  variable's qualifier; later occurrences of the same variable become
  conflux tree objects with that identity and no elements.
  A non-reference variable is replaced by its qualifier, if it has one,
  and otherwise by the replacement."
  ([query]
   (fixed-term-to-template query ""))
  ([query nil-replacement]
   (letfn [(pre-fn [_ entity _ conflux-map]
             (cond
               (variable-query? entity)
               (if (variable-reference entity)
                 (let [name (variable-name entity)]
                   (if-let [id (get conflux-map name)]
                     [(make-conflux-tree-object id []) conflux-map]
                     (let [id (make-tree-id (:next-number conflux-map))]
                       [(make-conflux-tree-object
                         id (all-elements (variable-qualifier entity)))
                        (-> conflux-map
                            (assoc name id)
                            (update :next-number inc))])))
                 [(or (variable-qualifier entity) nil-replacement) conflux-map])
               (nil? entity)
               [nil-replacement conflux-map]
               (and (element? entity)
                    (or (= entity '(nil :order)) (special-form? entity)))
               [:entity/omit conflux-map]
               true
               [entity conflux-map]))]
     (first
      (threaded-traverse
       query pre-fn identity-post-fn {:next-number 1})))))

(ns cosheet.server.model-utils
  (:require
   (cosheet
    [debug :refer [simplify-for-print]]
    [utils :refer [threaded-map replace-in-seqs
                   extract-first]]
    [orderable :refer [initial split]]
    [reporter-macros :refer [app-R let-R]]
    [canonical :refer [canonicalize]]
    [store :refer [new-element-store
                   update-source add-link declare-ephemeral-id
                   target-label->ids get-new-object-id]]
    [entity :refer [primitive? object?
                    link-type-object? object-type-object?
                    uniquely-identified-object? interned-object?
                    id-identified-object?
                    element? label-element? id->entity
                    content elements orientation containing-elements
                    link-type object-type name-label
                    content->elements label->elements label->element
                    label->content
                    map-subparts
                    target-entity entity-key
                    make-tree-element make-tree-object
                    make-tree-object-copying-id make-tree-id
                    make-conflux-tree-object conflux-tree-object-id
                    conflux-tree-object? non-conflux-tree-object?
                    convert-unneeded-conflux-tree-objects
                    identity-pre-fn identity-post-fn
                    threaded-traverse
                    repetition-avoiding-threaded-traverse
                    add-elements-to-entity
                    entity-complexity stored-entity?
                    tree-entity? presumed-interned-object?
                    in-different-store]]
    [store-utils :refer [add-object add-element remove-entity-by-id
                         find-object-by-name add-universal-objects
                         link-type-object object-type-object]]
    [query :refer [matching-items matching-elements
                   not-query special-form? variable-query?
                   variable-name variable-qualifier variable-reference
                   special-form-type sub-query variable-query
                   extended-by?]]
    [query-impl :refer [separate-negations]]
    [query-calculator :refer [matching-item-ids-R]])
   (cosheet.server
    [order-utils :refer [semantic-element? orderable-entity?
                         ordered-ids-R ordered-entities
                         order-element-for-item]]
    [format-convert :refer [current-format]])))

;;; Utilities that know about how information is encoded in terms of the store.

;;; Creating new labels

(defn next-new-string
  "Given a string, return the next string."
  [string]
  (if (empty? string)
    "A"
    (let [prefix (subs string 0 (dec (count string)))
          end (last string)]
      (if (>= (int end) (int \Z))
        (str (next-new-string prefix) "A")
        (str prefix (char (inc (int end))))))))

(defn get-new-string
  "Given an immutable store, return a new short string of all cap letters
   that does not occur in the store, and an updated store that knows what
   the last new string was."
  [store]
  (let [last-string-item (first (matching-items
                                 '(nil :last-new-string) store))]
    (loop [last-new (when last-string-item (content last-string-item))]
      (let [next-new (next-new-string last-new)]
        (if (seq (matching-items next-new store))
          (recur next-new)
          [next-new
           (if last-string-item
             (update-source
              store (:item-id last-string-item) next-new)
             (first (add-element store nil `(~next-new :last-new-string))))])))))

(defn get-n-new-strings
  "Get n new strings"
  [store n]
  (reduce (fn [[strings store] _]
            (let [[new-string store] (get-new-string store)]
              [(conj strings new-string) store]))
          [[] store] (range n)))

(defn semantic-elements
  "Return the elements of an entity that are semantic."
  [immutable-entity]
  (filter semantic-element? (elements immutable-entity)))

(defn ordered-semantic-elements
  "Return the semantic elements of an entity, in the order that the
  :order information calls for."
  [immutable-entity]
  (ordered-entities (semantic-elements immutable-entity)))

(defn semantic-label-elements
  "Return the semantic elements of an entity that are labels."
  [entity]
  (filter #(and (label-element? %) (semantic-element? %)) (elements entity)))

(defn semantic-non-label-elements
  "Return the semantic elements of an entity that are not labels."
  [entity]
  (filter #(and (not (label-element? %)) (semantic-element? %))
          (elements entity)))

(defn remove-semantic-elements
  "Return the store with all semantic elements of the given id removed."
  [immutable-store id]
  (let [item (id->entity id immutable-store)]
    (reduce remove-entity-by-id
            immutable-store
            (map :item-id (semantic-elements item)))))

(defn internal-semantic-to-tree
  "Make a tree form of the entity that includes only its semantic
  information. Interned objects encountered during the traversal are
  not expanded; they appear as references. elements-fn is called on
  each entity to obtain its semantic elements and their order."
  [entity elements-fn]
  (letfn [(pre-fn [_ e _ conflux-seen]
            (cond (conflux-tree-object? e)
                  [(make-conflux-tree-object
                    (conflux-tree-object-id e) (elements-fn e))
                   true]
                  
                  (non-conflux-tree-object? e)
                  [(make-tree-object (elements-fn e))
                   conflux-seen]
                  
                  (element? e)
                  [(make-tree-element
                    (orientation e) (content e) (elements-fn e))
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

(defn ordered-semantic-to-tree
  "Given an immutable entity, make a list representation of the
  semantic information of the entity, putting elements in the order that the
  :order information calls for."
  [immutable-entity]
  (internal-semantic-to-tree immutable-entity ordered-semantic-elements))

(defn object-semantic-to-tree
  "Given an immutable object, make a list representation of its semantic
  information, even if the object is identified. Put elements in the
  order that the :order information calls for."
  [immutable-entity]
  ;; internal-semantic-to-tree wouldn't expand an interned object, so
  ;; do the first expansion here and call internal-semantic-to-tree on
  ;; each of the resulting sub-elements.
  (->> (ordered-semantic-elements immutable-entity)
       (map ordered-semantic-to-tree)
       make-tree-object))

(defn entity->canonical-semantic
  "Return the canonical form of the semantic information for the entity.
  Only works on immutable entities."
  [entity]
  (canonicalize (semantic-to-tree entity)))

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
;;;             It may have 'anything as they are allowed in items. But
;;;             since they are only allowed in selector items, they will
;;;             be turned into "" when put into non-selector items. 
;;;    generic: a pattern or template that has '??? to indicate values
;;;             that need to be filled in with unique strings.

(defn replace-anything-by-nil
  "If the value is 'anything, replace it with nil."
  [value]
  (if (= 'anything value) nil value))

(defn add-non-selector-to-fixed-term
  "Given a fixed-term pattern, return a pattern that additionally
  requires that the matched item not have a (:selector) element."
  [pattern]
  (add-elements-to-entity pattern [(not-query '(:selector))]))

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
    * If require-orders is true and an element has nil content, or the
      pattern is an object, add a '(nil :order) element to make it only
      match user editable items."
  [pattern {:keys [require-not-type require-orders] :as options}]
  (assert (tree-entity? pattern))
  (letfn [(pre-fn [_ entity _ conflux-map]
            (cond
              (= entity 'anything)
              [nil conflux-map]

              (conflux-tree-object? entity)
              (let [id (conflux-tree-object-id entity)]
                (if-let [cached-var (get conflux-map id)]
                  ;; Already encountered: substitute with the cached
                  ;; reference variable.
                  [cached-var conflux-map]
                  ;; First encounter: store a qualifier-less reference
                  ;; variable in the map, to use when recursing into
                  ;; the conflux's elements, which might mention the
                  ;; same conflux id. They will turn into the
                  ;; qualifier for the variable, and that is the one
                  ;; place where variables with the same name don't
                  ;; have to repeat the qualifier.
                  [entity
                   (assoc conflux-map id
                          (variable-query (gensym "v") :reference true))]))

              :else
              [entity conflux-map]))
          (post-fn [original assembled _ conflux-map]
            (cond
              (element? original)
              (let [c (content assembled)
                    new-elements
                    (cond-> (or (elements assembled) [])
                      (and (nil? c) require-orders)
                      (concat ['(nil :order)]))]
                [(make-tree-element
                  (orientation original) c new-elements)
                 conflux-map])

              (presumed-interned-object? original)
              [original conflux-map]

              (conflux-tree-object? original)
              (if (conflux-tree-object? assembled)
                ;; First encounter: assembled is the rebuilt conflux
                ;; with post-recursion elements. Build the qualifier
                ;; from them, produce a reference variable carrying
                ;; that qualifier, and record it in the map, to use
                ;; for all occurrences of the same conflux-id
                ;; elsewhere.
                (let [id (conflux-tree-object-id original)
                      v-name (label->content (get conflux-map id)
                                             :cosheet.query/name)
                      qualifier (make-tree-object (elements assembled))
                      qualified-var (variable-query v-name
                                                    :qualifier qualifier
                                                    :reference true)]
                  [qualified-var (assoc conflux-map id qualified-var)])
                ;; Back-reference: pre-fn substituted the cached
                ;; reference variable, which is already in assembled
                ;; (after the natural element-case rebuild). Pass it
                ;; through.
                (do (assert (variable-query? assembled))
                    [assembled conflux-map]))

              (object? original)
              (let [non-type (and (not (link-type-object? original))
                                  (not (object-type-object? original)))]
                [(make-tree-object-copying-id
                  original
                  (cond-> (or (elements assembled) [])
                    (and require-not-type non-type)
                    (concat [(not-query `(~link-type))
                             (not-query `(~object-type))])
                    require-orders
                    (concat ['(nil :order)])))
                 conflux-map])

              :else  ; primitive
              [assembled conflux-map]))]
    (let [[tree _] (repetition-avoiding-threaded-traverse
                    pattern pre-fn post-fn {})]
      tree)))

(defn entity->fixed-term
  "Convert the entity to a list, and change 'anything to nil."
  [entity]
  (-> entity
      semantic-to-tree
      (transform-pattern-toward-fixed-term {})))

(defn entity->fixed-term-with-negations
    "Given an entity, alter it to work as a query that assumes everything
     it is querying over is semantic. Specifically:
    * Replace 'anything by nil.
    * If an element is not a label, then require it to not match labels."
  [entity]
  (-> entity
      semantic-to-tree
      (transform-pattern-toward-fixed-term {:require-not-type true})))

(defn pattern-to-fixed-term
  "Given a pattern, alter it to work as a fixed-term. Specifically:
    * Replace 'anything by nil.
    * If an element is not a label, require it not to not match labels.
    * If an entity has nil content, add a '(nil :order) element to make
      it only match user editable elements."
  [pattern]
  (transform-pattern-toward-fixed-term
   pattern {:require-not-type true :require-orders true}))

(defn exemplar-to-fixed-term
  "Given an exemplar entity, turn it into a fixed-term"
  [entity]
  (pattern-to-fixed-term (semantic-to-tree entity)))

(defn fixed-term-to-template
  "Given a fixed-term, turn it into a template by removing any (nil :order),
  removing any negations, replacing any nil by the specified
  replacement (which defaults to the empty string) and turning
  reference variables into conflux tree objects.
  The first occurrence of a reference variable becomes a conflux tree
  object with a fresh identity and with elements matching the
  variable's qualifier; later occurrences of the same variable become
  conflux tree objects with that identity and no elements."
  ([query]
   (fixed-term-to-template query ""))
  ([query nil-replacement]
   (letfn [(pre-fn [_ entity _ conflux-map]
             (cond
               (and (variable-query? entity) (variable-reference entity))
               (let [name (variable-name entity)]
                 (if-let [id (get conflux-map name)]
                   [(make-conflux-tree-object id []) conflux-map]
                   (let [id (make-tree-id (:next-number conflux-map))]
                     [(make-conflux-tree-object
                       id (elements (variable-qualifier entity)))
                      (-> conflux-map
                          (assoc name id)
                          (update :next-number inc))])))
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

;;; Adding new elements and objects, noting the orders in their
;;; templates.

(defn match-terms-and-targets
  "Given a sequence of fixed terms and a sequence of targets, make the
  best possible pairing of fixed terms with targets that extend
  them. Return a seq of the matched pairs, a seq of the unpaired fixed
  terms and a seq of the unpaired targets.
  We handle fixed terms that are stored entities with non-semantic
  parts, by matching only their semantic parts."
  [fixed-terms targets]
  ;; We order the terms starting from highest complexity
  ;; (hardest to find an extension for), and the object elements
  ;; starting from lowest complexity (hardest to be an
  ;; extension). This way, when we start choosing matches, and
  ;; there is a choice, we take ones that are least likely to
  ;; preclude subsequent matches.
  (let [sorted-fixed-terms (->> fixed-terms (sort-by entity-complexity) reverse)
        sorted-targets (->> targets (sort-by entity-complexity))]
    (reduce
     (fn [[pairs unmatched-terms unmatched-targets] fixed-term]
       (let [;; If the fixed term is a stored entity; we only want to
             ;; match its semantic parts.
             semantic (if (stored-entity? fixed-term)
                        (semantic-to-tree fixed-term)
                        fixed-term)
             [matching-target remaining-targets]
             (extract-first #(extended-by? semantic %) unmatched-targets)]
         (if matching-target
           [(conj pairs [fixed-term matching-target])
            unmatched-terms
            remaining-targets]
           [pairs
            (conj unmatched-terms fixed-term)
            unmatched-targets])))
     [[] [] sorted-targets]
     sorted-fixed-terms)))

(defn elements-to-change-to-satisfy-fixed-term-elements
  "Given a fixed-term and a stored object, return templates for elements
  that must be added to the stored object, and ids of any elements
  that may be removed from it, in order to make the object have as few
  elements as possible and still satisfy:
     * The fixed-term.
     * All positive queries that the original object satisfies.
     * No positive queries that aren't satisfied by an object
       consisting of the union of the elements of the original object
       and of the template.
  The last condition implies that you can't merge two elements from
  the two arguments, as that could satisfy a query the union doesn't
  satisfy. In particular, you can't match a template nil to a content
  from the original object, because that acts like a merge. Instead,
  you have to start with the union of the elements of the two
  arguments, but you can remove an element from the union if it is
  extended by an element from the other argument.
  Return a pair of a seq of templates to add, and a seq of
  elements to remove."
  [fixed-term object]
  (assert object? fixed-term)
  (assert object? object)
  (let [;; First find elements that extend targets. We will need to add the
        ;; un-matched targets.
        [_ unmatched-term-elements unmatched-object-elements]
        (match-terms-and-targets (remove special-form? (elements fixed-term))
                                 (elements object))
        templates-to-add (map fixed-term-to-template unmatched-term-elements)
        ;; Now find unmatched targets that extend unmatched
        ;; elements. We won't need the elements that are extended.
        [object-term-pairs _ _]
        (match-terms-and-targets unmatched-object-elements templates-to-add)]
    [templates-to-add (map first object-term-pairs)]))

(def update-add-object-with-order-without-revisiting)

(def update-add-elements-with-order-without-revisiting)

(defn update-add-position-and-elements-with-order-without-revisiting
  "Given the id of an already-created entity, add an :order element to
   it for its own position, and add the given template-elements to it,
   each with its own order. The provided order is split into a bigger
   and smaller piece, with the entity getting the bigger piece if
   use-bigger is true, otherwise the smaller one. The seen map (stored
   object → entity id) is threaded through for cycle detection.
   Return [store remainder seen]."
  [store entity-id template-elements order position use-bigger seen]
  (let [entity-order-index (case position :before 0 :after 1)
        other-position ([:after :before] entity-order-index)
        split-order (split order (if use-bigger position other-position))
        bigger-index (if use-bigger
                       entity-order-index
                       (- 1 entity-order-index))
        bigger-order (split-order bigger-index)
        smaller-order (split-order (- 1 bigger-index))
        [store bigger-order seen]
        (update-add-elements-with-order-without-revisiting
         store entity-id template-elements bigger-order position seen)
        [store _] (add-element
                   store entity-id
                   `(~(if use-bigger bigger-order smaller-order) :order))]
    [store (if use-bigger smaller-order bigger-order) seen]))

(defn update-add-element-with-order-and-ephemeral-without-revisiting
  "Like update-add-element-with-order-and-ephemeral, but threads the
   seen map for cycle detection. If the element's content is a stored
   object already in seen, the element is treated as a back-reference
   and skipped entirely (no link is added), since the corresponding
   link in the other direction has already been created by the call
   that added the object to seen. The attachment-id is the endpoint
   of the new link that does NOT hold the element's content: for an
   element with :source orientation it becomes the link's target, and
   for :target orientation it becomes the link's source. Return
   [store id remainder seen]."
  [store attachment-id template order position use-bigger seen]
  (assert (empty? (label->elements template :order)))
  (let [template-content (content template)
        template-elements (elements template)
        is-ephemeral (some (fn [element] (= (content element) :ephemeral))
                           template-elements)]
    (cond
      (and (stored-entity? template-content)
           (object? template-content)
           (not (interned-object? template-content))
           (contains? seen (entity-key template-content)))
      [store nil order seen]
      (not (orderable-entity? template))
      (let [[s id] (add-element store attachment-id template)]
        [s id order seen])
      :else
      (let [[store value-to-store order seen]
            (if (object? template-content)
              (update-add-object-with-order-without-revisiting
               store template-content order position false seen)
              [store template-content order seen])
            [store id] (if (= (orientation template) :target)
                         (add-link store value-to-store attachment-id)
                         (add-link store attachment-id value-to-store))
            [store remainder seen]
            (update-add-position-and-elements-with-order-without-revisiting
             store id template-elements order position use-bigger seen)]
        [(if is-ephemeral (declare-ephemeral-id store id) store)
         id
         remainder
         seen]))))

(defn update-add-elements-with-order-without-revisiting
  "Return [store order seen]."
  [store attachment-id elements order position seen]
  (let [[s _ order seen]
        (reduce (fn [[store _ order seen] element]
                  (update-add-element-with-order-and-ephemeral-without-revisiting
                   store attachment-id element order position false seen))
                [store nil order seen]
                (case position
                  :before elements
                  ;; If we are adding them after the current order
                  ;; chunk, then each one is before the previous
                  ;; one, as the chunk shrinks.
                  :after (reverse elements)))]
    [s order seen]))

(defn update-add-object-with-given-elements-and-order-without-revisiting
  "Like update-add-object-with-given-elements-and-order. If seen-key
   is non-nil, register seen-key → new object id in seen before
   recursing into the elements (to break cycles). Return
   [store object-id remainder seen]."
  [store element-templates order position use-bigger seen seen-key]
  (let [[store object-id] (get-new-object-id store)
        seen (cond-> seen seen-key (assoc seen-key object-id))
        [store remainder seen]
        (update-add-position-and-elements-with-order-without-revisiting
         store object-id element-templates order position use-bigger seen)]
    [store object-id remainder seen]))

(defn get-or-make-ordered-object-by-name-without-revisiting
  "Like get-or-make-ordered-object-by-name. If seen-key is non-nil,
   register seen-key → object id in seen before recursing into the
   additional elements. Return [store object-id order seen]."
  [store name fixed-term order position use-bigger seen seen-key]
  (assert (object? fixed-term) fixed-term)
  ;; First, get or make an object with the given name.
  (let [[store object-id order seen]
        (if-let [object (find-object-by-name store name fixed-term)]
          [store (:item-id object) order
           (cond-> seen seen-key (assoc seen-key (:item-id object)))]
          (update-add-object-with-given-elements-and-order-without-revisiting
           store `((~name (~name-label))) order position use-bigger seen
           seen-key))
        ;; Now make it satisfy the fixed term.
        [templates-to-add elements-to-remove]
        (elements-to-change-to-satisfy-fixed-term-elements
         fixed-term (id->entity object-id store))
        [store order seen]
        (update-add-elements-with-order-without-revisiting
         store object-id templates-to-add order position seen)
        store (reduce remove-entity-by-id store
                      (map :item-id elements-to-remove))]
    [store object-id order seen]))

(defn update-add-object-with-order-without-revisiting
  "Like update-add-object-with-order, but checks seen for cycle
   detection. If the template is a stored entity already in seen,
   return its cached id without recursing into its elements. Otherwise
   process the template, registering it in seen before recursing into
   any sub-objects. Return [store id remainder seen]."
  [store template order position use-bigger seen]
  (assert (object? template) template)
  (let [seen-key (when (and (stored-entity? template)
                            (not (interned-object? template)))
                   (entity-key template))]
    (if-let [cached-id (when seen-key (get seen seen-key))]
      [store cached-id order seen]
      (cond (id-identified-object? template)
            (let [id (:item-id template)]
              [store id order
               (cond-> seen seen-key (assoc seen-key id))])
            (and (stored-entity? template) (nil? (:store template)))
            (do (assert (uniquely-identified-object?
                         (in-different-store template store))
                        template)
                (let [id (:item-id template)]
                  [store id order
                   (cond-> seen seen-key (assoc seen-key id))]))
            (uniquely-identified-object? template)
            (let [name-elements (label->elements template name-label)]
              (assert (seq name-elements) template)
              (get-or-make-ordered-object-by-name-without-revisiting
               store (content (first name-elements)) template order position
               use-bigger seen seen-key))
            true
            (do (assert (empty? (label->elements template :order)))
                (update-add-object-with-given-elements-and-order-without-revisiting
                 store (elements template) order position use-bigger seen
                 seen-key))))))

(defn update-add-element-adjacent-to-without-revisiting
  "Like update-add-element-adjacent-to, threading seen. Return
   [store id seen]."
  [store attachment-id element adjacent-to position use-bigger seen]
  (let [order-element (order-element-for-item adjacent-to store)
        order (content order-element)
        [store id remainder seen]
        (update-add-element-with-order-and-ephemeral-without-revisiting
         store attachment-id element order position use-bigger seen)]
    [(update-source store (:item-id order-element) remainder) id seen]))

(defn update-add-object-adjacent-to-without-revisiting
  "Like update-add-object-adjacent-to, threading seen. Return
   [store object-id seen]."
  [store object-template adjacent-to position use-bigger seen]
  (assert (object? object-template))
  (assert (empty? (label->elements object-template :order)))
  (let [order-element (order-element-for-item adjacent-to store)
        order (content order-element)
        [store object-id remainder seen]
        (update-add-object-with-order-without-revisiting
         store object-template order position use-bigger seen)]
    [(update-source store (:item-id order-element) remainder)
     object-id
     seen]))

;;; Client-facing wrappers: call the -without-revisiting versions with
;;; an empty seen map.

(defn update-add-element-with-order-and-ephemeral
  "Add an element, described in list form, to the store, attached to
  attachment-id (the endpoint of the new link that does NOT hold the
  element's content). Add ordering information to the element and each
  part of it, except for non-semantic elements, splitting the provided
  order for the orders, and returning an unused piece of it.  Put the
  new entity in the specified position (:before or :after) of the
  returned order, and make the entity use the bigger piece if
  use-bigger is true, otherwise use the smaller piece.  If the
  template has a :ephemeral element, mark it ephemeral in the store.
  Return the new store, the id of the item, and the remaining order."
  [store attachment-id template order position use-bigger]
  (let [[store id remainder _]
        (update-add-element-with-order-and-ephemeral-without-revisiting
         store attachment-id template order position use-bigger {})]
    [store id remainder]))

(defn get-or-make-ordered-object-by-name
  "Find or make an object with the given name, and satisfying the
  fixed-term.  If an object is found, add elements to it if necessary
  to make it satisfy the fixed term, and remove elements that were
  rendered redundant because we added a more specific element. A newly
  created object gets the bigger piece of the order split if
  use-bigger is true, otherwise the smaller piece. Return the new
  store, the id of the matching object, and the unused part of the
  order."
  [store name fixed-term order position use-bigger]
  (let [[store id order _]
        (get-or-make-ordered-object-by-name-without-revisiting
         store name fixed-term order position use-bigger {} nil)]
    [store id order]))

(defn update-add-object-with-order
  "Add an object matching the template to the store, or update a unique
  one to match the template. The new object gets the bigger piece of
  the order split if use-bigger is true, otherwise the smaller piece.
  Return the new store, the id of the object, and the unused part of
  the order."
  [store template order position use-bigger]
  (let [[store id remainder _]
        (update-add-object-with-order-without-revisiting
         store template order position use-bigger {})]
    [store id remainder]))

(defn update-add-element-adjacent-to
  "Add an entity attached to attachment-id (the endpoint of the new
   link that does NOT hold the element's content), taking its order
   from the given item, in the given position, and giving the entity
   the bigger piece if use-bigger is true. Return the updated store
   and the id of the entity."
  [store attachment-id element adjacent-to position use-bigger]
  (let [[store id _]
        (update-add-element-adjacent-to-without-revisiting
         store attachment-id element adjacent-to position use-bigger {})]
    [store id]))

(defn update-add-object-adjacent-to
  "Add a top-level object matching the template,
   taking its order from the given item, in the given position,
   and giving the new object the bigger piece of the order
   if use-bigger is true.
   Return the updated store and the id of the new object."
  [store object-template adjacent-to position use-bigger]
  (let [[store id _]
        (update-add-object-adjacent-to-without-revisiting
         store object-template adjacent-to position use-bigger {})]
    [store id]))

;;; Handling of generics and templates

(defn specialize-generic
  "Adjust a generic to make it ready for adding as an
  element. Specifically, replace each '??? with a new unique string
  with a leading non-breaking space. Allocating new strings will require
  updating the store. Return the specialized template and the new store."
  [generic store]
  (letfn [(pre-fn [_ e _ store]
            (if (= e '???)
              (let [[s new-store] (get-new-string store)]
                [(str "\u00A0" s) new-store])
              [e store]))]
    (repetition-avoiding-threaded-traverse
     generic pre-fn identity-post-fn store)))

(defn template-to-possible-non-selector-template
  "Given a template, alter it to work as a template for a possible
  non-selector. Specifically, replace 'anything by the empty string,
  unless in a part of the template that is marked as a selector, in
  which case don't modify it."
    [pattern]
    (cond (some #(= (content %) :selector) (elements pattern))
          pattern
          (= 'anything pattern)
          ""
          :else
          (map-subparts template-to-possible-non-selector-template pattern)))

(defn selector?
  "Return whether the entity is (or is part of) a selector."
  [entity]
  (or (seq (content->elements entity :selector))
      (cond (element? entity) (when-let [target (target-entity entity)]
                                (selector? target))
            (object? entity) (let [containing (containing-elements entity)]
                                 (when (= (count containing) 1)
                                   (selector? (first containing)))))))

(defn create-possible-selector-entity
  "Create an entity matching the template, but first modifying the
  template to not be a selector if the target-id is not a
  selector. Return the updated store and the id of the new entity."
  [template target-id adjacent-id position use-bigger store]
  (let [template (if (and target-id
                          (selector? (id->entity target-id store)))
                   template
                   (template-to-possible-non-selector-template template))]
    (if (object? template)
      (update-add-object-adjacent-to store template
                                     (id->entity adjacent-id store)
                                     position use-bigger)
      (update-add-element-adjacent-to store target-id template
                                      (id->entity adjacent-id store)
                                      position use-bigger))))

(defn create-possible-selector-entities
  "Create entities, specializing the template as appropriate, depending on
   whether each target is a selector. Return the new ids and the updated
   store."
  [template targets adjacents position use-bigger store]
  (let [[specialized-template store] (specialize-generic template store)]
    (threaded-map
     (fn [[target adjacent] store]
       (let [[store id] (create-possible-selector-entity
                         specialized-template
                         target adjacent position use-bigger store)]
         [id store]))
     (map vector targets adjacents)
     store)))

;;; Creating new tabs and tables.

(defn tabs-holder-id-R
  "Return the entity that holds all the tabs."
  [store]
  (let-R [holders (matching-item-ids-R '(nil :tabs) store)]
    (first holders)))

(defn ordered-tabs-ids-R
  "Return the ids of the tabs, in order."
  [store]
  (let-R [holder-id (tabs-holder-id-R store)]
    (ordered-ids-R (target-label->ids store holder-id :tab)
                   store)))

(def label-object-template
  (make-tree-object [`(~'anything (~name-label)) `(~link-type)]))

;;; A table item has a :table element, and has the following elements
;;; that describe the table:
;;;   :row-condition  The content is an item whose list form gives the
;;;                   requirements for an item to appear as a row.
;;;                   It is marked as :selector.
;;;  :column-headers  The content is an item whose list form gives the
;;;                   conditions for the column headers. Generally, the
;;;                   content will be the keyword 'anything, to
;;;                   indicate no constraint on the content of an
;;;                   element in the row, without breaking the rule
;;;                   that the database doesn't contain nil. The
;;;                   exception is the special content :other, which
;;;                   means to show everything not shown in any other
;;;                   column. (:other not yet implemented.)
;;;                   It is marked as :selector.

;;; For doing matches for batch edits, it is sort of like each header
;;; is part of the row condition, but only aone header at a time. That
;;; means that there are two ways to match the query condition:
;;;    A The row condition matches, by itself.
;;;      In this case, each header gets a chance to match the stack,
;;;      as well as each part of the row condition.

;;;    B Case A fails, but the combination of the row condition and a
;;;      header.
;;;      In this case, only a header that makes the match succeed can
;;;      match the stack.

(defn table-column-headers-id
  [table-id immutable-store]
  (first (target-label->ids immutable-store table-id :column-headers)))

(defn table-column-headers-element
  [table-item]
  (label->element table-item :column-headers))

(defn table-row-condition-id
  [table-id immutable-store]
  (first (target-label->ids immutable-store table-id :row-condition)))

(defn table-row-condition-element
  [table-item]
  (label->element table-item :row-condition))

(defn table-row-condition-object
  "Return the object inside the row-condition element of the table."
  [table-item]
  (content (table-row-condition-element table-item)))

(defn table-row-condition->row-template
  "Return the row template from the row condition. The template is the
   object each row must extend."
  [row-condition]
  (object-semantic-to-tree (content row-condition)))

(defn table-row-template
  "Return the row condition as a template."
  [table-item]
  (object-semantic-to-tree
   (table-row-condition-object table-item)))

(defn tab-table-element
  "Return the element that gives the information for a table in a new
  tab with the given row condition elements and header elements. One
  of the row condition elements should be an object type, which will
  give the kind of object the rows should contain"
  [row-condition-elements header-elements]
  `("" ; a keyword here would make this non-semantic and so not orderable.
    :tab-topic
    :table
    (~(make-tree-object (conj row-condition-elements :selector))
     :row-condition)
    ~(concat '(anything :column-headers :selector)
             header-elements)))

(def new-tab-table-element
  (tab-table-element [`(~(object-type-object '???))]
                     [`(~'anything (~(link-type-object '???)))]))

(def column-header-template
  ;; The minimum content for a column header.
  ;; In addition, a column header must be stored as an element
  ;; of the :column-headers entity.
  'anything ; a keyword here would make this non-semantic
            ; and hence not orderable
  )

(def unspecified-column-header-template
  ;; A header for a newly created column that we don't know anything about.
  ;; We give it a new label, so that it won't start out match everything.
  (add-elements-to-entity
   column-header-template [`(~(link-type-object '???))]))

(defn starting-store
  "Return an initial immutable store. If a tab name is provided, the store
  will have a single tab with that name and a table with that name."
  [tab-name]
  (let [store (add-universal-objects (new-element-store))
        [store _] (add-element store nil
                               (list current-format :format))
        [store orderable-id] (add-element store nil
                                          (list initial :unused-orderable))
        [store tabs-holder-id] (add-element store nil
                                            '("tabs" :tabs))]
    (if tab-name
      (let [[tab store] (specialize-generic
                         `(""
                           ~tab-name
                           :tab
                           ~(tab-table-element
                             [`(~(object-type-object tab-name))]
                             [`(~'anything (~(link-type-object '???)))]))
                         store)]
        (first (update-add-element-adjacent-to
                store tabs-holder-id tab                   
                (id->entity orderable-id store) :after false)))
      store)))

;;; Consistency checks

(defn column-header-problem
  "Return something truthy if the given immutable entity is a column header
   with a problem."
  [entity]
  ;; A header is a problem if it has the vacuous condition.
  (and
   ;; It has universal content
   (= 'anything (content entity))
   ;; It is a column header.
   (some #(= (content %) :column-headers) (elements (target-entity entity)))
   ;; It has no elements, except 'amything
   (let [semantic (semantic-elements entity)]
     (or (empty? semantic)
         (and (empty? (rest semantic))
              (= (content (first semantic)) 'anything)
              (empty? (semantic-elements (first semantic))))))))

(defn abandon-problem-changes
  "Given an old store, a new store, both immutable, and an id where
   changes were made, return the new store if the changes don't have any
   problems, otherwise the old store."
  [old-store new-store id]
  (if (and id
           (let [revised-entity (id->entity id new-store)]
             (or (column-header-problem revised-entity)
                 (column-header-problem (target-entity revised-entity)))))
    old-store
    new-store))

;;; CSV file importing

(defn add-rows
  "Given a sequence of rows, each of which is a sequence of values,
  add data corresponding to them to the store, each as a new top-level
  object satisfying the row-template (which must be an object).
  Return the store and the column header names."
  [store rows row-template]
  (assert (object? row-template) row-template)
  (let [num-columns (apply max (map count rows))
        first-row (first rows)
        num-first (count first-row)
        first-row-is-header (and (= num-first num-columns)
                                 (every? string? first-row)
                                 (= (count (set (map clojure.string/lower-case
                                                     first-row)))
                                    num-first))
        [headers data-rows store] (if first-row-is-header
                                    [first-row (rest rows) store]
                                    (let [[headers store]
                                          (get-n-new-strings store num-columns)]
                                      [headers rows store]))
        order-element (order-element-for-item nil store)]
    [(reduce
      (fn [store row]
        (let [[store row-id] (update-add-object-adjacent-to
                              store row-template
                              order-element :before false)]
          (reduce
           (fn [store [header-value cell-value]]
             (first (update-add-element-adjacent-to
                     store row-id
                     ;; TODO: This is inefficient, since it looks up
                     ;; the header link-type-object every
                     ;; time. Instead, make a list of all the header
                     ;; objects once, then use them here.
                     `(~cell-value (~(link-type-object header-value)))
                     order-element :before false)))
           store
           (map vector headers row))))
      store
      data-rows)
     headers]))

(defn add-table-tab
  "Given a sequence of header names, add a tab with a table consisting of those
  headers."
  [store table-name headers]
  (let [tabs-holder-id (tabs-holder-id-R store) ;; Won't be a reporter.
        last-tab (last (matching-items '(nil :tabs) store))
        [store new-tab] (update-add-element-adjacent-to
                         store tabs-holder-id
                         `(""
                           :tab
                           ~table-name
                           ~(tab-table-element
                             [`(~(object-type-object table-name))]
                             (map (fn [header]
                                    `(~'anything (~(link-type-object header))))
                                  headers)))
                         last-tab :after true)]
    store))

(defn add-table
  "Given a sequence of rows, each a sequence of values,
  add a table corresponding to them to the store, with its own tab."
  [store table-name rows]
  (let [rows-template (make-tree-object
                       [`(~(object-type-object table-name))])
        [store headers] (add-rows store rows rows-template)]
    (add-table-tab store table-name headers)))

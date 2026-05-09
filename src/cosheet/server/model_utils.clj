(ns cosheet.server.model-utils
  (:require
   (cosheet
    [debug :refer [simplify-for-print]]
    [utils :refer [thread-map replace-in-seqs
                   extract-first]]
    [orderable :refer [initial split]]
    [reporter-macros :refer [app-R let-R]]
    [canonical :refer [canonicalize]]
    [store :refer [new-element-store
                   update-source add-link declare-temporary-id
                   target-label->ids get-new-object-id]]
    [entity :refer [primitive? object?
                    link-type-object? object-type-object?
                    uniquely-identified-object? interned-object?
                    id-identified-object?
                    element? label-element? id->entity
                    content elements orientation containing-elements
                    link-type object-type name-label
                    content->elements label->elements label->element
                    map-elements pre-walk-entity post-walk-entity
                    target-entity entity-key
                    make-element-list make-object-list
                    add-elements-to-entity
                    entity-complexity stored-entity?]]
    [store-utils :refer [add-object add-element remove-entity-by-id
                         find-object-by-name add-universal-objects]]
    [query :refer [matching-items matching-elements
                   not-query special-form?
                   special-form-type sub-query
                   extended-by?]]
    [query-impl :refer [separate-negations]]
    [query-calculator :refer [matching-item-ids-R]])
   (cosheet.server
    [order-utils :refer [semantic-element? orderable-entity?
                         ordered-ids-R ordered-entities
                         order-element-for-item]]
    [format-convert :refer [current-format]])))

;;; TODO: !!! Get rid of :top-level on row templates once we start
;;;           using the fact that they are objects, which implies top
;;;           level.

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

(def internal-semantic-to-list)

(defn internal-object-semantic-to-list
  "The skipped element lets semantic-to-list avoid going back up a link
  it just traversed to this object."
  [object use-order expand-identified skipped-element]
  (if (or expand-identified (not (interned-object? object)))
    (->> (cond-> (semantic-elements object)
           use-order (ordered-entities))
         (remove #(= (entity-key %) (entity-key skipped-element)))
         (map #(internal-semantic-to-list % use-order))
         (make-object-list))
    object))

(defn internal-semantic-to-list
  [immutable-entity use-order]
  (cond (primitive? immutable-entity)
        immutable-entity
        (object? immutable-entity)
        (internal-object-semantic-to-list immutable-entity use-order false nil)
        true
        (let [content (content immutable-entity)
              elements (cond-> (semantic-elements immutable-entity)
                         use-order (ordered-entities))
              content-semantic (if (object? content)
                                 (internal-object-semantic-to-list
                                  content use-order false immutable-entity)
                                 (internal-semantic-to-list content use-order))
              element-semantics (map #(internal-semantic-to-list % use-order)
                                     elements)]
          (make-element-list (orientation immutable-entity)
                             content-semantic
                             element-semantics))))

(defn semantic-to-list
  "Given an immutable entity, make a list representation of the
  semantic information of the entity."
  [immutable-entity]
  (internal-semantic-to-list immutable-entity false))

(defn ordered-semantic-to-list
  "Given an immutable entity, make a list representation of the
  semantic information of the entity, putting elements in the order that the
  :order information calls for."
  [immutable-entity]
  (internal-semantic-to-list immutable-entity true))

(defn object-semantic-to-list
  "Given an immutable object, make a list representation of its semantic
  information, even if the object is identified. Put elements in the
  order that the :order information calls for."
  [immutable-entity]
  (internal-object-semantic-to-list immutable-entity true true nil))

(defn entity->canonical-semantic
  "Return the canonical form of the semantic information for the entity.
  Only works on immutable entities."
  [entity]
  (canonicalize (semantic-to-list entity)))

;;; We have various list forms of entities for different purposes:
;;;      query: a form suitable for use as a query. It can have nils,
;;;             which means it can't be saved in the store. It may
;;;             include non-semantic information, like :order or
;;;             :top-level to restrict what it matches.
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

(def transform-pattern-toward-fixed-term)

(defn transform-pattern-elements-toward-fixed-term
  "Given elements of a pattern, alter them according to the options for
  transform-pattern-toward-fixed-term."
  [pattern-elements options]
  (->> pattern-elements
       ;; First turn nils into elements so they'll get :order if
       ;; needed.
       (map #(if (nil? (replace-anything-by-nil %)) '(nil) %))
       (map #(transform-pattern-toward-fixed-term % options))))

;; TODO: !!! How do you prevent matching a system object? User objects
;; don't have :order elements to mark them as non-system. Do system
;; objects need a special element to mark them as such?
(defn transform-pattern-toward-fixed-term
  "Given a pattern, alter it in accordance with the options. Specifically:
    * Replace 'anything by nil.
    * If require-not-type is true and an object is not a type, then
      require it not to have a link-type or object-type element.
    * If require-orders is true and an element has nil content, add a
      '(nil :order) element to make it only match user editable elements."
  [pattern {:keys [require-not-type require-orders] :as options}]
  (cond
    (primitive? pattern)
    (replace-anything-by-nil pattern)
    (element? pattern)
    (let [new-content
          (let [replaced-content (replace-anything-by-nil (content pattern))]
            (cond-> replaced-content
              (object? replaced-content)
              (transform-pattern-toward-fixed-term options)))]
      (make-element-list
       (orientation pattern)
       new-content
       (cond-> (transform-pattern-elements-toward-fixed-term
                (elements pattern) options)
         (and (nil? new-content) require-orders)
         (concat ['(nil :order)]))))
    (interned-object? pattern)
    pattern
    (object? pattern)
    (make-object-list
     (cond-> (transform-pattern-elements-toward-fixed-term
              (elements pattern) options)
       (and require-not-type
            (not (link-type-object? pattern))
            (not (object-type-object? pattern)))
       (concat [(not-query `(~link-type)) (not-query `(~object-type))])))
    true
    (assert false pattern)))

(defn entity->fixed-term
  "Convert the entity to a list, and change 'anything to nil."
  [entity]
  (-> entity
      semantic-to-list
      (transform-pattern-toward-fixed-term {})))

(defn entity->fixed-term-with-negations
    "Given an entity, alter it to work as a query that assumes everything
     it is querying over is semantic. Specifically:
    * Replace 'anything by nil.
    * If an element is not a label, then require it to not match labels."
  [entity]
  (-> entity
      semantic-to-list
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
  (pattern-to-fixed-term (semantic-to-list entity)))

(defn fixed-term-to-template
  "Given a fixed-term, turn it into a template by removing any (nil :order),
   removing any negations, and replacing any nil by the specified replacement,
   which defaults to the empty string."
  ([query]
   (fixed-term-to-template query ""))
  ([query nil-replacement]
   (pre-walk-entity
    (fn [query] (cond (nil? query) nil-replacement
                      (element? query) (when (not (or (= query '(nil :order))
                                                      (special-form? query)))
                                         query)
                      true query))
    query)))

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
                        (semantic-to-list fixed-term)
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

(def update-add-element-with-order-and-temporary)

(defn add-elements-with-order
  [store target-id elements order position]
  (let [[s id order]
        (reduce (fn [[store _ order] element]
                  (update-add-element-with-order-and-temporary
                   store target-id element order position false))
                [store nil order]
                (case position
                  :before elements
                  ;; If we are adding them after the current order
                  ;; chunk, then each one is before the previous
                  ;; one, as the chunk shrinks.
                  :after (reverse elements)))]
    [s order]))

(defn update-add-object-with-given-elements-and-order
  "Add an object with the given elements to the store, in the given
  order. If the elements describe a uniquely identified object, there
  must not already be a matching on in the store. Return the new
  store, the id of the new object, and the unused part of the order."
  [store element-templates order position]
  (let [[store object-id] (get-new-object-id store)
        [store order] (add-elements-with-order
                       store object-id element-templates order position)]
    [store object-id order]))

(defn get-or-make-ordered-object-by-name
  "Find or make an object with the given name, and satisfying the
  fixed-term.
  If an object is found, add elements to it if necessary
  to make it satisfy the fixed term, and remove elements that are
  rendered redundant. Return the new store, the id of the matching
  object, and the unused part of the order."
  [store name fixed-term order position]
  (assert (object? fixed-term) fixed-term)
  ;; First, get or make an object with the given name.
  (let [[store object-id order]
        (if-let [object (find-object-by-name store name fixed-term)]
          [store (:item-id object) order]
          (update-add-object-with-given-elements-and-order
           store `((~name (~name-label))) order position))]
    ;; Now make it satisfy the fixed term.
    (let [[templates-to-add elements-to-remove]
          (elements-to-change-to-satisfy-fixed-term-elements
           fixed-term (id->entity object-id store))
          [store order] (add-elements-with-order
                         store object-id templates-to-add order position)
          store (reduce remove-entity-by-id store
                        (map :item-id elements-to-remove))]
      [store object-id order])))

(defn update-add-object-with-order
  "Add an object matching the template to the store, or update a unique
  one to match the template. Return the new store, the id of the
  object, and the unused part of the order."
  [store template order position]
  (assert (object? template) template)
  (cond (id-identified-object? template)
        [store (:item-id template) order]
        (uniquely-identified-object? template)
        (let [name-elements (label->elements template name-label)]
          (assert (seq name-elements) template)
          (get-or-make-ordered-object-by-name
           store (content (first name-elements)) template order position))
        true
        (update-add-object-with-given-elements-and-order
         store (elements template) order position)))

(defn update-add-element-with-order-and-temporary
  "Add an element, described in list form, to the store, with the given
  target.  Add ordering information to the element and each part of it,
  except for :label or :category specifiers and non-semantic elements,
  splitting the provided order for the orders, and returning an unused
  piece of it.  Put the new entity in the specified position (:before
  or :after) of the returned order, and make the entity use the bigger
  piece if use-bigger is true, otherwise use the smaller piece.  If
  the template has a :temporary element, mark it temporary in the store.
  Return the new store, the id of the item, and the remaining order."
  [store target-id template order position use-bigger]
  (let [template-content (content template)
        template-elements (elements template)
        is-temporary (some (fn [element] (= (content element) :temporary))
                           template-elements)]
    (if (not (orderable-entity? template))
      (let [[s id] (add-element store target-id template)]
        [s id order])
      (let [[s0 value-to-store order]
            (if (object? template-content)
              (update-add-object-with-order
               store template-content order position)
              [store template-content order])
            [s1 id] (add-link s0 target-id value-to-store)
            ;; The next bunch of complication is to split the order up
            ;; the right way in all cases. First, we split it into a
            ;; bigger and a smaller part, putting the bigger part in
            ;; correct position. Then, when we recursively add the
            ;; elements, we take their order from the bigger position,
            ;; leaving most of the space on the bigger position. Finally,
            ;; we use the appropriate position for the entity and the
            ;; return value.
            entity-order-index (case position :before 0 :after 1)
            other-position ([:after :before] entity-order-index)
            split-order (split order (if use-bigger position other-position))
            bigger-index (if use-bigger
                           entity-order-index
                           (- 1 entity-order-index))
            bigger-order (split-order bigger-index)
            smaller-order (split-order (- 1 bigger-index))
            [s2 bigger-order]
            (add-elements-with-order
             s1 id template-elements bigger-order position)
            [s3 _] (add-element
                    s2 id `(~(if use-bigger bigger-order smaller-order)
                            :order))]
        [(if is-temporary (declare-temporary-id s3 id) s3)
         id
         (if use-bigger smaller-order bigger-order)]))))

(defn update-add-element-adjacent-to
  "Add an entity with the given target id and contents,
   taking its order from the given item, in the given position,
   and giving the entity the bigger piece if use-bigger is true.
   Return the updated store and the id of the entity."
  [store target-id element adjacent-to position use-bigger]
  (let [order-element (order-element-for-item adjacent-to store)
        order (content order-element)
        [store id remainder] (update-add-element-with-order-and-temporary
                              store target-id element
                              order position use-bigger)]
    [(update-source store (:item-id order-element) remainder) id]))

(defn update-add-object-adjacent-to
  "Add an object with the given contents,
   taking its order from the given item, in the given position,
   and giving the entity the bigger piece if use-bigger is true.
   Return the updated store and the id of the entity."
  [store object adjacent-to position use-bigger]
  (assert (object? object))
  (update-add-element-adjacent-to nil object adjacent-to position use-bigger))

;;; Handling of generics and templates

(defn specialize-generic
  "Adjust a generic to make it ready for adding as an
  element. Specifically, replace each '??? with a new unique string
  with a leading non-breaking space. Allocating new strings will require
  updating the store. Return the specialized template and the new store."
  [generic store]
  (cond
    (= generic '???)
    (let [[string new-store] (get-new-string store)]
      [(str "\u00A0" string) new-store])
    (sequential? generic)
    (thread-map specialize-generic generic store)
    true
    [generic store]))

(defn template-to-possible-non-selector-template
  "Given a template, alter it to work as a template for a possible
  non-selector. Specifically, replace 'anything by the empty string,
  unless in a part of the template that is marked as a selector, in
  which case don't modify it."
  [pattern]
  (if (some #(= (content %) :selector) (elements pattern))
    pattern
    (post-walk-entity
     (fn [element]
       (let [contents (content element)]
         (if-let [revised-contents
                  (cond (= 'anything contents) ""
                        ;; Check that no sub-part is a selector.
                        (= :selector contents) (assert false element))]
           (make-element-list (orientation element)
                              revised-contents
                              (elements element))
           element)))
     pattern)))

(defn selector?
  "Return whether the entity is (or is part of) a selector."
  [entity]
  (or (seq (content->elements entity :selector))
      (cond (element? entity) (when-let [target (target-entity entity)]
                                (selector? target))
            (object? entity) (let [containing (containing-elements entity)]
                                 (when (= (count containing) 1)
                                   (selector? (first containing)))))))

(defn create-possible-selector-element
  "Create an element, modifying the template if the target-id is not a
   a selector. Return the updated store and the id of the new element."
  [template target-id adjacent-id position use-bigger store]
  (let [template (if (and target-id
                          (selector? (id->entity target-id store)))
                   template
                   (template-to-possible-non-selector-template template))]
    (update-add-element-adjacent-to store target-id template
                                  (id->entity adjacent-id store)
                                  position use-bigger)))

(defn create-possible-selector-elements
  "Create elements, specializing the template as appropriate, depending on
   whether each target is a selector. Return the new ids and the updated
   store."
  [template targets adjacents position use-bigger store]
  (let [[specialized-template store] (specialize-generic template store)]
    (thread-map
     (fn [[target adjacent] store]
       (let [[store id] (create-possible-selector-element
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
  (make-object-list [`(~'anything (~name-label)) `(~link-type)]))

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

(defn table-row-condition->row-template
  [row-condition]
  (let [condition-elements (semantic-elements row-condition)
        elements-as-lists (map semantic-to-list condition-elements)]
    (concat '(anything) elements-as-lists [:top-level])))

(defn table-row-template
  "Return the row condition as a template."
  [table-item]
  (table-row-condition->row-template
   (table-row-condition-element table-item)))

(defn tab-table-element
  "Return the element that gives the information for a table in a new tab
  with the given row condition and header elements."
  [row-condition-elements header-elements]
  `("" ; a keyword here would make this non-semantic and so not orderable.
    :tab-topic
    :table
    ~(concat '(anything :row-condition :selector)
              row-condition-elements)
    ~(concat '(anything :column-headers :selector)
             header-elements)))

(def new-tab-table-element
  (tab-table-element ['(??? :label)] ['(anything (??? :label))]))

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
   column-header-template ['(??? :label)]))

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
                             [`(~tab-name :label)] ['(anything (??? :label))]))
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
   ;; It has no elements, or only a :label element.
   (let [semantic (semantic-elements entity)]
     (or (empty? semantic)
         (and (empty? (rest semantic))
              (#{'anything} (content (first semantic)))
              (every? #(= (content %) :label)
                      (semantic-elements (first semantic))))))))

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
  add data corresponding to them to the store, each following the
  row-template in the order.
  Return the store and the column header names."
  [store rows row-template]
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
        (let [[store row-id] (update-add-element-adjacent-to
                              store nil row-template
                              order-element :before false)]
          (reduce
           (fn [store [header-value cell-value]]
             (first (update-add-element-adjacent-to
                     store row-id `(~cell-value (~header-value :label))
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
                             [`(~table-name :label)]
                             (map (fn [header] `(~'anything (~header :label)))
                                  headers)))
                         last-tab :after true)]
    store))

(defn add-table
  "Given a sequence of rows, each a sequence of values,
  add a table corresponding to them to the store, with its own tab."
  [store table-name rows]
  (let [rows-template `("" (~table-name :label) :top-level)
        [store headers] (add-rows store rows rows-template)]
    (add-table-tab store table-name headers)))

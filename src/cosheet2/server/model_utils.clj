(ns cosheet2.server.model-utils
  (:require (cosheet2 [debug :refer [simplify-for-print]]
                      [utils :refer [thread-map prewalk-seqs replace-in-seqs
                                     add-elements-to-entity-list]]
                      [orderable :refer [initial]]
                      [expression :refer [expr expr-let expr-seq expr-filter]]
                      [canonical :refer [canonicalize]]
                      [store :refer [new-element-store update-source
                                     target-label->ids]]
                      [entity :refer [primitive? label? id->entity
                                      content elements content->elements
                                      label->elements label->element
                                      target-entity]]
                      [store-utils :refer [add-entity remove-entity-by-id]]
                      [query :refer [matching-items matching-elements
                                     not-query special-form?]]
                      [query-calculator :refer [matching-item-ids-R]])
            (cosheet2.server
             [order-utils :refer [semantic-entity?
                                  ordered-ids-R ordered-entities
                                  order-element-for-item
                                  update-add-entity-adjacent-to]]
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
             (first (add-entity store nil `(~next-new :last-new-string))))])))))

(defn get-n-new-strings
  "Get n new strings"
  [store n]
  (reduce (fn [[strings store] _]
            (let [[new-string store] (get-new-string store)]
              [(conj strings new-string) store]))
          [[] store] (range n)))

;;; We have various list forms of items for different purposes:
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

;;; TODO: If this never causes a failure, get rid of it; that means it
;;; isn't needed.
(defn flatten-nested-content
  "If item has a form anywhere like ((a ...b...) ...c...), turn that into
  (a ...b... ...c...)"
  [item]
  (clojure.walk/postwalk
   (fn [item]
     (if (and (seq? item) (seq? (first item)))
       (assert false ["Template with nested content" (first item)])
       ;; Here's the code in case we do need to handle this case after all:
       ;; (apply list (concat (first item) (rest item)))
       item))
   item))

(defn semantic-elements
  "Return the elements of an entity that are semantic."
  [immutable-entity]
  (filter semantic-entity? (elements immutable-entity)))

(defn remove-semantic-elements
  "Return the store with all semantic elements of the given id removed."
  [immutable-store id]
  (let [item (id->entity id immutable-store)]
    (reduce remove-entity-by-id
            immutable-store
            (map :item-id (semantic-elements item)))))

(defn semantic-to-list
  "Given an immutable entity, make a list representation of the
  semantic information of the entity."
  [immutable-entity]
  (if (primitive? immutable-entity)
    (content immutable-entity)
    (let [content (content immutable-entity)
          elements (semantic-elements immutable-entity)
          content-semantic (semantic-to-list content)
          element-semantics (map semantic-to-list elements)]
      (if (empty? element-semantics)
        content-semantic
        (apply list (into [content-semantic] element-semantics))))))

(defn ordered-semantic-to-list
  "Given an immutable entity, make a list representation of the
  semantic information of the entity, putting elements in the order that the
  :order information calls for."
  [immutable-entity]
  (if (primitive? immutable-entity)
    (content immutable-entity)
    (let [content (content immutable-entity)
          elements (ordered-entities (semantic-elements immutable-entity))
          content-semantic (semantic-to-list content)
          element-semantics (map ordered-semantic-to-list elements)]
      (if (empty? element-semantics)
        content-semantic
        (apply list (into [content-semantic] element-semantics))))))

(defn entity->canonical-semantic
  "Return the canonical form of the semantic information for the entity.
  Only works on immutable entities."
  [entity]
  (canonicalize (semantic-to-list entity)))

(defn semantic-label-elements
  "Return the semantic elements of an entity that are labels."
  [entity]
  (filter #(and (label? %) (semantic-entity? %)) (elements entity)))

(defn semantic-non-label-elements
  "Return the semantic elements of an entity that are not labels."
  [entity]
  (filter #(and (not (label? %)) (semantic-entity? %)) (elements entity)))

(defn selector?
  "Return whether the entity is (or is part of) a selector."
  [entity]
  (or (seq (content->elements entity :selector))
      (if-let [target (target-entity entity)]
        (selector? target))))

(defn transform-pattern-toward-fixed-term
  "Given a pattern, alter it in accordance with the options. Specifically:
    * Replace 'anything by nil.
    * If require-not-labels and an element is not a label, then require it
      not to have a :label element.
    * If require-orders and an entity has nil content, add a '(nil :order)
      element to make it only match user editable elements."
  [pattern & {:keys [require-not-labels require-orders] :as options}]
  (let [old-content (content pattern)
        new-content (if (= 'anything old-content) nil old-content) 
        new-elements (cond-> (map #(transform-pattern-toward-fixed-term
                                    %
                                    :require-not-labels require-not-labels
                                    :require-orders require-orders)
                                  (elements pattern))
                       (and require-not-labels
                            (not (label? pattern))
                            (or (nil? new-content)
                                (string? new-content)
                                (number? new-content)))
                       (concat [(not-query :label)])
                       (and (nil? new-content) require-orders)
                       (concat ['(nil :order)]))]
    (if (seq new-elements)
      (apply list (cons new-content new-elements))
      new-content)))

(defn entity->fixed-term
  "Convert the entity to a list, and change 'anything to nil."
  [entity]
  (-> entity
      semantic-to-list
      transform-pattern-toward-fixed-term))

(defn entity->fixed-term-with-negations
    "Given an entity, alter it to work as a query that assumes everything
     it is querying over is semantic. Specifically:
    * Replace 'anything by nil.
    * If an element is not a label, then require it not to have
      a :label element."
  [entity]
  (-> entity
      semantic-to-list
      (transform-pattern-toward-fixed-term :require-not-labels true)))

(defn pattern-to-fixed-term
  "Given a pattern, alter it to work as a fixed-term. Specifically:
    * Replace 'anything by nil.
    * If an element is not a label, require it not to have a :label element.
    * If an entity has nil content, add a '(nil :order) element to make
      it only match user editable elements."
  [pattern]
  (transform-pattern-toward-fixed-term pattern
                                      :require-not-labels true
                                      :require-orders true))

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
   (prewalk-seqs (fn [query] (cond (nil? query)
                                   nil-replacement
                                   (seq? query)
                                   (remove #(or (= % '(nil :order))
                                                (special-form? %))
                                           query)
                                   true
                                   query))
                 query)))

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
  "Given a template alter it to work as a template for a possible non-selector.
   Specifically, replace 'anything by the empty
   string, unless in a part of the template that is marked as a selector,
   in which case don't modify it."
  [pattern]
  (if (sequential? pattern)
    (if (some #(= (content %) :selector)
              (elements pattern))
      pattern
      (map template-to-possible-non-selector-template pattern))
    (if (= 'anything pattern)
      ""
      pattern)))

(defn create-selector-or-non-selector-element
  "Create an element, modifying the template if the target is not a
   a selector. Return the updated store and the id of the new element."
  [template target-id adjacent-id position use-bigger store]
  (let [template (if (and target-id
                          (selector? (id->entity target-id store)))
                   template
                   (template-to-possible-non-selector-template template))]
    (update-add-entity-adjacent-to store target-id template
                                   (id->entity adjacent-id store)
                                   position use-bigger)))

(defn create-possible-selector-elements
  "Create elements, specializing the template as appropriate, depending on
   whether each target is a selector. Return the new ids and the updated
   store."
  [template targets adjacents position use-bigger store]
  (let [[specialized-template store] (specialize-generic template store) 
        flattened-template (flatten-nested-content specialized-template)]
    (thread-map
     (fn [[target adjacent] store]
       (let [[store id] (create-selector-or-non-selector-element
                         flattened-template
                         target adjacent position use-bigger store)]
         [id store]))
     (map vector targets adjacents)
     store)))

;;; Creating new tabs and tables.

(defn tabs-holder-id-R
  "Return the entity that holds all the tabs."
  [store]
  (expr-let [holders (matching-item-ids-R '(nil :tabs) store)]
    (first holders)))

(defn ordered-tabs-ids-R
  "Return the ids of the tabs, in order."
  [store]
  (expr-let [holder-id (tabs-holder-id-R store)]
    (ordered-ids-R (target-label->ids store holder-id :tab)
                   store)))

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
  (add-elements-to-entity-list
   column-header-template ['(??? :label)]))

(defn starting-store
  "Return an initial immutable store. If a tab name is provided, the store
  will have a single tab with that name and a table with that name."
  [tab-name]
  (let [[store _] (add-entity (new-element-store)
                                      nil (list current-format :format))
        [store orderable-id] (add-entity store
                                         nil (list initial :unused-orderable))
        [store tabs-holder-id] (add-entity store nil
                                           '("tabs" :tabs))]
    (if tab-name
      (let [[tab store] (specialize-generic
                         `(""
                           ~tab-name
                           :tab
                           ~(tab-table-element
                             [`(~tab-name :label)] ['(anything (??? :label))]))
                         store)]
        (first (update-add-entity-adjacent-to
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
        (let [[store row-id] (update-add-entity-adjacent-to
                              store nil row-template
                              order-element :before false)]
          (reduce
           (fn [store [header-value cell-value]]
             (first (update-add-entity-adjacent-to
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
        [store new-tab] (update-add-entity-adjacent-to
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


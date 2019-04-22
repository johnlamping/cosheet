(ns cosheet.server.model-utils
  (:require (cosheet [debug :refer [simplify-for-print]]
                     [utils :refer [thread-map prewalk-seqs]]
                     [orderable :as orderable]
                     [expression :refer [expr expr-let expr-seq]]
                     [canonical :refer [canonicalize-list]]
                     [store :refer [new-element-store update-content]]
                     [entity :refer [atom? description->entity elements
                                     content subject label->elements
                                     updating-call-with-immutable
                                     updating-with-immutable
                                     in-different-store]]
                     [store-utils :refer [add-entity]]
                     [query :refer [matching-items]])
            (cosheet.server
             [referent :refer [referent?]]
             [order-utils :refer [order-items-R order-element-for-item
                                  update-add-entity-adjacent-to]])))

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
             (update-content
              store (:item-id last-string-item) next-new)
             (first (add-entity store nil `(~next-new :last-new-string))))])))))

(defn get-n-new-strings
  "Get n new strings"
  [store n]
  (reduce (fn [[headers store] column]
            (let [[header store] (get-new-string store)]
              [(conj headers header) store]))
          [[] store] (range n)))

;;; We have various generic list forms:
;;;  pattern:   a form of a query that can be saved in the store. It uses
;;;             'anything and 'anything-immutable for wildcards,
;;;             where a query would have nil. Patterns that are
;;;             referent exemplars can also contain item referents,
;;;             which will be replaced by the list form of their contents
;;;             in the store.
;;;  query:     a form suitable for use as a query. It can have nils. It may
;;;             specify :top-level to require a top level row item.
;;;  template:  the list form for the content of a new item. It may
;;;             be a pattern, but it can also contain virtual referents.
;;;             It may have 'anything and 'anything-immutable, as they
;;;             are allowed in items. But since they are only allowed
;;;             in selector items, they will be turned into "" when put
;;;             into non-selector items. 
;;;  generic:   a pattern or template that has '??? to indicate values
;;;             that need to be filled out with unique strings.

(defn flatten-nested-content
  "If item has a form anywhere like ((a ...b...) ...c...), turn that into
  (a ...b... ...c...)"
  [item]
  (clojure.walk/postwalk
   (fn [item]
     (if (and (seq? item) (seq? (first item))  (not (referent? (first item))))
       (apply list (concat (first item) (rest item)))
       item))
   item))

(defn pattern-to-query
  "Given a pattern, alter it to work as a query. Specifically,
  replace 'anything and 'anything-immutable by nil
  to make them work as a wild card that avoids matching non-user editable
  elements."
  [pattern]
  (flatten-nested-content ;; Because we can add nested content.
   (clojure.walk/postwalk
    (fn [item] (if (#{'anything 'anything-immutable} item)
                 nil
                 item))
    pattern)))

(defn query-to-template
  "Given a query, turn it into a template by replacing any nil
   by the specified replacement, or the empty string by default."
  ([query]
   (query-to-template query ""))
  ([query nil-replacement]
   (prewalk-seqs
    (fn [query]
      (if (nil? query) nil-replacement query))
    query)))

(defn specialize-template
  "Adjust a template to make it ready for adding as an
  element. Specifically, replace each '??? with a new unique string
  with a leading non-breaking space. Allocating new strings will require
  updating the store. Return the specialized template and the new store."
  [template store]
  (cond
    (= template '???)
    (let [[string new-store] (get-new-string store)]
      [(str "\u00A0" string) new-store])
    (and (sequential? template) (not (referent? template)))
    (thread-map specialize-template template store)
    true
    [template store]))

(defn template-to-possible-non-selector-template
  "Given a template alter it to work as a template for a possible non-selector.
   Specifically, replace 'anything, and 'anything-immutable by the empty
   string, unless in a part of the template that is marked as a selector,
   in which case don't modify it."
  [pattern]
  (if (sequential? pattern)
    (if (some #(or (= % :selector)
                   (and (sequential? %) (= (first %) :selector)))
              (rest pattern))
      pattern
      (map template-to-possible-non-selector-template pattern))
    (if (#{'anything 'anything-immutable} pattern)
      ""
      pattern)))

;;; For purposes of comparing two entities, not all of their elements
;;; matter. In particular, order information, or other information
;;; about how to display the elements is considered irrelevant for
;;; matching a condition. We call the elements that matter the semantic
;;; elements.

(defn semantic-element?
  "Return true if an element counts as semantic information."
  [immutable-entity]
  (let [cont (content immutable-entity)]
    (or (string? cont)
         (number? cont)
         (#{:tag 'anything 'anything-immutable} cont))))

(defn semantic-element?-R
  "Return true if an element counts as semantic information."
  [entity]
   (expr-let [cont (content entity)]
     (or (string? cont)
         (number? cont)
         (#{:tag 'anything 'anything-immutable} cont))))

(defn semantic-elements-R
  "Return the elements of an entity that are semantic to it."
  [entity]
  (updating-with-immutable
   [immutable entity]
   (let [semantic (filter semantic-element? (elements immutable))]
     (if (= immutable entity)
       semantic
       (map #(in-different-store % entity) semantic)))))

(defn immutable-semantic-to-list
  "Given an immutable item, make a list representation of the
  semantic information of the item."
  [item]
  (if (atom? item)
    (content item)
    (let [content (content item)
          elements (semantic-elements-R item)
          content-semantic (immutable-semantic-to-list content)
          element-semantics (map immutable-semantic-to-list elements)]
      (if (empty? element-semantics)
        content-semantic
        (apply list (into [content-semantic] element-semantics))))))

(defn semantic-to-list-R
  "Given an item, make a list representation of the
  semantic information of the item."
  [item]
  (updating-call-with-immutable item immutable-semantic-to-list))

(defn item->canonical-semantic
  "Return the canonical form of the semantic information for the item.
  Only works on immutable items."
  [item]
  (canonicalize-list (immutable-semantic-to-list item)))

(defn item->canonical-semantic-R
  "Return the canonical form of the semantic information for the item."
  [item]
  (updating-call-with-immutable item item->canonical-semantic))

(defn visible-item?-R
  "Return true if an item should be visible information."
  [entity]
  (expr-let [semantic (semantic-element?-R entity)
             elements (elements entity)
             element-contents (expr-seq map content elements)]
    (and semantic
         (not-any? #{:invisible} element-contents))))

(defn visible-elements-R
  "Return the elements of an entity that are visible information about it."
  [entity]
  (expr-let [semantic (semantic-elements-R entity)
             invisible (label->elements entity :invisible)]
    (remove (set invisible) semantic)))

(defn immutable-visible-to-list
  "Given an immutable item, make a list representation of the
  visible information of the item."
  [item]
  (if (atom? item)
    (content item)
    (let [content (content item)
          elements (visible-elements-R item)
          content-visible (immutable-visible-to-list content)
          element-visibles (map immutable-visible-to-list elements)]
      (if (empty? element-visibles)
        content-visible
        (apply list (into [content-visible] element-visibles))))))

(defn visible-to-list-R
  "Given an item, make a list representation of the
  semantic information of the item."
  [item]
  (updating-call-with-immutable item immutable-visible-to-list))

(defn item->canonical-visible
  "Return the canonical form of the semantic information for the item.
  Only works on immutable items."
  [item]
  (canonicalize-list (immutable-visible-to-list item)))

(defn item->canonical-visible-R
  "Return the canonical form of the semantic information for the item."
  [item]
  (updating-call-with-immutable item item->canonical-visible))

(defn selector?
  "Return whether the item is (or is part of) a selector."
  [immutable-item]
  (or (some #(= (content %) :selector) (elements immutable-item))
      (if-let [subj (subject immutable-item)]
        (selector? subj))))

(defn create-selector-or-non-selector-element
  "Create an element, modifying the template if the subject is not a
   a selector. Return the updated store and the id of the new element."
  [template subject adjacent position use-bigger store]
  (let [template (if (and subject (selector? subject))
                   template
                   (template-to-possible-non-selector-template template))]
    (update-add-entity-adjacent-to store (:item-id subject)
                                   template adjacent position use-bigger)))
;;; Creating new tabs and tables.

(defn tabs-holder-item-R
  "Return the item that holds all the tabs."
  [store]
  (expr-let [holders (matching-items '(nil :tabs) store)]
    (first holders)))

(defn first-tab-R
  "Return the first tab, if there is one."
  [store]
  (expr-let [holder (tabs-holder-item-R store)
             tabs (expr order-items-R (label->elements holder :tab))]
    (first tabs)))

(def table-header-template
  ;; A table header is an element of the row condition, so that batch
  ;; edit changes can match table headers. We strip the headers out when
  ;; we get the table condition.
  '(anything :column))

(defn table-tab-non-semantic-elements
  "Return the non-semantic elements for a new tab for a table
  with the given row condition and column conditions."
  [row-condition-elements header-conditions-elements]
  `(:tab
    (:blank
     :tab-topic
     :table
     ~(apply list (concat
                   ['anything]
                   row-condition-elements
                   (map (fn [header-condition-elements]
                          (apply list (concat
                                       table-header-template
                                       header-condition-elements)))
                        header-conditions-elements)
                   [:row-condition
                    :selector])))))

(def new-tab-elements
  (table-tab-non-semantic-elements ['(??? :tag)] [['(??? :tag)]]))

(defn starting-store
  "Return an initial immutable store. If a tab name is provided, the store
  will have a single tab with that name and a table with that name."
  [tab-name]
  (let [[store orderable-id] (add-entity
                              (new-element-store) nil
                              (list orderable/initial :unused-orderable))
        [store tabs-holder-id] (add-entity store nil
                                           '("tabs" :tabs))]
    (if tab-name
      (let [[tab store] (specialize-template
                             (cons "" (cons tab-name
                                            (table-tab-non-semantic-elements
                                             [`(~tab-name :tag)]
                                             [['(??? :tag)]])))
                             store)]
        (first (update-add-entity-adjacent-to
                store tabs-holder-id tab                   
                (description->entity orderable-id store) :after false)))
      store)))

;;; Consistency checks

(defn column-header-problem
  "Return something truthy if the given immutable item is a column header
   with a problem."
  [immutable-item]
  ;; A header is a problem if it has the vacuous condition
  (and ({'anything 'anything-immutable} (content immutable-item))
       (some #(= (content %) :column) (elements immutable-item))
       (not-any? semantic-element? (elements immutable-item))))

(defn avoid-problems
  "Given an old store, a new store, both immutable, and an item where
   changes were made, return the new store if the changes don't have any
   problems, otherwise the old store."
  [old-store new-store item]
  (let [item (in-different-store item new-store)]
    (if (column-header-problem item)
      old-store
      new-store)))

;;; CSV file importing

(defn add-rows
  "Given a sequence of rows, each a sequence of values,
  add data corresponding to them to the store. Return the store and
  values corresponding to the column headers."
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
                     store row-id `(~cell-value (~header-value :tag))
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
  (let [tabs-holder (tabs-holder-item-R store)
        last-tab (last (matching-items '(nil :tabs) store))
        [store new-tab] (update-add-entity-adjacent-to
                         store (:item-id tabs-holder)
                         (cons "" (cons table-name
                                        (table-tab-non-semantic-elements
                                         [`(~table-name :tag)]
                                         (map (fn [header] [`(~header :tag)])
                                              headers))))
                         last-tab :after true)]
    store))

(defn add-table
  "Given a sequence of rows, each a sequence of values,
  add a table corresponding to them to the store, with its own tab."
  [store table-name rows]
  (let [rows-template `("" (~table-name :tag) :top-level)
        [store headers] (add-rows store rows rows-template)]
    (add-table-tab store table-name headers)))


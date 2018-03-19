(ns cosheet.server.model-utils
  (:require (cosheet [debug :refer [simplify-for-print]]
                     [utils :refer [thread-recursive-map]]
                     [orderable :as orderable]
                     [expression :refer [expr expr-let expr-seq]]
                     [store :refer [new-element-store update-content]]
                     [entity :refer [description->entity
                                     content label->elements]]
                     [store-utils :refer [add-entity]]
                     [query :refer [matching-items]])
            (cosheet.server
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

(defn specialize-template
  "Adjust a template or condition to make it ready for adding as an
  element. Specifically, replace each '??? with a new unique string
  with a leading non-breaking space. Allocating new strings will require
  updating the store. Return the new condition and the new store."
  [condition store]
  (thread-recursive-map (fn [item store]
                          (if (= item '???)
                            (let [[string new-store] (get-new-string store)]
                              [(str "\u00A0" string) new-store])
                            [item store]))
                        condition store))


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
  ;; A table header is an element of the condition, so that batch edit changes
  ;; can match table headers. But a header doesn't count as a semantic part
  ;; of the condition, because it shouldn't affect what rows are selected.
  '(anything-immutable (:column :non-semantic)
                       (:non-semantic :non-semantic)))

(defn table-tab-non-semantic-elements
  "Return the non-semantic elements for a new tab for a table
  with the given row condition and column conditions."
  [row-condition-elements header-conditions-elements]
  `((:tab :non-semantic)
    ~(concat
      `(""
        (:non-semantic :non-semantic)
        (:tab-topic :non-semantic)
        (:table :non-semantic)
        ~(apply list (concat
                      ['anything]
                      row-condition-elements
                      (map (fn [header-condition-elements]
                             (apply list (concat
                                          table-header-template
                                          header-condition-elements)))
                           header-conditions-elements)
                      ['(:row-condition :non-semantic)
                       '(:selector :non-semantic)
                       '(:non-semantic :non-semantic)]))))))

(def new-tab-elements
  (table-tab-non-semantic-elements ['(??? :tag)] [['(??? :tag)]]))

(defn starting-store
  "Return an initial store. If a tab name is provided, the store
  will have a single tab with that name and a table with that name."
  [tab-name]
  (let [[store orderable-id] (add-entity
                              (new-element-store) nil
                              (list orderable/initial :unused-orderable))
        [store tabs-holder-id] (add-entity store nil
                                           '("tabs" (:tabs :non-semantic)))]
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
  (let [rows-template `("" (~table-name :tag) (:top-level :non-semantic))
        [store headers] (add-rows store rows rows-template)]
    (add-table-tab store table-name headers)))

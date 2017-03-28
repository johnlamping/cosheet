(ns cosheet.server.model-utils
  (:require [clojure-csv.core :refer [parse-csv]]
            (cosheet [debug :refer [simplify-for-print]]             
                     [expression :refer [expr expr-let expr-seq]]
                     [store :refer [update-content]]
                     [entity :refer [content label->elements]]
                     [store-utils :refer [add-entity]]
                     [query :refer [matching-items]])
            (cosheet.server [order-utils :refer [order-items-R]])))

;;; Utilities that know about how information is encoded in terms of the store.

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

(def new-tab-elements '((:tab :non-semantic)
                        (""
                         (:non-semantic :non-semantic)
                         (:tab-topic :non-semantic)
                         (:table :non-semantic)
                         (anything (??? :tag)
                                   (:row-condition :non-semantic)
                                   (:non-semantic :non-semantic))
                         (anything-immutable (??? :tag)
                                             (:column :non-semantic)
                                             (:non-semantic :non-semantic)))))

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

(comment (defn (add-rows
                "Given a sequence of rows, each a sequence of values,
 add data corresponding to them to the store. Return the store and
 values corresponding to the column headers."[store rows]
                (let [num-columns (apply max (apply count rows))
                      [initial-columns data-rows] (if (every ))]))))


(ns cosheet.server.format-convert
  (:require (cosheet [debug :refer [simplify-for-print]]             
                     [entity :refer [to-list label->elements content]]
                     [store :refer [update-content]]
                     [store-utils :refer [add-entity remove-entity-by-id]]
                     [query :refer [matching-items]])
            (cosheet.server
             [model-utils :refer [specialize-template]])))

(defn convert-from-0-to-1
  "Convert a store from format 0 to format 1.
  The only difference between the two is that in format 1, table columns
  appear as non-semantic elements of the row condition, rather than as
  elements of the table. And format 0 stores don't have a :format indicator."
  [store]
  (let [tables (matching-items '(nil :table) store)]
    (reduce
     (fn [store table]
       (let [condition (first (label->elements table :row-condition))
             columns (label->elements table :column)]
         (reduce
          (fn [store column]
            (first (add-entity (remove-entity-by-id store (:item-id column))
                               (:item-id condition)
                               (to-list column))))
          store columns)))
     (first (add-entity store nil '(1 :format)))
     tables)))

(defn convert-from-1-or-2-to-3
  "Convert a store from format 1 or 2 to format 3.
   The only difference is that in format 3, each table condition is also marked
   with (:selector :non-semantic). We leave behind (:selector :non-semantic)
   that format 2 had on column headers, as they do no harm."
  [store]
  (let [format (first (matching-items '(nil :format) store))
        tables (matching-items '(nil :table) store)]
    (reduce
     (fn [store table]
       (let [condition (first (label->elements table :row-condition))]
         (first (add-entity store (:item-id condition)
                            '(:selector :non-semantic)))))
     (update-content store (:item-id format) 3)
     tables)))

(defn convert-to-current
  "Convert a store to the current format (That's 1 right now."
  [store]
  (let [format (first (matching-items '(nil :format) store))
        format (if format (content format) 0)]
    (assert (and (>= format 0) (<= format 3))
            (str "Store in unknown format " format))
    (cond-> store
      (<= format 0) convert-from-0-to-1
      (<= format 2) convert-from-1-or-2-to-3)))

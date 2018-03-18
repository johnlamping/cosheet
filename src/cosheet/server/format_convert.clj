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

(defn convert-from-1-to-2
  "Convert a store from format 1 to format 2.
   The only difference is that in format 2, each table header is also marked
   with (:selector :non-semantic)."
  [store]
  (let [format (first (matching-items '(nil :format) store))
        tables (matching-items '(nil :table) store)]
    (reduce
     (fn [store table]
       (let [condition (first (label->elements table :row-condition))
             columns (label->elements condition :column)]
         (reduce
          (fn [store column]
            (first (add-entity store
                               (:item-id column)
                               '(:selector :non-semantic))))
          store columns)))
     (update-content store (:item-id format) 2)
     tables)))

(defn convert-to-current
  "Convert a store to the current format (That's 1 right now."
  [store]
  (let [format (first (matching-items '(nil :format) store))
        format (if format (content format) 0)]
    (assert (and (>= format 0) (<= format 2))
            (str ("Store in unknown format " format)))
    (cond-> store
      (<= format 0) convert-from-0-to-1
      (<= format 1) convert-from-1-to-2)))

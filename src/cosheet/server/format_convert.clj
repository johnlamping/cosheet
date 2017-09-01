(ns cosheet.server.format-convert
  (:require (cosheet [debug :refer [simplify-for-print]]             
                     [entity :refer [to-list label->elements content]]
                     [store-utils :refer [add-entity remove-entity-by-id]]
                     [query :refer [matching-items]])
            (cosheet.server
             [model-utils :refer [specialize-template]])))

(defn convert-from-0-to-1
  "Convert a store from format 0 to format 1.
  The only difference between the two is that in format 1, table columns
  appear as non-semantic elements of the row condition, rather than as
  elements of the table."
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

(defn convert-to-current
  "Convert a store to the current format (That's 1 right now."
  [store]
  (let [format (first (matching-items '(nil :format) store))
        format (when format (content format))]
    (cond (nil? format) (convert-from-0-to-1 store)
          (= format 1) store
          true (assert false (str ("Store in unknown format " format))))))

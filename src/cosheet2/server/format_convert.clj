(ns cosheet2.server.format-convert
  (:require (cosheet2 [debug :refer [simplify-for-print]]             
                      [entity :refer [to-list label->elements content]]
                      [store :refer [update-content id-valid?]]
                      [store-utils :refer [add-entity remove-entity-by-id]]
                      [query :refer [matching-items]])))

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
    (assert (#{1 2} (content format)))
    (reduce
     (fn [store table]
       (let [condition (first (label->elements table :row-condition))]
         (first (add-entity store (:item-id condition)
                            '(:selector :non-semantic)))))
     (update-content store (:item-id format) 3)
     tables)))

(defn convert-from-3-to-4
  "Convert a store from format 3 to 4.
  The only difference is that where table-headers had 'anything-immutable
  for their content, they now have 'anything."
  [store]
  (let [format (first (matching-items '(nil :format) store))
        tables (matching-items '(nil :table) store)]
    (assert (= (content format) 3))
    (reduce
     (fn [store table]
       (let [condition (first (label->elements table :row-condition))
             columns (label->elements condition :column)]
         (reduce
          (fn [store column]
            (if (= (content column) 'anything-immutable)
              (update-content store (:item-id column) 'anything)
              store))
          store columns)))
     (update-content store (:item-id format) 4)
     tables)))


(defn convert-from-4-to-5
  "Convert a store from format 4 to 5.
  The only difference is that where there was any element that had
  :non-semantic, remove the :non-semantic, and if it was not a column
  header and its contents was a string or number, replace the contents
  with :blank to make the element non-semantic."
  [store]
  (let [format (first (matching-items '(nil :format) store))
        column-headers (set (matching-items '(nil :column) store))
        wrongly-semantic (filter #(and (or (string? (content %))
                                           (number? (content %)))
                                       (not (column-headers %)))
                                 (matching-items '(nil :non-semantic) store))]
    (assert (= (content format) 4))
    (as-> store store
      (update-content store (:item-id format) [5])
      (reduce (fn [s wrong]
                (update-content s (:item-id wrong) :blank))
              store wrongly-semantic)
      (reduce (fn [s non-semantic]
                (if (id-valid? s (:item-id non-semantic))
                  (remove-entity-by-id s (:item-id non-semantic))
                  s))
              store (matching-items :non-semantic store)))))

(defn convert-from-5-to-6
  "Convert a store from format 5 to 6.
  The only difference is that all :tag values are replaced with :label."
  [store]
  (let [format (first (matching-items '(nil :format) store))
        tags (matching-items :tag store)]
    (assert (#{5 [5]} (content format)))
    (as-> store store
      (update-content store (:item-id format) 6)
      (reduce (fn [s tag]
                (update-content s (:item-id tag) :label))
              store tags))))

(defn convert-to-current
  "Convert a store to the latest format."
  [store]
  (let [format (first (matching-items '(nil :format) store))
        format (if format (content format) 0)
        format (if (vector? format) (first format) format)]
    (assert (and (>= format 0) (<= format 5))
            (str "Store in unknown format " format))
    (cond-> store
      (<= format 0) convert-from-0-to-1
      (<= format 2) convert-from-1-or-2-to-3
      (<= format 3) convert-from-3-to-4
      (<= format 4) convert-from-4-to-5
      (<= format 5) convert-from-5-to-6)))

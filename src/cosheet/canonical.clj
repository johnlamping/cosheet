(ns cosheet.canonical
  (:require (cosheet [utils :refer [multiset replace-in-seqs]]
                     [debug :refer [simplify-for-print]])))

;;; Utilities for converting to and from a canonical description
;;; of an entity and the list form of it, and for operating on the
;;; canonical description.

;;; The canonical form describes atomic elements as themselves,
;;; and non-atomic elements as a pair of their content and a multiset
;;; of the canonical descriptions of their elements.
;;; This makes the description independent of the order of the elements.
;;; It is easier than sorting, because Clojure doesn't define
;;; a sort order between heterogenous types, like strings and ints.

(defn canonicalize-list
  "Given the list form of an entity, return a canonical representation of it."
  [entity]
  (if (sequential? entity)
    [(canonicalize-list (first entity))
     (multiset (map canonicalize-list (rest entity)))]
    entity))

(ns cosheet.mutable-set
  (:require (cosheet [mutable-manager
                      :refer [new-mutable-management
                              get-or-make-reporter
                              describe-and-swap!]])))

;;; Suppor for treating a mutable management as a mutable set of
;;; items, which supports an operation to find the intersection of the
;;; current content with a given set.

(defn new-mutable-set
  [initial]
  (assert (set? initial))
  (new-mutable-management initial))

(defn mutable-set-intersection
  "Return a reporter with the intersection of the current value
  of the set with the collection argument.
  Works on both sets and mutable-sets."
  [mutable-set collection]
  (if (set? mutable-set)
    (clojure.set/intersection mutable-set (set collection))
    (get-or-make-reporter
     collection clojure.set/intersection mutable-set (set collection))))

(defn mutable-set-swap!
  "Update the value in accord with f"
  [mutable-set f]
  (describe-and-swap!
     mutable-set
     (fn [old] (let [new (f old)]
                 [new (clojure.set/union (clojure.set/difference old new)
                                         (clojure.set/difference new old))]))))


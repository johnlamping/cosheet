(ns cosheet.query
  (:require [cosheet.entity :as entity]))

;;; Variables are represented as items of the form
;;; (:variable (<name> :name)
;;;            (<condition> :condition)
;;;            (true :value-may-extend)
;;;            (true :reference) 
;;; where each of the elements is optional
;;;    A variable without a name is considered distinct from any
;;; other variable.
;;;    A variable with a condition can only match entities satisfying
;;; the condition.
;;;    A bound variable with value-may-extend will match any extension of
;;; its value.
;;;    A variable with :reference binds to a reference to an item
;;; in the store, rather than to the item considered as a value. Only
;;; one instance of each reference variable should occur in a query,
;;; since it can never match two different structures.

(defmulti extended-by?
  "Return true if the template, which must not have variables, is
   extended by the target"
  (fn [template target] true))

(defmulti template-matches
  "Return a lazy seq of environments that are extensions of the given
   environment and where the target matches the template."
  (fn ([template target] :no-env)
      ([template env target] :env)))
  
(defmulti query-matches
  "Return a lazy seq of environments that are extensions of the given
   environment and where the store satisfies the instantiated query."
  (fn ([query store] :no-env)
      ([query env store] :env)))
  

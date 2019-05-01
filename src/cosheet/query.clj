(ns cosheet.query
  (:require [cosheet.entity :as entity]
            [cosheet.store :as store]
            [cosheet.expression :refer [expr expr-seq expr-let]]))

;;; There are three levels of matching:
;;;        extended-by?:  Takes a template with no variables and an
;;;                       entity. Says whether the entity extends the template.
;;;    template-matches:  Takes a template, which may have
;;;                       variables, an environment, and an entity.
;;;                       Returns a set of extensions of the environment
;;;                       that cause the entity to be an extension
;;;                       of the template.
;;;       query-matches:  Takes a template, which may have
;;;                       variables, an environment, and a store.
;;;                       Returns a set of extensions of the environment
;;;                       that cause some entity in the store to be
;;;                       an extension of the template.
;;;
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

(defmulti extended-by-m?
  (fn [template target] true))

(defn extended-by?
  "Return true if the template, which must be immutable and not have
  variables, is extended by the target"
  [template target]
  (extended-by-m? template target))

(defmulti template-matches-m
  (fn [template env target] true))

(defn template-matches
  "Return a lazy seq of environments that are extensions of the given
  environment and where the target matches the template, which must be
  immutable."
  ([template target] (template-matches-m template {} target))
  ([template env target] (template-matches-m template env target)))

(defmulti best-template-match-m
  (fn [templates env target] true))

(defn best-template-match
  "Given a sequence of immutable templates, return the most specific
  of those that matches the target, if any."
  ([templates target] (best-template-match-m templates {} target))
  ([templates env target] (best-template-match-m templates env target)))

(defn matching-elements
  "Return all elements of the target that match the condition (which
  must be immutable.)"
  [condition target]
  (assert (not (entity/mutable-entity? condition)))
  (if (or (nil? condition) (= condition '()))
    (entity/elements target)
    (let [template `(nil (:variable (:v :name)
                                    (~condition :condition)
                                    (true :reference)))]
      (if (and (entity/mutable-entity? target)
               (satisfies? entity/StoredEntity target))
        ;; Optimized case to not build reporters for all the subsidiary tests. 
        (expr-let [matches (entity/updating-with-immutable
                            [immutable target]
                            (template-matches template immutable))]
          (map #(entity/in-different-store (:v %) target) matches))
        (expr-let [matches (template-matches template target)]
          (map :v matches))))))
  
(defmulti query-matches-m
  "Return a lazy seq of environments that are extensions of the given
   environment and where the store satisfies the instantiated query."
  (fn [query env store] true))

(defn query-matches
  "Return a lazy seq of environments that are extensions of the given
   environment and where the store satisfies the instantiated query."
  ([query store] (query-matches-m query {} store))
  ([query env store] (query-matches-m query env store)))

(defn matching-items
  "Return all items in the store that match the condition."
  [condition store]
  (expr-let [condition condition]
    (let [template `(:variable (:v :name)
                               (~condition :condition)
                               (true :reference))]
      (if (satisfies? store/MutableStore store)
        ;; Optimized case to not build reporters for all the subsidiary tests. 
        (expr-let [matches (store/call-dependent-on-id
                            store nil #(query-matches template %))]
          (map #(-> %
                    :v  ;; item that is the value of variable :v
                    :item-id  ;; its item id
                    (entity/description->entity store)) 
               matches))
        (expr-let [matches (query-matches template store)]
          (map :v matches))))))

  

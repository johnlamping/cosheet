(ns cosheet.query
  (:require [cosheet.entity :as entity]
            [cosheet.store :as store]
            [cosheet.expression :refer [expr expr-seq expr-let]]
            [cosheet.utils :refer [add-elements-to-entity-list]]))

;;; There are three levels of matching:
;;;        extended-by?:  Takes a template, which may not have variables, and an
;;;                       entity. Says whether the entity extends the template.
;;; matching-extensions:  Takes a template, which may have
;;;                       variables, an environment, and an entity.
;;;                       Returns a set of extensions of the environment
;;;                       that cause the entity to be an extension
;;;                       of the template.
;;;       query-matches:  Takes a query, which may have
;;;                       variables, an environment, and a store.
;;;                       Returns a set of extensions of the environment
;;;                       that cause some entity in the store to be
;;;                       an extension of the template.
;;;
;;; Queries are entities. Normally, they require a straight match, but
;;; a few special forms are recognized, all indicated by their content
;;; being ::special-form and an element (<special-form> :type)
;;; A variable can match anything, and what it matches is recorded
;;;   (::special-form (:variable ::type)
;;;                   (<name> ::name)
;;;                   <template ::template>
;;;                   (true ::value-may-extend)
;;;                   (true ::reference)
;;;   Each of the additional elements is optional.
;;;   A variable without a name is considered distinct from any
;;;   other variable.
;;;   A variable with a template can only match entities satisfying
;;;   the template, which may not contain variables.
;;;   A bound variable with value-may-extend will match any extension of
;;;   its value.
;;;   A variable with :reference binds to a reference to an item
;;;   in the store, rather than to the item considered as a value. Only
;;;   one instance of each reference variable should occur in a query,
;;;   since it can never match two different structures.
;;; A not matches if its template does not match.
;;;   (::special-form (:not ::type) <template ::parameter>)
;;; An and matches if both its templates match, with consistent
;;;   variable bidings.
;;;   (::special-form (:and ::type)
;;;                   <template (::template :first)>
;;;                   <template (::template :second)>
;;; A forall matches if its template matches for every way its variable
;;;   can be bound.
;;;   (::special-form (:forall ::type)
;;;                   <variable ::variable>
;;;                   <template ::template>
;;; An exists matches if the template matches for some way its variable
;;;   can be bound.
;;;   (::special-form (:exists ::type)
;;;                   <variable ::variable>
;;;                   <template ::template>
;;; Only variables and negations may appear as elements in a query,
;;; and :not may currently only appear there.

(defn variable-query
  [name & {:keys [template reference value-may-extend]
           :as keywords}]
  (assert (every? #{:template :reference :value-may-extend} (keys keywords)))
  (when reference (assert (= reference true)))
  (when value-may-extend (assert (= value-may-extend true)))
  (apply list
         (cond-> [::special-form '(:variable ::type)]
           name (conj `(~name ::name))
           template (conj (add-elements-to-entity-list template [::template]))
           reference (conj '(true ::reference))
           value-may-extend (conj '(true ::value-may-extend)))))

(defn not-query
  [query]
  `(::special-form
    (:not ::type)
    ~(add-elements-to-entity-list query ['(::template :first)])))

(defn and-query
  [query1 query2]
  `(::special-form
    (:and ::type)
    ~(add-elements-to-entity-list query1 ['(::template :first)])
    ~(add-elements-to-entity-list query2 ['(::template :second)])))

(defn forall-quqery
  [variable-name variable-template query]
  `(::special-form
    (:forall ::type)
    ~(add-elements-to-entity-list
      (variable-query variable-name :template variable-template) [::variable])
    ~(add-elements-to-entity-list query [::template])))

;;; TODO: Get rid of template for exists
(defn exists-query
  [variable-name variable-template query]
  `(::special-form
    (:exists ::type)
    ~(add-elements-to-entity-list
      (variable-query variable-name :template variable-template) [::variable])
    ~(add-elements-to-entity-list query [::template])))

(defmulti extended-by-m?
  (fn [template target] true))

(defn extended-by?
  "Return true if the template, which must be immutable and not have
  variables, is extended by the target"
  [template target]
  (extended-by-m? template target))

(defmulti matching-extensions-m
  (fn [template env target] true))

(defn matching-extensions
  "Return a lazy seq of environments that are extensions of the given
  environment and where the target matches the template, which must be
  immutable."
  ([template target] (matching-extensions-m template {} target))
  ([template env target] (matching-extensions-m template env target)))

(defmulti best-template-match-m
  (fn [templates env target] true))

(defn best-template-match
  "Given a sequence of immutable templates, return the most specific
  of those that matches the target, if any."
  ([templates target] (best-template-match-m templates {} target))
  ([templates env target] (best-template-match-m templates env target)))

(defn matching-elements
  "Return all elements of the target that match the template (which
  must be immutable.)"
  [template target]
  (assert (not (entity/mutable-entity? template)))
  (if (or (nil? template) (= template '()))
    (entity/elements target)
    (let [template `(nil ~(variable-query
                           ::v :template template :reference true))]
      (if (and (entity/mutable-entity? target)
               (satisfies? entity/StoredEntity target))
        ;; Optimized case to not build reporters for all the subsidiary tests. 
        (expr-let [matches (entity/updating-with-immutable
                            [immutable target]
                            (matching-extensions template immutable))]
          (map #(entity/in-different-store (::v %) target) matches))
        (expr-let [matches (matching-extensions template target)]
          (map ::v matches))))))
  
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
  "Return all items in the store that match the template."
  [template store]
  (let [template (variable-query ::v :template template :reference true)]
    (if (satisfies? store/MutableStore store)
      ;; Optimized case to not build reporters for all the subsidiary tests. 
      (expr-let [matches (store/call-dependent-on-id
                          store nil #(query-matches template %))]
        (map #(-> %
                  ::v  ;; item that is the value of variable ::v
                  :item-id  ;; its item id
                  (entity/description->entity store)) 
             matches))
      (expr-let [matches (query-matches template store)]
        (map ::v matches)))))

  

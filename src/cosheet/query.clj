(ns cosheet.query
  (:require [cosheet.entity :as entity]
            [cosheet.store :as store]
            [cosheet.expression :refer [expr expr-seq expr-let]]
            [cosheet.utils :refer [add-elements-to-entity-list]]))

;;; The simplest query is just an entity that constitutes a pattern that is to
;;; be matched against a target. There are three levels of elaboration
;;; that incorporated more kinds of objects into entities to yield more
;;; involved queries.
;;;   fixed-term  May have nil as a content, indicating anything.
;;;               And may have negated elements, which match if the target
;;;               does not have a matching element.
;;;         term  May also have variables. All occurrences of a variable
;;;               with the same name have to match the same value.
;;;        query  May also have quantifiers and conjunctions.

;;; There are also three levels of scope of querying. They support progressively
;;; larger generality of querying, and accept progressively more general
;;; expressions
;;;        extended-by?:  Takes a fixed-term and an entity
;;;                       Says whether the entity extends the term.
;;; matching-extensions:  Takes a term, an environment, and an entity.
;;;                       Returns a set of extensions of the environment
;;;                       that cause the term to be an extension
;;;                       of the term.
;;;       query-matches:  Takes a query, an environment, and a store.
;;;                       Returns a set of extensions of the environment
;;;                       that cause some entity in the store to be
;;;                       an extension of the query.

;;; Internally, the elaborations are indicated with special forms, indicated by
;;; their content being ::special-form and an element (<special-form> :type)
;;; Client code should never have to know these details, as there are functions
;;; to construct each of the special forms.
;;; A variable can match anything, and what it matches is recorded.
;;;   (::special-form (:variable ::type)
;;;                   (<name> ::name)
;;;                   <qualifier ::sub-query>
;;;                   (true ::value-may-extend)
;;;                   (true ::reference))
;;;   Each of the elements except for the type is optional.
;;; A not matches if its sub-query does not match.
;;;   (::special-form (:not ::type) <sub-query ::sub-query>)
;;; An and matches if both its sub-queries match, with consistent
;;;   variable bidings.
;;;   (::special-form (:and ::type)
;;;                   <sub-query (::sub-query :first)>
;;;                   <sub-query (::sub-query :second)>)
;;; A forall matches if its query matches for every way its variable
;;;   can be bound.
;;;   (::special-form (:forall ::type)
;;;                   <variable ::variable>
;;;                   <sub-query ::sub-query>)
;;; An exists matches if its query matches for some way its variable
;;;   can be bound.
;;;   (::special-form (:exists ::type)
;;;                   <variable ::variable>
;;;                   <sub-query ::sub-query>)

;;; A variable with a name of nil is considered distinct from any
;;; other variable.
;;; A variable with a qualifier can only match entities satisfying
;;; the qualifier, which may not contain variables.
;;; A bound variable with value-may-extend will match any extension of
;;; its value.
;;; A variable with :reference binds to a reference to an item
;;; in the store, rather than to the value of the item. Only
;;; one instance of reference variable with a given name should occur
;;; in a query,
;;; since it can never match two different structures.
(defn variable-query
  [name & {:keys [qualifier reference value-may-extend]
           :as keywords}]
  (assert (every? #{:qualifier :reference :value-may-extend} (keys keywords)))
  (when reference (assert (= reference true)))
  (when value-may-extend (assert (= value-may-extend true)))
  (apply list
         (cond-> [::special-form '(:variable ::type)]
           name (conj `(~name ::name))
           qualifier (conj (add-elements-to-entity-list
                            qualifier [::sub-query]))
           reference (conj '(true ::reference))
           value-may-extend (conj '(true ::value-may-extend)))))

(defn not-query
  [query]
  `(::special-form
    (:not ::type)
    ~(add-elements-to-entity-list query ['(::sub-query :first)])))

(defn and-query
  [query1 query2]
  `(::special-form
    (:and ::type)
    ~(add-elements-to-entity-list query1 ['(::sub-query :first)])
    ~(add-elements-to-entity-list query2 ['(::sub-query :second)])))

(defn forall-query
  [variable-name variable-qualifier query]
  `(::special-form
    (:forall ::type)
    ~(add-elements-to-entity-list
      (variable-query variable-name :qualifier variable-qualifier) [::variable])
    ~(add-elements-to-entity-list query [::sub-query])))

(defn exists-query
  [variable-name variable-qualifier query]
  `(::special-form
    (:exists ::type)
    ~(add-elements-to-entity-list
      (variable-query variable-name :qualifier variable-qualifier) [::variable])
    ~(add-elements-to-entity-list query [::sub-query])))

(defmulti extended-by-m?
  (fn [fixed-term target] true))

(defn extended-by?
  "Return true if the fixed-term is extended by the target"
  [fixed-term target]
  (extended-by-m? fixed-term target))

(defmulti matching-extensions-m
  (fn [term env target] true))

(defn matching-extensions
  "Return a lazy seq of environments that are extensions of the given
  environment and where the target matches the term, which must be
  immutable."
  ([term target] (matching-extensions-m term {} target))
  ([term env target] (matching-extensions-m term env target)))

(defmulti best-matching-query-m
  (fn [terms env target] true))

(defn best-matching-query
  "Given a sequence of immutable terms, return the most specific
  of those that matches the target, if any."
  ([terms target] (best-matching-query-m terms {} target))
  ([terms env target] (best-matching-query-m terms env target)))

(defn matching-elements
  "Return all elements of the target that match the term (which
  must be immutable.)"
  [term target]
  (assert (not (entity/mutable-entity? term)))
  (if (or (nil? term) (= term '()))
    (entity/elements target)
    (let [term `(nil ~(variable-query
                        ::v :qualifier term :reference true))]
      (if (and (entity/mutable-entity? target)
               (satisfies? entity/StoredEntity target))
        ;; Optimized case to not build reporters for all the subsidiary tests. 
        (expr-let [matches (entity/updating-with-immutable
                            [immutable target]
                            (matching-extensions term immutable))]
          (map #(entity/in-different-store (::v %) target) matches))
        (expr-let [matches (matching-extensions term target)]
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
  (let [template (variable-query ::v :qualifier template :reference true)]
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

  

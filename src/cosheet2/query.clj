(ns cosheet2.query
  (:require (cosheet2 [entity :refer [content label->element label->content
                                      to-list]]
                      [utils :refer [add-elements-to-entity-list]])))

;;; The simplest query is just an entity that constitutes a pattern that is to
;;; be matched against a target. There are three levels of elaboration
;;; that incorporate more kinds of objects into entities to yield more
;;; involved queries.
;;;   fixed-term  May have nil as a content, indicating anything.
;;;               And may have negated elements, which match if the target
;;;               does not have an element that matches them.
;;;         term  May also have variables. All occurrences of a variable
;;;               with the same name have to match the same value.
;;;        query  May also have quantifiers and conjunctions.

;;; There are several querying operations, that differ in how elaborate
;;; a kind of query they take and in whether their target is a single entity
;;; or the whole store. All only take immutable arguments.

;;;        extended-by?:  Takes a fixed-term and an entity
;;;                       Says whether the entity extends the term.
;;; matching-extensions:  Takes a term, an environment, and an entity.
;;;                       Returns a set of extensions of the environment
;;;                       that cause the term to be an extension
;;;                       of the entity.
;;;   matching-elements:  Takes a term and a target. Returns a seq of all
;;;                       elements of the target that are extensions of
;;;                       the term.
;;;  best-matching-term:  Takes a seq of terms and a target. Returns the most
;;;                       specific of the terms for which the target is an
;;;                       extension.
;;;      matching-items: Takes a term and a store.  Returns
;;;                       a seq of all items in the store that are
;;;                       extensions of the term.
;;;       query-matches:  Takes a query, an environment, and a store.
;;;                       Returns a set of extensions of the environment
;;;                       that cause some entity in the store to be
;;;                       an extension of the query.

;;; TODO: Add functions that return all items matching a query, and that return
;;; whether an item matches a query. Change query-calculator to use them,
;;; rather than requiring terms.

;;; Internally, the elaborations are indicated with special forms, indicated by
;;; their content being ::special-form and an element (<special-form> :type)
;;; Client code should never have to know these details, as there are functions
;;; to construct each of the special forms.

;;; A variable can match anything, and what it matches is recorded.
;;;   (::special-form (:variable ::type)
;;;                   (<name> ::name)
;;;                   <qualifier ::sub-query>
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
;;; A variable with :reference binds to a reference to an item
;;; in the store, rather than to the value of the item. Only
;;; one instance of a reference variable with a given name should occur
;;; in a query,
;;; since it can never match two different structures.
(defn variable-query
  [name & {:keys [qualifier reference]
           :as keywords}]
  (assert (every? #{:qualifier :reference} (keys keywords)))
  (when reference (assert (= reference true)))
  (apply list
         (cond-> [::special-form '(:variable ::type)]
           name (conj `(~name ::name))
           qualifier (conj (add-elements-to-entity-list
                            qualifier [::sub-query]))
           reference (conj '(true ::reference)))))

(defn not-query
  [query]
  `(::special-form
    (:not ::type)
    ~(add-elements-to-entity-list query [::sub-query])))

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

(defn special-form?
  [query]
  (and (seq? query) (= (first query) ::special-form)))

(defn variable-query? [query]
  (and (= (content query) ::special-form)
        (= (label->content query ::type) :variable)))

(defn variable-qualifier [variable]
  (label->element variable ::sub-query))

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

(defmulti best-matching-term-m
  (fn [terms env target] true))

(defn best-matching-term
  "Given a sequence of immutable terms, return the most specific
  of those that matches the target, if any."
  ([terms target] (best-matching-term-m terms {} target))
  ([terms env target] (best-matching-term-m terms env target)))

(defmulti matching-elements-m
  "Return all elements of the target that match the term."
  (fn [term target] true))

(defn matching-elements
   "Return all elements of the target that match the term."
  [term target]
  (matching-elements-m term target))

(defmulti matching-items-m
  "Return all items in the store that match the term."
  (fn [term store] true))

(defn matching-items [term store]
  (matching-items-m term store))
  
(defmulti query-matches-m
  "Return a lazy seq of environments that are extensions of the given
   environment and where the store satisfies the instantiated query."
  (fn [query env store] true))

(defn query-matches
  "Return a lazy seq of environments that are extensions of the given
   environment and where the store satisfies the instantiated query."
  ([query store] (query-matches-m query {} store))
  ([query env store] (query-matches-m query env store)))

;;; This is a utility for debugging the results of queries. We put it here
;;; because it is query specific
(defn envs-to-list [envs]
  "Given a vector of environments, as returned by a query, turn it into maps
   of the list form of the environments."
  (seq (for [env envs]
         (zipmap (keys env)
                 (map #(to-list %) (vals env))))))

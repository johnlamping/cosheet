(ns cosheet.query
  (:require (cosheet [entity :refer [content elements
                                      label->elements label->content
                                      to-tree object? primitive? orientation
                                      make-element-list
                                      add-elements-to-entity]])))

;;; Querying involves looking for entities that are extensions of a
;;; query term.  For an entity to be an extension, it must be possible
;;; to turn the term into the entity by some combination of
;;;   * Adding elements to some of its entities.
;;;   * Replacing some of its nil contents with entities.
;;;   * Replacing any of its variables by entities, using the same
;;;     replacement for each occurrence of a variable. (And then those
;;;     replacements may not be extended; each replacement must be identical.

;;; The simplest query is just an entity that constitutes a pattern that is to
;;; be matched against a subject. There are three levels of elaboration
;;; that incorporate more kinds of objects into the patterns to yield more
;;; involved queries.
;;;   fixed-term  May have nil as the content of an element, indicating
;;;               anything.  And may have negated elements, which
;;;               match if the subject does not have an element that
;;;               matches them.
;;;         term  May also have variables. All occurrences of a variable
;;;               with the same name have to match the same value.
;;;        query  May also have quantifiers and conjunctions.

;;; Internally, the elaborations are indicated with special forms,
;;; which are elements whose content is ::special-form and that have
;;; an element (<special-form> :type) Client code should never have to
;;; know these details, as there are functions to construct each of
;;; the special forms.

;;; All special forms can have sub-queries. These are encoded by
;;; elements with a label of ::sub-query. But they need to be able to
;;; encode primitives, objects, or elements. And for elements, they
;;; need to be able to encode either orientation. These cases are
;;; indicated by a few additional labels on the sub-query.
;;;   * If the sub-query is a primitive or an object, it is
;;;     represented by an element with the sub-query as its content
;;;     and with the ::content and :sub-query keywords.
;;;   * Otherwise, the sub-query is an element.
;;;       * If it has orientation :source, it is represented by
;;;         itself, plus the ::sub-query keyword.
;;;       * if it has orientation :target, it is represented by the
;;;         equivalent element, but with orientation :source, plus the
;;;         keywords ::reversed and ::sub-query.

;;; A variable can match anything, and what it matches is recorded.
;;;   (::special-form (:variable ::type)
;;;                   (<name> ::name)
;;;                   <qualifier> encoded as a sub-query
;;;                   (true ::reference))
;;;
;;; Each of the elements except for the type is optional.
;;;   * A variable with a name of nil is considered distinct from any
;;;     other variable.
;;;   * A variable with a qualifier can only match entities satisfying
;;;     the qualifier.
;;;   * A variable with ::reference binds to an item in the
;;;     store, rather than to an abstract pattern.
;;;     If more than one instance of a reference variable with a given
;;;     name occurs in a query, it can only match named objects or
;;;     constants, since those are the only things that can be
;;;     identical at different sites.

;;; A not matches if its sub-query does not match.
;;;   (::special-form (:not ::type) <sub-query>)

;;; An and matches if both its sub-queries match, with consistent
;;; variable bidings.
;;;   (::special-form (:and ::type)
;;;                   <sub-query> also tagged with ::first
;;;                   <sub-query> also tagged with ::second)

;;; A forall matches if its query matches for every way its variable
;;; can be bound.
;;;   (::special-form (:forall ::type)
;;;                   Note: appends ::variable as an element of the variable.
;;;                   <variable ::variable>
;;;                   <sub-query>)

;;; An exists matches if its query matches for some way its variable
;;; can be bound.
;;;   (::special-form (:exists ::type)
;;;                   Note: appends ::variable as an element of the variable.
;;;                   <variable ::variable>
;;;                   <sub-query>)

;;; There are several querying operations, that differ in how
;;; elaborate a kind of query they take and in whether they operate on
;;; a single entity or on the whole store. All only take immutable
;;; arguments. Where an environment is mentioned, it means a binding
;;; from query variables to entities.
;;;        extended-by?: Takes a fixed-term and a subject entity. Says
;;;                      whether the subject extends the term.
;;; matching-extensions: Takes a term, an environment, and a subject
;;;                      entity.  Returns a set of extensions of the
;;;                      environment that cause the subject to be an
;;;                      extension of the term.
;;;   matching-elements: Takes a term and a subject entity. Returns a
;;;                      seq of all elements of the subject that are
;;;                      extensions of the term.
;;;      matching-items: Takes a term and a store.  Returns
;;;                      a seq of all items in the store that denote
;;;                      entities that are extensions of the term.
;;;       query-matches: Takes a query, an environment, and a store.
;;;                      Returns a seq of extensions of the
;;;                      environment that cause some entity in the
;;;                      store to be an extension of the query.

;;; TODO: Add functions that return all items matching a query, and ones
;;; that return whether an item matches a query. Change
;;; query-calculator to use them, rather than requiring terms.

(defn encode-sub-query
  "Encode a sub-query as an element, as described above."
  [sub-query]
  (concat (if (or (primitive? sub-query) (object? sub-query))
            (make-element-list :source sub-query '(::content))
            (if (= (orientation sub-query) :target)
              (make-element-list :source
                                 (content sub-query)
                                 (concat (elements sub-query)
                                         '(::reversed)))
              sub-query))
          '(::sub-query)))

(defn decode-sub-query
  [encoded]
  "Decode a sub-query that was encoded as an element, as described above."
  (when encoded
    (if (some #(= ::content %) (elements encoded))
      (content encoded)
      (let [cleaned (remove #{::sub-query ::first ::second ::reversed}
                            encoded)]
        (if (some #(= ::reversed %) (elements encoded))
          (make-element-list :target (content cleaned) (elements cleaned))
          cleaned)))))

(defn variable-query
  [name & {:keys [qualifier reference]
           :as keywords}]
  (assert (every? #{:qualifier :reference} (keys keywords)))
  (when reference (assert (= reference true)))
  (apply list
         (cond-> [::special-form '(:variable ::type)]
           name (conj `(~name ::name))
           qualifier (conj (encode-sub-query qualifier))
           reference (conj '(true ::reference)))))

(defn not-query
  [query]
  `(::special-form
    (:not ::type)
    ~(encode-sub-query query)))

(defn and-query
  [query1 query2]
  `(::special-form
    (:and ::type)
    ~(add-elements-to-entity (encode-sub-query query1) '(::first))
    ~(add-elements-to-entity (encode-sub-query query2) '(::second))))

(defn forall-query
  [variable-name variable-qualifier query]
  `(::special-form
    (:forall ::type)
    ~(add-elements-to-entity
      (variable-query variable-name :qualifier variable-qualifier) [::variable])
    ~(encode-sub-query query)))

(defn exists-query
  [variable-name variable-qualifier query]
  `(::special-form
    (:exists ::type)
    ~(add-elements-to-entity
      (variable-query variable-name :qualifier variable-qualifier) [::variable])
    ~(encode-sub-query query)))

(defn special-form?
  [query]
  (and (seq? query) (= (first query) ::special-form)))

(defn special-form-type
  [query]
  (label->content query ::type))

(defn variable-query? [query]
  (and (= (content query) ::special-form)
       (= (label->content query ::type) :variable)))

(defn variable-name [variable]
  (label->content variable ::name))

(defn variable-qualifier [variable]
  (let [qualifier (first (label->elements variable ::sub-query))]
    (decode-sub-query qualifier)))

(defn variable-reference [variable]
  (label->content variable ::reference))

(defn sub-query
  "Return the unique sub-query of the query"
  [query]
  (decode-sub-query (first (label->elements query ::sub-query))))

(defn sub-queries
  "The query must have two sub-queries. Return them in order." 
  [query]
  (let [queries (label->elements query ::sub-query) ]
    (map decode-sub-query
         (if (label->content (second queries) ::first)
           (reverse queries)
           queries))))

(defn quantifier-variable
  "Return the variable that a quantifier quantifies over."
  [quantifier-query]
  (first (label->elements quantifier-query ::variable)))

;;; We want to declare functions here, and implement them in
;;; query-impl.  But there doesn't seem to be a way to declare a
;;; function in one namespace and give its implementation in
;;; another. So, instead, for each function, we define a related
;;; multimethod here, which will be implemented in query-impl. Then,
;;; we define the function to call the multimethod.

(defmulti extended-by-m?
  (fn [fixed-term subject] true))

(defn extended-by?
  "Return true if the fixed-term is extended by the subject entity"
  [fixed-term subject]
  (extended-by-m? fixed-term subject))

(defmulti matching-extensions-m
  (fn [term env subject] true))

(defn matching-extensions
  "Return a lazy seq of environments that are extensions of the given
  environment and for which the subject entity matches the term, which
  must be immutable."
  ([term subject] (matching-extensions-m term {} subject))
  ([term env subject] (matching-extensions-m term env subject)))

(defmulti matching-elements-m
  "Return all elements of the subject entity that match the term."
  (fn [term subject] true))

(defn matching-elements
   "Return all elements of the subject entity that match the term."
  [term subject]
  (matching-elements-m term subject))

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
;;; because it is query specific.
(defn envs-to-list [envs]
  "Given a vector of environments, as returned by a query, turn it into maps
   of the list form of the environments."
  (seq (for [env envs]
         (zipmap (keys env)
                 (map #(to-tree %) (vals env))))))

(ns cosheet.query
  (:require (cosheet [entity :refer [content all-elements
                                     forward-elements reverse?
                                     label->elements label->content
                                     object? primitive?
                                     tree-entity? 
                                     make-tree-element make-tree-object
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
;;; be matched against a subject. There are four levels of elaboration
;;; that incorporate more kinds of objects into the patterns to yield more
;;; involved queries.
;;;  simple-term  May have nil as the content of an element, indicating
;;;               anything.
;;;   fixed-term  May have nil as the content of an element, indicating
;;;               anything.  And may have negated elements, which
;;;               match if the subject does not have an element that
;;;               matches them.
;;;         term  May also have variables. All occurrences of a variable
;;;               with the same name have to match the same value.
;;;        query  May also have quantifiers and conjunctions.

;;; Internally, the elaborations are indicated with special forms,
;;; which are entities marked by having an element of the form (<type>
;;; ::special-form-type). A special form that matches primitives or
;;; objects will be an object, while one that matches elements will be
;;; an element. (Its content it will be ::special-form, but it is
;;; never used.)

;;; Many special forms can have subsidiary queries. These include, for
;;; example, the sub-queries of a conjunction, and the variable and
;;; contained query of a quantifier.

;;; Each special term constructor takes an optional form argument,
;;; which must have value :element or :object, and says which to
;;; make. Without it, the form follows the constructor's sub-query: an
;;; object sub-query gives an object, while an element or primitive
;;; sub-query gives an element. (A primitive sub-query typically
;;; stands for an element.)

;;; Subqueries are encoded by elements with a label of ::sub-query or
;;; ::variable.  But they need to be able to encode primitives,
;;; objects, or elements. And for elements, they need to be able to
;;; encode either direction. The different cases are indicated by a
;;; possible additional labels on the subquery.
;;;   * If the subquery is a primitive or an object, it is represented
;;;     by an element with with an additional ::content keyword and
;;;     with the subquery as its content.
;;;   * Otherwise, the sub-query is an element.
;;;       * If it is a forward element, it is represented by itself
;;;         (plus the label that tags it as a subquery).
;;;       * if it is a reverse element, it is represented by the
;;;         equivalent forward element, plus the additional label
;;;         ::flipped.

;;; Client code should never have to know these details, as there are
;;; functions to construct each of the special forms.

;;; A variable can match anything, and the entity it matches is
;;; recorded. Its possible elements are:
;;;    (:variable ::special-form-type)
;;;    (<name> ::name)
;;;    (<qualifier> ::sub-query) - with subquery encoding
;;;    (true ::reference))
;;; The qualifier and ::reference are optional.
;;; Here are the rules for variable matching:
;;;   * If a term contains several variables with the same name, each
;;;     occurrence must match equivalent entities (entities that
;;;     canonicalize the same). What is recorded is the entity matched
;;;     by the first variable occurrence the query processing
;;;     encounters.
;;;   * Further, if the variables with the same name are marked
;;;     ::reference, all their occurrences must match the exact same
;;;     entity. (If there is more than one instance of the variable,
;;;     this means it can only match named objects or constants, since
;;;     those are the only things that can be identical at different
;;;     sites.)
;;;   * A variable with a qualifier can only match entities satisfying
;;;     the qualifier. If a term has several variables with the same
;;;     name, they should each have the same qualifier, with one
;;;     exception: The qualifier of a ::reference variable might,
;;;     itself, contain a ::reference variable with that same name;
;;;     that instance doesn't need to have a qualifier. (In fact, it
;;;     couldn't have the same qualifier without creating
;;;     circularity.) (The implementation of queries only checks the
;;;     qualifier for the first occurrence of a variable with any
;;;     given name that it encounters. There are no guarantees about
;;;     which occurrence it will encounter first, except that it will
;;;     encounter a variable before it encounters any variable in its
;;;     qualifier.)

;;; A not matches if its sub-query does not match. It has only two
;;; elements:
;;;   (:not ::special-form-type)
;;;   (<sub-query> ::sub-query)  - with subquery encoding

;;; An and matches if both its sub-queries match, with consistent
;;; variable bidings. Without a form argument, its sub-queries must
;;; call for the same form. It's elements are:
;;;   (:and ::special-form-type)
;;;   (<sub-query> ::sub-query ::first) - with subquery encoding
;;;   (<sub-query> ::sub-query ::second) - with subquery encoding

;;; A forall matches if its query matches for every way its variable
;;; can be bound. Its variable will be an object iff the quantifier is.
;;; Its elements are:
;;;   (:forall ::special-form-type)
;;;   (<variable> ::variable) - with subquery encoding
;;;   (<sub-query> ::sub-query) - with subquery encoding

;;; An exists matches if its query matches for some way its variable
;;; can be bound. Its variable will be an object iff the quantifier is.
;;; Its elements are:
;;;   (:exists ::special-form-type)
;;;   (<variable> ::variable) - with subquery encodingg
;;;   (<sub-query> ::sub-query) - with subquery encoding

;;; There are several querying operations. They differ in how
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

(defn- encode-subquery
  "Encode a query as an element with the given tag, which defaults to
  ::sub-query, as described above for sub-queries."
  ([query] (encode-subquery query ::sub-query))
  ([query tag]
   (concat (if (or (primitive? query) (object? query))
             (make-tree-element query '(::content))
             (if (reverse? query)
               (make-tree-element (content query)
                                  (concat (all-elements query)
                                          '(::flipped)))
               query))
           (list tag))))

(defn decode-sub-query
  [encoded]
  "Decode a sub-query, or a quantifier's variable, that was encoded as
  an element, as described above."
  (when encoded
    (if (some #(= ::content %) (forward-elements encoded))
      (content encoded)
      (let [cleaned (remove #{::sub-query ::first ::second ::flipped
                              ::variable}
                            encoded)]
        (if (some #(= ::flipped %) (forward-elements encoded))
          (make-tree-element
           (content cleaned) (forward-elements cleaned) :reverse)
          cleaned)))))

(defn- make-special-form
  "Make a special form of the given type with the given elements, as
  an element or an object according to entity-type."
  [entity-type special-form-type elements]
  (assert (#{:element :object} entity-type) entity-type)
  (let [elements-with-type (cons `(~special-form-type ::special-form-type)
                                 elements)]
    (case entity-type
      :element (make-tree-element ::special-form elements-with-type)
      :object (make-tree-object elements-with-type))))

(defn- form-for-sub-query
  "Return the form for a special form with the given sub-query: an
  object if the sub-query is an object, otherwise an element."
  [sub-query]
  (if (object? sub-query) :object :element))

(defn make-variable-query
  [name & {:keys [qualifier reference form]
           :as keywords}]
  (assert (or (symbol? name) (string? name)) name)
  (assert (every? #{:qualifier :reference :form} (keys keywords)))
  (when reference (assert (= reference true)))
  (assert (or form qualifier) "A variable without a qualifier needs a form.")
  (make-special-form (or form (form-for-sub-query qualifier))
                     :variable
                     (cond-> []
                       name (conj `(~name ::name))
                       qualifier (conj (encode-subquery qualifier))
                       reference (conj '(true ::reference)))))

(defn make-not-query
  ([query]
   (make-not-query query (form-for-sub-query query)))
  ([query form]
   (make-special-form form :not [(encode-subquery query)])))

(defn make-and-query
  ([query1 query2]
   (let [form (form-for-sub-query query1)]
     (assert (= form (form-for-sub-query query2)) [query1 query2])
     (make-and-query query1 query2 form)))
  ([query1 query2 form]
   (make-special-form
    form :and
    [(add-elements-to-entity (encode-subquery query1) '(::first))
     (add-elements-to-entity (encode-subquery query2) '(::second))])))

(defn- quantifier-elements
  "Return the elements of a quantifier of the given form over the
  named variable, with the given qualifier, and with the given query
  as its body."
  [variable-name variable-qualifier query form]
  [(encode-subquery
    (make-variable-query
     variable-name :qualifier variable-qualifier :form form)
    ::variable)
   (encode-subquery query)])

(defn make-forall-query
  ([variable-name variable-qualifier query]
   (make-forall-query variable-name variable-qualifier query
                      (form-for-sub-query query)))
  ([variable-name variable-qualifier query form]
   (make-special-form form :forall
                      (quantifier-elements
                       variable-name variable-qualifier query form))))

(defn make-exists-query
  ([variable-name variable-qualifier query]
   (make-exists-query variable-name variable-qualifier query
                      (form-for-sub-query query)))
  ([variable-name variable-qualifier query form]
   (make-special-form form :exists
                      (quantifier-elements
                       variable-name variable-qualifier query form))))

(defn special-form?
  "Return true if the query is a special form: an entity with an
  element labeled ::special-form-type."
  [query]
  (boolean (seq (label->elements query ::special-form-type))))

(defn special-form-type
  [query]
  (label->content query ::special-form-type))

(defn variable-query? [query]
  (and (special-form? query)
       (= (special-form-type query) :variable)))

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
  (decode-sub-query (first (label->elements quantifier-query ::variable))))

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
  (assert (tree-entity? fixed-term))
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


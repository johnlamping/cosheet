(ns cosheet2.query-impl
  (:require (cosheet2 [store :as store :refer [candidate-matching-ids]]
                      [entity :refer [mutable-entity? primitive? object?
                                      uniquely-identified-object?
                                      stored-entity?
                                      id->entity entity-key
                                      orientation content elements
                                      make-element-list make-object-list
                                      label->elements
                                      label->content
                                      to-list
                                      label-element?]]
                      [query :refer [extended-by-m?
                               matching-extensions-m
                               matching-elements-m
                               matching-items-m
                               query-matches-m
                               special-form?
                               special-form-type
                               variable-query?
                               variable-name
                               variable-qualifier
                               variable-reference
                               sub-queries
                               sub-query
                               quantifier-variable]]
                      [canonical :refer [equivalent-primitives?
                                         canonicalize]]
                      [utils :refer [prewalk-seqs unzip
                                     conj-disjoint-combinations
                                     disjoint-combinations]])))

;;; TODO: !!! Do checking for non-generic objects, not moving into them.

;;; TODO:
;;; Add a term syntax that lets variables bind to the subject.
;;; Add a unification operation on terms, so rule matching can work.
;;;    The environment must include variable numbers, for renaming,
;;;    and an indication of the number of the current term, which must
;;;    be the highest number.

(defn distinct-concat
  "Given a sequence of sequences, each sequence having no repeated
   elements, concatenate the sequences, and remove duplicates."
  [sequences]
  (if (empty? (rest sequences))
    (first sequences)
    (seq (distinct (apply concat sequences)))))

(defn separate-negations
  "Given a seq of terms, return two seqs, one of positive terms,
   and one of negated terms."
  [terms]
  (let [grouped (group-by
                 (fn [term]
                   (if (and (special-form? term)
                            (= (special-form-type term) :not))
                         :negation
                         :positive))
                 terms)]
    [(:positive grouped) (map sub-query (:negation grouped))]))

(declare extended-by?)

(defn combine-exact-matches
  "Given two exact match indicators from different parts of a term,
  return their joint indicator.
  The possible input values are:
                       false: not an exact match
                        true: an exact match
     a set of variable names: an exact match, provided no other unbound
                              references to those variables appear
  The output is in the same format."
  [m1 m2]
  (if (and m1 m2)
   (if (= m1 true)
     m2
     (if (= m2 true)
       m1
       (if (not-any? m1 m2)
         (clojure.set/union m1 m2)
         false)))
   false))

(defn term-orientation
  "Return the orientation required to match a term. Return nil if either
  orientation will match."
  [term env]
  (if (variable-query? term)
    (let [var-name (variable-name term)
          qualifier (variable-qualifier term)
          value (env var-name)]
      (cond
        value (orientation value)
        qualifier (orientation qualifier)
        true nil))
    (orientation term)))

(defn contextualize-variable
  "If the term is a variable, replace it by it's value in the environment,
  or if there is no value, then by its qualifier. Also return the
  information for whether the contextualized result is an exact match
  for the term, using the format expected by combine-exact-matches.)"
  [term env]
  (if (variable-query? term)
    (let [var-name (variable-name term)
          value (env var-name)]
      (if value
        ;; We are not an exact match if we are looking for a
        ;; particular entity for this variable, unless it is a named
        ;; object, because for anything else we'll just return a
        ;; pattern, not the object.
        [value (or (not (variable-reference term))
                   (uniquely-identified-object? value))]
        (let [[contextual exact]
              (contextualize-variable (variable-qualifier term) env)]
          [contextual (combine-exact-matches exact #{var-name})])))
    [term true]))

(defn minimal-label?
  "Given a part of a query that is a label, return true if it is as
  small as it can be while still being a label. The query must be in
  list form."
  [entity]
  (and (let [elements (elements entity)]
         (or (empty? elements) (= elements '(:label))))
       (let [content (content entity)]
         (or (primitive? content) (stored-entity? content)))))

(defn labels-for-element
  "Given an element of a term, find primitives that can serve as labels
  for finding matching elements of other entities. Returns either '(),
  meaning that no labels were found; a sequence of labels, any of
  which will work; or a single label, which means a perfect fit: an
  element of an entity will match the query element if and only if it
  has the right orientation, and has that label."
  [element env]
  (let [[contextualized exact-match] (contextualize-variable element env)
        elems (map #(first (contextualize-variable % env))
                   (elements contextualized))
        [positive negative] (separate-negations elems)
        candidates (filter label-element? positive)]
    ;; Test for the special case of looking for nothing but an element
    ;; with a primitive value. That is the case where we can return a
    ;; single label which is a perfect fit.
    (if (and (nil? (content contextualized))
             (= exact-match true)
             (empty? negative)
             (not (empty? candidates))
             (empty? (rest positive))
             (minimal-label? (first candidates)))
      (content (first candidates))
      (keep #(let [label (content %)]
               (when (not (nil? label))
                 label))
              candidates))))

(defn candidate-elements
  "Given a entity whose elements we are searching over, and labels that
  all the elements we are looking for will have, return a set of
  candidate elements that is guaranteed to include all the possible
  matches and to all have the correct orientations."
  [labels required-orientation entity]
  (let [filter-orientation
        (if (nil? required-orientation)
          identity
          (fn [elements] (filter #(= (orientation %) required-orientation)
                                 elements)))]
    (if (empty? labels)
      (filter-orientation (elements entity))
      (let [candidateses (->> labels
                              (map #(label->elements entity %))
                              (map filter-orientation))]
        (loop [best nil
               candidateses candidateses]
          (if (empty? candidateses)
            best
            (let [candidates (first candidateses)]
              (if (empty? candidates)
                nil
              ;;; TODO: When there are several labels, and their
              ;;; lengths are not that different, intersect their
              ;;; candidates, like what store does.
                (recur (if (or (nil? best) (< (count candidates) (count best)))
                         candidates
                         best)
                       (rest candidateses))))))))))

(defn elements-satisfying [fixed-term entity]
  "Return a list of the entity's elements satisfying the given
  fixed-term, which must be an element."
  (assert (not (object? fixed-term)))
  (when (not (primitive? entity))
    (let [labels (labels-for-element fixed-term {})]
      (if (seq? labels)
        (filter #(extended-by? fixed-term %)
                (candidate-elements
                 labels (orientation fixed-term) entity))
        ;; The special case where being in the label index guarantees
        ;; satisfing the fixed-term.
        (label->elements entity labels)))))

(defn extended-by? [fixed-term entity]
  (or (nil? fixed-term)
      (cond
        ;; Nil matches anything
        (nil? fixed-term)
        true
        ;; Primitives match anything with matching content.
        (primitive? fixed-term)
        (equivalent-primitives? fixed-term (content entity))
        ;; A uniquely identified stored object only matches
        ;; itself. (But the list form of what would be a uniquely
        ;; identified object has to be able match a stored form,
        ;; because we might be searching for the stored form.)
        (and (stored-entity? fixed-term)
             (uniquely-identified-object? fixed-term))
        (= (entity-key fixed-term) (entity-key entity))
        ;; In the general case, the parts have to match.
        true
        (and (= (object? fixed-term) (object? entity))
             (or (object? fixed-term)
                 (and (extended-by? (content fixed-term) (content entity))
                      (= (orientation fixed-term) (orientation entity))))
             (or (empty? (elements fixed-term))
                 (let [[positive negative] (separate-negations
                                            (elements fixed-term))]
		   ;; Our disjoint-combinations doesn't work right
		   ;; if the entity has repeated elements.
		   (assert (distinct? (elements entity)))
                   (let [positive-satisfying
                         (seq (map #(elements-satisfying % entity) positive))
                         negative-satisfying
                         (map #(elements-satisfying % entity) negative)]
                     (and (or (empty? positive)
                              (not (empty? (disjoint-combinations
                                            positive-satisfying))))
                          (not-any? #(not (empty? %))
                                    negative-satisfying)))))))))

(defmethod extended-by-m? true [fixed-term entity]
  (extended-by? fixed-term entity))

(defn is-fixed-term-special-form?
  "Return true if the term is a special form that fixed terms can
  have."
  [term]
  (and (special-form? term)
       (= (special-form-type term) :not)))

(defn closest-template
  "Given a term, return a template that the store can use to find
  candidate ids, and that is as close to the term as possible:
     Remove variables, replacing them with their value in the environment,
     or their qualifier.
     Remove any other special forms (to eliminate any not-query terms).
  Also return whether matching the template is exactly equal to matching
  the term, using the format of combine-exact-matches."
  [term env]
  (let [[contextualized exact-match] (contextualize-variable term env)]
    (let [as-list (to-list contextualized)]
      (if (is-fixed-term-special-form? as-list)
        [nil false]
        (do (assert (not (special-form? as-list)))
            (if (or (primitive? as-list) (uniquely-identified-object? as-list))
              [as-list exact-match]
              (let [{dropped-elements true
                     kept-elements false}
                    (group-by is-fixed-term-special-form? (elements as-list))
                    [converted-kept-elements converted-kept-exact]
                    (unzip (map #(closest-template % env)
                                kept-elements))
                    exact-element-match (reduce combine-exact-matches
                                                (list*
                                                 exact-match
                                                 (not (some special-form?
                                                            dropped-elements))
                                                 converted-kept-exact))]
                (if (object? as-list)
                  [(make-object-list converted-kept-elements)
                   exact-element-match]
                  (let [[converted-content content-exact]
                        (closest-template (content as-list) env)]
                    [(make-element-list (orientation as-list)
                                        converted-content
                                        converted-kept-elements)
                     (combine-exact-matches content-exact
                                            exact-element-match)])))))))))

(def matching-extensions)

(defn variable-match-extensions
  "Return a seq of environments for which the variable matches the entity.
  Each environment will a binding for this variable, if it has a name,
  plus bindings for any other variables in the qualifier."
  [var variable-element-filter env entity entity-element-filter]
  (let [name (variable-name var)
        qualifier (variable-qualifier var)
        reference (variable-reference var)]
    (let [value (env name)]
      (if (nil? value)
        (when (and (not (nil? entity))
                   (or (not reference) (stored-entity? entity)))
          (let [envs (if (nil? qualifier)
                       [env]
                       (matching-extensions
                        qualifier variable-element-filter env
                        entity entity-element-filter))]
            (if (nil? name)
              envs
              (seq (map #(assoc % name entity) envs)))))
        (if reference
          (when (and (= value entity) (stored-entity? entity))
                [env])
          (when (= (canonicalize value)
                   (canonicalize entity))
            [env]))))))

(defn make-element-filter
  "Given an element, return a function that removes elements elements
  with the same item id from a list of elements." 
  [element]
  (let [key (entity-key element)]
    (fn [elements]
      (remove #(= key (entity-key %)) elements))))

(defn element-match-map
  "Return a map from environment to seq of elements of the entity,
  that pass the element filter and that match the term in the
  environment. The term must be an element."
  [term env entity entity-element-filter]
  (assert (not (object? term)))
  (let [labels (labels-for-element term env)]
    (if (or (nil? labels) (seq? labels) (nil? (content labels)))
      (let [candidates (entity-element-filter
                        (candidate-elements
                         labels (term-orientation term env) entity))
            match-envs (map #(matching-extensions term identity env % identity)
                            candidates)]
        (reduce (fn [result [candidate matching-envs]]
                  (reduce (fn [result env]
                            (update result env #(conj (or % []) candidate)))
                          result matching-envs))
                {} (map vector candidates match-envs)))
      ;; The special case of looking for any element with one specific label.
      (let [matching-elements (entity-element-filter
                               (label->elements entity labels))]
        (cond (empty? matching-elements)
              {}
              (variable-query? term)
              (let [name (variable-name term)]
                (reduce (fn [result element]
                          (let [new-env (assoc env name element)]
                            (assoc result new-env [element])))
                        {} matching-elements))
              true
              {env matching-elements})))))

(defn element-matches [term env entity entity-element-filter]
  (keys (element-match-map term env entity entity-element-filter)))

(defn concat-maps
  "Given a sequence of maps from key to sequence of values, return a single
  map that concatenates all the values for a given key."
  [maps]
  (when (seq maps)
    (reduce (fn [m1 m2]
              (reduce (fn [m k] (update m k #(concat (or % []) (m2 k))))
                      m1 (keys m2)))
            (first maps) (rest maps))))

(defn conj-disjoint-maps
  "Given a sequence of tuples of elements, and map from environments
  to elements, do conj-disjoint-combinations between the collection and
  each pair in the map."
  [combinations match-map]
  (reduce-kv
   (fn [m k v]
     (if-let [disjoint (seq (conj-disjoint-combinations combinations v))]
       (assoc m k disjoint)
       m))
   {} match-map))

(defn disjoint-element-match-extensions
  "Given a sequence of terms, a map from environments to sequences of
  disallowed elements, and a entity, return a sequence of environments
  where each term matches a different element in the entity that
  passes the filter, and is not a disallowed element from the map."
  [terms env-map entity entity-element-filter]
  (if (empty? terms)
    (keys env-map)
    (let [matching-maps (map #(element-match-map
                               (first terms) % entity entity-element-filter)
                             (keys env-map))
          disjoint-map (concat-maps
                        (map conj-disjoint-maps
                             (vals env-map) matching-maps))]
      (disjoint-element-match-extensions
       (rest terms) disjoint-map entity entity-element-filter))))

(defn no-element-matches
  "Return true if none of the queries are matched, given the environment,
   by any elements of the entity that pass the filter."
  [queries env entity entity-element-filter]
  (if (empty? queries)
    true
    (when (empty? (element-matches
                   (first queries) env entity entity-element-filter))
      (no-element-matches (rest queries) env entity entity-element-filter))))

(defn sub-elements-match-extensions
  "Return all extensions of the environments for which the the elements
  of the entity match the elements of the item, ignoring filtered out
  elements."
  [item item-element-filter envs entity entity-element-filter]
  (let [item-elements (item-element-filter (elements item))]
    (if (empty? item-elements)
      envs
      (let [[positive negative] (separate-negations item-elements)]
        (let [envs-matching-positive
              (cond
                (empty? positive)
                envs
                (empty? (rest positive))
                (-> (map #(element-matches
                           (first positive) % entity entity-element-filter)
                         envs)
                    distinct-concat)
                true
                (disjoint-element-match-extensions
                 positive
                 (zipmap envs (repeat [[]]))
                 entity
                 entity-element-filter))]
          (if (empty? negative)
            envs-matching-positive
            (filter #(no-element-matches
                      negative % entity entity-element-filter)
                    envs-matching-positive)))))))

(defn object-match-extensions
  "Return all extensions of the environment for which the entity matches
  the item, which must be an object. Filtered out elements are
  ignored."
  [item item-element-filter env entity entity-element-filter]
  (when (object? entity)
    (if (= (entity-key item) (entity-key entity))
      [env]
      ;; A stored named object, can only match itself. But can be
      ;; matched, in the other direction, by a pattern.
      (when (or (not (stored-entity? item))
                (not (uniquely-identified-object? item)))
        (sub-elements-match-extensions item item-element-filter [env]
                                       entity entity-element-filter)))))

(defn element-match-extensions
   "Return all extensions of the environment for which the entity matches
  the item, which must be an element. Filtered out elements are
  ignored."
  [item item-element-filter env entity entity-element-filter]
  (when (and (not (object? entity)) ;; We might match a primitive.
             (= (orientation item) (orientation entity)))
    (let [extensions
          ;; When matching content that is an object, our element
          ;; will also appear as an element of it, just in the
          ;; reverse orientation. If we checked that that element
          ;; matched we could go into an infinite loop, bouncing
          ;; back and forth between objects. And there is no point,
          ;; because we know the element is going to be there, by
          ;; virtue of our element being present. So we tell
          ;; subsequent checks to filter out our element before
          ;; checking.
          (matching-extensions
           (content item) (make-element-filter item) env
           (content entity) (make-element-filter entity))]
      (when (seq extensions)
        (sub-elements-match-extensions item item-element-filter extensions
                                       entity entity-element-filter)))))

(defn matching-extensions [term term-element-filter env
                           entity entity-element-filter]
  (assert (not (mutable-entity? term)))
  (cond (primitive? term) (when (extended-by? term entity) [env])
        (variable-query? term) (variable-match-extensions
                                term term-element-filter env
                                entity entity-element-filter)
        (object? term) (object-match-extensions term term-element-filter env
                                                entity entity-element-filter)
        true (element-match-extensions term term-element-filter env
                                       entity entity-element-filter)))

(defmethod matching-extensions-m true [term env entity]
  (matching-extensions term identity env entity identity))

(defmethod matching-elements-m true [term entity]
  (if (or (nil? term) (= term '()))
    (elements entity)
    (let [match-map (element-match-map term {} entity identity)]
      (distinct-concat (vals match-map)))))

(defn matching-items [term store]
  (filter
   #(not (empty? (matching-extensions term identity {} % identity)))
   ;; TODO: Make this use precise information.
   (let [[template precise] (closest-template term {})]
     (map #(id->entity % store)
          (first (candidate-matching-ids store template))))))

(defmethod matching-items-m true [term store]
  (matching-items term store))

(def query-matches)

(defn variable-matches-in-store [var env store]
  (let [name (variable-name var)
        reference (variable-reference var)
        value (env name)]
    (if (nil? value)
      (let [[template precise] (closest-template var env)
            ;; TODO: Make this use precise information.
            candidate-ids (first (candidate-matching-ids store template))
            matches (map #(variable-match-extensions
                           var identity env (id->entity % store) identity)
                         candidate-ids)]
        (distinct-concat matches))
      (when (seq (query-matches value env store)) [env]))))

(defn exists-matches-in-store [exists env store]
  (let [var (quantifier-variable exists)
        name (variable-name var)
        qualifier (variable-qualifier var)
        body (sub-query exists)]
    (let [matches (if qualifier
                    (mapcat #(query-matches body % store)
                            (query-matches qualifier env store))
                    (query-matches body env store))]
      (seq (distinct (map #(dissoc % name) matches))))))

(defn forall-matches-in-store [forall env store]
  (let [var (quantifier-variable forall)
        name (variable-name var)
        qualifier (variable-qualifier var)
        body (sub-query forall)]
    ;; We need to return each environment where all entities
    ;; satisfying the qualifier also satisfy the body. And our binding
    ;; doesn't appear in those environment. So we first find all
    ;; matches of the qualifier, which may include bindings for our
    ;; variable. Then we group them by their bindings for all other
    ;; free variables. Each group is a starting point for a possible
    ;; binding we may return. For each group, we try to match our body
    ;; for each matching entity under that group's binding. And we
    ;; return the interection of the bindings under that group.
    (let [matches (query-matches qualifier env store)
          groups (group-by #(dissoc % name) matches)]
      ;; Get [for each group of bindings matching the qualifier
      ;;       [for each binding in the group (which will bind the var)
      ;;         [each extension of the binding satisfying the body]]]
      (let [binding-groups (map (fn [group]
                                  (map (fn [binding]
                                         (query-matches body binding store))
                                       (groups group)))
                                (keys groups))]
        (seq (mapcat
              (fn [binding-group]
                (apply clojure.set/intersection
                       (map (fn [extensions]
                              (set (map #(dissoc % name) extensions)))
                            binding-group)))
              binding-groups))))))

(defn and-matches-in-store [and env store]
  (let [[first-q second-q] (sub-queries and)]
    (let [first-matches (query-matches first-q env store)
          both-matches (map #(query-matches second-q % store)
                            first-matches)]
      (distinct-concat both-matches))))

(defn item-matches-in-store [item env store]
  (let [[template template-exact] (closest-template item env)
        [candidate-ids precise] (candidate-matching-ids store template)
        candidates (map #(id->entity % store) candidate-ids)]
    (if (and template-exact precise)
      (when (seq candidates)
        [env])
      (let [matches (seq (map #(matching-extensions
                                item identity env % identity)
                              candidates))]
        (distinct-concat matches)))))

(defn query-matches
  ([query store] (query-matches query {} store))
  ([query env store]
   (assert (not (mutable-entity? query)))
   (if (special-form? query)
     (case (special-form-type query)
       :variable (variable-matches-in-store query env store)
       :exists (exists-matches-in-store query env store)
       :forall (forall-matches-in-store query env store)
       :and (and-matches-in-store query env store))
     (item-matches-in-store query env store))))

(defmethod query-matches-m true [query env store]
  (query-matches query env store))

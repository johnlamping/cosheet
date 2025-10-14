(ns cosheet2.query-impl
  (:require (cosheet2 [store :as store :refer [candidate-matching-ids]]
                      [entity :refer [Entity StoredEntity
                                      mutable-entity? primitive?
                                      id->entity
                                      orientation content elements
                                      make-element-list 
                                      label->elements
                                      label->content
                                      to-list
                                      in-different-store]]
                      [entity :refer [label? minimal-label?]]
                      [query :as query
                       :refer [extended-by-m?
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
                               sub-query]]
                      [canonical :refer [equivalent-primitives?
                                         canonicalize]]
                      [utils :refer [prewalk-seqs
                                     conj-disjoint-combinations
                                     disjoint-combinations]])))

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
   and one of negated terms. Discard any terms whose content
   is ::query/sub-query, as those are tags that identify sub-querys,
   not a condition to be matched."
  [terms]
  (let [grouped (group-by
                 (fn [term]
                   (cond (and (special-form? term)
                              (= (special-form-type term) :not))
                         :negation
                         (= (content term) ::query/sub-query)
                         :ignore
                         true
                         :positive))
                 terms)]
    [(:positive grouped) (map sub-query (:negation grouped))]))

(declare extended-by?)

(defn combine-exact-matches
  "Given two exact match indicators from different parts of a term,
  returning their joint indicator.
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
        ;; We are not an exact match if the query is looking for a
        ;; particular entity, because the callers of this function
        ;; aren't aware of entity identities.
        [value (not (variable-reference term))]
        (let [[contextual exact]
              (contextualize-variable (variable-qualifier term) env)]
          [contextual (combine-exact-matches exact #{var-name})])))
    [term true]))

(defn labels-for-element
  "Given an entity that is an element of a term, find primitives that
  can serve as labels for finding matching elements of a
  subject. Returns either '(), meaning that no labels were found; a
  sequence of labels, any of which will work; or a single label, which
  means a perfect fit: an element of a subject will match the query
  element if and only if it has that label."
  [element env]
  (let [[contextualized exact-match] (contextualize-variable element env)
        elems (map #(first (contextualize-variable % env))
                   (elements contextualized))
        [positive negative] (separate-negations elems)
        candidates (filter label? positive)]
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
      (mapcat #(let [label (content %)]
                 (when (and (not (nil? label))
                            (not= label ::query/special-form))
                   [label]))
              candidates))))

(defn candidate-elements
  "Given a subject, and labels that all the elements we are looking for
   will have, return a set of candidate elements that is guaranteed
   to include all the possible matches."
  [labels subject]
  (if (empty? labels)
    (elements subject)
    (let [candidateses (map #(label->elements subject %) labels)]
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
                     (rest candidateses)))))))))

(defn elements-satisfying [fixed-term subject]
  "Return a list of the subject elements satisfying the given fixed-term."
  (when (not (primitive? subject))
    (let [labels (labels-for-element fixed-term {})]
      (if (seq? labels)
        (filter #(extended-by? fixed-term %)
                (candidate-elements labels subject))
        ;; The special case where being in the label index guarantees
        ;; satisfing the fixed-term.
        (label->elements subject labels)))))

(defn extended-by? [fixed-term subject]
  (or (nil? fixed-term)
      (if (primitive? fixed-term)
        (equivalent-primitives? (content fixed-term)
                                (content subject))
        (and (extended-by? (content fixed-term) (content subject))
             (or (empty? (elements fixed-term))
                 (let [[positive negative] (separate-negations
                                            (elements fixed-term))]
                   (let [positive-satisfying
                         (seq (map #(elements-satisfying % subject) positive))
                         negative-satisfying
                         (map #(elements-satisfying % subject) negative)]
                     (and (or (empty? positive)
                              (not (empty? (disjoint-combinations
                                            positive-satisfying))))
                          (not-any? #(not (empty? %))
                                    negative-satisfying)))))))))

(defmethod extended-by-m? true [fixed-term subject]
  (extended-by? fixed-term subject))

(defn is-fixed-term-special-form?
  "Return true if the term is a special form that fixed terms can
  have."
  [term]
  (or (= (content term) ::query/sub-query)
      (and (special-form? term)
           (= (special-form-type term) :not))))

(defn closest-template
  "Given a term, return a template that the store can use to find
  candidate ids, and that is as close to the term as possible:
     Remove variables, replacing them with their value in the environment,
     or their qualifier.
     Remove any ::query/sub-query annotations.
     Remove any other special forms (to eliminate any not-query terms).
  Also return whether matching the template is exactly equal to matching
  the term, using the format of combine-exact-matches."
  [term env]
  ;; TODO: Get rid of this check that we are not getting a mutable query.
  (assert (not (mutable-entity? term)))
  (let [[contextualized exact-match] (contextualize-variable term env)]
    ;; TODO: Get rid of this check that we are not getting a mutable
    ;; query once we contextualize. Then get rid of the following
    ;; as-list too.
    (assert (not (mutable-entity? contextualized)))
    ;; While the term is guaranteed to be immutable, its contextualized
    ;; value might be an item in a mutable store.
    ;; TODO: Not any more.
    (let [as-list (to-list contextualized)]
      (if (is-fixed-term-special-form? as-list)
        [nil false]
        (do (assert (not (special-form? as-list)))
            (if (seq? as-list)
              (let [{dropped-elements true
                     kept-elements false}
                    (group-by is-fixed-term-special-form? (elements as-list))
                    converted-content (closest-template (content as-list) env)
                    converted-kept-elements (map #(closest-template % env)
                                                 kept-elements)
                    exact-match (reduce combine-exact-matches
                                          (concat
                                           [exact-match
                                            (not (some special-form?
                                                       dropped-elements))
                                            (second converted-content)]
                                           (map second
                                                converted-kept-elements)))]
                [(make-element-list (orientation as-list)
                                    (first converted-content)
                                    (map first converted-kept-elements))
                 exact-match])
              [as-list exact-match]))))))

(def matching-extensions)

(defn variable-matches
  "Return a seq of environments for which the variable matches the subject.
  Each environment will a binding for this variable, if it has a name,
  plus bindings for any other variables in the qualifier."
  [var env subject]
  (let [name (variable-name var)
        qualifier (variable-qualifier var)
        reference (variable-reference var)]
    (let [value (env name)]
      (if (nil? value)
        (when (and (not (nil? subject))
                   (or (not reference) (satisfies? StoredEntity subject)))
          (let [envs (if (nil? qualifier)
                       [env]
                       (matching-extensions qualifier env subject))]
            (if (nil? name)
              envs
              (seq (map #(assoc % name subject) envs)))))
        (if reference
          (when (and (= value subject) (satisfies? StoredEntity subject))
                [env])
          (when (= (canonicalize value)
                   (canonicalize subject))
            [env]))))))

(defn element-match-map
  "Return a map from environment to seq of elements of the subject that match
  the term in the environment."
  [term env subject]
  (let [labels (labels-for-element term env)]
    (if (or (nil? labels) (seq? labels) (nil? (content labels)))
      (let [candidates (candidate-elements labels subject)
            match-envs (map #(matching-extensions term env %) candidates)]
        (reduce (fn [result [candidate matching-envs]]
                  (reduce (fn [result env]
                            (update result env #(conj (or % []) candidate)))
                          result matching-envs))
                {} (map vector candidates match-envs)))
      ;; The special case of looking for any element with one specific label.
      (let [matching-elements (label->elements subject labels)]
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

(defn element-matches [term env subject]
  (keys (element-match-map term env subject)))

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

(defn multiple-element-matches
  "Given a sequence of terms, a map from environments to sequences
  of disallowed elements, and a subject, return a sequence of environments
  where each term matches a different element in the subject,
  and not a disallowed element."
  [terms env-map subject]
  (if (empty? terms)
    (keys env-map)
    (let [matching-maps (map #(element-match-map (first terms) % subject)
                             (keys env-map))
          disjoint-map (concat-maps
                        (map conj-disjoint-maps
                             (vals env-map) matching-maps))]
      (multiple-element-matches
       (rest terms) disjoint-map subject))))

(defn no-element-matches
  "Return true if none of the queries are matched
   by any elements of the subject, given the environment."
  [queries env subject]
  (if (empty? queries)
    true
    (when (empty? (element-matches (first queries) env subject))
      (no-element-matches (rest queries) env subject))))

(defn item-matches [item env subject]
  (let [content-match-envs
        (if-let [item-content (content item)]
          (matching-extensions item-content env (content subject))
          [env])]
    (when (seq content-match-envs)
      (let [item-elements (elements item)]
        (if (empty? item-elements)
          content-match-envs
          (let [[positive negative] (separate-negations item-elements)]
            (let [envs
                  (cond
                    (empty? positive)
                    content-match-envs
                    (empty? (rest positive))
                    (-> (map #(element-matches (first positive) % subject)
                             content-match-envs)
                        distinct-concat)
                    true
                    (multiple-element-matches
                     positive
                     (zipmap content-match-envs (repeat [[]]))
                     subject))]
              (if (empty? negative)
                envs
                (filter #(no-element-matches negative % subject) envs)))))))))

(defn matching-extensions [term env subject]
  (assert (not (mutable-entity? term)))
  (if (primitive? term)
    (when (extended-by? term subject) [env])
    (if (variable-query? term)
      (variable-matches term env subject)
      (item-matches term env subject))))

(defmethod matching-extensions-m true [term env subject]
  (matching-extensions term env subject))

(defn matching-elements
  [term subject]
  (if (or (nil? term) (= term '()))
    (elements subject)
    (let [match-map (element-match-map term {} subject)]
      (distinct-concat (vals match-map)))))

(defmethod matching-elements-m true [term subject]
  (matching-elements term subject))

(defn matching-items [term store]
  (filter
   #(not (empty? (matching-extensions term {} %)))
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
            matches (map #(variable-matches
                           var env (id->entity % store))
                     candidate-ids)]
        (distinct-concat matches))
      (when (seq (query-matches value env store)) [env]))))

(defn exists-matches-in-store [exists env store]
  (let [var (first (label->elements exists ::query/variable))
        name (variable-name var)
        qualifier (variable-qualifier var)
        body (sub-query exists)]
    (let [matches (if qualifier
                    (mapcat #(query-matches body % store)
                            (query-matches qualifier env store))
                    (query-matches body env store))]
      (seq (distinct (map #(dissoc % name) matches))))))

(defn forall-matches-in-store [forall env store]
  (let [var (first (label->elements forall ::query/variable))
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
  (let [queries (label->elements and ::query/sub-query)
        [first-q second-q] (if (label->content (second queries) :first)
                             (reverse queries)
                             queries)]
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
      (let [matches (seq (map (partial matching-extensions item env)
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

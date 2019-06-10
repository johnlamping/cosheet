(ns cosheet.query-impl
  (:require (cosheet [store :as store]
                     [entity :refer [Entity StoredEntity
                                     mutable-entity? atom?
                                     content-reference
                                     description->entity
                                     content elements label->elements
                                     label->content atomic-value
                                     to-list deep-to-list current-version
                                     in-different-store
                                     updating-with-immutable]]
                     [query :as query
                            :refer [extended-by-m?
                                    matching-extensions-m
                                    best-matching-term-m
                                    matching-elements-m
                                    matching-items-m
                                    query-matches-m
                                    variable-query?
                                    variable-qualifier]]
                     [expression :refer [expr expr-let expr-seq expr-filter]]
                     [utils :refer [equivalent-atoms? prewalk-seqs]]
                     [debug :refer [trace-current
                                    simplify-for-print]])))

;;; TODO:
;;; Add a term syntax that lets variables bind to the subject.
;;; Add a unification operation on terms, so rule matching can work.
;;;    The environment must include variable numbers, for renaming,
;;;    and an indication of the number of the current term, which must
;;;    be the highest number.

(defn conj-disjoint-combinations
  "Given a sequence of collections of elements, and a sequence of elements,
   choose all combinations of a collection and an element not in the collection,
  returning a sequence of such combinations."
  [combinations elements]
  (mapcat (fn [combination]
            (keep (fn [element] (when (not (some (partial = element)
                                                 combination))
                                  (conj combination element)))
                  elements))
          combinations))

(defn disjoint-combinations
  "Given a sequence of sequences of elements,
   return all disjoint combinations of one element from each sequence."
  [sequences]
  (if (empty? sequences)
    [[]]
    (reduce conj-disjoint-combinations
            (map vector (first sequences))
            (rest sequences))))

(defn distinct-concat
  "Given a sequence of sequences, each sequence having internally distinct
   elements, return the concatenation of the sequences, with duplicates across
   sequences removed."
  [sequences]
  (if (empty? (rest sequences))
    (first sequences)
    (seq (distinct (apply concat sequences)))))

(defn separate-negations
  "Given a seq of terms, return two seqs, one of positive terms,
   and one of negated terms. Discard any terms with content ::template."
  [terms]
  (let [grouped (group-by (fn [term]
                            (cond (and (= (content term) ::query/special-form)
                                       (= (label->content term ::query/type)
                                          :not))
                                  :negation
                                  (= (content term) ::query/sub-query)
                                  :ignore
                                  true
                                  :positive))
                          terms)]
    [(:positive grouped) (map #(first (label->elements % ::query/sub-query))
                             (:negation grouped))]))

(def extended-by?)

(defn contextualize-variable
  "If the term is a variable, replace it by it's value in the environment,
  or, if there is no value, then its qualifier."
  [term env]
  (if (variable-query? term)
    (or (env (label->content term ::query/name))
        (contextualize-variable (variable-qualifier term) env))
    term))

(defn labels-for-element
  "Given an entity that is an element of a term, find atoms that can serve
   as labels for finding matching elements of a target. Returns either '(),
   meaning that no labels were found; a sequence of labels, any of which
   will work; or a single label, which means a perfect fit: an element
   of a target will match the query iff and only if it has that label."
  [element env]
  (let [contextualized (contextualize-variable element env)
        elems (map #(contextualize-variable % env)
                   (elements contextualized))
        [positive negative] (separate-negations elems)]
    ;; Test for the special case of looking for any element with a given atomic
    ;; tag. That is the case where we can return a single label which is a
    ;; perfect fit.
    (if (and (nil? (content contextualized))
             (empty? negative)
             (not (empty? positive))
             (empty? (rest positive))
             (empty? (elements (first positive))))
      (content (first positive))
      (mapcat #(let [label (atomic-value %)]
                 (when (and (not (nil? label))
                            (not= label ::query/special-form))
                   [label]))
              positive))))

(defn candidate-elements
  "Given a target, and labels that all the elements we are looking for
   will have, return a set of candidate elements that is guaranteed
   to include all the possible matches."
  [labels target]
  (if (empty? labels)
    (elements target)
    (expr-let [candidateses (expr-seq
                             map #(label->elements target %) labels)]
      (loop [best nil
             candidateses candidateses]
        (if (empty? candidateses)
          best
          (let [candidates (first candidateses)]
            (if (empty? candidates)
              nil
              (recur (if (or (nil? best) (< (count candidates) (count best)))
                       candidates
                       best)
                     (rest candidateses)))))))))

(defn elements-satisfying [fixed-term target]
  "Return a list of the target elements satisfying the given fixed-term."
  (when (not (atom? target))
    (let [labels (labels-for-element fixed-term {})]
      (if (seq? labels)
        (expr-let
            [candidates (candidate-elements labels target)]
          (expr-filter #(extended-by? fixed-term %) candidates))
        ;; The special case where being in the label index guarantees
        ;; satisfing the fixed-term.
        (label->elements target labels)))))

(defn has-element-satisfying? [fixed-term target]
  "Return true if the target item has an element satisfying
  the given fixed-term)."
  (expr-let [satisfying (elements-satisfying fixed-term target)]
    (not (empty? satisfying))))

(defn extended-by? [fixed-term target]
  (or (nil? fixed-term)
      (if (atom? fixed-term)
        (expr-let [target-value (atomic-value target)]
          (equivalent-atoms? (atomic-value fixed-term) target-value))
        (expr-let
            [content-extended (expr extended-by?
                                (content fixed-term)
                                (content target))]
          (when content-extended
            (or (empty? (elements fixed-term))
                (let [[positive negative] (separate-negations
                                           (elements fixed-term))]
                  (expr-let [positive-satisfying
                             (expr-seq map #(elements-satisfying % target)
                                       positive)
                             negative-satisfying
                             (expr-seq map #(elements-satisfying % target)
                                       negative)]
                    (and (or (empty? positive)
                             (not (empty? (disjoint-combinations
                                           positive-satisfying))))
                         (not-any? #(not (empty? %))
                                   negative-satisfying))))))))))

(defmethod extended-by-m? true [fixed-term target]
  (extended-by? fixed-term target))

(defn closest-template-R
  "Given a term, return a template that the store can use to find candidate
   ids, and that is as close to the term as possible:
      Remove variables, replacing them with their value in the environment,
      or their qualifier.
      Replace bound variables with their current values.
      Remove any ::query/sub-query annotations.
      Remove any other special forms (to eliminate any not-query terms)."
  [term env]
  (let [current (if (satisfies? StoredEntity term)
                  (current-version term)
                  term)
        contextualized (contextualize-variable current env)]
    ;; While the term is guaranteed to be immutable, its contextualized
    ;; value might be an item in a mutable store.
    (expr-let [as-list (to-list contextualized)]
      (if (#{::query/sub-query ::query/special-form} (content as-list))
        (do (assert (not (and (= ::query/special-form (content as-list))
                              (not= :not
                                    (label->content as-list ::query/type)))))
            nil)
        (if (seq? as-list)
          (expr-let [converted (expr-seq map #(closest-template-R % env)
                                         as-list) 
                     kept-elements (remove nil? (rest converted))]
            (if (empty? kept-elements)
              (first converted)
              (cons (first converted) kept-elements)))
          as-list)))))

;;; Code to bind an immutable entity in an environment

(def bind-entity)

(defrecord
    ^{:doc "An entity that is another entity with its variables bound
            by an environment.
            It will never be an atom, a mutable entity,
            or a bare bound variable."}
    BoundEntity

  [wrapped  ; The entity we wrap
   env]     ; The environment it is wrapped in
  
  Entity

  (mutable-entity? [this] false)
  
  ;; Note: this assumes that an implicit content reference doesn't
  ;; outlive a promotion of its the content.  
  (atom? [this] (atom? wrapped))

  (label->elements [this label]
    (seq (filter #(some (partial equivalent-atoms? label)
                          (map atomic-value (elements %)))
                   (map #(bind-entity % env)
                        (elements wrapped)))))

  (elements [this]
    (let [unbound-elements (elements wrapped)]
      (seq (map #(bind-entity % env) unbound-elements))))

  (content [this]
    (bind-entity (content wrapped) env))

  (updating-call-with-immutable [this fun]
    (fun this))

  (current-version [this]
    this))

(defn bind-entity [entity env]
  (assert (not (mutable-entity? entity)))
  (if (atom? entity)
    (atomic-value entity)
    (or (and (variable-query? entity)
             (env (label->content entity ::query/name)))
        (->BoundEntity entity env))))

(def matching-extensions)

(defn current-entity-value
  [entity]
  (if (mutable-entity? entity)
    (deep-to-list entity)
    entity))

(defn variable-matches [var env target]
  (let [name (label->content var ::query/name)
        qualifier (variable-qualifier var)
        value-may-extend (label->content var ::query/value-may-extend)
        reference (label->content var ::query/reference)]
    (let [value (env name)]
      (if (not (nil? value))
        (if reference
          (when (= value target)
                [env])
          (expr-let
              [value-as-immutable (current-entity-value value)
               target-extends (extended-by? value-as-immutable target)]
            (when target-extends
              (if value-may-extend
                [env]
                (expr-let [target-as-immutable (current-entity-value target)]
                  (when (extended-by? target-as-immutable value-as-immutable)
                    [env]))))))
        (expr-let [target-as-immutable (current-entity-value target)]
          (when (not (nil? target-as-immutable))
            (expr-let
                [envs (if (nil? qualifier)
                        [env]
                        (matching-extensions qualifier env target))]
              (if (not (nil? name))
                (expr-let [value (if reference target target-as-immutable)]
                  (seq (map #(assoc % name value) envs)))
                envs))))))))

(defn element-match-map
  "Return a map from environment to seq of elements of the target that match
   the term in the environment."
  [term env target]
  (let [labels (labels-for-element term env)]
    (if (or (nil? labels) (seq? labels))
      (expr-let [candidates (candidate-elements labels target)
                 match-envs (expr-seq map #(matching-extensions term env %)
                                      candidates)]
        (reduce (fn [result [candidate matching-envs]]
                  (reduce (fn [result env]
                            (update result env #(conj (or % []) candidate)))
                          result matching-envs))
                {} (map vector candidates match-envs)))
      ;; The special case of looking for any element with a specific label.
      (expr-let [matching-elements (label->elements target labels)]
        (cond (empty? matching-elements)
              {}
              (variable-query? term)
              (let [name (label->content term ::query/name)]
                (reduce (fn [result element]
                          (let [new-env (assoc env name element)]
                            (assoc result new-env [element])))
                        {} matching-elements))
              true
              {env matching-elements})))))

(defn element-matches [term env target]
  (expr-let [match-map (element-match-map term env target)]
    (keys match-map)))

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
  "Given a collection of disallowed elements, and map from environments
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
  "Given a sequence of terms, a map from environments to collections
  of disallowed elements, and a target, return a sequence of environments
  where each term matches a different element in the target,
  and not a disallowed element."
  [terms env-map target]
  (if (empty? terms)
    (keys env-map)
    (expr-let [matching-maps
               (expr-seq map #(element-match-map (first terms) % target)
                         (keys env-map))]
      (let [disjoint-map (concat-maps
                          (map conj-disjoint-maps
                               (vals env-map) matching-maps))]
        (multiple-element-matches
         (rest terms) disjoint-map target)))))

(defn no-element-matches
  "Return true if none of the queries are matched
   by any elements of the target, given the environment."
  [queries env target]
  (if (empty? queries)
    true
    (expr-let [matches (element-matches (first queries) env target)]
      (when (empty? matches)
        (no-element-matches (rest queries) env target)))))

(defn item-matches [item env target]
  (expr-let [content-match-envs
             (if-let [item-content (content item)]
               (expr matching-extensions
                 item-content env (content-reference target))
               [env])]
    (when (seq content-match-envs)
      (let [item-elements (elements item)]
        (if (empty? item-elements)
          content-match-envs
          (let [[positive negative] (separate-negations item-elements)]
            (expr-let
                [envs
                 (cond
                   (empty? positive)
                   content-match-envs
                   (empty? (rest positive))
                   (expr-let [env-matches
                              (expr-seq map #(element-matches
                                              (first positive) % target)
                                        content-match-envs)]
                     (distinct-concat env-matches))
                   true
                   (multiple-element-matches
                    positive (zipmap content-match-envs (repeat [[]])) target))]
              (if (empty? negative)
                envs
                (expr-filter #(no-element-matches negative % target)
                             envs)))))))))

(defn matching-extensions [query env target]
  (assert (not (mutable-entity? query)))
  (let [answer
        (if (atom? query)
          (expr-let [extended (extended-by? query target)]
            (when extended [env]))
          (if (variable-query? query)
            (variable-matches query env target)
            (item-matches query env target)))]
    answer))

(defmethod matching-extensions-m true [query env target]
  (matching-extensions query env target))

(defmethod best-matching-term-m true [terms env target]
  (expr-let [matches (expr-seq map #(matching-extensions % env target) terms)]
    (when-let [candidates (->> (map (fn [match query]
                                      (when (seq match) query))
                                   matches terms)
                               (remove nil?)
                               (seq))]
      (reduce (fn [best query]
                (if (seq (matching-extensions best env query))
                  query
                  best))
              (first candidates) (rest candidates)))))

(defn matching-elements
  [term target]
  (if (or (nil? term) (= term '()))
    (elements target)
    (if (and (mutable-entity? target)
             (satisfies? cosheet.entity/StoredEntity target))
      ;; Optimize to not build reporters for all the subsidiary tests. 
      (expr-let [matches
                 (updating-with-immutable
                  [immutable target]
                  (when (not (nil? immutable))
                    (expr-let [match-map (element-match-map term {} immutable)]
                      (distinct-concat (vals match-map)))))]
        (map #(in-different-store % target) matches))
      (expr-let [match-map (element-match-map term {} target)]
        (distinct-concat (vals match-map))))))

(defmethod matching-elements-m true [term target]
  (matching-elements term target))

(defn matching-items [term store]
  (if (satisfies? store/MutableStore store)
    ;; Rather than build reporters for all subsidiary tests, fix the store
    ;; and recompute the whole query whenever the store changes.
    (expr-let [template (closest-template-R term {})
               ids (store/call-dependent-on-id
                    store nil
                    (fn [immutable-store]
                      (filter
                       #(let [entity (description->entity % immutable-store)]
                          (not (empty? (matching-extensions term {} entity))))
                       (store/candidate-matching-ids
                        immutable-store template))))]
      (map #(description->entity % store) ids))
    ;; The store is immutable.
    (filter
     #(not (empty? (matching-extensions term {} %)))     
     (map #(description->entity % store)
          (store/candidate-matching-ids
           store (closest-template-R term {}))))))

(defmethod matching-items-m true [term store]
  (matching-items term store))

(def query-matches)

(defn variable-matches-in-store [var env store]
  (let [name (label->content var ::query/name)
        reference (label->content var ::query/reference)
        value (env name)]
    (if (nil? value)
      (expr-let [template (closest-template-R var env)
                 candidate-ids (store/candidate-matching-ids store template)
                 matches (expr-seq
                          map #(variable-matches
                                var env (description->entity % store))
                          candidate-ids)]
        (distinct-concat matches))
      (if reference
        [env]
        (expr-let [value-as-immutable (current-entity-value value)
                   matches (query-matches value-as-immutable env store)]
          (when matches [env]))))))

(defn exists-matches-in-store [exists env store]
  (let [var (first (label->elements exists ::query/variable))
        name (label->content var ::query/name)
        qualifier (first (label->elements var ::query/sub-query))
        body (first (label->elements exists ::query/sub-query))]
    (expr-let [matches (if qualifier
                         (expr apply concat
                               (expr-seq map #(query-matches body % store)
                                         (query-matches qualifier env store)))
                         (query-matches body env store))]
      (seq (distinct (map #(dissoc % name) matches))))))

(defn forall-matches-in-store [forall env store]
  (let [var (first (label->elements forall ::query/variable))
        name (label->content var ::query/name)
        qualifier (first (label->elements var ::query/sub-query))
        body (first (label->elements forall ::query/sub-query))]
    (expr-let [matches (query-matches qualifier env store)]
      (let [groups (group-by #(dissoc % name) matches)]
        ;; Get [for each group of bindings matching the qualifier
        ;;       [for each binding in the group (which will bind the vars)
        ;;         [each extension of the binding satisfying the body]]]
        (expr-let [binding-groups
                   (expr-seq map (fn [group]
                                   (expr-seq
                                    map (fn [binding]
                                          (query-matches body binding store))
                                    (groups group)))
                             (keys groups))]
          (seq (mapcat
                (fn [binding-group]
                  (apply clojure.set/intersection
                         (map (fn [extensions]
                                (set (map #(dissoc % name) extensions)))
                              binding-group)))
                binding-groups)))))))

(defn and-matches-in-store [and env store]
  (let [queries (label->elements and ::query/sub-query)
        [first-q second-q] (if (label->content (first queries) :first)
                             queries
                             (reverse queries))]
    (expr-let [first-matches (query-matches first-q env store)
               both-matches (expr-seq map #(query-matches second-q % store)
                                      first-matches)]
      (distinct-concat both-matches))))

(defn item-matches-in-store [item env store]
  (expr-let [template (closest-template-R item env)
             candidates (expr map
                          #(description->entity % store)
                          (store/candidate-matching-ids store template))
             matches (expr-seq map (partial matching-extensions item env)
                               candidates)]
    (distinct-concat matches)))

(defn query-matches
  ([query store] (query-matches query {} store))
  ([query env store]
   (assert (not (mutable-entity? query)))
   (let [query-content (content query)]
     (if (= ::query/special-form query-content)
       (case (label->content query ::query/type)
         :variable (variable-matches-in-store query env store)
         :exists (exists-matches-in-store query env store)
         :forall (forall-matches-in-store query env store)
         :and (and-matches-in-store query env store))
       (item-matches-in-store query env store)))))

(defmethod query-matches-m true [query env store]
  (query-matches query env store))

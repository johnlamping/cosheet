(ns cosheet.query-impl
  (:require [clojure.pprint :refer [pprint]]
            (cosheet [store :as store]
                     [entity :refer [Entity StoredEntity
                                     mutable-entity? atom?
                                     content-reference
                                     description->entity
                                     content elements label->elements
                                     label->content atomic-value
                                     to-list deep-to-list current-version]]
                     [query :refer [extended-by-m?
                                    template-matches-m
                                    best-template-match-m
                                    query-matches-m]]
                     [expression :refer [expr expr-let expr-seq]]
                     [utils :refer [equivalent-atoms? prewalk-seqs]]
                     [debug :refer [trace-current
                                    simplify-for-print]])))

;;; TODO:
;;; Change the special form syntax so all special forms have content
;;;    :special-form.
;;; Add a term syntax that lets variables bind to the subject.
;;; Add a unification operation on terms, so rule matching can work.
;;;    The environment must include variable numbers, for renaming,
;;;    and an indication of the number of the current term, which must
;;;    be the highest number.

(def ^:dynamic verbose false)

;;; There are three levels of matching:
;;;        extended-by?:  Takes two entities.
;;;                       Says whether one is an extension of the other.
;;;    template-matches:  Takes an entity, a template, which may have
;;;                       variables, and an environment.
;;;                       Returns a set of extensions of the environment
;;;                       that cause the entity to be an extension
;;;                       of the template.
;;;       query-matches:  Takes a database, a template, which may have
;;;                       variables, and an environment.
;;;                       Returns a set of extensions of the environment
;;;                       that cause some entity in the database to be
;;;                       an extension of the template.

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

(def extended-by?)

(defn label-for-element
  "Given an entity that is an element, find an atom that can serve as
  a label for that element."
  [element]
  (let [annotations (elements element)
        labels (map atomic-value annotations)]
    (first (filter (partial not= nil) labels))))

(defn elements-satisfying [element target]
  "Return a list of the target elements satisfying the given element."
  (when (not (atom? target))
    (if (and (seq? element)
             (nil? (first element))
             (atom? (second element))
             (= (count element) 2))
      (label->elements target (atomic-value (second element)))
      (let [label (label-for-element element)]
        (expr-let
            [candidates (if (not (nil? label))
                          (label->elements target label)
                          (elements target))
             extended-by (expr-seq map (partial extended-by? element)
                                   candidates)]
          (keep-indexed (fn [index candidate]
                          (when (nth extended-by index) candidate))
                        candidates))))))

(defn has-element-satisfying? [element target]
  "Return true if the target item has an element satisfying
  the given element)."
  (expr-let [satisfying (elements-satisfying element target)]
    (not (empty? satisfying))))

(defn extended-by? [template target]
  (or (nil? template)
      (if (atom? template)
        (expr-let [target-value (atomic-value target)]
          (equivalent-atoms? (atomic-value template) target-value))
        (expr-let
            [content-extended (expr extended-by?
                                (content template)
                                (content target))]
          (when content-extended
            (expr-let [template-elements (elements template)]
              (or (empty? template-elements)
                  (expr-let
                      [satisfied-elements (expr-seq
                                           map #(elements-satisfying % target)
                                           template-elements)]
                    (not (empty? (disjoint-combinations
                                  satisfied-elements)))))))))))

(defmethod extended-by-m? true [template target]
  (extended-by? template target))

(defn variable? [template]
  (= (content template) :variable))

(defn variable-condition [variable]
  (let [conditions (label->elements variable :condition)]
    (when conditions (content (first conditions)))))

(defn turn-into-template
  "Remove the variables in a item, replacing them with their condition.
   and replace item referents with their ids"
  [template]
  (prewalk-seqs (fn [template]
                  (cond (variable? template)
                        (turn-into-template (variable-condition template))
                        (satisfies? StoredEntity template)
                        (atomic-value (current-version template))
                        true
                        template))
                template))

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
    (or (and (variable? entity)
             (env (label->content entity :name)))
        (->BoundEntity entity env))))

(def template-matches)

(defn current-entity-value
  [entity]
  (if (mutable-entity? entity)
    (deep-to-list entity)
    entity))

(defn variable-matches [var env target]
  (expr-let [name (label->content var :name)
             condition (label->content var :condition)
             value-may-extend (label->content var :value-may-extend)
             reference (label->content var :reference)]
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
                [envs (if (nil? condition)
                        [env]
                        (template-matches condition env target))]
              (if (not (nil? name))
                (expr-let [value (if reference target target-as-immutable)]
                  (seq (map #(assoc % name value) envs)))
                envs))))))))

(defn element-match-map
  "Return a map from environment to seq of elements that match in that
  environment."
  [element env target]
  (when verbose (println "element-match-map" element (deep-to-list target)))
  ;; Test for the special case of looking for any element with a specific label.
  (if (and (seq? element)
           (nil? (first element))
           (atom? (second element))
           (= (count element) 2))
    (expr-let [matching-elements (label->elements
                                  target (atomic-value (second element)))]
      (if (empty? matching-elements) {} {env matching-elements}))
    (let [label (if (variable? element)
                  (let [condition (label->content element :condition)]
                    (label-for-element condition))
                  (let [candidate-elements
                        (map #(atomic-value (bind-entity % env))
                             (elements element))]
                    (first (filter #(and (not= % nil) (not= % :variable))
                                   candidate-elements))))]
      (expr-let [candidates (if (not (nil? label))
                              (label->elements target label)
                              (elements target))
                 match-envs (expr-seq map (partial template-matches element env)
                                      candidates)]
        (reduce (fn [result [candidate matching-envs]]
                  (reduce (fn [result env]
                            (update result env #(conj (or % []) candidate)))
                          result matching-envs))
                {} (map vector candidates match-envs))))))

(defn element-matches [element env target]  
  (expr-let [match-map (element-match-map element env target)]
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
  "Given a sequence of elements, a map from environments to collections
  of disallowed elements, and a target, return a sequence of environments
  where each element matches a different element in the target,
  and not a disallowed element."
  [elements env-map target]
  (if (empty? elements)
    (keys env-map)
    (expr-let [matching-maps
               (expr-seq map #(element-match-map (first elements) % target)
                         (keys env-map))]
      (let [disjoint-map (concat-maps
                          (map conj-disjoint-maps
                               (vals env-map) matching-maps))]
        (multiple-element-matches
         (rest elements) disjoint-map target)))))

(defn item-matches [item env target]
  (when verbose (println "item-matches"))
  (expr-let [content-match-envs
             (if-let [item-content (content item)]
               (expr template-matches
                 item-content env (content-reference target))
               [env])]
    (when (seq content-match-envs)
      (let [item-elements (elements item)]
        (cond (empty? item-elements)
              content-match-envs
              (empty? (rest item-elements))
              (expr-let [env-matches
                         (expr-seq map #(element-matches
                                         (first item-elements) % target)
                                   content-match-envs)]
                (seq (distinct (apply concat env-matches))))
              true
              (multiple-element-matches item-elements
                                        (zipmap content-match-envs
                                                (repeat [[]]))
                                        target))))))

(defn template-matches [template env target]
  (assert (not (mutable-entity? template)))
  (when verbose
    (println "(template-matches " template
             (zipmap (keys env) (map deep-to-list (vals env)))
             (simplify-for-print target)
             ")"))
  (if (atom? template)
    (expr-let [extended (extended-by? template target)]
      (when extended [env]))
    (if (variable? template)
      (variable-matches template env target)
      (item-matches template env target))))

(defmethod template-matches-m true [template env target]
  (template-matches template env target))

(defmethod best-template-match-m true [templates env target]
  (expr-let [matches (expr-seq map #(template-matches % env target) templates)]
    (when-let [candidates (->> (map (fn [match template]
                                      (when (seq match) template))
                                   matches templates)
                               (remove nil?)
                               (seq))]
      (reduce (fn [best template]
                (if (seq (template-matches best env template))
                  template
                  best))
              (first candidates) (rest candidates)))))

(def query-matches)

(defn variable-matches-in-store [var env store]
  (let [name (label->content var :name)
        reference (label->content var :reference)
        value (env name)]
    (if (nil? value)
      (expr-let [candidate-ids (store/candidate-matching-ids
                                store (turn-into-template var))
                 matches (expr-seq
                          map #(variable-matches
                                var env (description->entity % store))
                          candidate-ids)]
        (seq (distinct (apply concat matches))))
      (if reference
        [env]
        (expr-let [value-as-immutable (current-entity-value value)
                   matches (query-matches value-as-immutable env store)]
          (when matches [env]))))))

(defn exists-matches-in-store [exists env store]
  (let [names (expr-seq map content
                        (label->elements exists :variable-name))
        qualifier (label->content exists :qualifier)
        body (label->content exists :body)]
    (expr-let [matches (if qualifier
                         (expr apply concat
                               (expr-seq map #(query-matches body % store)
                                         (query-matches qualifier env store)))
                         (query-matches body env store))]
      (seq (distinct (map #(apply dissoc % names) matches))))))

(defn forall-matches-in-store [forall env store]
  (let [names (expr-seq map content
                             (label->elements forall :variable-name))
             qualifier (label->content forall :qualifier)
        body (label->content forall :body)]
    (expr-let [matches (query-matches qualifier env store)]
      (let [groups (group-by #(apply dissoc % names) matches)]
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
                                (set (map #(apply dissoc % names) extensions)))
                              binding-group)))
                binding-groups)))))))

(defn and-matches-in-store [and env store]
  (let [first (label->content and :first)
        second (label->content and :second)]
    (expr-let [matches (expr apply concat
                             (expr-seq map #(query-matches-m second % store)
                                       (query-matches-m first env store)))]
      (seq (distinct matches)))))

(defn item-matches-in-store [item env store]
  (expr-let [matches
             (expr apply concat
                   (expr-seq
                    map
                    (partial template-matches item env)
                    (expr map
                      #(description->entity % store)
                      (expr store/candidate-matching-ids
                        store (turn-into-template
                               (to-list (bind-entity item env)))))))]
    (seq (distinct matches))))

(defn query-matches
  ([query store] (query-matches query {} store))
  ([query env store]
   (assert (not (mutable-entity? query)))
   (when verbose
     (println "query" query "env"
              (zipmap (keys env) (map deep-to-list (vals env)))))
   (if (atom? query)
     (assert false "queries may not be atoms.")
     (let [query-content (content query)]
       (if (keyword? query-content)
         (case query-content
           :variable (variable-matches-in-store query env store)
           :exists (exists-matches-in-store query env store)
           :forall (forall-matches-in-store query env store)
           :and (and-matches-in-store query env store))
         (item-matches-in-store query env store))))))

(defmethod query-matches-m true [query env store]
  (query-matches query env store))

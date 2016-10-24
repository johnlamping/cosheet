(ns cosheet.query-impl
  (:require [clojure.pprint :refer [pprint]]
            (cosheet [store :as store]
                     [entity :refer [Entity
                                     mutable-entity? atom?
                                     content-reference
                                     description->entity
                                     content elements label->elements
                                     label->content atomic-value
                                     deep-to-list]]
                     [query :refer [extended-by-m?
                                    template-matches-m
                                    query-matches-m]]
                     [expression :refer [expr expr-let expr-seq]]
                     [debug :refer [trace-current
                                    simplify-for-print]])))

;;; TODO:
;;; Assume that the query is immutable, using call-with-immutable when
;;;     variables are instantiated from the environment
;;; Change the special form syntax so all special forms have content
;;;    :special-form.
;;; Change the store's lookup to take a template, and return all
;;;    candidates, maybe with an indication of whether they are
;;;    guaranteed to satisfy the template.
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

(def extended-by?)

(defn label-for-element
  "Given an entity that is an element, find an atom that can serve as
  a label for that element."
  [element]
  (let [annotations (elements element)
        labels (map atomic-value annotations)]
    (first (filter (partial not= nil) labels))))

(defn has-element-satisfying? [element target]
  "Return true if the target item has an element satisfying
  the given element)."
  (when (not (atom? target))
    (if (and (seq? element)
             (nil? (first element))
             (atom? (second element))
             (= (count element) 2))
      (expr-let [matching-elements (label->elements
                                    target (atomic-value (second element)))]
        (not (empty? matching-elements)))
      (let [label (label-for-element element)]
        (expr-let
            [candidates (if (not (nil? label))
                          (label->elements target label)
                          (elements target))
             extended-by (expr-seq map (partial extended-by? element)
                                   candidates)]
          (some #(not (nil? %)) extended-by))))))

(defn extended-by? [template target]
  (or (nil? template)
      (if (atom? template)
        (expr-let [target-value (atomic-value target)]
          (= (atomic-value template) target-value))
        (expr-let
            [content-extended (expr extended-by?
                                (content template)
                                (content target))]
          (when content-extended
            (expr-let
                [satisfied-elements (expr-seq
                                     map #(has-element-satisfying? % target)
                                     (elements template))]
              (every? identity satisfied-elements)))))))

(defmethod extended-by-m? true [template target]
  (extended-by? template target))

;;; Code to bind an entity in an environment

(defn variable? [template]
  (expr = (content template) :variable))

(def bind-entity)

(defrecord
    ^{:doc "An entity that is another entity with its variables bound
            by an environment.
            It will never be an atom or a bare bound variable."}
    BoundEntity

  [wrapped  ; The entity we wrap
   env]     ; The environment it is wrapped in
  
  Entity

  (mutable-entity? [this] (mutable-entity? wrapped))
  
  ;; Note: this assumes that an implicit content reference doesn't
  ;; outlive a promotion of its the content.  
  (atom? [this] (atom? wrapped))

  (label->elements [this label]
    (if (mutable-entity? wrapped)
      (expr-let [my-elements (elements this)
                 matches (expr-seq map #(expr some (partial = label)
                                              (expr-seq map atomic-value
                                                        (elements %)))
                                   my-elements)]
        (seq (map second (filter first (map vector matches elements)))))
      (seq (filter #(some (partial = label)
                          (map atomic-value (elements %)))
                   (map #(bind-entity % env)
                        (elements wrapped))))))

  (elements [this]
    (expr-let [unbound-elements (elements wrapped)]
      (seq (map #(bind-entity % env) unbound-elements))))

  (content [this]
    (expr bind-entity (content wrapped) env))

  (call-with-immutable [this fun]
    (assert false "call-with-immutable not supported by bound entities")))

(defn bind-entity [entity env]
  (if (atom? entity)
    (atomic-value entity)
    (if (mutable-entity? entity)
      (expr-let [is-variable (variable? entity)]
        (if is-variable
          (expr-let [name (label->content entity :name)]
            (or (env name) (->BoundEntity entity env)))
          (->BoundEntity entity env)))
      (or (and (variable? entity)
               (env (label->content entity :name)))
          (->BoundEntity entity env)))))

;;; TODO: Eventually extend template-matches to pass along a set of used
;;;       up stuff, so it can count repeated stuff in templates.

(def template-matches)

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
              [value-as-immutable (if (mutable-entity? value)
                                    (deep-to-list value)
                                    value)
               target-extends (extended-by? value-as-immutable target)]
            (when target-extends
              (if value-may-extend
                [env]
                (expr-let [target-as-list (deep-to-list target)]
                  (when (extended-by? target-as-list value-as-immutable)
                    [env]))))))
        (when (not (nil? target))
          (expr-let
              [envs (if (nil? condition)
                      [env]
                      (template-matches condition env target))]
            (if (not (nil? name))
              (expr-let [value (if reference target (deep-to-list target))]
                (seq (map #(assoc % name value) envs)))
              envs)))))))

;;; Find matches of elements of the target with the element.
(defn element-matches [element env target]
  (assert (not (mutable-entity? element)))
  (when verbose (println "element-matches" element (deep-to-list target)))
  ;; Test for the special case of looking for any element with a specific label.
  (if (and (seq? element)
           (nil? (first element))
           (atom? (second element))
           (= (count element) 2))
    (expr-let [matching-elements (label->elements
                                  target (atomic-value (second element)))]
      (when (not (empty? matching-elements)) [env]))
    (expr-let [label (expr-let [is-variable (variable? element)]
                       (if is-variable
                         (expr-let [condition (label->content
                                               element :condition)]
                           (label-for-element condition))
                         (expr-let [candidate-elements
                                    (expr-seq map #(expr atomic-value
                                                     (bind-entity % env))
                                              (elements element))]
                           (first (filter #(and (not= % nil) (not= % :variable))
                                          candidate-elements)))))
               candidates (if (not (nil? label))
                            (label->elements target label)
                            (elements target))
               match-envs (expr-seq map (partial template-matches element env)
                                    candidates)]
      (seq (distinct (apply concat match-envs))))))

(defn item-matches [item env target]
  (when verbose (println "item-matches"))
  (expr reduce
    (fn [envs element]
      (expr-let [env-matches (expr-seq map #(element-matches element % target)
                                       envs)]
        (seq (distinct (apply concat env-matches)))))
    (expr-let [item-content (content item)]
      (if item-content
        (expr template-matches
          item-content env (content-reference target))
        [env]))
    (elements item)))

(defn template-matches [template env target]
  (assert (not (mutable-entity? template)))
  (when verbose
    (println "template-matches(" template
             (zipmap (keys env) (map deep-to-list (vals env)))
             (simplify-for-print target)
             ")"))
  (if (atom? template)
    (expr-let [extended (extended-by? template target)]
      (when extended [env]))
    (expr-let [is-variable (variable? template)]
      (if is-variable
        (variable-matches template env target)
        (item-matches template env target)))))

(defmethod template-matches-m true [template env target]
  (template-matches template env target))

(def query-matches)

(defn variable-matches-in-store [var env store]
  (expr-let [name (label->content var :name)
             reference (label->content var :reference)]
    (let [value (env name)]
      (if (nil? value)
        (expr-let [candidate-ids (store/candidate-matching-ids store nil)
                   matches (expr-seq
                            map #(variable-matches
                                  var env (description->entity % store))
                            candidate-ids)]
          (seq (distinct (apply concat matches))))
        (if reference
          [env]
          (expr-let [matches (query-matches value env store)]
            (when matches [env])))))))

(defn exists-matches-in-store [exists env store]
  (expr-let [names (expr-seq map content
                             (label->elements exists :variable-name))
             qualifier (label->content exists :qualifier)
             body (label->content exists :body)
             matches (if qualifier
                       (expr apply concat
                             (expr-seq map #(query-matches body % store)
                                       (query-matches qualifier env store)))
                       (query-matches body env store))]
    (seq (distinct (map #(apply dissoc % names) matches)))))

(defn forall-matches-in-store [forall env store]
  (expr-let [names (expr-seq map content
                             (label->elements forall :variable-name))
             qualifier (label->content forall :qualifier)
             body (label->content forall :body)
             matches (query-matches qualifier env store)]
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
              binding-groups))))))

(defn and-matches-in-store [and env store]
  (expr-let [first (label->content and :first)
             second (label->content and :second)
             matches (expr apply concat
                           (expr-seq map #(query-matches-m second % store)
                                     (query-matches-m first env store)))]
    (seq (distinct matches))))

(defn item-matches-in-store [item env store]
  ;;; We don't handle mutable items, because we pass them to
  ;;; store/candidate-matching-ids, which can't handle them.
  ;;; TODO: Maybe handle this case?
  (assert (not (mutable-entity? item)))
  (expr-let [matches
             (expr apply concat
                   (expr-seq
                    map
                    (partial template-matches item env)
                    (expr map
                      #(description->entity % store)
                      (expr store/candidate-matching-ids
                        store (bind-entity item env)))))]
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
     (expr-let [query-content (content query)]
       (if (keyword? query-content)
         (case query-content
           :variable (variable-matches-in-store query env store)
           :exists (exists-matches-in-store query env store)
           :forall (forall-matches-in-store query env store)
           :and (and-matches-in-store query env store))
         (item-matches-in-store query env store))))))

(defmethod query-matches-m true [query env store]
  (query-matches query env store))

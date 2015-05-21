(ns cosheet.query-impl
  (:require [clojure.pprint :refer [pprint]]
            (cosheet [store :as store]
                     [entity :as entity]
                     [query :refer [extended-by-m?
                                    template-matches-m
                                    query-matches-m]]
                     [reporters :refer [expr expr-let expr-seq]]
                     [debug :refer [current-value trace-current
                                    simplify-for-print]])))

;;; TODO:
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

(def extended-by?)

(defn has-element-satisfying? [element target]
  "Return true if the item has an element satisfying the given element)."
  (when (not (entity/atom? target))
    (expr-let
        [annotations (expr entity/elements element)
         labels (expr-seq map entity/atomic-value annotations)
         label (first (filter (partial not= nil) labels))
         candidates (if (not (nil? label))
                      (expr entity/label->elements target label)
                      (expr entity/elements target))
         extended-by (expr-seq map (partial extended-by? element) candidates)]
      (some #(not (nil? %)) extended-by))))

(defn extended-by? [template target]
  (or (nil? template)
      (if (entity/atom? template)
        (expr-let [template-value (expr entity/atomic-value template)
                   target-value (expr entity/atomic-value target)]
          (= template-value target-value))
        (expr-let
            [content-extended (expr extended-by?
                                (expr entity/content template)
                                (expr entity/content target))]
          (when content-extended
            (expr-let
                [template-elements (expr entity/elements template)
                 satisfied-elements (expr-seq
                                     map #(has-element-satisfying? % target)
                                     template-elements)]
              (every? identity satisfied-elements)))))))

(defmethod extended-by-m? true [template target]
  (extended-by? template target))

;;; Code to bind an entity in an environment

(defn variable? [template]
  (if (entity/mutable-entity? template)
    (expr = (entity/content template) :variable)
    (= (entity/content template) :variable)))

(def bind-entity)

(defrecord
    ^{:doc "An entity that is another entity with its variables bound
            by an environment.
            It will never be an atom or a bare bound variable."}
    BoundEntity

  [wrapped  ; The entity we wrap
   env]     ; The environment it is wrapped in
  
  entity/Entity

  (entity/mutable-entity? [this] (entity/mutable-entity? wrapped))
  
  ;; Note: this assumes that an implicit content reference doesn't
  ;; outlive a promotion of its the content.  
  (entity/atom? [this] (entity/atom? wrapped))

  (entity/label->elements [this label]
    (if (entity/mutable-entity? wrapped)
      (expr-let [elements (entity/elements this)
                 matches (expr-seq map #(expr some (partial = label)
                                              (expr-seq map entity/atomic-value
                                                        (entity/elements %)))
                                   elements)]
        (seq (map second (filter first (map vector matches elements)))))
      (seq (filter #(some (partial = label)
                          (map entity/atomic-value (entity/elements %)))
                   (map #(bind-entity % env)
                        (entity/elements wrapped))))))

  (entity/elements [this]
    (if (entity/mutable-entity? wrapped)
      (expr-let [unbound-elements (entity/elements wrapped)]
        (seq (map #(bind-entity % env) unbound-elements)))
      (seq (map #(bind-entity % env)
                (entity/elements wrapped)))))

  (entity/content [this]
    (if (entity/mutable-entity? wrapped)
      (expr bind-entity (entity/content wrapped) env)
      (bind-entity (entity/content wrapped) env))))

(defn bind-entity [entity env]
  (if (entity/atom? entity)
    (entity/atomic-value entity)
    (if (entity/mutable-entity? entity)
      (expr-let [is-variable (variable? entity)]
        (if is-variable
          (expr-let [name (entity/label->content entity :name)]
            (or (env name) (->BoundEntity entity env)))
          (->BoundEntity entity env)))
      (or (and (variable? entity)
               (env (entity/label->content entity :name)))
          (->BoundEntity entity env)))))

;;; TODO: Eventually extend template-matches to pass along a set of used
;;;       up stuff, so it can count repeated stuff in templates.

(def template-matches)

(defn variable-matches [var env target]
  (expr-let [name (entity/label->content var :name)
             condition (entity/label->content var :condition)
             value-may-extend (entity/label->content var :value-may-extend)
             reference (entity/label->content var :reference)]
    (let [value (env name)]
      (if (not (nil? value))
        (if reference
          (when (= value target)
            [env])
          (expr-let
              [target-extends (expr extended-by? value target)]
            (when target-extends
              (if value-may-extend
                [env]
                (expr-let [value-extends (expr extended-by? target value)]
                  (when value-extends [env]))))))
        (when (not (nil? target))
          (expr-let
              [envs (if (nil? condition)
                      [env]
                      (expr template-matches condition env target))]
            (if (not (nil? name))
              (expr-let [value (if reference target (entity/to-list target))]
                (seq (map #(assoc % name value) envs)))
              envs)))))))

;;; Find matches of elements of the target with the element.
(defn element-matches [element env target]
  (when verbose (println "element-matches" element (entity/to-list target)))
  (expr-let [label (expr-let [is-variable (variable? element)]
                     (if is-variable
                       nil
                       (expr-let [candidate-elements
                                  (expr-seq map #(expr entity/atomic-value
                                                   (bind-entity % env))
                                            (entity/elements element))]
                         (first (filter #(and (not= % nil) (not= % :variable))
                                        candidate-elements)))))
             candidates (if (not (nil? label))
                          (entity/label->elements target label)
                          (entity/elements target))
             match-envs (expr-seq map (partial template-matches element env)
                                  candidates)]
    (seq (distinct (apply concat match-envs)))))

(defn item-matches [item env target]
  (when verbose (println "item-matches"))
  (expr reduce
    (fn [envs element]
      (expr-let [env-matches (expr-seq map #(element-matches element % target)
                                       envs)]
        (seq (distinct (apply concat env-matches)))))
    (expr-let [content (entity/content item)]
      (if content
        (expr template-matches
          (entity/content item) env (entity/content-reference target))
        [env]))
    (entity/elements item)))

(defn template-matches [template env target]
  (when verbose
    (println "template-matches(" template
             (zipmap (keys env) (map entity/to-list (vals env)))
             (simplify-for-print target)
             ")"))
  (if (entity/atom? template)
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
  (expr-let [name (entity/label->content var :name)
             reference (entity/label->content var :reference)]
    (let [value (env name)]
      (if (nil? value)
        ;; TODO: Shouldn't this check the condition to make sure the
        ;; variable really matches?
        (expr-let [candidate-ids (store/candidate-matching-ids store nil)]
          (map #(assoc env name (entity/description->entity % store))
               candidate-ids))
        (if reference
          [env]
          (expr-let [matches (query-matches value env store)]
            (when matches [env])))))))

(defn exists-matches-in-store [exists env store]
  (expr-let [names (expr-seq map entity/content
                             (entity/label->elements exists :variable-name))
             qualifier (entity/label->content exists :qualifier)
             body (entity/label->content exists :body)
             matches (if qualifier
                       (expr apply concat
                             (expr-seq map #(query-matches body % store)
                                       (query-matches qualifier env store)))
                       (query-matches body env store))]
    (seq (distinct (map #(apply dissoc % names) matches)))))

(defn forall-matches-in-store [forall env store]
  (expr-let [names (expr-seq map entity/content
                             (entity/label->elements forall :variable-name))
             qualifier (entity/label->content forall :qualifier)
             body (entity/label->content forall :body)
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
  (expr-let [first (entity/label->content and :first)
             second (entity/label->content and :second)
             matches (expr apply concat
                           (expr-seq map #(query-matches-m second % store)
                                     (query-matches-m first env store)))]
    (seq (distinct matches))))

;;; TODO: We are passing store/candidate-matching-ids a possibly
;;; mutable entity, which it can't handle
(defn item-matches-in-store [item env store]
  (expr-let [matches
             (expr apply concat
                   (expr-seq
                    map
                    (partial template-matches item env)
                    (expr map
                      #(entity/description->entity % store)
                      (expr store/candidate-matching-ids
                        store (bind-entity item env)))))]
    (seq (distinct matches))))

(defn query-matches
  ([query store] (query-matches query {} store))
  ([query env store]
   (when verbose
     (println "query" query "env"
              (zipmap (keys env) (map entity/to-list (vals env)))))
   (if (entity/atom? query)
     (assert false "queries may not be atoms.")
     (expr-let [content (entity/content query)]
               (if (keyword? content)
                 (case content
                   :variable (variable-matches-in-store query env store)
                   :exists (exists-matches-in-store query env store)
                   :forall (forall-matches-in-store query env store)
                   :and (and-matches-in-store query env store))
                 (item-matches-in-store query env store))))))

(defmethod query-matches-m true [query env store]
  (query-matches query env store))

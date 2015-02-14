(ns cosheet.query-impl
  (:require [clojure.pprint :refer [pprint]]
(cosheet [store :as store]
                     [entity :as entity]
                     [query :refer :all]
                     [compute :as compute :refer [expr expr-let expr-map]]
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

(def extended-by?9)

(defn has-element-satisfying?9 [element target]
  "Return true if the item has an element satisfying the given element)."
  (when (not (entity/atom? target))
    (expr-let
     [annotations (expr entity/elements element)
      labels (expr-map entity/atomic-value annotations)
      label (first (filter (partial not= nil) labels))
      candidates (if (not (nil? label))
                   (expr entity/label->elements target label)
                   (expr entity/elements target))
      extended-by (expr-map (partial extended-by?9 element) candidates)]
     (some #(not (nil? %)) extended-by))))

(defn has-element-satisfying? [element target]
  (current-value [has-element-satisfying?9 element target]))

(defn extended-by?9 [template target]
  (or (nil? template)
      (if (entity/atom? template)
        (expr-let [template-value (expr entity/atomic-value template)
                   target-value (expr entity/atomic-value target)]
                  (= template-value target-value))
        (expr-let
         [content-extended (expr extended-by?9
                                 (expr entity/content template)
                                 (expr entity/content target))]
         (when content-extended
           (expr-let
            [template-elements (expr entity/elements template)
             satisfied-elements (expr-map #(has-element-satisfying?9 % target)
                                          template-elements)]
            (every? identity satisfied-elements)))))))

(defmethod extended-by-m? true [template target]
  (current-value [extended-by?9 template target]))

;;; Code to bind an entity in an environment

(defn variable? [template]
  (= (entity/content template) :variable))

(def bind-entity)

(defrecord
    ^{:doc "An entity that is another entity with its variables bound
            by an environment.
            It will never be an atom or a bare bound variable.
            The underlying entity must be immutable."}
    BoundEntity

  [wrapped  ; The entity we wrap
   env]     ; The environment it is wrapped in
  
  entity/Entity

  (entity/mutable-entity? [this] false)
  
  (entity/atom? [this] false)

  (entity/label->elements [this label]
    (seq (filter #(some (partial = label)
                        (map entity/atomic-value (entity/elements %)))
                 (map #(bind-entity % env)
                      (entity/elements wrapped)))))

  (entity/elements [this]
    (seq (map #(bind-entity % env)
              (entity/elements wrapped))))

  (entity/content [this]
    (bind-entity (entity/content wrapped) env)))

(defn bind-entity [entity env]
  (if (entity/atom? entity)
    (entity/atomic-value entity)
    (or (and (variable? entity)
             (env (entity/label->content entity :name)))
        (->BoundEntity entity env))))

;;; TODO: Eventually extend template-matches to pass along a set of used
;;;       up stuff, so it can count repeated stuff in templates.

(def template-matches9)

(defn variable-matches9 [var env target]
  (let [name (entity/label->content var :name)
        condition (entity/label->content var :condition)
        value-may-extend (entity/label->content var :value-may-extend)
        reference (entity/label->content var :reference)]
    (let [value (env name)]
      (if (not (nil? value))
        (if reference
          (when (= value target)
            [env])
          (expr-let
           [target-extends (expr extended-by?9 value target)]
           (when target-extends
             (if value-may-extend
               [env]
               (expr-let [value-extends (expr extended-by?9 target value)]
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

(defn variable-matches [var env target]
  (current-value [variable-matches9 var env target]))

;;; Find matches of elements of the target with the element.
(defn element-matches9 [element env target]
  (when verbose (println "element-matches" element (entity/to-list target)))
  (expr-let [label (if (variable? element)
                     nil
                     (expr-let [candidate-elements
                                (expr-map #(entity/atomic-value
                                            (bind-entity % env))
                                          (entity/elements element))]
                       (first (filter #(and (not= % nil) (not= % :variable))
                                      candidate-elements))))
             candidates (if (not (nil? label))
                          (entity/label->elements target label)
                          (entity/elements target))
             match-envs (expr-map (partial template-matches9 element env)
                                  candidates)]
    (seq (distinct (apply concat match-envs)))))

(defn element-matches [element env target]
  (current-value [element-matches9 element env target]))

(defn item-matches9 [item env target]
  (when verbose (println "item-matches"))
  (expr reduce
        (fn [envs element]
          (expr-let [env-matches (expr-map #(element-matches9 element % target)
                                           envs)]
                    (seq (distinct (apply concat env-matches)))))
        (expr-let [content (entity/content item)]
                  (if content
                    (expr template-matches9
                          (entity/content item) env
                          (entity/content-reference target))
                    [env]))
        (entity/elements item)))

(defn item-matches [item env target]
  (current-value [item-matches9 item env target]))

(defn template-matches9 [template env target]
  (when verbose
    (println "template-matches(" template
             (zipmap (keys env) (map entity/to-list (vals env)))
             (simplify-for-print target)
             ")")
    (println (entity/atom? template))
    (println (extended-by?9 template target))
    (println (expr-let [extended (extended-by?9 template target)]
                       (when extended [env])))
    (pprint (simplify-for-print
             (trace-current
              (expr-let [extended (extended-by?9 template target)]
                        (when extended [env]))))))
  (cond
    (entity/atom? template)
    (expr-let [extended (extended-by?9 template target)]
              (when extended [env]))
    (variable? template)
    (variable-matches9 template env target)
    :else
    (item-matches9 template env target)))

(defmethod template-matches-m :no-env [template target]
  (current-value [template-matches9 template {} target]))

(defmethod template-matches-m :env [template env target]
  (current-value [template-matches9 template env target]))

(defn variable-matches-in-store [var env store]
  (let [name (entity/label->content var :name)
        reference (entity/label->content var :reference)
        value (env name)]
    (if (nil? value)
      (seq (map #(assoc env name
                        (let [value (entity/description->entity % store)]
                          (if reference value (entity/to-list value) )))
                (store/candidate-matching-ids store nil)))
      (if (or reference (query-matches value env store))
        [env]))))

(defn exists-matches-in-store [exists env store]
  (let [names (map entity/content
                   (entity/label->elements exists :variable-name))
        qualifier (entity/label->content exists :qualifier)
        body (entity/label->content exists :body)]
    (seq (distinct
          (map #(apply dissoc % names)
               (if qualifier
                 (mapcat #(query-matches body % store)
                         (query-matches qualifier env store))
                 (query-matches body env store)))))))

(defn forall-matches-in-store [forall env store]
  (let [names (map entity/content
                   (entity/label->elements forall :variable-name))
        qualifier (entity/label->content forall :qualifier)
        body (entity/label->content forall :body)]

    (let [groups (group-by #(apply dissoc % names)
                           (query-matches qualifier env store))]
      (seq (mapcat
            (fn [group]
              (apply clojure.set/intersection
                     (map (fn [binding]
                            (set (map #(apply dissoc % names)
                                      (query-matches body binding store))))
                          (groups group))))
            (keys groups))))))

(defn envs-to-list [envs]
  (seq (map #(zipmap (keys %) (map entity/to-list (vals %))) envs)))

(defn and-matches-in-store [and env store]
  (let [first (entity/label->content and :first)
        second (entity/label->content and :second)]
    (let [matches (mapcat #(query-matches second % store)
                          (query-matches first env store))]
      (seq (distinct matches)))))

(defn item-matches-in-store [item env store]
  (seq (distinct
        (mapcat (partial template-matches item env)
                (map #(entity/description->entity % store)
                     (store/candidate-matching-ids
                      store (bind-entity item env)))))))

(defmethod query-matches :no-env [query store]
  (query-matches query {} store))

(defmethod query-matches :env [query env store]
  (when verbose
    (println "query" query "env"
             (zipmap (keys env) (map entity/to-list (vals env)))))
  (cond
    (entity/atom? query)
    (assert false "queries may not be atoms.")
    (keyword? (entity/content query))
    (case (entity/content query)
      :variable (variable-matches-in-store query env store)
      :exists (exists-matches-in-store query env store)
      :forall (forall-matches-in-store query env store)
      :and (and-matches-in-store query env store))
    :default
    (item-matches-in-store query env store)))

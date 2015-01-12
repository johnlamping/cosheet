(ns cosheet.query-impl
  (:require [cosheet.store :as store]
            [cosheet.entity :as entity]
            [cosheet.query :refer :all]))

;;; TODO:
;;; Change the special form syntax so all special forms have content
;;;    :special-form.
;;; Make sure the store supports multiple subjects for the same
;;;    element.
;;; Change the store's lookup to take a template, and return all
;;;    candidates, maybe with an indication of whether they are
;;;    guaranteed to satisfy the template.
;;; Add a term syntax that lets variables bind to the subject.
;;; Change the API so that functions don't recurse, but either return
;;;    a value or a set of queries and a function to call when the
;;;    values of those queries are known. (Think about how this works
;;;    for queries that return environments, and not just lists of items.
;;; Change the API so that things that return multiple environments
;;;    also take multiple environments as arguments.
;;; Add a unification operation on terms, so rule matching can work.
;;;    The environment must include variable numbers, for renaming,
;;;    and an indication of the number of the current term, which must
;;;    be the highest number.

(def ^:dynamic verbose false)

(defn has-element-satisfying? [element target]
  "Return true if the item has an element satisfying the given element)."
  (when (not (entity/atom? target))
    (let [candidates
          (let [label (first (->> (entity/elements element)
                                  (map entity/atomic-value)
                                  (filter (partial not= nil))))]
            (if label
              (entity/label->elements target label)
              (entity/elements target)))]
      (some (partial extended-by? element)
            candidates))))

(defmethod extended-by? true [template target]
  (if (entity/atom? template)
    (= (entity/atomic-value template) (entity/atomic-value target))
    (and (let [content (entity/content template)]
           (or (nil? content) (extended-by? content target)))
         (every? #(has-element-satisfying? % target)
                 (entity/elements template)))))

;;; Code to bind an entity in an environment

(defn variable? [template]
  (= (entity/content template) :variable))

(def bind-entity)

(defrecord
    ^{:doc "An entity that is another entity with its variables bound
            by an environment.
            It will never be an atom or a bare bound variable."}
    BoundEntity

  [wrapped  ; The entity we wrap
   env]     ; The environment it is wrapped in
  
  entity/Entity
  
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

;;; TODO: Eventually extend template-matches to  pass along a set of used
;;;       up stuff, so it can count repeated stuff in templates.

(defn variable-matches [var env target]
  (let [name (entity/label->content var :name)
        condition (entity/label->content var :condition)
        value-may-extend (entity/label->content var :value-may-extend)
        reference (entity/label->content var :reference)]
    (let [value (env name)]
      (if (not (nil? value))
        (if (if reference
              (= value target)
              (and (extended-by? value target)
                   (or value-may-extend (extended-by? target value))))
          [env])
        (when (not (nil? target))
          (let [envs (if (nil? condition)
                       [env]
                       (template-matches condition env target))]
            (if (not (nil? name))
              (let [value (if reference target (entity/to-list target))]
                (seq (map #(assoc % name value) envs)))
              envs)))))))

;;; Find matches of elements of the target with the element.
(defn element-matches [element env target]
  (when verbose (println "element-matches" element (entity/to-list target)))
  (let [label (if (variable? element)
                nil
                (first (->> (entity/elements element)
                            (map #(entity/atomic-value (bind-entity % env)))
                            (filter #(and (not= % nil) (not= % :variable))))))
        candidates (if (not (nil? label))
                     (entity/label->elements target label)
                     (entity/elements target))]
    (seq (distinct (mapcat (partial template-matches element env)
                           candidates)))))

(defn item-matches [item env target]
  (when verbose (println "item-matches"))
  (seq
   (reduce
    (fn [envs element]
      (distinct (mapcat #(element-matches element % target)
                        envs)))
    (if (entity/content item)
      (template-matches
       ;; TODO: Should be content-reference for second arg below.
       (entity/content item) env (entity/content-reference target))
      [env])
    (entity/elements item))))

(defmethod template-matches :no-env [template target]
  (template-matches template {} target))

(defmethod template-matches :env [template env target]
  (when verbose (println "template matches" template
                         (zipmap (keys env) (map entity/to-list (vals env)))
                         (entity/to-list target)
                         "xxx"))
  (cond
   (entity/atom? template)
   (if (extended-by? template target) [env])
   (variable? template)
   (variable-matches template env target)
   :else
   (item-matches template env target)))

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

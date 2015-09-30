(ns cosheet.debug
  (:require [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            (cosheet [store :as store]
                     [entity :as entity]
                     [reporters :refer [reporter? attended? data value
                                       set-attendee!]]
                     [entity-impl :as entity-impl]
                     store-utils
                     store-impl
                     mutable-store-impl
                     [mutable-map :as mm])))

(defn- function-name [f]
  (let [name (str f)
        matches (re-matches #"(.+?)(?:__\d+)?@.+" name)]
    (symbol (if matches
              (let [name (matches 1)
                    matches (re-matches #"clojure.*\$(.*)" name)]
                (if matches
                  (matches 1)
                  (let [matches (re-matches #"cosheet\.(.*)" name)]
                    (clojure.string/replace
                     (clojure.string/replace
                      (clojure.string/replace
                       (if matches (matches 1) name)
                       "_QMARK_" "?")
                      "$" "/")
                     "_" "-")))) 
              name))))

(defn reporter-computation
  "Return an expression indicating the computation of the reporter,
  recursing down to sub-expressions."
  [expr]
  (if (reporter? expr)
    (let [data (data expr)
          computation (or (:expression data) (:fetch data))]
      (if computation
        (map reporter-computation computation)
        "Reporter"))
    expr))

(defn simplify-for-print [item]
  (cond (satisfies? store/Store item)
        (if (store/mutable-store? item)
          (symbol "MutableStore")
          (symbol "Store"))
        (satisfies? store/StoredItemDescription item)
        (symbol (store/stored-item-description-name item))
        (or (instance? cosheet.entity_impl.StoredItem item)
            (instance? cosheet.entity_impl.MutableStoredItem item))
        (symbol
         (clojure.string/join ["Entity-" (simplify-for-print (:item-id item))]))
        (reporter? item)
        (simplify-for-print (reporter-computation item))
        (instance? clojure.lang.PersistentHashSet item)
        (set (map simplify-for-print item))
        (instance? clojure.lang.PersistentArrayMap item)
        (let [keys (keys item)]
          (zipmap (map simplify-for-print keys)
                  (for [key keys] (simplify-for-print (get item key)))))
        (instance? clojure.lang.PersistentVector item)
        (if (and (not (empty? item))
                 (every? #(instance? clojure.lang.Atom %) item))
          (simplify-for-print (mm/current-contents item))
          (vec (map simplify-for-print item)))
        (instance? clojure.lang.Fn item)
        (function-name item)
        (sequential? item)
        (map simplify-for-print item)
        :else
        item))

;;; Trivial scheduler that just runs everything and returns the
;;; current value.

(defn current-value
  "Run computation on the reporter returning the current value,
   rather than tracking dependencies."
  [expr]
  (if (reporter? expr)
    (let [data (data expr)
          expression (:expression data)]
      (if expression
        ((or (:trace data) identity)
         #(current-value (apply (fn [f & args] (apply f args))
                                (map current-value expression))))
        (do (when (and (:manager data) (not (attended? expr)))
              (set-attendee! expr :request (fn [key reporter] nil)))
            (value expr))))
    expr))

(defn- unpack-if-trivial-nested [item]
  (if (and (sequential? item)
           (= (count item) 2)
           (= (first item) (second item)))
    (first item)
    item))

(defn trace-current
  "Run computation on the reporter, returning a trace of the item
   with all intermediate values filled in."
  [expr]
    (if (reporter? expr)
      (let [data (data expr)
            expression (:expression data)]
        (if expression
          (let [parts (map trace-current expression)
                simplified-parts (map unpack-if-trivial-nested parts)
                values (map first parts)
                result ((fn [[f & args]] (apply f args)) values)
                trace (trace-current result)]
            (conj (if (= (first trace) (second trace)) (vec (rest trace)) trace)
                  (cons :expr simplified-parts)))
          [(:value data)]))      
      [expr]))

(defn pst [item]
  (pprint (simplify-for-print (trace-current item))))

(defmacro let-propagated-impl [[var entity & more-bindings] exp]
  (let [body (if (empty? more-bindings)
               exp
               `(let-propagated-impl ~more-bindings ~exp))]
    `(let [s# (cosheet.store/new-element-store)
           ms# (cosheet.store/new-mutable-store s#)
           ;; Get the id the entity will have after we add it.
           [_ id#] (cosheet.store-utils/add-entity s# nil ~entity)
           ~var (cosheet.entity/description->entity id# ms#)
           exp-val# ~body]
       (cosheet.store-utils/add-entity! ms# nil ~entity)
       exp-val#)))

;;; A macro to test propagation of changes of an entity through an expression.
;;; Set up the values of the bindings to be entities that are
;;; currently empty, but that will
;;; equal the specified entity. Evaluate exp with a current value of
;;; that entity being empty, then set the entity in the mutable
;;; store, and return the new current value of the expression.
(defmacro let-propagated [bindings exp]
  `(current-value (let-propagated-impl ~bindings ~exp)))

(defmacro print-propagated [bindings exp]
  `(pst (let-propagated-impl ~bindings ~exp)))

;;; A macro to test propagation of changes through a store. Set up var
;;; to a mutable store, initialized from the initial immutable store.
;;; Evaluate exp with the store in that state. Then call the mutator
;;; with the mutable store as an argument, and return the new current
;;; value of the expression.
(defmacro let-propagated-store [[var initial mutator] exp]
  `(let [~var (store/new-mutable-store ~initial)
         exp-val# ~exp]
     (~mutator ~var)
     (current-value exp-val#)))

(defn envs-to-list [envs]
  "Given a vector of environments, as returned by a query, turn it into maps
   of the current value of the environments."
  (seq (for [env envs]
         (zipmap (keys env)
                 (map #(current-value (entity/to-list %)) (vals env))))))


(ns cosheet.debug
  (:require [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            (cosheet [compute :refer :all]
                     [state :refer :all]
                     [store :as store]
                     [entity :as entity]
                     [entity-impl :as entity-impl]
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

(defn simplify-for-print [item]
  (cond (state? item)
        `(~(symbol "State")  ~(simplify-for-print (state-value item)))
        (expression? item)
        (cons :expr (simplify-for-print
                     (cons (expression-fn item) (expression-args item))))
        (satisfies? store/Store item)
        (if (store/mutable-store? item)
          (symbol "MutableStore")
          (symbol "Store"))
        (satisfies? store/StoredItemDescription item)
        (symbol (store/stored-item-description-name item))
        (or (instance? cosheet.entity_impl.StoredItem item)
            (instance? cosheet.entity_impl.MutableStoredItem item))
        (symbol
         (clojure.string/join ["Entity-" (simplify-for-print (:item-id item))]))
        (instance? clojure.lang.PersistentHashSet item)
        (set (map simplify-for-print item))
        (instance? clojure.lang.PersistentArrayMap item)
        (let [keys (keys item)]
          (zipmap (map simplify-for-print keys)
                  (for [key keys] (simplify-for-print (get item key)))))
        (instance? clojure.lang.PersistentVector item)
        (if (every? #(instance? clojure.lang.Atom %) item)
          (simplify-for-print (mm/current-contents item))
          (vec (map simplify-for-print item)))
        (instance? clojure.lang.Fn item)
        (function-name item)
        (sequential? item)
        (map simplify-for-print item)
        :else
        item))

(defn envs-to-list [envs]
  (seq (map #(zipmap (keys %) (map entity/to-list (vals %))) envs)))

;;; Trivial scheduler that just runs everything and returns the
;;; current value.

(def current-value)

(defn reference-current-value
  [[fn & args]]
  (current-value (apply fn args)))

(defn current-value
  "Run computation on the computational item, returning the current value,
   rather than tracking dependencies."
  [expr]
  (cond (state? expr)
        (state-value expr)
        (expression? expr)
        ((expression-tracer expr)
         #(reference-current-value
           (cons (current-value (expression-fn expr))
                 (map current-value (expression-args expr)))))
        :else
        expr))

(def trace-current)

(defn reference-trace-current [[fn & args]]
  (trace-current (apply fn args)))

(defn- unpack-if-trivial-nested [item]
  (if (and (sequential? item)
           (= (count item) 2)
           (= (first item) (second item)))
    (first item)
    item))

(defn trace-current
  "Run computation on the computational item, returning a trace of the item
   with all intermediate values filled in."
  [expr]
  (cond
    (state? expr)
    [(state-value expr) expr]
    (expression? expr)
    (let [parts (cons (trace-current (expression-fn expr))
                      (map trace-current (expression-args expr)))
          simplified-parts (map unpack-if-trivial-nested parts)
          trace (reference-trace-current (map first parts))]
      (conj (if (= (first trace) (second trace)) (vec (rest trace)) trace)
            (cons :expr simplified-parts)))
    :else
    [expr expr]))

(defn pst [item]
  (pprint (simplify-for-print (trace-current item))))

(defmacro let-propagated-impl [[var entity & more-bindings] exp]
  (let [body (if (empty? more-bindings)
               exp
               `(let-propagated-impl ~more-bindings ~exp))]
    `(let [s# (store/new-element-store)
           ms# (store/new-mutable-store s#)
           ;; Get the id the entity will have after we add it.
           [_ id#] (store-utils/add-entity s# nil ~entity)
           ~var (entity/description->entity id# ms#)
           exp-val# ~body]
       (store-utils/add-entity! ms# nil ~entity)
       exp-val#)))

;;; A macro to test propagation of changes through an expression.
;;; Set up var to be an entity that is currently empty, but that will
;;; equal the specified entity. Evaluate exp with a current value of
;;; that entitity being empty, then set the entity in the mutable
;;; store, and return the new current value of the expression.
(defmacro let-propagated [bindings exp]
  `(current-value (let-propagated-impl ~bindings ~exp)))

(defmacro print-propagated [bindings exp]
  `(pst (let-propagated-impl ~bindings ~exp)))


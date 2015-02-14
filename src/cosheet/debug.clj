(ns cosheet.debug
  (:require [clojure.set :as set]
            (cosheet [compute :refer :all]
                     [state :refer :all]
                     [store :as store]
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
                      (if matches (matches 1) name)
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

;;; Trivial scheduler that just runs everything and returns the
;;; current value.

(def reference-current-value)

(defn expression-current-value [expr]
  (if (expression? expr)
    ((expression-tracer expr)
     #(reference-current-value
       (cons (expression-current-value (expression-fn expr))
             (map expression-current-value (expression-args expr)))))
    expr))

(defn reference-current-value
  [[fn & args]]
  (let [result (apply fn args)]
    (cond (state? result) (state-value result)
          (expression? result) (expression-current-value result)
          :else result)))

(defn current-value
  "Run computation on the computational item, returning the current value,
   rather than tracking dependencies."
 [item]
  (cond (state? item) (state-value item)
        (expression? item) (expression-current-value item)
        (sequential? item) (reference-current-value item)
        :else item))

(def reference-trace-current)

(defn expression-trace-current [expr]
  (cond
    (state? expr)
    [(state-value expr) expr]
    (expression? expr)
    (let [parts (cons (expression-trace-current (expression-fn expr))
                      (map expression-trace-current (expression-args expr)))
          trace (reference-trace-current (map first parts))]
      (conj (if (= (first trace) (second trace)) [(first trace)] trace)
            (cons :expr (map  #(if (= (first %) (second %)) (first %) %)
                              parts))))
    :else
    [expr expr]))

(defn reference-trace-current [[fn & args]]
  (expression-trace-current (apply fn args)))

(defn trace-current
  "Run computation on the computational item, returning a trace of the item
   with all intermediate values filled in."
  [item]
  (if (and (sequential? item) (not (expression? item)) (not (state? item)))
    ;; As a convenience, we allow a vector to be used as a reference.
    (conj (reference-trace-current item) item)
    (expression-trace-current item)))

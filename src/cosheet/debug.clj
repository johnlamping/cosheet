(ns cosheet.debug
  (:require [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            (cosheet [utils :refer [parse-string-as-number]]
                     [store :as store]
                     [entity :as entity]
                     [reporters :refer [reporter? attended? data value
                                        set-attendee!]]
                     [expression-manager :as expression-manager]
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
                    matches (re-matches #"(clojure.*)\$(.*)" name)]
                (if matches
                  (if (number? (parse-string-as-number (matches 2)))
                    (matches 1)
                    (matches 2))
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
        (list* "R" (simplify-for-print (reporter-computation item)))
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

;;; Code to walk reporters and generate a profile.
;;; See doc for reporters-profile for a description of the output.
;;; Many of these functions also take a set of reporters already seen
;;; on some other path through the dag. They will be charged only to
;;; the first path seen to them. They also take a seq of function
;;; names of ancestors to the given reporter.

(def accumulate-profile)

(defn accumulate-invocations
  "Count one invocation of fun-name, under each of its ancestors, plus
  just itself."
  [acc fun-name ancestors]
  (reduce (fn [acc ancestor]
            (update-in acc [ancestor fun-name] (fnil inc 0)))
          acc (conj ancestors nil)))

(defn accumulate-expression-reporter-profile
  "Accumulate one expression into the profile information, given its data."
  [acc seen data ancestors]
  (let [expression (:expression data)
        source (:value-source data)
        fun (first expression)
        fun-name (when (instance? clojure.lang.Fn fun) (function-name fun))
        [acc ancestors] (if fun-name
                          [(accumulate-invocations acc fun-name ancestors)
                           (conj ancestors fun-name)]
                          [acc ancestors])
        reporters (cond-> (filter reporter? (rest expression))
                    source (conj source))]
    (accumulate-profile acc seen reporters ancestors)))

(defn accumulate-mutable-reporter-profile
  "Accumulate one mutable-manager reporter into the profile
  information, given its data."
  [acc seen data ancestors]
  (let [application (:application data)
        fun (first application)
        fun-name (when (instance? clojure.lang.Fn fun) (function-name fun))]
    [(cond-> acc
       fun-name (accumulate-invocations fun-name ancestors))
     seen]))

(defn accumulate-reporter-profile
  "Accumulate one reporter into the profile. Return the profile, 
  set of reporters seen, and a seq of [reporter ancestors] pairs
  that still need to be processed."
  [acc seen reporter ancestors]
  (if (seen reporter)
    [acc seen]
    (let [seen (conj seen reporter)
          data (data reporter)]
      (cond
        (:expression data)
        (accumulate-expression-reporter-profile acc seen data ancestors)
        (:application data)
        (accumulate-mutable-reporter-profile acc seen data ancestors)
        true
        [acc seen]))))

(defn accumulate-profile
  "Accumulate profile information on reporters, returning the profile
  and the set of reporters seen."
  [acc seen reporters ancestors]
  (reduce (fn [[acc seen] reporter]
            (accumulate-reporter-profile acc seen reporter ancestors))
          [acc seen] reporters))

(defn reporters-profile
  "Return profile information on reporters. The profile is a map of
  maps of counts: f -> f -> n, from name of function to name of
  function necessary to evaluate expressions headed by the first
  funtion to how often that happens. The first  function name can also
  be nil, in which case the count is just the number of invocations of
  the second function.

  Notice that unlike a typical profile, which notes the functions
  called by a function, this notes the function calls necessary to
  evaluate all applications headed by a function. That is what is
  recorded in the reporter tree. Since the tree doesn't include
  intermediate function calls that weren't reporter expressions, its
  picture of the reporters created by a function call would be quite
  hard to interpret, as many intermediate causes would be missing."
  [reporters]
  (first (accumulate-profile {} #{} reporters #{})))

(defn print-profile
  "Print a summary of a profile, showing only the max-fns most
  important functions, and, for each of them, only the most important
  max-descendants."
  [profile max-fns max-descendants]
  (let [calls (profile nil)
        num-calls (apply + (vals calls))
        top-subsidiaries (->> (seq profile)
                              (filter #(not (nil? (first %))))
                              (map (fn [[fun counts]]
                                     [fun (apply + (vals counts))]))
                              (sort-by #(+ (get calls (first %) 0) (second %)))
                              reverse
                              (take max-fns))]
    (println "Total calls:" num-calls)
    (doseq [[fun below-count] top-subsidiaries]
      (println fun "calls:" (calls fun) " has-below:" below-count)
      (let [top-descendants (->> (seq (profile fun))
                                 (sort-by second)
                                 reverse
                                 (take max-descendants))]
        (doseq [[fun count] top-descendants]
          (println "  " fun "called:" count))))))

(defn profile-and-print-reporters
  ([reporters] (profile-and-print-reporters reporters 10 10))
  ([reporters max-fns max-descendants]
   (print-profile (reporters-profile reporters) max-fns max-descendants)))

(defn set-and-propagate
  "Support function for let-mutated. Given a function of n items
  and a list of n entities in list form, create an empty mutable
  store, determine the ids the entities will get in the new store,
  call the function with entities correspondin to those ids, evaluate
  the resulting reporter, then add the entities to the store, and
  recompute the reporter, returning its value."
  [fun entities]
  (let [s (store/new-element-store)
        ms (store/new-mutable-store s)
        md (expression-manager/new-expression-manager-data)
        [_ ids] (reduce
                 (fn [[store ids] entity]
                   (let [[new-store new-id] (cosheet.store-utils/add-entity
                                             store nil entity)]
                     [new-store (conj ids new-id)]))
                 [s []] entities)
        mutable-entities (map #(cosheet.entity/description->entity % ms) ids)
        result (apply fun mutable-entities)]
    (expression-manager/request result md)
    (expression-manager/compute md)
    (doseq [entity entities]
      (cosheet.store-utils/add-entity! ms nil entity))
    (expression-manager/compute md)
    (value result)))

;;; A macro to test propagation of changes of an entity through an
;;; expression. Set up the values of the bindings to be entities that
;;; are currently empty, but that will equal the specified entity.
;;; Evaluate exp with a current value of that entity being empty, then
;;; set the entity in the mutable store, recompute, and return the new
;;; current value of the expression.
(defmacro let-mutated [bindings exp]
  (let [binding-pairs (partition 2 bindings)
        vars (vec (map first binding-pairs))
        entities (vec (map second binding-pairs))]
    `(set-and-propagate (fn ~vars ~exp) ~entities)))

;;; A macro to test propagation of changes through a store. Set up var
;;; to a mutable store, initialized from the initial immutable store.
;;; Evaluate exp with the store in that state. Then call the mutator
;;; with the mutable store as an argument, and return the new current
;;; value of the expression.
(defmacro let-mutated-store [[var initial mutator] exp]
  `(let [~var (store/new-mutable-store ~initial)
         md# (expression-manager/new-expression-manager-data)
         exp-val# ~exp]
     (expression-manager/request exp-val# md#)
     (expression-manager/compute md#)    
     (~mutator ~var)
     (expression-manager/compute md#)
     (value exp-val#)))

(defn envs-to-list [envs]
  "Given a vector of environments, as returned by a query, turn it into maps
   of the current value of the environments."
  (seq (for [env envs]
         (zipmap (keys env)
                 (map #(current-value (entity/to-list %)) (vals env))))))


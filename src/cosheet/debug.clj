(ns cosheet.debug
  (:require [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            (cosheet [utils :refer [parse-string-as-number]]
                     [store :as store]
                     [entity :as entity]
                     [query :as query]
                     [reporters :refer [reporter? attended? data value
                                        valid? set-attendee!]]
                     [orderable]
                     [expression-manager :refer [current-value]]
                     [entity-impl :as entity-impl]
                     store-utils
                     store-impl
                     mutable-store-impl
                     [mutable-map :as mm])
            (cosheet.server [order-utils :refer [order-items-R]])))

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
          computation (or (:expression data) (:application data))]
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
        (instance? cosheet.orderable.Orderable item)
        "Orderable"
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

(defn- unpack-if-trivial-nested [item]
  (cond (and (sequential? item)
             (= (count item) 1))
        (first item)
        (and (sequential? item)
           (= (count item) 2)
           (= (first item) (second item)))
        (unpack-if-trivial-nested (first item))
        true             
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
                result (or (:value-source data)
                           (let [v (:value data)] (when (valid? v) v))
                           ((fn [[f & args]] (apply f args)) values))
                trace (trace-current result)
                simplified-trace (if (= (first trace) (second trace))
                                   (vec (rest trace))
                                   trace)]
            (vec (cons (first simplified-trace) 
                       (cons (cons :expr simplified-parts)
                             (rest simplified-trace)))))
          (let [application (:application data)]
            (if application
              (let [parts (map trace-current application)
                    simplified-parts (map unpack-if-trivial-nested parts)]
                (vec (cons (:value data)
                           [(cons :application simplified-parts)])))
              [(:value data)]))))      
      [expr]))

(defn pst [item]
  (pprint (simplify-for-print (trace-current item))))

(defn generate-backtrace
  "Print a stack of requestors of the given reporter."
  [reporter]
  (when (reporter? reporter)
    (let [data (data reporter)
          expr (or (:expression data) (:application data))
          attendees (:attendees data)
          requestor (when attendees
                      (first (mapcat (fn [key]
                                       (cond (reporter? key)
                                             [key]
                                             (sequential? key)
                                             (filter reporter? key)))
                                     (keys attendees))))
          rest (if requestor (generate-backtrace requestor) [])]
      (if expr (cons expr rest) rest))))

(defn print-backtrace [reporter]
  (doseq [r (generate-backtrace reporter)]
    (println r)))

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
        args (filter reporter? (rest expression))
        [acc seen] (accumulate-profile acc seen args ancestors)
        acc (cond-> acc
              fun-name (accumulate-invocations fun-name ancestors))]
    (if source
      (accumulate-profile acc seen [source]
                          (cond-> ancestors fun-name (conj fun-name)))
      [acc seen])))

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
  function heading expressions called by that function. The first 
  function name can also be nil, in which case the count is just
  the number of invocations of the second function.

  Notice that unlike a typical profile, which notes the functions
  called directly by a function, this notes the function calls
  anywhere underneath a function. That is what is
  recorded in the reporter tree. It also only records functions recorded
  in reporters, not all intermediate functions."
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

(defn envs-to-list [envs]
  "Given a vector of environments, as returned by a query, turn it into maps
   of the current value of the environments."
  (seq (for [env envs]
         (zipmap (keys env)
                 (map #(current-value (entity/to-list %)) (vals env))))))

;;; Showing items in a file.

(defn name-to-path [name]
  (let [homedir (System/getProperty "user.home")]
    (clojure.string/join "" [homedir  "/cosheet/" name ".cst"])))

(defn read-store-file [name]
  (with-open [stream (clojure.java.io/input-stream (name-to-path name))]
    (store/read-store (store/new-element-store) stream)))

(defn to-limited-list [entity depth width]
  (if (entity/atom? entity)
    (entity/atomic-value entity)
    (let [content (entity/content entity)
          elements (entity/elements entity)]
      (if (empty? elements)
        content
        (if (= depth 0)
          (cons content '("..."))
          (let [elements (take width (try (order-items-R elements)
                                          (catch Exception e elements)))]
            (cons content
                  (map #(to-limited-list % (- depth 1) width) elements))))))))

(def show-state
  (atom {:store nil
         :name nil
         :depth 3
         :width 10}))

(defn show [pattern & {:keys [store name depth width]
                       :or {store (:store @show-state)
                            name (:name @show-state)
                            depth (:depth @show-state)
                            width (:width @show-state)}
                       :as state}]
  (reset! show-state {:store store
                      :name name
                      :depth depth
                      :width width})
  (let [store (or store (read-store-file name))
        results (vec (query/matching-items pattern store))
        lists (vec (map #(-> %
                             (to-limited-list depth width)
                             simplify-for-print)
                        (take width results)))]
    (clojure.pprint/pprint lists)))


(ns cosheet.debug
  (:require [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            (cosheet [utils :refer [parse-string-as-number]]
                      [store :as store]
                      store-impl
                      [entity :as entity]
                      [entity-impl :as entity-impl]
                      [query :as query]
                      [reporter :refer [reporter? attended?
                                        reporter-data reporter-value-or-invalid
                                        reporter-valid?]]
                      [orderable]
                      [task-queue :refer [make-priority-task-queue
                                          current-tasks]]
                      [calculator :refer [current-value computation-value
                                          make-calculator-data]]
                      [mutable-map :as mm])))

(defn simplified-function-name
  "Return a simplified name of the function, getting rid of uniquifying
  numbers and unnecessary package names."
  [f]
  (let [name (str f)
        matches (re-matches #"(.+?)(?:__\d+)?@.+" name)]
    (symbol (if matches
              (let [name (clojure.string/replace (matches 1) #"(__\d+)" "")
                    matches (re-matches #"(clojure.*)\$(.*)" name)]
                (if matches
                  (if (number? (parse-string-as-number (matches 2)))
                    (matches 1)
                    (matches 2))
                  (let [matches (re-matches #"cosheet\.(.*)" name)]
                    (-> (if matches (matches 1) name)
                        (clojure.string/replace "_QMARK_" "?")
                        (clojure.string/replace "_" "-")
                        (clojure.string/replace "$" "/"))))) 
              name))))

(defn reporter-computation
  "Return an expression indicating the computation of the reporter,
  recursing down to sub-expressions."
  [expr]
  (if (reporter? expr)
    (let [data (reporter-data expr)
          application (:application data)
          value-source (:value-source data)]
      (cond
        application (map reporter-computation application)
        value-source (reporter-computation value-source)
        true "Reporter"))
    expr))

(defn store-as-list [store]
  (map
   #(entity/to-tree (entity/id->entity % store))
   (filter #(nil? (store/id->target store %))
           (first (store/candidate-matching-ids store nil)))))

(defn simplify-for-print [item]
  (cond (satisfies? store/Store item)
        (if (store/mutable-store? item)
          (symbol "MutableStore")
          (symbol "Store"))
        (store/item-id? item)
        (symbol (store/item-id-name item))
        (entity/stored-entity? item)
        (symbol (clojure.string/join
                 [(if (entity/element? item)
                    (if (= (entity/orientation item) :target)
                      "Reverse-Element" "Element")
                    (if (entity/uniquely-identified-object? item)
                      "Identified-Object" "Object"))
                  "-" (simplify-for-print (:item-id item))]))
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
        (vector? item)
        (if (and (not (empty? item))
                 (every? #(instance? clojure.lang.Atom %) item))
          (simplify-for-print (mm/current-contents item))
          (vec (map simplify-for-print item)))
        (instance? clojure.lang.Fn item)
        (simplified-function-name item)
        (sequential? item)
        (map simplify-for-print item)
        :else
        item))

(defn print-current-stack
  "Print the current stack."
  []
  (try (throw (Exception. ""))
       (catch Exception e
         (clojure.stacktrace/print-stack-trace e)
         (println "XXXXX"))))

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
   with all intermediate values filled in.  A trace consists of
   a vector of
     the value
     the trace of each part of the expression that was initially evaluated
     if the initial expression returned an expression, the trace of it
     if that expression returned an expression, the trace of it
     ..."
  [expr]
    (if (reporter? expr)
      (let [data (reporter-data expr)
            application (:application data)
            value-source (:value-source data)]
        (cond
          application
          (let [parts (map trace-current application)
                simplified-parts (map unpack-if-trivial-nested parts)
                values (map first parts)
                result (or (:value-source data)
                           ;; If there is no value-source, then the
                           ;; initial value wasn't a reporter, so
                           ;; we don't have to run the application.
                           (let [v (:value data)] (when (reporter-valid? v) v))
                           ((fn [[f & args]] (apply f args)) values))
                trace (trace-current result)
                simplified-trace (if (= (first trace) (second trace))
                                   (vec (rest trace))
                                   trace)]
            (vec (cons (first simplified-trace) 
                       (cons (cons :application simplified-parts)
                             (rest simplified-trace)))))
          value-source
          (vec (cons :copying
                     (trace-current value-source)))
          true
          [(:value data)]))      
      [expr]))

(defn pst [item]
  (pprint (simplify-for-print (trace-current item))))

(defn generate-backtrace
  "Print a stack of requestors of the given reporter.  (Not all
  requestor paths, just one.)"
  [reporter]
  (when (reporter? reporter)
    (let [data (reporter-data reporter)
          expr (:application data)
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

(defn delayed-print-backtrack-from-queue
  "Wait the given number of seconds (default 1), then print the number
   of tasks in the queue, and if there is at least one, print the first
   one. If the task involves a reporter, print the reporter's backtrace
   of requesters."
  ([queue] (delayed-print-backtrack-from-queue queue 1.0))
  ([queue delay]
   (let [millis (* 1000 delay)]
     (future
       (Thread/sleep millis)
       (let [tasks (current-tasks queue)]
         (println ["!!! PENDING TASKS:" (count tasks)])
         (when (seq tasks)
           (let [task (first tasks)]
             (when-let [rep (first (filter reporter? task))]
               (println ["Backtrace for first task:" rep])
               (print-backtrace rep)))))))))


(defn envs-to-list [envs]
  "Given a vector of environments, as returned by a query, turn it into maps
   of the current value of the environments."
  (seq (for [env envs]
         (zipmap (keys env)
                 (map #(current-value (entity/to-tree %)) (vals env))))))

;;; Showing items in a file.

(defn name-to-path [name]
  (let [homedir (System/getProperty "user.home")]
    (clojure.string/join "" [homedir  "/cosheet/" name ".cst"])))

(defn read-store-file [name]
  (with-open [stream (clojure.java.io/input-stream (name-to-path name))]
    (store/read-store (store/new-element-store) stream)))

;;; Make a list form, but only to a limited depth
(defn to-depth-limited-list [entity depth width]
  (if (entity/primitive? entity)
    entity
    (let [content (entity/content entity)
          elements (entity/elements entity)]
      (if (empty? elements)
        content
        (if (= depth 0)
          (cons content '("..."))
          ;; TODO: Once Order is present, try order-items-R on the elements.
          (cons content
                (map #(to-depth-limited-list % (- depth 1) width)
                     elements)))))))

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
                             (to-depth-limited-list depth width)
                             simplify-for-print)
                        (take width results)))]
    (clojure.pprint/pprint lists)))


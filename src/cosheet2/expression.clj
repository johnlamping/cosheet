(ns cosheet2.expression
  (:require (cosheet2
             [reporter :refer [new-reporter reporter?]]
             [application-calculator :refer [application-calculator]]
             [cache-calculator :refer [cache-calculator]]
             [category-change-calculator :refer [category-change-calculator]])))

;;; Code for creating expression containing reporters.

;;; Let invalid be imported from here, as well.
(def invalid cosheet2.reporter/invalid)

(defn new-application
  "Takes an application, and optionally a trace thunk, and a calculator,
   and additional arguments, and returns a new expression reporter.
   But if it has the application calculator, and none of the parts
   are reporters, then it just evaluates the expression."
  [application & {:keys [trace calculator]
                 :as args
                  :or {calculator application-calculator value invalid}}]
  ;; Catch some errors that leave no stack trace.
  (assert ((some-fn ifn? reporter?) (first application)))
  (if (or (not= calculator application-calculator)
          (some reporter? application))
    (apply new-reporter
           :application application
           :trace trace
           :calculator calculator
           (apply concat (dissoc args :trace :calculator)))
    (apply (first application) (rest application))))

(defmacro expr
  "Takes a function and a series of arguments, and produces an application
   reporter with a tracing thunk. Extra information can be added as meta
   on the function."
  [& args]
  `(new-application ~(vec args)
                    :trace (fn [thunk#] (thunk#))
                    ~@(apply concat (seq (meta (first args))))))

(defmacro cache
  "Takes a function and a series of arguments, and produces a cache
   reporter with a tracing thunk. Extra information can be added as meta
   on the function."
  [& args]
  `(new-application ~(vec args)
                    :trace (fn [thunk#] (thunk#))
                    :calculator cache-calculator
                    ~@(apply concat (seq (meta (first args))))))

(defn category-change
  "Takes a set of categories and a reporter and returns a reporter with
   the same value, but that only reports changes of the given categories."
  [categories reporter]
  (new-reporter
   :value-source reporter
   :categories categories
   :calculator category-change-calculator))

(defn- symbols
  "Return all the variables in a form."
  [form]
  (cond (symbol? form) #{form}
        (coll? form) (apply clojure.set/union (map symbols form))
        :else #{}))

(defn- split-bindings
  "Given a set of variables and a sequence of bindings,
   find a (possibly empty) prefix of the bindings whose values
   don't depend on any of the input variables or the variables bound
   in the prefix. Return a list of the binding forms, the values,
   and the suffix of remaining bindings."
  [vars bindings]
  (if (empty? bindings)
    [nil nil nil]
    (let [[binding-form value & rest] bindings]
      (if (empty? (clojure.set/intersection vars (symbols value)))
        (let [[binding-forms values suffix]
              (split-bindings (clojure.set/union vars (symbols binding-form))
                              rest)]
          [(cons binding-form binding-forms) (cons value values) suffix])
        [nil nil bindings]))))

(defmacro expr-let
  "A let like construct that turns the body into a function that is called
   with the arguments, all inside an expr."
  [bindings & body]
  (assert (even? (count bindings))
          "Bindings must have an even number of forms")
  (let [[binding-forms values suffix] (split-bindings #{} bindings)]
    `(expr
         (fn ~(symbol (str binding-forms))
           ~(vec binding-forms)
           ~@(if (empty? suffix) body [`(expr-let ~(vec suffix) ~@body)]))
       ~@values)))

;;; TODO: These are eager. Consider adding support for lazy sequences of
;;; reporters. That requires adding a new lazy sequence calculator type
;;; that returns a valid value of a lazy-seq, which when requested
;;; sets the value to invalid, and sets up the computation of the
;;; pieces.

(defmacro expr-seq
  "Given an expression that may evaluate to a sequence of reporters, make
   a reporter whose value is the sequence of corresponding values."
  [& args]
  `(expr-let
       [sequence# ~(list* 'cosheet2.expression/expr args)]
     (when (not (empty? sequence#))
       (new-application (cons vector sequence#)
                       :trace (fn [thunk#] (thunk#))))))

(defn expr-filter
  "Given a function that might return a reporter, and a sequence,
  return a reporter whose value is the subsequence for which the filter
  is truthy."
  [condition items]
  (expr-let [passed (expr-seq map #(expr-let [passes (condition %)]
                                     (if passes % ::fail))
                              items)]
    (filter #(not= % ::fail) passed)))



(ns cosheet.expression
  (:require (cosheet [reporters :as reporter])))

;;; Code for creating expression reporters.

;;; Let invalid be imported from here, as well.
(def invalid reporter/invalid)

(defn new-expression
  "Takes an expression, and optionally a trace thunk, a manager type,
   and additional arguments, and returns a new expression reporter.
   But if the manager type is :eval and none of the parts are reporters,
   then it just evaluates the expression."
  [expression & {:keys [trace manager-type]
                 :as args
                 :or {manager-type :eval value invalid}}]
  ;; Avoid some errors that leave no stack trace.
  (assert ((some-fn ifn? reporter/reporter?) (first expression)))
  (if (or (not= manager-type :eval) (some reporter/reporter? expression))
    (apply reporter/new-reporter
           :expression expression
           :trace trace
           :manager-type manager-type
           (apply concat (dissoc args :trace :manager-type)))
    (apply (first expression) (rest expression))))

(defmacro expr
  "Takes a function and a series of arguments, and produces an eval
   reporter with a tracing thunk. Extra information can be added as meta
   on the function."
  [& args]
  `(new-expression ~(vec args)
                  :trace (fn [thunk#] (thunk#))
                  ~@(apply concat (seq (meta (first args))))))

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

;;; TODO: This is eager. Consider adding support for lazy sequences of
;;; reporters. That requires adding a new lazy sequence  manager type
;;; that returns a valid value of a lazy-seq, which when requested
;;; sets the value to invalid, and sets up the computation of the
;;; pieces.
(defmacro expr-seq
  "Given an expression that may evaluate to a sequence of reporters, make
   a reporter whose value is the sequence of corresponding values."
  [& args]
  `(expr-let
       [sequence# ~(list* 'cosheet.expression/expr args)]
     (when (not (empty? sequence#))
       (new-expression (cons vector sequence#)
                       :trace (fn [thunk#] (thunk#))))))

(defmacro cache
  "Takes a function and a series of arguments, and produces a cache
   reporter with a tracing thunk. Extra information can be added as meta
   on the function."
  [& args]
  `(new-expression ~(vec args)
                   :trace (fn [thunk#] (thunk#))
                   :manager-type :cache
                   ~@(apply concat (seq (meta (first args))))))


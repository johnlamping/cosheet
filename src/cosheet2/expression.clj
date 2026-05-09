(ns cosheet2.expression
  (:require (cosheet2
             [reporter :refer [new-reporter reporter? universal-category]]
             [application-calculator :refer [application-calculator]]
             [cache-calculator :refer [data-for-forwarding-reporter]]
             [category-change-calculator :refer [category-change-calculator]])))

;;; Code for creating reporters that contain expressions.

(defn new-application
  "Takes an application, and optionally a trace thunk, and a calculator,
  and additional arguments, and returns a new expression reporter.
  But if it has the application calculator, and none of the parts
  are reporters, then it just evaluates the expression.
  The trace thunk should be a function that calls its one argument. It
  should be created at the point in the code where an application is
  generated. It will be placed on the stack by
  calculator/current-value, so that the stack backtrace will contain a
  record of where applications were created. Without the trace, stack
  will just contain a bunch of recursive calls to current-value."
  [application & {:keys [trace calculator]
                  :as args
                  :or {calculator application-calculator}}]
  ;; Catch some errors that leave no stack trace.
  (assert ((some-fn ifn? reporter?) (first application)))
  (if (and (not (some reporter? application))
           (= calculator application-calculator))
    ;; In this case, none of the arguments are reporters, and we have
    ;; an application calculator, so just run the application now.  No
    ;; need to make a reporter for it.  (Of course, the application
    ;; might return a reporter.)
    (apply (first application) (rest application))
    ;; In this case, either we can't run the application yet, or it
    ;; might have a caching calculator.  If it has a caching
    ;; calculator, we don't want to run the application now, even if
    ;; we could, because we want to cache its computation.  That way,
    ;; if the computation returns an application reporter, all calls
    ;; will return the identical reporter, from the cache, so that
    ;; reporter's computation won't be duplicated either.
    (apply new-reporter
           :application application
           :trace trace
           :calculator calculator
           (apply concat (dissoc args :trace :calculator)))))

(defmacro app-R
  "Takes a function and a series of arguments, and produces an
  application reporter with a tracing thunk. Extra information to be
  recorded in the reporter can be added as meta on the function."
  [& args]
  `(new-application ~(vec args)
                    :trace (fn [thunk#] (thunk#))
                    ~@(apply concat (seq (meta (first args))))))

(defmacro cache-R
  "Takes a function and a series of arguments, and produces a cached
  reporter with a tracing thunk. Extra information to be recorded in
  the reporter can be added as meta on the function."
  [& args]
  `(let [application# ~(vec args)]
     (apply new-application
            application#
            :trace (fn [thunk#] (thunk#))
            ~@(apply concat (seq (meta (first args))))
            (data-for-forwarding-reporter application#))))

(defn category-change
  "Takes a set of categories and a reporter and returns a reporter that
  tracks its value, but only when it has a change in any of the given
  categories; the tracking reporter is only guaranteed to be up to
  date as of the last such change."
  [categories reporter]
  (assert (reporter? reporter))
  (if (or (nil? categories)
          (= categories [universal-category]))
    reporter ; The categories don't make a difference.
    (new-reporter
     :value-source reporter
     :categories categories
     :calculator category-change-calculator)))

(defn- symbols
  "Return all the variables in a form."
  [form]
  (cond (symbol? form) #{form}
        (coll? form) (apply clojure.set/union (map symbols form))
        :else #{}))

(defn- split-bindings
  "Given a set of variables and a sequence of bindings given as
  [binding_form value binding_form value ...], find a (possibly empty)
  prefix of the bindings whose values don't depend on any of the input
  variables or on any of the variables bound in the prefix. Return a
  list of the binding forms, the values, and the suffix of remaining
  bindings."
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

(defmacro let-R
  "A let like construct that turns the body into a function that is called
   with the arguments, all inside an app-R."
  [bindings & body]
  (assert (even? (count bindings))
          "Bindings must have an even number of forms")
  ;; We can't necessarily evaluate all the values at once, because
  ;; later ones might depend on earlier ones. Instead, we evaluate
  ;; values that don't depend on earlier ones, and then use an inner
  ;; let-R to handle any remaining ones.
  (let [[binding-forms values suffix] (split-bindings #{} bindings)]
    `(app-R
         (fn ~(symbol (str binding-forms)) ; a name for the function.
           ~(vec binding-forms)
           ~@(if (empty? suffix) body [`(let-R ~(vec suffix) ~@body)]))
         ~@values)))

;;; TODO: These are eager. Consider adding support for lazy sequences
;;; of reporters. That requires adding a lazy cons operation, which
;;; just takes two possible reporters, doesn't give them demand, and
;;; can return either one. Whenevethe lazy cons is accessed, it has to
;;; be done as one of the arguments to an app-R, so the app-R can deal
;;; with giving the reporter demand, and waiting for its value. This
;;; means that operations over the sequences, like map or filter,
;;; would need versions that include those app-R forms.

(defmacro seq-R
  "Given an argument that is a sequence or may a reporter and that
  returns a sequence, and where that sequence may contain reporters,
  make a reporter whose value is the sequence of corresponding
  values."
  [sequence]
  `(let-R [sequence# ~sequence]
     (when (not (empty? sequence#))
       (new-application (cons vector sequence#)
                        :trace (fn [thunk#] (thunk#))))))

(defmacro app-seq-R
  "Given an expression that may evaluate to a sequence of reporters, make
   a reporter whose value is the sequence of corresponding values."
  [& args]
  `(let-R
       [sequence# ~(list* 'cosheet2.expression/app-R args)]
     (when (not (empty? sequence#))
       (new-application (cons vector sequence#)
                        :trace (fn [thunk#] (thunk#))))))




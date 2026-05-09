(ns cosheet2.expression
  (:require (cosheet2
             [reporter :refer [new-reporter reporter? universal-category]]
             [application-calculator :refer [make-application-R]]
             [cache-calculator :refer [data-for-forwarding-reporter]]
             [category-change-calculator :refer [category-change-calculator]])))

;;; Convenient syntax for accepting and creating reporters, mostly macros.

(defmacro app-R
  "Takes a function and a series of arguments, and produces an
  application reporter with a tracing thunk. Extra information to be
  recorded in the reporter can be added as meta on the function."
  [& args]
  `(make-application-R ~(vec args)
                       :trace (fn [thunk#] (thunk#))
                       ~@(apply concat (seq (meta (first args))))))

(defmacro cache-R
  "Takes a function and a series of arguments, and produces a cached
  reporter with a tracing thunk. Extra information to be recorded in
  the reporter can be added as meta on the function."
  [& args]
  `(let [application# ~(vec args)]
     (apply make-application-R
            application#
            :trace (fn [thunk#] (thunk#))
            ~@(apply concat (seq (meta (first args))))
            (data-for-forwarding-reporter application#))))

;;; TODO: Move this to category-change-calculator, so we have only macros.
(defn category-change
  "Takes a set of categories and a reporter and returns a reporter that
  tracks the input reporter's value, but only when it has a change in
  any of the given categories; the tracking reporter is only
  guaranteed to be up to date as of the last such change."
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

;;; TODO: This are eager. Consider adding support for lazy sequences
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
       (make-application-R (cons vector sequence#)
                           :trace (fn [thunk#] (thunk#))))))

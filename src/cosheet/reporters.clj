(ns cosheet.reporters
  (:require (cosheet [utils :refer [dissoc-in
                                    swap-returning-both!
                                    call-with-latest-value]])))

(defn- check-callback [callback]
  (assert (fn? (first callback)) "Callback is not a function.")
  callback)

(defn- call-callback [callback & args]
  (apply (first callback) (concat args (rest callback))))

(def invalid ::invalid)

(defn valid? [value]
  (not= value invalid))

(defrecord ReporterImpl
    ^{:doc
      "A reporter object (an atom with a map of relevant information).
       We use a record only so we can have a special print method that
       can avoid printing circular references.
       A reporter monitors a computation, and records its current value, or
       the special token ::invalid.
       One or more callbacks can attend to the value of the reporter. Each
       callback is associated with a key, and optionally some additional
       arguments. Each attendee is guaranteed to eventually be called with
       the key, the reporter, and the attendee's additional arguments. It will
       eventually be similarly called after any change in the value, but not
       necessarily once per change.
       In addition to these generic functions, specific kinds of reporters will
       have additional information to support additional functionality.
       A manager callback can be registered with the reporter, to implement
       that functionality. It will be called the first time there are any
       attendees to the reporter, and whenever there is a transition in
       whether there are attendees. It can do things
       like registering for callbacks to update its state, or removing those
       callbacks when there is no more interest."}
  [;;; An atom holding a map including :value, and possibly :manager,
   ;;; and :attendees, as well as other keys.
   data]
  )

(defn reporter? [r]
  (instance? ReporterImpl r))

(defn value
  "Return the current value of the reporter. If it is not a reporter, treat
   it as a constant reporter, and return the object."
  [r]
  (if (reporter? r)
    (:value @(:data r))
    r))

(defn data-atom
  "Return the atom holding the reporter's data. If any of the standard fields
   are changed, the appropriate notifications must be done."
  [reporter]
  (:data reporter))

(defn data
  "Return all the current data for the reporter."
  [reporter]
  @(:data reporter))

(defn data-attended? [data]
  (not (empty? (:attendees data))))

(defn attended? [reporter]
  (data-attended? @(:data reporter)))

(defn inform-attendees
  "Notify the attendees that the value may have changed."
  [reporter]
  ;;; Since the only guarantee is eventual callback, we can fetch the
  ;;; attendees map outside of any lock, since anything that changed
  ;;; the attendees will also do callbacks if appropriate.
  (doseq [[key callback] (:attendees @(:data reporter))]
    (call-callback callback key reporter)))

(defn set-value!
  "Set the value of the reporter, informing any attendees."
  [reporter value]
  (let [[old current]
        (swap-returning-both! (:data reporter) #(assoc % :value value))]
    (if (not= (:value old) (:value current))
      (inform-attendees reporter))))

(defn set-manager!
  "Set the manager for the reporter.
   Once set, the manager can never be changed."
  [reporter & manager]
  (check-callback manager)
  (let [[old current]
        (swap-returning-both! (:data reporter) assoc :manager manager)]
    (let [old-manager (:manager old)]
      (when old-manager
        (assert (= old-manager manager)
                (format "Cannot change manager: expression %s."
                        (:expression old)))))
    (when (data-attended? current)
      (call-callback manager reporter))))

(defn set-attendee!
  "Add an attending callback to a reporter, under a key that must be unique
   to each callback. If no callback is provided, remove any callback that is
   present. If a callback is provided, call it."
  [r key & callback]
  (if (reporter? r)
    (let [[old current]
          (swap-returning-both!
           (:data r)
           (fn [data]
             (if (nil? callback)
               (dissoc-in data [:attendees key])
               (assoc-in data [:attendees key] (check-callback callback)))))]
      (when callback
        (call-callback callback key r))
      (when (and (:manager current)
                 (not= (data-attended? old) (data-attended? current)))
        (call-callback (:manager current) r)))
    (when callback
      (call-callback callback key r))))

(defn new-reporter
  [& {[key & callback] :attendee manager :manager :as args}]
  (let [args (merge {:value invalid} args)]
    (let [reporter (->ReporterImpl
                    (atom (dissoc args :attendee :manager)))]
      (when manager
        (apply set-manager! reporter
               (check-callback (if (sequential? manager) manager [manager]))))
      (when key
        (apply set-attendee! reporter key callback))
      reporter)))

(defmethod print-method ReporterImpl [s ^java.io.Writer w]
  (.write w "<Reporter")
  (.write w (str (dissoc @(:data s) :attendees :manager)))
  (.write w ">"))

(defn new-expression
  "Takes a expression, and optionally a trace thunk, a manager type,
   and additional arguments, and returns a new expression reporter.
   But if the manager type is :eval and none of the parts are reporters,
   then it just evaluates the expression."
  [expression & {:keys [trace manager-type]
                 :as args
                 :or {manager-type :eval value invalid}}]
  (if (or (not= manager-type :eval) (some reporter? expression))
    (apply new-reporter
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
  [vars, bindings]
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
  `(expr-let [sequence# ~(list* 'expr args)]
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


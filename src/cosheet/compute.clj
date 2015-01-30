(ns cosheet.compute
  (:require [cosheet.state :refer :all]))

(defprotocol Notifier
  "Something that can return the value or State objects for expressions."
  (notifier-value [this expression]
    "Return the current  value of the expression.")
  (notifier-state [this expression]
    "Return the state (or just value if immutable) of the expression"))

(defprotocol Scheduler
  "A notifier that schedules evaluations, as well as tracking
   dependencies. It requires its expressions to be a list of a form
   followed by arguments, and requires the form to be a Clojure
   function that knows how to compute the value of the expression in
   the context of the notifier, given the unevaluated arguments.
   To evaluate an expression, the scheduler calls the form of the
   expression, with the rest of the expression as arguments, as well as
   possibly other information. That function can return either a value,
   a State, if the value may change later, or a eval-and-call to request
   the scheduler to do further evaluations.
   Thus, all the information on how to evaluate is held by the functions
   in the expressions; the scheduler just coordinates everything,
   including state changes."
  (request [this expression]
    "Request the given computation.")
  (ready? [this expression]
    "True if the value of the computation is available."))

(defn make-eval-and-call
  "Make an eval-and call form, with the given function and arguments.
   trace must be (fn [thunk] (thunk)), and is called by the scheduler
   as part of evaluating its arguments to leave a strack trace with a line
   number corresponding to the original code."
  [trace f & args]
  (apply list :eval-and-call trace f args))

(defmacro eval-and-call
  "Takes a function and a series of arguments, and produces a result
   that when run under a scheduler requests evaluation of the
   arguments. Any argument that evaluates to a vector is interpreted as
   an expression, indicating a call of a function with arguments, that
   also needs to be run under the scheduler. Once any necessary
   subsidiary evaluations are done, the function is called with the
   values, and its result returned. The new result may be another
   eval-and-call, in which case the process repeats.
   If the scheduler is an approximating scheduler, an argument may be
   the special indication (:monotonic expression). This indicate that
   the result is a monotonic function of the value of the expression,
   so that if only lower bounds for the arguments are available, a
   lower bound for the result can still be computed. If the expression
   has a :lower-bound meta value, the scheduler can use that as the
   starting point for approximations. The scheduler knows nothing
   about the ordering of values in the monotonic hierarchy; it is up
   to the functions to make sure that the ordering is consistent
   between an expression and its users."
  [f & args]
  ;; TODO: make this a macro that creates a function that gets called
  ;; by the scheduler to then call the wrapped function, so that the
  ;; stack track shows where control is. It gets called with a thunk,
  ;; which it calls, and causes evaluation of the arguments.
  `(make-eval-and-call (fn [thunk#] (thunk#)) ~f ~@args))

(defmacro eval-let
  "A let like construct that has the scheduler evaluate the bindings.
   It expands to eval-and-call forms."
  [bindings & body]
  (assert (even? (count bindings))
          "Bindings must have an even number of forms")
  (if (nil? bindings)
    `(do ~@body)
    (let [var (first bindings)
          expr (second bindings)
          rest (nnext bindings)]
      `(eval-and-call (fn [~var] (eval-let ~rest ~@body))
                      ;; TODO: This if shouldn't be necessary, but
                      ;; there is a bug in query_impl without it.
                      ~(if (sequential? expr) (vec expr) expr)))))

;;; TODO: This is eager. Consider adding support for lazy sequences of
;;; expressions. The challenge is that using them would require
;;; special forms, since evaluations of expressions in the sequence
;;; can't be done inline, but requires returning an eval-and-call.
;;; TODO: Make this into a macro that leaves a proper track
(defn eval-map
  "Return a request to compute a seq of the evaluation
   of the function on each element of the sequence."
  [f sequence]
  (apply make-eval-and-call (fn [thunk] (thunk)) vector
         (map (fn [elem] [f elem]) sequence)))

;;; Factory method for ApproximatingScheduler
(defmulti new-approximating-scheduler
  (constantly true))

(defmulti current-value
  "Run computation on the expression, which may use the Scheduler protocol
   for its return values, returning the current value,
   rather than tracking dependencies."
  (constantly true))


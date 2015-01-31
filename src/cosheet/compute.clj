(ns cosheet.compute
  (:require [cosheet.state :refer :all]))

(defprotocol Notifier
  "Something that can return the value or State objects for references."
  (notifier-value [this reference]
    "Return the current  value of the reference.")
  (notifier-state [this reference]
    "Return the state (or just value if immutable) of the reference"))

(defprotocol Scheduler
  "A notifier that schedules evaluations of references, as well as
   tracking dependencies. References must be a list of a function
   followed by arguments, where the function knows how to compute the
   value of the reference in the context of the notifier, given the
   arguments. To evaluate a reference, the scheduler calls the form of
   the reference, with the rest of the reference as arguments, as well
   as possibly other information. That function can return either a
   value, a State, if the value may change later, or a expression to
   request the scheduler to do further evaluations.
   Thus, all the information on how to evaluate is held by the functions
   in the references; the scheduler just coordinates everything,
   including state changes."
  (request [this reference]
    "Request the given computation.")
  (ready? [this reference]
    "True if the value of the computation is available."))

(defn make-expression
  "Make an eval-and call form, with the given function and arguments.
   trace must be (fn [thunk] (thunk)), and may be called by the scheduler
   as part of evaluating its arguments to leave a strack trace with a line
   number corresponding to the original code."
  [trace f & args]
  (apply list :expression trace f args))

(defmacro expr
  "Takes a function and a series of arguments, and produces a expression
   that when run under a scheduler requests evaluation of the
   arguments. Any argument can be another expression, that
   also needs to be run under the scheduler. Once any necessary
   subsidiary evaluations are done, the reslting function and arguments
   is a function is a reference, whose value is the value of the expression.
   If the scheduler is an approximating scheduler, an argument may be
   the special indication (:monotonic reference). This indicate that
   the result is a monotonic function of the value of the reference,
   so that if only lower bounds for the arguments are available, a
   lower bound for the result can still be computed. If the reference
   has a :lower-bound meta value, the scheduler can use that as the
   starting point for approximations. The scheduler knows nothing
   about the ordering of values in the monotonic hierarchy; it is up
   to the functions to make sure that the ordering is consistent
   between a reference and its users."
  [f & args]
  `(make-expression (fn [thunk#] (thunk#)) ~f ~@args))

(defmacro eval-let
  "A let like construct that has the scheduler evaluate the bindings.
   It expands to expression forms."
  [bindings & body]
  (assert (even? (count bindings))
          "Bindings must have an even number of forms")
  (if (nil? bindings)
    `(do ~@body)
    (let [var (first bindings)
          expr (second bindings)
          rest (nnext bindings)]
      `(expr (fn [~var] (eval-let ~rest ~@body))
                      ;; TODO: This if shouldn't be necessary, but
                      ;; there is a bug in query_impl without it.
                      ~(if (sequential? expr) (vec expr) expr)))))

;;; TODO: This is eager. Consider adding support for lazy sequences of
;;; references. The challenge is that using them would require
;;; special forms, since evaluations of references in the sequence
;;; can't be done inline, but requires returning an expression.
;;; TODO: Make this into a macro that leaves a proper track
(defn eval-map
  "Return a request to compute a seq of the evaluation
   of the function on each element of the sequence."
  [f sequence]
  (apply make-expression (fn [thunk] (thunk)) vector
         (map (fn [elem] [f elem]) sequence)))

;;; Factory method for ApproximatingScheduler
(defmulti new-approximating-scheduler
  (constantly true))

(defmulti current-value
  "Run computation on the reference, which may use the Scheduler protocol
   for its return values, returning the current value,
   rather than tracking dependencies."
  (constantly true))


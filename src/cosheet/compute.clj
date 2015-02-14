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
  "Make an expr form, with the given function and arguments.
   trace must be (fn [thunk] (thunk)), and may be called by the scheduler
   as part of evaluating its arguments to leave a strack trace with a line
   number corresponding to the original code."
  [trace f & args]
  (apply list :expression trace f args))

(defn expression? [expr]
  (and (list? expr) (= (first expr) :expression)))

(defn expression-tracer [expr]
  (second expr))

(defn expression-fn [expr]
  (nth expr 2))

(defn expression-args [expr]
  (seq (nthnext expr 3)))

(defmacro expr
  "Takes a function and a series of arguments, and produces an expression
   that when run under a scheduler requests further evaluation of the
   arguments. The function and arguments are all evaluated in Clojure,
   but any of them can return an expression, that then also gets run
   under the scheduler, or a state, whose value will be used and tracked.
   Once any necessary subsidiary evaluations are
   done, the resulting function and arguments is a reference, whose
   value will become the value of the expression.
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

(defmacro expr-let
  "A let like construct that evaluate the bindings and puts the result under
   control of the scheduler. It expands to expr forms."
  [bindings & body]
  (assert (even? (count bindings))
          "Bindings must have an even number of forms")
  (if (nil? bindings)
    `(do ~@body)
    (let [var (first bindings)
          exp (second bindings)
          rest (nnext bindings)]
      `(expr (fn ~var [~var] (expr-let ~rest ~@body))
             ~exp))))

;;; TODO: This is eager. Consider adding support for lazy sequences of
;;; references. The challenge is that using them would require
;;; special forms, since evaluations of references in the sequence
;;; can't be done inline, but requires returning an expression.
(defmacro expr-map
  "Return an expr which returns a vector, where each element is itself an expr
   of the function applied to an element of the sequence."
  [f sequence]
  `(expr-let [f# ~f
              sequence# ~sequence]
     (apply make-expression (fn [thunk#] (thunk#))
            vector (map (fn [elem#] (expr f# elem#)) sequence#))))

;;; Factory method for ApproximatingScheduler
(defmulti new-approximating-scheduler
  (constantly true))



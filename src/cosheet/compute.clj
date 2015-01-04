(ns cosheet.compute)

(defprotocol State
  "The methods of something that holds a single value that can change."
  (state-value [this]
    "The current value of the state")
  (subscribe [this callback]
    ;; Note: the signature can't be [this callback & args] because
    ;; defprotocols don't support varadic arguments.
    "Returns the current value, or nil if it is currently unknown. If
     the current value changes, the callback will eventually be called.
     It must be a function or a sequence of a function and
     arguments. The function will be passed the new value and the
     additional arguments, if any. The function may not be called for every
     change, but will eventually be called after all changes.")
  (unsubscribe [this callback]
    "Removes the specified subscription."))

(defprotocol Notifier
  "Something that can return the value or State objects for expressions."
  (current-value [this expression]
    "Return the current  value of the expression.")
  (state [this expression]
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
   a State, if the value may change later, or a application to request
   the scheduler to do further evaluations. Thus, all the information
   on how to evaluate is held by the functions in the expressions; the
   scheduler just coordinates everything, including state changes."
  (request [this expression]
    "Request the given computation.")
  (ready? [this expression]
    "True if the value of the computation is available."))

(defmacro application [fn & args]
  "Return value that a function running under a scheduler may use to
   indicate that it wants the scheduler to evaluate some expressions
   and call the provided function with their values. The result of
   that call will be treated as the return value of the original
   function. The new result may be another application, in which case
   the process repeats.
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
  `(list :application ~fn ~@args))

;;; Factory method for ApproximatingScheduler
(defmulti new-approximating-scheduler
  (constantly true))

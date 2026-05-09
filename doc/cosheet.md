The cosheet project uses a custom api called reporter. A reporter is
an atom that holds a current value, which can be changed. Furthermore,
a reporter accepts registrations for notifications when the value
changes. It uses a lock-free interface, and provides only an eventual
consistency guarantee that every change will be followed by some
notification. But that doesn't imply that there will be a notification
for every change, or that a notification will only happen when the
value changed. Furthermore, the value may have been changed between
the time a notification was generated and when it is delivered. But in
that case, a subsequent notification will be generated.

The preferred way to work with reporters is with the macros: 
expr, expr-seq and expr-let.

The expr macro's arguments are the function and values of an
application. The macro returns a reporter whose value will track the
value of the application, but using the current values of any
reporters it mentions, in place of the reporters themselves. This is
the way to let a function that isn't aware of reporters use their
values.

The expr-seq macro adds one additional behavior to the expr macro: if
the value of the reporter it returns would be a sequence of reporters,
it will instead be the sequence of their current values. This is often
used when mapping a function that can return reporters.

The exper-let macro returns a reporter whose value will track the
value of its body, but with any its variables that would be bound to a
reporter bound instead to the reporter's current value. It is
syntactic sugar for the expr macro, in the same way that let is
syntactic sugar for an application.

In unit tests, rather than use (is (= ...)) when comparing large
structures, use (is (check ...)), instead. The check function is found
in test_utils.clj. It also tests for equality, but on a failure, it
returns an explanation of where equality failed.

The execution environment is based on lein.
To run tests, use "lein test" plus any additional needed arguments.
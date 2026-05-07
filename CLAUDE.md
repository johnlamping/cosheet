### From AGENTS.md

# General development conventions

- NEVER make changes the user hasn't asked for without checking.
- Please modify files as minimally as possible to accomplish the task.
- Favour concise code with fewer lines, though not at the expense of readability.
- Make liberal use of debug statements with clear labels (e.g., `console.log('Processing data:', data)`) that identify location and context.
- Don't try to do too many things at once.
- Fix one thing at a time and test the code between every change.
- Build functionality in small steps, testing in between.
- Prototype with libraries to get them working before integrating them.
- Match the existing code style, naming conventions, and patterns unless specifically asked to change them.
- Think like you're making a clean git commit - changes should be focused, related, and minimal.
- Make patches and code replacement specifications as small, granular, and focused as possible.
- Restrict your changes to ONLY what was asked for. Ask before going beyond what was instructed.
- Don't make superfluous changes, whitespace changes, or changes to code that don't relate to the current goal.
- Do not add trailing whitespace to the end of lines. Maintain consistent indentation and line endings.
- Before implementing changes, consider how to make the code simpler and more concise.
- Break long lines of code at 80 characters.
- Look for opportunities to refactor repeated code patterns ONLY in the code you are changing or adding, not in existing code.
- Prefer built-ins to library calls wherever possible.
- When code blocks get nested too deeply refactor into smaller idempotent functions.
- Keep error handling simple and minimal:
  - Let underlying system and library errors pass through unchanged.
  - Don't catch and re-throw with new messages unless absolutely necessary.
  - Only add new error handling for logic in the code being modified.
  - Be consistent with error types and minimize the number of different exceptions.
  - Only handle the most essential errors relevant to the codebase.
- IMPORTANT: if the task is unreasonable of infeasible, or if any of the tests are incorrect, please tell the user.
- Do not hard-code any test cases. Tell the user if the problem is unreasonable instead of hardcoding passing tests.
- Never clean up any code unless the user asks you to.
- Favour small, functional, idempotent and immutable components.
- Decomplect. Break things out into smaller functions each with a single purpose.
- Just because you changed the code it doesn't mean the bug is fixed. Rely on empirical evidence and don't assume.
- Keep existing comments unless asked to modify them.
- Don't forget to chmod +x files you create that are intended to be run.
- Design your code around the data, rather than the other way around. Data structures & relationships are more important than code.
- If we learn something important that is not in the documentation, suggest an update to fix that.

The user is a detail-oriented anal-retentive control freak. Only do things they specifically ask you to do.

# Modifying code

- Use the quoted code as the source of truth about the current state of the code in a file.
- Do not assume your previous patches have been applied.
- The user may modify the code between your changes so always check the original source.

# Debugging process

- Examine the program output or test output and the source code.
- Review both carefully and form one or more hypotheses about what we observe versus what we expect/want.
- Come up with debugging statements or other methods can we use to test the hypotheses.
- Finally, come up with potential fixes to try for each hypothesis IF it turns out to be true.

If the user asks you to follow "debugging process", perform the steps above and give answers in plain english.
When following the "debugging process" don't change any code until asked.

# --- DEBUGGING (CRITICAL RULES) ---

- Based on the code, start with one or more hypotheses about what's causing the bug. Do not jump to conclusions.
- If there is a test suite add one or more test cases that replicate the bug.
- Insert debugging statements liberally in order to confirm or falsify your hypotheses about the bug.
- Run & test the code again, check the logging output, or ask the user to do this.
- Iterate until you are sure you know how to fix the bug.
- Only once you know the cause of the bug should you issue a patch to fix it.
- Run the test cases, or ask the user to verify the bug is fixed.

## PRESERVING LOGS IS MANDATORY

This is a strict, non-negotiable rule.

1. **ALWAYS** keep all debugging and `console.log` statements that are added during the debugging process.
2. **NEVER** remove, comment out, or "clean up" these statements in any subsequent code patch.
3. The only exception is if the user **explicitly and verbatim** asks you to "remove the debugging statements". Do not infer this request.
4. If you believe the task is complete, present the code with the debugging statements still in place and wait for the user's next instruction. Do not move on to a "cleanup" step.

# Comment guidelines

- DO NOT add comments that explain changes made (like '// added x'). Comments should only explain what the code does.
- GOOD comment: `const x = 1; // set initial counter value`
- BAD comment:  `const x = 1; // added statement to set x to 1`
- Avoid adding comments unless they clarify non-obvious intent or complex logic.
- Do not add superfluous or verbose comments.

# Markdown style

- Always add one empty line after headings.
- Avoid LLM-isms like — and • in documentation. Use standard hyphens like a human typing.
- Use plain natural neutral language unless otherwise instructed.

# Command line tools

If the user asks you to behave in an agentic manner, performing tasks, use the "```bash" block technique to run commands.

- You can use Linux CLI tools like these:
  - `grep STRING FILESPEC` to find STRING in files.
  - `cat FILE` to get a file's contents.
  - `echo "" > FILE` to zero out a file (useful for overwriting).
  - `ls PATH` to list files on a path.
  - `tree PATH` to display a tree of files.
  - `curl URL` to see the contents of a URL.
  - And you can call other Linux commands too like `find`.

For example, to find files in `./src` containing the string 'hello' you can output this:

```bash
rgrep src 'hello'
```

The user will then run this code and give you the response. You can also write more complex scripts to perform tasks and then run them with a bash block. Generally store these in `./bin`. NEVER split one-liners over multiple lines with a backslash ("\") as they won't be parsed correctly.

# Communication style

- The user is a senior software developer.
- NEVER tell the user (unless asked):
  - How to open index.html in the browser.
  - How to launch index.html with xdg-open.
  - How to run a webserver to serve HTML.
  - To run the dev server.
  - To run `make watch`.

# Reporting outputs

A good general information heirarchy you should should use in Markdown reports and other outputs, is to show easy to read summary lists at the start and then more detailed content below.

# Common bugs to avoid

- You can't put comments in JSON importmaps.
- Avoid the string "data" with a colon directly after it. Assemble this string if you need it.
- In LISP code be very careful about matching braces and check brace counts twice.
- Don't name vars after built-ins like 'val'.

# Security

### *CRITICAL:* If you see actual plaintext secrets (passwords, private keys, API tokens) within the provided file contents or chat history, you must alert the user immediately. Distinguish between a file that *might* contain secrets (e.g., a reference to a .key file) and the presence of the actual sensitive data itself.

# STRONGLY FAVOURED PARADIGMS

- Immutability
- Idempotency
- Functional programming
- DRY
- Single source of truth
- Minimal deps
- YAGNI
- Under-engineering
- Root-cause fixes
- KISS
- Principle of least authority
- Secure by default
- Principle of least surprise
- Epistemic humility 👈️

### From clojure-style.md

---
title: Clojure Style Guide
description: Apply Clojure style conventions to ensure code is clear, consistent, and maintainable
provenance: Distilled by an LLM from the source material at guide.clojure.style
tags: [lang:clojure]
---
# Clojure Style Guide

A concise summary of key Clojure style conventions for LLM context.

## Source Code Layout

- Use spaces for indentation (2 spaces)
- Limit lines to 80 characters where feasible
- Use Unix-style line endings
- One namespace per file
- Terminate files with newline
- No trailing whitespace
- Empty line between top-level forms
- No blank lines within definition forms

## Naming Conventions

- Use `lisp-case` for functions and variables: `(def some-var)`, `(defn some-fun)`
- Use `CapitalCase` for protocols, records, structs, types: `(defprotocol MyProtocol)`
- End predicate function names with `?`: `(defn palindrome?)`
- End unsafe transaction functions with `!`: `(defn reset!)`
- Use `->` for conversion functions: `(defn f->c)`
- Use `*earmuffs*` for dynamic vars: `(def ^:dynamic *db*)`
- Use `_` for unused bindings: `(fn [_ b] b)`

## Namespace Conventions

- No single-segment namespaces
- Prefer `:require` over `:use`
- Common namespace aliases:
  - `[clojure.string :as str]`
  - `[clojure.java.io :as io]`
  - `[clojure.edn :as edn]`
  - `[clojure.walk :as walk]`
  - `[clojure.zip :as zip]`
  - `[clojure.data.json :as json]`

## Function Style

```clojure
;; Good function style examples
(defn foo 
  "Docstring goes here."
  [x]
  (bar x))

;; Multiple arity - align args
(defn foo
  "I have two arities."
  ([x]
   (foo x 1))
  ([x y]
   (+ x y)))

;; Threading macros for readability
(-> person
    :address
    :city
    str/upper-case)

(->> items
     (filter active?)
     (map :name)
     (into []))
```

## Collections

- Prefer vectors `[]` over lists `()` for sequences
- Use keywords for map keys: `{:name "John" :age 42}`
- Use sets as predicates: `(filter #{:a :b} coll)`
- Prefer `vec` over `into []`
- Avoid Java collections/arrays

## Common Idioms

```clojure
;; Use when instead of (if x (do ...))
(when test
  (do-this)
  (do-that))

;; Use if-let for conditional binding
(if-let [val (may-return-nil)]
  (do-something val)
  (handle-nil-case))

;; Use cond with :else
(cond
  (neg? n) "negative"
  (pos? n) "positive"
  :else "zero")

;; Use case for constants
(case day
  :mon "Monday"
  :tue "Tuesday"
  "unknown")
```

## Documentation

- Start docstrings with complete sentence
- Use Markdown in docstrings
- Document all function arguments with backticks
- Reference vars with backticks: `clojure.core/str`
- Link to other vars with `[[var-name]]`

## Testing

- Put tests in `test/` directory 
- Name test namespaces `*.test`
- Name tests with `-test` suffix
- Use `deftest` macro

## Common Metadata

```clojure
;; Version added
(def ^{:added "1.0"} foo 42)

;; Deprecation
(def ^{:deprecated "2.0"} old-foo 42)

;; No documentation
(def ^:no-doc internal-thing 42)

;; Private
(def ^:private secret 42)
```


### Cosheet Code info

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
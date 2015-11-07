(ns cosheet.utils)

;;; Simple multiset operations.

(defn multiset-conj
  "Add an item to a multiset,
   represented as a map from items to multiplicities."
  [ms item]
  (update-in ms [item] #((fnil + 0) % 1)))

(defn multiset
  "Turn a seq into a multiset."
  [items]
  (reduce multiset-conj {} items))

(defn update-last
  "Update the last element of a vector."
  [vec fun]
  (if (empty? vec)
    [(fun nil)]
    (update-in vec [(dec (count vec))] fun)))

;;; Utilities for making clean maps.

(defn dissoc-in
  "Remove (get-in map keys), and if that creates an empty map one
  level up, keep removing."
  [map keys]
  (if (empty? keys)
    nil
    (let [key (first keys)
          lower (dissoc-in (get map key) (rest keys))]
      (if (and (empty? lower) (not (record? map)))
        ;; We don't want to dissoc in a record, because that turns it
        ;; into a map.
        (dissoc map key)
        (assoc map key lower)))))

(defn update-in-clean-up
  "Like update-in, but removes empty collections."
  [map keys fn]
  (let [result (fn (get-in map keys))]
      (if (or (nil? result) (and (coll? result) (empty? result)))
        (dissoc-in map keys)
        (assoc-in map keys result))))

;; Utils for working with atoms in a lock free way.

(defn swap-returning-both!
  "Swap, and return a vector of the old and new values"
  [cell f & args]
  (loop []
    (let [old @cell
          new (apply f old args)]
      (if (compare-and-set! cell old new)
        [old new]
        (recur)))))

(defn swap-control-return!
  "Like swap!, except f should return two values:
   a new value for the atom and the value to return from the swap."
  [cell f & args]
  (loop []
    (let [old @cell
          [new return-value] (apply f old args)]
      (if (compare-and-set! cell old new)
        return-value
        (recur)))))

(defn call-with-latest-value
  "Call the function with the current value of the thunk,
   and the other arguments, repeating until the value of the thunk after the
   call  matches the value used in the call. This is good for registering a
   current state with some other place that needs to track it."
  [thunk f & args]
  (loop [value (thunk)]
    (apply f value args)
    (let [latest-value (thunk)]
      (when (not= value latest-value) (recur latest-value)))))

(defmacro with-latest-value
  "Run body with the current value of expression,
  repeating until the value of the expression after it has been run
  is the same as it was before."
  [[var expression] & body]
  `(call-with-latest-value
    (fn [] ~expression)
    (fn [~var] ~@body)))


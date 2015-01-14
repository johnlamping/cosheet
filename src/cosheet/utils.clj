(ns cosheet.utils)

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

(defn swap-returning-both! [cell f & args]
  (loop []
    (let [old @cell
          new (apply f old args)]
      (if (compare-and-set! cell old new)
        [old new]
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


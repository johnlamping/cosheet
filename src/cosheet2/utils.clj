(ns cosheet2.utils
  (:require clojure.set))

;;; A pseudo-set is an implementation of a set that is much more
;;; efficient when the set has no items or just one item. It is either
;;; nil, a non-nil atom, or a set, representing either the empty set,
;;; a singleton item, or a set with multiple items.

(defn pseudo-set-seq [pseudo-set]
  (cond (nil? pseudo-set)
        nil
        (set? pseudo-set)
        (seq pseudo-set)
        true
        (seq [pseudo-set])))

(defn pseudo-set-contains? [pseudo-set item]
  (cond (nil? pseudo-set)
        false
        (set? pseudo-set)
        (contains? pseudo-set item)
        true
        (= pseudo-set item)))

(defn pseudo-set-conj [pseudo-set item]
  (cond (or (nil? pseudo-set) (= pseudo-set item))
        item
        (set? pseudo-set)
        (conj pseudo-set item)
        true
        #{pseudo-set item}))

(defn pseudo-set-disj [pseudo-set item]
  (cond (nil? pseudo-set)
        nil
        (set? pseudo-set)
        (let [result (disj pseudo-set item)]
          (cond (empty? result)
                nil
                (= (count result) 1)
                (first result)
                true
                result))
        (= item pseudo-set)
        nil
        true
        pseudo-set))

(defn pseudo-set-set-membership
  "Ensure the item is in or out of the pseudo-set, based on the value
  of member?"
  [pseudo-set item member?]
  (if member?
    (pseudo-set-conj pseudo-set item)
    (pseudo-set-disj pseudo-set item)))

;;; Simple multiset operations.

(defn multiset-conj
  "Add an item (with optional multiplicity) to a multiset,
   represented as a map from items to multiplicities."
  ([ms item]
   (multiset-conj ms item 1))
  ([ms item count]
   (update-in ms [item] #((fnil + 0) % count))))

(defn multiset
  "Turn a seq into a multiset."
  [items]
  (reduce multiset-conj {} items))

(defn multiset-diff
  "Given two multisets, return a triple of multisets,
   of what is in the first, but not the second, what is in the second,
   but not the first, and what is in both."
  [first second]
  (reduce (fn [[first-only second-only both] key]
            (let [first-count (get first key 0)
                  second-count (get second key 0)]
              [(cond-> first-only
                 (> first-count second-count)
                 (assoc key (- first-count second-count)))
               (cond-> second-only
                 (> second-count first-count)
                 (assoc key (- second-count first-count)))
               (cond-> both
                 (and (pos? first-count) (pos? second-count))
                 (assoc key (min first-count second-count)))]))
          [{} {} {}]
          (clojure.set/union (keys first) (keys second))))

(defn multiset-sum
  "Return the sum of two multisets."
  [first second]
  (let [keys (clojure.set/union (keys first) (keys second))]
    (zipmap keys
            (map (fn [key] (+ (get first key 0) (get second key 0)))
                 keys))))

(defn multiset-to-generating-values
  "Given a multi-set, a list of keys, and corresponding list of
  values for those keys, return a list of values whose
  keys add up to the multi-set."
  [multiset keys values]
  (let [;; A map from key to a vector of values with that key.
        key-values-map (reduce (fn [map [value key]]
                                 (update-in map [key] #(conj % value)))
                               {} (map vector values keys))]
    (reduce (fn [result [key count]]
              (concat result (take count (key-values-map key))))
            [] multiset)))

;;; Utilities for making maps that clean up empty values.

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

(defn assoc-if-non-empty
  "Like assoc, but does a dissoc if the value is empty."
  [m k value]
  (if (empty? value)
    (dissoc m k)
    (assoc m k value)))

;; Utils for working with atoms in a lock free way.

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

(defn swap-returning-both!
  "Swap, and return a vector of the old and new values."
  [cell f & args]
  (swap-control-return! cell
                        (fn [old]
                          (let [new (apply f old args)]
                            [new [old new]]))))

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

;;; Threading state through map style operations

(defn thread-map
  "Call f on each element of the sequence, passing it the current state
  as its second argument. f must return a pair of a value and the new state.
  Return the sequence of values and the final state.
  If the initial items are a seq, have the returned sequence be a seq."
  [f items state]
  (let [[mapped state]
        (reduce (fn [[accum state] item]
                  (let [[value state] (f item state)]
                    [(conj accum value) state]))
                [[] state] items)]
    [(if (seq? items) (list* mapped) mapped) state]))

(defn thread-recursive-map
  "Walk the possibly nested sequence, calling f on each element,
  passing it the current state as its first argument. f must return a
  pair of a value and the new state
  Return the nested sequence of values the final state."
  [f items state]
  (if (sequential? items)
    (thread-map (fn [items state] (thread-recursive-map f items state))
                items state)
    (f items state)))

;;; Parsing

(defn parse-string-as-number
  "Parse user entered characters into a number if possible.
  Otherwise return the characters as a string."
  ;; NOTE: This is not compabible with ClojureScript.
  [str]
  (try (let [x (Float/parseFloat (clojure.string/trim str))
             int-x (int x)]
         (if (== x int-x) int-x x))
       (catch Exception e str)))

;;; Misc

(defn union-seqs
  "Given two seqs, return the seq of their union."
  [s1 s2]
  (cond (empty? s1) s2
        (empty? s2) s1
        true (distinct (concat s1 s2))))

(defn remove-first
  "Remove the first item of the seq that matches the pred"
  [pred coll]
  ((fn inner [coll]
     (lazy-seq
      (when-let [[x & xs] (seq coll)]
        (if (pred x)
          xs
          (cons x (inner xs))))))
   coll))

(defn separate-by
  "Split a seq into those where the predicate is true, and those where it
  is false."
  [fun x]
  (let [groups (group-by #(if (fun %) true false) x)]
    [(groups true) (groups false)]))

(defn update-last
  "Update the last element of a vector."
  [vec fun]
  (if (empty? vec)
    [(fun nil)]
    (update-in vec [(dec (count vec))] fun)))

(defn map-map
  "Map two levels down."
  [fun x]
  (map #(map fun %) x))

(defn map-with-first-last
  "Map, but also call the function with whether or not the item
  is first in the list and whether or not it is last."
  [fun x]
  (when (seq x)
    (let [falses (repeat (- (count x) 1) false)]
      (map fun x (concat [true] falses) (concat falses [true])))))

(defn replace-in-seqs
  "Replace from with to recursively through the sequence."
  [x from to]
  (cond (sequential? x) (map #(replace-in-seqs % from to) x)
        (= x from) to
        true x))

(defn prewalk-seqs
  "Like clojure.walk/prewalk, but only descends into seqs"
  [fun x]
  (let [pre (fun x)]
    (if (sequential? pre)
      (map (partial prewalk-seqs fun) pre)
      pre)))

(defn ensure-in-map
  "Given a map, if the map has a value for the key, 
   return the map and that value. If not, call the function with the key,
   put its result into the map, and return the new map and the result."
  [map key fun]
  (if (contains? map key)
    [map (map key)]
    (let [val (fun key)]
      [(assoc map key val) val])))

(defn ensure-in-atom-map!
  "Given an atom holding a map, if the map has a value for the key, 
   return that value. If not, call the function with the key,
   put its result into the map, and the result."
  [atom-map key fun]
  (swap-control-return! atom-map
                        #(ensure-in-map % key fun)))

(defn canonical-atom-form
  "Convert a value to its canonical form, so that equivalent atoms will have
  equal canonical forms. (This means trimmed lower case strings.)"
  [value]
  (if (string? value)
    (loop [result (clojure.string/trim (clojure.string/lower-case value))]
      (if (and (not= result "") (= (nth result 0) \u00A0))
        (recur (subs result 1))
        result))
    value))

(defn equivalent-atoms?
  "Return true if the canonical forms of the atoms are equal."
  [a1 a2]
  (or (= a1 a2)
      (and (string? a1) (string? a2)
           (= (canonical-atom-form a1) (canonical-atom-form a2)))))

(defn add-elements-to-entity-list
  [entity elements]
  (if (empty? elements)
    entity
    (concat (if (sequential? entity) entity (list entity))
            elements)))


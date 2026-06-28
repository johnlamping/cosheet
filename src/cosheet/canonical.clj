(ns cosheet.canonical
  (:require
   (cosheet
    [utils :refer [multiset multiset-diff multiset-sum multiset-conj]]
    [entity :refer [mutable-entity? primitive? object? element?
                    content elements orientation in-different-store
                    make-tree-element interned-object?
                    conflux-tree-object? conflux-tree-object-id
                    make-conflux-tree-object
                    to-tree]])))

;;; Utilities for converting to and from a canonical description of an
;;; entity, and for operating on the canonical description. The
;;; canonical description doesn't support access to the parts of an
;;; entity, but two entities are equal if and only if their canonical
;;; forms are identical, unless they have cycles that might be
;;; traversed in different directions.

;;; (We can't guarantee equality for entities with cycles that can be
;;; traversed in different directions, because the tree we end up with
;;; can depend on the order we traverse elements. But the whole point
;;; of canonicalization is to be able to ignore the order of
;;; elements.)

;;; The canonical form depends on the type of entity
;;;                            Strings: Their trimmed lower case
;;;                   Other primitives: Themselves
;;;                   Mutable entities: Themselves
;;;         Immutable interned-objects: Themselves, but with their
;;;                                     store set to nil.
;;;   Immutable non-identified objects: A pair of
;;;              [:object
;;;               A multiset of the canonical descriptions of their elements.]
;;;       Immutable conflux-tree-objects: A triple of
;;;              [:conflux-object
;;;               The conflux-tree-object's id.
;;;               A multiset of the canonical descriptions of their elements.]
;;;           Other immutable elements: A triple of
;;;              [Their orientation.
;;;               The canonical form of their content.
;;;               A multiset of the canonical descriptions of their elements.]

;;; This form makes the description independent of the order of the
;;; elements and the case of their strings. And, in the case of
;;; interned objects, it also makes them independent of what store the
;;; objects refer to, since their identity is independent of store.

;;; This is an easier way to get order independence than sorting,
;;; because Clojure doesn't define a sort order between heterogenous
;;; types, like strings and ints.

(defn canonical-primitive-form
  "Convert a primitive value to its canonical form, so that equivalent
  primitives will have equal canonical forms. The only transformation
  we do is for strings, which we trim and lower case. Our trimming includes
  removing leading non-breaking spaces, which generated names have."
  [value]
  (if (string? value)
    (loop [result (clojure.string/trim (clojure.string/lower-case value))]
      (if (and (not= result "") (= (nth result 0) \u00A0))
        (recur (subs result 1))
        result))
    value))

(defn equivalent-primitives?
  "Return true if the arguments are primitives and their canonical forms
  are equal."
  [a1 a2]
  (and (primitive? a1)
       (primitive? a2)
       (or (= a1 a2)
           (and (string? a1) (string? a2)
                (= (canonical-primitive-form a1)
                   (canonical-primitive-form a2))))))

(defn canonical-tree-object?
  "Return true if c is the canonical form of a tree-object — a vector
  starting with :object or :conflux-object."
  [c]
  (and (vector? c) (#{:object :conflux-object} (first c))))

(defn simplest-canonical
  "Given an orientation, a canonical content and a multiset of canonical
  elements, return the most compact canonical representation."
  [orientation canonical-content canonical-elements]
  (cond (= canonical-content :object)
        (do (assert (= orientation :source))
            [:object canonical-elements])
        (and (= orientation :source)
             (empty? canonical-elements)
             (primitive? canonical-content))
        canonical-content
        true [orientation canonical-content canonical-elements]))

(defn internal-canonicalize
  "Given a tree-form entity, return a canonical representation of it.
  Conflux-tree-object identity is preserved (the canonical form for a
  conflux-tree-object carries its id) so shared references are
  recognizable in the canonical form."
  [entity]
  (cond (mutable-entity? entity)
        entity
        (primitive? entity)
        (canonical-primitive-form entity)
        (object? entity)
        (if (interned-object? entity)
          (in-different-store entity nil)
          ;; Since the object is not interned, we have to expand it
          ;; out, so it can match other non-interned objects.
          (let [canonicals (multiset (map internal-canonicalize
                                          (elements entity)))]
            (if (conflux-tree-object? entity)
              [:conflux-object (conflux-tree-object-id entity) canonicals]
              [:object canonicals])))
        true
        (do
          (assert (element? entity))
          (simplest-canonical
           (orientation entity)
           (internal-canonicalize (content entity))
           (multiset (map internal-canonicalize (elements entity)))))))

(defn canonicalize
  "Given an entity, return a canonical representation of it."
  [entity]
  (internal-canonicalize (to-tree entity)))

(def canonical-to-tree)

(defn canonical-set-to-list
  "Given a multiset of canonicalized elements, return a list of the entities
  they represent."
  [set]
  (when (not (empty? set))
    (reduce (fn [result [key count]]
              (concat result (repeat count (canonical-to-tree key))))
            [] (seq set))))

(defn canonical-to-tree
  "Given a canonicalized list form of an entity, return a list form for it."
  [entity]
  (if (vector? entity)
    (cond (= (first entity) :object)
          (do (assert (= (count entity) 2))
              (apply vector :object (canonical-set-to-list (second entity))))
          (= (first entity) :conflux-object)
          (do (assert (= (count entity) 3))
              (make-conflux-tree-object
               (second entity)
               (canonical-set-to-list (nth entity 2))))
          :else
          (do (assert (= (count entity) 3))
              (let [[orientation content elements] entity]
                (make-tree-element orientation
                                   (canonical-to-tree content)
                                   (canonical-set-to-list elements)))))
    entity))

(def common-canonical)

(defn canonical-orientation-and-content
  "Given a canonical representation of an element, return its
  orientation and the canonical form of its content."
  [r]
  (assert (not (canonical-tree-object? r)))
  (if (vector? r)
    [(first r) (second r)]
    [:source r]))

(defn canonical-elements-multiset
  [r]
  (if (vector? r)
    (case (count r)
      3 (nth r 2)
      2 (nth r 1)
      {})
    {}))

(defn update-canonical-content
  "Update the content of a canonical representation of an element with a
  new primitive."
  [r c]
  (let [ac (canonical-primitive-form c)]
    (if (sequential? r)
      (if (= (count r) 3)
        (assoc r 1 ac)
        r)
      ac)))

(defn clear-canonical-elements
  "clear the elements of a canonical representation of an element."
  [r]
  (if (vector? r)
    (let [[orientation content elements] r]
      (assert (= (count r) 3))
      (assert (#{:source :target} (first r)))
      (simplest-canonical orientation content {}))
    r))

(defn partition-by-orientation-and-content
  "Given a multiset of canonical representations of elements, partition
  by content and orientation, and return a map from [orientation
  content] to multiset of canonical representations with that
  orientation and content. Plain tree-object content is normalized to
  :object in the key (so elements with object content share a
  partition regardless of which object they hold); conflux-tree-object
  content is normalized to [:conflux-object id] (so references to the
  same conflux share a partition independent of its elements)."
  [m]
  (reduce
   (fn [accum [entity count]]
     (let [[orientation content] (canonical-orientation-and-content entity)
           key [orientation
                (cond (not (canonical-tree-object? content)) content
                      (= (first content) :conflux-object)
                      [:conflux-object (second content)]
                      :else :object)]]
       (update accum key #(multiset-conj (or % {}) entity count))))
   {} (seq m)))

(defn common-canonical-multiset
  "Given two multisets of canonical representations, return a maximal
  multiset that can be extended by both. One multiset can be
  'extended' to another if it can be made identical to it by some
  combination of extending its members and adding members."
  ;;; TODO: We don't always return the largest possible extension,
  ;;; because we don't recognize all cases of partial matching. The
  ;;; challenge is that when there are partial matches, there isn't
  ;;; always a unique most in common.  And since this function must be
  ;;; commutative, we can't use argument order to choose among
  ;;; different candidates. Instead, we currently only recognize
  ;;; commonality when there is identity, or when the non identical
  ;;; stuff has only one entity from each multiset. That should cover
  ;;; the vast majority of cases, but it could do better. For example,
  ;;; if one of the non-identical has only one contained entity, and
  ;;; it has a largest match with the other non-identical, that is
  ;;; still unique. (Largest requires a notion of size of an entity,
  ;;; which can be the number of parts it has, which can be easily
  ;;; computed from its multiset.
  [s1 s2]
  (let [m1 (partition-by-orientation-and-content s1)
        m2 (partition-by-orientation-and-content s2)
        commons
        (keep
         (fn [[key entities1]]
           (when-let [entities2 (m2 key)]
             (let [[first-only second-only both]
                   (multiset-diff entities1 entities2)]
               (or (when (and (= (count first-only) 1)
                              (= (count second-only) 1))
                     (let [[[first-entity first-count] & _]
                           (seq first-only)
                           [[second-entity second-count] & _]
                           (seq second-only)
                           common (common-canonical first-entity
                                                    second-entity)]
                       (when common
                         (let [count (min first-count second-count)]
                           (multiset-conj both common count)))))
                   (when (not (empty? both)) both)
                   (let [example (first (first (seq entities1)))
                         [orientation content]
                         (canonical-orientation-and-content example)]
                     (if (canonical-tree-object? content)
                       ;; Within this partition the elements all have
                       ;; object content but the contents differ; the
                       ;; commonality is the common-canonical of one
                       ;; example content from each side, wrapped as an
                       ;; element with no sub-elements.
                       (let [other (first (first (seq entities2)))
                             [_ other-content]
                             (canonical-orientation-and-content other)
                             common (common-canonical content other-content)]
                         (multiset-conj
                          {} (simplest-canonical orientation common {})))
                       (multiset-conj
                        {} (clear-canonical-elements example))))))))
         (seq m1))]
    (reduce multiset-sum {} commons)))

(defn common-canonical
  "Given two canonical representations, return the canonincal
  representation, if any, that captures their commmonality. If one
  argument is an object and the other isn't, return nil.

  For two objects, return a canonical object whose elements are the
  commonality of the two objects' elements. If both arguments are
  conflux-objects with the same id, the result is a conflux-object
  with that same id; otherwise it is a plain object.

  For two non-objects (primitives or elements), their orientations
  and contents must match; if so, the result has that orientation and
  content, with elements that are the commonality of the two
  arguments' elements."
  [c1 c2]
  (let [obj1? (canonical-tree-object? c1)
        obj2? (canonical-tree-object? c2)]
    (when (= (boolean obj1?) (boolean obj2?))
      (if obj1?
        ;; Both are objects.
        (let [common-elements (common-canonical-multiset
                               (canonical-elements-multiset c1)
                               (canonical-elements-multiset c2))]
          (if (and (= (first c1) :conflux-object)
                   (= (first c2) :conflux-object)
                   (= (second c1) (second c2)))
            [:conflux-object (second c1) common-elements]
            [:object common-elements]))
        ;; Both are non-objects.
        (let [[orientation1 content1] (canonical-orientation-and-content c1)
              [orientation2 content2] (canonical-orientation-and-content c2)
              [contents-match common-content]
              (if (and (canonical-tree-object? content1)
                       (canonical-tree-object? content2))
                ;; Objects always have objectness in common.
                [true (common-canonical content1 content2)]
                [(= content1 content2) content1])]
          (when (and (= orientation1 orientation2) contents-match)
            (if (and (sequential? c1) (sequential? c2))
              (let [common-elements (common-canonical-multiset
                                     (canonical-elements-multiset c1)
                                     (canonical-elements-multiset c2))]
                (simplest-canonical orientation1 common-content common-elements))
              (simplest-canonical orientation1 common-content {}))))))))

(defn canonical-extended-by?
  "Return true if every part of c1 has a corresponding part in c2.
  Doesn't currently recognize all cases, specifically, when an
  element of the first list corresponds to an extension of it in the
  second."
  [c1 c2]
  (let [obj1? (canonical-tree-object? c1)
        obj2? (canonical-tree-object? c2)]
    (when (= (boolean obj1?) (boolean obj2?))
      (if obj1?
        ;; Both are objects. The id constraint of c1 (if it has one)
        ;; must be met by c2; then c1's elements must be a sub-multiset
        ;; of c2's elements.
        (when (or (not= (first c1) :conflux-object)
                  (and (= (first c2) :conflux-object)
                       (= (second c1) (second c2))))
          (let [common-elements (common-canonical-multiset
                                 (canonical-elements-multiset c1)
                                 (canonical-elements-multiset c2))]
            (= common-elements (canonical-elements-multiset c1))))
        ;; Both are non-objects (primitives or elements).
        (let [[orientation1 content1] (canonical-orientation-and-content c1)
              [orientation2 content2] (canonical-orientation-and-content c2)]
          (when (and (or (nil? content1)
                         (= content1 content2))
                     (= orientation1 orientation2))
            (or (not (sequential? c1))
                (and (sequential? c2)
                     (let [common-elements (common-canonical-multiset
                                            (canonical-elements-multiset c1)
                                            (canonical-elements-multiset c2))]
                       (= common-elements
                          (canonical-elements-multiset c1)))))))))))

(defn canonical-have-common-elaboration?
  "Return true if there is a common elaboration of c1 and c2."
  [c1 c2]
  (let [obj1? (canonical-tree-object? c1)
        obj2? (canonical-tree-object? c2)]
    (when (= (boolean obj1?) (boolean obj2?))
      (if obj1?
        ;; Both are objects. They are incompatible only if both are
        ;; conflux-objects with different ids.
        (not (and (= (first c1) :conflux-object)
                  (= (first c2) :conflux-object)
                  (not= (second c1) (second c2))))
        ;; Both are non-objects.
        (let [[orientation1 content1] (canonical-orientation-and-content c1)
              [orientation2 content2] (canonical-orientation-and-content c2)]
          (and (or (nil? content1)
                   (nil? content2)
                   (= content1 content2))
               (= orientation1 orientation2)))))))

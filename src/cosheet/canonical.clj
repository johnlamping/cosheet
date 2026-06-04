(ns cosheet.canonical
  (:require
   (cosheet
    [utils :refer [multiset multiset-diff multiset-sum multiset-conj]]
    [entity :refer [mutable-entity? primitive? object? element?
                    content elements orientation in-different-store
                    make-element-list interned-object?]])))

;;; Utilities for converting to and from a canonical description of an
;;; entity, and for operating on the canonical description. The
;;; canonical description doesn't support access to the parts of an
;;; entity, but two entities are equal if and only if their canonical
;;; forms are identical.

;;; The canonical form depends on the type of entity
;;;                            Strings: Their trimmed lower case
;;;                   Other primitives: Themselves
;;;                   Mutable entities: Themselves
;;;         Immutable interned-objects: Themselves, but with their
;;;                                     store set to nil.
;;;   Immutable non-identified objects: A pair of
;;;              [:object
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

(defn canonicalize
  "Given an entity, return a canonical representation of it."
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
          [:object
           (multiset (map canonicalize (elements entity)))])
        true
        (do
          (assert (element? entity))
          (simplest-canonical
           (orientation entity)
           (canonicalize (content entity))
           (multiset (map canonicalize (elements entity)))))))

(def canonical-to-list)

(defn canonical-set-to-list
  "Given a multiset of canonicalized elements, return a list of the entities
  they represent."
  [set]
  (when (not (empty? set))
    (reduce (fn [result [key count]]
              (concat result (repeat count (canonical-to-list key))))
            [] (seq set))))

(defn canonical-to-list
  "Given a canonicalized list form of an entity, return a list form for it."
  [entity]
  (if (vector? entity)
    (if (= (first entity) :object)
      (do (assert (= (count entity) 2))
          (apply vector :object (canonical-set-to-list (second entity))))
      (do (assert (= (count entity) 3))
          (let [[orientation content elements] entity]
            (make-element-list orientation
                               (canonical-to-list content)
                               (canonical-set-to-list elements)))))
    entity))

(def common-canonical)

(defn canonical-content
  "Return the content of a canonical representation (as a
  canonical). For objects, return :object as the content."
  [r]
  (if (vector? r)
    (if (= (count r) 3)
      (second r)
      :object)
    r))

(defn canonical-orientation-and-content
  "Return the orientation and content of a canonical
  representation (returning the canonical form of the content)."
  [r]
  (if (vector? r)
    (if (= (count r) 3) 
      [(first r) (second r)]
      [:source :object])
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
      (simplest-canonical orientation content {}))
    r))

(defn common-canonical-multisets-for-same-orientation-and-content
  "Given two non-empty multisets of canonical representations of
  elements, all with the same orientation and content, return what
  they have in common.  More precisely, return as large a multiset as
  possible that can be extended to both arguments.  One multiset can
  be 'extended' to another if it can be made identical to it by some
  combination of extending its members and adding members.
  Both multisets must be non-empty."
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
  (assert (not (empty? s1)))
  (assert (not (empty? s2)))
  (let [[first-only second-only both] (multiset-diff s1 s2)]
    (or (when (and (= (count first-only) 1) (= (count second-only) 1))
          (let [[[first-entity first-count] & _] (seq first-only)
                [[second-entity second-count] & _] (seq second-only)
                common (common-canonical first-entity second-entity)]
            (when common
              (let [count (min first-count second-count)]
                (multiset-conj both common count)))))
        (when (not (empty? both)) both)
        (let [example (first (first (seq s1)))]
          (multiset-conj {} (clear-canonical-elements example))))))

(defn partition-by-orientation-and-content
  "Given a multiset of canonical representations, partition by content
  and orientation, and return a map from [orientation content] to
  multiset of canonical representations with that orientation and
  content."
  [m]
  (reduce
   (fn [accum [entity count]]
     (update accum (canonical-orientation-and-content entity)
             #(multiset-conj (or % {}) entity count)))
   {} (seq m)))

(defn common-canonical-multisets
  "Given two multisets of canonical representations or elements, return
  a maximal multiset of representations that can be extended by both."
  [s1 s2]
  (let [m1 (partition-by-orientation-and-content s1)
        m2 (partition-by-orientation-and-content s2)
        commons
        (keep
         (fn [[content1 entities1]]
           (when-let [entities2 (m2 content1)]
             (common-canonical-multisets-for-same-orientation-and-content
              entities1 entities2)))
         (seq m1))]
    (reduce multiset-sum {} commons)))

(defn common-canonical
  "Given two canonical representations of elements, return the
  canonincal representation, if any, that captures their
  commmonality. To have commonality, their orders and contents must be
  the same. If that is satisfied, then commonality is an entity with
  that content. In addition, if some of the elements of the first
  representation can be paired up with elements of the second, and the
  pairs have commonality, then those commonalities are elements of the
  overall commonality."
  [c1 c2]
  (let [[orientation1 content1] (canonical-orientation-and-content c1)
        [orientation2 content2] (canonical-orientation-and-content c2)]
    (when (and (= orientation1 orientation2) (= content1 content2))
      (if (and (sequential? c1) (sequential? c2))
        (let [common-elements (common-canonical-multisets
                               (canonical-elements-multiset c1)
                               (canonical-elements-multiset c2))]
          (simplest-canonical orientation1 content1 common-elements))
        (simplest-canonical orientation1 content1 {})))))

(defn canonical-extended-by?
  "Return true if every part of c1 has a corresponding part in c2.
  Doesn't currently recognize all cases, specifically, when an
  element of the first list corresponds to an extension of it in the
  second."
  [c1 c2]
  (let [[orientation1 content1] (canonical-orientation-and-content c1)
        [orientation2 content2] (canonical-orientation-and-content c2)]
    (when (and (or (nil? content1)
                   (= content1 content2))
               (= orientation1 orientation2))
      (or (not (sequential? c1))
          (and (sequential? c2)
               (let [common-elements (common-canonical-multisets
                                      (canonical-elements-multiset c1)
                                      (canonical-elements-multiset c2))]
                 (= common-elements (canonical-elements-multiset c1))))))))

(defn canonical-have-common-elaboration?
  "Return true if there is a common elaboration of c1 and c2."
  [c1 c2]
  (let [[orientation1 content1] (canonical-orientation-and-content c1)
        [orientation2 content2] (canonical-orientation-and-content c2)]
    (and (or (nil? content1)
             (nil? content2)
             (= content1 content2))
         (= orientation1 orientation2))))

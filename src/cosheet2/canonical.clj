(ns cosheet2.canonical
  (:require (cosheet2 [utils :refer [multiset multiset-diff multiset-sum
                                     multiset-conj]])))

;;; Utilities for converting to and from a canonical description
;;; of an entity and the list form of it, and for operating on the
;;; canonical description.

;;; The canonical form describes primitive entities as the canonical
;;; form of themselves, and describes non-primitive entities as a pair
;;; of:
;;;    The canonical form of their content
;;;    A multiset of the canonical descriptions of their elements.

;;; This form makes the description independent of the order of the
;;; elements and the case of their strings.

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
  "Return true if the canonical forms of the primitives are equal."
  [a1 a2]
  (or (= a1 a2)
      (and (string? a1) (string? a2)
           (= (canonical-primitive-form a1) (canonical-primitive-form a2)))))

(defn canonicalize-list
  "Given the list form of an entity, return a canonical representation of it."
  [entity]
  (if (sequential? entity)
    [(canonicalize-list (canonical-primitive-form (first entity)))
     (multiset (map canonicalize-list (rest entity)))]
    (canonical-primitive-form entity)))

(def canonical-to-list)

(defn canonical-set-to-list
  "Given a multiset of canonicalized forms, return a list of the entities
  they represent."
  [set]
  (when (not (empty? set))
    (reduce (fn [result [key count]]
              ;; TODO: Remove this sanity check.
              (when (map? key)
                (println "XXXXXXXXX nested set in canonical" key)
                (assert false))
              (concat result
                      (repeat count (canonical-to-list key))))
            [] (seq set))))

(defn canonical-to-list
  "Given a canonicalized list form of an entity, return a list form for it."
  [entity]
  (if (sequential? entity)
    (do (assert (= (count entity) 2))
        (cons (canonical-to-list (first entity))
              (canonical-set-to-list (second entity))))
    entity))

(def common-canonical)

(defn canonical-content
  "Return the content of a canonical representation."
  [r]
  (if (sequential? r) (first r) r))

(defn update-canonical-content
  "Update the content of a canonical representation."
  [r c]
  (let [ac (canonical-primitive-form c)]
    (if (sequential? r) [ac (second r)] ac)))

(defn common-canonical-multisets-for-same-content
  "Given two non-empty multisets of canonical representations,
  all representing entities with the same content, return what the
  have in common.  More precisely, return as large a multiset as
  possible that can be extended to both arguments.  One multiset can
  be 'extended' to another if it can be made identical to it by some
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
  (let [[first-only second-only both] (multiset-diff s1 s2)]
    (or (when (and (= (count first-only) 1) (= (count second-only) 1))
          (let [[first-entity first-count] (first (seq first-only))
                [second-entity second-count] (first (seq second-only))
                common (common-canonical first-entity second-entity)]
            (when common
              (let [count (min first-count second-count)]
                (multiset-conj both common count)))))
        (when (not (empty? both)) both)
        (multiset-conj {} (canonical-content (first (keys s1)))))))

(defn partition-by-content
  "Given a multiset of canonical representations, partition by content,
  and return a map from content to multiset of canonical
  representations with that content."
  [m]
  (reduce
   (fn [accum [entity count]]
     (update accum (canonical-content entity)
             #(multiset-conj (or % {}) entity count)))
   {} (seq m)))

(defn common-canonical-multisets
  "Given two multisets of canonical representations, return a maximal multiset
  of representations that can be extended by both."
  [s1 s2]
  (let [m1 (partition-by-content s1)
        m2 (partition-by-content s2)
        commons (keep (fn [[content1 entities1]]
                        (when-let [entities2 (m2 content1)]
                          (common-canonical-multisets-for-same-content
                           entities1 entities2)))
                      (seq m1))]
    (reduce multiset-sum {} commons)))

(defn common-canonical
  "Given two canonical representations, return the canonincal
  representation, if any, that captures their commmonality. To have
  commonality, the contents must be the same. If that is satisfied,
  then commonality is an entity with that content. In addition, if
  some of the elements of the first representation can be paired up
  with elements of the second, and the pairs have commonality, then
  those commonalities are elements of the overall commonality."
  [c1 c2]
  (let [content1 (canonical-content c1)
        content2 (canonical-content c2)]
    (when (= content1 content2)
      (if (and (sequential? c1) (sequential? c2))
        (let [common-elements (common-canonical-multisets
                               (second c1) (second c2))]
          (if (empty? common-elements)
            content1
            (list content1 common-elements)))
        content1))))

(defn canonical-extended-by
  "Return true if every part of c1 has a corresponding part in c2.
   Doesn't currently recognize all cases, specifically, when an
  element of the first list corrresponds to an extension of it in the
  second."
  [c1 c2]
  (let [content1 (canonical-content c1)
        content2 (canonical-content c2)]
    (when (or (nil? content1) (= content1 content2))
      (or (not (sequential? c1))
          (and (sequential? c2)
               (let [common-elements (common-canonical-multisets
                                      (second c1) (second c2))]
                 (= common-elements (second c1))))))))

(defn canonical-have-common-elaboration
  "Return true if there is a common elaboration of c1 and c2."
  [c1 c2]
  (let [content1 (canonical-content c1)
        content2 (canonical-content c2)]
    (or (nil? content1) (nil? content2) (= content1 content2))))

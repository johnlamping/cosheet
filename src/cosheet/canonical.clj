(ns cosheet.canonical
  (:require (cosheet [utils :refer [multiset multiset-diff multiset-union
                                    multiset-conj
                                    canonical-atom-form equivalent-atoms?]]
                     [debug :refer [simplify-for-print]])))

;;; Utilities for converting to and from a canonical description
;;; of an entity and the list form of it, and for operating on the
;;; canonical description.

;;; The canonical form describes atomic elements as the canonical form of
;;; themselves, and non-atomic elements as a pair of the canonical form
;;; of their content and a multiset of the canonical descriptions of
;;; their elements.
;;; This makes the description independent of the order of the elements
;;; and the case of their strings.
;;; It gets order independence easier than sorting, because Clojure doesn't
;;; define a sort order between heterogenous types, like strings and ints.

(defn canonicalize-list
  "Given the list form of an entity, return a canonical representation of it."
  [entity]
  (if (sequential? entity)
    [(canonicalize-list (canonical-atom-form (first entity)))
     (multiset (map canonicalize-list (rest entity)))]
    (canonical-atom-form entity)))

(def canonical-to-list)

(defn canonical-set-to-list
  "Given a multiset of canonicalized lists or sets,
   with the set also in canonical form, return a list of the items."
  [set]
  (when (not (empty? set))
    (reduce (fn [result [key count]]
              (concat result
                      (repeat count (if (map? key)
                                      (canonical-set-to-list key)
                                      (canonical-to-list key)))))
            [] (seq set))))

(defn canonical-to-list
  "Given a canonicalized list form of an item, return a list form for it."
  [item]
  (if (sequential? item)
    (do (assert (= (count item) 2))
        (cons (canonical-to-list (first item))
              (canonical-set-to-list (second item))))
    item))

(def common-canonical)

(defn content
  "Return the content of a canonical representation."
  [r]
  (if (sequential? r) (first r) r))

(defn common-canonical-multisets-for-same-content
  "Given two non-empty multisets of canonical representations,
  all with the same content, return the multiset of representations
  with the most in common."
  ;;; TODO: We don't handle all cases of partial matching. The challenge is
  ;;; that there isn't always a unique most in common, but this function
  ;;; must be commutative. So we currently only handle identity, and when the
  ;;; non identical stuff has only one item in each multiset. That should
  ;;; cover the vast majority of cases, but it could do better by, for example,
  ;;; checking for unique subsets.
  [s1 s2]
  (let [[first-only second-only both] (multiset-diff s1 s2)]
    (or (when (and (= (count first-only) 1) (= (count second-only) 1))
          (let [[first-item first-count] (first (seq first-only))
                [second-item second-count] (first (seq second-only))
                common (common-canonical first-item second-item)]
            (when common
              (let [count (min first-count second-count)]
                (multiset-conj both common count)))))
        (when (not (empty? both)) both)
        (multiset-conj {} (content (first (keys s1)))))))

(defn map-by-content
  "Given a multiset of canonical representations, partition by content,
  and return a map from content to multiset of representations with
  that content."
  [m]
  (reduce
   (fn [accum [item count]]
     (update accum (content item)
             #(multiset-conj (or % {}) item count)))
   {} (seq m)))

(defn common-canonical-multisets
  "Given two multisets of canonical representations, return the multiset
  of representations with the most in common."
  [s1 s2]
  (let [m1 (map-by-content s1)
        m2 (map-by-content s2)
        commons (keep (fn [[content1 items1]]
                        (when-let [items2 (m2 content1)]
                          (common-canonical-multisets-for-same-content
                           items1 items2)))
                      (seq m1))]
    (reduce multiset-union {} commons)))

(defn common-canonical
  "Given two canonical representations, return the canonincal
  representation, if any, that captures their commmonality. To have
  commonality, the contents must be the same. If that is satisfied,
  then commonality is an item with that content. In addition, if
  some of the elements of the first representation can be paired up
  with elements of the second, and the pairs have commonality, then
  those commonalities are elements of the overall commonality."
  [c1 c2]
  (let [content1 (content c1)
        content2 (content c2)]
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
   Doesn't currently recognize all cases, where an element of the
   first list corrresponds to an extension of it in the second."
  [c1 c2]
  (let [content1 (content c1)
        content2 (content c2)]
    (when (or (nil? content1) (= content1 content2))
      (or (not (sequential? c1))
          (and (sequential? c2)
               (let [common-elements (common-canonical-multisets
                                      (second c1) (second c2))]
                 (= common-elements (second c1))))))))

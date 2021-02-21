(ns cosheet2.orderable)

;;; This module defines a set of ordered items, such that given any
;;; item, you can split it into a pair of items, each having the same
;;; ordering compared to all other items as the original item did, but
;;; one item of the pair being before the other item.

;;; To implement this, the first idea is to represent an item with a
;;; pair of integers, (left, right) which stands for the interval of
;;; integers between left and right, inclusive. Splitting an item
;;; splits that interval. The second idea allows for futher splitting
;;; once an interval is reduced to a single integer. Logically, that
;;; integer is opened up into a whole interval. The first part of the
;;; pair is replaced with the integer that was split, followed by the
;;; start of the interior interval, while the second part gives the
;;; endpoint at the interior interval. So (a, a) is turned into
;;;([a 0], <big number>). The process can be repeated, with, for
;;; example, ([a b], b) being split into ([a b 0], <big number>).

;;; This choice of representation is designed to make comparison as
;;; fast as possible, at the cost of making splitting a bit more
;;; awkward.

;;; We use a record to hold an orderable, so an orderable can be
;;; distinguished from list syntax for an item.

(defrecord
    ^{:doc "An object that can be ordered, and that can split into two."}
    Orderable
  [left right])

(def initial
  (->Orderable 0, 1152921504606846975))  ;; 2^60 - 1

(defn earlier-sequence?
  "Internal function for comparing sequences in lexicographic order"
  [a b]
  (cond (empty? a) (not (empty? b))
        (empty? b) false
        true (let [fa (first a), fb (first b)]
               (cond (< fa fb) true
                     (> fa fb) false
                     true (earlier-sequence? (rest a) (rest b))))))

(defn earlier?
  "Return true if a comes before b."
  [a b]
  (let [a-left (:left a), b-left (:left b)]
    (if (sequential? a-left)
      (if (sequential? b-left)
        (earlier-sequence? a-left b-left)
        (< (first a-left) b-left))
      (if (sequential? b-left)
        (< a-left (first b-left))
        (< a-left b-left)))))

(defn orderable-compare
  "Compare two orderables consistently with java.util.Comparator"
  [a b]
  (cond (earlier? a b) -1
        (earlier? b a) 1
        true 0))

(declare split)

(defn- get-midpoint
  "Internal function that picks a midpoint between two intgers, biasing
   so that most of the space is on one side, if requested. Left must be
   less than right. The result might be equal to the left input,
   but will always be less than the right one."
  [left right larger-side]
  (case larger-side
    nil (quot (+ right left) 2)
    :before (if (> (- right left) 1125899906842624)  ; 2^50
                (- right 1099511627776)  ; 2^40
                (quot (+ left (* 7 right)) 8))
    :after (if (> (- right left) 1125899906842624)
               (+ left 1099511627776)
               (quot (+ (* 7 left) right) 8))))

(defn- split-sequence
  "Internal function that splits, given the sequence for the left."
  [left larger-side]
  (let [least (:left initial) most (:right initial)]
    (split (->Orderable (conj left least) most) larger-side)))

(defn split
  "Split the item into two, returning the earlier and later halves.
   If larger-side is non-nil, it must be
   :before or :after, in which case the majority of the space
   is put on that side."
  [a & larger-side]
  (let [left (:left a)
        right (:right a)
        [larger-side] larger-side]
    (if (sequential? left)
      (let [last-left (peek left)]
        (if (== last-left right)
          (split-sequence left larger-side)
          (let [m (get-midpoint last-left right larger-side)]
            [(->Orderable left m)
             (->Orderable (conj (pop left) (+ m 1)) right)])))
      (if (== left right)
        (split-sequence [left] larger-side)
        (let [m (get-midpoint left right larger-side)]
          [(->Orderable left m) (->Orderable (+ m 1) right)])))))



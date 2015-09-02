(ns cosheet.orderable)

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

(declare split)

(defn- get-midpoint
  "Internal function that picks the (earlier) midpoint between two intgers."
  [left right preferred-side]
  (case preferred-side
    nil (quot (+ right left) 2)
    :before (if (> (- right left) 1024)
                (- right 127)
                (quot (+ left (* 7 right)) 8))
    :after (if (> (- right left) 1024)
               (+ left 128)
               (quot (+ (* 7 left) right) 8))))

(defn- split-sequence
  "Internal function that splits, given the sequence for the left."
  [left preferred-side]
  (let [least (:left initial) most (:right initial)]
    (split (->Orderable (conj left least) most) preferred-side)))

(defn split
  "Split the item into two. If preferred-side is non-nil, it must be
   :before or :after, in which case the majority of the space
   is put on that side."
  [a & preferred-side]
  (let [left (:left a)
        right (:right a)
        [preferred-side] preferred-side]
    (if (sequential? left)
      (let [last-left (peek left)]
        (if (== last-left right)
          (split-sequence left preferred-side)
          (let [m (get-midpoint last-left right preferred-side)]
            [(->Orderable left m)
             (->Orderable (conj (pop left) (+ m 1)) right)])))
      (if (== left right)
        (split-sequence [left] preferred-side)
        (let [m (get-midpoint left right preferred-side)]
            [(->Orderable left m) (->Orderable (+ m 1) right)])))))


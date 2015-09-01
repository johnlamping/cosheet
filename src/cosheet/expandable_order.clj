(ns cosheet.expandable-order)

;;; This module defines a set of ordered items, such that given any
;;; item, you can split it into a pair of items, each having the same
;;; ordering compared to all other items as the original item did, but
;;; one item of the pair being before the other item.

;;; To implement this, the first idea is to represent an item with a
;;; pair of integers, (a, b) which stands for the interval of integers
;;; between a and b, inclusive. Splitting an item splits that
;;; interval. The second idea allows for futher splitting once an
;;; interval is reduced to a single integer. Logically, that integer
;;; is opened up into a whole interval. The first part of the pair is
;;; replaced with the integer that was split, followed by the start of
;;; the interior interval, while the second part gives the endpoint at
;;; the interior interval. So (a, a) is turned into
;;; ([a 0], <big number>). The process can be repeated, with, for
;;; example, ([a b], b) being split into ([a b 0], <big number>).

;;; This choice of representation is designed to make comparison as
;;; fast as possible, at the cost of making splitting a bit more awkward.

(def initial-expandable
  '(0, 1152921504606846975)) ;; 2^60 - 1

(defn earlier-sequence?
  "Internal function for comparing sequences in lexicographic order"
  [a b]
  (cond (empty? a) (not (empty? b))
        (empty? b) false
        true (let [fa (first a), fb (first b)]
               (cond (< fa fb) true
                     (> fa fb) false
                     true (earlier-sequence? (rest a) (rest b))))))

(defn earlier-in-order?
  "Return true if a comes before b."
  [a b]
  (let [fa (first a), fb (first b)]
    (if (sequential? fa)
      (if (sequential? fb)
        (earlier-sequence? fa fb)
        (< (first fa) fb))
      (if (sequential? fb)
        (< fa (first fb))
        (< fa fb)))))

(defn split-in-order
  "Split the item into two."
  [a]
  (let [[fa sa] a]
    (if (sequential? fa)
      (let [lfa (peek fa)]
        (if (== lfa sa)
          (let [[f s] initial-expandable]
            (split-in-order (list (conj fa f) s)))
          (let [m (quot (+ lfa sa) 2)]
            [(list fa m) (list (conj (pop fa) (+ m 1)) sa)])))
      (if (== fa sa)
        (let [[f s] initial-expandable]
          (split-in-order (list [fa f] s)))
        (let [m (quot (+ fa sa) 2)]
            [(list fa m) (list (+ m 1) sa)])))))


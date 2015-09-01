(ns cosheet.expandable-order-test
  (:require [clojure.test :refer [deftest is]]
            [cosheet.expandable-order :refer :all]
            ; :reload
            ))

(deftest earlier-sequence?-test
  (is (= (earlier-sequence? [1 2 3] [1 2 4]) true))
  (is (= (earlier-sequence? [1 2 3] [2 1 1]) true))
  (is (= (earlier-sequence? [1 2 3] [1 2 3 4]) true))
  (is (= (earlier-sequence? [1 2 3] [1 2 3]) false))
  (is (= (earlier-sequence? [1 2 4] [1 2 3]) false))
  (is (= (earlier-sequence? [2 1 1] [1 2 3]) false))
  (is (= (earlier-sequence? [1 2 3 4] [1 2 3]) false)))

(deftest earlier-in-order?-test
  (is (= (earlier-in-order? '(1 3) '(4 6)) true))
  (is (= (earlier-in-order? '([1 7] 9) '(4 6)) true))
  (is (= (earlier-in-order? '(1 3) '([4 0] 0)) true))
  (is (= (earlier-in-order? '([1 2 1] 3) '([1 2 4] 6)) true))
  (is (= (earlier-in-order? '([1 2 1] 3) '([1 3 0] 0)) true))
  (is (= (earlier-in-order? '(4 6) '(4 6)) false))
  (is (= (earlier-in-order? '(4 6) '(1 3)) false))
  (is (= (earlier-in-order? '(4 6) '([1 7] 9)) false))
  (is (= (earlier-in-order? '([4 0] 0) '(1 3)) false))
  (is (= (earlier-in-order? '([1 2 4] 6) '([1 2 4] 6)) false))
  (is (= (earlier-in-order? '([1 2 4] 6) '([1 2 1] 3)) false))
  (is (= (earlier-in-order? '([1 3 0] 0) '([1 2 1] 3)) false)))

(deftest split-in-order-test
  (is (= (split-in-order '(1 10)) '[(1 5) (6 10)]))
  (is (= (split-in-order '(1 11)) '[(1 6) (7 11)]))
  (is (= (split-in-order '(1 2)) '[(1 1) (2 2)]))
  (is (= (split-in-order '(2 2))
         '[([2 0] 576460752303423487)
           ([2 576460752303423488] 1152921504606846975)]))
  (is (= (split-in-order '([2 1] 10)) '[([2 1] 5) ([2 6] 10)]))
  (is (= (split-in-order '([1 3 2] 2))
         '[([1 3 2 0] 576460752303423487)
           ([1 3 2 576460752303423488] 1152921504606846975)]))
  (is (= (split-in-order '(1 10) :before) '[(1 8) (9 10)]))
  (is (= (split-in-order '(1 10) :after) '[(1 2) (3 10)]))
  (is (= (split-in-order '(1 2) :after) '[(1 1) (2 2)]))
  (is (= (split-in-order '(2 2) :before)
         '[([2 0] 1152921504606846848)
           ([2 1152921504606846849] 1152921504606846975)])))

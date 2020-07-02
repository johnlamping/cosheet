(ns cosheet2.orderable-test
  (:require [clojure.test :refer [deftest is]]
            [cosheet2.orderable :as orderable :refer :all]
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

(deftest earlier?-test
  (is (= (earlier? (->Orderable 1 3) (->Orderable 4 6)) true))
  (is (= (earlier? (->Orderable [1 7] 9) (->Orderable 4 6)) true))
  (is (= (earlier? (->Orderable 1 3) (->Orderable [4 0] 0)) true))
  (is (= (earlier? (->Orderable [1 2 1] 3) (->Orderable [1 2 4] 6)) true))
  (is (= (earlier? (->Orderable [1 2 1] 3) (->Orderable [1 3 0] 0)) true))
  (is (= (earlier? (->Orderable 4 6) (->Orderable 4 6)) false))
  (is (= (earlier? (->Orderable 4 6) (->Orderable 1 3)) false))
  (is (= (earlier? (->Orderable 4 6) (->Orderable [1 7] 9)) false))
  (is (= (earlier? (->Orderable [4 0] 0) (->Orderable 1 3)) false))
  (is (= (earlier? (->Orderable [1 2 4] 6) (->Orderable [1 2 4] 6)) false))
  (is (= (earlier? (->Orderable [1 2 4] 6) (->Orderable [1 2 1] 3)) false))
  (is (= (earlier? (->Orderable [1 3 0] 0) (->Orderable [1 2 1] 3)) false)))

(deftest split-test
  (is (= (split (->Orderable 1 10)) [(->Orderable 1 5) (->Orderable 6 10)]))
  (is (= (split (->Orderable 1 11)) [(->Orderable 1 6) (->Orderable 7 11)]))
  (is (= (split (->Orderable 1 2)) [(->Orderable 1 1) (->Orderable 2 2)]))
  (is (= (split (->Orderable 2 2))
         [(->Orderable [2 0] 576460752303423487)
          (->Orderable [2 576460752303423488] 1152921504606846975)]))
  (is (= (split (->Orderable [2 1] 10))
         [(->Orderable [2 1] 5) (->Orderable [2 6] 10)]))
  (is (= (split (->Orderable [1 3 2] 2))
         [(->Orderable [1 3 2 0] 576460752303423487)
          (->Orderable [1 3 2 576460752303423488] 1152921504606846975)]))
  (is (= (split (->Orderable 1 10) :before)
         [(->Orderable 1 8) (->Orderable 9 10)]))
  (is (= (split (->Orderable 1 10) :after)
         [(->Orderable 1 2) (->Orderable 3 10)]))
  (is (= (split (->Orderable 1 2) :after)
         [(->Orderable 1 1) (->Orderable 2 2)]))
  (is (= (split (->Orderable 2 2) :before)
         [(->Orderable [2 0] 1152920405095219199)
          (->Orderable [2 1152920405095219200] 1152921504606846975)])))


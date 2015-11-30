(ns cosheet.server.key-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet 
             [store-impl :refer [->ItemId]])
            (cosheet.server
             [key :refer :all])
            ; :reload
            ))

(deftest prepend-to-key-test
  (let [a (item-referent :a)
        b (item-referent :b)
        c (item-referent :c)
        p (parallel-referent [] [b])
        pp (parallel-referent [p a] [b])]
    (is (= (prepend-to-key a [b]) [a b]))
    (is (= (prepend-to-key c [p a])
           [(parallel-referent [c] [b]) a]))
    (is (= (prepend-to-key c [pp a])
           [(parallel-referent [(parallel-referent [c] [b]) a]
                               [b])
            a]))))

(deftest remove-first-referent-test
  (is (= (remove-first-referent [3 4])
         [4]))
  (is (= (remove-first-referent [[:parallel [] [2 3]] 4])
         [4]))
  (is (= (remove-first-referent [[:parallel [0 1] [2 3]] 4])
          [[:parallel [1] [2 3]] 4])))

(deftest remove-content-referent-test
  (is (= (remove-content-referent [])
         []))
  (is (= (remove-content-referent [[:content] 3 4])
         [3 4]))
  (is (= (remove-content-referent [[:parallel [[:content] 1] [2 3]] 4])
         [[:parallel [1] [2 3]] 4]))
  (is (= (remove-content-referent [[:parallel [0 1] [2 3]] 4])
         [[:parallel [0 1] [2 3]] 4])))

(deftest item-ids-referred-to-test
  (is (= (set (item-ids-referred-to [[:parallel [0 1] [2 3]] 4]))
         #{0 1 4})))

(deftest item-determining-referents-test
  (let [id (->ItemId "a")]
    (is (= (item-determining-referents
            [[:parallel :a :b] [:content] [:group "a"] [:condition :b] id])
           [[:parallel :a :b] id]))))


(ns cosheet2.mutable-map-test
  (:require [clojure.test :refer [deftest is]]
            [cosheet2.mutable-map :as mm]
            ; :reload
            ))

(deftest mutable-map-test
  (let [mm (mm/new-mutable-map)]
    
    (is (= (mm/update! mm :foo (fn [x] 5)) 5))
    (is (= (mm/update! mm :foo (fn [x] (+ x 2))) 7))
    (is (= (mm/mm-get mm :foo)) 7)
    
    (is (= (mm/update-in! mm [:bar :baz] (fn [x] 5)) 5))
    (is (= (mm/mm-get-in mm [:bar :baz]) 5))
    
    (is (= (mm/update-in-returning-both! mm [:bar :baz] (fn [x] 6)) [5 6]))
    (is (= (mm/mm-get-in mm [:bar :baz]) 6))

    (is (= (mm/current-contents mm) {:foo 7, :bar {:baz 6}}))
    
    (is (= (mm/update! mm :foo (fn [x] nil)) nil))
    (is (nil? (mm/mm-get mm :foo)))
    
    (is (= (mm/update-in-clean-up-returning-both!
            mm [:bar :baz] (constantly nil))
           [6 nil]))
    (is (nil? (mm/mm-get mm :foo)))

    (is (= (mm/current-contents mm) {:foo nil}))
    
    (is (= (mm/assoc-in! mm [:bar :baz] 8) 8))
    (is (= (mm/mm-get-in mm [:bar :baz]) 8))
    
    (mm/dissoc-in! mm [:bar :baz])
    (is (nil? (mm/mm-get mm :bar)))
    
    (mm/update-in-clean-up! mm [:foo :bar] (constantly nil))
    (is (nil? (mm/mm-get mm :foo)))))


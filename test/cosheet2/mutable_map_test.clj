(ns cosheet2.mutable-map-test
  (:require [clojure.test :refer [deftest is]]
            [cosheet2.mutable-map :as mm]
            ; :reload
            ))

(deftest mutable-map-test
  (let [mm (mm/new-mutable-map)]
    (is (= (mm/update! mm :foo (fn [x] 5)) 5))
    (is (= (mm/update! mm :foo (fn [x] (+ x 1))) 6))
    (is (= (mm/get! mm :foo)) 6)
    (mm/update! mm :foo (fn [x] nil))
    (is (= (mm/get! mm :foo)) nil)
    (is (= (mm/update-in! mm [:foo :bar] (fn [x] 5)) 5))
    (is (= (mm/get-in! mm [:foo :bar]) 5))
    (is (= (mm/update-in-returning-both! mm [:foo :bar] (fn [x] 6)) [5 6]))
    (is (= (mm/get-in! mm [:foo :bar]) 6))
    (is (= (mm/update-in-clean-up-returning-both!
            mm [:foo :bar] (constantly nil))
           [6 nil]))
    (is (= (mm/get! mm :foo) nil))
    (is (= (mm/assoc-in! mm [:bar :baz] 8) 8))
    (is (= (mm/get-in! mm [:bar :baz]) 8))
    (mm/dissoc-in! mm [:bar :baz])
    (is (nil? (mm/get! mm :bar)))
    (mm/update-in-clean-up! mm [:foo :bar] (constantly nil))
    (is (nil? (mm/get! mm :foo)))))

(deftest current-contents-test
  (let [mm (mm/new-mutable-map)]
    (mm/assoc-in! mm [:a] 1)
    (mm/assoc-in! mm [:b] 2)
    (is (= (mm/current-contents mm) {:a 1 :b 2}))))

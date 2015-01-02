(ns cosheet.mutable-map-test
  (:require [clojure.test :refer [deftest is]]
            [cosheet.mutable-map :as mm :refer [dissoc-in update-in-clean-up]]
            :reload))

(deftest dissoc-in-test
  (is (empty? (dissoc-in {:a {:b {:c 1}}} [:a :b :c])))
  (is (= (dissoc-in {:a {:b {:c 1}} :x 0} [:a :b :c])
         {:x 0}))
  (is (= (dissoc-in {:a {:b {:c 1} :y 0} :x 0} [:a :b :c])
         {:a {:y 0} :x 0}))
  (is (= (dissoc-in {:a {:b {:c 1 :z 0} :y 0} :x 0} [:a :b :c])
         {:a {:b {:z 0} :y 0} :x 0}))
  (is (= (dissoc-in {:a {:b {:c 1}} :x 0} [:a :b :w :u])
          {:a {:b {:c 1}} :x 0})))

(deftest update-in-clean-up-test
  (is (empty (update-in-clean-up {:a {:b {:c 1}}} [:a :b :c]
                                       (constantly nil))))
    (is (empty (update-in-clean-up {:a {:b {:c 1}}} [:a :b :c]
                                       (constantly #{}))))
  (is (= (update-in-clean-up {:a {:b {:c 1}}} [:a :b :d] (constantly nil))
         {:a {:b {:c 1}}}))
  (is (= (update-in-clean-up  {:a {:b {:c 1}}} [:a :b :c] inc)
         {:a {:b {:c 2}}}))
  (is (= (update-in-clean-up  {:a {:b {:c 1}}} [:a :b :d]  (constantly 2))
         {:a {:b {:c 1 :d 2}}}))
  (is (= (update-in-clean-up  {:a {:b {:c 1}}} [:a :b :d]  (constantly 2))
         {:a {:b {:c 1 :d 2}}})))

(deftest swap-returning-both!-test
  (let [a (atom 1)]
    (is (= (mm/swap-returning-both! a (fn [old] (is (= old 1)) 2))) [1 2])
    (is (= @a 2))))

(deftest mutable-map-test
  (let [mm (mm/new-mutable-map)]
    (mm/update! mm :foo (fn [x] 5))
    (mm/update! mm :foo (fn [x] (+ x 1)))
    (is (= (mm/get! mm :foo)) 6)
    (mm/update! mm :foo (fn [x] nil))
    (is (= (mm/get! mm :foo)) nil)
    (mm/update-in! mm [:foo :bar] (fn [x] 5))
    (is (= (mm/get-in! mm [:foo :bar]) 5))
    (is (= (mm/update-in-returning-both! mm [:foo :bar] (fn [x] 6)) [5 6]))
    (is (= (mm/get-in! mm [:foo :bar]) 6))
    (is (= (mm/update-in-clean-up-returning-both!
            mm [:foo :bar] (constantly nil))
           [6 nil]))
    (is (= (mm/get! mm :foo) nil))
    (mm/assoc-in! mm [:bar :baz] 8)
    (is (= (mm/get-in! mm [:bar :baz]) 8))
    (mm/dissoc-in! mm [:bar :baz])
    (is (nil? (mm/get! mm :bar)))
    (mm/update-in-clean-up! mm [:foo :bar] (constantly nil))
    (is (nil? (mm/get! mm :foo)))))

(deftest call-with-latest-value!-test
  (let [mm (mm/new-mutable-map)
        path [:a :b]]
    (mm/assoc-in! mm path 1)
    (mm/call-with-latest-value-in!
     mm path
     (fn [current arg]
       (is (= arg "arg"))
       (mm/assoc-in! mm path (min (inc current) 5)))
     "arg")
    (is (= (mm/get-in! mm path) 5))))

(deftest call-and-clear-in!-test
  (let [mm (mm/new-mutable-map)
        path [:a :b :c]
        history (atom [])]
    (mm/assoc-in! mm path 1)
    (mm/call-and-clear-in!
     mm path
     (fn [current arg]
       (is (= arg "arg"))
       (swap! history #(conj % current))
       (when (< current 5)
         (mm/update-in! mm path (constantly (inc current)))))
     "arg")
    (is (= @history [1 2 3 4 5]))
    (is (= (mm/get-in! mm [:a :b]) nil))))

(deftest current-contents-test
  (let [mm (mm/new-mutable-map)]
    (mm/assoc-in! mm [:a] 1)
    (mm/assoc-in! mm [:b] 2)
    (is (= (mm/current-contents mm) {:a 1 :b 2}))))

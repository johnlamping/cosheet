(ns cosheet.utils-test
  (:require [clojure.test :refer [deftest is]]
            [cosheet.utils :refer :all]
            ; :reload
            ))

(deftest multiset-test
  (is (= (multiset [:a :b :c :b])
         {:a 1 :b 2 :c 1})))

(deftest update-last-test
  (is (= (update-last [1 2 3] inc) [1 2 4])))

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
    (is (= (swap-returning-both! a (fn [old] (is (= old 1)) 2))) [1 2])
    (is (= @a 2))))

(deftest swap-control-return!-test
  (let [a (atom 1)]
    (is (= (swap-control-return! a (fn [old] (is (= old 1)) [2 3]))) 3)
    (is (= @a 2))))

(deftest call-with-latest-value-test
  (let [cell (atom 1)]
    (call-with-latest-value
     (fn [] @cell)
     (fn [current arg]
       (is (= arg "arg"))
       (reset! cell (min (inc current) 5)))
     "arg")
    (is (= @cell 5))))


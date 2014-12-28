(ns cosheet.synchronize-test
  (:require [clojure.test :refer [deftest is]]
            [cosheet.synchronize :refer :all]
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
    (is (= (swap-returning-both! a (fn [old] (is (= old 1)) 2))) [1 2])
    (is (= @a 2))))

(deftest mutable-map-test
  (let [mm (new-mutable-map)]
    (mm-update! mm :foo (fn [x] 5))
    (mm-update! mm :foo (fn [x] (+ x 1)))
    (is (= (mm-get! mm :foo)) 6)
    (mm-update! mm :foo (fn [x] nil))
    (is (= (mm-get! mm :foo)) nil)
    (mm-update-in! mm [:foo :bar] (fn [x] 5))
    (is (= (mm-get-in! mm [:foo :bar]) 5))
    (is (= (mm-update-in-returning-both! mm [:foo :bar] (fn [x] 6)) [5 6]))
    (is (= (mm-get-in! mm [:foo :bar]) 6))
    (mm-assoc-in! mm [:bar :baz] 8)
    (is (= (mm-get-in! mm [:bar :baz]) 8))
    (mm-dissoc-in! mm [:bar :baz])
    (is (nil? (mm-get! mm :bar)))
    (mm-update-in-clean-up! mm [:foo :bar] (constantly nil))
    (is (nil? (mm-get! mm :foo)))))

(deftest tasks-test
  (let [queue (new-priority-task-queue)
        history (atom [])
        task-factory (fn [& expected]
                       (fn [& args]
                         (is (= args expected))
                         (swap! history #(conj % args))))]
    (add-task queue (task-factory :a1 :a2) :a1 :a2)
    (add-task-with-priority queue -1 (task-factory :a3 :a4) :a3 :a4)
    (add-task-with-priority queue 1 (task-factory :a5) :a5)
    (is (run-pending-task queue))
    (is (run-pending-task queue))
    (is (run-pending-task queue))
    (is (= @history [[:a3 :a4] [:a1 :a2] [:a5]]))
    (is (not (run-pending-task queue)))
    (is (= @history [[:a3 :a4] [:a1 :a2] [:a5]]))))

(deftest mm-call-with-latest-value!-test
  (let [mm (new-mutable-map)
        path [:a :b]]
    (mm-assoc-in! mm path 1)
    (mm-call-with-latest-value-in!
     mm path
     (fn [current arg]
       (is (= arg "arg"))
       (mm-assoc-in! mm path (min (inc current) 5)))
     "arg")
    (is (= (mm-get-in! mm path) 5))))

(deftest mm-call-and-clear-in!-test
  (let [mm (new-mutable-map)
        path [:a :b :c]
        history (atom [])]
    (mm-assoc-in! mm path 1)
    (mm-call-and-clear-in!
     mm path
     (fn [current arg]
       (is (= arg "arg"))
       (swap! history #(conj % current))
       (when (< current 5)
         (mm-update-in! mm path (constantly (inc current)))))
     "arg")
    (is (= @history [1 2 3 4 5]))
    (is (= (mm-get-in! mm path) nil))))

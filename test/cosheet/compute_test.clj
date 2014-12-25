(ns cosheet.compute-test
  (:require [clojure.test :refer [deftest is]]
            [cosheet.compute :refer :all]
            [cosheet.compute-impl :refer :all]
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

(deftest mutable-map-test
  (let [mm (new-mutable-map)]
    (dosync (mm-update! mm :foo (fn [x] 5)))
    (dosync (mm-update! mm :foo (fn [x] (+ x 1))))
    (is (= (mm-get! mm :foo)) 6)
    (dosync (mm-update! mm :foo (fn [x] nil)))
    (is (= (mm-get! mm :foo)) nil)
    (dosync (mm-update-in! mm [:foo :bar] (fn [x] 5)))
    (is (= (mm-get-in! mm [:foo :bar]) 5))
    (dosync (mm-assoc-in! mm [:bar :baz] 8))
    (is (= (mm-get-in! mm [:bar :baz]) 8))
    (dosync (mm-dissoc-in! mm [:bar :baz]))
    (is (nil? (mm-get! mm :bar)))
    (dosync (mm-update-in-clean-up! mm [:foo :bar] (constantly nil)))
    (is (nil? (mm-get! mm :foo)))))

(deftest tasks-test
  (let [scheduler (new-approximating-scheduler)
        history (atom #{})
        task-factory (fn [e1 e2]
                       (fn [s a1 a2]
                         (is (= s scheduler))
                         (is (= a1 e1))
                         (is (= a2 e2))
                         (swap! history #(conj % [a1 a2]))))]
    (add-task scheduler (task-factory :a1 :a2) :a1 :a2)
    (add-task scheduler (task-factory :a3 :a4) :a3 :a4)
    (run-pending-task scheduler)
    (run-pending-task scheduler)
    (is (= @history #{[:a1 :a2] [:a3 :a4]}))
    (run-pending-task scheduler)
    (is (= @history #{[:a1 :a2] [:a3 :a4]}))))

(deftest valid?-test
  (is (valid? {}))
  (is (not (valid? {:value-inputs-changed true})))
  (is (not (valid? {:uncertain-inputs #{5}}))))

(comment "How to do local redefinitions in a rest"
  (def ^:dynamic counter 0)
  (deftest conflicts-mutable-map-test
    (let [mm (new-mutable-map)
          orig-update-non-nil update-non-nil
          orig-add-mutable-value add-mutable-value]
      (binding [counter 0]
        (with-redefs [update-non-nil
                      (fn [va f]
                        (set! counter (inc counter))
                        (if (not= (mod counter 5) 0)
                          (swap! va (fn [x] nil)))
                        (orig-update-non-nil va f))
                      add-mutable-value
                      (fn [mm key value]
                        (set! counter (inc counter))
                        (if (not= (mod counter 6) 0)
                          (swap! mm (fn [m] (assoc m key (atom 3)))))
                        (orig-add-mutable-value mm key value))]
          (update! mm :foo (fn [x] 5))
          (is (= (get! mm :foo)) 5)
          (is (< 5 counter 15)))))))

(comment
 (deftest updated-dependency-info-test
   (is (= (updated-dependency-info
           {:used-info {:a {:value "a"}}
            :uncertain-inputs #{:a}}
           {:expression :a :value "b"})
          {:used-info {:a {:value "a"}}
           :changed-inputs #{:a}}))
   (is (= (updated-dependency-info
           {:used-info {:a {:value "a"}}
            :changed-inputs #{:a}}
           {:expression :a :value "a"
            :changed-inputs #{:x}})
          {:used-info {:a {:value "a"}}
           :uncertain-inputs #{:a}}))
   (is (= (updated-dependency-info
           {:used-info {:a {:value "a"}}
            :changed-inputs #{:a}}
           {:expression :z :value "a"
            :changed-inputs #{:x}})
          {:used-info {:a {:value "a"}}
           :changed-inputs #{:a}}))))


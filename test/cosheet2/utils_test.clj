(ns cosheet2.utils-test
  (:require [clojure.test :refer [deftest is]]
            [cosheet2.utils :refer :all]
            ; :reload
            ))

(deftest pseudo-set-test
  (let [ps (loop [ps nil in [] out [1 2 3 4 5]]
             (is (= (pseudo-set-set ps) (set in)))
             (is (= (set (pseudo-set-seq ps)) (set in)))
             (doseq [x in] (is (pseudo-set-contains? ps x)))
             (doseq [x out] (is (not (pseudo-set-contains? ps x))))
             (cond (empty? in)
                   (is (nil? ps))
                   (= (count in) 1)
                   (is (= ps (first in))))
             (if (not (empty? out))
               (recur (if (= 0 (mod (count in) 2))
                        (pseudo-set-conj ps (first out))
                        (pseudo-set-set-membership ps (first out) true))
                      (conj in (first out))
                      (rest out))
               ps))]
    (loop [ps ps in [1 2 3 4 5] out []]
      (is (= (pseudo-set-set ps) (set in)))
      (is (= (set (pseudo-set-seq ps)) (set in)))
      (doseq [x in] (is (pseudo-set-contains? ps x)))
      (doseq [x out] (is (not (pseudo-set-contains? ps x))))
      (cond (empty? in)
            (is (nil? ps))
            (= (count in) 1)
            (is (= ps (first in))))
      (when (not (empty? in))
        (recur (if (= 1 (mod (count in) 2))
                 (pseudo-set-disj ps (first in))
                 (pseudo-set-set-membership ps (first in) false))
               (rest in)
               (conj out (first in)))))))

(deftest multiset-test
  (is (= (multiset [:a :b :c :b])
         {:a 1 :b 2 :c 1})))

(deftest multiset-diff-test
  (is (= (multiset-diff {:a 1 :b 3 :c 3} {:b 1 :c 3 :d 4})
         [{:a 1 :b 2} {:d 4} {:b 1 :c 3}])))

(deftest multiset-sum-test
  (is (= (multiset-sum {:a 1 :b 3 :c 3} {:b 1 :c 3 :d 4})
         {:a 1 :b 4 :c 6 :d 4})))

(deftest multiset-to-generating-values-test
  (is (= (set (multiset-to-generating-values
               {:a 1 :b 2} [:a :a :a :b :b :b] [:a1 :a2 :a3 :b1 :b2 :b3]))
         (set [:a3 :b2 :b3]))))

(deftest union-seqs-test
  (is (= (union-seqs [] [1 3]) [1 3]))
  (is (= (union-seqs [1 3] []) [1 3]))
  (is (= (union-seqs [1 2 3] [4 3 2]) [1 2 3 4])))

(deftest remove-first-test
  (is (= (remove-first even? [1 2 3 2 4])
         [1 3 2 4])))

(deftest separate-by-test
  (is (= (separate-by even? [1 2 3 4 5 6])
         [[2 4 6] [1 3 5]])))

(deftest update-last-test
  (is (= (update-last [1 2 3] inc) [1 2 4]))
  (is (= (update-last [] #(conj % 1)) [[1]])))

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

(deftest assoc-if-non-empty-test
  (is (assoc-if-non-empty {:a 1 :b 2} :a nil) {:b 2})
  (is (assoc-if-non-empty {:a 1 :b 2} :a {}) {:b 2})
  (is (assoc-if-non-empty {:a 1 :b 2} :a #{nil}) {:a #{nil} :b 2}))

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

(deftest with-latest-value-test
  (let [cell (atom 1)]
    (with-latest-value [current @cell]
      (reset! cell (min (inc current) 5)))
    (is (= @cell 5))))

(deftest thread-map-test
  (let [[result state] (thread-map (fn [i s] [(* i 2) (conj s i)])
                                   [1 2 3 4] [])]
    (is (= state [1 2 3 4]))
    (is (= result [2 4 6 8]))))

(deftest thread-recursive-map-test
  (let [[result state] (thread-recursive-map (fn [i s] [(* i 2) (conj s i)])
                                             [[1 2] [3 4]] [])]
    (is (= state [1 2 3 4]))
    (is (= result [[2 4] [6 8]]))))

(deftest parse-string--as-number-test
  (is (= (parse-string-as-number "x") "x"))
  (is (= (parse-string-as-number "1") 1))
  (is (= (parse-string-as-number " 1 ") 1))
  (is (= (parse-string-as-number "1 1") "1 1"))
  (is (= (parse-string-as-number " 1.0 ") 1))
  (is (= (parse-string-as-number "-1.0") -1))
  (is (= (parse-string-as-number " 1.5 ") 1.5)))

(deftest map-map-test
  (is (= (map-map inc [[1 2] [5 7]])
         [[2 3] [6 8]])))

(deftest map-with-first-last-test
  (is (= (map-with-first-last vector [:a :b :c])
         [[:a true false] [:b false false] [:c false true]]))
  (is (= (map-with-first-last vector [:a])
         [[:a true true]]))
  (is (= (map-with-first-last vector [])
         nil)))

(deftest replace-in-seqs-test
  (is (= (replace-in-seqs [1 [2 [1 2]]] 2 3)
         [1 [3 [1 3]]])))

(deftest prewalk-seqs-test
  (is (= (prewalk-seqs #(if (sequential? %)
                          (remove (partial = [1 2]) %)
                          %)
                       '(0 (1 2 (1 2))))
         '(0 (1 2)))))


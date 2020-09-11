(ns cosheet2.expression-manager-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet2 [reporter :refer [new-reporter reporter-atom reporter-data
                                        reporter-value set-value!
                                        data-value 
                                        valid? reporter?]]
                      [task-queue :refer [new-priority-task-queue]]
                      [calculator :refer :all]
                      [utils :refer [update-new-further-action
                                     update-new-further-actions]])
            ; :reload
            ))

(defn fib [n s]
  (if (<= n 1)
    s   
    (new-reporter :application [+ (fib (- n 1) s) (fib (- n 2) s)]
                  :calculator (fn [& _] nil))))

(deftest new-calculator-data-test
  (let [cd (new-calculator-data (atom :q))]
    (is (= @(:queue cd) :q))
    (is (not (nil? (:cache cd))))))

(deftest modify-and-act!-test
  (let [r (new-reporter :test 10)
        a (atom 1)]
    (modify-and-act! r (fn [data]
                        (-> data
                            (update-in [:test] inc)
                            (update-new-further-action swap! a inc)
                            (update-new-further-actions [[swap! a #(* % 3)]]))))
    (is (= (:test (reporter-data r)) 11))
    (is (= @a 6))))

(defn activated? [r]
  (if (reporter? r)
    (let [data (reporter-data r)]
      (and (or (= (:calculator-data data) :cd)
               (valid? (data-value data)))
           (every? activated? (:application data))))
    true))

(deftest propagate-calculator-data!-test
  (let [state (new-reporter :value 0)
        f6 (fib 6 state)]
    (is (not (activated? f6)))
    (propagate-calculator-data! f6 :cd)
    (is (activated? f6))))

(deftest copy-value-test
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        r1 (new-reporter :value :v)
        r2 (new-reporter :value-source r1
                         :value-source-priority-delta 1
                         :calculator-data cd)]
    (register-for-value-source r2 r1 copy-value-callback cd)
    (compute cd)
    (is (= (reporter-value r2) :v))
    (set-value! r1 :w)
    (compute cd)
    (is (= (reporter-value r2) :w))
    (swap! (reporter-atom r2) dissoc :value-source)
    (register-for-value-source r2 r1 copy-value-callback cd)
    (compute cd)
    (set-value! r1 :x)
    (compute cd)
    (is (= (reporter-value r2) :w))))

(deftest current-value-test
  (let [state (new-reporter :value 0)
        fib6 (fib 6 state)]
    (is (= (current-value fib6) 0))
    (set-value! state 1)
    (is (= (current-value fib6) 13))))

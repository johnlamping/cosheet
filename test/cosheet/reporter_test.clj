(ns cosheet.reporter-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [reporter :refer :all]
                     [debug :refer [trace-current]])
            ; :reload
            ))

(deftest valid-test
  (is (valid? 1))
  (is (not (valid? invalid))))

(deftest constant-test
  (is (= (value 2) 2))
  (let [history (atom [])
        callback (fn [& args] (swap! history #(conj % args)))]
    (set-attendee! 2 :key 0 callback)
    (is (= @history [[:key 2]]))))

(deftest reporter-test
  (let [history (atom [])
        callback (fn [& args] (swap! history #(conj % args)))
        r (new-reporter :value 2
                        :manager [callback :m]
                        :extra :e)]
    (is (reporter? r))
    (is (not (reporter? 2)))
    (is (not (attended? r)))
    (set-attendee! r :foo 0 callback :f)
    (is (= (value r) 2))
    (is (= (:extra (data r)) :e))
    (is (= @history [[:foo r :f] [r :m]]))
    (set-value! r 3)
    (is (= (value r) 3))
    (is (= @history [[:foo r :f] [r :m] [:foo r :f]]))
    (set-attendee! r :foo)
    (is (not (attended? r)))
    (is (= @history [[:foo r :f] [r :m] [:foo r :f] [r :m]]))
    (reset! history [])
    (set-attendee! r :bar 0 callback :b)
    (is (= @history [[:bar r :b] [r :m]]))
    (set-attendee! r :tst 0 callback :t)
    (is (= @history [[:bar r :b] [r :m] [:tst r :t]]))
    (set-value! r 4)
    (is (= (set @history)
           (set [[:bar r :b] [r :m] [:tst r :t] [:bar r :b] [:tst r :t]])))
    (is (thrown? java.lang.AssertionError (set-attendee! r :foo 0 1 2)))
    (set-manager! r callback :m)
    (is (thrown? java.lang.AssertionError (set-manager! r callback 0 2)))))







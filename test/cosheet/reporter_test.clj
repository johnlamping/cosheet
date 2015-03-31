(ns cosheet.reporter-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [reporter :refer :all])
            ; :reload
            ))

(deftest valid-test
  (is (valid? 1))
  (is (not (valid? invalid))))

(deftest reporter-test
  (let [history (atom [])
        callback (fn [& args] (swap! history #(conj % args)))
        r (new-reporter :value 2
                        :manager [callback :m]
                        :attendee [:foo callback :f]
                        :extra :e)]
    (is (reporter? r))
    (is (not (reporter? 2)))
    (is (= (value r) 2))
    (is (= @history [[:foo r :f] [r :m]]))
    (set-value r 3)
    (is (= (value r) 3))
    (is (= @history [[:foo r :f] [r :m] [:foo r :f]]))
    (set-attendee r :foo)
    (is (= @history [[:foo r :f] [r :m] [:foo r :f] [r :m]]))
    (reset! history [])
    (set-attendee r :bar callback :b)
    (is (= @history [[:bar r :b] [r :m]]))
    (set-attendee r :tst callback :t)
    (is (= @history [[:bar r :b] [r :m] [:tst r :t]]))
    (set-value r 4)
    (is (= (set @history)
           (set [[:bar r :b] [r :m] [:tst r :t] [:bar r :b] [:tst r :t]])))
    (is (thrown? java.lang.AssertionError (set-attendee r :foo 1 2)))
    (is (thrown? java.lang.AssertionError (set-manager r 1 2)))))


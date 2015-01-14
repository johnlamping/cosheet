(ns cosheet.state-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [state :refer :all])
            :reload))

(def history (atom []))

(defn generic-callback [& args]
  (swap! history #(conj % args)))

(deftest state-test
  (let [s (new-state :value 1)]
    (reset! history [])
    (is (= (:a s) nil))
    (is (state? s))
    (is (state-value s) 1)
    (state-set s 2)
    (is (state-value s) 2)
    (subscribe s generic-callback "val")
    (is (= @history []))
    (state-set s 3)
    (is (state-value s) 3)
    (is (= @history [[3 s "val"]]))
    (unsubscribe s generic-callback "val")
    (is (= @history [[3 s "val"]]))
    (state-set s 4)
    (is (= @history [[3 s "val"]])))
  ;; Now, try all the optional arguments.
  (let [s (new-state :value 1
                     :callback [generic-callback "sub"]
                     :additional {:a 9})]
    (reset! history [])
    (is (= (:a s) 9))
    (is (state? s))
    (is (not (state? {:value (atom 2)})))
    (is (state-value s) 1)
    (state-set s 2)
    (is (state-value s) 2)
    (subscribe s generic-callback "val")
    (is (= @history [[true s "sub"]]))
    (state-set s 3)
    (is (state-value s) 3)
    (is (= @history [[true s "sub"] [3 s "val"]]))
    (state-set s 3)
    (is (= @history [[true s "sub"] [3 s "val"]]))
    (unsubscribe s generic-callback "foo")
    (is (= @history [[true s "sub"] [3 s "val"]]))
    (unsubscribe s generic-callback "val")
    (is (= @history [[true s "sub"] [3 s "val"] [false s "sub"]]))
    (state-set s 4)
    (is (= @history [[true s "sub"] [3 s "val"] [false s "sub"]]))))

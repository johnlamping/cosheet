(ns cosheet.state-map-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet
             [reporters :as reporter :refer [set-attendee! value invalid]]
             [state-map :refer :all])
            ; :reload
            ))

(defn- request [reporter]
  (set-attendee! reporter :demand (fn [key reporter] nil)))

(deftest state-map-test
  (let [sm (new-state-map {:a 1 :b 3})
        ra (state-map-get sm :a)
        rb (state-map-get sm :b)
        rc (state-map-get sm :c "nobody here")]
    (request ra)
    (is (= (value ra) 1))
    (request rb)
    (is (= (value rb) 3))
    (request rc)
    (is (= (value rc) "nobody here"))
    (state-map-swap! sm :a inc)
    (is (= (value ra) 2))
    (is (= (state-map-get-current-value sm :a) 2))
    (is (= (value rb) 3))
    (state-map-reset! sm :b 5)
    (is (= (value rb) 5))
    (let [result (state-map-swap-control-return! sm :b (fn [x] [(inc x) 99]))]
      (is (= (value rb) 6))
      (is (= result 99)))))


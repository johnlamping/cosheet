(ns cosheet.mutable-set-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet
             [reporters :as reporter :refer [set-attendee! value invalid]]
             [mutable-set :refer :all])
            ; :reload
            ))

(defn- request [reporter]
  (set-attendee! reporter :demand (fn [key reporter] nil)))

(deftest mutable-set-test
  (let [ms (new-mutable-set #{1 2 3})
        r234 (mutable-set-intersection ms [2 3 4])
        r456 (mutable-set-intersection ms [4 5 6])]
    (request r234)
    (is (= (value r234) #{2 3}))
    (request r456)
    (is (= (value r456) #{}))
    (mutable-set-swap! ms #(conj % 4))
    (is (= (value r234) #{2 3 4}))
    (is (= (value r456) #{4}))
    (mutable-set-swap! ms #(disj % 2))
    (is (= (value r234) #{3 4}))
    (is (= (value r456) #{4}))))

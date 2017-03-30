(ns cosheet.server.model-utils-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet [store :refer [new-element-store]]
                     [debug :refer [simplify-for-print]]
                     [test-utils :refer [check any as-set let-mutated]])
            (cosheet.server [model-utils :refer :all])
            ; :reload
            ))

(deftest specialize-template-test
  (let [[c1 [s1 m1]] (specialize-template '("x" (??? :a) (??? 22))
                                       [(new-element-store) {}])
        [c2 [s2 m2]] (specialize-template '("x" (???1 "y") (???1 "22"))
                                       [s1 {}])]
    (is (= c1  '("x" ("\u00A0A" :a) ("\u00A0B" 22))))
    (is (= m1 {}))
    (is (= c2  '("x" ("\u00A0C" "y") ("\u00A0C" "22"))))
    (is (= m2 {"1" "\u00A0C"}))))

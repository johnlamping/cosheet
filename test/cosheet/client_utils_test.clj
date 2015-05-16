(ns cosheet.client-utils-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [client-utils :refer :all])
            ; :reload
            ))

(deftest replace-in-struct-test
  (is (= (replace-in-struct {:a 1 :b 2}
                            {'(3 :a) [{:a '(:d :b)} :c]
                             4 '([] {} () :a)})
          {'(3 1) [{1 '(:d 2)} :c]
           4 '([] {} () 1)})))

(deftest into-atom-map-test
  (let [a1 (atom 1)
        a2 (atom 2)
        a3 (atom 3)
        am (atom {:a1 a1 :a2 a2 :a3 a3})]
    (into-atom-map am {:a2 22, :a3 nil, :a4 4})
    (is (= (count @am) 3))
    (is (= (@am :a1) a1))
    (is (= (@am :a2) a2))
    (is (= @a2 22))
    (is (= @(@am :a4) 4))))


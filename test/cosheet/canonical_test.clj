(ns cosheet.canonical-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet [canonical :refer :all]
                     [debug :refer [simplify-for-print]]
                     [test-utils :refer [check]])
            ; :reload
            ))

(def jane-list `("Jane" "plain" "plain"))
(def joe-list '("Joe"
                "male"
                (39 ("age" tag) ("doubtful" "confidence") )
                "married"
                (45 ("age" tag))))

(deftest canonicalize-list-test
  (is (check (canonicalize-list joe-list)
             '("Joe" {"male" 1
                      "married" 1
                      (39 {["age" {tag 1}] 1
                           ("doubtful" {"confidence" 1}) 1}) 1
                      (45 {["age" {tag 1}] 1}) 1}))))

(deftest canonical-to-list-test
  (let [starting [joe-list jane-list jane-list]
        canonical (canonicalize-list [starting])]
    (is (= (canonicalize-list (canonical-to-list canonical))
           canonical))))


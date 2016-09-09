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
    (is (check (canonicalize-list (canonical-to-list canonical))
               canonical))))

(deftest common-canonical-test
  (is (= (common-canonical "joe" "joe") "joe"))
  (is (= (common-canonical "joe" "jane") nil))
  (is (= (common-canonical "joe" (canonicalize-list '("joe" "name")))
         "joe"))
  (is (= (common-canonical "joe" (canonicalize-list '("jane" "name")))
         nil))
  (is (= (common-canonical (canonicalize-list '("joe" "appelation"))
                           (canonicalize-list '("joe" "name")))
         "joe"))
  (is (= (common-canonical (canonicalize-list '("joe" "a" "name"))
                           (canonicalize-list '("joe" "name" "b")))
         (canonicalize-list '("joe" "name"))))
  (is (= (common-canonical (canonicalize-list '("joe" "a" ("name" "c")))
                           (canonicalize-list '("joe" "name" "b")))
         (canonicalize-list '("joe" "name"))))
  (is (= (common-canonical (canonicalize-list '("joe" "a" ("name" "c" "e")))
                           (canonicalize-list '("joe" ("name" "d" "c") "b")))
         (canonicalize-list '("joe" ("name" "c")))))
  (is (= (common-canonical
          (canonicalize-list '("joe" "a" "b" ("name" "c" "e")))
          (canonicalize-list '("joe" ("name" "d" "c") "b" "b")))
         (canonicalize-list '("joe" "b" ("name" "c")))))
  (is (= (common-canonical
          (canonicalize-list '("joe" "a" "b" ("name" "c" "e") "name"))
          (canonicalize-list '("joe" ("name" "d" "c") "b" "b")))
         (canonicalize-list '("joe" "b" "name"))))
  (is (= (common-canonical
          (canonicalize-list '("joe" "a" "b" ("name" "c" "e") "name"))
          (canonicalize-list '("joe" ("name" "e" "c") ("name" "c") "b")))
         (canonicalize-list '("joe" "b" ("name" "e" "c") "name"))))
  (is (= (common-canonical
          (canonicalize-list '("joe" "a" "b" ("name" "c" "e") ("name" "c")))
          (canonicalize-list '("joe" ("name" "e" "c") ("name" "c") "b")))
         (canonicalize-list '("joe" "b" ("name" "e" "c") ("name" "c"))))))

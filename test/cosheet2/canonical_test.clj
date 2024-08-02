(ns cosheet2.canonical-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet2 [canonical :refer :all]
                      [test-utils :refer [check]])
            ; :reload
            ))

(def jane-list `("Jane" "plain" "plain"))
(def joe-list '("Joe"
                "Male"
                (39 ("age" tag) ("doubtful" "confidence") )
                "married"
                (45 ("age" tag))))

(deftest canonicalize-test
  (is (check (canonicalize joe-list)
             '("joe" {"male" 1
                      "married" 1
                      (39 {["age" {tag 1}] 1
                           ("doubtful" {"confidence" 1}) 1}) 1
                      (45 {["age" {tag 1}] 1}) 1}))))

(deftest canonical-to-list-test
  (let [starting `(~joe-list ~jane-list ~jane-list)
        canonical (canonicalize `(~starting))]
    (is (check (canonicalize (canonical-to-list canonical))
               canonical))))

(deftest update-canonical-content-test
  (is (= (update-canonical-content (canonicalize jane-list) "Jeanette")
         (canonicalize `("Jeanette" "plain" "plain"))))
  (is (= (update-canonical-content (canonicalize "Jane") "Jeanette")
         (canonicalize "Jeanette"))))

(deftest common-canonical-test
  (is (= (common-canonical "joe" "joe") "joe"))
  (is (= (common-canonical "joe" "jane") nil))
  (is (= (common-canonical "joe" (canonicalize '("joe" "name")))
         "joe"))
  (is (= (common-canonical "joe" (canonicalize '("jane" "name")))
         nil))
  (is (= (common-canonical (canonicalize '("joe" "appelation"))
                           (canonicalize '("joe" "name")))
         "joe"))
  (is (= (common-canonical (canonicalize '("joe" "a" "name"))
                           (canonicalize '("joe" "name" "b")))
         (canonicalize '("joe" "name"))))
  (is (= (common-canonical (canonicalize '("joe" "a" ("name" "c")))
                           (canonicalize '("joe" "name" "b")))
         (canonicalize '("joe" "name"))))
  (is (= (common-canonical (canonicalize '("joe" "a" ("name" "c" "e")))
                           (canonicalize '("joe" ("name" "d" "c") "b")))
         (canonicalize '("joe" ("name" "c")))))
  (is (= (common-canonical
          (canonicalize '("joe" "a" "b" ("name" "c" "e")))
          (canonicalize '("joe" ("name" "d" "c") "b" "b")))
         (canonicalize '("joe" "b" ("name" "c")))))
  (is (= (common-canonical
          (canonicalize '("joe" "a" "b" ("name" "c" "e") "name"))
          (canonicalize '("joe" ("name" "d" "c") "b" "b")))
         (canonicalize '("joe" "b" "name"))))
  (is (= (common-canonical
          (canonicalize '("joe" "a" "b" ("name" "c" "e") "name"))
          (canonicalize '("joe" ("name" "e" "c") ("name" "c") "b")))
         (canonicalize '("joe" "b" ("name" "e" "c") "name"))))
  (is (= (common-canonical
          (canonicalize '("joe" "a" "b" ("name" "c" "e") ("name" "c")))
          (canonicalize '("joe" ("name" "e" "c") ("name" "c") "b")))
         (canonicalize '("joe" "b" ("name" "e" "c") ("name" "c"))))))

(deftest canonical-extended-by-test
  (is (canonical-extended-by
       (canonicalize '("joe" ("name" "e" "c") ("name" "c") "b"))
       (canonicalize '("joe" "b" ("name" "c" "e") ("name" "c")))))
  (is (canonical-extended-by
       (canonicalize '("joe" ("name" "e" "c") ("name" "c") "b"))
       (canonicalize '("joe" "a" "b" ("name" "c" "e") ("name" "c")))))
  (is (not (canonical-extended-by
            (canonicalize '("joe" "a" "b" ("name" "c" "e") ("name" "c")))
            (canonicalize '("joe" ("name" "e" "c") ("name" "c") "b")))))
  (is (not (canonical-extended-by
            (canonicalize '("joe" "a" ("name" "c" "e") ("name" "c")))
            (canonicalize '("joe" ("name" "e" "c") ("name" "c") "b")))))
  (is (not (canonical-extended-by
            (canonicalize '("joe" "a" ("name" "c" "e") ("name" "c")))
            (canonicalize '("joe" ("name" "e" "c") ("name" "c") "b"))))))

(deftest canonical-have-common-elaboration-test
  (is (canonical-have-common-elaboration '("joe" "a") '("joe" "b")))
  (is (canonical-have-common-elaboration '(nil "a") '("joe" "b")))
  (is (canonical-have-common-elaboration '("joe" "a") '(nil "b")))
  (is (not (canonical-have-common-elaboration '("joe" "a") '("fred" "a")))))

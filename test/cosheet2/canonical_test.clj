(ns cosheet2.canonical-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet2 [canonical :refer :all]
                      [test-utils :refer [check]]
                      [entity :refer [make-object-list]]
                      [entity-impl])
            ; :reload
            ))

(def jane-list `("Jane" "plain" "plain"))
(def joe-list '("Joe"
                "Male"
                (39 ("age" tag) ("doubtful" "confidence") )
                "married"
                (45 ("age" tag))))
(def joe-object (make-object-list (rest joe-list)))

(deftest canonicalize-test
  (is (check (canonicalize joe-list)
             '[:source
               "joe"
               {"male" 1
                "married" 1
                [:source 39 {[:source "age" {tag 1}] 1
                             [:source "doubtful" {"confidence" 1}] 1}] 1
                [:source 45 {[:source "age" {tag 1}] 1}] 1}]))
  (is (check (canonicalize joe-object)
             '[:object
               {"male" 1
                "married" 1
                [:source 39 {[:source "age" {tag 1}] 1
                             [:source "doubtful" {"confidence" 1}] 1}] 1
                [:source 45 {[:source "age" {tag 1}] 1}] 1}])))

(deftest canonical-to-list-test
  (let [starting `("starting" ~joe-list ~jane-list ~jane-list)
        canonical (canonicalize starting)]
    (is (check (canonicalize (canonical-to-list canonical))
               canonical)))
  (let [starting `[:object ~joe-list ~jane-list ~jane-list]
        canonical (canonicalize starting)]
    (is (check (canonicalize (canonical-to-list canonical))
               canonical))))

(deftest update-canonical-content-test
  (is (= (update-canonical-content (canonicalize jane-list) "Jeanette")
         (canonicalize `("Jeanette" "plain" "plain"))))
  (is (= (update-canonical-content (canonicalize "Jane") "Jeanette")
         (canonicalize "Jeanette")))
  (is (= (update-canonical-content (canonicalize '((:target "Jane"))) "Jeanette")
         (canonicalize '((:target "Jeanette")))))
  (is (= (update-canonical-content (canonicalize joe-object) "Jeanette")
         (canonicalize joe-object))))

(deftest common-canonical-test
  (is (= (common-canonical (canonicalize "joe") (canonicalize "joe"))
         (canonicalize "joe")))
  (is (= (common-canonical (canonicalize "joe") (canonicalize joe-object))
         nil))
  (is (= (common-canonical (canonicalize [:object "male"]) (canonicalize joe-object))
         (canonicalize [:object "male"])))
  (is (= (common-canonical (canonicalize '("joe" [:object "male" "junk"]))
                           (canonicalize `("joe" ~joe-object)))
         (canonicalize '("joe" [:object "male"]))))
  (is (= (common-canonical (canonicalize joe-object) (canonicalize joe-object))
         (canonicalize joe-object)))
  (is (= (common-canonical (canonicalize "joe") (canonicalize '((:source "joe"))))
         (canonicalize "joe")))
  (is (= (common-canonical (canonicalize "joe") (canonicalize '((:target "joe"))))
         nil))
  (is (= (common-canonical (canonicalize '((:target "joe")))
                           (canonicalize '((:target "joe"))))
         (canonicalize '((:target "joe")))))
  (is (= (common-canonical (canonicalize "joe") (canonicalize "jane")) nil))
  (is (= (common-canonical (canonicalize "joe") (canonicalize '("joe" "name")))
         (canonicalize "joe")))
  (is (= (common-canonical (canonicalize "joe") (canonicalize '("jane" "name")))
         nil))
  (is (= (common-canonical (canonicalize '("joe" "appelation"))
                           (canonicalize '("joe" "name")))
         (canonicalize "joe")))
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

(deftest canonical-extended-by?-test
  (is (canonical-extended-by?
       (canonicalize "joe")
       (canonicalize "joe")))
  (is (not (canonical-extended-by?
            (canonicalize "joe")
            (canonicalize "jane"))))
  (is (canonical-extended-by?
       (canonicalize "joe")
       (canonicalize '("joe" "b" ("name" "c" "e") ("name" "c")))))
  (is (not (canonical-extended-by?
            (canonicalize '("joe" "b" ("name" "c" "e") ("name" "c")))
            (canonicalize "joe"))))
  (is (canonical-extended-by?
       (canonicalize '("joe" ("name" "e" "c") ("name" "c") "b"))
       (canonicalize '("joe" "b" ("name" "c" "e") ("name" "c")))))
  (is (not (canonical-extended-by?
            (canonicalize '("joe" ((:target "name") "e" "c") ("name" "c") "b"))
            (canonicalize '("joe" "a" "b" ("name" "c" "e") ("name" "c"))))))
  (is (canonical-extended-by?
       (canonicalize '("joe" ((:target "name") "e" "c") ("name" "c") "b"))
       (canonicalize '("joe" "a" "b" ((:target "name") "c" "e") ("name" "c")))))
  (is (canonical-extended-by?
       (canonicalize '("joe" ((:source "name") "e" "c") ("name" "c") "b"))
       (canonicalize '("joe" "a" "b" ("name" "c" "e") ("name" "c")))))
  (is (not (canonical-extended-by?
            (canonicalize '("joe" "a" "b" ("name" "c" "e") ("name" "c")))
            (canonicalize '("joe" ("name" "e" "c") ("name" "c") "b")))))
  (is (not (canonical-extended-by?
            (canonicalize '("joe" "a" ("name" "c" "e") ("name" "c")))
            (canonicalize '("joe" ("name" "e" "c") ("name" "c") "b")))))
  (is (not (canonical-extended-by?
            (canonicalize '("joe" "a" ("name" "c" "e") ("name" "c")))
            (canonicalize '("joe" ("name" "e" "c") ("name" "c") "b")))))
  (is (canonical-extended-by?
       (canonicalize [:object "male"])
       (canonicalize joe-object)))
  (is (not (canonical-extended-by?
            (canonicalize joe-object)
            (canonicalize [:object "male"]))))
  (is (not (canonical-extended-by?
            (canonicalize joe-object)
            (canonicalize joe-list))))
  (is (not (canonical-extended-by?
            (canonicalize joe-list)
            (canonicalize joe-object)))))

(deftest canonical-have-common-elaboration?-test
  (is (canonical-have-common-elaboration? (canonicalize '("joe" "a"))
                                          (canonicalize '("joe" "b"))))
  (is (not (canonical-have-common-elaboration? (canonicalize '("joe" "a"))
                                               (canonicalize '((:target "joe") "b")))))
  (is (canonical-have-common-elaboration? (canonicalize '((:target "joe") "a"))
                                          (canonicalize '((:target "joe") "b"))))
  (is (canonical-have-common-elaboration? (canonicalize '(nil "a"))
                                          (canonicalize '("joe" "b"))))
  (is (canonical-have-common-elaboration? (canonicalize '("joe" "a"))
                                          (canonicalize '(nil "b"))))
  (is (not (canonical-have-common-elaboration? (canonicalize '("joe" "a"))
                                               (canonicalize '("fred" "a"))))))

(ns cosheet.canonical-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet [canonical :refer :all]
                      [test-utils :refer [check]]
                      [store :refer [make-item-id new-element-store]]
                      store-impl
                      [store-utils :refer [add-object add-universal-objects]]
                      [entity :refer [make-tree-object id->object
                                      make-conflux-tree-object make-tree-id
                                      in-different-store]]
                      [entity-impl])
            (cosheet.server
             [server-test-utils :refer [cyclic-and-shared-a-b-stores]])
            ; :reload
            ))

(def jane-list `("Jane" "plain" "plain"))
(def joe-list '("Joe"
                "Male"
                (39 ("age" tag) ("doubtful" "confidence") )
                "married"
                (45 ("age" tag))))
(def joe-anonymous-object (make-tree-object (rest joe-list)))
(def joe-named-object
  (let [[store id] (add-object
                    (add-universal-objects (new-element-store))
                    (make-tree-object
                     (conj (rest joe-list)
                           `("Joe" (~(id->object (make-item-id "name")
                                                 nil))))))]
    (id->object id store)))

(deftest canonicalize-test
  (is (check (canonicalize joe-list)
             '[:source
               "joe"
               {"male" 1
                "married" 1
                [:source 39 {[:source "age" {tag 1}] 1
                             [:source "doubtful" {"confidence" 1}] 1}] 1
                [:source 45 {[:source "age" {tag 1}] 1}] 1}]))
  (is (check (canonicalize joe-anonymous-object)
             '[:object
               {"male" 1
                "married" 1
                [:source 39 {[:source "age" {tag 1}] 1
                             [:source "doubtful" {"confidence" 1}] 1}] 1
                [:source 45 {[:source "age" {tag 1}] 1}] 1}]))
    (is (= (canonicalize joe-named-object)
           (in-different-store joe-named-object nil))))

(deftest canonicalize-cycle-test
  ;; canonicalize on a stored entity that participates in a cycle.
  ;; The to-tree call dedupes shared references via conflux-tree-
  ;; objects, so internal-canonicalize doesn't loop; and because
  ;; internal-canonicalize uses make-tree-object-copying-id, the
  ;; conflux-tree-object id is preserved in the canonical form.
  (let [{:keys [cyclic-shared-store a-id]} (cyclic-and-shared-a-b-stores)
        item-a (id->object a-id cyclic-shared-store)
        canonical (canonicalize item-a)]
    ;; canonicalize must terminate and be deterministic.
    (is (= canonical (canonicalize item-a)))
    ;; The top-level result is a :conflux-object form preserving the
    ;; tree's conflux id (rather than a plain :object form).
    (is (= :conflux-object (first canonical)))))

(deftest canonical-to-tree-test
  (let [starting `("starting" ~joe-list ~jane-list ~jane-list)
        canonical (canonicalize starting)]
    (is (check (canonicalize (canonical-to-tree canonical))
               canonical)))
  (let [starting `[:object ~joe-list ~jane-list ~jane-list]
        canonical (canonicalize starting)]
    (is (check (canonicalize (canonical-to-tree canonical))
               canonical)))
  ;; Two references to the same conflux-tree-object keep the conflux
  ;; id alive through to-tree, so the canonical form contains a
  ;; :conflux-object that canonical-to-tree must handle.
  (let [starting [:object
                  (make-conflux-tree-object (make-tree-id 1) ["x"])
                  (make-conflux-tree-object (make-tree-id 1) [])]
        canonical (canonicalize starting)]
    (is (check (canonicalize (canonical-to-tree canonical))
               canonical))))

(deftest update-canonical-content-test
  (is (= (update-canonical-content (canonicalize jane-list) "Jeanette")
         (canonicalize `("Jeanette" "plain" "plain"))))
  (is (= (update-canonical-content (canonicalize "Jane") "Jeanette")
         (canonicalize "Jeanette")))
  (is (= (update-canonical-content (canonicalize '((:target "Jane")))
                                   "Jeanette")
         (canonicalize '((:target "Jeanette")))))
  (is (= (update-canonical-content (canonicalize joe-anonymous-object)
                                   "Jeanette")
         (canonicalize  joe-anonymous-object))))

(deftest common-canonical-test
  (is (= (common-canonical (canonicalize "joe")
                           (canonicalize "joe"))
         (canonicalize "joe")))
  (is (= (common-canonical (canonicalize "joe")
                           (canonicalize joe-anonymous-object))
         nil))
  (is (= (common-canonical (canonicalize [:object "male"])
                           (canonicalize joe-anonymous-object))
         (canonicalize [:object "male"])))
  ;; With the objects wrapped as elements, common-canonical compares
  ;; their containing elements by content equality; the two contents
  ;; (different objects) don't match, so no nested commonality is
  ;; surfaced and only the bare "joe" content remains.
  (is (= (common-canonical (canonicalize '("joe" ([:object "male" "junk"])))
                           (canonicalize `("joe" (~joe-anonymous-object))))
         (canonicalize "joe")))
  (is (= (common-canonical (canonicalize joe-anonymous-object)
                           (canonicalize joe-anonymous-object))
         (canonicalize joe-anonymous-object)))
  (is (= (common-canonical (canonicalize "joe")
                           (canonicalize '((:source "joe"))))
         (canonicalize "joe")))
  (is (= (common-canonical (canonicalize "joe")
                           (canonicalize '((:target "joe"))))
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
            (canonicalize
             [:object "joe" '((:target "name") "e" "c") '("name" "c") "b"])
            (canonicalize '("joe" "a" "b" ("name" "c" "e") ("name" "c"))))))
  (is (canonical-extended-by?
       (canonicalize
        [:object "joe" '((:target "name") "e" "c") '("name" "c") "b"])
       (canonicalize
        [:object "joe" "a" "b" '((:target "name") "c" "e") '("name" "c")])))
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
       (canonicalize joe-anonymous-object)))
  (is (not (canonical-extended-by?
            (canonicalize joe-anonymous-object)
            (canonicalize [:object "male"]))))
  (is (not (canonical-extended-by?
            (canonicalize joe-anonymous-object)
            (canonicalize joe-list))))
  (is (not (canonical-extended-by?
            (canonicalize joe-list)
            (canonicalize joe-anonymous-object)))))

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

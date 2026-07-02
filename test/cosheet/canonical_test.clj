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
            (cosheet
             [test-setup :refer [cyclic-and-shared-a-b-stores]])
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
  (is (= (common-canonical (canonicalize '("joe" ([:object "male" "junk"])))
                           (canonicalize `("joe" (~joe-anonymous-object))))
         (canonicalize '("joe" ([:object "male"])))))
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
         (canonicalize '("joe" "b" ("name" "e" "c") ("name" "c")))))
  ;; Two plain objects share their common elements; no common elements
  ;; produces an empty :object.
  (is (= (common-canonical [:object {"a" 1}]
                           [:object {"b" 1}])
         [:object {}]))
  (is (= (common-canonical [:object {"a" 1 "x" 1}]
                           [:object {"x" 1 "y" 1}])
         [:object {"x" 1}]))
  ;; Two conflux-objects with the same id keep that id.
  (is (= (common-canonical
          [:conflux-object (make-tree-id 1) {"x" 1 "y" 1}]
          [:conflux-object (make-tree-id 1) {"x" 1 "z" 1}])
         [:conflux-object (make-tree-id 1) {"x" 1}]))
  ;; Two conflux-objects with different ids collapse to a plain object.
  (is (= (common-canonical
          [:conflux-object (make-tree-id 1) {"x" 1 "y" 1}]
          [:conflux-object (make-tree-id 2) {"x" 1 "z" 1}])
         [:object {"x" 1}]))
  ;; A plain object and a conflux-object give a plain object.
  (is (= (common-canonical [:object {"x" 1 "y" 1}]
                           [:conflux-object (make-tree-id 1)
                            {"x" 1 "z" 1}])
         [:object {"x" 1}]))
  ;; An object and an element are not commensurable.
  (is (= (common-canonical [:object {"x" 1}]
                           [:source "joe" {"x" 1}])
         nil))
  (is (= (common-canonical [:conflux-object (make-tree-id 1) {"x" 1}]
                           "joe")
         nil))
  ;; Two element-wrapped objects on the c1 side, one on the c2 side, all
  ;; in the same partition (object content) but with differing object
  ;; multisets. common-canonical-multiset's third branch must surface
  ;; the nested-object commonality (here [:object 1]) rather than
  ;; picking one entry arbitrarily.
  (is (= (common-canonical
          (canonicalize '("joe" ([:object 1 2]) ([:object 1 3])))
          (canonicalize '("joe" ([:object 1 4]))))
         (canonicalize '("joe" ([:object 1]))))))

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
            (canonicalize joe-anonymous-object))))
  ;; Plain object extended-by plain object: elements subset.
  (is (canonical-extended-by? [:object {"x" 1}]
                              [:object {"x" 1 "y" 1}]))
  (is (not (canonical-extended-by? [:object {"x" 1 "y" 1}]
                                   [:object {"x" 1}])))
  ;; Plain object extended-by conflux: c1 has no id requirement, so
  ;; conflux c2 satisfies it as long as the elements are a superset.
  (is (canonical-extended-by? [:object {"x" 1}]
                              [:conflux-object (make-tree-id 1) {"x" 1}]))
  ;; Conflux extended-by plain: c1 demands an id that c2 doesn't have.
  (is (not (canonical-extended-by?
            [:conflux-object (make-tree-id 1) {"x" 1}]
            [:object {"x" 1}])))
  ;; Conflux extended-by same-id conflux: elements check.
  (is (canonical-extended-by?
       [:conflux-object (make-tree-id 1) {"x" 1}]
       [:conflux-object (make-tree-id 1) {"x" 1 "y" 1}]))
  ;; Conflux extended-by different-id conflux: id mismatch.
  (is (not (canonical-extended-by?
            [:conflux-object (make-tree-id 1) {"x" 1}]
            [:conflux-object (make-tree-id 2) {"x" 1}])))
  ;; Object vs non-object: not commensurable.
  (is (not (canonical-extended-by? [:object {"x" 1}]
                                   [:source "joe" {"x" 1}])))
  (is (not (canonical-extended-by? "joe"
                                   [:object {}]))))

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
                                               (canonicalize '("fred" "a")))))
  ;; Two plain objects can always be elaborated together.
  (is (canonical-have-common-elaboration? [:object {"x" 1}]
                                          [:object {"y" 1}]))
  ;; A plain object and a conflux-object can be elaborated together
  ;; (the conflux just adds an id constraint).
  (is (canonical-have-common-elaboration?
       [:object {"x" 1}]
       [:conflux-object (make-tree-id 1) {"y" 1}]))
  ;; Same-id conflux-objects can be elaborated together.
  (is (canonical-have-common-elaboration?
       [:conflux-object (make-tree-id 1) {"x" 1}]
       [:conflux-object (make-tree-id 1) {"y" 1}]))
  ;; Different-id conflux-objects can not.
  (is (not (canonical-have-common-elaboration?
            [:conflux-object (make-tree-id 1) {"x" 1}]
            [:conflux-object (make-tree-id 2) {"y" 1}])))
  ;; Object vs non-object: not commensurable.
  (is (not (canonical-have-common-elaboration? [:object {"x" 1}]
                                               [:source "joe" {"x" 1}])))
  (is (not (canonical-have-common-elaboration? "joe" [:object {}]))))

(ns cosheet.server.semantic-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet [orderable :refer [split initial]]
                     [entity :refer [link-type object-type name-label
                                     make-tree-object make-tree-element
                                     make-conflux-tree-object make-tree-id
                                     id->object id->entity to-tree
                                     content all-elements
                                     label->elements label->content]]
                     [store :refer [new-element-store make-item-id
                                    name-label-id add-link]]
                     [store-utils :refer [add-element add-object
                                          add-isolated-element
                                          add-universal-objects
                                          find-object-by-name
                                          link-type-object]]
                     [query :refer [matching-items matching-elements
                                    make-not-query make-variable-query
                                    variable-name]]
                     entity-impl
                     [reporter :refer [reporter-value-or-invalid]]
                     [calculator :refer [request compute make-calculator-data]]
                     [task-queue :refer [make-priority-task-queue]]
                     [canonical :refer [canonicalize]]
                     [test-utils :refer [check as-set]])
            (cosheet
             [test-setup :refer [cyclic-and-shared-a-b-stores]])
            (cosheet.server
             [semantic :refer :all]
             [model-utils :refer [starting-store ordered-tabs-ids-R
                                  tab-topic]])
            ; :reload
            ))

(def orderables (reduce (fn [os _]
                          (vec (concat (pop os)
                                       (split (peek os) :after))))
                        [initial]
                        (range 5)))
(def o1 (nth orderables 0))
(def o2 (nth orderables 1))
(def o3 (nth orderables 2))
(def o4 (nth orderables 3))
(def o5 (nth orderables 4))

;; Before we add Joe, we use an object list for the age label object
;; template. After we add it, we expect to see the actual age label
;; object. This function can make either one.
(defn joe-list-maker
  [age-label-object]
  `("Joe"
    (~o2 :order)
    ("male" (~o1 :order))
    (39 (~o3 :order)
        (~age-label-object (~o3 :order))
        ("doubtful" ("confidence" (~o4 :order))
         (~o4 :order)))
    ("married" (~o2 :order))
    (45 (~o4 :order)
        (~age-label-object (~o3 :order)))))
(def joe-list (joe-list-maker (link-type-object "age")))

(def t1 (add-isolated-element (new-element-store) joe-list))
(def joe-id (second t1))
(def store (first t1))
(def joe (id->entity joe-id store))

(deftest semantic-element?-test
  (is (semantic-element? (make-tree-element 1 [2])))
  (is (semantic-element? (make-tree-element "1" [2])))
  (is (semantic-element? (make-tree-element 'anything [2])))
  (is (semantic-element? (make-tree-element
                          (id->object name-label-id nil)
                          [2])))
  (is (semantic-element? (make-tree-element
                          (make-tree-object [3]) [2])))
  (is (semantic-element? (make-tree-element
                          (make-tree-object [3]) [2] :reverse)))
  (is (not (semantic-element? (make-tree-element :foo [2])))))

(deftest semantic-test
  (let [age-label-obj (find-object-by-name
                       store "age" (link-type-object ""))
        stored-joe-list (joe-list-maker age-label-obj)]
    (is (check (map canonicalize
                    (map to-tree (semantic-elements joe)))
               (as-set (map canonicalize (rest (rest stored-joe-list))))))
    (is (check (canonicalize (semantic-to-tree joe))
               (canonicalize (semantic-to-tree stored-joe-list)))))
  (is (= (semantic-to-tree '(1 (2 (:foo))))
         '(1 2)))
  (is (= (semantic-to-tree (make-tree-element [:object 1] '(2 (:foo)) :reverse))
         (make-tree-element [:object 1] '(2) :reverse)))
  (let [nl (list (id->object name-label-id nil))]
    (is (= (semantic-to-tree `(~(make-tree-object [3 nl :bar]) (2 (:foo))))
           `(~(make-tree-object [3 nl]) 2)))
    (let [named (id->object (make-item-id "A") (new-element-store))]
      (is (= (semantic-to-tree `(~named (2 (:foo))))
             `(~named 2))))
    (is (= (semantic-to-tree `(1 (~(make-tree-object [3 :bar]) (:foo))))
           `(1 (~(make-tree-object [3]))))))
  (let [s (add-universal-objects (new-element-store))
        age-label (make-tree-object `(("age" (~name-label))
                                      (~link-type)))
        [s1 age-label-id] (add-object s age-label)
        age-label (id->object age-label-id s1)
        named-joe-list (make-tree-object
                               `(("Joe" (~name-label))
                                 (59 (~age-label))))
        [store joe-id] (add-object s1 named-joe-list)
        joe (id->object joe-id store)]
    ;; semantic-to-tree shouldn't go inside named objects.
    (is (= (semantic-to-tree joe) joe)))
  ;; semantic-to-tree on a non-interned object that participates in a
  ;; cycle. Without repetition-avoiding-threaded-traverse, this would
  ;; loop forever.
  (let [{:keys [cyclic-shared-store a-id]} (cyclic-and-shared-a-b-stores)
        item-a (id->object a-id cyclic-shared-store)]
    (is (check (semantic-to-tree item-a)
               (as-set
                (make-conflux-tree-object
                 (make-tree-id 1)
                 ["x"
                  `(~(as-set
                      (make-tree-object
                       ["y"
                        `("z" (~(make-conflux-tree-object
                                 (make-tree-id 1) [])))])))]))))))

(deftest semantic-to-tree-excluding-elements-test
  (let [[s obj-id] (add-isolated-element (new-element-store)
                                         `("top"
                                           ("a" (~o1 :order))
                                           ("b" (~o2 :order))))
        obj (id->entity obj-id s)
        a-element (first (matching-elements "a" obj))
        b-element (first (matching-elements "b" obj))]
    ;; Excluding no ids gives the same as semantic-to-tree.
    (is (check (semantic-to-tree-excluding-elements obj #{})
               (as-set (semantic-to-tree obj))))
    ;; Excluding an element's id omits that element.
    (is (check (semantic-to-tree-excluding-elements
                obj #{(:item-id a-element)})
               (as-set '("top" "b"))))
    ;; Excluding all elements leaves just the content.
    (is (check (semantic-to-tree-excluding-elements
                obj #{(:item-id a-element) (:item-id b-element)})
               '("top")))))

(deftest labels-test
  (let [a `("a" (~o1 :order))
        b `("b " "x" (~o2 :order))
        c `(~(link-type-object "c") (~o3 :order))
        d `(~(link-type-object "d") (~o4 :order))
        test-list (list "test" a b c d)
        labels (semantic-label-elements test-list)
        non-labels (semantic-non-label-elements test-list)]
    (is (= (set non-labels) #{a b}))
    (is (= (set labels) #{c d}))))

(deftest selector-semantic-to-tree-test
  (let [unmarked (make-tree-object
                  [`("x" (~o1 :order))
                   `(~(make-tree-object [`("y" (~o2 :order))]) (~o3 :order))])
        [s1 id] (add-object (new-element-store)
                            (mark-template-as-selector unmarked))
        ;; An unmarked element is left out.
        [s2 _] (add-element s1 id `("z" (~o4 :order)))
        ;; A marked element from another object, seen in reverse from
        ;; this one, is left out too, since its :selector isn't :reverse.
        [s3 other-id] (add-object s2 (make-tree-object
                                      [`("other" (~o5 :order))]))
        [s4 link-id] (add-link s3 other-id id)
        [s5 _] (add-link s4 link-id :selector)
        object (id->object id s5)]
    (is (seq (matching-elements "z" object)))
    (is (check (canonicalize (selector-semantic-to-tree object))
               (canonicalize (semantic-to-tree unmarked))))
    ;; From the other object, the marked link is followed, and the
    ;; object it reaches is expanded the same way, while the other
    ;; object's own unmarked element is left out.
    (is (check (canonicalize (selector-semantic-to-tree
                              (id->object other-id s5)))
               (canonicalize (make-tree-object
                              [(list (semantic-to-tree unmarked))]))))))

(deftest transform-pattern-toward-fixed-term-test
  (let [pattern (make-tree-element
                 [:object]
                 ['anything
                  `(~(make-tree-object `((~link-type))))
                  `(~(make-tree-object `("a")))]
                 :reverse)]
    (is (check (transform-pattern-toward-fixed-term
                pattern {})
               (make-tree-element
                 [:object]
                 [nil
                  `(~(make-tree-object `((~link-type))))
                  `(~(make-tree-object `("a")))]
                 :reverse)))
    (is (check (transform-pattern-toward-fixed-term
                pattern {:require-not-type true})
               (make-tree-element
                 (make-tree-object `(~(make-not-query `(~link-type))
                                     ~(make-not-query `(~object-type))))
                 [nil
                  `(~(make-tree-object `((~link-type))))
                  `(~(make-tree-object `("a"
                                         ~(make-not-query `(~link-type))
                                         ~(make-not-query `(~object-type)))))]
                 :reverse)))
    (is (check (transform-pattern-toward-fixed-term
                pattern {:require-not-type true
                         :require-semantic true})
               (make-tree-element
                 (make-tree-object `(~(make-not-query `(~link-type))
                                     ~(make-not-query `(~object-type))
                                     (nil :order)
                                     ~(make-not-query '(:non-semantic))))
                 ['(nil (nil :order))
                  `(~(make-tree-object
                      `((~link-type) (nil :order)
                        ~(make-not-query '(:non-semantic)))))
                  `(~(make-tree-object `("a"
                                         ~(make-not-query `(~link-type))
                                         ~(make-not-query `(~object-type))
                                         (nil :order)
                                         ~(make-not-query '(:non-semantic)))))]
                 :reverse)))
  ;; In cyclic-shared-store, a is reachable via two paths (the back-
  ;; link from b and z-link's sub-element), so to-tree preserves a as
  ;; a conflux-tree-object. transform-pattern-toward-fixed-term
  ;; substitutes each occurrence with a reference variable (the first
  ;; carries a qualifier whose elements are the conflux's elements;
  ;; the second uses just the name). Using the term as a query against
  ;; the store should still pick out item-a.
  (let [{:keys [cyclic-shared-store a-id]} (cyclic-and-shared-a-b-stores)
        item-a (id->object a-id cyclic-shared-store)
        pattern (to-tree item-a)
        fixed-term (transform-pattern-toward-fixed-term pattern {})
        v-name (label->content fixed-term :cosheet.query/name)
        inner-var (make-variable-query v-name :reference true :form :object)]
    ;; The element order of the stored object is incidental, so
    ;; compare canonical forms.
    (is (= (canonicalize fixed-term)
           (canonicalize
            (make-variable-query
             v-name
             :qualifier (make-tree-object
                         [(make-tree-element
                           (make-tree-object
                            [(make-tree-element
                              "z"
                              [(make-tree-element inner-var [])])
                             "y"])
                           [])
                          "x"])
             :reference true))))
    (is (= [item-a] (matching-items fixed-term cyclic-shared-store))))
  ;; A conflux-tree-object whose elements include a back-reference to
  ;; itself. The back-reference shows up inside the qualifier as the
  ;; same reference variable.
  (let [pattern (make-conflux-tree-object
                 (make-tree-id 1)
                 ["x"
                  `(~(make-conflux-tree-object (make-tree-id 1) []))])
        fixed-term (transform-pattern-toward-fixed-term pattern {})
        v-name (variable-name fixed-term)
        inner-var (make-variable-query v-name :reference true :form :object)]
    (is (= fixed-term
           (make-variable-query
            v-name
            :qualifier (make-tree-object
                        ["x" (make-tree-element inner-var [])])
            :reference true))))
  ;; A second reference to a conflux tree object, outside of the first
  ;; one. Both should have the qualifier.
  (let [pattern `("y"
                  ~(make-conflux-tree-object (make-tree-id 1) ["x"])
                  ~(make-conflux-tree-object (make-tree-id 1) []))
        fixed-term (transform-pattern-toward-fixed-term pattern {})
        variable (content (first (all-elements fixed-term)))]
    (is (check variable
               (make-variable-query
                (variable-name variable)
                :qualifier (make-tree-object ["x"])
                :reference true)))
    (is (check fixed-term
               `("y"
                 ~(make-tree-element variable [])
                 ~(make-tree-element variable [])))))
  ;; All changes should apply to the qualifier elements.
  (let [pattern (make-conflux-tree-object (make-tree-id 1) ['anything "x"])
        fixed-term (transform-pattern-toward-fixed-term
                    pattern {:require-semantic true})
        v-name (label->content fixed-term :cosheet.query/name)]
    (is (= fixed-term
           (make-variable-query
            v-name
            :qualifier (make-tree-object ['(nil (nil :order)) "x"])
            :reference true))))))

(deftest selector?-test
  (let [[s1 selector-root-id] (add-isolated-element
                               (starting-store "starting-tab")
                               `("thing" :selector
                                         ("child" (1 :order)
                                                  ("grandchild" :selector)
                                                  :selector)
                                 (~(make-tree-object [4 :selector]) "object")))
        [s non-selector-root-id] (add-isolated-element
                                  s1
                                  `("thing" ("child" (1 :order)
                                                      "grandchild")
                                            (~(make-tree-object [4]) "object")))
        selector-root (id->entity selector-root-id s)
        selector-child (first (matching-elements "child" selector-root))
        selector-grandchild (first (matching-elements "grandchild"
                                                      selector-child))
        selector-object (content (first (matching-elements
                                         `(~(make-tree-object []))
                                         selector-root)))
        non-selector-root (id->entity non-selector-root-id s)
        non-selector-child (first (matching-elements "child" non-selector-root))
        non-selector-grandchild (first (matching-elements "grandchild"
                                                          non-selector-child))
        non-selector-object (content (first (matching-elements
                                             `(~(make-tree-object []))
                                             non-selector-root)))
        ordered-tab-ids (ordered-tabs-ids-R s)
        cd (make-calculator-data (make-priority-task-queue 0))]
    (request ordered-tab-ids cd)
    (compute cd)
    (let [first-tab (id->entity
                     (first (reporter-value-or-invalid ordered-tab-ids)) s)]
      (is (selector? (content (first (label->elements (tab-topic first-tab)
                                                      :row-condition))))))
    (is (selector? selector-root))
    (is (selector? selector-child))
    (is (selector? selector-grandchild))
    (is (selector? selector-object))
    (is (not (selector? non-selector-root)))
    (is (not (selector? non-selector-child)))
    (is (not (selector? non-selector-grandchild)))
    (is (not (selector? non-selector-object)))))

(deftest fixed-term-to-template-test
  ;; nil becomes the replacement, (nil :order) elements and negations
  ;; are removed.
  (is (= (fixed-term-to-template
          `(nil "x" (nil :order) ~(make-not-query 5)))
         '("" "x")))
  (is (= (fixed-term-to-template
          `(nil "x" (nil :order) ~(make-not-query 5)) 'anything)
         '(anything "x")))
  ;; A reference variable becomes a conflux tree object whose elements
  ;; match the variable's qualifier. This round-trips a conflux with a
  ;; cyclic back-reference to itself.
  (let [pattern (make-conflux-tree-object
                 (make-tree-id 1)
                 ["x" `(~(make-conflux-tree-object (make-tree-id 1) []))])
        fixed-term (transform-pattern-toward-fixed-term pattern {})]
    (is (check (canonicalize (fixed-term-to-template fixed-term))
               (canonicalize pattern))))
  ;; Two separate references to the same conflux share an identity.
  (let [pattern `("y"
                  ~(make-conflux-tree-object (make-tree-id 1) ["x"])
                  ~(make-conflux-tree-object (make-tree-id 1) []))
        fixed-term (transform-pattern-toward-fixed-term pattern {})]
    (is (check (canonicalize (fixed-term-to-template fixed-term))
               (canonicalize pattern))))
  ;; A non-reference variable is replaced by its qualifier, or by the
  ;; replacement if it has none.
  (is (= (fixed-term-to-template (make-variable-query "v" :qualifier '(2 3)))
         '(2 3)))
  (is (= (fixed-term-to-template (make-variable-query "v" :form :element))
         '("")))
  (is (= (fixed-term-to-template (make-variable-query "v" :form :element)
                                 'anything)
         '(anything)))
  (is (= (fixed-term-to-template
          `(1 ~(make-variable-query "v" :qualifier '(2 3))))
         '(1 (2 3)))))

(deftest add-non-selector-to-fixed-term-test
  (is (check (add-non-selector-to-fixed-term
              (make-tree-object [`(~(link-type-object "hi"))]))
             (make-tree-object [`(~(link-type-object "hi"))
                                (make-not-query '(:selector))])))
  (is (check (add-non-selector-to-fixed-term
              `(nil (~(link-type-object "hi"))))
             `(nil (~(link-type-object "hi"))
                   ~(make-not-query '(:selector))))))

(deftest mark-template-as-selector-test
  (let [template (make-tree-object
                  [`("age" "years")
                   (make-tree-element (make-tree-object [1]) [] :reverse)
                   `(~(make-tree-object [`("Fred" (~name-label))]))])]
    (is (check (mark-template-as-selector template)
               (make-tree-object
                ['("age" ("years" :selector) :selector)
                 (make-tree-element
                  (make-tree-object ['(1 :selector) :selector])
                  ['(:selector :reverse)]
                  :reverse)
                 ;; The "Fred" object should not be marked as a selector,
                 ;; since it is named.
                 `(~(make-tree-object [`("Fred" (~name-label))]) :selector)
                 :selector]))))
  ;; A primitive template becomes a marked element.
  (is (check (mark-template-as-selector 'anything) '(anything :selector)))
  (is (check (mark-template-as-selector "") '("" :selector)))
  ;; A non-semantic element is not marked, and its sub-elements are not
  ;; visited, so a semantic sub-element it contains is left unmarked.
  (is (check (mark-template-as-selector
              (make-tree-object [`(:tag ("deep" "leaf"))]))
             (make-tree-object
              ['(:tag ("deep" "leaf"))
               :selector])))
  ;; The traversal uses repetition-avoiding-threaded-traverse, so it
  ;; terminates on a template that shares an object across paths. Here a,
  ;; carrying "x", is referenced twice from b: once as z's content and
  ;; once via a reverse link. Both references share one conflux id, and
  ;; the shared object's elements are marked at its first occurrence.
  (let [{:keys [cyclic-shared-store b-id]} (cyclic-and-shared-a-b-stores)
        template (to-tree (id->object b-id cyclic-shared-store))
        a (make-conflux-tree-object
           (make-tree-id 1)
           ['("x" :selector) :selector])
        a-ref (make-conflux-tree-object (make-tree-id 1) [:selector])]
    (is (check (mark-template-as-selector template)
               (as-set
                (make-tree-object
                 [`("z" (~a :selector) :selector)
                  '("y" :selector)
                  (make-tree-element a-ref ['(:selector :reverse)] :reverse)
                  :selector]))))))

(deftest replace-anythings-with-empty-test
  (is (= (replace-anythings-with-empty 'anything) ""))
  (is (= (replace-anythings-with-empty "x") "x"))
  (is (check (replace-anythings-with-empty
              `(~'anything (~'anything 5) "x"))
             '("" ("" 5) "x")))
  (is (check (replace-anythings-with-empty
              (make-tree-object ['anything `(~'anything 3)]))
             (make-tree-object ["" '("" 3)])))
  ;; An 'anything that is the content of a reversed element becomes an
  ;; empty object.
  (is (check (replace-anythings-with-empty
              (make-tree-object
               [(make-tree-element 'anything ["x"] :reverse)]))
             (make-tree-object
              [(make-tree-element (make-tree-object []) ["x"] :reverse)]))))

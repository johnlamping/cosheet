(ns cosheet.server.model-utils-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet [orderable :refer [split initial]]
                      [entity :refer [in-different-store stored-entity?
                                      link-type object-type name-label
                                      make-tree-object make-tree-element
                                      make-conflux-tree-object make-tree-id
                                      object?
                                      recursively-in-different-store
                                      id->object id->entity
                                      label->elements label->content
                                      content->elements
                                      content elements
                                      to-tree]]
                      [orderable :as orderable]
                      [store :refer [new-element-store update-source
                                     make-item-id]]
                      [store-utils :refer [add-element add-object
                                           add-universal-objects
                                           remove-entity-by-id
                                           find-object-by-name
                                           object-type-object
                                           link-type-object]]
                      [query :refer [matching-items matching-elements
                                     not-query variable-query variable-name]]
                      entity-impl
                      [reporter :refer [reporter-value-or-invalid]]
                      [calculator :refer [request compute make-calculator-data]]
                      [task-queue :refer [make-priority-task-queue]]
                      [canonical :refer [canonicalize]]
                      [debug :refer [simplify-for-print]]
                      [test-utils :refer [check any as-set]])
            (cosheet
             [test-setup :refer [cyclic-and-shared-a-b-stores]])
            (cosheet.server
             [model-utils :refer :all]
             [order-utils :refer [ordered-entities]])
            ; :reload
            ))

(def orderables (reduce (fn [os _]
                          (vec (concat (pop os)
                                       (split (peek os) :after))))
                        [initial]
                        (range 8)))
(def o1 (nth orderables 0))
(def o2 (nth orderables 1))
(def o3 (nth orderables 2))
(def o4 (nth orderables 3))
(def o5 (nth orderables 4))
(def o6 (nth orderables 5))
(def o7 (nth orderables 6))
(def unused-orderable (nth orderables 7))

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

(def t1 (add-element (new-element-store) nil joe-list))
(def joe-id (second t1))
(def store (first t1))
(def joe (id->entity joe-id store))

(deftest transform-pattern-toward-fixed-term-test
  (let [pattern (make-tree-element
                 :target
                 'anything
                 ['anything
                  `(~(make-tree-object `((~link-type))))
                  `(~(make-tree-object `("a")))])]
    (is (check (transform-pattern-toward-fixed-term
                pattern {})
               (make-tree-element
                 :target
                 nil
                 [nil
                  `(~(make-tree-object `((~link-type))))
                  `(~(make-tree-object `("a")))])))
    (is (check (transform-pattern-toward-fixed-term
                pattern {:require-not-type true})
               (make-tree-element
                 :target
                 nil
                 [nil
                  `(~(make-tree-object `((~link-type))))
                  `(~(make-tree-object `("a"
                                         ~(not-query `(~link-type))
                                         ~(not-query `(~object-type)))))])))
    (is (check (transform-pattern-toward-fixed-term
                pattern {:require-not-type true
                         :require-orders true})
               (make-tree-element
                 :target
                 nil
                 ['(nil (nil :order))
                  `(~(make-tree-object `((~link-type) (nil :order))))
                  `(~(make-tree-object `("a"
                                         ~(not-query `(~link-type))
                                         ~(not-query `(~object-type))
                                         (nil :order))))
                  '(nil :order)])))
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
        inner-var (variable-query v-name :reference true)]
    (is (= fixed-term
           (variable-query
            v-name
            :qualifier (make-tree-object
                        [(make-tree-element
                          :source
                          (make-tree-object
                           [(make-tree-element
                             :source "z"
                             [(make-tree-element :source inner-var [])])
                            "y"])
                          [])
                         "x"])
            :reference true)))
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
        inner-var (variable-query v-name :reference true)]
    (is (= fixed-term
           (variable-query
            v-name
            :qualifier (make-tree-object
                        ["x" (make-tree-element :source inner-var [])])
            :reference true))))
  ;; A second reference to a conflux tree object, outside of the first
  ;; one. Both should have the qualifier.
  (let [pattern `("y"
                  ~(make-conflux-tree-object (make-tree-id 1) ["x"])
                  ~(make-conflux-tree-object (make-tree-id 1) []))
        fixed-term (transform-pattern-toward-fixed-term pattern {})
        variable (content (first (elements fixed-term)))]
    (is (check variable
               (variable-query
                (variable-name variable)
                :qualifier (make-tree-object ["x"])
                :reference true)))
    (is (check fixed-term
               `("y"
                 ~(make-tree-element :source variable [])
                 ~(make-tree-element :source variable [])))))
  ;; All changes should apply to the qualifier elements.
  (let [pattern (make-conflux-tree-object (make-tree-id 1) ['anything "x"])
        fixed-term (transform-pattern-toward-fixed-term
                    pattern {:require-orders true})
        v-name (label->content fixed-term :cosheet.query/name)]
    (is (= fixed-term
           (variable-query
            v-name
            :qualifier (make-tree-object ['(nil (nil :order)) "x"])
            :reference true))))))

(deftest add-non-selector-to-fixed-term-test
  (is (check (add-non-selector-to-fixed-term
              (make-tree-object [`(~(link-type-object "hi"))]))
             (make-tree-object [`(~(link-type-object "hi"))
                                (not-query '(:selector))])))
  (is (check (add-non-selector-to-fixed-term
              `(nil (~(link-type-object "hi"))))
             `(nil (~(link-type-object "hi"))
                   ~(not-query '(:selector))))))

(deftest specialize-generic-test
  (let [[c1 s1] (specialize-generic '("x" (??? :a) (??? 22))
                                     (new-element-store))
        [c2 s2] (specialize-generic '("x" (??? "y") (??? "22"))
                                     s1)]
    (is (= c1  '("x" ("\u00A0A" :a) ("\u00A0B" 22))))
    (is (= c2  '("x" ("\u00A0C" "y") ("\u00A0D" "22"))))))

(deftest template-to-possible-non-selector-template-test
  (is (check (template-to-possible-non-selector-template 'anything)
             ""))
  (is (check (template-to-possible-non-selector-template '(anything 2))
             '("" 2)))
  (is (check (template-to-possible-non-selector-template '(2 anything))
             '(2 "")))
  (is (check (template-to-possible-non-selector-template
              '(anything 2 :selector))
             '(anything 2 :selector)))
  (is (check (template-to-possible-non-selector-template
              '(2 anything :selector))
             '(2 anything :selector)))
  (is (check (template-to-possible-non-selector-template
              (make-tree-object '(2 anything)))
             (make-tree-object '(2 ""))))
  (is (check (template-to-possible-non-selector-template
              (make-tree-object '(2 anything  :selector)))
             (make-tree-object '(2 anything :selector))))
  (is (check (template-to-possible-non-selector-template
              `(~label-object-template))
             `(~(make-tree-object [`("" (~name-label)) `(~link-type)])))))

(deftest semantic-test
  (let [age-label-obj (find-object-by-name
                       store "age" (link-type-object ""))
        stored-joe-list (joe-list-maker age-label-obj)
        expected (ordered-semantic-to-tree stored-joe-list)]
    (is (check (map canonicalize
                    (map to-tree (semantic-elements joe)))
               (as-set (map canonicalize (rest (rest stored-joe-list))))))
    (is (check (canonicalize (semantic-to-tree joe))
               (canonicalize expected)))
    (is (check (ordered-semantic-to-tree joe)
               expected)))
  (let [removed (remove-semantic-elements store joe-id)
        removed-joe (id->entity joe-id removed)]
    (is (check (to-tree removed-joe)
               `("Joe" (~(any) :order)))))
  (is (= (semantic-to-tree '(1 (2 (:foo))))
         '(1 2)))
  (is (= (semantic-to-tree (make-tree-element :target 1 '(2 (:foo))))
         (make-tree-element :target 1 '(2))))
  (is (= (semantic-to-tree `(~(make-tree-object [3 :name :bar]) (2 (:foo))))
         `(~(make-tree-object [3 :name]) 2)))
  (let [named (id->object (make-item-id "A") (new-element-store))]
    (is (= (semantic-to-tree `(~named (2 (:foo))))
           `(~named 2))))
  (is (= (semantic-to-tree `(1 (~(make-tree-object [3 :bar]) (:foo))))
         `(1 (~(make-tree-object [3])))))
  (is (= (ordered-semantic-to-tree
          `(~(make-tree-object [3 :name :bar]) (2 (:foo))))
         `(~(make-tree-object [3 :name]) 2)))
  (is (= (ordered-semantic-to-tree
          `(1 (~(make-tree-object [3 :bar]) (:foo))))
         `(1 (~(make-tree-object [3])))))
  (let [s (add-universal-objects (new-element-store))
        age-label (make-tree-object `(("age" (~name-label))
                                      (~link-type)))
        ;; We use add-object, rather than update-add-object-with-order,
        ;; which hasn't been tested at this point.
        [s1 age-label-id] (add-object s age-label)
        age-label (id->object age-label-id s1)
        named-joe-list (make-tree-object
                               `(("Joe" (~name-label))
                                 (59 (~age-label))))
        [store joe-id] (add-object s1 named-joe-list)
        joe (id->object joe-id store)]
    ;; semantic-to-tree shouldn't go inside named objects.
    (is (= (semantic-to-tree joe) joe))
    (is (= (ordered-semantic-to-tree joe) joe))
    ;; object-semantic-to-tree should go inside a named object.
    (is (check
         (object-semantic-to-tree joe)
         ;; We can't be sure of the order of elements.
         (as-set (recursively-in-different-store named-joe-list store)))))
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

(deftest is-selector-test
  (let [[s1 selector-root-id] (add-element
                               (starting-store "starting-tab") nil
                               `("thing" :selector
                                         ("child" (1 :order)
                                                  "grandchild")
                                 (~(make-tree-object [4]) "object")))
        [s non-selector-root-id] (add-element
                                  s1 nil
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
      (is (selector? (content (first (label->elements
                                      (first (label->elements first-tab
                                                              :tab-topic))
                                      :row-condition))))))
    (is (selector? selector-root))
    (is (selector? selector-child))
    (is (selector? selector-grandchild))
    (is (selector? selector-object))
    (is (not (selector? non-selector-root)))
    (is (not (selector? non-selector-child)))
    (is (not (selector? non-selector-grandchild)))
    (is (not (selector? non-selector-object)))))

(deftest match-terms-and-targets-test
  (is (check (match-terms-and-targets [1 2 3] [2 3 4])
             [(as-set [[3 3] [2 2]]) [1] [4]]))
  (is (check (match-terms-and-targets [1 2 '(2 6) 3] ['(2 6) 2 3 4])
             [(as-set[['(2 6) '(2 6)] [3 3] [2 2]]) [1] [4]]))
  (is (check (match-terms-and-targets [1 2 '(nil 6) 3] ['(2 6) 2 3 4])
             [(as-set[['(nil 6) '(2 6)] [3 3] [2 2]]) [1] [4]]))
  (is (check (match-terms-and-targets [1 '(nil 6) 3] ['(2 6) 2 3 4])
             [(as-set[['(nil 6) '(2 6)] [3 3]]) [1] [2 4]]))
  (is (check (match-terms-and-targets [1 2 3] ['(2 6) 2 3 4])
             [(as-set [[2 2] [3 3]]) [1] (as-set ['(2 6) 4])]))
  (is (check (match-terms-and-targets [1 '(nil 6) 3] [2 3 4])
             [[[3 3]] (as-set [1 '(nil 6)]) (as-set [2 4])])))

(deftest elements-to-change-to-satisfy-fixed-term-elements-test
  (is (check (elements-to-change-to-satisfy-fixed-term-elements
              (make-tree-object [1 2 3]) (make-tree-object [2 3 4]))
             [[1] []]))
  (is (check (elements-to-change-to-satisfy-fixed-term-elements
              (make-tree-object ['(nil 1) 2 3]) (make-tree-object [2 3 4]))
             [['("" 1)] []]))
  (is (check (elements-to-change-to-satisfy-fixed-term-elements
              (make-tree-object [2 '(nil 1) '(3 4)])
              (make-tree-object ['(2 1 3) 3]))
             [(as-set ['(3 4) 2]) [3]]))
  (is (check (elements-to-change-to-satisfy-fixed-term-elements
              (make-tree-object [2 '(nil 1) 3])
              (make-tree-object ['(2 5) 4]))
             [(as-set [3 '("" 1)]) []])))

(deftest get-or-make-ordered-object-by-name-test
  (let
      ;; First, make a new object.
      [[s1 id1 order1] (get-or-make-ordered-object-by-name
                        store
                        "Tina" (make-tree-object [1 2])
                        unused-orderable :before false)
       ;; Ask for it again.
       [s2 id2 order2] (get-or-make-ordered-object-by-name
                        s1
                        "Tina" (make-tree-object [1 2])
                        order1 :before false)
       ;; Ask for it again, with fewer required elements.
       [s3 id3 order3] (get-or-make-ordered-object-by-name
                        s2
                        "Tina" (make-tree-object [1])
                        order2 :before false)
       ;; Ask for it, with no additional required elements.
       [s4 id4 order4] (get-or-make-ordered-object-by-name
                        s3
                        "Tina" (make-tree-object [])
                        order3 :before false)
       ;; Ask for it, with different required elements.
       [s5 id5 order5] (get-or-make-ordered-object-by-name
                        s4
                        "Tina" (make-tree-object [2 '(3 4)])
                        order4 :before false)
       ;; Ask for it, with elements that have some commonality with
       ;; existing ones.
       [s6 id6 order6] (get-or-make-ordered-object-by-name
                        s5
                        "Tina" (make-tree-object ['(1 2) 3])
                        order5 :before false)
       ;; Ask for an object with a different name than existing ones.
       [s7 id7 order7] (get-or-make-ordered-object-by-name
                        s6
                        "Tony" (make-tree-object [])
                        order6 :before false)]
    
    ;; The new Tina object should match the template.
    (is (check (object-semantic-to-tree (id->object id1 s1))
               (as-set (recursively-in-different-store
                        (make-tree-object [`("Tina" (~name-label)) 1 2])
                        s1))))
    ;; Nothing should have changed when it was asked for again.
    (is (= s1 s2))
    (is (= id1 id2))
    (is (= order1 order2))
    ;; Nothing should have changed when it was asked for with fewer elements.
    (is (= s1 s3))
    (is (= id1 id3))
    (is (= order1 order3))
    ;; Nothing should have changed when it was asked for with no elements.
    (is (= s1 s4))
    (is (= id1 id4))
    (is (= order1 order4))
    ;; The Tina object should have gotten an additional element so it
    ;; matches the additional template.
    (is (= id1 id5))
    (is (check (object-semantic-to-tree (id->object id1 s5))
               (as-set (recursively-in-different-store
                        (make-tree-object [`("Tina" (~name-label)) 1 2 '(3 4)])
                        s5))))
    ;; The Tina object should have gotten rid of a redundant element.
    (is (= id1 id6))
    (is (check (object-semantic-to-tree (id->object id1 s6))
               (as-set (recursively-in-different-store
                        (make-tree-object
                         [`("Tina" (~name-label)) '(1 2) 2 '(3 4)])
                        s6))))
    ;; The new Tony object should match its (empty) template.
    (is (not= id1 id7))
    (is (check (object-semantic-to-tree (id->object id7 s7))
               (as-set (recursively-in-different-store
                        (make-tree-object [`("Tony" (~name-label))])
                        s7))))))

(deftest update-add-element-with-order-and-ephemeral-test
  (let [[s id order] (update-add-element-with-order-and-ephemeral
                      store joe-id 6
                      unused-orderable :before true)
        joe (id->entity joe-id s)
        new-entity (first (matching-elements 6 joe))
        [o5 o6] (orderable/split unused-orderable :before)]
    (is (= (to-tree new-entity)
           `(6 (~o5 :order))))
    (is (= order o6))
    (is (= (:item-id new-entity) id)))
  (let [[s id order] (update-add-element-with-order-and-ephemeral
                      store joe-id 6
                      unused-orderable :before false)
        joe (id->entity joe-id s)
        new-entity (first (matching-elements 6 joe))
        [o5 o6] (orderable/split unused-orderable :after)]
    (is (= (to-tree new-entity)
           `(6 (~o5 :order))))
    (is (= order o6))
    (is (= (:item-id new-entity) id)))    
  (let [[s id order] (update-add-element-with-order-and-ephemeral
                      store joe-id 6
                      unused-orderable :after true)
        joe (id->entity joe-id s)
        new-entity (first (matching-elements 6 joe))
        [o5 o6] (orderable/split unused-orderable :after)]
    (is (= (to-tree new-entity)
           `(6 (~o6 :order))))
    (is (= order o5))
    (is (= (:item-id new-entity) id)))
  (let [[s id order] (update-add-element-with-order-and-ephemeral
                      store joe-id `(6 (~(link-type-object "height")))
                      unused-orderable :before true)
        joe (id->entity joe-id s)
        height-label-obj (find-object-by-name
                          s "height" (link-type-object ""))
        new-entity (first (label->elements joe height-label-obj))]
    (is (= (:item-id new-entity) id))
    (is (check (to-tree new-entity)
               (as-set `(6 (~(any) :order)
                           (~height-label-obj (~(any) :order)))))))
   ;; Try adding something that requires adding an non-identified object.
  (let [[s id order] (update-add-element-with-order-and-ephemeral
                      store joe-id `(6 (~(make-tree-object
                                          [1 2])))
                      unused-orderable :before true)
        joe (id->entity joe-id s)
        new-entity (first (content->elements joe 6))]
    (is (= (:item-id new-entity) id))
    (is (check (ordered-semantic-to-tree new-entity)
               `(6 (~(make-tree-object [1 2]))))))
  ;; Try adding something that requires adding an identified object.
  (let [[s id order] (update-add-element-with-order-and-ephemeral
                      store joe-id `(6 (~(make-tree-object
                                          [`("Tina" (~name-label)) 1 2])))
                      unused-orderable :before true)
        joe (id->entity joe-id s)
        new-entity (first (content->elements joe 6))
        tina (find-object-by-name s "Tina" (make-tree-object nil))]
    (is (= (:item-id new-entity) id))
    (is (check (ordered-semantic-to-tree new-entity)
               `(6 (~tina))))
    (is (check (object-semantic-to-tree tina)
               (as-set (recursively-in-different-store
                        (make-tree-object [`("Tina" (~name-label)) 1 2])
                        s))))
    ;; Now try adding another element that references the same object.
    (let [[s1 id1 order1] (update-add-element-with-order-and-ephemeral
                           s joe-id `(7 (~(make-tree-object
                                           [`("Tina" (~name-label)) 2 3])))
                           order :before true)
          joe (id->entity joe-id s1)
          new-entity (first (content->elements joe 7))
          tina (find-object-by-name s1 "Tina" (make-tree-object nil))]
      (is (= (:item-id new-entity) id1))
      (is (check (ordered-semantic-to-tree new-entity)
                 `(7 (~tina))))
      (is (check (object-semantic-to-tree tina)
                 (as-set
                  (recursively-in-different-store
                   ;; Tina should have gotten an extra property.
                   (make-tree-object [`("Tina" (~name-label)) 1 2 3])
                   s1))))))
  
  ;; Check that order in the list style entity is preserved in the
  ;; :order values.
  ;; Also check and that non-semantic elements don't get order information
  ;; and that the entity is marked ephemeral, if requested.
  (let [[s id order] (update-add-element-with-order-and-ephemeral
                      store joe-id `(6 (~(link-type-object "height"))
                                       (~(link-type-object "other"))
                                       :ephemeral
                                       (:other-keyword ""))
                      unused-orderable :after false)
        joe (id->entity joe-id s)
        height-label-obj (find-object-by-name
                          s "height" (link-type-object ""))
        other-label-obj (find-object-by-name
                          s "other" (link-type-object ""))
        new-entity (first (label->elements joe height-label-obj))]
    (is (check (to-tree new-entity)
               (as-set
                `(6 (~(any) :order)
                    (~height-label-obj (~(any) :order))
                    (~other-label-obj (~(any) :order))
                    :ephemeral
                    (:other-keyword "")))))
    (is ((:ephemeral-ids s) id))
    (is (= (:item-id new-entity) id))))

(deftest cycle-detection-test
  ;; Create two non-interned objects A and B with a single link A -> B
  ;; in the store. Because elements include reversed links between
  ;; objects, A's elements list contains B and B's elements list
  ;; contains A. Without cycle detection, walking the template would
  ;; bounce between them forever, reversing direction each time.
  (let [s0 (new-element-store)
        [s1 a-id] (add-object s0 (make-tree-object []))
        [s2 b-id] (add-object s1 (make-tree-object []))
        [s3 _] (add-element s2 a-id `(~(id->object b-id s2)))
        ;; A primitive anchor (with order) for the adjacent-to variant.
        [s4 anchor-id] (add-element s3 nil
                                    `("anchor" (~unused-orderable :order)))
        a-template (id->object a-id s4)]
    ;; First case: copy a-template with update-add-object-with-order.
    ;; Without cycle detection, this call would not return.
    (let [[s5 new-a-id _]
          (update-add-object-with-order
           s4 a-template unused-orderable :before false)
          new-a (id->entity new-a-id s5)
          new-b (content (first (filter #(object? (content %))
                                        (elements new-a))))
          new-b-id (:item-id new-b)]
      (is (not= new-a-id a-id))
      ;; new-a's to-tree: an object with new-a's own :order and the
      ;; forward link to new-b. new-b only contributes its :order here
      ;; because the reverse link back to new-a is skipped as the
      ;; parent element.
      (is (check (to-tree new-a)
                 (as-set (make-tree-object
                          [`(~(any) :order)
                           `(~(as-set (make-tree-object
                                       [`(~(any) :order)]))
                             (~(any) :order))]))))
      (is (not= new-b-id b-id))
      ;; new-b's to-tree is symmetric: the back-pointer to new-a is
      ;; rendered as a (:target ...) element.
      (is (check (to-tree (id->entity new-b-id s5))
                 (as-set (make-tree-object
                          [`((:target ~(as-set (make-tree-object
                                                [`(~(any) :order)])))
                             (~(any) :order))
                           `(~(any) :order)])))))
    ;; Second case: same A->B setup, but exercising
    ;; update-add-object-adjacent-to and using B as the template, so
    ;; the recursion traverses the link in the opposite order (from B
    ;; through its reverse-link view to A).
    (let [anchor (id->entity anchor-id s4)
          b-template (id->object b-id s4)
          [s5 new-b-id]
          (update-add-object-adjacent-to s4 b-template anchor :before false)
          new-b (id->entity new-b-id s5)
          new-a-id (:item-id (content (first (filter #(object? (content %))
                                                     (elements new-b)))))]
      (is (not= new-b-id b-id))
      ;; Because the element's :target orientation is honored, the new
      ;; link is added in the same direction as the original A->B
      ;; link, so new-b's to-tree is structurally the same as case 1's
      ;; new-b: the back-pointer to new-a appears with (:target ...).
      (is (check (to-tree new-b)
                 (as-set (make-tree-object
                          [`((:target ~(as-set (make-tree-object
                                                [`(~(any) :order)])))
                             (~(any) :order))
                           `(~(any) :order)]))))
      (is (not= new-a-id a-id))))
  ;; Three non-interned objects A, B, C connected A->B->C. The
  ;; bidirectional element view exposes the chain in both directions
  ;; (A<->B<->C); without cycle detection the recursion would bounce
  ;; between adjacent pairs forever. (A fully-closed triangle of
  ;; non-interned objects isn't allowed by the store: it disallows new
  ;; links between two non-interned objects that both already have
  ;; links to other non-interned objects.)
  (let [s0 (new-element-store)
        [s1 a-id] (add-object s0 (make-tree-object []))
        [s2 b-id] (add-object s1 (make-tree-object []))
        [s3 c-id] (add-object s2 (make-tree-object []))
        [s4 _] (add-element s3 a-id `(~(id->object b-id s3)))
        [s5 _] (add-element s4 b-id `(~(id->object c-id s4)))
        a-template (id->object a-id s5)
        [s6 new-a-id _]
        (update-add-object-with-order
         s5 a-template unused-orderable :before false)
        new-a (id->entity new-a-id s6)
        new-b (content (first (filter #(object? (content %))
                                      (elements new-a))))
        new-b-id (:item-id new-b)
        ;; The two object-content elements of new-b are new-a and
        ;; new-c (in some order).
        new-b-obj-ids (set (map #(:item-id (content %))
                                (filter #(object? (content %))
                                        (elements new-b))))
        new-c-id (first (disj new-b-obj-ids new-a-id))
        new-c (id->entity new-c-id s6)]
    (is (not= new-a-id a-id))
    (is (not= new-b-id b-id))
    (is (not= new-c-id c-id))
    (is (distinct? new-a-id new-b-id new-c-id))
    (is (contains? new-b-obj-ids new-a-id))
    ;; new-a's to-tree walks the chain forward. At each step the
    ;; reverse link back to the parent is filtered out.
    (is (check
         (to-tree new-a)
         (as-set (make-tree-object
                  [`(~(any) :order)
                   `(~(as-set (make-tree-object
                               [`(~(any) :order)
                                `(~(as-set (make-tree-object
                                            [`(~(any) :order)]))
                                  (~(any) :order))]))
                     (~(any) :order))]))))
    ;; new-b's to-tree shows both directions: the (:target ...) entry
    ;; is the reverse link to new-a; the bare object entry is the
    ;; forward link to new-c.
    (is (check
         (to-tree new-b)
         (as-set (make-tree-object
                  [`((:target ~(as-set (make-tree-object
                                        [`(~(any) :order)])))
                     (~(any) :order))
                   `(~(any) :order)
                   `(~(as-set (make-tree-object [`(~(any) :order)]))
                     (~(any) :order))]))))
    ;; new-c's to-tree walks the chain backward; the back-pointer to
    ;; new-b appears as (:target ...), and inside that new-b the
    ;; further back-pointer to new-a appears the same way.
    (is (check
         (to-tree new-c)
         (as-set (make-tree-object
                  [`((:target ~(as-set (make-tree-object
                                        [`((:target
                                            ~(as-set (make-tree-object
                                                      [`(~(any) :order)])))
                                           (~(any) :order))
                                         `(~(any) :order)])))
                     (~(any) :order))
                   `(~(any) :order)]))))))

(deftest starting-store-test
  (let [s (starting-store "hi")
        hi-label-obj (find-object-by-name
                      s "hi" (object-type-object ""))
        tabs (matching-items '(nil "hi" :tab
                               (nil :tab-topic :table))
                             s)
        tab (first tabs)
        rows (matching-items
              (add-non-selector-to-fixed-term
               (pattern-to-fixed-term
                (make-tree-object [`(~(object-type-object "hi"))])))
              s)
        table (first (matching-elements '(nil :table) tab))
        row-conditions (matching-elements '(nil :row-condition) table)
        column-headers-list (matching-elements '(nil :column-headers) table)]
    (is (= (count tabs) 1))
    (is (= (count row-conditions) 1))
    (is (= (count column-headers-list) 1))
    (is (check (to-tree tab)
               (as-set
                `(""
                  :tab
                  ("hi" (~(any) :order))
                  (~(any) :order)
                  ~(as-set
                    `(""
                      ~(as-set
                        `(~(as-set (make-tree-object
                                    [`(~hi-label-obj (~(any) :order))
                                     `(~(any) :order)
                                     :selector]))
                          :row-condition
                          (~(any) :order)))
                      (~(any) :order)
                      ~(as-set
                        `(~'anything
                          (~(any) :order)
                          :selector
                          ~(as-set
                            `(~'anything
                              (~(any) :order)
                              (~(any) (~(any) :order))))
                          :column-headers))
                      :tab-topic
                      :table))))))
    (is (= rows []))
    (is (check (object-semantic-to-tree (content (first row-conditions)))
               (make-tree-object [`(~hi-label-obj)])))
    (is (check (map semantic-to-tree
                    (ordered-entities
                     (semantic-elements (first column-headers-list))))
               [`(~'anything (~(any)))]))))

(deftest add-table-test
  (let [s (starting-store "hi")
        s1 (add-table s "there" [["a" "b"] [1 2] [3]])
        there-label-obj (find-object-by-name
                         s1 "there" (object-type-object ""))
        a-label-obj (find-object-by-name
                     s1 "a" (link-type-object ""))
        b-label-obj (find-object-by-name
                     s1 "b" (link-type-object ""))
        tabs (matching-items '(nil "there" :tab
                               (nil :tab-topic :table))
                             s1)
        tab (first tabs)
        rows (matching-items
              (add-non-selector-to-fixed-term
               (pattern-to-fixed-term
                (make-tree-object [`(~(object-type-object "there"))])))
              s1)
        table (first (matching-elements '(nil :table) tab))
        row-condition (first (matching-elements '(nil :row-condition) table))
        column-headers (first (matching-elements '(nil :column-headers) table))]
    (is (= (count tabs) 1))
    (is (check (to-tree tab)
               (as-set
                `(""
                  (~(any) :order)
                  :tab
                  ("there" (~(any) :order))
                  ~(as-set
                    `(""
                      ~(as-set
                        `(~(as-set (make-tree-object
                                    [`(~there-label-obj (~(any) :order))
                                     `(~(any) :order)
                                     :selector]))
                          (~(any) :order)
                          :row-condition))
                      ~(as-set
                        `(~'anything
                          ~(as-set `(~'anything
                                     (~(any) :order)
                                     (~a-label-obj (~(any) :order))))
                          ~(as-set `(~'anything
                                     (~(any) :order)
                                     (~b-label-obj (~(any) :order))))
                          :selector :column-headers
                          (~(any) :order)))
                      :table
                      :tab-topic
                      (~(any) :order)))
                 ))))
    (is (check (map semantic-to-tree
                    (ordered-entities rows))
               [(as-set (make-tree-object
                         [`(~there-label-obj)
                          `(1 (~a-label-obj))
                          `(2 (~b-label-obj))]))
                (as-set (make-tree-object
                         [`(~there-label-obj)
                          `(3 (~a-label-obj))]))]))
    (is (check (object-semantic-to-tree (content row-condition))
               (make-tree-object [`(~there-label-obj)])))
    (is (check (map semantic-to-tree (ordered-entities
                                      (semantic-elements column-headers)))
               [`(~'anything (~a-label-obj))
                `(~'anything (~b-label-obj)) ]))))

(deftest avoid-problems-test
  (let [store (starting-store "test")
        columns (first (matching-items '(nil :column-headers) store))
        column (first (semantic-elements columns))
        label (first (semantic-elements column))
        bad-store (remove-entity-by-id store (:item-id label))
        good-store (update-source bad-store (:item-id column) "something")]
    (is (not (column-header-problem column)))
    (is (column-header-problem (in-different-store column bad-store)))
    (is (not (column-header-problem (in-different-store column good-store))))
    (is (= (abandon-problem-changes store bad-store (:item-id column))
           store))
    (is (= (abandon-problem-changes bad-store good-store (:item-id column))
           good-store))))

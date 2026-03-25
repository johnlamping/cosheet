(ns cosheet2.server.model-utils-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet2 [orderable :refer [split initial]]
                      [entity :refer [in-different-store stored-entity?
                                      link-type object-type name-label
                                      make-object-list make-element-list
                                      recursively-in-different-store
                                      id->object id->entity
                                      label->elements content->elements
                                      to-list elements]]
                      [orderable :as orderable]
                      [store :refer [new-element-store update-source
                                     make-item-id]]
                      [store-utils :refer [add-element add-object
                                           add-universal-objects
                                           remove-entity-by-id
                                           find-object-by-name]]
                      [query :refer [matching-items matching-elements
                                     not-query]]
                      entity-impl
                      [reporter :refer [reporter-value]]
                      [calculator :refer [request compute new-calculator-data]]
                      [task-queue :refer [new-priority-task-queue]]
                      [expression :refer [expr expr-let expr-seq]]
                      [canonical :refer [canonicalize]]
                      [debug :refer [simplify-for-print]]
                      [test-utils :refer [check any as-set]])
            (cosheet2.server
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
(def joe-list `("Joe"
                (~o2 :order)
                ("male" (~o1 :order))
                (39 (~o3 :order)
                    ("age" :label (~o3 :order))
                    ("doubtful" ("confidence" (~o4 :order))
                                (~o4 :order)) )
                ("married" (~o2 :order))
                (45 (~o4 :order)
                    ("age" :label (~o3 :order)))))
(def t1 (add-element (new-element-store) nil joe-list))
(def joe-id (second t1))
(def store (first t1))
(def joe (id->entity joe-id store))

(deftest transform-pattern-toward-fixed-term-test
  (let [pattern (make-element-list
                 :target
                 'anything
                 ['anything
                  `(~(make-object-list `((~link-type))))
                  `(~(make-object-list `("a")))])]
    (is (check (transform-pattern-toward-fixed-term
                pattern {})
               (make-element-list
                 :target
                 nil
                 [nil
                  `(~(make-object-list `((~link-type))))
                  `(~(make-object-list `("a")))])))
    (is (check (transform-pattern-toward-fixed-term
                pattern {:require-not-type true})
               (make-element-list
                 :target
                 nil
                 [nil
                  `(~(make-object-list `((~link-type))))
                  `(~(make-object-list `("a"
                                         ~(not-query `(~link-type))
                                         ~(not-query `(~object-type)))))])))
    (is (check (transform-pattern-toward-fixed-term
                pattern {:require-not-type true
                         :require-orders true})
               (make-element-list
                 :target
                 nil
                 ['(nil (nil :order))
                  `(~(make-object-list `((~link-type))))
                  `(~(make-object-list `("a"
                                         ~(not-query `(~link-type))
                                         ~(not-query `(~object-type)))))
                  '(nil :order)])))))

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
              (make-object-list '(2 anything)))
             (make-object-list '(2 ""))))
  (is (check (template-to-possible-non-selector-template
              (make-object-list '(2 anything  :selector)))
             (make-object-list '(2 anything :selector))))
  (is (check (template-to-possible-non-selector-template
              `(~label-object-template))
             `(~(make-object-list [`("" (~name-label)) `(~link-type)])))))

(deftest semantic-test
  (is (check (map canonicalize
                  (map to-list (semantic-elements joe)))
             (as-set (map canonicalize (rest (rest joe-list))))))
  (let [expected '("Joe"
                   "male"
                   "married"
                   (39 ("age" :label)
                       ("doubtful" "confidence"))
                   (45 ("age" :label)))]
    (is (= (canonicalize (semantic-to-list joe))
           (canonicalize expected)))
    (is (= (ordered-semantic-to-list joe)
           expected)))
  (let [removed (remove-semantic-elements store joe-id)
        removed-joe (id->entity joe-id removed)]
    (is (check (to-list removed-joe)
               `("Joe" (~(any) :order)))))
  (is (= (semantic-to-list '(1 (2 (:foo))))
         '(1 2)))
  (is (= (semantic-to-list (make-element-list :target 1 '(2 (:foo))))
         (make-element-list :target 1 '(2))))
  (is (= (semantic-to-list `(~(make-object-list [3 :name :bar]) (2 (:foo))))
         `(~(make-object-list [3 :name]) 2)))
  (let [named (id->object (make-item-id "A") (new-element-store))]
    (is (= (semantic-to-list `(~named (2 (:foo))))
           `(~named 2))))
  (is (= (semantic-to-list `(1 (~(make-object-list [3 :bar]) (:foo))))
         `(1 (~(make-object-list [3])))))
  (is (= (ordered-semantic-to-list
          `(~(make-object-list [3 :name :bar]) (2 (:foo))))
         `(~(make-object-list [3 :name]) 2)))
  (is (= (ordered-semantic-to-list
          `(1 (~(make-object-list [3 :bar]) (:foo))))
         `(1 (~(make-object-list [3])))))
  (let [s (add-universal-objects (new-element-store))
        age-label (make-object-list `(("age" (~name-label))
                                      (~link-type)))
        ;; We use add-object, rather than update-add-object-with-order,
        ;; which hasn't been tested at this point.
        [s1 age-label-id] (add-object s age-label)
        age-label (id->object age-label-id s1)
        named-joe-list (make-object-list
                               `(("Joe" (~name-label))
                                 (59 (~age-label))))
        [store joe-id] (add-object s1 named-joe-list)
        joe (id->object joe-id store)]
    ;; semantic-to-list shouldn't go inside named objects.
    (is (= (semantic-to-list joe) joe))
    (is (= (ordered-semantic-to-list joe) joe))
    ;; object-semantic-to-list should go inside a named object.
    (is (check
         (object-semantic-to-list joe)
         ;; We can't be sure of the order of elements.
         (as-set (recursively-in-different-store named-joe-list store))))))

(deftest labels-test
  (let [a `("a" (~o1 :order))
        b `("b " "x" (~o2 :order))
        c `("c" :label (~o3 :order))
        d `("d" :label (~o4 :order))
        test-list (list "test" a b c d)
        labels (semantic-label-elements test-list)
        non-labels (semantic-non-label-elements test-list)]
    (is (= (set non-labels) #{a b}))
    (is (= (set labels) #{c d}))))

(deftest is-selector-test
  (let [[s1 selector-root-id] (add-element
                               (starting-store "starting-tab") nil
                               '(thing :selector
                                       (child (1 :order)
                                              grandchild)))
        [s non-selector-root-id] (add-element
                                  s1 nil
                                  '(thing (child (1 :order)
                                                 grandchild)))
        selector-root (id->entity selector-root-id s)
        selector-child (first (matching-elements 'child selector-root))
        selector-grandchild (first (matching-elements 'grandchild
                                                      selector-child))
        non-selector-root (id->entity non-selector-root-id s)
        non-selector-child (first (matching-elements 'child non-selector-root))
        non-selector-grandchild (first (matching-elements 'grandchild
                                                          non-selector-child))
        ordered-tab-ids (ordered-tabs-ids-R s)
        cd (new-calculator-data (new-priority-task-queue 0))]
    (request ordered-tab-ids cd)
    (compute cd)
    (let [first-tab (id->entity
                     (first (reporter-value ordered-tab-ids)) s)]
      (is (selector? (first (label->elements
                             (first (label->elements first-tab :tab-topic))
                             :row-condition)))))
    (is (selector? selector-root))
    (is (selector? selector-child))
    (is (selector? selector-grandchild))
    (is (not (selector? non-selector-root)))
    (is (not (selector? non-selector-child)))
    (is (not (selector? non-selector-grandchild)))))

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
              (make-object-list [1 2 3]) (make-object-list [2 3 4]))
             [[1] []]))
  (is (check (elements-to-change-to-satisfy-fixed-term-elements
              (make-object-list ['(nil 1) 2 3]) (make-object-list [2 3 4]))
             [['("" 1)] []]))
  (is (check (elements-to-change-to-satisfy-fixed-term-elements
              (make-object-list [2 '(nil 1) '(3 4)])
              (make-object-list ['(2 1 3) 3]))
             [(as-set ['(3 4) 2]) [3]]))
  (is (check (elements-to-change-to-satisfy-fixed-term-elements
              (make-object-list [2 '(nil 1) 3])
              (make-object-list ['(2 5) 4]))
             [(as-set [3 '("" 1)]) []])))

(deftest get-or-make-ordered-object-by-name-test
  (let
      ;; First, make a new object.
      [[s1 id1 order1] (get-or-make-ordered-object-by-name
                        store
                        "Tina" (make-object-list [1 2])
                        unused-orderable :before)
       ;; Ask for it again.
       [s2 id2 order2] (get-or-make-ordered-object-by-name
                        s1
                        "Tina" (make-object-list [1 2])
                        order1 :before)
       ;; Ask for it again, with fewer required elements.
       [s3 id3 order3] (get-or-make-ordered-object-by-name
                        s2
                        "Tina" (make-object-list [1])
                        order2 :before)
       ;; Ask for it, with no additional required elements.
       [s4 id4 order4] (get-or-make-ordered-object-by-name
                        s3
                        "Tina" (make-object-list [])
                        order3 :before)
       ;; Ask for it, with different required elements.
       [s5 id5 order5] (get-or-make-ordered-object-by-name
                        s4
                        "Tina" (make-object-list [2 '(3 4)])
                        order4 :before)
       ;; Ask for it, with elements that have some commonality with
       ;; existing ones.
       [s6 id6 order6] (get-or-make-ordered-object-by-name
                        s5
                        "Tina" (make-object-list ['(1 2) 3])
                        order5 :before)
       ;; Ask for an object with a different name than existing ones.
       [s7 id7 order7] (get-or-make-ordered-object-by-name
                        s6
                        "Tony" (make-object-list [])
                        order6 :before)]
    
    ;; The new Tina object should match the template.
    (is (check (object-semantic-to-list (id->object id1 s1))
               (as-set (recursively-in-different-store
                        (make-object-list [`("Tina" (~name-label)) 1 2])
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
    (is (check (object-semantic-to-list (id->object id1 s5))
               (as-set (recursively-in-different-store
                        (make-object-list [`("Tina" (~name-label)) 1 2 '(3 4)])
                        s5))))
    ;; The Tina object should have gotten rid of a redundant element.
    (is (= id1 id6))
    (is (check (object-semantic-to-list (id->object id1 s6))
               (as-set (recursively-in-different-store
                        (make-object-list
                         [`("Tina" (~name-label)) '(1 2) 2 '(3 4)])
                        s6))))
    ;; The new Tony object should match its (empty) template.
    (is (not= id1 id7))
    (is (check (object-semantic-to-list (id->object id7 s7))
               (as-set (recursively-in-different-store
                        (make-object-list [`("Tony" (~name-label))])
                        s7))))))

(deftest update-add-element-with-order-and-temporary-test
  (let [[s id order] (update-add-element-with-order-and-temporary
                      store joe-id 6
                      unused-orderable :before true)
        joe (id->entity joe-id s)
        new-entity (first (matching-elements 6 joe))
        [o5 o6] (orderable/split unused-orderable :before)]
    (is (= (to-list new-entity)
           `(6 (~o5 :order))))
    (is (= order o6))
    (is (= (:item-id new-entity) id)))
  (let [[s id order] (update-add-element-with-order-and-temporary
                      store joe-id 6
                      unused-orderable :before false)
        joe (id->entity joe-id s)
        new-entity (first (matching-elements 6 joe))
        [o5 o6] (orderable/split unused-orderable :after)]
    (is (= (to-list new-entity)
           `(6 (~o5 :order))))
    (is (= order o6))
    (is (= (:item-id new-entity) id)))    
  (let [[s id order] (update-add-element-with-order-and-temporary
                      store joe-id 6
                      unused-orderable :after true)
        joe (id->entity joe-id s)
        new-entity (first (matching-elements 6 joe))
        [o5 o6] (orderable/split unused-orderable :after)]
    (is (= (to-list new-entity)
           `(6 (~o6 :order))))
    (is (= order o5))
    (is (= (:item-id new-entity) id)))
  (let [[s id order] (update-add-element-with-order-and-temporary
                      store joe-id '(6 ("height" :label))
                      unused-orderable :before true)
        joe (id->entity joe-id s)
        new-entity (first (label->elements joe "height"))
        [x o5] (orderable/split unused-orderable :before)
        [o6 o7] (orderable/split x :after)]
    (is (= (:item-id new-entity) id))
    (is (check (canonicalize (to-list new-entity))
               (canonicalize `(6 (~o7 :order)
                                 ("height" :label
                                  (~o6 :order))))))
    (is (= order o5)))
   ;; Try adding something that requires adding an non-identified object.
  (let [[s id order] (update-add-element-with-order-and-temporary
                      store joe-id `(6 (~(make-object-list
                                          [1 2])))
                      unused-orderable :before true)
        joe (id->entity joe-id s)
        new-entity (first (content->elements joe 6))]
    (is (= (:item-id new-entity) id))
    (is (check (ordered-semantic-to-list new-entity)
               `(6 (~(make-object-list [1 2]))))))
  ;; Try adding something that requires adding an identified object.
  (let [[s id order] (update-add-element-with-order-and-temporary
                      store joe-id `(6 (~(make-object-list
                                          [`("Tina" (~name-label)) 1 2])))
                      unused-orderable :before true)
        joe (id->entity joe-id s)
        new-entity (first (content->elements joe 6))
        tina (find-object-by-name s "Tina" (make-object-list nil))]
    (is (= (:item-id new-entity) id))
    (is (check (ordered-semantic-to-list new-entity)
               `(6 (~tina))))
    (is (check (object-semantic-to-list tina)
               (as-set (recursively-in-different-store
                        (make-object-list [`("Tina" (~name-label)) 1 2])
                        s))))
    ;; Now try adding another element that references the same object.
    (let [[s1 id1 order1] (update-add-element-with-order-and-temporary
                           s joe-id `(7 (~(make-object-list
                                           [`("Tina" (~name-label)) 2 3])))
                           order :before true)
          joe (id->entity joe-id s1)
          new-entity (first (content->elements joe 7))
          tina (find-object-by-name s1 "Tina" (make-object-list nil))]
      (is (= (:item-id new-entity) id1))
      (is (check (ordered-semantic-to-list new-entity)
                 `(7 (~tina))))
      (is (check (object-semantic-to-list tina)
                 (as-set
                  (recursively-in-different-store
                   ;; Tina should have gotten an extra property.
                   (make-object-list [`("Tina" (~name-label)) 1 2 3])
                   s1))))))
  
  ;; Check that order in the list style entity is preserved in the
  ;; :order values.
  ;; Also check and that non-semantic elements don't get order information
  ;; and that the entity is marked temporary, if requested.
  (let [[s id order] (update-add-element-with-order-and-temporary
                      store joe-id '(6 ("height" :label)
                                       ("" :label)
                                       :temporary
                                       (:other ""))
                      unused-orderable :after false)
        joe (id->entity joe-id s)
        new-entity (first (label->elements joe "height"))
        [x o5] (orderable/split unused-orderable :before)
        [x o6] (orderable/split x :before)
        [o8 o7] (orderable/split x :before)]
    (is (check (canonicalize (to-list new-entity))
               (canonicalize
                `(6 (~o5 :order)
                    ("height" :label (~o7 :order))
                    ("" :label (~o6 :order))
                    :temporary
                    (:other "")))))
    (is ((:temporary-ids s) id))
    (is (= order o8))
    (is (= (:item-id new-entity) id))))

(deftest starting-store-test
  (let [s (starting-store "hi")
        tabs (matching-items '(nil "hi" :tab
                               (nil :tab-topic :table))
                             s)
        tab (first tabs)
        rows (matching-items
              '(nil :top-level) s)
        table (first (matching-elements '(nil :table) tab))
        row-conditions (matching-elements '(nil :row-condition) table)
        column-headers-list (matching-elements '(nil :column-headers) table)]
    (is (= (count tabs) 1))
    (is (= (count row-conditions) 1))
    (is (= (count column-headers-list) 1))
    (is (check (to-list tab)
               (as-set
                `(""
                  :tab
                  ("hi" (~(any) :order))
                  (~(any) :order)
                  ~(as-set
                    `(""
                      ~(as-set
                        `(~'anything
                          :selector
                          ~(as-set `("hi" (~(any) :order) :label))
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
                              (" A" (~(any) :order) :label)))
                          :column-headers
                          ))
                      :tab-topic
                      :table))))))
    (is (= rows []))
    (is (check (semantic-to-list (first row-conditions))
               '(anything ("hi" :label))))
    (is (check (map semantic-to-list
                    (ordered-entities
                     (semantic-elements (first column-headers-list))))
               ['(anything (" A" :label))]))))

(deftest add-table-test
  (let [s (starting-store "hi")
        s1 (add-table s "there" [["a" "b"] [1 2] [3]])
        tabs (matching-items '(nil "there" :tab
                               (nil :tab-topic :table))
                             s1)
        tab (first tabs)
        rows (matching-items
              '(nil :top-level) s1)
        table (first (matching-elements '(nil :table) tab))
        row-condition (first (matching-elements '(nil :row-condition) table))
        column-headers (first (matching-elements '(nil :column-headers) table))]
    (is (= (count tabs) 1))
    (is (check (to-list tab)
               (as-set
                `(""
                  (~(any) :order)
                  :tab
                  ("there" (~(any) :order))
                  ~(as-set
                    `(""
                       ~(as-set
                         `(~'anything
                           (~(any) :order)
                           :selector
                           ~(as-set `("there" :label (~(any) :order)))
                           :row-condition))
                      ~(as-set
                        `(~'anything
                          ~(as-set `(~'anything (~(any) :order)
                                     ~(as-set `("a" :label (~(any) :order)))))
                          ~(as-set `(~'anything (~(any) :order)
                                     ~(as-set `("b" :label (~(any) :order)))))
                          :selector :column-headers
                          (~(any) :order)))
                      :table
                      :tab-topic
                      (~(any) :order)))
                 ))))
    (is (check (map semantic-to-list
                    (ordered-entities rows))
               [(as-set '(""
                          ("there" :label)
                          (1 ("a" :label))
                          (2 ("b" :label))))
                (as-set '("" ("there" :label) (3 ("a" :label))))]))
    (is (check (semantic-to-list row-condition)
               (as-set
                '(anything ("there" :label)))))
    (is (check (map semantic-to-list (ordered-entities
                                      (semantic-elements column-headers)))
               [(as-set '(anything ("a" :label)))
                (as-set '(anything ("b" :label)))]))))

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

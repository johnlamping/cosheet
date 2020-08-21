(ns cosheet2.server.model-utils-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet2 [orderable :refer [split initial]]
                      [entity :refer [in-different-store]]
                      [store :refer [new-element-store update-content]]
                      [store-utils :refer [add-entity remove-entity-by-id]]
                      [query :refer [matching-items matching-elements
                                     not-query]]
                      [entity :refer [description->entity label->elements
                                      to-list]]
                      entity-impl
                      [reporter :refer [reporter-value]]
                      [calculator :refer [request compute new-calculator-data]]
                      [task-queue :refer [new-priority-task-queue]]
                      [expression :refer [expr expr-let expr-seq]]
                      [canonical :refer [canonicalize-list]]
                      [debug :refer [simplify-for-print]]
                      [test-utils :refer [check any as-set]])
            (cosheet2.server
             [model-utils :refer :all]
             [order-utils :refer [order-entities]])
            ; :reload
            ))

(def orderables (reduce (fn [os _]
                          (vec (concat (pop os)
                                       (split (peek os) :after))))
                        [initial]
                        (range 4)))
(def o1 (nth orderables 0))
(def o2 (nth orderables 1))
(def o3 (nth orderables 2))
(def o4 (nth orderables 3))
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
(def t1 (add-entity (new-element-store) nil joe-list))
(def joe-id (second t1))
(def store (first t1))
(def joe (description->entity joe-id store))

(deftest transform-pattern-toward-query-test
  (let [pattern '(anything anything ("a" :label))]
    (is (= (transform-pattern-toward-query pattern)
           '(nil nil ("a" :label))))
    (is (= (transform-pattern-toward-query pattern :require-not-labels true)
           `(nil (nil ~(not-query :label)) ("a" :label) ~(not-query :label))))
    (is (= (transform-pattern-toward-query
            pattern :require-not-labels true :require-orders true)
           `(nil (nil ~(not-query :label) (nil :order))
                 ("a" :label)
                 ~(not-query :label)
                 (nil :order))))))

(deftest specialize-generic-test
  (let [[c1 s1] (specialize-generic '("x" (??? :a) (??? 22))
                                     (new-element-store))
        [c2 s2] (specialize-generic '("x" (??? "y") (??? "22"))
                                     s1)]
    (is (= c1  '("x" ("\u00A0A" :a) ("\u00A0B" 22))))
    (is (= c2  '("x" ("\u00A0C" "y") ("\u00A0D" "22"))))))

(deftest semantic-test
  (let [semantic (semantic-to-list joe)]
    (is (= (first semantic) "Joe")))
  (is (check (map canonicalize-list
                  (map to-list (semantic-elements joe)))
         (as-set (map canonicalize-list (rest (rest joe-list))))))
  (let [expected ["joe" {"male" 1
                         "married" 1
                         [39 {["age" {:label 1}] 1
                              ["doubtful" {"confidence" 1}] 1}] 1
                         [45 {["age" {:label 1}] 1}] 1}]]
    (is (= (entity->canonical-semantic joe-list) expected))))

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
  (let [[s1 selector-root-id] (add-entity
                               (starting-store "tab") nil
                               '(thing :selector
                                       (child (1 :order)
                                              grandchild)))
        [s non-selector-root-id] (add-entity
                                  s1 nil
                                  '(thing (child (1 :order)
                                                 grandchild)))
        selector-root (description->entity selector-root-id s)
        selector-child (first (matching-elements 'child selector-root))
        selector-grandchild (first (matching-elements 'grandchild
                                                      selector-child))
        non-selector-root (description->entity non-selector-root-id s)
        non-selector-child (first (matching-elements 'child non-selector-root))
        non-selector-grandchild (first (matching-elements 'grandchild
                                                          non-selector-child))
        ordered-tab-ids (ordered-tabs-ids-R s)
        cd (new-calculator-data (new-priority-task-queue 0))]
    (request ordered-tab-ids cd)
    (compute cd)
    (let [first-tab (description->entity
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

(deftest add-table-test
  (let [s (starting-store "hi")
        s1 (add-table s "there" [["a" "b"] [1 2] [3]])
        tabs (matching-items '(nil "there" :tab
                               (nil :tab-topic :table))
                             s1)
        tab (first tabs)
        rows (matching-items
              '(nil :top-level) s1)
        table (first (matching-elements
                              '(nil :table)
                              tab))
        row-condition (first (matching-elements
                              '(nil :row-condition)
                              table))
        headers (matching-elements
                 '(nil :column)
                 row-condition)]
    (is (= (count tabs) 1))
    (is (check (to-list tab)
               (as-set
                `(""
                  :tab
                  ~(as-set
                    `(:blank :tab-topic
                             ~(as-set
                               `(~'anything
                                 ("there" :label)
                                 ~(as-set `(~'anything ("a" :label) :column))
                                 ~(as-set `(~'anything ("b" :label) :column))
                                 :row-condition
                                 :selector))
                             :table))
                  ("there" (~(any) :order))
                  (~(any) :order)))))
    (is (check (map semantic-to-list
                    (order-entities rows))
               [(as-set '(""
                          ("there" :label)
                          (1 ("a" :label))
                          (2 ("b" :label))))
                (as-set '("" ("there" :label) (3 ("a" :label))))]))
    (is (check (semantic-to-list row-condition)
               (as-set
                '(anything ("there" :label)
                           (anything ("a" :label))
                           (anything ("b" :label))))))
    (is (check (map semantic-to-list (order-entities headers))
               (as-set ['(anything ("a" :label))
                        '(anything ("b" :label))])))))

(deftest avoid-problems-test
  (let [store (starting-store "test")
        column (first (matching-items '(nil :column) store))
        label (first (semantic-elements column))
        bad-store (remove-entity-by-id store (:item-id label))
        good-store (update-content bad-store (:item-id column) "something")]
    (is (not (column-header-problem column)))
    (is (column-header-problem (in-different-store column bad-store)))
    (is (not (column-header-problem (in-different-store column good-store))))
    (is (= (abandon-problem-changes store bad-store column)
           store))
    (is (= (abandon-problem-changes bad-store good-store column)
           good-store))))

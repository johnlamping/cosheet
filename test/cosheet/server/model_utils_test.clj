(ns cosheet.server.model-utils-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet [orderable :as orderable]
                     [entity :refer [in-different-store]]
                     [store :refer [new-element-store update-content]]
                     [store-utils :refer [add-entity remove-entity-by-id]]
                     [query :refer [matching-items matching-elements]]
                     [entity :refer [description->entity label->elements
                                     to-list]]
                     [expression :refer [expr expr-let expr-seq]]
                     [canonical :refer [canonicalize-list]]
                     [debug :refer [simplify-for-print]]
                     [test-utils :refer [check any as-set let-mutated]])
            (cosheet.server
             [model-utils :refer :all]
             [order-utils :refer [order-items-R]])
            ; :reload
            ))

(def orderables (reduce (fn [os _]
                          (vec (concat (pop os)
                                       (orderable/split (peek os) :after))))
                        [orderable/initial]
                        (range 4)))
(def o1 (nth orderables 0))
(def o2 (nth orderables 1))
(def o3 (nth orderables 2))
(def o4 (nth orderables 3))
(def joe-list `("Joe"
                (~o2 :order)
                ("male" (~o1 :order))
                (39 (~o3 :order)
                    ("age" :tag (~o3 :order))
                    ("doubtful" ("confidence" (~o4 :order))
                                (~o4 :order)) )
                ("married" (~o2 :order))
                (45 (~o4 :order)
                    ("age" :tag (~o3 :order)))
                ("spy" :invisible)))

(deftest specialize-template-test
  (let [[c1 s1] (specialize-template '("x" (??? :a) (??? 22))
                                     (new-element-store))
        [c2 s2] (specialize-template '("x" (??? "y") (??? "22"))
                                     s1)]
    (is (= c1  '("x" ("\u00A0A" :a) ("\u00A0B" 22))))
    (is (= c2  '("x" ("\u00A0C" "y") ("\u00A0D" "22"))))))

(deftest semantic-test
  (let [semantic (let-mutated [him joe-list]
                  (semantic-to-list-R him))]
    (is (= (first semantic) "Joe")))
  (is (check (map canonicalize-list
                  (let-mutated [him joe-list]
                    (expr-seq map to-list (semantic-elements-R him))))
         (as-set (map canonicalize-list (rest (rest joe-list))))))
  (let [expected ["joe" {"male" 1
                         "married" 1
                         [39 {["age" {:tag 1}] 1
                              ["doubtful" {"confidence" 1}] 1}] 1
                         [45 {["age" {:tag 1}] 1}] 1
                         "spy" 1}]]
    (is (= (item->canonical-semantic joe-list) expected))))

(deftest labels-R-test
  (let [[[a] [b] [c] [d] labels non-labels [split-labels split-non-labels]]
        (let-mutated [it '("test"
                            ("a" (~o1 :order))
                            ("b " "x" (~o2 :order))
                            ("c" :tag (~o3 :order))
                            ("d" :tag (~o4 :order)))]
          (expr-let [labels (visible-labels-R it)
                     non-labels (visible-non-labels-R it)
                     split (expr split-out-labels-R (visible-elements-R it))
                     a (matching-elements "a" it)
                     b (matching-elements "b" it)
                     c (matching-elements "c" it)
                     d (matching-elements "d" it)]
            [a b c d labels non-labels split]))]
    (println (simplify-for-print [a b c d labels split-labels non-labels split-non-labels]))
    (is (= (set non-labels) #{a b}))
    (is (= (set split-non-labels) #{a b}))
    (is (= (set labels) #{c d}))
    (is (= (set split-labels) #{c d}))))

(deftest visible-test
  (let [expected #{"male"
                   "married"
                   [39 {["age" {:tag 1}] 1
                        ["doubtful" {"confidence" 1}] 1}]
                   [45 {["age" {:tag 1}] 1}]}
        visible (set (map item->canonical-semantic
                          (visible-elements-R joe-list)))]
    (is (check visible expected))
    (let [expected ["joe" {"male" 1
                           "married" 1
                           [39 {["age" {:tag 1}] 1
                              ["doubtful" {"confidence" 1}] 1}] 1
                         [45 {["age" {:tag 1}] 1}] 1}]]
    (is (= (item->canonical-visible joe-list) expected)))))

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
                                                          non-selector-child))]
    
    (is (selector? (first (label->elements
                           (first (label->elements
                                   (first-tab-R s) :tab-topic))
                           :row-condition))))
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
    (is (check (cosheet.entity/to-list tab)
           `(""
             (:blank :tab-topic :table
                     (~'anything ("there" :tag)
                      (~'anything :column ("a" :tag))
                      (~'anything :column ("b" :tag))
                      :row-condition :selector))
             :tab
             ("there" (~(any) :order))
             (~(any) :order))))
    (is (check (map immutable-semantic-to-list (order-items-R rows))
               [(as-set '("" ("there" :tag) (1 ("a" :tag)) (2 ("b" :tag))))
                (as-set '("" ("there" :tag) (3 ("a" :tag))))]))
    (is (check (immutable-semantic-to-list row-condition)
               '(anything ("there" :tag)
                          (anything ("a" :tag))
                          (anything ("b" :tag)))))
    (is (check (map immutable-semantic-to-list (order-items-R headers))
               ['(anything ("a" :tag))
                '(anything ("b" :tag))]))))

(deftest avoid-problems-test
  (let [store (starting-store "test")
        column (first (matching-items '(nil :column) store))
        label (first (semantic-elements-R column))
        bad-store (remove-entity-by-id store (:item-id label))
        good-store (update-content bad-store (:item-id column) "something")]
    (is (not (column-header-problem column)))
    (is (column-header-problem (in-different-store column bad-store)))
    (is (not (column-header-problem (in-different-store column good-store))))
    (is (= (abandon-problem-changes store bad-store column)
           store))
    (is (= (abandon-problem-changes bad-store good-store column)
           good-store))))

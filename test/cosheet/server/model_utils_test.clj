(ns cosheet.server.model-utils-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet [orderable :as orderable]
                     [store :refer [new-element-store]]
                     [store-utils :refer [add-entity]]
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
                (~o2 :order :non-semantic)
                ("male" (~o1 :order :non-semantic))
                (39 (~o3 :order :non-semantic)
                    ("age" ~'tag (~o3 :order :non-semantic))
                    ("doubtful" ("confidence" (~o4 :order :non-semantic))
                                (~o4 :order :non-semantic)) )
                ("married" (~o2 :order :non-semantic))
                (45 (~o4 :order :non-semantic)
                    ("age" ~'tag (~o3 :order :non-semantic)))))

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
  (is (= (set (map canonicalize-list
                   (let-mutated [him joe-list]
                     (expr-seq map to-list (semantic-elements-R him)))))
         (set (map canonicalize-list (rest (rest joe-list))))))
  (let [expected ["joe" {"male" 1
                         "married" 1
                         [39 {["age" {'tag 1}] 1
                              ["doubtful" {"confidence" 1}] 1}] 1
                              [45 {["age" {'tag 1}] 1}] 1}]]
    (is (= (item->canonical-semantic joe-list) expected)))
  (let [joes `("x"
              ("Joe" ("name" ~'tag) ("id" ~'tag) (~o1 :order :non-semantic))
              ("Joe" ("name" ~'tag) (~o2 :order :non-semantic))) ]
    (is (= (best-matching-element '("Joe" ("name" tag)) joes)
           [(nth joes 2)]))
    (is (= (best-matching-element '("Joe" ("name" tag)  ("id" tag)) joes)
           [(nth joes 1)]))
    (is (= (best-matching-element '("Joe" ("age" tag)) joes)
           nil))))

(deftest is-selector-test
  (let [[s1 selector-root-id] (add-entity
                               (starting-store "tab") nil
                               '(thing (:selector :non-semantic)
                                       (child (1 (:order :non-semantic))
                                              grandchild)))
        [s non-selector-root-id] (add-entity
                                  s1 nil
                                  '(thing (child (1 (:order :non-semantic))
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
        tabs (matching-items '(nil "there" (:tab :non-semantic)
                               (nil
                                (:non-semantic :non-semantic)
                                (:tab-topic :non-semantic)
                                (:table :non-semantic)))
                             s1)
        tab (first tabs)
        rows (matching-items
              '(nil (:top-level :non-semantic)) s1)
        table (first (matching-elements
                              '(nil (:table :non-semantic))
                              tab))
        row-condition (first (matching-elements
                              '(nil (:row-condition :non-semantic)
                                    (:non-semantic :non-semantic))
                              table))
        headers (matching-elements
                 '(nil (:column :non-semantic)
                       (:non-semantic :non-semantic))
                 row-condition)]
    (is (= (count tabs) 1))
    (is (= (immutable-semantic-to-list tab) '("" "there")))
    (is (check (map immutable-semantic-to-list (order-items-R rows))
               [(as-set '("" ("there" :tag) (1 ("a" :tag)) (2 ("b" :tag))))
                (as-set '("" ("there" :tag) (3 ("a" :tag))))]))
    (is (check (immutable-semantic-to-list row-condition)
               '(anything ("there" :tag))))
    (is (check (map immutable-semantic-to-list (order-items-R headers))
               ['(anything ("a" :tag))
                '(anything ("b" :tag))]))))

(ns cosheet.server.model-utils-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet [store :refer [new-element-store]]
                     [query :refer [matching-items matching-elements]]
                     [entity :refer [to-list]]
                     [debug :refer [simplify-for-print]]
                     [test-utils :refer [check any as-set let-mutated]])
            (cosheet.server
             [model-utils :refer :all]
             [order-utils :refer [order-items-R]]
             [referent :refer [item->canonical-semantic
                               immutable-semantic-to-list]])
            ; :reload
            ))

(deftest specialize-template-test
  (let [[c1 s1] (specialize-template '("x" (??? :a) (??? 22))
                                     (new-element-store))
        [c2 s2] (specialize-template '("x" (??? "y") (??? "22"))
                                     s1)]
    (is (= c1  '("x" ("\u00A0A" :a) ("\u00A0B" 22))))
    (is (= c2  '("x" ("\u00A0C" "y") ("\u00A0D" "22"))))))

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
                 table)]
    (is (= (count tabs) 1))
    (is (= (immutable-semantic-to-list tab) '("" "there")))
    (is (check (map immutable-semantic-to-list (order-items-R rows))
               [(as-set '("" ("there" :tag) (1 ("a" :tag)) (2 ("b" :tag))))
                (as-set '("" ("there" :tag) (3 ("a" :tag))))]))
    (is (check (immutable-semantic-to-list row-condition)
               '(anything ("there" :tag))))
    (is (check (map immutable-semantic-to-list (order-items-R headers))
               ['(anything-immutable ("a" :tag))
                '(anything-immutable ("b" :tag))]))))

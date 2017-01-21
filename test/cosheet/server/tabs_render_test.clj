(ns cosheet.server.tabs-render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [orderable :as orderable]
             [entity :as entity  :refer [label->elements]]
             [expression :refer [expr expr-let expr-seq]]
             [expression-manager :refer [current-value]]
             [debug :refer [envs-to-list simplify-for-print]]
             entity-impl
             [test-utils :refer [check any as-set evals-to let-mutated]])
            (cosheet.server
             [referent :refer [item-referent union-referent exemplar-referent
                               query-referent elements-referent
                               virtual-referent]]
             [item-render :refer [item-without-labels-DOM-R
                                  item-DOM-R]]
             [tabs-render :refer :all])
             ; :reload
            ))

(def orderables (reduce (fn [os _]
                          (vec (concat (pop os)
                                       (orderable/split (peek os) :after))))
                        [orderable/initial]
                        (range 3)))
(def o1 (nth orderables 0))
(def o2 (nth orderables 1))
(def o3 (nth orderables 2))
(def unused-orderable (nth orderables 3))

(deftest tabs-DOM-test
  (let [inherited {:priority 1
                   :width 3.0
                   :key-prefix [:foo]}]
    (let [tabs-list `(""
                      ("" "foo"
                       (:tab :non-semantic)
                       ("" (:table :non-semantic) (:non-semantic :non-semantic))
                       (~o1 :order :non-semantic))
                      ("" "foo" "bar"
                       (:tab :non-semantic)
                       ("" (:table :non-semantic) (:non-semantic :non-semantic))
                       (~o2 :order :non-semantic))
                      ("" "baz"
                       (:tab :non-semantic)
                       ("" (:table :non-semantic) (:non-semantic :non-semantic))
                       (~o3 :order :non-semantic)))
          [dom tabs t2] (let-mutated [tabs tabs-list]
                          (expr-let [t2 (expr first (label->elements tabs o2))
                                     dom (tabs-DOM-R tabs t2 inherited)]
                            [dom tabs t2]))
          t1 (first (current-value (label->elements tabs o1)))
          t3 (first (current-value (label->elements tabs o3)))]
      (println dom))))

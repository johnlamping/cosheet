(ns cosheet.server.batch-edit-render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [orderable :as orderable]
             [query :refer [matching-elements]]
             [expression :refer [expr expr-let expr-seq]]
             [expression-manager :refer [current-value]]
             [debug :refer [envs-to-list simplify-for-print]]
             entity-impl
             [store :refer [make-id]]
             store-impl
             mutable-store-impl
             [test-utils :refer [check any as-set
                                 let-mutated item->immutable]])
            (cosheet.server
             [referent :refer [item-referent query-referent elements-referent]]
             [batch-edit-render :refer :all])
            ; :reload
            ))

(def orderables (reduce (fn [os _]
                          (vec (concat (pop os)
                                       (orderable/split (peek os) :after))))
                        [orderable/initial]
                        (range 2)))
(def o1 (nth orderables 0))
(def o2 (nth orderables 1))
(def unused-orderable (nth orderables 2))

(def base-inherited {:priority 0
                     :key-prefix [:root]
                     :subject-referent nil
                     :width 2.0})

(deftest item-DOM-R-test-simple
  ;; Test a simple cell
  (let [age-as-list `(39 ("high"
                          ("confidence" :tag (~o1 :order :non-semantic))
                          (~o2 :order :non-semantic)))
        age (let-mutated [age age-as-list] age)
        store (:store age)]
    (let [high (first (current-value (matching-elements "high" age)))
          confidence (first (current-value
                             (matching-elements "confidence" high)))]
      ;; TODO: Add some tests of the results here.
      (current-value
       (expr-let [dom (batch-edit-DOM-R age nil store base-inherited)]))
      (current-value
       (expr-let [dom (batch-edit-DOM-R age high store base-inherited)])))))

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

(def orderables (reduce (fn [os _]
                          (vec (concat (pop os)
                                       (orderable/split (peek os) :after))))
                        [orderable/initial]
                        (range 6)))
(def o1 (nth orderables 0))
(def o2 (nth orderables 1))
(def o3 (nth orderables 2))
(def o4 (nth orderables 3))
(def o5 (nth orderables 4))
(def o6 (nth orderables 5))

(deftest batch-edit-DOM-R-test
  ;; Test a simple cell
  (let [joe-list `("Joe"
                   :top-level
                   (~o2 :order)
                   ("male" (~o1 :order))
                   ("married" (~o2 :order))
                   (39 (~o3 :order)
                       ("age" :tag (~o3 :order))
                       ("doubtful" (~o1 :order) ("confidence" (~o3 :order))))
                   (45 (~o4 :order)
                       ("age" :tag (~o3 :order)))
                   ("Joe" (~o5 :order)
                          ("name" :tag (~o3 :order)))
                   ("Joseph" (~o6 :order)
                             ("name" :tag (~o1 :order))
                             ("id" :tag (~o2 :order))))
        row-selector-list '(anything
                            (anything ("age" :tag))
                            :batch-row-selector :batch-selector :selector)
        batch-elements-list '(anything
                              (anything ("age" :tag))
                              (anything ("age" :tag) ("doubtful" :tag))
                              :batch-elements :batch-selector :selector)
        [joe row-selector batch-elements]
        (let-mutated [joe joe-list
                      row-selector row-selector-list
                      batch-elements batch-elements-list]
          [joe row-selector batch-elements])
        store (:store joe)]
    ;; TODO: Add some tests of the results here.
    (println
     (simplify-for-print
      (current-value (batch-edit-DOM-R [row-selector batch-elements]
                                       store base-inherited))))))

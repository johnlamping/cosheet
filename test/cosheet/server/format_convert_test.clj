(ns cosheet.server.format-convert-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [orderable :as orderable]
             [entity  :refer [to-list]]
             [canonical :refer [canonicalize-list]]
             [store :refer [new-element-store]]
             store-impl
             [store-utils :refer [add-entity]]
             [query :refer [matching-items]]
             [debug :refer [simplify-for-print]]
             entity-impl
             [test-utils :refer [check any as-set evals-to let-mutated]])
            (cosheet.server
             [format-convert :refer :all])
             ; :reload
            ))

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


(deftest convert-test
  (let [table-list-0 `("table"
                       :table
                       (~'anything (~'anything ("age" :tag))
                        (:row-condition :non-semantic))
                       (~'anything ("single" :tag (~o1 :order :non-semantic))
                        (~o1 :order :non-semantic)
                        (:column :non-semantic)
                        (:non-semantic :non-semantic))
                       (~'anything ("name" :tag (~o1 :order :non-semantic))
                        (~o2 :order :non-semantic)
                        (:column :non-semantic)
                        (:non-semantic :non-semantic)))
        table-list-1 `("table"
                       :table
                       (~'anything (~'anything ("age" :tag))
                        (:row-condition :non-semantic)
                        (~'anything ("single" :tag (~o1 :order :non-semantic))
                         (~o1 :order :non-semantic)
                         (:column :non-semantic)
                         (:non-semantic :non-semantic))
                        (~'anything ("name" :tag (~o1 :order :non-semantic))
                         (~o2 :order :non-semantic)
                         (:column :non-semantic)
                         (:non-semantic :non-semantic))))]
    (let [store-0 (first (add-entity (new-element-store) nil table-list-0))
          store-1 (convert-from-0-to-1 store-0)
          tables (matching-items '(nil :table) store-1)
          versions (matching-items '(nil :format) store-1)]
      (is (= (count tables) 1))
      (is (check (canonicalize-list (to-list (first tables)))
                 (canonicalize-list table-list-1)))
      (is (= (count versions) 1))
      (is (= (to-list (first versions)) '(1 :format)))
      (is (= (convert-to-current store-0) store-1))
      (is (= (convert-to-current store-1) store-1)))))

(ns cosheet2.server.format-convert-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet2
             [orderable :as orderable]
             [entity  :refer [to-list]]
             [canonical :refer [canonicalize-list]]
             [store :refer [new-element-store]]
             store-impl
             [store-utils :refer [add-entity]]
             [query :refer [matching-items]]
             query-impl
             [debug :refer [simplify-for-print]]
             entity-impl
             [test-utils :refer [check any as-set]])
            (cosheet2.server
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
                       (~'anything-immutable
                        ("single" :tag (~o1 :order :non-semantic))
                        (~o1 :order :non-semantic)
                        (:column :non-semantic)
                        (:non-semantic :non-semantic))
                       (~'anything-immutable
                        ("name" :tag (~o1 :order :non-semantic))
                        (~o2 :order :non-semantic)
                        (:column :non-semantic)
                        (:non-semantic :non-semantic))
                       (42
                        ("answer" :tag (~o1 :order :non-semantic))
                        (~o1 :order :non-semantic)
                        (:column :non-semantic)
                        (:non-semantic :non-semantic)))
        table-list-1 `("table"
                       :table
                       (~'anything (~'anything ("age" :tag))
                        (:row-condition :non-semantic)
                        (~'anything-immutable
                         ("single" :tag (~o1 :order :non-semantic))
                         (~o1 :order :non-semantic)
                         (:column :non-semantic)
                         (:non-semantic :non-semantic))
                        (~'anything-immutable
                         ("name" :tag (~o1 :order :non-semantic))
                         (~o2 :order :non-semantic)
                         (:column :non-semantic)
                         (:non-semantic :non-semantic))
                        (42
                         ("answer" :tag (~o1 :order :non-semantic))
                         (~o1 :order :non-semantic)
                         (:column :non-semantic)
                         (:non-semantic :non-semantic))))
        table-list-3 `("table"
                       :table
                       (~'anything (~'anything ("age" :tag))
                        (:row-condition :non-semantic)
                        (:selector :non-semantic)
                        (~'anything-immutable
                         ("single" :tag (~o1 :order :non-semantic))
                         (~o1 :order :non-semantic)
                         (:column :non-semantic)
                         (:non-semantic :non-semantic))
                        (~'anything-immutable
                         ("name" :tag (~o1 :order :non-semantic))
                         (~o2 :order :non-semantic)
                         (:column :non-semantic)
                         (:non-semantic :non-semantic))
                        (42
                         ("answer" :tag (~o1 :order :non-semantic))
                         (~o1 :order :non-semantic)
                         (:column :non-semantic)
                         (:non-semantic :non-semantic))))
        table-list-4 `("table"
                       :table
                       (~'anything (~'anything ("age" :tag))
                        (:row-condition :non-semantic)
                        (:selector :non-semantic)
                        (~'anything ("single" :tag (~o1 :order :non-semantic))
                         (~o1 :order :non-semantic)
                         (:column :non-semantic)
                         (:non-semantic :non-semantic))
                        (~'anything ("name" :tag (~o1 :order :non-semantic))
                         (~o2 :order :non-semantic)
                         (:column :non-semantic)
                         (:non-semantic :non-semantic))
                        (42 ("answer" :tag (~o1 :order :non-semantic))
                         (~o1 :order :non-semantic)
                         (:column :non-semantic)
                         (:non-semantic :non-semantic))))
        table-list-5 `("table"
                       :table
                       (~'anything (~'anything ("age" :tag))
                        :row-condition
                        :selector
                        (~'anything ("single" :tag (~o1 :order))
                         (~o1 :order)
                         :column)
                        (~'anything ("name" :tag (~o1 :order))
                         (~o2 :order)
                         :column)
                        (42 ("answer" :tag (~o1 :order))
                         (~o1 :order)
                         :column)))
        table-list-6 `("table"
                       :table
                       (~'anything (~'anything ("age" :label))
                        :row-condition
                        :selector
                        (~'anything ("single" :label (~o1 :order))
                         (~o1 :order)
                         :column)
                        (~'anything ("name" :label (~o1 :order))
                         (~o2 :order)
                         :column)
                        (42 ("answer" :label (~o1 :order))
                         (~o1 :order)
                         :column)))
        table-list-7 `("table"
                       :table
                       (~'anything
                        (~'anything ("age" :label))
                        :row-condition
                        :selector
                        :non-semantic)
                       (~'anything
                        :column-headers
                        :selector
                        :non-semantic
                        (~'anything ("single" :label (~o1 :order))
                         (~o1 :order))
                        (~'anything ("name" :label (~o1 :order))
                         (~o2 :order))
                        (42 ("answer" :label (~o1 :order))
                         (~o1 :order))))]
    (let [store-0 (first (add-entity (new-element-store) nil table-list-0))
          store-1 (convert-from-0-to-1 store-0)
          tables-1 (matching-items '(nil :table) store-1)
          versions-1 (matching-items '(nil :format) store-1)
          store-3 (convert-from-1-or-2-to-3 store-1)
          tables-3 (matching-items '(nil :table) store-3)
          versions-3 (matching-items '(nil :format) store-3)
          store-4 (convert-from-3-to-4 store-3)
          tables-4 (matching-items '(nil :table) store-4)
          versions-4 (matching-items '(nil :format) store-4)
          store-5 (convert-from-4-to-5 store-4)
          tables-5 (matching-items '(nil :table) store-5)
          versions-5 (matching-items '(nil :format) store-5)
          store-6 (convert-from-5-to-6 store-5)
          tables-6 (matching-items '(nil :table) store-6)
          versions-6 (matching-items '(nil :format) store-6)
          store-7 (convert-from-6-to-7 store-6)
          tables-7 (matching-items '(nil :table) store-7)
          versions-7 (matching-items '(nil :format) store-7)]
      (is (= (count tables-1) 1))
      (is (check (canonicalize-list (to-list (first tables-1)))
                 (canonicalize-list table-list-1)))
      (is (= (count versions-1) 1))
      (is (= (to-list (first versions-1)) '(1 :format)))
      
      (is (= (count tables-3) 1))
      (is (check (canonicalize-list (to-list (first tables-3)))
                 (canonicalize-list table-list-3)))
      (is (= (count versions-3) 1))
      (is (= (to-list (first versions-3)) '(3 :format)))
      
      (is (= (count tables-4) 1))
      (is (check (canonicalize-list (to-list (first tables-4)))
                 (canonicalize-list table-list-4)))
      (is (= (count versions-4) 1))
      (is (= (to-list (first versions-4)) '(4 :format)))
 
      (is (= (count tables-5) 1))
      (is (check (canonicalize-list (to-list (first tables-5)))
                 (canonicalize-list table-list-5)))
      (is (= (count versions-5) 1))
      (is (= (to-list (first versions-5)) '([5] :format)))
      
      (is (= (count tables-6) 1))
      (is (check (canonicalize-list (to-list (first tables-6)))
                 (canonicalize-list table-list-6)))
      (is (= (count versions-6) 1))
      (is (= (to-list (first versions-6)) '(6 :format)))

      (is (= (count tables-7) 1))
      (println "!!!" (to-list (first tables-7)))
      (is (check (canonicalize-list (to-list (first tables-7)))
                 (canonicalize-list table-list-7)))
      (is (= (count versions-7) 1))
      (is (= (to-list (first versions-7)) '(7 :format)))

      (is (= (convert-to-current store-0) store-7))
      (is (= (convert-to-current store-1) store-7))
      (is (= (convert-to-current store-3) store-7))
      (is (= (convert-to-current store-4) store-7))
      (is (= (convert-to-current store-5) store-7))
      (is (= (convert-to-current store-6) store-7)))))

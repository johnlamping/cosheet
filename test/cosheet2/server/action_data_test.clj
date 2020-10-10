(ns cosheet2.server.action-data-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet2 [orderable :as orderable]
                      [entity :as entity  :refer [description->entity]]
                      [store :refer [new-element-store]]
                      [store-utils :refer [add-entity]]
                      [query :refer [matching-elements]]
                      [expression :refer [expr expr-let expr-seq]]
                      [debug :refer [simplify-for-print]]
                      [test-utils :refer [check any as-set]])
            (cosheet2.server [action-data :refer :all])
            ; :reload
            ))

(deftest run-action-data-getter-test
  (is (= (run-action-data-getter
          [(fn [spec cad action store extra]
             (is (= spec {:spec "spec"}))
             (is (= cad {:value 2}))
             (is (= action :action))
             (is (= store :store))
             (is (= extra 1))
             {:value 3})
           1]
          {:spec "spec"} {:value 2} :action :store)
         {:value 3}))
  (is (= (run-action-data-getter
          [(fn [spec cad action store]
             (is (= spec {:spec "spec"}))
             (is (= cad {:value 2  :store "foo"}))
             (is (= action :action))
             (is (= store "foo"))
             {:value 3})]
          {:spec "spec"} {:value 2 :store "foo"} :action :store)
         {:value 3})))

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
                    ("age" :tag (~o3 :order :non-semantic))
                    ("doubtful" ("confidence" (~o4 :order :non-semantic))
                                (~o4 :order :non-semantic)) )
                ("married" (~o2 :order :non-semantic))
                (45 (~o4 :order :non-semantic)
                    ("age" :tag (~o3 :order :non-semantic)))))
(def jane-list `("Jane" (~o1 :order :non-semantic)
                 ("female" (~o2 :order :non-semantic))
                 (45 (~o3 :order :non-semantic)
                     ("age" :tag (~o3 :order :non-semantic)))))
(def dup-list `("dup" (~o1 :order :non-semantic)
                ("female" (~o2 :order :non-semantic))
                ("female" (~o3 :order :non-semantic))))
(def age-condition-list '(anything ("age" :tag)))
(def t1 (add-entity (new-element-store) nil joe-list))
(def joe-id (second t1))
(def t2 (add-entity (first t1) nil jane-list))
(def jane-id (second t2))
(def t3 (add-entity (first t2) nil dup-list))
(def dup-id (second t3))
(def t4 (add-entity (first t3) nil age-condition-list))
(def age-condition-id (second t4))
(def store (first t4))
(def joe (description->entity joe-id store))
(def joe-age (first (matching-elements 45 joe)))
(def joe-male (first (matching-elements "male" joe)))
(def joe-bogus-age (first (matching-elements 39 joe)))
(def joe-age-tag (first (matching-elements "age" joe-age)))
(def jane (description->entity jane-id store))
(def jane-age (first (matching-elements 45 jane)))
(def jane-female (first (matching-elements "female" jane)))
(def jane-age-tag (first (matching-elements "age" jane-age)))
(def dup (description->entity dup-id store))
(def dup-females (matching-elements "female" dup))
(def dup-female-1 (first dup-females))
(def dup-female-2 (second dup-females))

(deftest best-match-test
  (is (nil? (best-match 1 [])))
  (is (= (best-match 1 [1]) 1))
  (is (= (best-match '(nil 1) ['(1 1) '("" 1)])  '("" 1)))
  (is (= (best-match '(nil 1) ['(1 1) '(anything 1)])  '(anything 1)))
  (is (= (best-match '(nil 1) ['(1 1 2) '(1 1) '(1 1 1)])  '(1 1))))

(deftest get-item-or-exemplar-action-data-test
  (is (= (get-item-or-exemplar-action-data
          {:relative-id joe-id} {} nil store)
         {:target-ids [joe-id]}))
  (is (= (get-item-or-exemplar-action-data
          {:relative-id (:item-id joe-age)}
          {:target-ids [joe-id]} nil store)
         {:target-ids [(:item-id joe-age)]}))
  (is (= (get-item-or-exemplar-action-data
          {:relative-id (:item-id jane-age)}
          {:target-ids [joe-id jane-id]} nil store)
         {:target-ids [(:item-id joe-age) (:item-id jane-age)]}))
  (is (= (get-item-or-exemplar-action-data
          {:relative-id (:item-id dup-female-2)}
          {:target-ids [joe-id jane-id dup-id]} nil store)
         {:target-ids [(:item-id jane-female) (:item-id dup-female-2)]})))

(deftest composed-get-action-data-test
  (is (= (composed-get-action-data
          {:spec "spec"} {:value 2} :action :store
          (fn [spec cad action store]
            (is (= spec {:spec "spec"}))
            (is (= cad {:value 2}))
            (is (= action :action))
            (is (= store :store))
            {:value 3})
          (fn [spec cad action store]
            (is (= spec {:spec "spec"}))
            (is (= cad {:value 3}))
            (is (= action :action))
            (is (= store :store))
            {:value 4}))
         {:value 4})))


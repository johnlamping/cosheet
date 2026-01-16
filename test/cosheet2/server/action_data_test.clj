(ns cosheet2.server.action-data-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet2 [orderable :as orderable]
                      [entity :as entity  :refer [id->entity
                                                  elements to-list]]
                      [store :refer [new-element-store ImmutableStore
                                     id->target id->source
                                     target-label->ids]] 
                      [store-utils :refer [add-element]]
                      [query :refer [matching-elements]]
                      [expression :refer [expr expr-let expr-seq]]
                      [debug :refer [simplify-for-print]]
                      [test-utils :refer [check any as-set]])
            (cosheet2.server [action-data :refer :all]
                             [order-utils :refer [add-order-elements
                                                  ordered-entities
                                                  order-recursively]]
                             [model-utils :refer [semantic-to-list
                                                  semantic-elements]]
                             [render-utils :refer [make-sequential-template]])
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
                (~o2 :order)
                ("male" (~o1 :order))
                (39 (~o3 :order)
                    ("age" :tag (~o3 :order))
                    ("doubtful" ("confidence" (~o4 :order))
                                (~o4 :order)) )
                ("married" (~o2 :order))
                (45 (~o4 :order)
                    ("age" :tag (~o3 :order)))))
(def jane-list `("Jane" (~o1 :order)
                 (:selector)
                 ("female" (~o2 :order))
                 (45 (~o3 :order)
                     ("age" :tag (~o3 :order)))))
(def dup-list `("dup" (~o1 :order)
                ("female" (~o2 :order))
                ("female" (~o3 :order))))
(def age-condition-list '(anything ("age" :tag)))
(def t1 (add-element (new-element-store) nil joe-list))
(def joe-id (second t1))
(def t2 (add-element (first t1) nil jane-list))
(def jane-id (second t2))
(def t3 (add-element (first t2) nil dup-list))
(def dup-id (second t3))
(def t4 (add-element (first t3) nil age-condition-list))
(def age-condition-id (second t4))
(def store (first t4))
(def joe (id->entity joe-id store))
(def joe-age (first (matching-elements 45 joe)))
(def joe-male (first (matching-elements "male" joe)))
(def joe-bogus-age (first (matching-elements 39 joe)))
(def joe-age-tag (first (matching-elements "age" joe-age)))
(def jane (id->entity jane-id store))
(def jane-age (first (matching-elements 45 jane)))
(def jane-female (first (matching-elements "female" jane)))
(def jane-age-tag (first (matching-elements "age" jane-age)))
(def dup (id->entity dup-id store))
(def dup-females (matching-elements "female" dup))
(def dup-female-1 (first dup-females))
(def dup-female-2 (second dup-females))

(deftest best-match-test
  (is (nil? (best-match 1 [])))
  (is (= (best-match 1 [1]) 1))
  (is (= (best-match '(nil 1) ['(1 1) '("" 1)])  '("" 1)))
  (is (= (best-match '(nil 1) ['(1 1) '(anything 1)])  '(anything 1)))
  (is (= (best-match '(nil 1) ['(1 1 2) '(1 1) '(1 1 1)])  '(1 1)))
  (is (= (best-match 1 ['(1 1 2) '(1 1) '(1 1 1)])  '(1 1))))

(deftest get-id-action-data-test
  (is (= (get-id-action-data
          {:relative-id joe-id} {:foo :bar} nil store :test-id)
         {:foo :bar
          :subject-ids [:test-id]})))

(deftest get-item-or-exemplar-action-data-test
  (is (= (get-item-or-exemplar-action-data
          {:relative-id joe-id} {:foo :bar} nil store)
         {:foo :bar
          :subject-ids [joe-id]}))
  (is (= (get-item-or-exemplar-action-data
          {:relative-id (:item-id joe-age)}
          {:subject-ids [joe-id] :past-subject-ids [["x"]]} nil store)
         {:subject-ids [(:item-id joe-age)]
          :past-subject-ids [[joe-id] ["x"]]}))
  (is (= (get-item-or-exemplar-action-data
          {:relative-id (:item-id jane-age)}
          {:subject-ids [joe-id jane-id]} nil store)
         {:subject-ids [(:item-id joe-age) (:item-id jane-age)]
          :past-subject-ids [[joe-id jane-id]]}))
  (is (= (get-item-or-exemplar-action-data
          {:relative-id (:item-id dup-female-2)}
          {:subject-ids [joe-id jane-id dup-id]} nil store)
         {:subject-ids [(:item-id jane-female) (:item-id dup-female-2)]})))

(defn get-order [id store]
  (let [elements (target-label->ids store id :order)]
    (id->source store (first elements))))

(deftest get-virtual-action-data-test
  (let [data (get-virtual-action-data
              {:template 'anything} {:subject-ids [joe-id]} nil store)]
    (is (check data {:subject-ids [(any)]
                     :past-subject-ids [[joe-id]]
                     :store (any #(satisfies? ImmutableStore %))}))
    (let [original-store store
          {:keys [subject-ids store]} data
          id (first subject-ids)]
      (is (= (id->target store id) joe-id))
      (is (= (:right (get-order id store))
             (:right (get-order joe-id original-store))))
      (is (= (semantic-to-list (id->entity id store))
             ""))))
  ;; Try several initial targets, one a selector and one not, and a
  ;; vector as the template.
  (let [data (get-virtual-action-data
              {:template (make-sequential-template
                          ['anything '(2 ("name" :label))])}
              {:subject-ids [jane-id joe-id]} nil store)]
    (println jane-id joe-id)
    (is (check data {:subject-ids [(any) (any)]
                     :past-subject-ids [[(any) (any)] [jane-id joe-id]]
                     :store (any #(satisfies? ImmutableStore %))}))
    (let [original-store store
          {:keys [subject-ids store]} data
          [new-jane-id new-joe-id] subject-ids]
      (is (=  (id->target store (id->target store new-joe-id)) joe-id))
      (is (check (semantic-to-list (id->entity new-joe-id store))
                 '(2 ("name" :label))))
      (is (check (semantic-to-list (id->entity
                                    (id->target store new-joe-id) store))
                 '("" (2 ("name" :label)))))
      (is (= (id->target store (id->target store new-jane-id)) jane-id))
      (is (check (semantic-to-list (id->entity new-jane-id store))
                 '(2 ("name" :label))))
      (is (check (semantic-to-list (id->entity
                                    (id->target store new-jane-id) store))
                 '(anything (2 ("name" :label)))))))
  ;; Try :sibling true
  (let [data (get-virtual-action-data
              {:template 'anything
               :sibling true
               :position :before}
              {:subject-ids [(:item-id joe-age)]} nil store)]
    (is (check data {:subject-ids [(any)]
                     :past-subject-ids [[(:item-id joe)]]
                     :store (any #(satisfies? ImmutableStore %))}))
    (let [original-store store
          {:keys [subject-ids store]} data
          id (first subject-ids)]
      (is (= (id->target store id) joe-id))
      (is (= (:left (get-order id store))
             (:left (get-order (:item-id joe-age) original-store))))
      (is (< (:right (get-order id store))
             (:right (get-order (:item-id joe-age) original-store))))
      (is (check (semantic-to-list (id->entity id store))
                 ""))))
  ;; Try an adjacent query.
  (let [data (get-virtual-action-data
              {:template '(anything 2)
               :adjacent-query '(nil "age")
               :position :before}
              {:subject-ids [jane-id joe-id]} nil store)]
    (is (check data {:subject-ids [(any) (any)]
                     :past-subject-ids [[jane-id joe-id]]
                     :store (any #(satisfies? ImmutableStore %))}))
    (let [original-store store
          {:keys [subject-ids store]} data
          [new-jane-id new-joe-id] subject-ids]
      (is (= (id->target store new-joe-id) joe-id))
      (is (check (semantic-to-list (id->entity new-joe-id store))
                 '("" 2)))
      (is (check (map semantic-to-list
                      (semantic-elements
                       (order-recursively
                        (id->entity joe-id store))))
                 '("male" "married"
                   ("" 2) (39 "age" ("doubtful" "confidence")) (45 "age"))))
      (is (= (id->target store new-jane-id) jane-id))
      (is (check (map semantic-to-list
                      (ordered-entities
                       (semantic-elements
                        (id->entity jane-id store))))
                 '("female" (anything 2) (45 "age")))))))

(deftest get-item-do-batch-edit-action-data-test
  (is (check (get-item-do-batch-edit-action-data
              {:item-id (:item-id joe-id)}
              {:query-ids []
               :stack-ids [jane-id]}
              nil store)
             {:query-ids []
              :stack-ids [jane-id]}))
  (is (check (get-item-do-batch-edit-action-data
              {:item-id joe-id}
              {:query-ids [jane-id]
               :stack-ids [joe-id]}
              nil store)
             {:query-ids [jane-id]
              :stack-ids [joe-id]
              :selected-index 0}))
  (is (check (get-item-do-batch-edit-action-data
              {:item-id joe-id}
              {:query-ids []
               :stack-ids [jane-id joe-id]}
              nil store)
             {:query-ids []
              :stack-ids [jane-id joe-id]
              :selected-index 1}))
  (is (check (get-item-do-batch-edit-action-data
              {:item-id (:item-id joe-age)}
              {:query-ids []
               :stack-ids [jane-id joe-id]}
              nil store)
             {:query-ids []
              :stack-ids [jane-id joe-id]
              :selected-index 1
              :selection-sequence [(:item-id joe-age)]}))
  (is (check (get-item-do-batch-edit-action-data
              {:item-id (:item-id joe-age)}
              {:query-ids []
               :stack-ids [jane-id joe-id]
               :selected-index 1}
              nil store)
             {:query-ids []
              :stack-ids [jane-id joe-id]
              :selected-index 1
              :selection-sequence [(:item-id joe-age)]}))
  (is (check (get-item-do-batch-edit-action-data
              {:relative-id (:item-id joe-age-tag)}
              {:query-ids []
               :stack-ids [jane-id joe-id]
               :selected-index 1
               :selection-sequence [(:item-id joe-age)]}
              nil store)
             {:query-ids []
              :stack-ids [jane-id joe-id]
              :selected-index 1
              :selection-sequence [(:item-id joe-age)
                                   (:item-id joe-age-tag)]})))

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

(deftest parallel-items-get-action-data-test
  (is (= (parallel-items-get-action-data
          {:parallel-ids [joe-id]}
          {} nil store get-item-or-exemplar-action-data)
         {:subject-ids [joe-id]}))
  (is (= (parallel-items-get-action-data
          {:parallel-ids [(:item-id joe-age)]}
          {:subject-ids [joe-id]} nil store 
          get-item-or-exemplar-action-data)
         {:subject-ids [(:item-id joe-age)]}))
  (is (check (parallel-items-get-action-data
              {:parallel-ids  [(:item-id jane-age)]}
              {:subject-ids [joe-id jane-id]} nil store
              get-item-or-exemplar-action-data) 
             {:subject-ids [(:item-id joe-age) (:item-id jane-age)]}))
  (is (check (parallel-items-get-action-data
              {:parallel-ids [(:item-id jane-age) (:item-id joe-male)]}
              {:subject-ids [joe-id jane-id]} nil store
              get-item-or-exemplar-action-data)
             {:subject-ids [(:item-id joe-age) (:item-id jane-age)
                           (:item-id joe-male)]})))

(deftest parallel-items-get-do-batch-edit-action-data-test
  (is (check (parallel-items-get-do-batch-edit-action-data
              {:item-id (:item-id joe-age)
               :parallel-ids [(:item-id joe-age-tag)]}
              {:query-ids [jane-id]
               :stack-ids [joe-id]
               :selected-index 0
               :selection-sequence [(:item-id joe-age)]}
              nil store get-item-do-batch-edit-action-data)
             {:query-ids [jane-id]
              :stack-ids [joe-id]
              :selected-index 0
              :selection-sequence [(:item-id joe-age)
                                   (:item-id joe-age-tag)]})))


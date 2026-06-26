(ns cosheet.server.order-utils-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet
             [entity :as entity :refer [id->entity elements
                                        make-tree-element make-tree-object]]
             [orderable :as orderable]
             [reporter :refer [reporter-value-or-invalid make-reporter invalid
                               set-value!]]
             [reporter-macros :refer [app-R]]
             [task-queue :refer [make-priority-task-queue]]
             [calculator :refer [make-calculator-data request compute]]
             entity-impl
             [query :refer [matching-elements]]
             query-impl
             [store :refer [new-element-store new-mutable-store store-update!]]
             store-impl
             mutable-store-impl
             [store-utils :refer [add-element remove-entity-by-id
                                  link-type-object]]
             [test-utils :refer [check]])
            (cosheet.server
             [order-utils :refer :all]
             [model-utils :refer [semantic-to-tree]])
            ; :reload
            ))

(def orderables (reduce (fn [os _]
                          (vec (concat (pop os)
                                       (orderable/split (peek os) :after))))
                        [orderable/initial]
                        (range 7)))
(def o1 (nth orderables 0))
(def o2 (nth orderables 1))
(def o3 (nth orderables 2))
(def o4 (nth orderables 3))
(def o5 (nth orderables 4))
(def o6 (nth orderables 5))
(def o7 (nth orderables 6))
(def joe-list `("Joe"
                (~o1 :order)
                ("male" (~o2 :order))
                ("married" (~o3 :order))
                (39 (~o4 :order)
                    (~(link-type-object "age") (~o6 :order))
                    ("doubtful" "confidence" (~o7 :order)))
                (45 (~o5 :order)
                    (~(link-type-object "age")))))
(def t1 (add-element (new-element-store) nil joe-list))
(def joe-id (second t1))
(def store (first t1))
(def joe (id->entity joe-id store))
(def joe-male (first (matching-elements "male" joe)))
(def joe-married (first (matching-elements "married" joe)))
(def joe-39 (first (matching-elements 39 joe)))
(def joe-45 (first (matching-elements 45 joe)))

(def joe-reversed-list `("Joe"
                         (~o1 :order)
                         (45 (~(link-type-object "age"))
                             (~o5 :order))
                         (39 (~o4 :order)
                             ("doubtful" "confidence" (~o7 :order))
                             (~(link-type-object "age") (~o6 :order)))
                         ("married" (~o3 :order))
                         ("male" (~o2 :order))))

(deftest semantic-element?-test
  (is (semantic-element? (make-tree-element :source 1 [2])))
  (is (semantic-element? (make-tree-element :source "1" [2])))
  (is (semantic-element? (make-tree-element :source 'anything [2])))
  (is (semantic-element? (make-tree-element :source :name [2])))
  (is (semantic-element? (make-tree-element
                          :source (make-tree-object [3]) [2])))
  (is (semantic-element? (make-tree-element
                          :target (make-tree-object [3]) [2])))
  (is (not (semantic-element? (make-tree-element :source :foo [2])))))

(deftest ordered-ids-test
  ;; Also tests ordered-ids-R on an immutable store.
  (let [joe-semantic-elements (filter semantic-element? (elements joe))
        joe-ordered-semantic-elements [joe-male joe-married joe-39 joe-45]]
    (is (= (ordered-ids (map :item-id joe-semantic-elements) store)
           (map :item-id joe-ordered-semantic-elements)))
    (is (= (ordered-ids (reverse (map :item-id joe-semantic-elements)) store)
           (map :item-id joe-ordered-semantic-elements)))
    (is (= (ordered-ids-R (map :item-id joe-semantic-elements) store)
           (map :item-id joe-ordered-semantic-elements)))
    (is (= (ordered-ids-R (reverse (map :item-id joe-semantic-elements)) store)
           (map :item-id joe-ordered-semantic-elements)))))

(deftest ordered-entities-test
  (let [joe-semantic-elements (filter semantic-element? (elements joe))
        joe-ordered-semantic-elements [joe-male joe-married joe-39 joe-45]]
    (is (= (ordered-entities joe-semantic-elements)
           joe-ordered-semantic-elements))
    (is (= (ordered-entities (reverse joe-semantic-elements))
           joe-ordered-semantic-elements))))

(deftest order-recursively-test
  (is (check (semantic-to-tree (order-recursively joe-reversed-list))
             (semantic-to-tree joe-list))))

(deftest ordered-ids-R-test
  (let [joe-semantic-elements (filter semantic-element? (elements joe))
        joe-semantic-element-ids (map :item-id joe-semantic-elements)
        joe-ordered-semantic-elements [joe-male joe-married joe-39 joe-45]
        joe-ordered-semantic-element-ids (map :item-id
                                              joe-ordered-semantic-elements)
        mutable-store (new-mutable-store store)
        elements-R (make-reporter :value joe-semantic-element-ids)
        ordered-R (ordered-ids-R elements-R mutable-store)
        copy-of-ordered-R (app-R identity ordered-R)
        cd (make-calculator-data (make-priority-task-queue 0))]
    (request copy-of-ordered-R cd)
    (is (= (reporter-value-or-invalid ordered-R) invalid))
    (is (= (reporter-value-or-invalid copy-of-ordered-R) invalid))
    (compute cd)
    (is (check (reporter-value-or-invalid ordered-R)
               joe-ordered-semantic-element-ids))
    (is (check (reporter-value-or-invalid copy-of-ordered-R)
               joe-ordered-semantic-element-ids))
    (set-value! elements-R (conj joe-semantic-element-ids joe-id))
    (compute cd)
    (is (check (reporter-value-or-invalid copy-of-ordered-R)
               (concat [joe-id] joe-ordered-semantic-element-ids)))
    (set-value! elements-R joe-semantic-element-ids)
    (compute cd)
    (is (check (reporter-value-or-invalid copy-of-ordered-R)
               joe-ordered-semantic-element-ids))
    (let [joe-39-order (first (matching-elements '(nil :order) joe-39))
          joe-39-order-id (:item-id joe-39-order)]
      (store-update! mutable-store #(remove-entity-by-id % joe-39-order-id)))
    (compute cd)
    (is (check (reporter-value-or-invalid copy-of-ordered-R)
               (map :item-id [joe-39 joe-male joe-married joe-45])))))

(deftest furthest-item-test
  (is (= (furthest-item [joe-married] :before) joe-married))
  (is (= (furthest-item [joe-married joe-male] :before) joe-male))
  (is (= (furthest-item [joe-married joe-male] :after) joe-married)))

(deftest furthest-element-test
  (is (= (furthest-element joe :after) joe-45))
  (is (= (furthest-element joe :before) joe-male)))


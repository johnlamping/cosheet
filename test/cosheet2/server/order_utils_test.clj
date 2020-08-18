(ns cosheet2.server.order-utils-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet2
             [entity :as entity :refer [description->entity to-list
                                        label->elements elements]]
             [orderable :as orderable]
             [reporter :refer [reporter-value new-reporter invalid
                               set-value!]]
             [expression :refer [expr]]
             [task-queue :refer [new-priority-task-queue]]
             [calculator :refer [new-calculator-data request compute]]
             [debug :refer [profile-and-print-reporters
                            simplify-for-print]]
             entity-impl
             [query :refer [matching-elements]]
             query-impl
             [store :refer [new-element-store new-mutable-store store-update!]]
             store-impl
             mutable-store-impl
             [store-utils :refer [add-entity remove-entity-by-id]]
             [canonical :refer [canonicalize-list]]
             [test-utils :refer [check any]])
            (cosheet2.server
             [order-utils :refer :all])
            ; :reload
            ))

(def orderables (reduce (fn [os _]
                          (vec (concat (pop os)
                                       (orderable/split (peek os) :after))))
                        [orderable/initial]
                        (range 5)))
(def o1 (nth orderables 0))
(def o2 (nth orderables 1))
(def o3 (nth orderables 2))
(def o4 (nth orderables 3))
(def o5 (nth orderables 4))
(def unused-orderable (nth orderables 4))
(def joe-list `("Joe"
                (~o1 :order)
                ("male" (~o2 :order))
                (39 (~o4 :order)
                    ("age" :label)
                    ("doubtful" "confidence"))
                ("married" (~o3 :order))
                (45 (~o5 :order)
                    ("age" :label))))
(def t1 (add-entity (new-element-store) nil joe-list))
(def joe-id (second t1))
(def store (first t1))
(def joe (description->entity joe-id store))
(def joe-male (first (matching-elements "male" joe)))
(def joe-married (first (matching-elements "married" joe)))
(def joe-39 (first (matching-elements 39 joe)))
(def joe-45 (first (matching-elements 45 joe)))

(deftest order-items-test
  (let [joe-semantic-elements (filter semantic-element? (elements joe))
        joe-ordered-semantic-elements [joe-male joe-married joe-39 joe-45]]
    (is (= (order-items joe-semantic-elements)
           joe-ordered-semantic-elements))
    (is (= (order-items (reverse joe-semantic-elements))
           joe-ordered-semantic-elements))))

(deftest ordered-ids-R-test
  (let [joe-semantic-elements (filter semantic-element? (elements joe))
        joe-semantic-element-ids (map :item-id joe-semantic-elements)
        joe-ordered-semantic-elements [joe-male joe-married joe-39 joe-45]
        joe-ordered-semantic-element-ids (map :item-id
                                              joe-ordered-semantic-elements)
        mutable-store (new-mutable-store store)
        elements-R (new-reporter :value joe-semantic-element-ids)
        ordered-R (ordered-ids-R elements-R mutable-store)
        copy-of-ordered-R (expr identity ordered-R)
        cd (new-calculator-data (new-priority-task-queue 0))]
    (request copy-of-ordered-R cd)
    (is (= (reporter-value ordered-R) invalid))
    (is (= (reporter-value copy-of-ordered-R) invalid))
    (compute cd)
    (is (check (reporter-value ordered-R)
               joe-ordered-semantic-element-ids))
    (is (check (reporter-value copy-of-ordered-R)
               joe-ordered-semantic-element-ids))
    (set-value! elements-R (conj joe-semantic-element-ids joe-id))
    (compute cd)
    (is (check (reporter-value copy-of-ordered-R)
               (concat [joe-id] joe-ordered-semantic-element-ids)))
    (set-value! elements-R joe-semantic-element-ids)
    (compute cd)
    (is (check (reporter-value copy-of-ordered-R)
               joe-ordered-semantic-element-ids))
    (let [joe-39-order (first (matching-elements '(nil :order) joe-39))
          joe-39-order-id (:item-id joe-39-order)]
      (store-update! mutable-store #(remove-entity-by-id % joe-39-order-id)))
    (compute cd)
    (is (check (reporter-value copy-of-ordered-R)
               (map :item-id [joe-39 joe-male joe-married joe-45])))))

(deftest update-add-entity-with-order-test
  (let [[s id order] (update-add-entity-with-order-and-temporary
                      store joe-id 6
                      unused-orderable :before true)
        joe (description->entity joe-id s)
        new-entity (first (matching-elements 6 joe))
        [o5 o6] (orderable/split unused-orderable :before)]
    (is (= (to-list new-entity)
           `(6 (~o5 :order))))
    (is (= order o6))
    (is (= (:item-id new-entity) id)))
  (let [[s id order] (update-add-entity-with-order-and-temporary
                      store joe-id 6
                      unused-orderable :before false)
        joe (description->entity joe-id s)
        new-entity (first (matching-elements 6 joe))
        [o5 o6] (orderable/split unused-orderable :after)]
    (is (= (to-list new-entity)
           `(6 (~o5 :order))))
    (is (= order o6))
    (is (= (:item-id new-entity) id)))    
  (let [[s id order] (update-add-entity-with-order-and-temporary
                      store joe-id 6
                      unused-orderable :after true)
        joe (description->entity joe-id s)
        new-entity (first (matching-elements 6 joe))
        [o5 o6] (orderable/split unused-orderable :after)]
    (is (= (to-list new-entity)
           `(6 (~o6 :order))))
    (is (= order o5))
    (is (= (:item-id new-entity) id)))
  (let [[s id order] (update-add-entity-with-order-and-temporary
                      store joe-id '(6 ("height" :label))
                      unused-orderable :before true)
        joe (description->entity joe-id s)
        new-entity (first (label->elements joe "height"))
        [x o5] (orderable/split unused-orderable :before)
        [o6 o7] (orderable/split x :after)]
    (is (check (canonicalize-list (to-list new-entity))
               (canonicalize-list `(6 (~o7 :order)
                                      ("height" :label
                                       (~o6 :order))))))
    (is (= order o5))
    (is (= (:item-id new-entity) id)))
  ;; Check that order in the list style entity is preserved in the
  ;; :order values.
  ;; Also check and that non-semantic elements don't get order information
  ;; and that the entity is marked temporary, if requested.
  (let [[s id order] (update-add-entity-with-order-and-temporary
                      store joe-id '(6 ("height" :label)
                                       ("" :label)
                                       :temporary
                                       (:other ""))
                      unused-orderable :after false)
        joe (description->entity joe-id s)
        new-entity (first (label->elements joe "height"))
        [x o5] (orderable/split unused-orderable :before)
        [x o6] (orderable/split x :before)
        [o8 o7] (orderable/split x :before)]
    (is (check (canonicalize-list (to-list new-entity))
               (canonicalize-list
                `(6 (~o5 :order)
                    ("height" :label (~o7 :order))
                    ("" :label (~o6 :order))
                    :temporary
                    (:other "")))))
    (is ((:temporary-ids s) id))
    (is (= order o8))
    (is (= (:item-id new-entity) id))))

(deftest furthest-item-test
  (is (= (furthest-item [joe-married] :before) joe-married))
  (is (= (furthest-item [joe-married joe-male] :before) joe-male))
  (is (= (furthest-item [joe-married joe-male] :after) joe-married)))

(deftest furthest-element-test
  (is (= (furthest-element joe :after) joe-45))
  (is (= (furthest-element joe :before) joe-male)))

(deftest add-order-elements-test
  (let [ordered (add-order-elements `("a" ("b" "c") "d" ("e" :label)))]
    (is (check ordered
               `("a" ("b" ("c" (~(any) :order))
                      (~(any) :order))
                   ("d" (~(any) :order))
                   ("e" :label (~(any) :order))
                   (~(any) :order))))
    (is (orderable/earlier? (-> ordered second second second first)
                            (-> ordered second (nth 2) first)))
    (is (orderable/earlier? (-> ordered second (nth 2) first)
                            (-> ordered (nth 2) second first)))
    ;; The last element is not semantic, as it is order information.
    (let [semantic-elements (butlast (elements ordered))]
      (is (check (order-items semantic-elements)
                 semantic-elements))
      (is (check (order-items (reverse semantic-elements))
                 semantic-elements)))))

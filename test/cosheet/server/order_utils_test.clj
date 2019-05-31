(ns cosheet.server.order-utils-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [entity :as entity :refer [description->entity to-list
                                        label->elements]]
             [orderable :as orderable]
             [debug :refer [profile-and-print-reporters
                            simplify-for-print]]
             entity-impl
             [query :refer [matching-elements]]
             [store :refer [new-element-store]]
             store-impl
             [store-utils :refer [add-entity]]
             [canonical :refer [canonicalize-list]]
             [test-utils :refer [check any]])
            (cosheet.server
             [order-utils :refer :all])
            ; :reload
            ))

(def orderables (reduce (fn [os _]
                          (vec (concat (pop os)
                                       (orderable/split (peek os) :after))))
                        [orderable/initial]
                        (range 4)))
(def o1 (nth orderables 0))
(def o2 (nth orderables 1))
(def o3 (nth orderables 2))
(def o4 (nth orderables 3))
(def unused-orderable (nth orderables 4))
(def joe-list `("Joe"
                (~o2 :order)
                ("male" (~o1 :order))
                (39 (~o3 :order)
                    ("age" :tag)
                    ("doubtful" "confidence"))
                ("married" (~o2 :order))
                (45 (~o4 :order)
                    ("age" :tag))))
(def t1 (add-entity (new-element-store) nil joe-list))
(def joe-id (second t1))
(def store (first t1))
(def joe (description->entity joe-id store))
(def joe-male (first (matching-elements "male" joe)))
(def joe-married (first (matching-elements "married" joe)))
(def joe-45 (first (matching-elements 45 joe)))

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
                      store joe-id '(6 ("height" :tag))
                      unused-orderable :before true)
        joe (description->entity joe-id s)
        new-entity (first (label->elements joe "height"))
        [x o5] (orderable/split unused-orderable :before)
        [o6 o7] (orderable/split x :after)]
    (is (check (canonicalize-list (to-list new-entity))
               (canonicalize-list `(6 (~o7 :order)
                                      ("height" :tag
                                       (~o6 :order))))))
    (is (= order o5))
    (is (= (:item-id new-entity) id)))
  ;; Check that order in the list style entity is preserved in the
  ;; :order values.
  ;; Also check and that non-semantic elements don't get order information
  ;; and that the entity is marked temporary, if requested.
  (let [[s id order] (update-add-entity-with-order-and-temporary
                      store joe-id '(6 ("height" :tag)
                                       ("" :tag)
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
                    ("height" :tag (~o7 :order))
                    ("" :tag (~o6 :order))
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
  (let [ordered (add-order-elements `("a" ("b" "c") "d" ("e" :tag)))]
    (is (check ordered
               `("a" ("b" ("c" (~(any) :order))
                      (~(any) :order))
                   ("d" (~(any) :order))
                   ("e" :tag (~(any) :order))
                   (~(any) :order))))
    (is (orderable/earlier? (-> ordered second second second first)
                           (-> ordered second (nth 2) first)))
    (is (orderable/earlier? (-> ordered second (nth 2) first)
                           (-> ordered (nth 2) second first)))))

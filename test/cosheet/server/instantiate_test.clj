(ns cosheet.server.instantiate-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [orderable :as orderable]
                     [entity :as entity  :refer [description->entity
                                                 in-different-store]]
                     [store :refer [new-element-store]]
                     [store-utils :refer [add-entity]]
                     [query :refer [matching-elements]]
                     [expression :refer [expr expr-let expr-seq]]
                     [canonical :refer [canonicalize-list]]
                     [debug :refer [simplify-for-print]]
                     [test-utils :refer [check any as-set let-mutated]])
            (cosheet.server [instantiate :refer :all]
                            [referent :refer [item-referent exemplar-referent
                                              elements-referent query-referent
                                              union-referent difference-referent
                                              virtual-referent]]
                            [model-utils :refer [semantic-elements-R
                                                 semantic-to-list-R]]
                            [order-utils :refer [order-items-R]])
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
(def joe-list `("Joe"
                (~o2 :order :non-semantic)
                ("male" (~o1 :order :non-semantic))
                (39 (~o3 :order :non-semantic)
                    ("age" ~'tag (~o3 :order :non-semantic))
                    ("doubtful" ("confidence" (~o4 :order :non-semantic))
                                (~o4 :order :non-semantic)) )
                ("married" (~o2 :order :non-semantic))
                (45 (~o4 :order :non-semantic)
                    ("age" ~'tag (~o3 :order :non-semantic)))))
(def jane-list `("Jane" (~o1 :order :non-semantic)
                 ("female" (~o2 :order :non-semantic))
                 (45 (~o3 :order :non-semantic)
                     ("age" ~'tag (~o3 :order :non-semantic)))))
(def dup-list `("dup" (~o1 :order :non-semantic)
                ("female" (~o2 :order :non-semantic))
                ("female" (~o3 :order :non-semantic))))
(def age-condition-list '(anything ("age" tag)))
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

(deftest best-matching-test
  (let [joes `("x"
              ("Joe" ("name" ~'tag) ("id" ~'tag) (~o1 :order :non-semantic))
              ("Joe" ("name" ~'tag) (~o2 :order :non-semantic))) ]
    (is (= (best-matching-element '("Joe" ("name" tag)) joes)
           [(nth joes 2)]))
    (is (= (best-matching-element '("Joe" ("name" tag)  ("id" tag)) joes)
           [(nth joes 1)]))
    (is (= (best-matching-element '("Joe" ("age" tag)) joes)
           nil))))

(deftest condition-to-list-test
  (is (= (condition-to-list '(1 (2 :a "s")) store) '(1 (2 :a "s"))))
  (is (= (canonicalize-list (condition-to-list (item-referent jane) store))
         (canonicalize-list '("Jane" "female" (45 ("age" tag))))))
  (is (= (canonicalize-list
          (condition-to-list `(~(item-referent jane) 1 (2 3)) store))
         (canonicalize-list '("Jane" "female" (45 ("age" tag)) 1 (2 3))))))

(deftest instantiate-referent-test
  (let [referent (item-referent joe)]
    (is (= (instantiate-referent referent store) [joe])))
  (let [referent (exemplar-referent joe-age (item-referent jane))]
    (is (= (instantiate-referent referent store)
           [jane-age])))
  (let [referent (elements-referent '(nil ("age" tag)) (item-referent joe))]
    (is (check (instantiate-referent referent store)
               (as-set [joe-age joe-bogus-age]))))
  (let [referent (query-referent '(nil (nil "age")))]
    (is (check (instantiate-referent referent store)
               (as-set [joe jane]))))
  (let [referent (exemplar-referent joe-age
                                    (query-referent '(nil (nil "age"))))]
    (is (check (instantiate-referent referent store)
               (as-set [joe-age jane-age]))))
  ;; An elements referent with an item for its condition.
  (is (check (instantiate-referent
              (elements-referent age-condition-id
                                 (query-referent '(nil (nil "age")))) store)
             (as-set [joe-age joe-bogus-age jane-age])))
  (let [referent (union-referent
               [(item-referent joe-age)
                (elements-referent '(nil "age") (item-referent jane))
                (query-referent '(nil (nil "age")))])]
    (is (check (instantiate-referent referent store)
               (as-set [joe-age jane-age joe jane]))))
  ;; Exemplar of union
  (let [referent (exemplar-referent joe-age
                                    (union-referent [(item-referent joe)
                                                     (item-referent jane)]))]
    (is (check (instantiate-referent referent store)
               (as-set [joe-age jane-age]))))
  (let [referent (difference-referent (query-referent '(nil (nil "age")))
                                      (item-referent joe))]
    (is (check (instantiate-referent referent store)
               [jane])))
  ;; Exemplar of union of elements
  (let [referent (exemplar-referent
               (item-referent joe-age-tag)
               (union-referent [(elements-referent 45 (item-referent joe))
                                (elements-referent 45 (item-referent jane))]))]
    (is (check (instantiate-referent referent store)
               [joe-age-tag jane-age-tag])))
  ;; Preference of Exemplar of for exemplar element
  (is (check (instantiate-referent
              (exemplar-referent (item-referent dup-female-1)
                                 (item-referent dup))
              store)
             [dup-female-1]))
    (is (check (instantiate-referent
              (exemplar-referent (item-referent dup-female-2)
                                 (item-referent dup))
              store)
             [dup-female-2]))
  ;; Union of non-trivial sequences
  (let [referent (union-referent
                  [(elements-referent 45 (query-referent '(nil (nil "age"))))
                   (elements-referent 39 (query-referent '(nil (nil "age"))))])]
    (is (check (instantiate-referent referent store)
               (as-set [joe-age jane-age joe-bogus-age])))))

(deftest instantiate-or-create-referent-test
  (let [referent (exemplar-referent joe-age (item-referent jane))]
    (is (= (instantiate-or-create-referent referent store)
           [[jane-age] nil store])))
  ;; The simplest virtual referent.
  (let [referent (virtual-referent "male" (item-referent jane)
                                   (item-referent jane-age))
        [items new-ids store0] (instantiate-or-create-referent
                                 referent store)]
    (is (check items [(any)]))
    (is (= new-ids [(:item-id (first items))]))
    (let [[item] items
          new-jane (in-different-store jane item)]
      (is (= (:store item) store0))
      (is (= (entity/subject item) new-jane))
      (is (= (semantic-to-list-R item) "male"))
      (is (= (canonicalize-list (semantic-to-list-R new-jane))
             (canonicalize-list
              '("Jane" "female" (45 ("age" tag)) "male"))))))
  ;; A referent for the exemplar.
  (let [referent (virtual-referent joe-male (item-referent jane)
                                   (item-referent jane-age))
        [items new-ids store] (instantiate-or-create-referent
                                referent store)]
    (is (check items [(any)]))
    (is (= new-ids [(:item-id (first items))]))
    (let [[item] items
          new-jane (in-different-store jane item)]
      (is (= (:store item) store))
      (is (= (entity/subject item) new-jane))
      (is (= (semantic-to-list-R item) "male"))
      (is (= (canonicalize-list (semantic-to-list-R new-jane))
             (canonicalize-list
              '("Jane" "female" (45 ("age" tag)) "male"))))))
  ;; Multiple subjects
  (let [referent (virtual-referent '("hi" tag)
                                   (query-referent '(nil (nil "age")))
                                   (query-referent '(nil (nil "age")))
                                   :position :before)
        [items new-ids store] (instantiate-or-create-referent
                                referent store)]
    (is (check items [(any) (any)]))
    (is (= new-ids [(:item-id (first items))]))
    (let [[item1 item2] items
          new-joe (in-different-store joe item1)
          new-jane (in-different-store jane item1)]
      (is (= (:store item1) store))
      (is (= (:store item2) store))
      (is (#{new-joe new-jane} (entity/subject item1)))
      (is (#{new-joe new-jane} (entity/subject item2)))
      (is (= (semantic-to-list-R item1) '("hi" tag)))
      (is (= (semantic-to-list-R item2) '("hi" tag)))
      (is (= (canonicalize-list (semantic-to-list-R new-joe))
             (canonicalize-list
              '("Joe" ("hi" tag) "male" (45 ("age" tag))
                (39 ("age" tag) ("doubtful" "confidence")) "married"))))
      (is (= (canonicalize-list (semantic-to-list-R new-jane))
             (canonicalize-list
              '("Jane" "female" (45 ("age" tag)) ("hi" tag)))))))
  ;; Exemplar is virtual, and needs adjusting
  (let [referent (virtual-referent
                  (virtual-referent '??? (item-referent jane)
                                    (item-referent jane))
                  (item-referent joe) (item-referent joe))
        [items new-ids store] (instantiate-or-create-referent
                                referent store)]
    (is (check items [(any)]))
    (let [[item] items
          new-joe (in-different-store joe item)
          new-jane (in-different-store jane item)
          new-sym (semantic-to-list-R item)]
      (is (check new-ids [(:item-id (first items)) (any)]))
      (is (= (semantic-to-list-R (description->entity (second new-ids) store))
             new-sym))
      (is (= (:store item) store))
      (is (= (entity/subject item) new-joe))
      (is (= new-sym "\u00A0A"))
      (is (= (canonicalize-list (semantic-to-list-R new-joe))
             (canonicalize-list
              `("Joe" ~new-sym "male" "married" (45 ("age" ~'tag))
                (39 ("age" ~'tag) ("doubtful" "confidence"))))))
      (is (= (canonicalize-list (semantic-to-list-R new-jane))
             (canonicalize-list
              `("Jane" "female" (45 ("age" ~'tag)) ~new-sym))))))
  ;; Subject is virtual too, plus nil adjacent
  (let [referent (virtual-referent
                  (virtual-referent '??? (item-referent jane))
                  (virtual-referent '??? (item-referent joe)
                                    (item-referent joe))
                  (item-referent joe))
        [items new-ids store] (instantiate-or-create-referent
                                referent store)]
    (is (check items [(any)]))
    (let [[item] items
          new-joe (in-different-store joe item)
          new-jane (in-different-store jane item)
          new-sym (semantic-to-list-R item)]
      (is (check new-ids [(:item-id item) (any) (any)]))
      (is (= (semantic-to-list-R (description->entity (second new-ids) store))
             `("\u00A0B" ~new-sym)))
      (is (= (semantic-to-list-R (description->entity (nth new-ids 2) store))
             new-sym))
      (is (= (:store item) store))
      (is (= (entity/subject (entity/subject item)) new-joe))
      (is (= new-sym "\u00A0A"))
      (is (check (canonicalize-list (semantic-to-list-R new-joe))
                 (canonicalize-list
                  `("Joe"
                    ("\u00A0B" ~new-sym)
                    "male"
                    "married"
                    (45 ("age" ~'tag))
                    (39 ("age" ~'tag) ("doubtful" "confidence"))))))
      (is (check (canonicalize-list (semantic-to-list-R new-jane))
                 (canonicalize-list
                  `("Jane" "female" (45 ("age" ~'tag)) ~new-sym))))))
  ;; Multiple adjacents
  (let [referent (virtual-referent '("hi" tag)
                                   (item-referent jane)
                                   [(item-referent jane-age)
                                    (item-referent jane-female)]
                                   :position :after)
        [items new-ids store] (instantiate-or-create-referent
                                referent store)]
    (is (check items [(any)]))
    (let [[item] items
          new-jane (in-different-store jane item)]
      (is (= new-ids [(:item-id item)]))
      (is (= (:store item) store))
      (is (= new-jane (entity/subject item)))
      (is (= (semantic-to-list-R item) '("hi" tag)))
      (is (= (canonicalize-list (semantic-to-list-R new-jane))
             (canonicalize-list
              '("Jane" "female" (45 ("age" tag)) ("hi" tag)))))
      (let [ordered-elements (order-items-R (semantic-elements-R new-jane))]
        (is (= item (nth ordered-elements 2))))))
  ;; Multiple adjacents in the other order
  (let [referent (virtual-referent '("hi" tag)
                                   (item-referent jane)
                                   [(item-referent jane-age)
                                    (item-referent jane-female)]
                                   :position :before)
        [items new-ids store] (instantiate-or-create-referent
                                referent store)]
    (is (check items [(any)]))
    (let [[item] items
          new-jane (in-different-store jane item)]
      (is (= new-ids [(:item-id item)]))
      (is (= (:store item) store))
      (is (= new-jane (entity/subject item)))
      (is (= (semantic-to-list-R item) '("hi" tag)))
      (is (= (canonicalize-list (semantic-to-list-R new-jane))
             (canonicalize-list
              '("Jane" "female" (45 ("age" tag)) ("hi" tag)))))
      (let [ordered-elements (order-items-R (semantic-elements-R new-jane))]
        (is (= item (nth ordered-elements 0))))))
  ;; A union of virtual referents
  (let [r1 (virtual-referent "male" (item-referent jane)
                             (item-referent jane-age))
        r2 (virtual-referent "hello" (item-referent joe))
        [items new-ids store0] (instantiate-or-create-referent
                                 (union-referent [r1 r2]) store)]
    (is (check items [(any) (any)]))
    (is (= new-ids [(:item-id (first items))]))
    (let [[item1 item2] items
          new-jane (in-different-store jane store0)
          new-joe (in-different-store joe store0)]
      (is (= (:store item1) store0))
      (is (= (:store item2) store0))
      (is (= (entity/subject item1) new-jane))
      (is (= (entity/subject item2) new-joe))
      (is (= (semantic-to-list-R item1) "male"))
      (is (= (semantic-to-list-R item2) "hello"))
      (is (= (canonicalize-list (semantic-to-list-R new-jane))
             (canonicalize-list
              '("Jane" "female" (45 ("age" tag)) "male")))))))

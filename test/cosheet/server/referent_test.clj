(ns cosheet.server.referent-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [orderable :as orderable]
                     [entity :as entity  :refer [description->entity]]
                     [store :refer [new-element-store]]
                     [store-impl :refer [->ItemId]]
                     [store-utils :refer [add-entity]]
                     [query :refer [matching-elements]]
                     [expression :refer [expr expr-let expr-seq]]
                     [canonical :refer [canonicalize-list]]
                     [debug :refer [simplify-for-print]]
                     [test-utils :refer [check any as-set let-mutated]])
            (cosheet.server [referent :refer :all])
            ; :reload
            ))

(deftest string-conversion-test
  (let [referent (exemplar-referent
                  (item-referent (->ItemId 0))
                  (union-referent
                   [(elements-referent (item-referent (->ItemId 1))
                                       (parallel-union-referent
                                        [(item-referent (->ItemId 3))
                                         (item-referent (->ItemId 4))]))
                    (difference-referent
                     (query-referent (list (item-referent (->ItemId 3))
                                           '(nil (:root :A_a))
                                           :b))
                     (item-referent (->ItemId 6789)))]))
        serialized (referent->string referent)
        parsed (string->referent serialized)]
    (is (check parsed referent))))

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
(def age-condition-list '(anything ("age" tag)))
(def t1 (add-entity (new-element-store) nil joe-list))
(def joe-id (second t1))
(def t2 (add-entity (first t1) nil jane-list))
(def jane-id (second t2))
(def t3 (add-entity (first t2) nil age-condition-list))
(def age-condition-id (second t3))
(def store (first t3))
(def joe (description->entity joe-id store))
(def joe-age (first (matching-elements 45 joe)))
(def joe-bogus-age (first (matching-elements 39 joe)))
(def joe-age-tag (first (matching-elements "age" joe-age)))
(def jane (description->entity jane-id store))
(def jane-age (first (matching-elements 45 jane)))
(def jane-age-tag (first (matching-elements "age" jane-age)))

(deftest item-or-exemplar-referent-test
  (is (= (item-or-exemplar-referent joe nil)
         (item-referent joe)))
   (is (= (item-or-exemplar-referent joe (query-referent '(:hi :there)))
          (exemplar-referent joe (query-referent '(:hi :there))))))

(deftest semantic-test
  (let [semantic (let-mutated [him joe-list]
                  (semantic-to-list-R him))]
    (is (= (first semantic) "Joe")))
  (is (= (set (map canonicalize-list
                   (let-mutated [him joe-list]
                     (expr-seq map entity/to-list (semantic-elements-R him)))))
         (set (map canonicalize-list (rest (rest joe-list))))))
  (let [expected ["joe" {"male" 1
                         "married" 1
                         [39 {["age" {'tag 1}] 1
                              ["doubtful" {"confidence" 1}] 1}] 1
                              [45 {["age" {'tag 1}] 1}] 1}]]
    (is (= (item->canonical-semantic joe-list) expected)))
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
          (condition-to-list [(item-referent jane) 1 '(2 3)] store))
         (canonicalize-list '("Jane" "female" (45 ("age" tag)) 1 (2 3))))))

(deftest instantiate-referent-test
  ;; This also tests first-group-referent
  (let [referent (item-referent joe)]
    (is (= (instantiate-referent referent store) [[joe]]))
    (is (= [(first (instantiate-referent referent store))]
           (instantiate-referent (first-group-referent referent) store))))
  (let [referent (exemplar-referent joe-age (item-referent jane))]
    (is (= (instantiate-referent referent store)
           [[jane-age]]))
    (is (= [(first (instantiate-referent referent store))]
           (instantiate-referent (first-group-referent referent) store))))
  (let [referent (elements-referent '(nil ("age" tag)) (item-referent joe))]
    (is (check (instantiate-referent referent store)
               [(as-set [joe-age joe-bogus-age])]))
    (is (check [(first (instantiate-referent referent store))]
               (instantiate-referent (first-group-referent referent) store))))
  (let [referent (query-referent '(nil (nil "age")))]
    (is (check (instantiate-referent referent store)
               [(as-set [joe jane])]))
    (is (check [(first (instantiate-referent referent store))]
               (instantiate-referent (first-group-referent referent) store))))
  (let [referent (exemplar-referent joe-age
                                    (query-referent '(nil (nil "age"))))]
    (is (check (instantiate-referent referent store)
               [(as-set [joe-age jane-age])]))
    (is (check [(first (instantiate-referent referent store))]
               (instantiate-referent (first-group-referent referent) store))))
  ;; An elements referent with an item for its condition.
  (is (check (instantiate-referent
              (elements-referent age-condition-id
                                 (query-referent '(nil (nil "age")))) store)
             [(as-set [joe-age joe-bogus-age jane-age])]))
  (let [referent (union-referent
               [(item-referent joe-age)
                (elements-referent '(nil "age") (item-referent jane))
                (query-referent '(nil (nil "age")))])]
    (is (check (instantiate-referent referent store)
               (as-set [[joe-age] [jane-age] (as-set [joe jane])])))
    (is (check [(first (instantiate-referent referent store))]
               (instantiate-referent (first-group-referent referent) store))))
  ;; Exemplar of union
  (let [referent (exemplar-referent joe-age
                                    (union-referent [(item-referent joe)
                                                     (item-referent jane)]))]
    (is (check (instantiate-referent referent store)
               (as-set [[joe-age] [jane-age]])))
    (is (check [(first (instantiate-referent referent store))]
               (instantiate-referent (first-group-referent referent) store))))
  (let [referent (difference-referent (query-referent '(nil (nil "age")))
                                      (item-referent joe))]
    (is (check (instantiate-referent referent store)
               [[jane]]))
    (is (check [(first (instantiate-referent referent store))]
               (instantiate-referent (first-group-referent referent) store))))
  ;; Exemplar of union of elements
  (let [referent (exemplar-referent
               (item-referent joe-age-tag)
               (union-referent [(elements-referent 45 (item-referent joe))
                                (elements-referent 45 (item-referent jane))]))]
    (is (check (instantiate-referent referent store)
               [[joe-age-tag] [jane-age-tag]]))
    (is (check [(first (instantiate-referent referent store))]
               (instantiate-referent (first-group-referent referent) store))))
  ;; Union of non-trivial sequences
  (let [referent (union-referent
                  [(elements-referent 45 (query-referent '(nil (nil "age"))))
                   (elements-referent 39 (query-referent '(nil (nil "age"))))])]
    (is (check (instantiate-referent referent store)
               (as-set [(as-set [joe-age jane-age]) [joe-bogus-age]])))
    (is (check [(first (instantiate-referent referent store))]
               (instantiate-referent (first-group-referent referent) store))))
  ;; Corresponding parallel union
  (let [union-ref (union-referent [(item-referent joe) (item-referent jane)])
        referent (parallel-union-referent [(elements-referent 45 union-ref)
                                           (elements-referent 39 union-ref)])]
    (is (check (instantiate-referent referent store)
               [(as-set [joe-age joe-bogus-age]) [jane-age]]))
    (is (check [(first (instantiate-referent referent store))]
               (instantiate-referent (first-group-referent referent) store)))))

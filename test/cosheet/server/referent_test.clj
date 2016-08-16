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
                     (query-referent (item-referent (->ItemId 5)))
                     (item-referent (->ItemId 6789)))]))
        rep (referent->string referent)]
    (is (check (string->referent rep) referent))))

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
                    ("age" ~'tag)
                    ("doubtful" "confidence"))
                ("married" (~o2 :order :non-semantic))
                (45 (~o4 :order :non-semantic)
                    ("age" ~'tag))))
(def jane-list `("Jane" (~o1 :order :non-semantic)
                 ("female" (~o2 :order :non-semantic))
                 (45 (~o3 :order :non-semantic)
                     ("age" ~'tag))))
(def t1 (add-entity (new-element-store) nil joe-list))
(def joe-id (second t1))
(def t2 (add-entity (first t1) nil jane-list))
(def store (first t2))
(def jane-id (second t2))
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
    (is (= (first semantic) "Joe"))
    (is (= (set (map canonicalize-list (rest semantic)))
           #{"male"
             "married"
             '(39 {["age" {tag 1}] 1
                   ("doubtful" {"confidence" 1}) 1})
             '(45 {["age" {tag 1}] 1})})))
  (is (= (set (map canonicalize-list
                   (let-mutated [him joe-list]
                     (expr-seq map entity/to-list (semantic-elements-R him)))))
         (set (map canonicalize-list (rest (rest joe-list))))))
  (let [expected ["Joe" {"male" 1
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

(deftest instantiate-referent-test
  (is (= (instantiate-referent (item-referent joe) store) [[joe]]))
  (is (= (instantiate-referent
          (exemplar-referent joe-age (item-referent jane)) store)
         [[jane-age]]))
  (is (check (instantiate-referent
              (elements-referent '(nil ("age" tag)) (item-referent joe)) store)
             [(as-set [joe-age joe-bogus-age])]))
  (is (check (instantiate-referent (query-referent '(nil (nil "age"))) store)
             [(as-set [joe jane])]))
  (is (check (instantiate-referent
              (exemplar-referent joe-age
                                 (query-referent '(nil (nil "age")))) store)
             (as-set [[joe-age] [jane-age]])))
  (is (check (instantiate-referent
              (elements-referent '(nil "age")
                                 (query-referent '(nil (nil "age")))) store)
             (as-set [(as-set [joe-age joe-bogus-age])
                      [jane-age]])))
  (is (check (instantiate-referent
              (union-referent
               [(item-referent joe-age)
                (elements-referent '(nil "age") (item-referent jane))
                (query-referent '(nil (nil "age")))]) store)
             (as-set [[joe-age] [jane-age] (as-set [joe jane])])))
  ;; Exemplar of union
  (is (check (instantiate-referent
              (exemplar-referent joe-age
                                 (union-referent [(item-referent joe)
                                                  (item-referent jane)])) store)
             (as-set [[joe-age] [jane-age]])))
  (is (check (instantiate-referent
              (difference-referent (query-referent '(nil (nil "age")))
                                   (item-referent joe)) store)
             [[jane]]))
  ;; Exemplar of union of elements
  (is (check (instantiate-referent
              (exemplar-referent
               (item-referent joe-age-tag)
               (union-referent [(elements-referent 45 (item-referent joe))
                                (elements-referent 45 (item-referent jane))]))
              store)
             (as-set [[joe-age-tag] [jane-age-tag]])))
  ;; Union of non-trivial sequences
  (is (check (instantiate-referent
              (union-referent
               [(elements-referent 45 (query-referent '(nil (nil "age"))))
                (elements-referent 39 (query-referent '(nil (nil "age"))))])
              store)
             (as-set [(as-set [joe-age jane-age]) [joe-bogus-age]])))
  ;; Corresponding parallel union
  (is (check (instantiate-referent
              (parallel-union-referent
               [(elements-referent 45 (query-referent '(nil (nil "age"))))
                (elements-referent 39 (query-referent '(nil (nil "age"))))])
              store)
             (as-set [(as-set [joe-age joe-bogus-age]) [jane-age]]))))

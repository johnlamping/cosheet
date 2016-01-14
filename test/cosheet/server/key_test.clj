(ns cosheet.server.key-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [orderable :as orderable]
                     [entity :as entity  :refer [to-list description->entity
                                                 content elements
                                                 label->elements]]
                     [store :refer [new-element-store]]
                     [store-impl :refer [->ItemId]]
                     [store-utils :refer [add-entity]]
                     [expression :refer [expr expr-let expr-seq]]
                     [test-utils :refer [check any as-set let-mutated]])
            (cosheet.server [key :refer :all])
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
                (~o2 :order)
                ("male" (~o1 :order))
                (39 (~o3 :order)
                    ("age" ~'tag)
                    ("doubtful" "confidence"))
                ("married" (~o2 :order))
                (45 (~o4 :order)
                    ("age" ~'tag))))
(def jane-list `("Jane" (~o1 :order)
                 ("female" (~o2 :order))
                 (45 (~o3 :order)
                     ("age" ~'tag))))
(def t1 (add-entity (new-element-store) nil joe-list))
(def joe-id (second t1))
(def t2 (add-entity (first t1) nil jane-list))
(def store (first t2))
(def jane-id (second t2))
(def joe (description->entity joe-id store))
(def joe-age (first (filter #(= (content %) 45) (elements joe))))
(def joe-bogus-age (first (filter #(= (content %) 39) (elements joe))))
(def joe-age-tag (first (elements joe-age)))
(def joe-male (first (filter #(= (content %) "male") (elements joe))))
(def joe-married (first (filter #(= (content %) "married")
                                (elements joe))))
(def jane (description->entity jane-id store))
(def jane-age (first (label->elements jane "age")))
(def jane-age-tag (first (elements jane-age)))

(deftest prepend-to-key-test
  (let [a (item-referent (description->entity (->ItemId :a) store))
        b (item-referent (description->entity (->ItemId :b) store))
        c (item-referent (description->entity (->ItemId :c) store))
        p (parallel-referent [] [b])
        pp (parallel-referent [p a] [b] [b c])]
    (is (= (prepend-to-key a [b]) [a b]))
    (is (= (prepend-to-key c [p a])
           [(parallel-referent [c] [b]) a]))
    (is (= (prepend-to-key c [pp a])
           [(parallel-referent [(parallel-referent [c] [b]) a]
                               [b]
                               [b c])
            a]))))

(deftest remove-first-primitive-referent-test
  (let [a (item-referent (description->entity (->ItemId :a) store))
        b (item-referent (description->entity (->ItemId :b) store))
        c (item-referent (description->entity (->ItemId :c) store))]
    (is (= (remove-first-primitive-referent [a b])
           [b]))
    (is (= (remove-first-primitive-referent [[:parallel [] [a b]] c])
           [c]))
    (is (= (remove-first-primitive-referent [[:parallel [a b] [b c] [c a]] b])
           [[:parallel [b] [b c] [c a]] b]))))

(deftest remove-content-location-referent-test
  (let [a (item-referent (description->entity (->ItemId :a) store))
        b (item-referent (description->entity (->ItemId :b) store))
        c (item-referent (description->entity (->ItemId :c) store))]
    (is (= (remove-content-location-referent [])
           []))
    (is (= (remove-content-location-referent [(content-location-referent) a b])
           [a b]))
    (is (= (remove-content-location-referent
            [[:parallel [(content-location-referent) a] [b c] [c a]] c])
           [[:parallel [a] [b c] [c a]] c]))
    (is (= (remove-content-location-referent [[:parallel [a b] [b c]] b])
           [[:parallel [a b] [b c]] b]))))

(deftest remove-comments-test
  (is (= (remove-comments
          [[:parallel
            [(content-location-referent)
             (comment-referent :foo)
             :a
             (query-referent 1)]
            [(key-referent [(query-referent 1) (comment-referent :bar)]) :c]]
           (comment-referent :baz)])
         [[:parallel
            [(query-referent 1)]
           [(key-referent [(query-referent 1)])]]]))
   (is (= (remove-comments
          [[:parallel
            [(content-location-referent)
             (comment-referent :foo)
             :a
             (query-referent 1)]
            [(key-referent [(query-referent 1) (comment-referent :bar)]) :c]
            [(key-referent [(elements-referent 1) (comment-referent :bar)]) :c]]
           (comment-referent :baz)])
         [[:parallel
            [(query-referent 1)]
           [(key-referent [(query-referent 1)])]
           [(key-referent [(elements-referent 1)])]]])))

(deftest item-ids-referred-to-test
  (is (check (item-ids-referred-to [[:parallel [(->ItemId 0) (->ItemId 1)]
                                     [(->ItemId 2) (->ItemId 3)]]
                                    (->ItemId 4)])
             (as-set [(->ItemId 0) (->ItemId 1) (->ItemId 4)]))))

(deftest semantic-test
  (let [semantic (let-mutated [him joe-list]
                  (semantic-to-list him))]
    (is (= (first semantic) "Joe"))
    (is (= (set (map canonicalize-list (rest semantic)))
           #{"male"
             "married"
             '(39 {["age" {tag 1}] 1
                   ("doubtful" {"confidence" 1}) 1})
             '(45 {["age" {tag 1}] 1})})))
  (is (= (set (map canonicalize-list
                   (let-mutated [him joe-list]
                     (expr-seq map to-list (semantic-elements him)))))
         (set (map canonicalize-list (rest (rest joe-list)))))))

(deftest canonical-semantic-test
  (let [expected ["Joe" {"male" 1
                         "married" 1
                         [39 {["age" {'tag 1}] 1
                              ["doubtful" {"confidence" 1}] 1}] 1
                              [45 {["age" {'tag 1}] 1}] 1}]]
    (is (= (item->canonical-semantic joe-list) expected))))


(deftest semantic-matching-element-test
  (let [joe `("x"
              ("Joe" ("name" ~'tag) ("id" ~'tag) (~o1 :order))
              ("Joe" ("name" ~'tag) (~o2 :order)))
        matching (semantic-matching-element '("Joe" ("name" tag)) joe)]
    (is (= matching (nth joe 2)))))

(deftest bind-referents-test
  (is (check (bind-referents [(item-referent joe)
                              (comment-referent :foo)
                              (content-referent)
                              (key-referent
                               [(query-referent joe)
                                (elements-referent joe)])
                              (parallel-referent
                               [(parallel-referent
                                 [(item-referent joe)]
                                 [(item-referent jane)
                                  (key-referent [(item-referent joe)
                                                 (item-referent jane)])])
                                (item-referent joe)]
                               [(item-referent jane)
                                (key-referent [(item-referent joe)
                                               (item-referent jane)])]
                               [(item-referent joe)
                                (key-referent [(item-referent jane)
                                               (item-referent joe)])])]
                             store false)
             [[:bound-item joe]
              (content-referent)
              [:key [[:query (semantic-to-list joe)]
                     [:elements (semantic-to-list joe)]]]
              [:parallel
               [[:parallel
                 [[:template (semantic-to-list joe)]]
                 [[:template (semantic-to-list jane)]
                  [:key [[:template (semantic-to-list joe)]
                         [:template (semantic-to-list jane)]]]]]
                [:template (semantic-to-list joe)]]
               [[:bound-item jane]
                [:key [[:bound-item joe] [:bound-item jane]]]]
               [[:bound-item joe]
                [:key [[:bound-item jane] [:bound-item joe]]]]]])))

(deftest instantiate-exemplar-test
  (let [bind #(vec (bind-referents % store false))
        bind-e #(vec (bind-referents % store true))
        make-instantiator inc]
    (is (= (instantiate-bound-key store false
                                  (bind [(item-referent joe-male)])
                                  joe)
           [joe-male]))
    (is (= (instantiate-bound-key store false
                                  (bind [(comment-referent :c)
                                         (item-referent joe-male)
                                         (comment-referent :d)])
                                 joe)
           [joe-male]))
    (is (= (instantiate-bound-key store false
                                  (bind [(content-referent)
                                         (item-referent joe-male)])
                                 joe)
           ["male"]))
    (is (= (instantiate-bound-key store false (bind [(item-referent jane-age)])
                                  jane)
           [jane-age]))
    (is (= (instantiate-bound-key store false
                                  (bind-e [(item-referent jane-age)])
                                  joe)
           [joe-age]))
    (is (= (instantiate-bound-key store false
                                  (bind-e [(item-referent joe-age-tag)
                                           (item-referent joe-age)])
                                  jane)
           [jane-age-tag]))
    (is (= (instantiate-bound-key store false
                                  (bind-e [(key-referent
                                            [joe-age-tag joe-age])])
                                  jane)
           [jane-age-tag]))
    (is (= (instantiate-bound-key store false
                                  (bind [(parallel-referent
                                          [] [joe-male joe-age])])
                                 joe)
           [joe-male joe-age]))
    (is (= (instantiate-bound-key store false
                                  (bind [(parallel-referent
                                          [] [joe-male joe-age] [joe-age])])
                                  joe)
           [joe-male]))
    (is (= (instantiate-bound-key
            store false
            (bind [(parallel-referent [joe-age-tag] [joe-male joe-age])])
            joe)
           [joe-age-tag]))
    (is (= (instantiate-bound-key
            store false
            (bind [(parallel-referent
                    [joe-age-tag] [joe-male joe-age]  [joe-age])])
            joe)
           []))
    ;; Try key referents in a parallel
    (is (= (instantiate-bound-key
            store false
            (bind [(parallel-referent
                    []
                    [(key-referent [joe-age-tag joe-age])])])
            joe)
           [joe-age-tag]))
    ;; Try nested parallel.
    (is (= (instantiate-bound-key
            store false
            (bind [(parallel-referent
                    [(parallel-referent [] [joe-age-tag])]
                    [joe-male joe-age])])
            joe)
           [joe-age-tag]))
    ;; Now try the same things, but grouped.
    (is (= (instantiate-bound-key
            store true (bind [(item-referent joe-male)]) joe)
           [[joe-male]]))
    (is (empty? (instantiate-bound-key
                 store true (bind-e [(item-referent joe-male)])
                 jane)))
    (is (= (instantiate-bound-key
            store true (bind-e [(item-referent jane-age)]) jane)
           [[jane-age]]))
    (is (= (instantiate-bound-key
            store true (bind-e [(item-referent jane-age)]) joe)
           [[joe-age]]))
    (is (= (instantiate-bound-key
            store true
            (bind-e [(item-referent joe-age-tag) (item-referent joe-age)])
            jane)
           [[jane-age-tag]]))
    (is (= (instantiate-bound-key
            store true
            (bind [[:parallel [] [(item-referent joe-male)
                                  (item-referent joe-age)]]])
            joe)
           [[joe-male joe-age]]))
    (is (= (instantiate-bound-key
            store true
            (bind [[:parallel
                    [(item-referent joe-age-tag)]
                    [(item-referent joe-male) (item-referent joe-age)]]])
            joe)
           [[joe-age-tag]]))
    (is (= (instantiate-bound-key
            store true
            (bind [[:parallel
                    [(parallel-referent [] [joe-age-tag])]
                    [(item-referent joe-male) (item-referent joe-age)]]])
            joe)
           [[joe-age-tag]]))
    ;; Try a query referent
    (is (check (instantiate-bound-key
                store false
                (bind [(query-referent '(nil (nil "age")))])
                nil)
               (as-set [joe jane])))
    ;; With an item as the condition
    (is (check (instantiate-bound-key
                store false
                (bind [(query-referent joe-age)])
                nil)
               (as-set [joe-age jane-age])))
    ;; Try an element referent
    (is (check (instantiate-bound-key
                store false
                (bind [(elements-referent '(nil "age")) (item-referent joe)])
                nil)
               (as-set [joe-age joe-bogus-age])))
    ;; With an item as the condition
    (is (= (instantiate-bound-key
                store false
                (bind [(elements-referent jane-age) (item-referent joe)])
                nil)
           [joe-age]))
    ;; Try a parallel referent with an elements referent as its referents
    (is (check (instantiate-bound-key
                store true
                (bind [(parallel-referent
                        []
                        [(elements-referent '(nil "age"))])
                       (item-referent joe)])
                nil)
               [(as-set [joe-age joe-bogus-age])]))
    (is (check (instantiate-bound-key
                store true
                (bind [(parallel-referent
                        []
                        [(elements-referent '(nil "age"))]
                        [joe-age])
                       (item-referent joe)])
                nil)
               [[joe-bogus-age]]))
    ;; Try a parallel referent with another parallel referent as its referents.
    (is (check (instantiate-bound-key
                store false
                (bind [(parallel-referent
                        [(elements-referent '(nil "age"))]
                        [(parallel-referent
                          []
                          [(query-referent '(nil (nil "age")))])])])
                nil)
               (as-set [joe-age joe-bogus-age jane-age])))
    ))

(deftest key->items-test
  (is (= (key->items store [joe-id]) [joe]))
  (is (= (key->items store [joe-id jane-id]) [joe]))
  (is (check (key->items store [[:parallel [] [joe-id jane-id]]])
             (as-set [joe jane])))
  (is (check (key->items
              store
              [[:parallel [(item-referent joe-age)] [joe-id jane-id]]])
             (as-set [joe-age jane-age])))
  (is (check (key->items
              store
              [[:parallel
                [[:parallel
                  []
                  [(item-referent joe-male) (item-referent joe-age)]]]
                [joe-id jane-id]]])
             (as-set [joe-male joe-age jane-age]))))

(deftest key->item-groups-test
  (is (= (key->item-groups store [joe-id]) [[joe]]))
  (is (= (key->item-groups store [joe-id jane-id]) [[joe]]))
  (is (check (key->item-groups store [[:parallel [] [joe-id jane-id]]])
             [(as-set [joe jane])]))
  (is (check (key->item-groups store
                     [[:parallel [(item-referent joe-age)] [joe-id jane-id]]])
             (as-set [[joe-age] [jane-age]])))
  (is (check (key->item-groups store
                     [[:parallel
                       [[:parallel
                         []
                         [(item-referent joe-male) (item-referent joe-age)]]]
                       [joe-id jane-id]]])
             (as-set [(as-set [joe-male joe-age]) [jane-age]]))))

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
                     [debug :refer [let-mutated]]
                     [test-utils :refer [check any as-set]])
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
        pp (parallel-referent [p a] [b])]
    (is (= (prepend-to-key a [b]) [a b]))
    (is (= (prepend-to-key c [p a])
           [(parallel-referent [c] [b]) a]))
    (is (= (prepend-to-key c [pp a])
           [(parallel-referent [(parallel-referent [c] [b]) a]
                               [b])
            a]))))

(deftest remove-first-primitive-referent-test
  (is (= (remove-first-primitive-referent [3 4])
         [4]))
  (is (= (remove-first-primitive-referent [[:parallel [] [2 3]] 4])
         [4]))
  (is (= (remove-first-primitive-referent [[:parallel [0 1] [2 3]] 4])
          [[:parallel [1] [2 3]] 4])))

(deftest remove-content-location-referent-test
  (is (= (remove-content-location-referent [])
         []))
  (is (= (remove-content-location-referent [(content-location-referent) 3 4])
         [3 4]))
  (is (= (remove-content-location-referent
          [[:parallel [(content-location-referent) 1] [2 3]] 4])
         [[:parallel [1] [2 3]] 4]))
  (is (= (remove-content-location-referent [[:parallel [0 1] [2 3]] 4])
         [[:parallel [0 1] [2 3]] 4])))

(deftest remove-comments-test
  (is (= (remove-comments
          [[:parallel
            [(content-location-referent)
             (comment-referent :foo)
             1]
            [(key-referent [1 (comment-referent :bar)]) 3]]
           (comment-referent :baz)])
         [[:parallel
            [1]
            [(key-referent [1]) 3]]])))

(deftest item-ids-referred-to-test
  (is (= (set (item-ids-referred-to [[:parallel [0 1] [2 3]] 4]))
         #{0 1 4})))

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

(deftest instantiate-exemplar-test
  (let [make-instantiator
        (fn [item] #(instantiate-exemplar-item-id store % item))]
    (is (= (instantiate-exemplar store false
                                 [(item-referent joe-male)]
                                 joe (make-instantiator joe))
           [joe-male]))
    (is (= (instantiate-exemplar store false
                                 [(comment-referent :c)
                                  (item-referent joe-male)
                                  (comment-referent :d)]
                                 joe (make-instantiator joe))
           [joe-male]))
    (is (= (instantiate-exemplar store false
                                 [(content-referent)
                                  (item-referent joe-male)]
                                 joe (make-instantiator joe))
           ["male"]))
    (is (empty? (instantiate-exemplar store false
                                      [(item-referent joe-male)]
                                      jane (make-instantiator jane))))
    (is (= (instantiate-exemplar store false [(item-referent jane-age)]
                                 jane (make-instantiator jane))
           [jane-age]))
    (is (= (instantiate-exemplar store false [(item-referent jane-age)]
                                 joe (make-instantiator joe))
           [joe-age]))
    (is (= (instantiate-exemplar store false
                                 [(item-referent joe-age-tag)
                                              (item-referent joe-age)]
                                 jane (make-instantiator jane))
           [jane-age-tag]))
    (is (= (instantiate-exemplar store false
                                 [(key-referent [joe-age-tag joe-age])]
                                 jane (make-instantiator jane))
           [jane-age-tag]))
    (is (= (instantiate-exemplar store false
                                 [(parallel-referent [] [joe-male joe-age])]
                                 joe (make-instantiator joe))
           [joe-male joe-age]))
    (is (= (instantiate-exemplar
            store false
            [(parallel-referent [joe-age-tag] [joe-male joe-age])]
            joe (make-instantiator joe))
           [joe-age-tag]))
    ;; Try key referents in a parallel
    (is (= (instantiate-exemplar
            store false
            [(parallel-referent
              []
              [(key-referent [joe-age-tag joe-age])])]
            joe (make-instantiator joe))
           [joe-age-tag]))
    ;; Try nested parallel.
    (is (= (instantiate-exemplar
            store false
            [(parallel-referent
              [(parallel-referent [] [joe-age-tag])]
              [joe-male joe-age])]
            joe (make-instantiator joe))
           [joe-age-tag]))
    ;; Now try the same things, but grouped.
    (is (= (instantiate-exemplar
            store true [(item-referent joe-male)] joe (make-instantiator joe))
           [[joe-male]]))
    (is (empty? (instantiate-exemplar
                 store true [(item-referent joe-male)]
                 jane (make-instantiator jane))))
    (is (= (instantiate-exemplar
            store true [(item-referent jane-age)] jane (make-instantiator jane))
           [[jane-age]]))
    (is (= (instantiate-exemplar
            store true [(item-referent jane-age)] joe (make-instantiator joe))
           [[joe-age]]))
    (is (= (instantiate-exemplar
            store true
            [(item-referent joe-age-tag) (item-referent joe-age)]
            jane (make-instantiator jane))
           [[jane-age-tag]]))
    (is (= (instantiate-exemplar
            store true
            [[:parallel [] [(item-referent joe-male)
                            (item-referent joe-age)]]]
            joe (make-instantiator joe))
           [[joe-male joe-age]]))
    (is (= (instantiate-exemplar
            store true
            [[:parallel
              [(item-referent joe-age-tag)]
              [(item-referent joe-male) (item-referent joe-age)]]]
            joe (make-instantiator joe))
           [[joe-age-tag]]))
    (is (= (instantiate-exemplar
            store true
            [[:parallel
              [(parallel-referent [] [joe-age-tag])]
              [(item-referent joe-male) (item-referent joe-age)]]]
            joe (make-instantiator joe))
           [[joe-age-tag]]))
    ;; Try a query referent
    (is (check (instantiate-exemplar
                store false
                [(query-referent '(nil (nil "age")))]
                nil #(description->entity % store))
               (as-set [joe jane])))
    ;; With an item as the condition
    (is (check (instantiate-exemplar
                store false
                [(query-referent joe-age)]
                nil #(description->entity % store))
               (as-set [joe-age jane-age])))
    ;; Try an element referent
    (is (check (instantiate-exemplar
                store false
                [(elements-referent '(nil "age")) (item-referent joe)]
                nil #(description->entity % store))
               (as-set [joe-age joe-bogus-age])))
    ;; With an item as the condition
    (is (= (instantiate-exemplar
                store false
                [(elements-referent jane-age) (item-referent joe)]
                nil #(description->entity % store))
           [joe-age]))
    ;; Try a parallel referent with an elements referent as its referents
    (is (check (instantiate-exemplar
                store true
                [(parallel-referent
                  []
                  [(elements-referent '(nil "age"))])
                 (item-referent joe)]
                nil #(description->entity % store))
               [(as-set [joe-age joe-bogus-age])]))
    ;; Try a parallel referent with another parallel referent as its referents.
    (is (check (instantiate-exemplar
                store false
                [(parallel-referent
                  [(elements-referent '(nil "age"))]
                  [(parallel-referent
                    []
                    [(query-referent '(nil (nil "age")))])])]
                nil #(description->entity % store))
               (as-set [joe-age joe-bogus-age jane-age])))
    ))

(deftest key->items-test
  (is (= (key->items store [joe-id]) [joe]))
  (is (= (key->items store [joe-id jane-id]) [joe]))
  (is (= (key->items store [[:parallel [] [joe-id jane-id]]])
         [joe jane]))
  (is (= (key->items store
                     [[:parallel [(item-referent joe-age)] [joe-id jane-id]]])
         [joe-age jane-age]))
  (is (= (key->items store
                     [[:parallel
                       [[:parallel
                         []
                         [(item-referent joe-male) (item-referent joe-age)]]]
                       [joe-id jane-id]]])
         [joe-male joe-age jane-age])))

(deftest key->item-groups-test
  (is (= (key->item-groups store [joe-id]) [[joe]]))
  (is (= (key->item-groups store [joe-id jane-id]) [[joe]]))
  (is (= (key->item-groups store [[:parallel [] [joe-id jane-id]]])
         [[joe jane]]))
  (is (= (key->item-groups store
                     [[:parallel [(item-referent joe-age)] [joe-id jane-id]]])
         [[joe-age] [jane-age]]))
  (is (= (key->item-groups store
                     [[:parallel
                       [[:parallel
                         []
                         [(item-referent joe-male) (item-referent joe-age)]]]
                       [joe-id jane-id]]])
         [[joe-male joe-age] [jane-age]])))

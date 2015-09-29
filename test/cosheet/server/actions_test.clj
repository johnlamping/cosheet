(ns cosheet.server.actions-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [utils :refer [dissoc-in]]
             [orderable :as orderable]
             [entity :as entity :refer [description->entity to-list
                                        content elements label->elements]]
             [computation-manager :refer [new-management compute]]
             [debug :refer [current-value]]
             entity-impl
             [store :refer [new-element-store
                            id->content id->element-ids id-label->element-ids
                            new-mutable-store current-store]]
             [store-impl :refer [->ItemId]]
             [store-utils :refer [add-entity]]
             mutable-store-impl)
            (cosheet.server
             [render :refer [item-DOM canonicalize-list item-referent]]
             [dom-tracker :refer [new-dom-tracker add-dom  dom->subcomponents]]
             [actions :refer :all])
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
(def joe `("Joe"
           (~o2 :order)
           ("male" (~o1 :order))
           (39 (~o3 :order)
               ("age" ~'tag)
               ("doubtful" "confidence"))
           ("married" (~o2 :order))
           (45 (~o4 :order)
               ("age" ~'tag))))
(def jane `("Jane" (~o1 :order)
           ("female" (~o2 :order))
           (45 (~o3 :order)
               ("age" ~'tag))))
(def t1 (add-entity (new-element-store) nil joe))
(def joe-id (second t1))
(def t2 (add-entity (first t1) nil jane))
(def store (first t2))
(def jane-id (second t2))
(def joe-entity (description->entity joe-id store))
(def joe-age (first (filter #(= (content %) 45) (elements joe-entity))))
(def joe-age-tag (first (elements joe-age)))
(def joe-male (first (filter #(= (content %) "male") (elements joe-entity))))
(def jane-entity (description->entity jane-id store))
(def jane-age (first (label->elements jane-entity "age")))
(def jane-age-tag (first (elements jane-age)))

(defn new-joe-jane-tracker [mutable-store]
  (let [management (new-management)
        tracker (new-dom-tracker management)]
    (add-dom tracker
             "joe-root"
             [joe-id :bob]
             [item-DOM
              (description->entity joe-id mutable-store)
              [joe-id :bob] #{} {:depth 1}])
    (add-dom tracker
             "jane-root"
             [jane-id :bob]
             [item-DOM
              (description->entity jane-id mutable-store)
              [jane-id :bob] #{} {:depth 1}])
    (compute management)
    tracker))

(deftest remove-content-referent-test
  (is (= (remove-content-referent [[:content] 3 4])
         [3 4]))
  (is (= (remove-content-referent [[:parallel [[:content] 1] [2 3]] 4])
         [[:parallel [1] [2 3]] 4]))
  (is (= (remove-content-referent [[:parallel [0 1] [2 3]] 4])
          [[:parallel [0 1] [2 3]] 4])))

(deftest item-determining-referents-test
  (let [id (->ItemId "a")]
    (is (= (item-determining-referents
            [[:parallel :a :b] [:content] [:group "a"] [:condition :b] id])
           [[:parallel :a :b] id]))))

(deftest canonical-visible-test
  (let [expected ["Joe" {"male" 1
                         "married" 1
                         [39 {["age" {'tag 1}] 1
                              ["doubtful" {"confidence" 1}] 1}] 1
                              [45 {["age" {'tag 1}] 1}] 1}]]
    (is (= (item->canonical-visible joe-entity) expected))))

(deftest instantiate-exemplar-test
  (let [make-instantiator (fn [item] #(instantiate-item-id store % item))]
    (is (= (instantiate-exemplar store false
                                 [(:item-id joe-male)] (make-instantiator joe-entity))
           [joe-male]))
    (is (= (instantiate-exemplar store false
                                 [(:item-id joe-male)] (make-instantiator jane-entity))
           []))
    (is (= (instantiate-exemplar store false [(:item-id jane-age)]
                                 (make-instantiator jane-entity))
           [jane-age]))
    (is (= (instantiate-exemplar store false [(:item-id jane-age)]
                                 (make-instantiator joe-entity))
           [joe-age]))
    (is (= (instantiate-exemplar store false
                                 [(:item-id joe-age-tag)
                                              (:item-id joe-age)]
                                 (make-instantiator jane-entity))
           [jane-age-tag]))
    (is (= (instantiate-exemplar store false
                                 [[:parallel [] [(:item-id joe-male)
                                                 (:item-id joe-age)]]]
                                 (make-instantiator joe-entity))
           [joe-male joe-age]))
    (is (= (instantiate-exemplar store false
                                 [[:parallel
                                   [(:item-id joe-age-tag)]
                                   [(:item-id joe-male) (:item-id joe-age)]]]
                                 (make-instantiator joe-entity))
           [joe-age-tag]))
    (is (= (instantiate-exemplar store false
                                 [[:parallel
                                   [[:parallel [] [(:item-id joe-age-tag)]]]
                                   [(:item-id joe-male) (:item-id joe-age)]]]
                                 (make-instantiator joe-entity))
           [joe-age-tag]))
    (is (= (instantiate-exemplar
            store true [(:item-id joe-male)] (make-instantiator joe-entity))
           [[joe-male]]))
    (is (= (instantiate-exemplar
            store true [(:item-id joe-male)] (make-instantiator jane-entity))
           []))
    (is (= (instantiate-exemplar
            store true [(:item-id jane-age)] (make-instantiator jane-entity))
           [[jane-age]]))
    (is (= (instantiate-exemplar
            store true [(:item-id jane-age)] (make-instantiator joe-entity))
           [[joe-age]]))
    (is (= (instantiate-exemplar
            store true
            [(:item-id joe-age-tag) (:item-id joe-age)]
            (make-instantiator jane-entity))
           [[jane-age-tag]]))
    (is (= (instantiate-exemplar
            store true
            [[:parallel [] [(:item-id joe-male)
                            (:item-id joe-age)]]]
            (make-instantiator joe-entity))
           [[joe-male joe-age]]))
    (is (= (instantiate-exemplar
            store true
            [[:parallel
              [(:item-id joe-age-tag)]
              [(:item-id joe-male) (:item-id joe-age)]]]
            (make-instantiator joe-entity))
           [[joe-age-tag]]))
    (is (= (instantiate-exemplar
            store true
            [[:parallel
              [[:parallel [] [(:item-id joe-age-tag)]]]
              [(:item-id joe-male) (:item-id joe-age)]]]
            (make-instantiator joe-entity))
           [[joe-age-tag]]))))

(deftest key->items-test
  (is (= (key->items store [joe-id]) [joe-entity]))
  (is (= (key->items store [joe-id jane-id]) [joe-entity]))
  (is (= (key->items store [[:parallel [] [joe-id jane-id]]])
         [joe-entity jane-entity]))
  (is (= (key->items store
                     [[:parallel [(:item-id joe-age)] [joe-id jane-id]]])
         [joe-age jane-age]))
  (is (= (key->items store
                     [[:parallel
                       [[:parallel
                         []
                         [(:item-id joe-male) (:item-id joe-age)]]]
                       [joe-id jane-id]]])
         [joe-male joe-age jane-age])))

(deftest instantiate-exemplar-to-groups-test
  (let [make-instantiator (fn [item] #(instantiate-item-id store % item))]
    ))

(deftest key->item-groups-test
  (is (= (key->item-groups store [joe-id]) [[joe-entity]]))
  (is (= (key->item-groups store [joe-id jane-id]) [[joe-entity]]))
  (is (= (key->item-groups store [[:parallel [] [joe-id jane-id]]])
         [[joe-entity jane-entity]]))
  (is (= (key->item-groups store
                     [[:parallel [(:item-id joe-age)] [joe-id jane-id]]])
         [[joe-age] [jane-age]]))
  (is (= (key->item-groups store
                     [[:parallel
                       [[:parallel
                         []
                         [(:item-id joe-male) (:item-id joe-age)]]]
                       [joe-id jane-id]]])
         [[joe-male joe-age] [jane-age]])))

(deftest parse-string-test
  (is (= (parse-string "x") "x"))
  (is (= (parse-string "1") 1))
  (is (= (parse-string " 1 ") 1))
  (is (= (parse-string "1 1") "1 1"))
  (is (= (parse-string " 1.0 ") 1))
  (is (= (parse-string "-1.0") -1))
  (is (= (parse-string " 1.5 ") 1.5)))

(deftest update-add-entity-with-order-test
  (let [[s order] (update-add-entity-with-order
                   store joe-id 6
                   unused-orderable :before true)
        joe-entity (description->entity joe-id s)
        new-entity (first (filter #(= (content %) 6) (elements joe-entity)))
        [o5 o6] (orderable/split unused-orderable :before)]
    (is (= (to-list new-entity)
           `(6 (~o5 :order))))
    (is (= order o6)))
  (let [[s order] (update-add-entity-with-order
                   store joe-id 6
                   unused-orderable :before false)
        joe-entity (description->entity joe-id s)
        new-entity (first (filter #(= (content %) 6)
                                  (elements joe-entity)))
        [o5 o6] (orderable/split unused-orderable :after)]
    (is (= (to-list new-entity)
           `(6 (~o5 :order))))
    (is (= order o6)))    
  (let [[s order] (update-add-entity-with-order
                   store joe-id 6
                   unused-orderable :after true)
        joe-entity (description->entity joe-id s)
        new-entity (first (filter #(= (content %) 6)
                                  (elements joe-entity)))
        [o5 o6] (orderable/split unused-orderable :after)]
    (is (= (to-list new-entity)
           `(6 (~o6 :order))))
    (is (= order o5)))
  (let [[s order] (update-add-entity-with-order
                   store joe-id '(6 ("height" tag))
                   unused-orderable :before true)
        joe-entity (description->entity joe-id s)
        new-entity (first (label->elements joe-entity "height"))
        [x o5] (orderable/split unused-orderable :before)
        [o6 o7] (orderable/split x :after)]
    (is (= (canonicalize-list (to-list new-entity))
           (canonicalize-list `(6 (~o7 :order)
                                  ("height" ~'tag (~o6 :order))))))
    (is (= order o5)))
  ;; Check that order in the list style entity is preserved in the
  ;; :order values.
  (let [[s order] (update-add-entity-with-order
                   store joe-id '(6 ("height" tag) ("weight" tag))
                   unused-orderable :after false)
        joe-entity (description->entity joe-id s)
        new-entity (first (label->elements joe-entity "height"))
        [x o5] (orderable/split unused-orderable :before)
        [x o6] (orderable/split x :before)
        [o8 o7] (orderable/split x :before)]
    (is (= (canonicalize-list (to-list new-entity))
           (canonicalize-list `(6 (~o5 :order)
                                  ("height" ~'tag (~o7 :order))
                                  ("weight" ~'tag (~o6 :order))))))
    (is (= order o8))))

(deftest update-add-element-test
  (let [order-entity (first (label->elements jane-entity :order))
        order (content order-entity)
        s (update-add-element '("foo" 6) store jane-entity)
        new-jane-entity (description->entity jane-id s)
        new-element (first (filter #(= (content %) "foo")
                                   (elements new-jane-entity)))
        [x o5] (orderable/split order :before)
        [o6 o7] (orderable/split x :before)]
    (is (= (canonicalize-list (to-list new-element))
           (canonicalize-list `("foo" (~o5 :order) (6 (~o7 :order))))))
    (is (= (id->content s (:item-id order-entity)) o6)))
  ;; Test the case where the subject doesn't have its own order information.
  (let [[store sally-id] (add-entity (new-element-store) nil "Sally")
        [store unused-id] (add-entity store nil o4)
        [store _] (add-entity store unused-id :unused-orderable)
        sally-entity (description->entity sally-id store)
        order-entity (description->entity unused-id store)
        s (update-add-element '("bar" 6) store sally-entity)
        new-sally-entity (description->entity sally-id s)
        new-element (first (filter #(= (content %) "bar")
                                   (elements new-sally-entity)))
        [x o5] (orderable/split o4 :before)
        [o6 o7] (orderable/split x :before)]
    (is (= (canonicalize-list (to-list new-element))
           (canonicalize-list `("bar" (~o5 :order) (6 (~o7 :order))))))
    (is (= (id->content s (:item-id order-entity)) o6))))

(deftest add-element-handler-test
  (let [joe-age-id (:item-id joe-age)
        mutable-store (new-mutable-store store)
        tracker (new-joe-jane-tracker mutable-store)]
    (let [joe-age-dom-id (first (filter
                                 #(= (first (get-in @tracker [:id->key %]))
                                     joe-age-id)
                                 (keys (:id->key @tracker))))
          new-store (add-element-handler store tracker joe-age-dom-id)
          new-joe-age (description->entity joe-age-id new-store)]
      (is (= (item->canonical-visible new-joe-age)
             [45 {["age" {'tag 1}] 1, "" 1}])) )))

(deftest set-content-handler-test
  (let [mutable-store (new-mutable-store store)
        mutable-joe (description->entity joe-id mutable-store)
        tracker (new-joe-jane-tracker mutable-store)]
    (is (= (id->content
            (set-content-handler store tracker "joe-root" "Joe" "Jim")
            joe-id)
           "Jim"))
    (is (= (id->content
            (set-content-handler store tracker "joe-root" "Wrong" "Jim")
            joe-id)
           "Joe"))
    (swap! tracker #(-> %
                        (assoc-in [:id->key "test"]
                                  [[:parallel
                                    [(:item-id joe-age-tag) (:item-id joe-age)]
                                    [joe-id jane-id]]])
                        (assoc-in [:id->key "test2"]
                                  [[:parallel
                                    [[:content]
                                     (:item-id joe-age-tag) (:item-id joe-age)]
                                    [joe-id jane-id]]])))
    ;; TODO: Add a test for when an entity is added.
    (let [modified (set-content-handler store tracker "test" "age" "oldness")]
      (is (= (id->content modified (:item-id joe-age-tag)) "oldness"))
      (is (= (id->content modified (:item-id jane-age-tag)) "oldness")))
    (do-actions mutable-store tracker
                {1 [:set-content "joe-root" "Joe" "Fred"]})
    (is (= (current-value (content mutable-joe)) "Fred"))))

(deftest update-add-sibling-test
  (let [jane-dom (item-DOM jane-entity [(item-referent jane-entity)]
                           #{} {:depth 1})
        order-entity (first (label->elements jane-age :order))
        order (content order-entity)
        age-dom (first (filter #(= (first (:key (second %)))
                                   (item-referent jane-age))
                               (dom->subcomponents jane-dom)))
        s (update-add-sibling (:sibling-elements (second age-dom)) :after
                              store jane-age)
        new-jane-entity (description->entity jane-id s)
        new-element (first (filter #(= (content %) "")
                                   (elements new-jane-entity)))
        [o5 x] (orderable/split order :after)
        [o6 o7] (orderable/split x :before)]
    (is (= (canonicalize-list (to-list new-element))
           (canonicalize-list `("" (~o6 :order) ("age" ~'tag (~o7 :order))))))
    (is (= (id->content s (:item-id order-entity)) o5))))

(deftest add-sibling-handler-test
  (let [mutable-store (new-mutable-store store)
        tracker (new-joe-jane-tracker mutable-store)]
    (let [joe-age-dom-id (first (filter
                                 #(= (first (get-in @tracker [:id->key %]))
                                     (:item-id joe-age))
                                 (keys (:id->key @tracker))))
          new-store (add-sibling-handler store
                                         tracker joe-age-dom-id :after)]
      (let [joe-age-ids (id-label->element-ids new-store joe-id "age")
            joe-ages (map #(id->content new-store %) joe-age-ids)]
          (is (= (set joe-ages) #{"" 39 45}))))))

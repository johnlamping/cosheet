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
             [store :refer [new-element-store id->content id-label->element-ids
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
  (let [[store joe-id] (add-entity (new-element-store) nil joe)
        joe (description->entity joe-id store)]
    (let [expected ["Joe" {"male" 1
                           "married" 1
                           [39 {["age" {'tag 1}] 1
                                ["doubtful" {"confidence" 1}] 1}] 1
                           [45 {["age" {'tag 1}] 1}] 1}]]
      (is (= (item->canonical-visible joe) expected)))))

(deftest instantiate-exemplar-test
  (let [[store0 joe-id] (add-entity (new-element-store) nil joe)
        [store jane-id] (add-entity store0 nil jane)
        joe (description->entity joe-id store)
        joe-age (first (filter #(= (content %) 45) (elements joe)))
        joe-age-tag (first (elements joe-age))
        joe-male (first (filter #(= (content %) "male") (elements joe)))
        jane (description->entity jane-id store)
        jane-age (first (label->elements jane "age"))
        jane-age-tag (first (elements jane-age))]
    (is (= (instantiate-exemplar store [] joe) [joe]))
    (is (= (instantiate-exemplar store [(:item-id joe-male)] joe)
           [joe-male]))
    (is (= (instantiate-exemplar store [(:item-id joe-male)] jane)
           []))
    (is (= (instantiate-exemplar store [(:item-id jane-age)] jane)
           [jane-age]))
    (is (= (instantiate-exemplar store [(:item-id jane-age)] joe)
           [joe-age]))
    (is (= (instantiate-exemplar store
                                 [(:item-id joe-age-tag) (:item-id joe-age)]
                                 jane)
           [jane-age-tag]))
    (is (= (instantiate-exemplar store
                                 [[:parallel [] [(:item-id joe-male)
                                                 (:item-id joe-age)]]]
                                 joe)
           [joe-male joe-age]))
    (is (= (instantiate-exemplar store
                                 [[:parallel
                                   [(:item-id joe-age-tag)]
                                   [(:item-id joe-male) (:item-id joe-age)]]]
                                 joe)
           [joe-age-tag]))
    (is (= (instantiate-exemplar store
                                 [[:parallel
                                   [[:parallel [] [(:item-id joe-age-tag)]]]
                                   [(:item-id joe-male) (:item-id joe-age)]]]
                                 joe)
           [joe-age-tag]))))

(deftest key->items-test
  (let [[store0 joe-id] (add-entity (new-element-store) nil joe)
        [store jane-id] (add-entity store0 nil jane)
        joe (description->entity joe-id store)
        joe-age (first (filter #(= (content %) 45) (elements joe)))
        joe-male (first (filter #(= (content %) "male") (elements joe)))
        jane (description->entity jane-id store)
        jane-age (first (label->elements jane "age"))]
    (is (= (key->items store [joe-id]) [joe]))
    (is (= (key->items store [joe-id jane-id]) [joe]))
    (is (= (key->items store [[:parallel [] [joe-id jane-id]]]) [joe jane]))
    (is (= (key->items store
                       [[:parallel [(:item-id joe-age)] [joe-id jane-id]]])
           [joe-age jane-age]))
    (is (= (key->items store
                       [[:parallel
                         [[:parallel
                           []
                           [(:item-id joe-male) (:item-id joe-age)]]]
                         [joe-id jane-id]]])
           [joe-male joe-age jane-age]))))

(deftest parse-string-test
  (is (= (parse-string "x") "x"))
  (is (= (parse-string "1") 1))
  (is (= (parse-string " 1 ") 1))
  (is (= (parse-string "1 1") "1 1"))
  (is (= (parse-string " 1.0 ") 1))
  (is (= (parse-string "-1.0") -1))
  (is (= (parse-string " 1.5 ") 1.5)))

(deftest set-content-handler-test
  (let [[store0 joe-id] (add-entity (new-element-store) nil joe)
        [store jane-id] (add-entity store0 nil jane)
        mutable-store (new-mutable-store store)
        joe (description->entity joe-id mutable-store)
        joe-age (first (filter #(= (current-value (content %)) 45)
                               (current-value (elements joe))))
        joe-age-tag (first (current-value (elements joe-age)))
        jane (description->entity jane-id mutable-store)
        jane-age (first (current-value (label->elements jane "age")))
        jane-age-tag (first (current-value (elements jane-age)))
        management (new-management)
        tracker (new-dom-tracker management)]
    (add-dom tracker
             "joe-root"
             [joe-id :bob]
             [item-DOM joe [joe-id :bob] #{} {:depth 1}])
    (add-dom tracker
             "jane-root"
             [jane-id :bob]
             [item-DOM jane [jane-id :bob] #{} {:depth 1}])
    (compute management)
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
    (let [modified (set-content-handler store tracker "test" "age" "oldness")]
      (is (= (id->content modified (:item-id joe-age-tag)) "oldness"))
      (is (= (id->content modified (:item-id jane-age-tag)) "oldness")))
    (do-actions mutable-store tracker
                {1 [:set-content "joe-root" "Joe" "Fred"]})
    (is (= (current-value (content joe)) "Fred"))))

(deftest update-add-entity-with-order-test
  (let [[store joe-id] (add-entity (new-element-store) nil joe)]
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
      (is (= order o8)))))

(deftest update-add-element-test
  (let [[store jane-id] (add-entity (new-element-store) nil jane)
        jane-entity (description->entity jane-id store)
        order-entity (first (label->elements jane-entity :order))
        order (content order-entity)
        s (update-add-element [:condition 6] "foo" store jane-entity)
        jane-entity (description->entity jane-id s)
        new-element (first (filter #(= (content %) "foo")
                                   (elements jane-entity)))
        [x o5] (orderable/split order :before)
        [o6 o7] (orderable/split x :before)]
    (is (= (canonicalize-list (to-list new-element))
           (canonicalize-list `("foo" (~o5 :order) (6 (~o7 :order))))))
    (is (= (id->content s (:item-id order-entity)) o6)))
  (let [[store sally-id] (add-entity (new-element-store) nil "Sally")
        [store unused-id] (add-entity store nil o4)
        [store _] (add-entity store unused-id :unused-orderable)
        sally-entity (description->entity sally-id store)
        order-entity (description->entity unused-id store)
        s (update-add-element [:condition 6] "bar" store sally-entity)
        sally-entity (description->entity sally-id s)
        new-element (first (filter #(= (content %) "bar")
                                   (elements sally-entity)))
        [x o5] (orderable/split o4 :before)
        [o6 o7] (orderable/split x :before)]
    (is (= (canonicalize-list (to-list new-element))
           (canonicalize-list `("bar" (~o5 :order) (6 (~o7 :order))))))
    (is (= (id->content s (:item-id order-entity)) o6))))

(deftest update-add-sibling-test
  (let [[store jane-id] (add-entity (new-element-store) nil jane)
        jane-entity (description->entity jane-id store)
        jane-dom (item-DOM jane-entity [(item-referent jane-entity)]
                           #{} {:depth 1})
        age-element (first (filter #(= (content %) 45)
                                   (elements jane-entity)))
        order-entity (first (label->elements age-element :order))
        order (content order-entity)
        age-dom (first (filter #(= (first (:key (second %)))
                                   (item-referent age-element))
                               (dom->subcomponents jane-dom)))
        s (update-add-sibling (:sibling-elements (second age-dom)) :after
                              store age-element)
        jane-entity (description->entity jane-id s)
        new-element (first (filter #(= (content %) "")
                                   (elements jane-entity)))
        [o5 x] (orderable/split order :after)
        [o6 o7] (orderable/split x :before)]
    (is (= (canonicalize-list (to-list new-element))
           (canonicalize-list `("" (~o6 :order) ("age" ~'tag (~o7 :order))))))
    (is (= (id->content s (:item-id order-entity)) o5))))

(deftest add-sibling-handler-test
  (let [[store0 joe-id] (add-entity (new-element-store) nil joe)
        [store jane-id] (add-entity store0 nil jane)
        mutable-store (new-mutable-store store)
        joe (description->entity joe-id mutable-store)
        joe-age (first (filter #(= (current-value (content %)) 45)
                               (current-value (elements joe))))
        joe-age-tag (first (current-value (elements joe-age)))
        jane (description->entity jane-id mutable-store)
        management (new-management)
        tracker (new-dom-tracker management)]
    (add-dom tracker
             "joe-root"
             [joe-id :bob]
             [item-DOM joe [joe-id :bob] #{} {:depth 1}])
    (add-dom tracker
             "jane-root"
             [jane-id :bob]
             [item-DOM jane [jane-id :bob] #{} {:depth 1}])
    (compute management)
    (let [joe-age-dom-id (first (filter
                                 #(= (first (get-in @tracker [:id->key %]))
                                     (:item-id joe-age))
                                 (keys (:id->key @tracker))))
          new-store (add-sibling-handler (current-store mutable-store)
                                         tracker joe-age-dom-id :after)]
      (let [joe-age-ids  (id-label->element-ids new-store joe-id "age")
            joe-ages (map #(id->content new-store %) joe-age-ids)]
          (is (= (set joe-ages) #{"" 39 45}))))))

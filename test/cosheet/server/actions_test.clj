(ns cosheet.server.actions-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [utils :refer [dissoc-in]]
             [orderable :as orderable]
             [entity :as entity :refer [description->entity to-list
                                        content elements
                                        label->elements label->content]]
             [expression-manager :refer [new-expression-manager-data compute]]
             [debug :refer [profile-and-print-reporters
                            simplify-for-print]]
             [reporters :as reporter]
             entity-impl
             [query :refer [matching-elements]]
             [store :refer [new-element-store
                            new-mutable-store current-store]]
             [store-impl :refer [->ItemId]]
             [store-utils :refer [add-entity]]
             mutable-store-impl
             [canonical :refer [canonicalize-list]]
             [test-utils :refer [check any]])
            (cosheet.server
             [referent :refer [item-referent union-referent
                               immutable-semantic-to-list]]
             [referent :refer [item->canonical-semantic referent->string]]
             [dom-tracker :refer [new-dom-tracker update-set-component key->id]]
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
(def joe-list `("Joe"
                (~o2 :order :non-semantic)
                ("male" (~o1 :order :non-semantic))
                (39 (~o3 :order :non-semantic)
                    ("age" :tag)
                    ("doubtful" "confidence"))
                ("married" (~o2 :order :non-semantic))
                (45 (~o4 :order :non-semantic)
                    ("age" :tag))))
(def jane-list `("Jane" (~o1 :order :non-semantic)
                 ("female" (~o2 :order :non-semantic))
                 (45 (~o3 :order :non-semantic)
                     ("age" :tag))))
(def t1 (add-entity (new-element-store) nil joe-list))
(def joe-id (second t1))
(def t2 (add-entity (first t1) nil jane-list))
(def store (first t2))
(def jane-id (second t2))
(def joe (description->entity joe-id store))
(def joe-age (first (matching-elements 45 joe)))
(def joe-bogus-age (first (matching-elements 39 joe)))
(def joe-age-tag (first (matching-elements "age" joe-age)))
(def joe-male (first (matching-elements "male" joe)))
(def joe-married (first (matching-elements "married" joe)))
(def jane (description->entity jane-id store))
(def jane-female (first (matching-elements "female" jane)))
(def jane-age (first (matching-elements 45 jane)))
(def jane-age-tag (first (matching-elements "age" jane-age)))

(deftest substitute-in-key-test
  (let [key [[:pattern]
             [:pattern '(nil (:variable (:v :name)
                                        ((nil "age" "doubtful") :condition)
                                        (:true :reference)))]
             [:pattern '(nil (:variable (:v :name)
                                        ("male" :condition)
                                        (:true :reference)))]
             :foo "bar"]]
    (is (check  (substitute-in-key key joe)
                [(:item-id joe)
                 (:item-id joe-bogus-age)
                 (:item-id joe-male)
                 :foo "bar"]))))

(deftest update-add-entity-with-order-test
  (let [[s id order] (update-add-entity-with-order
                      store joe-id 6
                      unused-orderable :before true)
        joe (description->entity joe-id s)
        new-entity (first (matching-elements 6 joe))
        [o5 o6] (orderable/split unused-orderable :before)]
    (is (= (to-list new-entity)
           `(6 (~o5 :order :non-semantic))))
    (is (= order o6))
    (is (= (:item-id new-entity) id)))
  (let [[s id order] (update-add-entity-with-order
                      store joe-id 6
                      unused-orderable :before false)
        joe (description->entity joe-id s)
        new-entity (first (matching-elements 6 joe))
        [o5 o6] (orderable/split unused-orderable :after)]
    (is (= (to-list new-entity)
           `(6 (~o5 :order :non-semantic))))
    (is (= order o6))
    (is (= (:item-id new-entity) id)))    
  (let [[s id order] (update-add-entity-with-order
                      store joe-id 6
                      unused-orderable :after true)
        joe (description->entity joe-id s)
        new-entity (first (matching-elements 6 joe))
        [o5 o6] (orderable/split unused-orderable :after)]
    (is (= (to-list new-entity)
           `(6 (~o6 :order :non-semantic))))
    (is (= order o5))
    (is (= (:item-id new-entity) id)))
  (let [[s id order] (update-add-entity-with-order
                      store joe-id '(6 ("height" :tag))
                      unused-orderable :before true)
        joe (description->entity joe-id s)
        new-entity (first (label->elements joe "height"))
        [x o5] (orderable/split unused-orderable :before)
        [o6 o7] (orderable/split x :after)]
    (is (check (canonicalize-list (to-list new-entity))
               (canonicalize-list `(6 (~o7 :order :non-semantic)
                                      ("height" :tag
                                       (~o6 :order :non-semantic))))))
    (is (= order o5))
    (is (= (:item-id new-entity) id)))
  ;; Check that order in the list style entity is preserved in the
  ;; :order values.
  ;; Also check and that non-semantic elements don't get order information
  (let [[s id order] (update-add-entity-with-order
                      store joe-id '(6 ("height" :tag)
                                       ("" :tag)
                                       ("other" :non-semantic ""))
                      unused-orderable :after false)
        joe (description->entity joe-id s)
        new-entity (first (label->elements joe "height"))
        [x o5] (orderable/split unused-orderable :before)
        [x o6] (orderable/split x :before)
        [o8 o7] (orderable/split x :before)]
    (is (check (canonicalize-list (to-list new-entity))
               (canonicalize-list
                `(6 (~o5 :order :non-semantic)
                    ("height" :tag (~o7 :order :non-semantic))
                    ("" :tag (~o6 :order :non-semantic))
                    ("other" :non-semantic "")))))
    (is (= order o8))
    (is (= (:item-id new-entity) id))))

(deftest furthest-item-test
  (is (= (furthest-item [joe-married] :before) joe-married))
  (is (= (furthest-item [joe-married joe-male] :before) joe-male))
  (is (= (furthest-item [joe-married joe-male] :after) joe-married)))

(deftest adjust-condition-test
  (let [[c1 s1] (adjust-condition '("x" ('??? :a) ('??? 22))
                                  (new-element-store))
        [c2 s2] (adjust-condition '("x" ('??? "y") ('??? "22"))
                                  s1)]
    (is (= c1  '("x" ('???-0 :a) ('???-1 22))))
    (is (= c2  '("x" ('???-2 "y") ('???-3 "22"))))))

(deftest generic-add-test
  (let [order-entity (first (label->elements jane-age :order))
        order (content order-entity)
        ;; Try with sibling referent.
        result1 (generic-add store
                             {:template '(nil ("new" :tag))
                              :item-referent (item-referent jane-age)}
                             ["parent-key"] ["old-key"] true)
        s1 (:store result1)
        new-jane (description->entity jane-id s1)
        new-element (first (matching-elements '(nil "new") new-jane))
        [o5 x] (orderable/split order :after)
        [o6 o7] (orderable/split x :before)
        ;; Try with adjacent referent.
        result2 (generic-add store
                             {:template '(nil ("new" :tag))
                              :subject-referent (item-referent jane)
                              :adjacent-referent (item-referent jane-age)}
                             ["parent-key"] ["old-key"] true)]    
    (is (check (canonicalize-list (to-list new-element))
               (canonicalize-list `(""
                                    (~o6 :order :non-semantic)
                                    ("new" :tag (~o7 :order :non-semantic))))))
    (is (= (content (description->entity (:item-id order-entity) s1)) o5))
    (is (check (:select result1)
               [["parent-key" (:item-id new-element)]
                [["old-key"]]]))
    (is (check result2 result1))
    ;; Check that adding two separately, is the same as adding in parallel.
    (let [result12 (generic-add s1
                                {:template '(nil ("new" :tag))
                                 :item-referent (item-referent joe-age)}
                                [] [] true)
          result-both (generic-add store
                                   {:template '(nil ("new" :tag))
                                    :item-referent (union-referent
                                                    [(item-referent jane-age)
                                                     (item-referent joe-age)])}
                                   ["parent-key"] ["old-key"] true)]
      (is (check (:store result12) (:store result-both)))
      (is (check (:select result1) (:select result-both))))
    ;; Checking adding with nil-to-none
    (let [result (generic-add store {:template '(nil ("new" :tag nil))
                                     :nil-to-anything true
                                     :item-referent (union-referent
                                                     [(item-referent jane-age)
                                                      (item-referent joe-age)])}
                              ["parent-key"] ["old-key"] true)
          s1 (:store result)
          new-jane (description->entity jane-id s1)
          new-jane-element (first (matching-elements '(nil "new") new-jane))
          new-joe (description->entity joe-id s1)
          new-joe-element (first (matching-elements '(nil "new") new-joe))]
      (is (check (to-list new-jane-element)
                 ['anything
                  ["new" ['anything [(any) :order :non-semantic]]
                   :tag
                   [(any) :order :non-semantic]]
                  [(any) :order :non-semantic]]))
      (println "namespace" (namespace 'anything))
      (is (check (to-list new-joe-element)
                 [""
                  ["new" ["" [(any) :order :non-semantic]]
                   :tag
                   [(any) :order :non-semantic]]
                  [(any) :order :non-semantic]])))))

(deftest do-add-element-test
  (let [result (do-add-element
                store
                {:item-referent (item-referent jane-age)}
                {:target-key ["jane-age"]})
        new-store (:store result)]
    (is (check (item->canonical-semantic
                (to-list (description->entity (:item-id jane-age) new-store)))
               (canonicalize-list '(45 ("age" :tag) ""))))))

(deftest do-delete-test
  (let [new-store (do-delete
                   store
                   {:item-referent (item-referent jane-age)}
                   {:target-key "jane"})]
    (is (check (canonicalize-list
                (to-list (description->entity jane-id new-store)))
               (canonicalize-list `("Jane" (~o1 :order :non-semantic)
                                    ("female" (~o2 :order :non-semantic))))))))

(deftest do-set-content-test
  (is (= (content
          (description->entity
           joe-id (do-set-content store
                                  {:item-referent (item-referent joe)}
                                  {:target-key "joe"
                                   :from "Joe" :to "Jim"})))
         "Jim"))
  (is (= (content
          (description->entity
           joe-id (do-set-content store
                                  {:item-referent (item-referent joe)}
                                  {:target-key "joe"
                                   :from "Wrong" :to "Jim"})))
         "Joe"))
  ;; Now, try calling it when there is a parallel referent.
  (let [modified (do-set-content
                  store
                  {:item-referent (union-referent
                                   [(item-referent joe-age-tag)
                                    (item-referent jane-age-tag)])}
                  {:target-key "both"
                   :from "age" :to "oldness"})]
    (is (= (item->canonical-semantic
            (description->entity (:item-id joe-age-tag) modified))
           (canonicalize-list '("oldness" :tag))))
    (is (= (item->canonical-semantic
            (description->entity (:item-id jane-age-tag) modified))
           (canonicalize-list '("oldness" :tag)))))
  ;; Try creating new content
    (let [result (do-set-content
                  store
                  {:subject-referent (item-referent joe-male)
                   :adjacent-groups-referent (item-referent joe-male)
                   :template '(nil :tag)}
                  {:target-key ["joe-male"]
                   :from ""
                   :to "gender"})]
      (is (= (immutable-semantic-to-list
              (description->entity (:item-id joe-male) (:store result)))
             ["male" ["gender" :tag]]))))

(deftest do-expand-test
  (is (check (do-expand store
                        {:item-referent (item-referent joe)}
                        {:target-key "joe"
                         :session-state {:name "foo"}})
             {:store store
              :open (str "foo?referent="
                         (referent->string (item-referent joe)))})))

(deftest do-actions-test
  (let [mutable-store (new-mutable-store store)
        tracker (new-dom-tracker mutable-store)
        session-state {:tracker tracker
                       :alternate (atom nil)
                       :store mutable-store}
        attributes {:commands {:add-element nil}
                    :target {:item-referent
                             (union-referent [(item-referent jane)
                                              (item-referent joe)])
                             :narrow-alternate
                             {:broad-text ["Please" "Click me!"]
                              :narrow-text ["Click me!" "Please"]}}}]
    (swap! tracker update-set-component
           {:key ["jane"]
            :definition [(fn [& _] [:div])]
            :attributes attributes})
    (let [result (do-actions mutable-store session-state
                             [[:add-element (key->id tracker ["jane"])]])
          new-store (current-store mutable-store)
          select (:select result)
          new-id (last (first select))
          alternate @(:alternate session-state)
          alternate-target (-> (:target attributes)
                               (assoc :item-referent (item-referent jane))
                               (dissoc :narrow-alternate))]
      (is (check alternate
                 {:new-store new-store
                  :action [do-add-element
                           alternate-target
                           (assoc attributes :target-key ["jane"])]
                  :text ["Please" "Click me!"]}))
      (is (= select [["jane" new-id] [["jane"]]]))
      (is (check (item->canonical-semantic
                  (description->entity (:item-id jane) new-store))
                 (canonicalize-list '("Jane" "female" (45 ("age" :tag)) ""))))
      (is (check (item->canonical-semantic
                    (description->entity (:item-id joe) new-store))
                   (canonicalize-list '("Joe"
                                        "male" 
                                        (39 ("age" :tag)
                                            ("doubtful" "confidence"))
                                        "married"
                                        (45 ("age" :tag))
                                        ""))))
      (is (= (immutable-semantic-to-list
              (description->entity new-id new-store))
             ""))
      ;; Check undo redo.
      (do-actions mutable-store {:tracker tracker :alternate (atom nil)}
                  [[:undo]])
      (is (check (current-store mutable-store)
                 (assoc store :modified-ids #{})))
      ;; Check that alternate does nothing if the store isn't what it expects.
      (reset! (:alternate session-state) alternate)
      (do-actions mutable-store session-state [[:alternate]])
      ;; Check redo.
      (do-actions mutable-store {:tracker tracker :alternate (atom nil)}
                  [[:redo]])
      (is (check (current-store mutable-store) new-store))
      ;; Check alternate.
      (reset! (:alternate session-state) alternate)
      (let [result (do-actions mutable-store session-state [[:alternate]])
            alternate-store (current-store mutable-store)]
        (is (check (item->canonical-semantic
                    (description->entity (:item-id jane) alternate-store))
                   (canonicalize-list '("Jane" "female" (45 ("age" :tag)) ""))))
        (is (check (item->canonical-semantic
                    (description->entity (:item-id joe) alternate-store))
                   (canonicalize-list '("Joe"
                                        "male" 
                                        (39 ("age" :tag)
                                            ("doubtful" "confidence"))
                                        "married"
                                        (45 ("age" :tag))))))))))

(deftest confirm-actions-test
  (let [last (atom nil)]
    (is (= (confirm-actions {1 :a 2 :b 3 :c} last)
           [:a :b :c]))
    (is (= (confirm-actions {2 :b 3 :c 4 :d} last)
           [:d]))
    (is (= (confirm-actions {2 :b 3 :c 4 :d} last)
           []))
    (is (= (confirm-actions {} last)
           []))
    (is (= @last 4))))

(ns cosheet.server.actions-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [utils :refer [dissoc-in]]
             [orderable :as orderable]
             [state-map :refer [new-state-map state-map-get-current-value
                                state-map-reset!]]
             [entity :as entity :refer [description->entity to-list
                                        content elements
                                        label->elements label->content]]
             [expression-manager :refer [new-expression-manager-data compute]]
             [debug :refer [profile-and-print-reporters
                            store-as-list simplify-for-print]]
             entity-impl
             [query :refer [matching-elements matching-items]]
             [store :refer [new-element-store new-mutable-store
                            current-store id-valid?]]
             [store-impl :refer [->ItemId]]
             [store-utils :refer [add-entity]]
             mutable-store-impl
             [canonical :refer [canonicalize-list]]
             [test-utils :refer [check any]])
            (cosheet.server
             [referent :refer [item-referent union-referent virtual-referent
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
             [:pattern :subject '(nil (39 (:variable (:v :name)
                                                     ("age" :condition)
                                                     (:true :reference))))]
             :foo "bar"]]
    (is (check (substitute-in-key key [joe])
               [(:item-id joe)
                (:item-id joe-bogus-age)
                (:item-id joe-male)
                (:item-id joe-bogus-age)
                :foo "bar"]))
    (is (check (substitute-in-key [[:pattern :subject]] [joe-age])
               [(:item-id joe)]))))

(deftest do-add-element-test
  (let [result (do-add-element
                store
                {:referent (item-referent jane-age)
                 :target-key ["jane-age"]}
                {})
        new-store (:store result)]
    (is (check (item->canonical-semantic
                (to-list (description->entity (:item-id jane-age) new-store)))
               (canonicalize-list '(45 ("age" :tag) ""))))
    (is (check (:select result)
               [["jane-age" (any)] [["jane-age"]]]))))

(deftest do-add-label-test
  (let [result (do-add-label
                store
                {:referent (item-referent jane-age)
                 :target-key ["jane-age"]}
                {})
        new-store (:store result)]
    (is (check (item->canonical-semantic
                (to-list (description->entity (:item-id jane-age) new-store)))
               (canonicalize-list '(45 ("age" :tag) ("" :tag)))))
    (is (check (:select result)
               [[:label (any)] [["jane-age"]]]))))

(deftest do-add-twin-test
  (let [result (do-add-twin
                store
                {:referent (item-referent jane-age)
                 :template '(nil ("age" :tag))
                 :target-key ["jane" "jane-age"]}
                {})
        new-store (:store result)]
    (is (check (item->canonical-semantic
                (to-list (description->entity (:item-id jane) new-store)))
               (canonicalize-list '("Jane"
                                    "female"
                                    (45 ("age" :tag)) ("" ("age" :tag))))))
    (is (check (:select result)
               [["jane" (any)] [["jane" "jane-age"]]]))))

(deftest do-add-virtual-test
  (let [result (do-add-virtual
                store
                {:referent
                 (virtual-referent '(nil ("age" :tag))
                                   (union-referent [(item-referent jane)])
                                   (item-referent jane) :position :after)
                 :select-pattern ["jane" [:pattern]]
                 :target-key ["jane" "jane-age"]}
                {})]
    (is (check (item->canonical-semantic
                (to-list (description->entity (:item-id jane) (:store result))))
               (canonicalize-list '("Jane"
                                    "female"
                                    (45 ("age" :tag)) ("" ("age" :tag))))))
    (is (check (:select result)
               [["jane" (any)] [["jane" "jane-age"]]]))))

(deftest do-add-row-test
  (let [result (do-add-row
                store
                {:target-key ["jane" "jane-age"]}
                {:row {:referent (item-referent jane)
                       :template '("a" :new-row)
                       :key ["x" "y"]}
                 :column {:referent (item-referent joe)}})
        new-store (:store result)]
    (let [new-rows (matching-items "a" new-store)]
      (is (= (count new-rows) 1)))
    (is (check (:select result)
               [["x" (any) (item-referent joe)] [["jane" "jane-age"]]])))  )

(deftest do-delete-test
  (let [new-store (do-delete
                   store
                   {:referent (item-referent jane-age)
                    :target-key "jane"}
                   {})]
    (is (check (canonicalize-list
                (to-list (description->entity jane-id new-store)))
               (canonicalize-list `("Jane" (~o1 :order :non-semantic)
                                    ("female" (~o2 :order :non-semantic)))))))
  ;; Set up a store with some headers to test the special cases.
  (let [[store column1-id] (add-entity store nil
                                    `(~'anything
                                      ("name" :tag (~o1 :order :non-semantic))
                                      (~o1 :order :non-semantic)
                                      (:column :non-semantic)))
        column1 (description->entity column1-id store)
        name-header (first (matching-elements "name" column1))]
    ;; Test delete of the only element of a header.
    (let [new-store (do-delete
                     store
                     {:referent (item-referent column1)}
                     {:target-key "name"})]
      (is (not (id-valid? new-store column1-id))))
    ;; Test when it is not in header position.
    (let [new-store (do-delete
                     store
                     {:referent (union-referent
                                      [(item-referent joe)
                                       (item-referent name-header)])}
                     {:target-key "name"})]
      (is (id-valid? new-store column1-id)))))

(deftest do-set-content-test
  (is (= (content
          (description->entity
           (:item-id joe-age)
           (do-set-content store
                           {:referent (item-referent joe-age)
                            :target-key "joe"
                            :from "45" :to "46"}
                           {})))
         46))
  ;; Test making the new content be 'anything.
  (is (= (content
          (description->entity
           (:item-id joe-age)
           (do-set-content store
                           {:referent (item-referent joe-age)
                            :target-key "joe"
                            :from "45" :to ""}
                           {:selector-category :foo})))
         'anything))
  ;; Test doing nothing when the old doesn't match.
  (is (= (content
          (description->entity
           joe-id (do-set-content store
                                  {:referent (item-referent joe)
                                   :target-key "joe"
                                   :from "Wrong" :to "Jim"}
                                  {})))
         "Joe"))
  ;; Now, try calling it when there is a parallel referent.
  (let [modified (do-set-content
                  store
                  {:referent (union-referent
                                   [(item-referent joe-age-tag)
                                    (item-referent jane-age-tag)])
                   :target-key "both"
                   :from "age" :to "oldness"}
                  {})]
    (is (= (item->canonical-semantic
            (description->entity (:item-id joe-age-tag) modified))
           (canonicalize-list '("oldness" :tag))))
    (is (= (item->canonical-semantic
            (description->entity (:item-id jane-age-tag) modified))
           (canonicalize-list '("oldness" :tag)))))
    ;; Try creating new content
    (let [result (do-set-content
                  store
                  {:referent (virtual-referent
                              '(nil :tag) (item-referent joe-male)
                              (item-referent joe-male) :position :after)
                   :target-key ["joe-male"]
                   :from "" :to "gender"}
                  {})]
      (is (= (immutable-semantic-to-list
              (description->entity (:item-id joe-male) result))
             ["male" ["gender" :tag]]))))

(deftest do-expand-test
  (is (check (do-expand store
                        {:referent (item-referent joe)
                         :target-key "joe"
                         :session-state {:url-path "foo"}}
                        {})
             {:store store
              :open (str "foo?referent="
                         (referent->string (item-referent joe)))})))

(deftest do-actions-test
  (let [mutable-store (new-mutable-store store)
        tracker (new-dom-tracker mutable-store)
        session-state {:tracker tracker
                       :store mutable-store
                       :selector-interpretation :broad
                       :client-state (new-state-map {:last-action nil})}
        attributes {:commands {:add-element nil}
                    :selector-category :table-header
                    :target {:referent
                             (union-referent [(item-referent jane)
                                              (item-referent joe)])}}]
    (swap! tracker update-set-component
           {:key [:jane]
            :definition [(fn [& _] [:div])]
            :attributes attributes})
    (let [result (do-actions mutable-store session-state
                             [[:add-element (key->id tracker [:jane])]])
          new-store (current-store mutable-store)
          select (:select result)
          new-id (last (first select))]
      (is (= select [[:jane new-id] [[:jane]]]))
      (is (check (item->canonical-semantic
                  (description->entity (:item-id jane) new-store))
                 (canonicalize-list '("Jane" "female"
                                      (45 ("age" :tag))
                                      anything))))
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
             'anything))
      ;; Check undo redo.
      (do-actions mutable-store {:tracker tracker
                                 :client-state (new-state-map
                                                {:last-action nil})}
                  [[:undo]])
      (is (check (current-store mutable-store)
                 (assoc store :modified-ids #{})))
      ;; Check redo.
      (do-actions mutable-store {:tracker tracker
                                 :client-state (new-state-map
                                                {:last-action nil})}
                  [[:redo]])
      (is (check (current-store mutable-store) new-store)))))

(deftest confirm-actions-test
  (let [client-state (new-state-map {:last-action nil})]
    (is (= (confirm-actions {1 :a 2 :b 3 :c} client-state)
           [:a :b :c]))
    (is (= (confirm-actions {2 :b 3 :c 4 :d} client-state)
           [:d]))
    (is (= (confirm-actions {2 :b 3 :c 4 :d} client-state)
           []))
    (is (= (confirm-actions {} client-state)
           []))
    (is (= (state-map-get-current-value client-state :last-action) 4))))

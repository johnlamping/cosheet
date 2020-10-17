(ns cosheet2.server.actions-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet2
             [utils :refer [dissoc-in]]
             [orderable :as orderable]
             [map-state :refer [new-map-state map-state-get-current
                                map-state-reset!]]
             [entity :as entity :refer [description->entity to-list
                                        content elements
                                        label->elements label->content]]
             [calculator :refer [new-calculator-data compute]]
             [debug :refer [profile-and-print-reporters
                            store-as-list simplify-for-print]]
             entity-impl
             [query :refer [matching-elements matching-items variable-query]]
             [store :refer [new-element-store new-mutable-store
                            current-store id-valid? id->content]]
             [store-impl :refer [->ItemId]]
             [store-utils :refer [add-entity]]
             [task-queue :refer [new-priority-task-queue]]
             mutable-store-impl
             [canonical :refer [canonicalize-list]]
             [test-utils :refer [check any]])
            (cosheet2.server
             [dom-manager :refer [new-dom-manager add-root-dom]]
             [actions :refer :all]
             [model-utils :refer [entity->canonical-semantic
                                  semantic-to-list selector?]]
             [session-state :refer [update-add-session-temporary-element]])
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
                    ("age" :label)
                    ("doubtful" "confidence"))
                ("married" (~o2 :order :non-semantic))
                (45 (~o4 :order :non-semantic)
                    ("age" :label))))
(def jane-list `("Jane" (~o1 :order :non-semantic)
                 (:selector :non-semantic)
                 ("female" (~o2 :order :non-semantic))
                 (45 (~o3 :order :non-semantic)
                     ("age" :label))))
(def t1 (add-entity (new-element-store) nil joe-list))
(def joe-id (second t1))
(def t2 (add-entity (first t1) nil jane-list))
(def jane-id (second t2))
(def t3 (update-add-session-temporary-element (first t2)))
(def temporary-id (first t3))
(def store (first t3))
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

(def session-state {:session-temporary-id temporary-id})

(deftest selected-test
  (let [client-id1 "root_1"
        client-id2 "root_2"
        store1 (update-selected store joe-id client-id1)
        recovered-id1 (get-selected store1 joe-id)
        ;; Now, try overwriting an existing id.
        store2 (update-selected store1 joe-id client-id2)
        recovered-id2 (get-selected store2 joe-id)] 
    (is (= client-id1 recovered-id1))
    (is (= client-id2 recovered-id2))))

(deftest do-set-content-test
  (let [updated (do-set-content store
                                {:target-ids [(:item-id joe-age)]
                                 :from "45"
                                 :to ""})]
    (is (= (id->content updated (:item-id joe-age))
           "")))
  ;; Test making the new content be 'anything.
  (let [updated (do-set-content store
                                ;; Jane is a selector
                                {:target-ids [(:item-id jane-age)]
                                 :from "45"
                                 :to ""})]
    (is (= (id->content updated (:item-id jane-age))
           'anything)))
  ;; Test doing nothing when the old doesn't match.
  (let [updated (do-set-content store
                                {:target-ids [(:item-id joe-age)]
                                 :from "47"
                                 :to "46"})]
    (is (= (id->content updated (:item-id joe-age))
           45)))
  ;; Test updating multiple ids
  (let [updated (do-set-content store
                                {:target-ids [(:item-id jane-age)
                                              (:item-id joe-age)]
                                 :from "45"
                                 :to ""})]
    (is (= (id->content updated (:item-id joe-age))
           ""))
    (is (= (id->content updated (:item-id jane-age))
           'anything)))
  ;; Test that setting a column to 'anything does nothing.
  (let [[store column1-id] (add-entity
                            store nil
                            `(~'anything
                              ("name" :label (~o1 :order :non-semantic))
                              (~o1 :order :non-semantic)
                              (:column :non-semantic)
                              (:selector :non-semantic)))
        column1 (description->entity column1-id store)
        name-header (first (matching-elements "name" column1))
        new-store (do-set-content store
                                  {:target-ids [(:item-id name-header)]
                                   :from "name"
                                   :to ""})]
    (is (= (id->content new-store (:item-id name-header))
           "name"))))

(deftest do-add-twin-test
  (let [store (update-selected store temporary-id "old selection")
        result (do-add-twin store
                            {:target-ids [(:item-id joe-age)
                                          (:item-id jane-age)]
                             :session-state session-state
                             :template '(anything 5)})
        new-store (:store result)
        new-jane (description->entity jane-id new-store)
        new-joe (description->entity joe-id new-store)]
    (is (check (entity->canonical-semantic new-joe)
               (canonicalize-list
                '("Joe" "male" "married"
                  ("" 5)
                  (45 ("age" :label))
                  (39 ("age" :label) ("doubtful" "confidence"))))))
    (is (check (entity->canonical-semantic new-jane)
               (canonicalize-list '("Jane" "female"
                                    (anything 5)
                                    (45 ("age" :label))))))
    (let [new-joe-element (first (matching-elements "" new-joe))
          new-jane-element (first (matching-elements 'anything new-jane))]
      (is (check (dissoc result :store)
                 {:select-store-ids [(:item-id new-joe-element)
                                     (:item-id new-jane-element)]
                  :if-selected ["old selection"]})))))

(deftest do-add-element-test
  (let [store (update-selected store temporary-id "old selection")
        result (do-add-element store
                               {:target-ids [(:item-id joe-age)
                                             (:item-id jane-age)]
                                :session-state session-state
                                :elements-template '(anything 5)})
        new-store (:store result)
        new-jane-age (description->entity (:item-id jane-age) new-store)
        new-joe-age (description->entity (:item-id joe-age) new-store)]
    (is (check (entity->canonical-semantic new-joe-age)
               (canonicalize-list '(45 ("age" :label) ("" 5)))))
    (is (check (entity->canonical-semantic new-jane-age)
               (canonicalize-list '(45 ("age" :label) (anything 5)))))
    (let [new-joe-element (first (matching-elements "" new-joe-age))
          new-jane-element (first (matching-elements 'anything new-jane-age))]
      (is (check (dissoc result :store)
                 {:select-store-ids [(:item-id new-joe-element)
                                     (:item-id new-jane-element)]
                  :if-selected ["old selection"]})))))

(deftest do-add-label-test
  (let [store (update-selected store temporary-id "old selection")
        result (do-add-label store
                               {:target-ids [(:item-id joe-age)
                                             (:item-id jane-age)]
                                :session-state session-state})
        new-store (:store result)
        new-jane-age (description->entity (:item-id jane-age) new-store)
        new-joe-age (description->entity (:item-id joe-age) new-store)]
    (is (check (entity->canonical-semantic new-joe-age)
               (canonicalize-list '(45 ("age" :label) ("" :label)))))
    (is (check (entity->canonical-semantic new-jane-age)
               (canonicalize-list '(45 ("age" :label) (anything :label)))))
    (let [new-joe-element (first (matching-elements "" new-joe-age))
          new-jane-element (first (matching-elements 'anything new-jane-age))]
      (is (check (dissoc result :store)
                 {:select-store-ids [(:item-id new-joe-element)
                                     (:item-id new-jane-element)]
                  :if-selected ["old selection"]})))))

(deftest do-delete-test
  (let [new-store (do-delete store
                             {:target-ids [(:item-id joe-age)
                                           (:item-id jane-age)]})
        new-jane (description->entity jane-id new-store)
        new-joe (description->entity joe-id new-store)]
    (is (check (entity->canonical-semantic new-joe)
               (canonicalize-list
                '("Joe" "male" "married"
                  (39 ("age" :label) ("doubtful" "confidence"))))))
    (is (check (entity->canonical-semantic new-jane)
               (canonicalize-list '("Jane" "female")))))
  ;; Test that deleting the only element of a column does nothing.
  (let [[store column1-id] (add-entity
                            store nil
                            `(~'anything
                              ("name" :label (~o1 :order :non-semantic))
                              (~o1 :order :non-semantic)
                              (:column :non-semantic)))
        column1 (description->entity column1-id store)
        name-header (first (matching-elements "name" column1))
        new-store (do-delete store
                             {:target-ids [joe-id
                                           (:item-id name-header)]})]
    (is (not (id-valid? new-store joe-id)))
    (is (id-valid? new-store (:item-id name-header)))))

(deftest do-selected-test
  (let [store (update-selected store temporary-id "old selection")
        new-store (do-selected store
                               {:client-id "Larry"
                                :session-state session-state})]
    (is (= (get-selected new-store temporary-id)
           "Larry"))))

(comment

  (deftest do-add-twin-test
    (let [result (do-add-twin
                  store
                  {:referent (item-referent jane-age)
                   :template '(anything ("age" :label))
                   :target-key ["jane" "jane-age"]})
          new-store (:store result)]
      (is (check (item->canonical-semantic
                  (to-list (description->entity (:item-id jane) new-store)))
                 (canonicalize-list '("Jane"
                                      "female"
                                      (45 ("age" :label))
                                      (anything ("age" :label))))))
      (is (check (:select result)
                 [["jane" (any)] [["jane" "jane-age"]]]))))

  (deftest do-add-virtual-test
    (let [result (do-add-virtual
                  store
                  {:referent
                   (virtual-referent '(anything ("age" :label))
                                     (union-referent [(item-referent jane)])
                                     (item-referent jane) :position :after)
                   :select-pattern ["jane" [:pattern]]
                   :target-key ["jane" "jane-age"]})]
      (is (check (item->canonical-semantic
                  (to-list (description->entity (:item-id jane) (:store result))))
                 (canonicalize-list '("Jane"
                                      "female"
                                      (45 ("age" :label))
                                      (anything ("age" :label))))))
      (is (check (:select result)
                 [["jane" (any)] [["jane" "jane-age"]]]))))

  (deftest do-add-row-test
    (let [result (do-add-row
                  store
                  {:target-key ["jane" "jane-age"]
                   :row {:referent (item-referent jane)
                         :template '("a" :new-row)
                         :key ["x" "y"]}
                   :column {:referent (item-referent joe)}})
          new-store (:store result)]
      (let [new-rows (matching-items "a" new-store)]
        (is (= (count new-rows) 1)))
      (is (check (:select result)
                 [["x" (any) (item-referent joe)] [["jane" "jane-age"]]])))  )

  (deftest do-expand-test
    (is (check (do-expand store
                          {:referent (item-referent joe)
                           :target-key "joe"
                           :session-state {:url-path "foo"}})
               {:store store
                :open (str "foo?referent="
                           (referent->string (item-referent joe)))})))

  (deftest batch-edit-select-key-test
    (is (= (batch-edit-select-key
            [joe joe-age joe-age-tag]
            [jane joe])
           [:batch (:item-id joe) (:item-id joe-age-tag)]))
    (is (= (batch-edit-select-key
            [joe joe-age joe-age-tag]
            [jane])
           nil)))

  (deftest do-actions-test
    (let [queue (new-priority-task-queue 0)
          mutable-store (new-mutable-store store queue)
          tracker (new-dom-tracker mutable-store)
          session-state {:tracker tracker
                         :store mutable-store
                         :selector-interpretation :broad
                         :client-state (new-state-map {:last-action nil} queue)}
          attributes {:commands {:add-element nil}
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
                                        (45 ("age" :label))
                                        anything))))
        (is (check (item->canonical-semantic
                    (description->entity (:item-id joe) new-store))
                   (canonicalize-list '("Joe"
                                        "male" 
                                        (39 ("age" :label)
                                            ("doubtful" "confidence"))
                                        "married"
                                        (45 ("age" :label))
                                        ""))))
        (is (= (immutable-semantic-to-list
                (description->entity new-id new-store))
               'anything)))))

  (deftest confirm-actions-test
    (let [queue (new-priority-task-queue 0)
          client-state (new-state-map {:last-action nil} queue)]
      (is (= (confirm-actions {1 :a 2 :b 3 :c} client-state)
             [:a :b :c]))
      (is (= (confirm-actions {2 :b 3 :c 4 :d} client-state)
             [:d]))
      (is (= (confirm-actions {2 :b 3 :c 4 :d} client-state)
             []))
      (is (= (confirm-actions {} client-state)
             []))
      (is (= (state-map-get-current-value client-state :last-action) 4))))
  )

(deftest do-actions-test
  (let [queue (new-priority-task-queue 0)
        cd (new-calculator-data queue)
        mutable-store (new-mutable-store store)
        manager (new-dom-manager mutable-store cd)
        session-state {:dom-manager manager
                       :store mutable-store
                       :client-state (new-map-state {:last-action nil})}]
    (add-root-dom
     manager
     :root
     {:relative-id joe-id
      :render-dom (fn [spec store]
                    [:div "joe" [:component
                                 {:relative-id (:item-id joe-age)
                                  :render-dom (fn [spec store]
                                                [:div 45])}]])})
    (let [for-client (do-actions
                      mutable-store session-state
                      [[:set-content "root" :from "joe" :to "Joseph"]])
          new-store (current-store mutable-store)]
      (is (= (id->content new-store joe-id) "Joseph"))
      (is (= for-client {}))
      ;; TODO: Once we support selected, check that undo and redo ask
      ;; for the old selection.

      ;; Check undo.
      (let [for-client (do-actions mutable-store session-state [[:undo]])])
      (is (check (current-store mutable-store)
                 (assoc store :modified-ids #{})))
      ;; Check redo.
      (do-actions mutable-store session-state [[:redo]])
      (is (check (current-store mutable-store) new-store)))))

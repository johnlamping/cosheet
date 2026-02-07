(ns cosheet2.server.actions-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet2
             [utils :refer [dissoc-in]]
             [orderable :refer [initial split earlier?]]
             [map-state :refer [new-map-state map-state-get-current
                                map-state-reset!]]
             [entity :as entity :refer [id->entity id->object to-list
                                        content elements label->element
                                        label->elements label->content
                                        name-label link-type object-type
                                        make-object-list make-element-list
                                        uniquely-identified-object?
                                        in-different-store]]
             [calculator :refer [new-calculator-data compute]]
             [debug :refer [profile-and-print-reporters
                            store-as-list simplify-for-print]]
             entity-impl
             [query :refer [matching-elements matching-items variable-query]]
             [store :refer [new-element-store new-mutable-store
                            target-label->ids
                            current-store id-valid-link?
                            id->source id->target
                            get-new-object-id
                            update-source]]
             [store-utils :refer [add-element add-object
                                  add-universal-objects]]
             [task-queue :refer [new-priority-task-queue]]
             mutable-store-impl
             [canonical :refer [canonicalize]]
             [test-utils :refer [check any as-set]])
            (cosheet2.server
             [dom-manager :refer [new-dom-manager add-root-dom
                                  relative-ids->client-id
                                  client-id->relative-ids]]
             [actions :refer :all]
             [action-data :refer [get-id-action-data default-get-action-data
                                  update-action-data-for-component]]
             [order-utils :refer [ordered-entities add-order-elements]]
             [model-utils :refer [entity->canonical-semantic
                                  semantic-elements
                                  semantic-to-list selector?
                                  pattern-to-fixed-term]]
             [session-state :refer [update-add-session-temporary-element]]
             [render-utils :refer [make-component]]
             [item-render :refer [render-item-DOM]])
            ; :reload
            ))

(def orderables (reduce (fn [os _]
                          (vec (concat (pop os)
                                       (split (peek os) :after))))
                        [initial]
                        (range 5)))
(def o1 (nth orderables 0))
(def o2 (nth orderables 1))
(def o3 (nth orderables 2))
(def o4 (nth orderables 3))
(def o5 (nth orderables 4))
(def unused-orderable (nth orderables 4))
(def joe-list `("Joe"
                (~o2 :order)
                ("male" (~o1 :order))
                (39 (~o3 :order)
                    ("age" :label)
                    ("doubtful" "confidence"))
                ("married" (~o2 :order))
                (45 (~o4 :order)
                    ("age" :label))))
(def jane-list `("Jane"
                 (~o1 :order)
                 (:selector)
                 ("female" (~o2 :order))
                 (45 (~o3 :order)
                     ("age" :label))))
(def row-condition-elements ['(anything ("age" :label))])
(def column-headers ['(anything ("age" :label))
                     '(anything ("c2" :label))])
(def table-list (add-order-elements
                 `(:x
                   :selector
                   (:x :row-condition ~@row-condition-elements)
                   (:x :column-headers ~@column-headers))))
(def t0 (add-element (new-element-store) nil table-list))
(def table-id (second t0))
(def t1 (add-element (first t0) nil joe-list))
(def joe-id (second t1))
(def t2 (add-element (first t1) nil jane-list))
(def jane-id (second t2))
(def t3 (update-add-session-temporary-element (first t2)))
(def temporary-id (second t3))
(def store (first t3))
(def headers-id (first (target-label->ids
                       store table-id :column-headers)))
(def header-ids (map :item-id (semantic-elements
                               (id->entity headers-id store))))       
(def joe (id->entity joe-id store))
(def joe-age (first (matching-elements 45 joe)))
(def joe-bogus-age (first (matching-elements 39 joe)))
(def joe-age-tag (first (matching-elements "age" joe-age)))
(def joe-male (first (matching-elements "male" joe)))
(def joe-married (first (matching-elements "married" joe)))
(def jane (id->entity jane-id store))
(def jane-female (first (matching-elements "female" jane)))
(def jane-age (first (matching-elements 45 jane)))
(def jane-age-tag (first (matching-elements "age" jane-age)))

(def session-state {:session-temporary-id temporary-id
                    :store (new-mutable-store store)
                    :client-state (new-map-state {})})

;;; TODO: !!! This is the new format for table cells, where each is an object.
;;;       The store needs to convert to this.
(def new-joe-object-list (make-object-list
                          `(("Joe" (~name-label) (~o5 :order))
                            (~o2 :order)
                            ("male" (~o1 :order))
                            (39 (~o3 :order)
                                ("age" :label)
                                ("doubtful" "confidence"))
                            ("married" (~o2 :order))
                            (45 (~o4 :order)
                                ("age" :label)))))
(def new-jane-object-list (make-object-list
                           `(("Jane" (~name-label) (~o5 :order))
                             (~o1 :order)
                             (:selector)
                             ("female" (~o2 :order))
                             (45 (~o3 :order)
                                 ("age" :label)))))
(def new-t0 (add-element (new-element-store) nil table-list))
(def new-table-id (second new-t0))
(def new-t1 (add-object (first new-t0) new-joe-object-list))
(def new-joe-id (second new-t1))
(def new-t2 (add-object (first new-t1) new-jane-object-list))
(def new-jane-id (second new-t2))
(def new-t3 (update-add-session-temporary-element (first new-t2)))
(def new-temporary-id (second new-t3))
(def new-store (first new-t3))
(def new-headers-id (first (target-label->ids
                       new-store new-table-id :column-headers)))
(def new-header-ids (map :item-id (semantic-elements
                               (id->entity new-headers-id new-store))))       
(def new-joe (id->entity new-joe-id new-store))
(def new-joe-age (first (matching-elements 45 new-joe)))
(def new-joe-bogus-age (first (matching-elements 39 new-joe)))
(def new-joe-age-tag (first (matching-elements "age" new-joe-age)))
(def new-joe-male (first (matching-elements "male" new-joe)))
(def new-joe-married (first (matching-elements "married" new-joe)))
(def new-jane (id->entity new-jane-id new-store))
(def new-jane-female (first (matching-elements "female" new-jane)))
(def new-jane-age (first (matching-elements 45 new-jane)))
(def new-jane-age-tag (first (matching-elements "age" new-jane-age)))

(def new-session-state {:session-temporary-id new-temporary-id
                        :store (new-mutable-store new-store)
                        :client-state (new-map-state {})})

(deftest selected-test
  (let [client-id1 "root_1"
        client-id2 "root_2"
        store1 (update-selected store temporary-id client-id1)
        recovered-id1 (get-selected store1 temporary-id)
        ;; Now, try overwriting an existing id.
        store2 (update-selected store1 temporary-id client-id2)
        recovered-id2 (get-selected store2 temporary-id)] 
    (is (= client-id1 recovered-id1))
    (is (= client-id2 recovered-id2))))

(deftest get-or-make-object-by-name-test
  (let [[s id] (get-or-make-object-by-name
                new-store "Joe" (make-object-list []))]
    (is (= s new-store))
    (is (= new-joe-id id)))
  (let [[s id] (get-or-make-object-by-name
                new-store "Joey" (make-object-list []))
        joey (id->entity id s)]
    (is (uniquely-identified-object? joey))
    (is (= (map semantic-to-list (elements joey))
           `(("Joey" (~(in-different-store name-label s)))))))
  (let [[s id] (get-or-make-object-by-name
                new-store "Joe" (make-object-list ['(3 4)]))]
    (is (= s new-store))
    (is (= new-joe-id id)))
  (let [[s id] (get-or-make-object-by-name
                new-store "Joe" (make-object-list [`(~link-type) '(3 4)]))
        joe (id->entity id s)]
    (is (uniquely-identified-object? joe))
    (is (check (map semantic-to-list (elements joe))
               (as-set `(("Joe" (~(in-different-store name-label s)))
                         (~(in-different-store link-type s))
                         (3 4))))))
  (let [[s id] (get-or-make-object-by-name
                new-store "Joe" (make-object-list [`(~object-type) '(3 4)]))
        joe (id->entity id s)]
    (is (uniquely-identified-object? joe))
    (is (check (map semantic-to-list (elements joe))
               (as-set `(("Joe" (~(in-different-store name-label s)))
                         (~(in-different-store object-type s))
                         (3 4)))))))

(deftest do-set-content-test
  (let [result (do-set-content store
                                {:subject-ids [(:item-id joe-age)]
                                 :from "45"
                                 :to ""
                                 :session-state session-state})]
    (is (= (id->source (:store result) (:item-id joe-age))
           "")))
  ;; Test making the new content be 'anything.
  (let [result (do-set-content store
                                ;; Jane is a selector
                                {:subject-ids [(:item-id jane-age)]
                                 :from "45"
                                 :to ""
                                 :session-state session-state})]
    (is (= (id->source (:store result) (:item-id jane-age))
           'anything)))
  ;; Test doing nothing when the old doesn't match.
  (let [result (do-set-content store
                                {:subject-ids [(:item-id joe-age)]
                                 :from "47"
                                 :to "46"
                                 :session-state session-state})]
    (is (= (id->source (:store result) (:item-id joe-age))
           45)))
  ;; Test updating multiple ids
  (let [result (do-set-content store
                                {:subject-ids [(:item-id jane-age)
                                               (:item-id joe-age)]
                                 :from "45"
                                 :to ""
                                 :session-state session-state})]
    (is (= (id->source (:store result) (:item-id joe-age))
           ""))
    (is (= (id->source (:store result) (:item-id jane-age))
           'anything)))
  ;; Test that setting a column to 'anything does nothing.
  (let [[store columns-id] (add-element
                            store nil
                            `(~'anything :column-headers :selector
                              (~'anything
                               ("name" :label (~o1 :order))
                               (~o1 :order))))
        columns (id->entity columns-id store)
        column1 (first (matching-elements '(anything "name") columns))
        name-header (first (matching-elements "name" column1))
        result (do-set-content store
                                  {:subject-ids [(:item-id name-header)]
                                   :from "name"
                                   :to ""
                                   :session-state session-state})]
    (is (= (id->source (:store result) (:item-id name-header))
           "name"))))

(deftest do-set-content-named-object-test
  ;; This tests the whole path from rendering dom, getting its action data,
  ;; and doing a set content to a new object.
  
  (let [;; First, set up a store with two objects, Fred and Sally, and with
        ;; an element holding Fred.
        [s1 fred-oid] (-> (new-element-store)
                          (add-universal-objects)
                          (get-new-object-id))
        [s2 fred-name-id] (add-element s1 fred-oid `("Fred" ~name-label))
        [s3 fred-foo-id] (add-element s2 fred-oid "foo")
        [s4 fred-holder-id] (add-element s3 nil (id->object fred-oid s2))
        [s5 sally-oid] (get-new-object-id s4)
        [store sally-name-id] (add-element s5 sally-oid `("Sally" ~name-label))
        ;; Now, render the nesting doms: the holding element, the
        ;; object inside, and its name.
        holder-dom-spec {:relative-id fred-holder-id
                         :width 2.0
                         :template `(~(make-object-list ["foo"]))
                         :get-action-data default-get-action-data
                         :render-dom render-item-DOM}
        holder-dom (render-item-DOM holder-dom-spec store)
        [_ object-dom-spec] holder-dom
        object-dom ((:render-dom object-dom-spec) object-dom-spec store)
        [_ name-dom-spec] object-dom
        ;; Now, walk the nested doms to get the action data. 
        holder-component-atom (atom {:dom-specification holder-dom-spec})
        object-component-atom (atom {:dom-specification object-dom-spec})
        name-component-atom (atom {:dom-specification name-dom-spec})
        holder-action-data (update-action-data-for-component
                            holder-component-atom {}
                            :set-content store)
        object-action-data (update-action-data-for-component
                            object-component-atom holder-action-data
                            :set-content store)
        name-action-data (update-action-data-for-component
                            name-component-atom object-action-data
                            :set-content store)
        ;; And set up a function to run setting the name.
        run-set-name (fn [from to]
                       (let [action-data
                             (assoc name-action-data
                                    :template (:template name-dom-spec)
                                    :from from
                                    :to to
                                    :session-state session-state)]
                         (-> (do-set-content store action-data)
                             (normalize-handler-response store))))]
    
    ;; Test changing Fred to Sally.
    (let [[new-store for-client] (run-set-name "Fred" "Sally")]
      (is (= new-store (update-source store fred-holder-id sally-oid)))
      (is (= (:select-store-ids for-client) [sally-name-id])))

    ;; Test changing Fred to Fred.
    (let [[new-store for-client] (run-set-name "Fred" "Fred")]
      (is (= new-store store)))

    ;; Test changing Fred to fred.
    (let [[new-store for-client] (run-set-name "Fred" "fred")]
      (is (= new-store store)))
    
    ;; Test changing to an object that had to be created.
    (let [[new-store for-client] (run-set-name "Fred" "Bob")
          new-name-id (first (:select-store-ids for-client))
          new-object-id (id->target new-store new-name-id)
          new-object (id->entity new-object-id new-store)]
      (is (= (id->source new-store new-name-id) "Bob"))
      (is (= (id->source new-store fred-holder-id) new-object-id))
      (is (check (map to-list (elements new-object))
                 (as-set [`("Bob" (~(in-different-store name-label new-store)))
                          "foo"]))))))

(deftest do-add-twin-test
  (let [store (update-selected store temporary-id "old selection")
        result (do-add-twin store
                            {:subject-ids [(:item-id joe-age)
                                           (:item-id jane-age)]
                             :session-state session-state
                             :template '(anything 5)})
        new-store (:store result)
        new-jane (id->entity jane-id new-store)
        new-joe (id->entity joe-id new-store)]
    (is (check (entity->canonical-semantic new-joe)
               (canonicalize
                '("Joe" "male" "married"
                  ("" 5)
                  (45 ("age" :label))
                  (39 ("age" :label) ("doubtful" "confidence"))))))
    (is (check (entity->canonical-semantic new-jane)
               (canonicalize '("Jane" "female"
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
                               {:subject-ids [(:item-id joe-age)
                                              (:item-id jane-age)]
                                :session-state session-state})
        new-store (:store result)
        new-jane-age (id->entity (:item-id jane-age) new-store)
        new-joe-age (id->entity (:item-id joe-age) new-store)]
    (is (check (entity->canonical-semantic new-joe-age)
               (canonicalize '(45 ("age" :label) ""))))
    (is (check (entity->canonical-semantic new-jane-age)
               (canonicalize '(45 ("age" :label) anything))))
    (let [new-joe-element (first (matching-elements "" new-joe-age))
          new-jane-element (first (matching-elements 'anything new-jane-age))]
      (is (check (dissoc result :store)
                 {:select-store-ids [(:item-id new-joe-element)
                                     (:item-id new-jane-element)]
                  :if-selected ["old selection"]})))))

(deftest do-add-label-test
  (let [store (update-selected store temporary-id "old selection")
        result (do-add-label store
                               {:subject-ids [(:item-id joe-age)
                                              (:item-id jane-age)]
                                :session-state session-state})
        new-store (:store result)
        new-jane-age (id->entity (:item-id jane-age) new-store)
        new-joe-age (id->entity (:item-id joe-age) new-store)]
    (is (check (entity->canonical-semantic new-joe-age)
               (canonicalize '(45 ("age" :label) ("" :label)))))
    (is (check (entity->canonical-semantic new-jane-age)
               (canonicalize '(45 ("age" :label) (anything :label)))))
    (let [new-joe-element (first (matching-elements "" new-joe-age))
          new-jane-element (first (matching-elements 'anything new-jane-age))]
      (is (check (dissoc result :store)
                 {:select-store-ids [(:item-id new-joe-element)
                                     (:item-id new-jane-element)]
                  :if-selected ["old selection"]})))))

(deftest do-delete-test
  (let [new-store (do-delete store
                             {:subject-ids [(:item-id joe-age)
                                            (:item-id jane-age)]})
        new-jane (id->entity jane-id new-store)
        new-joe (id->entity joe-id new-store)]
    (is (check (entity->canonical-semantic new-joe)
               (canonicalize
                '("Joe" "male" "married"
                  (39 ("age" :label) ("doubtful" "confidence"))))))
    (is (check (entity->canonical-semantic new-jane)
               (canonicalize '("Jane" "female")))))
  ;; Test that deleting the only element of a column does nothing.
  (let [[store columns-id] (add-element
                            store nil
                            `(~'anything :column-headers :selector
                              (~'anything
                               ("name" :label (~o1 :order))
                               (~o1 :order))))
        columns (id->entity columns-id store)
        column1 (first (matching-elements '(anything "name") columns))
        name-header (first (matching-elements "name" column1))
        new-store (do-delete store
                             {:subject-ids [joe-id
                                            (:item-id name-header)]})]
    (is (not (id-valid-link? new-store joe-id)))
    (is (id-valid-link? new-store (:item-id name-header)))))

(deftest do-add-row-test
  (let [first-header-id (first header-ids)
        result (do-add-row store
                           {:target-key ["jane" "jane-age"]
                            :table-id table-id
                            :row-id jane-id
                            :column-ids [first-header-id]
                            :client-id (relative-ids->client-id
                                        [table-id jane-id first-header-id])})
        [new-store client-data] (normalize-handler-response result store)
        row-condition (pattern-to-fixed-term `(nil ~@row-condition-elements))
        rows (matching-items row-condition store)
        new-rows (matching-items row-condition new-store)]
    (is (= (count new-rows)
           (+ 1 (count rows))))
    (let [new-id (first (clojure.set/difference (set (map :item-id new-rows))
                                                (set (map :item-id rows))))]
      (is (check (client-id->relative-ids (:select client-data))
                 [table-id new-id first-header-id])))))

(deftest do-add-column-test
  (let [first-header-id (first header-ids)
        result (do-add-column store
                              {:target-key ["jane" "jane-age"]
                               :table-id table-id
                               :column-ids [first-header-id]
                               :row-id jane-id
                               :client-id (relative-ids->client-id
                                           [table-id jane-id first-header-id])})
        [new-store client-data] (normalize-handler-response result store)
        new-table-entity (id->entity table-id new-store)
        new-headers-entity (first (label->elements new-table-entity
                                                   :column-headers))
        new-headers (semantic-elements new-headers-entity)]
    (is (= (count new-headers)
           (+ 1 (count header-ids))))
    (let [new-id (first (clojure.set/difference (set (map :item-id new-headers))
                                                (set header-ids)))]
      (is (check (client-id->relative-ids (:select client-data))
                 [table-id jane-id new-id])))))

()
(deftest do-delete-row-test
  (let [result (do-delete-row store
                              {:target-key ["jane" "jane-age"]
                               :table-id table-id
                               :row-id jane-id})
        [new-store client-data] (normalize-handler-response result store)
        row-condition (pattern-to-fixed-term
                       `(nil ~@row-condition-elements))
        rows (matching-items row-condition store)
        new-rows (matching-items row-condition new-store)]
    (is (= (count new-rows)
           (- (count rows) 1)))))

(deftest do-delete-column-test
  (let [table-entity (id->entity table-id store)
        headers-entity (first (label->elements table-entity :column-headers))
        headers (semantic-elements headers-entity)
        first-header-id (:item-id (first headers))
        result (do-delete-column store
                                 {:target-key ["jane" "jane-age"]
                                  :table-id table-id
                                  :column-ids [first-header-id]})
        [new-store client-data] (normalize-handler-response result store)
        new-table-entity (id->entity table-id new-store)
        new-headers-entity (first (label->elements new-table-entity
                                                   :column-headers))
        new-headers (semantic-elements new-headers-entity)]
    (is (= (count new-headers)
           (- (count headers) 1)))
    ;; Make sure that an attempt to delete two columns does nothing.
    (let [second-header-id (:item-id (first headers))
          result (do-delete-column store
                              {:target-key ["jane" "jane-age"]
                               :table-id table-id
                               :column-ids [first-header-id second-header-id]})
          [new-store client-data] (normalize-handler-response result store)
          new-headers-entity (id->entity headers-id new-store)
          new-headers (semantic-elements new-headers-entity)]
      (is (= (count new-headers)
             (count headers))))))

(deftest do-batch-edit-test
  (let [updated (do-batch-edit
                 store
                 {:query-ids [joe-id]
                  :stack-ids [jane-id]
                  :selected-index 0
                  :selection-sequence [(:item-id jane-age)]
                  :session-state session-state})
        session-temporary (id->entity temporary-id (:store updated))
        query-item (first (label->elements session-temporary :batch-query))
        stack-item (first (label->elements session-temporary :batch-stack))]
    (is (check (canonicalize (semantic-to-list query-item))
               (canonicalize '(anything ("Joe"
                                              "male"
                                              "married"
                                              (39 ("age" :label)
                                                  ("doubtful" "confidence"))
                                              (45 ("age" :label)))))))
    (is (check (canonicalize (semantic-to-list stack-item))
               (canonicalize '(anything ("Jane"
                                              (45 ("age" :label))
                                              "female")))))
    (is (= (:select-store-ids updated)
           [(:item-id (first 
                       (matching-elements
                        45 (first (matching-elements
                                   "Jane" stack-item)))))]))
    ;; Now try an update to the new store, with no stack selector.
    (let [reupdated (do-batch-edit
                     (:store updated)
                     {:query-ids [jane-id joe-id]
                      :stack-ids []
                      :session-state session-state})
          session-temporary (id->entity temporary-id (:store reupdated))
          query-item (label->element session-temporary :batch-query)
          stack-item (label->element session-temporary :batch-stack)]
      (is (check (semantic-to-list stack-item)
                 'anything))
      (is (check (canonicalize (semantic-to-list query-item))
                 (canonicalize '(anything ("Joe"
                                           "male"
                                           "married"
                                           (39 ("age" :label)
                                               ("doubtful" "confidence"))
                                           (45 ("age" :label)))
                                          ("Jane"
                                           (45 ("age" :label))
                                           "female")))))
      (is (earlier? (label->content
                     (first (matching-elements "Jane" query-item)) :order)
                    (label->content
                     (first (matching-elements "Joe" query-item)) :order)))
      ;; Now try with no batch edit information in the target
      (let [rereupdated (do-batch-edit
                         (:store reupdated)
                         {:session-state session-state})
            new-session-temporary (id->entity temporary-id (:store reupdated))
            new-query-item (first (label->elements session-temporary
                                                   :batch-query))]
        (is (= new-query-item query-item))))))

(deftest do-selected-test
  (let [ms (new-mutable-store store)  ; We use a new mutable store,
                                      ; so we don't mess up the starting one.
        queue (new-priority-task-queue 0)
        cd (new-calculator-data queue)
        manager (new-dom-manager ms cd)
        ss (assoc session-state :store ms :dom-manager manager)]
    (add-root-dom manager {:relative-id :Larry
                           :render-dom (fn [& _] [:div])
                           :get-action-data [get-id-action-data :Larry]})
    (let [for-client (do-selected ms ss "Larry")]
      (is (= (get-selected (current-store ms) temporary-id)
             "Larry"))
      (is (nil? for-client)))))

(comment
  (deftest do-add-twin-test
    (let [result (do-add-twin
                  store
                  {:referent (item-referent jane-age)
                   :template '(anything ("age" :label))
                   :target-key ["jane" "jane-age"]})
          new-store (:store result)]
      (is (check (item->canonical-semantic
                  (to-list (id->entity (:item-id jane) new-store)))
                 (canonicalize '("Jane"
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
                  (to-list (id->entity (:item-id jane) (:store result))))
                 (canonicalize '("Jane"
                                      "female"
                                      (45 ("age" :label))
                                      (anything ("age" :label))))))
      (is (check (:select result)
                 [["jane" (any)] [["jane" "jane-age"]]]))))

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
                    (id->entity (:item-id jane) new-store))
                   (canonicalize '("Jane" "female"
                                        (45 ("age" :label))
                                        anything))))
        (is (check (item->canonical-semantic
                    (id->entity (:item-id joe) new-store))
                   (canonicalize '("Joe"
                                        "male" 
                                        (39 ("age" :label)
                                            ("doubtful" "confidence"))
                                        "married"
                                        (45 ("age" :label))
                                        ""))))
        (is (= (immutable-semantic-to-list
                (id->entity new-id new-store))
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
     {:relative-id :root
      :get-action-data [get-id-action-data :root]
      :render-dom (fn [spec store]
                    [:div [:component
                           {:relative-id (:item-id joe)
                            :render-dom (fn [spec store]
                                          [:div
                                           [:component
                                            {:relative-id (:item-id joe-age)
                                             :render-dom (fn [spec store]
                                                           [:div 45])}]])
                            :get-action-data [get-id-action-data (:item-id joe)]
                            }]])})
    (let [for-client (do-actions
                      mutable-store session-state
                      [[:set-content (str "root_" (:id (:item-id joe)))
                        :from "Joe" :to "Joseph"]])
          new-store (current-store mutable-store)]
      (is (= (id->source new-store joe-id) "Joseph"))
      (is (= for-client {:select-store-ids [joe-id]}))
      ;; TODO: Once we support selected, check that undo and redo ask
      ;; for the old selection.

      ;; Check undo.
      (let [for-client (do-actions mutable-store session-state [[:undo]])])
      (is (check (current-store mutable-store)
                 (assoc store :modified-ids #{})))
      ;; Check redo.
      (do-actions mutable-store session-state [[:redo]])
      (is (check (current-store mutable-store) new-store)))))

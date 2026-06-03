(ns cosheet.server.actions-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [utils :refer [dissoc-in]]
             [orderable :refer [initial split earlier?]]
             [map-reporter :refer [make-map-reporter map-reporter-get-current
                                map-reporter-reset!]]
             [entity :as entity :refer [id->entity id->object to-list
                                        content elements label->element
                                        label->elements label->content
                                        name-label link-type object-type
                                        make-object-list make-element-list
                                        uniquely-identified-object?
                                        in-different-store
                                        recursively-in-different-store]]
             [calculator :refer [make-calculator-data compute]]
             [debug :refer [profile-and-print-reporters
                            store-as-list simplify-for-print]]
             entity-impl
             [query :refer [matching-elements matching-items variable-query]]
             [store :refer [new-element-store new-mutable-store
                            target-label->ids
                            current-store id-valid-link?
                            id->source id->target
                            get-new-object-id
                            update-source store-update!
                            update-equivalent-undo-point]]
             [store-utils :refer [add-element add-object
                                  add-universal-objects]]
             [task-queue :refer [make-priority-task-queue]]
             mutable-store-impl
             [canonical :refer [canonicalize]]
             [test-utils :refer [check any as-set]])
            (cosheet.server
             [dom-manager :refer [make-dom-manager add-root-dom
                                  relative-ids->client-id
                                  client-id->relative-ids]]
             [actions :refer :all]
             [action-data :refer [get-id-action-data default-get-action-data
                                  update-action-data-for-component]]
             [order-utils :refer [ordered-entities add-order-elements]]
             [model-utils :refer [entity->canonical-semantic
                                  semantic-elements selector?
                                  semantic-to-list object-semantic-to-list
                                  pattern-to-fixed-term
                                  label-object-template
                                  update-add-object-with-order
                                  update-add-element-with-order-and-ephemeral]]
             [session-state :refer [update-add-session-ephemeral-element]]
             [render-utils :refer [make-component]]
             [item-render :refer [render-item-DOM-R]])
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
(def joe-list (add-order-elements
               '("Joe"
                 "male"
                 (39 ("age" :label) ("doubtful" "confidence"))
                 "married"
                 (45 ("age" :label)))))
(def jane-list (add-order-elements
                '("Jane" :selector "female" (45 ("age" :label)))))
(def row-condition-elements ['(anything ("age" :label))])
(def column-headers ['(anything ("age" :label))
                     '(anything ("c2" :label))])
(def table-list (add-order-elements
                 `(:x
                   :selector
                   (~(make-object-list row-condition-elements)
                    :row-condition)
                   (:x :column-headers ~@column-headers))))
(def t0 (add-element (new-element-store) nil table-list))
(def table-id (second t0))
(def t1 (add-element (first t0) nil joe-list))
(def joe-id (second t1))
(def t2 (add-element (first t1) nil jane-list))
(def jane-id (second t2))
(def t3 (update-add-session-ephemeral-element (first t2)))
(def ephemeral-id (second t3))
(def store (first t3))
(def headers-id (first (target-label->ids
                       store table-id :column-headers)))
(def header-ids (map :item-id (semantic-elements
                               (id->entity headers-id store))))       
(def joe (id->entity joe-id store))
(def joe-age (first (matching-elements 45 joe)))
(def joe-bogus-age (first (matching-elements 39 joe)))
(def joe-age-label (first (matching-elements "age" joe-age)))
(def joe-male (first (matching-elements "male" joe)))
(def joe-married (first (matching-elements "married" joe)))
(def jane (id->entity jane-id store))
(def jane-female (first (matching-elements "female" jane)))
(def jane-age (first (matching-elements 45 jane)))

(def session-state {:session-ephemeral-id ephemeral-id
                    :store (new-mutable-store store)
                    :client-state (make-map-reporter {})})

;;; TODO: !!! This is the new format for table rows, where each is an object.
;;;       Replace the previous store by this.
;;; TODO: !!! The labels need to be converted to label objects.
(def new-joe-object-list
  (make-object-list
   (map add-order-elements
        `(("Joe" (~name-label))
          "male"
          (39 ("age" :label) ("doubtful" "confidence"))
          "married"
          (45 ("age" :label))))))
(def new-jane-object-list
  (make-object-list
   (map add-order-elements
        `(("Jane" (~name-label)) :selector "female" (45 ("age" :label))))))
(def new-t0 (add-element (new-element-store) nil table-list))
(def new-table-id (second new-t0))
(def new-t1 (add-object (first new-t0) new-joe-object-list))
(def new-joe-id (second new-t1))
(def new-t2 (add-object (first new-t1) new-jane-object-list))
(def new-jane-id (second new-t2))
(def new-t3 (update-add-session-ephemeral-element (first new-t2)))
(def new-ephemeral-id (second new-t3))
(def new-store (first new-t3))
(def new-headers-id (first (target-label->ids
                       new-store new-table-id :column-headers)))
(def new-header-ids (map :item-id (semantic-elements
                               (id->entity new-headers-id new-store))))       
(def new-joe (id->entity new-joe-id new-store))
(def new-joe-age (first (matching-elements 45 new-joe)))
(def new-joe-bogus-age (first (matching-elements 39 new-joe)))
(def new-joe-age-label (first (matching-elements "age" new-joe-age)))
(def new-joe-male (first (matching-elements "male" new-joe)))
(def new-joe-married (first (matching-elements "married" new-joe)))
(def new-jane (id->entity new-jane-id new-store))
(def new-jane-female (first (matching-elements "female" new-jane)))
(def new-jane-age (first (matching-elements 45 new-jane)))

(def new-session-state {:session-ephemeral-id new-ephemeral-id
                        :store (new-mutable-store new-store)
                        :client-state (make-map-reporter {})})

(deftest current-source-matches-from?-test
  ;; Test numbers
  (let [[store five-id] (add-element new-store new-joe-id `(5 (~o5 :order)))]
    (is (current-source-matches-from? store five-id "5" nil))
    (is (not (current-source-matches-from? store five-id "6" nil))))
  ;; Test named objects.
  (let [[store friend-id] (add-element new-store new-joe-id
                                       `(~(id->object new-jane-id nil)
                                         (~o5 :order)))]
    (is (current-source-matches-from? store friend-id new-jane-id nil))
    (is (not (current-source-matches-from? store friend-id new-joe-id nil))))
  ;; Test uninterned objects.
  (let [common-elements [`("" (~name-label)
                           `(~o5 :order))
                          `(~link-type)]
        ;; Make two objects that differ only in a non-semantic element
        ;; and one object that differs from them semantically
        common-object1 (make-object-list (conj common-elements `(~o1 :order)))
        common-object2 (make-object-list (conj common-elements `(~o2 :order)))
        longer-object (make-object-list
                       (conj common-elements `(~o1 :order) `(5 (~o4 order))))
        [s1 common-id1] (add-element new-store new-joe-id `(~common-object1))
        [s2 common-id2] (add-element s1 new-joe-id `(~common-object2))
        [store longer-id] (add-element s2 new-joe-id `(~longer-object))]
    ;; Semantically matching
    (is (current-source-matches-from?
         store common-id1 (id->source store common-id2) nil))
    (is (current-source-matches-from?
         store longer-id (id->source store longer-id) nil))
    ;; Semantically not matching
    (is (not (current-source-matches-from?
              store common-id1 (id->source store longer-id) nil)))
    (is (not (current-source-matches-from?
              store longer-id (id->source store common-id1) nil)))
    (let [[store anything-id] (add-element store new-joe-id 'anything)
          [store a-id] (add-element store new-joe-id "A")]
      ;; "" in place of 'anything
      (is (current-source-matches-from? store anything-id "" nil))
      ;; A universal header matching something random
      (is (current-source-matches-from? store common-id1 "\u00A0..." nil))
      ;; The special case where a universal header shouldn't match
      (is (current-source-matches-from? store a-id "\u00A0A" "B"))
      (is (not (current-source-matches-from? store a-id "\u00A0A" ""))))))

(deftest do-set-content-test
  (let [new-store (do-set-content store
                                  {:subject-ids [(:item-id joe-age)]
                                   :from "45"
                                   :to ""
                                   :session-state session-state})]
    (is (= (id->source new-store (:item-id joe-age))
           "")))
  ;; Test making the new content be 'anything.
  (let [new-store (do-set-content store
                                  ;; Jane is a selector
                                  {:subject-ids [(:item-id jane-age)]
                                   :from "45"
                                   :to ""
                                   :session-state session-state})]
    (is (= (id->source new-store (:item-id jane-age))
           'anything)))
  ;; Test doing nothing when the old doesn't match.
  (let [new-store (do-set-content store
                                  {:subject-ids [(:item-id joe-age)]
                                   :from "47"
                                   :to "46"
                                   :session-state session-state})]
    (is (= (id->source (or new-store store) (:item-id joe-age))
           45)))
  ;; Test updating multiple ids
  (let [new-store (do-set-content store
                                  {:subject-ids [(:item-id jane-age)
                                                 (:item-id joe-age)]
                                   :from "45"
                                   :to ""
                                   :session-state session-state})]
    (is (= (id->source new-store (:item-id joe-age))
           ""))
    (is (= (id->source new-store (:item-id jane-age))
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
        new-store (do-set-content store
                                  {:subject-ids [(:item-id name-header)]
                                   :from "name"
                                   :to ""
                                   :session-state session-state})]
    (is (= (id->source (or new-store store) (:item-id name-header))
           "name"))))

(deftest do-set-content-named-object-test
  ;; This tests the whole path from rendering dom, getting its action data,
  ;; and doing a set content to a new object.
  (let [;; First, set up a store with two objects, Fred and Sally, and with
        ;; an element holding Fred.
        [s1 fred-oid order] (update-add-object-with-order
                             (add-universal-objects (new-element-store))
                             (make-object-list `(("Fred" (~name-label)) "foo"))
                             initial :after)
        [s2 fred-holder-id order] (update-add-element-with-order-and-ephemeral
                                   s1 nil
                                   `(~(id->object fred-oid s1))
                                   initial :after false)
        [store sally-oid order] (update-add-object-with-order
                                 s2
                                 (make-object-list `(("Sally" (~name-label))))
                                 order :after)
        sally-name (label->element (id->entity sally-oid store) name-label)
        sally-name-id (:item-id sally-name)
        ;; We include an empty name in the templates, to make sure
        ;; that is handled correctly.
        bare-template (make-object-list [`("" (~name-label))])
        foo-template (make-object-list [`("" (~name-label)) "foo"])
        ;; Now, render the nesting doms: the holding element, the
        ;; object inside, and its name.
        holder-dom-spec {:relative-id fred-holder-id
                         :width 2.0
                         :template `(~foo-template)
                         :get-action-data default-get-action-data
                         :render-dom render-item-DOM-R}
        holder-dom (render-item-DOM-R holder-dom-spec store)
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
        run-set-name (fn [from to template]
                       (let [action-data
                             (assoc name-action-data
                                    :template template
                                    :from from
                                    :to to
                                    :session-state session-state)]
                         (-> (do-set-content store action-data)
                             (normalize-handler-response store))))]
    
    ;; Test changing Fred to Sally.
    (let [[new-store for-client] (run-set-name "Fred" "Sally" bare-template)]
      (is (= (id->source new-store fred-holder-id) sally-oid))
      (is (= (dissoc-in new-store
                        [:ephemeral-data :following-selection-by-ids])
             (update-source store fred-holder-id sally-oid)))
      (is (check (:ephemeral-data new-store)
                 {:following-selection-by-ids [nil [sally-name-id]]})))

    ;; Test changing Fred to Sally, with a template that requires more.
    ;; Also test the template being an element.
    (let [[new-store for-client] (run-set-name "Fred" "Sally" `(~foo-template))]
      (is (check (object-semantic-to-list (id->object sally-oid new-store))
                 (as-set (make-object-list `(("Sally" (~(in-different-store
                                                         name-label new-store)))
                                             "foo")))))
      (is (check (:ephemeral-data new-store)
                    {:following-selection-by-ids [nil [sally-name-id]]})))

    ;; Test changing Fred to Fred.
    (let [[new-store for-client] (run-set-name "Fred" "Fred" foo-template)]
      (is (= new-store store)))

    ;; Test changing Fred to fred.
    (let [[new-store for-client] (run-set-name "Fred" "fred" foo-template)]
      (is (= new-store store)))

    ;; Test no change if the from field is wrong;
    (let [[new-store for-client] (run-set-name "Joe" "Sally" foo-template)]
      (is (= new-store store)))
    
    ;; Test changing to an object that had to be created.
    (let [[new-store for-client] (run-set-name "Fred" "Bob" foo-template)
          [_ [new-name-id]] (:following-selection-by-ids
                             (:ephemeral-data new-store))
          new-object-id (id->target new-store new-name-id)
          new-object (id->entity new-object-id new-store)]
      (is (= (id->source new-store new-name-id) "Bob"))
      (is (= (id->source new-store fred-holder-id) new-object-id))
      (is (check (map semantic-to-list (semantic-elements new-object))
                 (as-set [`("Bob" (~(in-different-store name-label new-store)))
                          "foo"]))))))

(deftest do-add-twin-test
  (let [new-store (do-add-twin store
                               {:subject-ids [(:item-id joe-age)
                                              (:item-id jane-age)]
                                :session-state session-state
                                :template '(anything 5)})
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
      (is (check (:ephemeral-data new-store)
                 {:following-selection-by-ids
                  [nil (as-set [(:item-id new-joe-element)
                                (:item-id new-jane-element)])]})))))

(deftest do-add-element-test
  (let [new-store (do-add-element store
                                  {:subject-ids [(:item-id joe-age)
                                                 (:item-id jane-age)]
                                   :session-state session-state})
        new-jane-age (id->entity (:item-id jane-age) new-store)
        new-joe-age (id->entity (:item-id joe-age) new-store)]
    (is (check (entity->canonical-semantic new-joe-age)
               (canonicalize '(45 ("age" :label) ""))))
    (is (check (entity->canonical-semantic new-jane-age)
               (canonicalize '(45 ("age" :label) anything))))
    (let [new-joe-element (first (matching-elements "" new-joe-age))
          new-jane-element (first (matching-elements 'anything new-jane-age))]
      (is (check (:ephemeral-data new-store)
                 {:following-selection-by-ids
                  [nil (as-set [(:item-id new-joe-element)
                                (:item-id new-jane-element)])]})))))

(deftest do-add-label-test
  ;; Test for adding a label when there is more than one subject id.
  (let [new-store (do-add-label store
                                {:subject-ids [(:item-id joe-age)
                                               (:item-id jane-age)]
                                 :session-state session-state})
        new-jane-age (id->entity (:item-id jane-age) new-store)
        new-joe-age (id->entity (:item-id joe-age) new-store)
        generic-label (recursively-in-different-store
                       label-object-template
                       new-store)
        blank-label (recursively-in-different-store
                     (make-object-list [`("" (~name-label))
                                        `(~link-type)])
                     new-store)]
    (is (check (entity->canonical-semantic new-joe-age)
               (canonicalize `(45 ("age" :label) (~blank-label)))))
    (is (check (entity->canonical-semantic new-jane-age)
               (canonicalize `(45 ("age" :label) (~generic-label)))))
    (let [new-joe-element (first (matching-elements `(~blank-label)
                                                    new-joe-age))
          new-jane-element (first (matching-elements `(~generic-label)
                                                     new-jane-age))]
      (is (check (:ephemeral-data new-store)
                 {:following-selection-by-ids
                  [nil (as-set [(:item-id new-joe-element)
                                (:item-id new-jane-element)])]}))))
  ;; Test that adding a label to a label does nothing.
  (let [result (do-add-label store
                             {:subject-ids [(:item-id joe-age-label)]
                              :session-state session-state})]
    (is (not result))))

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
        new-store (do-add-row store
                              {:target-key ["jane" "jane-age"]
                               :table-id table-id
                               :row-id jane-id
                               :column-ids [first-header-id]
                               :client-id (relative-ids->client-id
                                           [table-id jane-id first-header-id])})
        row-pattern (pattern-to-fixed-term
                     (make-object-list row-condition-elements))
        rows (matching-items row-pattern store)
        new-rows (matching-items row-pattern new-store)]
    (is (= (count new-rows)
           (+ 1 (count rows))))
    (let [new-id (first (clojure.set/difference (set (map :item-id new-rows))
                                                (set (map :item-id rows))))]
      (is (check (client-id->relative-ids
                  (:following-selection (:ephemeral-data new-store)))
                 [table-id new-id first-header-id])))))

(deftest do-add-column-test
  (let [first-header-id (first header-ids)
        new-store (do-add-column store
                                 {:target-key ["jane" "jane-age"]
                                  :table-id table-id
                                  :column-ids [first-header-id]
                                  :row-id jane-id
                                  :client-id (relative-ids->client-id
                                              [table-id jane-id first-header-id])})
        new-table-entity (id->entity table-id new-store)
        new-headers-entity (first (label->elements new-table-entity
                                                   :column-headers))
        new-headers (semantic-elements new-headers-entity)]
    (is (= (count new-headers)
           (+ 1 (count header-ids))))
    (let [new-id (first (clojure.set/difference (set (map :item-id new-headers))
                                                (set header-ids)))]
      (is (check (client-id->relative-ids
                  (:following-selection (:ephemeral-data new-store)))
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
        session-ephemeral (id->entity ephemeral-id (:store updated))
        query-item (first (label->elements session-ephemeral :batch-query))
        stack-item (first (label->elements session-ephemeral :batch-stack))]
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
    (is (check (:ephemeral-data (:store updated))
               {:following-selection-by-ids
                [nil [(:item-id (first
                                 (matching-elements
                                  45 (first (matching-elements
                                             "Jane" stack-item)))))]]}))
    ;; Now try an update to the new store, with no stack selector.
    (let [reupdated (do-batch-edit
                     (:store updated)
                     {:query-ids [jane-id joe-id]
                      :stack-ids []
                      :session-state session-state})
          session-ephemeral (id->entity ephemeral-id (:store reupdated))
          query-item (label->element session-ephemeral :batch-query)
          stack-item (label->element session-ephemeral :batch-stack)]
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
            new-session-ephemeral (id->entity ephemeral-id (:store reupdated))
            new-query-item (first (label->elements session-ephemeral
                                                   :batch-query))]
        (is (= new-query-item query-item))))))

(deftest do-selected-test
  (let [ms (new-mutable-store store)  ; We use a new mutable store,
                                      ; so we don't mess up the starting one.
        queue (make-priority-task-queue 0)
        cd (make-calculator-data queue)
        manager (make-dom-manager ms cd)
        ss (assoc session-state :store ms :dom-manager manager)]
    (add-root-dom manager {:relative-id :Larry
                           :render-dom (fn [& _] [:div])
                           :get-action-data [get-id-action-data :Larry]})
    (let [for-client (do-selected ms ss "Larry")]
      (is (nil? for-client)))))

(deftest do-undo-redo-test
  (let [preceding-client-id "preceding-client-id"
        following-client-id "following-client-id"
        ms (new-mutable-store store)
        initial-store (current-store ms)
        ;; Create an undoable state with :preceding-selection and
        ;; :following-selection set.
        _ (store-update! ms
                   (fn [s]
                     (-> (update-equivalent-undo-point s false)
                         (update-source joe-id "Joseph")
                         (assoc :ephemeral-data
                                {:preceding-selection preceding-client-id
                                 :following-selection following-client-id}))))
        following-store (current-store ms)]
    ;; do-undo should select the :preceding-selection from before the undo.
    (is (check (do-undo ms nil) {:select preceding-client-id}))
    (is (= (current-store ms) initial-store))
    ;; No more history to undo, so do-undo returns nil.
    (is (nil? (do-undo ms nil)))
    (is (= (current-store ms) initial-store))
    ;; do-redo should select the :following-selection from after the redo.
    (is (check (do-redo ms nil) {:select following-client-id}))
    (is (= (current-store ms) following-store))
    ;; No more future to redo, so do-redo returns nil.
    (is (nil? (do-redo ms nil)))
    (is (= (current-store ms) following-store))))

(deftest do-actions-test
  (let [queue (make-priority-task-queue 0)
        cd (make-calculator-data queue)
        joe-client-id (str "root_" (:id (:item-id joe)))
        mutable-store (new-mutable-store store)
        manager (make-dom-manager mutable-store cd)
        session-state {:session-ephemeral-id ephemeral-id
                       :dom-manager manager
                       :store mutable-store
                       :client-state (make-map-reporter {:last-action nil})}]
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
                      [[:set-content joe-client-id
                        :from "Joe" :to "Joseph"]])
          new-store (current-store mutable-store)]
      (is (= (id->source new-store joe-id) "Joseph"))
      (is (check for-client
                 {:select nil
                  :select-by-ids [joe-client-id [joe-id]]
                  :if-selected [joe-client-id]}))
      (is (check (:ephemeral-data new-store)
                 {:following-selection-by-ids [joe-client-id [joe-id]]
                  :preceding-selection joe-client-id}))
      ;; TODO: Once we support selected, check that undo and redo ask
      ;; for the old selection.

      ;; Check undo.
      (let [for-client (do-actions mutable-store session-state [[:undo]])])
      (is (check (current-store mutable-store)
                 (assoc store :modified-ids #{})))
      ;; Check redo.
      (do-actions mutable-store session-state [[:redo]])
      (is (check (current-store mutable-store) new-store))
      (is (= (:following-selection-by-ids
              (:ephemeral-data (current-store mutable-store)))
             [joe-client-id [joe-id]])))))

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
            [joe joe-age joe-age-label]
            [jane joe])
           [:batch (:item-id joe) (:item-id joe-age-label)]))
    (is (= (batch-edit-select-key
            [joe joe-age joe-age-label]
            [jane])
           nil)))

  (deftest do-actions-test
    (let [queue (make-priority-task-queue 0)
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
    (let [queue (make-priority-task-queue 0)
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


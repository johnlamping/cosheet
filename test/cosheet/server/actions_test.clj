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
             [debug :refer [current-value profile-and-print-reporters]]
             [reporters :as reporter]
             entity-impl
             [store :refer [new-element-store
                            id->content id->element-ids id-label->element-ids
                            new-mutable-store current-store]]
             [store-impl :refer [->ItemId]]
             [store-utils :refer [add-entity]]
             [mutable-manager :refer [current-mutable-value]]
             [mutable-set :refer [new-mutable-set]]
             mutable-store-impl)
            (cosheet.server
             [key :refer [item-referent elements-referent prepend-to-key
                          item-referent? parallel-referent?
                          remove-first-primitive-referent
                          canonicalize-list semantic-to-list
                          item->canonical-semantic]]
             [render :refer [item-DOM]]
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
(def jane-female (first (filter #(= (content %) "female") (elements jane))))
(def jane-age (first (label->elements jane "age")))
(def jane-age-tag (first (elements jane-age)))

(defn new-joe-jane-tracker [mutable-store]
  (let [md (new-expression-manager-data)
        tracker (new-dom-tracker md)]
    (add-dom tracker
             "joe-root"
             [joe-id :bob]
             [item-DOM
              (description->entity joe-id mutable-store)
              [:bob] #{} {:depth 1 :do-not-merge #{}}])
    (add-dom tracker
             "jane-root"
             [jane-id :bob]
             [item-DOM
              (description->entity jane-id mutable-store)
              [:bob] #{} {:depth 1  :do-not-merge #{}}])
    (compute md)
    tracker))

(defn find-dom-id
  "Given a tracker and an entity, find the id of the dom for that entity."
  [tracker entity]
  (let [id (:item-id entity)
        tracker-data @tracker]
    (first (filter #(= (first (get-in tracker-data [:id->key %])) id)
                   (keys (:id->key tracker-data))))))

(deftest update-add-entity-with-order-test
  (let [[s id order] (update-add-entity-with-order
                      store joe-id 6
                      unused-orderable :before true)
        joe (description->entity joe-id s)
        new-entity (first (filter #(= (content %) 6) (elements joe)))
        [o5 o6] (orderable/split unused-orderable :before)]
    (is (= (to-list new-entity)
           `(6 (~o5 :order))))
    (is (= order o6))
    (is (= (:item-id new-entity) id)))
  (let [[s id order] (update-add-entity-with-order
                      store joe-id 6
                      unused-orderable :before false)
        joe (description->entity joe-id s)
        new-entity (first (filter #(= (content %) 6)
                                  (elements joe)))
        [o5 o6] (orderable/split unused-orderable :after)]
    (is (= (to-list new-entity)
           `(6 (~o5 :order))))
    (is (= order o6))
    (is (= (:item-id new-entity) id)))    
  (let [[s id order] (update-add-entity-with-order
                      store joe-id 6
                      unused-orderable :after true)
        joe (description->entity joe-id s)
        new-entity (first (filter #(= (content %) 6)
                                  (elements joe)))
        [o5 o6] (orderable/split unused-orderable :after)]
    (is (= (to-list new-entity)
           `(6 (~o6 :order))))
    (is (= order o5))
    (is (= (:item-id new-entity) id)))
  (let [[s id order] (update-add-entity-with-order
                      store joe-id '(6 ("height" tag))
                      unused-orderable :before true)
        joe (description->entity joe-id s)
        new-entity (first (label->elements joe "height"))
        [x o5] (orderable/split unused-orderable :before)
        [o6 o7] (orderable/split x :after)]
    (is (= (canonicalize-list (to-list new-entity))
           (canonicalize-list `(6 (~o7 :order)
                                  ("height" ~'tag (~o6 :order))))))
    (is (= order o5))
    (is (= (:item-id new-entity) id)))
  ;; Check that order in the list style entity is preserved in the
  ;; :order values.
  (let [[s id order] (update-add-entity-with-order
                      store joe-id '(6 ("height" tag) ("weight" tag))
                      unused-orderable :after false)
        joe (description->entity joe-id s)
        new-entity (first (label->elements joe "height"))
        [x o5] (orderable/split unused-orderable :before)
        [x o6] (orderable/split x :before)
        [o8 o7] (orderable/split x :before)]
    (is (= (canonicalize-list (to-list new-entity))
           (canonicalize-list `(6 (~o5 :order)
                                  ("height" ~'tag (~o7 :order))
                                  ("weight" ~'tag (~o6 :order))))))
    (is (= order o8))
    (is (= (:item-id new-entity) id))))

(deftest update-add-element-test
  (let [order-entity (first (label->elements jane :order))
        order (content order-entity)
        [s id] (update-add-element '("foo" 6) store jane)
        new-jane (description->entity jane-id s)
        new-element (first (filter #(= (content %) "foo")
                                   (elements new-jane)))
        [x o5] (orderable/split order :before)
        [o6 o7] (orderable/split x :before)]
    (is (= (canonicalize-list (to-list new-element))
           (canonicalize-list `("foo" (~o5 :order) (6 (~o7 :order))))))
    (is (= (id->content s (:item-id order-entity)) o6))
    (is (= id (:item-id new-element))))
  ;; Test the case where the subject doesn't have its own order information.
  (let [[store sally-id] (add-entity (new-element-store) nil "Sally")
        [store unused-id] (add-entity store nil o4)
        [store _] (add-entity store unused-id :unused-orderable)
        sally-entity (description->entity sally-id store)
        order-entity (description->entity unused-id store)
        [s id] (update-add-element '("bar" 6) store sally-entity)
        new-sally-entity (description->entity sally-id s)
        new-element (first (filter #(= (content %) "bar")
                                   (elements new-sally-entity)))
        [x o5] (orderable/split o4 :before)
        [o6 o7] (orderable/split x :before)]
    (is (= (canonicalize-list (to-list new-element))
           (canonicalize-list `("bar" (~o5 :order) (6 (~o7 :order))))))
    (is (= (id->content s (:item-id order-entity)) o6))
    (is (= id (:item-id new-element)))))

(deftest add-element-handler-test
  (let [joe-age-id (:item-id joe-age)
        mutable-store (new-mutable-store store)
        tracker (new-joe-jane-tracker mutable-store)
        joe-age-dom-id (find-dom-id tracker joe-age)
        joe-age-key (get-in @tracker [:id->key joe-age-dom-id])]
    (let [{:keys [store select]} (add-element-handler
                                  store tracker joe-age-key)
          new-joe-age (description->entity joe-age-id store)
          new-element (first (filter #(= (content %) "")
                                     (elements new-joe-age)))]
      (is (= select [(prepend-to-key (item-referent new-element) joe-age-key)
                     [joe-age-key]]))
      (is (= (item->canonical-semantic new-joe-age)
             [45 {["age" {'tag 1}] 1, "" 1}])))))

(deftest adjust-condition-test
  (let [[c1 s1] (adjust-condition '(nil ('??? :a) ('??? 22))
                                  (new-element-store))
        [c2 s2] (adjust-condition '(nil ('??? nil) ('??? "22"))
                                  s1)]
    (is (= c1  '("" ('???-0 :a) ('???-1 22))))
    (is (= c2  '("" ('???-2 "") ('???-3 "22"))))))

(deftest update-add-sibling-test
  (let [jane-dom (current-value
                  (item-DOM jane [(item-referent jane)]
                            #{} {:depth 1 :do-not-merge #{}}))
        order-entity (first (label->elements jane-age :order))
        order (content order-entity)
        age-dom (first (filter #(= (first (:key (second %)))
                                   (item-referent jane-age))
                               (dom->subcomponents jane-dom)))
        [s id] (update-add-sibling (:sibling-condition (second age-dom))
                                   :after
                              store jane-age)
        new-jane (description->entity jane-id s)
        new-element (first (filter #(= (content %) "")
                                   (elements new-jane)))
        [o5 x] (orderable/split order :after)
        [o6 o7] (orderable/split x :before)]
    (is (= (canonicalize-list (to-list new-element))
           (canonicalize-list `("" (~o6 :order) ("age" ~'tag (~o7 :order))))))
    (is (= (id->content s (:item-id order-entity)) o5))
    (is (= id (:item-id new-element)))))

(deftest add-sibling-handler-test
  (let [mutable-store (new-mutable-store store)
        tracker (new-joe-jane-tracker mutable-store)
        joe-age-dom-id (find-dom-id tracker joe-age)
        joe-age-key (get-in @tracker [:id->key joe-age-dom-id])]
    (let [{:keys [store select]} (add-sibling-handler
                                  store tracker joe-age-key :after)]
      (let [joe-age-ids (id-label->element-ids store joe-id "age")
            joe-ages (map #(id->content store %) joe-age-ids)
            new-element-id (first (filter #(= (id->content store %) "")
                                       joe-age-ids))]
        (is (= (set joe-ages) #{"" 39 45}))
        (is (= select [(prepend-to-key
                        new-element-id
                        (remove-first-primitive-referent joe-age-key))
                       [joe-age-key]]))))))

(deftest furthest-item-test
  (is (= (furthest-item [joe-married] :before) joe-married))
  (is (= (furthest-item [joe-married joe-male] :before) joe-male))
  (is (= (furthest-item [joe-married joe-male] :after) joe-married)))

(deftest do-add-test
  (let [jane-dom (current-value
                  (item-DOM jane [(item-referent jane)]
                            #{} {:depth 1 :do-not-merge #{}}))
        order-entity (first (label->elements jane-age :order))
        order (content order-entity)
        age-dom (first (filter #(= (first (:key (second %)))
                                   (item-referent jane-age))
                               (dom->subcomponents jane-dom)))
        jane-age-key [(:item-id jane-age) (:item-id jane)]
        result1 (do-add store jane-age-key
                        :template '(nil ("age" tag))
                        :subject-key [(:item-id jane)]
                        :adjacent-key jane-age-key
                        :use-bigger true)
        s1 (:store result1)
        select (:select result1)
        new-jane (description->entity jane-id s1)
        new-element (first (filter #(= (content %) "")
                                   (elements new-jane)))
        [o5 x] (orderable/split order :after)
        [o6 o7] (orderable/split x :before)
        ;; Try with adjacent-group-key
        jane-elements-key [[:parallel
                            []
                            [(:item-id jane-age) (:item-id jane-female)]]
                           jane-id]
        result2 (do-add store jane-age-key
                        :template '(nil ("age" tag))
                        :subject-key [(:item-id jane)]
                        :adjacent-group-key jane-elements-key
                        :use-bigger true)]
    (is (= result2 result1))
    (is (= (canonicalize-list (to-list new-element))
           (canonicalize-list `("" (~o6 :order) ("age" ~'tag (~o7 :order))))))
    (is (= (id->content s1 (:item-id order-entity)) o5))
    (is (= select [[(:item-id new-element) (:item-id jane)]
                   [jane-age-key]]))))

(deftest add-row-handler-test
  (let [mutable-store (new-mutable-store store)
        tracker (new-joe-jane-tracker mutable-store)
        joe-age-dom-id (find-dom-id tracker joe-age)
        joe-age-tag-dom-id (first (filter
                                   #(let [key (get-in @tracker [:id->key %])
                                          leading (first key)]
                                      (and (parallel-referent? leading)
                                           (item-referent?
                                            (first (second leading)))))
                                   (keys (:id->key @tracker))))
        joe-age-key (get-in @tracker [:id->key joe-age-dom-id])
        joe-age-tag-key (get-in @tracker [:id->key joe-age-tag-dom-id])]
    (let [[v-store v-id] (update-add-sibling nil :after store joe-age)
          {:keys [store select]} (add-row-handler
                                  store tracker joe-age-tag-key :after)]
      (is (= store v-store))
      (is (= select [(prepend-to-key v-id (remove-first-primitive-referent
                                           joe-age-key))
                     [joe-age-tag-key]])))))

(deftest delete-handler-test
  (let [mutable-store (new-mutable-store store)
        tracker (new-joe-jane-tracker mutable-store)
        joe-bogus-age-dom-id (find-dom-id tracker joe-bogus-age)
        joe-bogus-age-key (get-in @tracker [:id->key joe-bogus-age-dom-id])]
    (let [new-store (delete-handler store tracker joe-bogus-age-key)]
      (let [joe-age-ids (id-label->element-ids new-store joe-id "age")
            joe-ages (map #(id->content new-store %) joe-age-ids)]
        (is (= (set joe-ages) #{45}))))))

(deftest do-delete-test
  (let [mutable-store (new-mutable-store store)
        tracker (new-joe-jane-tracker mutable-store)
        joe-bogus-age-dom-id (find-dom-id tracker joe-bogus-age)
        joe-age-dom-id (find-dom-id tracker joe-age)
        joe-age-key (get-in @tracker [:id->key joe-age-dom-id])
        joe-bogus-age-key (get-in @tracker [:id->key joe-bogus-age-dom-id])]
    (let [new-store (do-delete store joe-bogus-age-key)
          alt-store (do-delete store joe-age-key
                               :delete-key joe-bogus-age-key)
          joe-age-ids (id-label->element-ids new-store joe-id "age")
          joe-ages (map #(id->content new-store %) joe-age-ids)]
      (is (= new-store alt-store))
      (is (= (set joe-ages) #{45})))))

(deftest set-content-handler-test
  (let [mutable-store (new-mutable-store store)
        mutable-joe (description->entity joe-id mutable-store)
        mutable-joe-male (description->entity (:item-id joe-male)
                                              mutable-store)
        joe-male-tag-key [(elements-referent [nil 'tag])
                          (:item-id joe-male)
                          joe-id]
        mutable-joe-married (description->entity (:item-id joe-married)
                                                 mutable-store)
        joe-married-tag-key [(elements-referent [nil 'tag])
                             (:item-id joe-married)
                             joe-id]
        tracker (new-joe-jane-tracker mutable-store)]
    (is (= (id->content
            (set-content-handler store tracker [joe-id :bob] "Joe" "Jim")
            joe-id)
           "Jim"))
    (is (= (id->content
            (set-content-handler store tracker [joe-id :bob] "Wrong" "Jim")
            joe-id)
           "Joe"))
    ;; Now, try calling it when there is a parallel referent.
    (let [modified (set-content-handler
                    store tracker [[:parallel
                                    [(:item-id joe-age-tag) (:item-id joe-age)]
                                    [joe-id jane-id]]] "age" "oldness")]
      (is (= (id->content modified (:item-id joe-age-tag)) "oldness"))
      (is (= (id->content modified (:item-id jane-age-tag)) "oldness")))
    ;; Try calling it from do-actions.
    (do-actions mutable-store {:tracker tracker}
                {1 [:set-content "joe-root" "Joe" "Fred"]})
    (is (= (current-value (content mutable-joe)) "Fred"))
    ;; Try when an entry is added, also from do-actions.
    (swap! tracker #(-> %
                        (assoc-in [:id->key "test2"]
                                  joe-male-tag-key)
                        (assoc-in [:id->key "test3"]
                                  joe-married-tag-key)
                        (assoc-in [:components joe-married-tag-key
                                   :attributes :add-adjacent]
                                  [joe-id])
                        (assoc-in [:components joe-married-tag-key
                                   :attributes :add-direction]
                                  :before)))
    (do-actions mutable-store {:tracker tracker}
                {2 [:set-content "test2" nil "gender"]})
    (is (= (current-value (semantic-to-list mutable-joe-male))
           ["male" ["gender" 'tag]]))
    ;; Try when the sibling is explicitly specified.
    (let [old-joe-order (current-value (label->content mutable-joe :order))]
      (do-actions mutable-store {:tracker tracker}
                  {3 [:set-content "test3" nil "marital status"]})
      (is (= (current-value (semantic-to-list mutable-joe-married))
             ["married" ["marital status" 'tag]]))
      (let [new-joe-order (current-value (label->content mutable-joe :order))
            marital-status (first (current-value
                                   (label->elements mutable-joe-married 'tag)))
            marital-status-order (current-value
                                  (label->content marital-status :order))]
        (is (orderable/earlier? marital-status-order new-joe-order))
        (is (not= old-joe-order new-joe-order))))))

(deftest do-set-content-test
  (let [ joe-male-tag-key [(elements-referent [nil 'tag])
                          (:item-id joe-male)
                          joe-id]
        joe-married-tag-key [(elements-referent [nil 'tag])
                             (:item-id joe-married)
                             joe-id]]
    (is (= (id->content
            (do-set-content store [joe-id :bob] :from "Joe" :to "Jim")
            joe-id)
           "Jim"))
    (is (= (id->content
            (do-set-content store [joe-id :bob] :from "Wrong" :to "Jim")
            joe-id)
           "Joe"))
    ;; Now, try calling it when there is a parallel referent.
    (let [modified (do-set-content
                    store [[:parallel
                            [(:item-id joe-age-tag) (:item-id joe-age)]
                            [joe-id jane-id]]]
                    :from "age" :to "oldness")]
      (is (= (id->content modified (:item-id joe-age-tag)) "oldness"))
      (is (= (id->content modified (:item-id jane-age-tag)) "oldness")))))

(deftest do-create-content-test
  (let  [joe-male-key [(:item-id joe-male) joe-id]
         joe-male-tag-key (prepend-to-key (elements-referent [nil 'tag])
                                          joe-male-key)
         joe-married-tag-key [(elements-referent [nil 'tag])
                              (:item-id joe-married)
                              joe-id]]
    (let [result (do-create-content store joe-male-tag-key :content "gender")
          s1 (:store result)]
      (is (= (semantic-to-list (description->entity (:item-id joe-male) s1))
           ["male" ["gender" 'tag]])))
    (let [old-joe-order (current-value (label->content joe :order))
          result (do-create-content store joe-married-tag-key
                                    :content "marital status"
                                    :adjacent-key [joe-id]
                                    :position :before)
          s2 (:store result)]
      (let [new-joe (description->entity joe-id s2)
            new-joe-married (description->entity (:item-id joe-married) s2)
            new-joe-order (current-value (label->content new-joe :order))
            marital-status (first (label->elements new-joe-married 'tag))
            marital-status-order (current-value
                                  (label->content marital-status :order))]
        (is (= (semantic-to-list new-joe-married)
             ["married" ["marital status" 'tag]]))
        (is (orderable/earlier? marital-status-order new-joe-order))
        (is (not= old-joe-order new-joe-order))))))

(deftest selected-handler-test
  (let [mutable-store (new-mutable-store store)
        tracker (new-joe-jane-tracker mutable-store)
        joe-dom-id (find-dom-id tracker joe)
        joe-male-dom-id (find-dom-id tracker joe-male)
        jane-age-dom-id (find-dom-id tracker jane-age)
        joe-key (get-in @tracker [:id->key joe-dom-id])
        joe-male-key (get-in @tracker [:id->key joe-male-dom-id])
        jane-age-key (get-in @tracker [:id->key jane-age-dom-id])        
        do-not-merge (new-mutable-set #{})
        session-state {:tracker tracker
                       :do-not-merge do-not-merge}]
    (selected-handler store session-state joe-key)
    (is (= (current-mutable-value do-not-merge) #{joe}))
    (selected-handler store session-state joe-male-key)
    (is (= (current-mutable-value do-not-merge) #{joe joe-male}))
    (selected-handler store session-state jane-age-key)
    (is (= (current-mutable-value do-not-merge) #{jane-age}))
    (selected-handler store session-state "No such id")
    (is (= (current-mutable-value do-not-merge) #{}))))

;;; TODO: remove this once profiling is done.
(defn profile-test
  []
  (let [mutable-store (new-mutable-store store)
        tracker (new-joe-jane-tracker mutable-store)
        joe-dom-id (find-dom-id tracker joe)
        joe-male-dom-id (find-dom-id tracker joe-male)
        jane-age-dom-id (find-dom-id tracker jane-age)
        do-not-merge (new-mutable-set #{})
        session-state {:tracker tracker
                       :do-not-merge do-not-merge}]
    (profile-and-print-reporters (->> (vals (:components @tracker))
                                      (map :reporter)
                                      (filter reporter/reporter?)))))

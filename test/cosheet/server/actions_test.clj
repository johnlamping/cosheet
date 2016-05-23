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
             [query :refer [matching-elements]]
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
                          comment-referent surrogate-referent
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
           `(6 (~o5 :order :non-semantic))))
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
           `(6 (~o5 :order :non-semantic))))
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
    (is (= (canonicalize-list (to-list new-entity))
           (canonicalize-list `(6 (~o7 :order :non-semantic)
                                  ("height" :tag
                                   (~o6 :order :non-semantic))))))
    (is (= order o5))
    (is (= (:item-id new-entity) id)))
  ;; Check that order in the list style entity is preserved in the
  ;; :order values.
  (let [[s id order] (update-add-entity-with-order
                      store joe-id '(6 ("height" :tag) ("weight" :tag))
                      unused-orderable :after false)
        joe (description->entity joe-id s)
        new-entity (first (label->elements joe "height"))
        [x o5] (orderable/split unused-orderable :before)
        [x o6] (orderable/split x :before)
        [o8 o7] (orderable/split x :before)]
    (is (= (canonicalize-list (to-list new-entity))
           (canonicalize-list
            `(6 (~o5 :order :non-semantic)
                ("height" :tag (~o7 :order :non-semantic))
                ("weight" :tag (~o6 :order :non-semantic))))))
    (is (= order o8))
    (is (= (:item-id new-entity) id))))

(deftest adjust-condition-test
  (let [[c1 s1] (adjust-condition '(nil ('??? :a) ('??? 22))
                                  (new-element-store))
        [c2 s2] (adjust-condition '(nil ('??? nil) ('??? "22"))
                                  s1)]
    (is (= c1  '("" ('???-0 :a) ('???-1 22))))
    (is (= c2  '("" ('???-2 "") ('???-3 "22"))))))

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
                        :template '(nil ("age" :tag))
                        :subject-key [(:item-id jane)]
                        :adjacent-key jane-age-key
                        :use-bigger true)
        s1 (:store result1)
        select1 (:select result1)
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
                        :template '(nil ("age" :tag))
                        :subject-key [(:item-id jane)]
                        :adjacent-group-key jane-elements-key
                        :use-bigger true)
        ;; Try with select-key
        result3 (do-add store jane-age-key
                        :template '(nil ("age" :tag))
                        :subject-key [(:item-id jane)]
                        :select-key [(comment-referent "test")
                                     (surrogate-referent)
                                     (:item-id jane)]
                        :use-bigger true)
        select3 (:select result3)]    
    (is (= result2 result1))
    (is (= (canonicalize-list (to-list new-element))
           (canonicalize-list `(""
                                (~o6 :order :non-semantic)
                                ("age" :tag (~o7 :order :non-semantic))))))
    (is (= (id->content s1 (:item-id order-entity)) o5))
    (is (= select1 [[(:item-id new-element) (:item-id jane)]
                    [jane-age-key]]))
    (is (= (:store result1) (:store result3)))
    (is (= select3 [[(comment-referent "test")
                     (:item-id new-element)
                     (:item-id jane)]
                    [jane-age-key]]))))

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

(deftest do-set-content-test
  (let [joe-male-tag-key [(elements-referent [nil :tag])
                          (:item-id joe-male)
                          joe-id]
        joe-married-tag-key [(elements-referent [nil :tag])
                             (:item-id joe-married)
                             joe-id]
        mutable-store (new-mutable-store store)
        mutable-joe (description->entity joe-id mutable-store)]
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
      (is (= (id->content modified (:item-id jane-age-tag)) "oldness")))
    ;; Try calling it from do-actions.
    (let [tracker (new-joe-jane-tracker mutable-store)]
      (swap! tracker assoc-in
             [:components [joe-id :bob] :attributes :commands]
             {:test-command [:do-set-content]})
      (do-actions mutable-store {:tracker tracker}
                  [[:test-command "joe-root" :from "Joe" :to "Fred"]])
      (is (= (current-value (content mutable-joe)) "Fred")))))

(deftest do-create-content-test
  (let  [joe-male-key [(:item-id joe-male) joe-id]
         joe-male-tag-key (prepend-to-key (elements-referent [nil :tag])
                                          joe-male-key)
         joe-married-tag-key [(elements-referent [nil :tag])
                              (:item-id joe-married)
                              joe-id]]
    (let [result (do-create-content store joe-male-tag-key :to "gender")
          s1 (:store result)]
      (is (= (semantic-to-list (description->entity (:item-id joe-male) s1))
           ["male" ["gender" :tag]])))
    (let [old-joe-order (current-value (label->content joe :order))
          result (do-create-content store joe-married-tag-key
                                    :to "marital status"
                                    :adjacent-key [joe-id]
                                    :position :before)
          s2 (:store result)]
      (let [new-joe (description->entity joe-id s2)
            new-joe-married (description->entity (:item-id joe-married) s2)
            new-joe-order (current-value (label->content new-joe :order))
            marital-status (first (label->elements new-joe-married :tag))
            marital-status-order (current-value
                                  (label->content marital-status :order))]
        (is (= (semantic-to-list new-joe-married)
             ["married" ["marital status" :tag]]))
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
    (selected-handler store session-state joe-dom-id)
    (is (= (current-mutable-value do-not-merge) #{joe}))
    (selected-handler store session-state joe-male-dom-id)
    (is (= (current-mutable-value do-not-merge) #{joe joe-male}))
    (selected-handler store session-state jane-age-dom-id)
    (is (= (current-mutable-value do-not-merge) #{jane-age}))
    (selected-handler store session-state "No such id")
    (is (= (current-mutable-value do-not-merge) #{}))))

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

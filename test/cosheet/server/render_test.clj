(ns cosheet.server.render-test
  (:require [clojure.test :refer [deftest is assert-expr do-report]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [utils :refer [multiset-diff]]
             [orderable :as orderable]
             [mutable-set :refer [new-mutable-set mutable-set-swap!]]
             [entity :as entity  :refer [to-list description->entity
                                         label->elements]]
             [reporters :as reporter]
             [expression :refer [expr expr-let expr-seq]]
             [debug :refer [current-value envs-to-list simplify-for-print]]
             [expression-manager :refer [new-expression-manager-data
                                         request compute]] 
             [expression-manager-test :refer [check-propagation]]
             entity-impl
             [store :refer [new-element-store id->content id->subject
                            current-store]]
             store-impl
             [store-utils :refer [add-entity]]
             mutable-store-impl
             [dom-utils :refer [dom-attributes]]
             [test-utils :refer [check any as-set evals-to let-mutated]])
            (cosheet.server
             [key :refer [item-referent comment-referent parallel-referent
                          content-location-referent query-referent
                          key-referent content-referent elements-referent
                          canonicalize-list prepend-to-key
                          item-referent? parallel-referent? key-referent?]]
             [render :refer :all])
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
(def jane-list `("Jane" (~o1 :order :non-semantic) "plain" "plain"))
(def joe-list `("Joe"
               (~o2 :order :non-semantic)
               ("male" (~o1 :order :non-semantic))
               ("married" (~o2 :order :non-semantic))
               (39 (~o3 :order :non-semantic)
                   ("age" :tag)
                   ("doubtful" "confidence"))
               (45 (~o4 :order :non-semantic)
                   ("age" :tag))))

(defn get-dom-keys
  ;; TODO: Fix this to get the keys out of :commands too.
  "Return all keys in the dom"
  [dom]
  (when (sequential? dom)
    (concat (vals (select-keys (dom-attributes dom)
                               [:key]))
            (when (= (first dom) :div)
              (mapcat get-dom-keys (rest dom))))))

(defn valid-key?
  "Return true if the key is well formed,
  in particular that its items follow the subject/content chain."
  [key expected-item-id immutable-store]
  (or
   (empty? key)
   (let [[first-ref & remainder] key]
      (cond
        (keyword? first-ref)
        (valid-key? remainder expected-item-id immutable-store)
        (item-referent? first-ref)
        (and (or (not expected-item-id)
                 (if (= (first expected-item-id) :container)
                   (= (id->content immutable-store first-ref)
                      (second expected-item-id))
                   (= first-ref expected-item-id)))
             (valid-key? remainder
                           (or (id->subject immutable-store first-ref)
                               [:container first-ref])
                           immutable-store))
        (key-referent? first-ref)
        (let [[tag key] first-ref]
          (valid-key?
           (vec (concat key remainder)) expected-item-id immutable-store))
        (parallel-referent? first-ref)
        (and (not expected-item-id)
             (let [[tag exemplar item-ids] first-ref]
               (and (valid-key? exemplar expected-item-id immutable-store)
                    (every? 
                     #(valid-key? (into [%] remainder) nil immutable-store)
                     item-ids))))
        true
        (valid-key? remainder expected-item-id immutable-store)))))

(defn invalid-keys
  "Return all the keys in the dom that are not well formed.
  The item must have a store where the ids in the keys can be looked up."
  [dom item]
  (let [store (current-store (:store item)) ]
    (remove #(valid-key? % nil store)
            (get-dom-keys dom))))

;;; Used in (is (check <value> <pattern>))
;;; Handled by the method on assert-expr.
(def check-keys) 

(defmethod assert-expr 'check-keys [msg form]
  (let [[pred dom item] form]
    `(let [dom-value# ~dom
           item-value# ~item
           result# (invalid-keys dom-value# item-value#)]
       (if (empty? result#)
         (do-report {:type :pass, :message ~msg,
                     :expected '~form,
                     :actual (list ~pred dom-value# item-value#)})
         ;; A non-empty result indicates a failure, and describes it.
         (do-report {:type :fail, :message ~msg,
                     :expected '~form, :actual result#}))
       result#)))

(deftest condition-satisfiers-test
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers test '(nil :a)))))
             [:a]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers test '(nil :a :c)))))
             (as-set [:a :c])))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers test '(nil :a :b)))))
             [:a]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers test '(nil :a :b)))))
             [:a]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers test '(nil :a :a :b)))))
             [:a :a]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers test '(nil :a :a :b)))))
             [:a])))

(deftest canonical-info-set-test
  (is (= (let-mutated [joe joe-list, jane jane-list]
           (canonical-info-set [joe jane]))
         {["Joe"
           {"married" 1
            "male" 1
            [39 {["age" {:tag 1}] 1, ["doubtful" {"confidence" 1}] 1}] 1
            [45 {["age" {:tag 1}] 1}] 1}]1
            ["Jane" {"plain" 2}] 1})))

(deftest canonical-to-list-test
  (let [starting [joe-list jane-list jane-list]
        canonical (canonicalize-list [starting])]
    (is (= (canonicalize-list (canonical-to-list canonical))
           canonical))))

(deftest canonical-info-set-diff-test
  (is (= (multiset-diff {:a 1 :b 3 :c 3} {:b 1 :c 3 :d 4})
         [{:a 1 :b 2} {:d 4} {:b 1 :c 3}])))

(deftest append-to-hierarchy-test
  ;; Append to empty.
  (is (check (append-to-hierarchy [] {:a 1} :i true)
             [{:hierarchy-node true :properties {:a 1} :members [:i]}]))
  ;; Identical properties.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1}
                                    :members [:i]}]
                                  {:a 1} :j true)
             [{:hierarchy-node true :properties {:a 1} :members [:i :j]}]))
  ;; Completely different properties.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1}
                                    :members [:i]}]
                                  {:b 1} :j true)
             [{:hierarchy-node true :properties {:a 1} :members [:i]}
              {:hierarchy-node true :properties {:b 1} :members [:j]}]))
  ;; New element has added group.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1}
                                    :members [:i]}]
                                  {:a 1 :b 1} :j true)
             [{:hierarchy-node true
               :properties {:a 1}
               :members [:i]
               :children [{:hierarchy-node true
                           :properties {:b 1}
                           :members [:j]}]}]))
  ;; New element has fewer properties.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1 :b 1}
                                    :members [:i]}]
                                  {:a 1} :j true)
             [{:hierarchy-node true :properties {:a 1}
               :members []
               :children [{:hierarchy-node true
                           :properties {:b 1}
                           :members [:i]}
                          {:hierarchy-node true
                           :properties {},
                           :members [:j]}]}]))
  ;; Old and new have some in common.
  (is (check
       (append-to-hierarchy [{:hierarchy-node true
                              :properties {:a 1 :b 1}
                              :members [:i]}]
                            {:a 1 :c 1} :j true)
       [{:hierarchy-node true
         :properties {:a 1}
         :members []
         :children [{:hierarchy-node true :properties {:b 1} :members [:i]}
                    {:hierarchy-node true :properties {:c 1} :members [:j]}]}]))
  ;; Must add to child of last element.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1}
                                    :members [:i]
                                    :children [{:hierarchy-node true
                                                :properties {:b 1}
                                                :members [:j]}]}]
                                  {:a 1 :b 1 :c 1} :k true)
             [{:hierarchy-node true
               :properties {:a 1}
               :members [:i]
               :children [{:hierarchy-node true
                           :properties {:b 1}
                           :members [:j]
                           :children [{:hierarchy-node true
                                       :properties {:c 1}
                                       :members [:k]}]}]}]))
  ;; Must close off child of last element.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1}
                                    :members [:i]
                                    :children [{:hierarchy-node true
                                                :properties {:b 1}
                                                :members [:j]}]}]
                                  {:a 1} :k true)
             [{:hierarchy-node true
               :properties {:a 1}
               :members [:i]
               :children [{:hierarchy-node true
                           :properties {:b 1}
                           :members [:j]}
                          {:hierarchy-node true
                           :properties {}
                           :members [:k]}]}]))
  ;; Partial sharing with last element that has children.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1 :b 1}
                                    :members [:i]
                                    :children [{:properties {:c 1}
                                                :members [:j]}]}]
                                  {:a 1 :c 1} :k true)
             [{:hierarchy-node true
               :properties {:a 1}
               :members []
               :children [{:hierarchy-node true
                           :properties {:b 1}
                           :members [:i]
                           :children [{:properties {:c 1} :members [:j]}]}
                          {:hierarchy-node true
                           :properties {:c 1}
                           :members [:k]}]}]))
  ;; Append two empty properties
  (is (check
       (append-to-hierarchy [{:hierarchy-node true
                              :properties {}
                              :members [:i]}]
                            {} :j true)
       [{:hierarchy-node true :properties {} :members [:i]}
        {:hierarchy-node true  :properties {} :members [:j]}]))
  ;; Append empty group after empty group while nested.
  (is (check
       (append-to-hierarchy [{:hierarchy-node true
                              :properties {:a 1 :b 1}
                              :members [:i]
                              :children [{:hierarchy-node true
                                          :properties {:c 1}
                                          :members [:j]}
                                         {:hierarchy-node true
                                          :properties {}
                                          :members [:k]}]}]
                            {:a 1 :b 1} :l true)
       [{:hierarchy-node true
         :properties {:a 1 :b 1}
         :members [:i]
         :children [{:hierarchy-node true :properties {:c 1} :members [:j]}
                    {:hierarchy-node true :properties {} :members [:k :l]}]}]))
  ;; Append non-empty group after empty group.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1 :b 1}
                                    :members [:i]
                                    :children [{:hierarchy-node true
                                                :properties {:c 1}
                                                :members [:j]}
                                               {:hierarchy-node true
                                                :properties {}
                                                :members [:k]}]}]
                                  {:a 1 :b 1 :c 1} :l true)
             [{:hierarchy-node true
               :properties {:a 1 :b 1}
               :members [:i]
               :children [{:hierarchy-node true :properties {:c 1} :members [:j]}
                          {:hierarchy-node true :properties {} :members [:k]}
                          {:hierarchy-node true :properties {:c 1} :members [:l]}]
               }])))

(deftest split-by-do-not-merge-subset-test
  (is (check (split-by-do-not-merge-subset
              (map (fn [x] {:item x}) [:i :j :k :l :m :n :o])
              #{:i :l :m :o})
             [[{:item :i}]
              [{:item :j} {:item :k}]
              [{:item :l}]
              [{:item :m}]
              [{:item :n}]
              [{:item :o}]])))

(deftest hierarchy-by-canonical-info-test
  (is (check
       (hierarchy-by-canonical-info
        [{:property-canonicals [:a] :item `(:i (~o1 :order :non-semantic))}
         {:property-canonicals [:a :b] :item `(:j (~o2 :order :non-semantic))}
         {:property-canonicals [:a :c] :item `(:k (~o3 :order :non-semantic))}]
        #{})
       [{:hierarchy-node true
         :properties {:a 1}
         :members [{:property-canonicals [:a]
                    :item `(:i (~o1 :order :non-semantic))}]
         :children [{:hierarchy-node true
                     :properties {:b 1}
                     :members [{:property-canonicals [:a :b]
                                :item `(:j (~o2 :order :non-semantic))}]}
                    {:hierarchy-node true
                     :properties {:c 1}
                     :members [{:property-canonicals [:a :c]
                                :item `(:k (~o3 :order :non-semantic))}]}]}])))

(deftest hierarchy-node-descendants-test
  (is (check (set (hierarchy-node-descendants
                   {:hierarchy-node true
                    :properties {:a 1}
                    :members [:i]
                    :children [{:properties {:b 1} :members [:j]}]}))
             #{:i :j})))

(deftest hierarchy-node-next-level-test
  (is (check (hierarchy-node-next-level
              {:hierarchy-node true
               :properties {:a 1 :b 1}
               :members [:i]
               :children [{:hierarchy-node true :properties {:c 1} :members [:j :k]}
                          {:hierarchy-node true :properties {} :members [:l :m ]}]})
             [:i
              {:hierarchy-node true :properties {:c 1} :members [:j :k]}
              :l :m])))

(deftest flatten-hierarchy-test
  (is (check (flatten-hierarchy
              [{:hierarchy-node true
                :properties {:a 1}
                :members [:i]
                :children [{:hierarchy-node true
                            :properties {:b 1}
                            :members [:j]
                            :children [{:hierarchy-node true
                                        :properties {:c 1}
                                        :members [:l]}]}
                           {:hierarchy-node true
                            :properties {:c 1}
                            :members [:k]}]}]
              0 {})
             [{:hierarchy-node true
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :depth 0
               :members [:i]
               :children [{:hierarchy-node true
                           :properties {:b 1}
                           :members [:j]
                           :children [{:hierarchy-node true
                                       :properties {:c 1}
                                       :members [:l]}]}
                          {:hierarchy-node true
                           :properties {:c 1}
                           :members [:k]}]}
              {:hierarchy-node true
               :properties {:b 1}
               :cumulative-properties {:a 1 :b 1}
               :depth 1
               :members [:j]
               :children [{:hierarchy-node true
                           :properties {:c 1}
                           :members [:l]}]}
              {:hierarchy-node true
               :properties {:c 1}
               :cumulative-properties {:a 1 :b 1 :c 1}
               :depth 2 :members [:l]}
              {:hierarchy-node true
               :properties {:c 1}
               :cumulative-properties {:a 1 :c 1}
               :depth 1
               :members [:k]}])))

(deftest multiset-to-generating-values-test
  (is (check (multiset-to-generating-values
               {:a 1 :b 2} [:a1 :a2 :a3 :b1 :b2 :b3] [:a :a :a :b :b :b])
             (as-set [:a3 :b2 :b3]))))

(deftest add-row-header-border-info-test
  (is (check (add-row-header-border-info
              [{:depth 0}
               {:depth 1}
               {:depth 2}
               {:depth 2}
               {:depth 1}])
             [{:depth 0 :top-border :full :with-children true}
              {:depth 1 :top-border :indented :with-children true}
              {:depth 2 :top-border :indented}
              {:depth 2 :top-border :indented}
              {:depth 1 :top-border :indented :bottom-border :corner}])))

(deftest flattened-items-hierarchy-test
  (let [joe (let-mutated [joe joe-list] joe)
        gender (first (current-value (label->elements joe o1)))
        bogus-age (first (current-value
                          (label->elements joe "doubtful")))
        age (first (remove #{bogus-age}
                           (current-value
                            (label->elements joe "age"))))
        bogus-age-tag (first (current-value
                              (label->elements bogus-age :tag)))
        age-tag (first (current-value (label->elements age :tag)))]
    (is (check
         (let [hierarchy (current-value
                          (items-hierarchy-by-condition
                           [gender age bogus-age] #{} '(nil :tag)))]
           (flatten-hierarchy-add-row-header-border-info hierarchy))
         [{:depth 0 :top-border :full :bottom-border :corner
           :hierarchy-node true
           :properties {}
           :cumulative-properties {}
           :members [{:item gender, :property-elements '() :property-canonicals nil}]}
          {:depth 0 :for-multiple true :top-border :full :bottom-border :full
           :hierarchy-node true
           :properties {["age" {:tag 1}] 1}
           :cumulative-properties {["age" {:tag 1}] 1}
           :members [{:item bogus-age
                      :property-elements [bogus-age-tag]
                      :property-canonicals [["age" {:tag 1}]]}
                     {:item age
                      :property-elements [age-tag]
                      :property-canonicals [["age" {:tag 1}]]}]}]))))

(def t1 (add-entity (new-element-store) nil 'joe))
(def store (first t1))
(def rid :foo)
(def root (description->entity rid store))

(deftest row-header-elements-DOM-test
  (is (check (row-header-elements-DOM {:depth 0 :is-tags true}
                                      nil '(nil :tag) [rid] {})
             [:div {:class "editable full-row tags-column"
                    :key [[:elements [nil :tag]] [:comment [nil :tag]] rid]
                    :commands {:set-content [:do-create-content]
                               :add-row [:do-add :subject-key nil
                                         :adjacent-group-key [rid]]}}]))
  (let [[dom fred fred-tag]
        (let-mutated [fred '("Fred" :tag)]
          (expr-let [dom (row-header-elements-DOM {:depth 1
                                                   :is-tags true
                                                   :bottom-border :indented
                                                   :for-multiple true
                                                   :with-children true}
                                                  [fred] '(nil :tag) [rid]
                                                  {:depth 0})
                     fred-elements (entity/elements fred)]
            [dom fred (first fred-elements)]))]
    (is (check-keys dom fred))
    (is (check
         dom
         [:div {:class "indent-wrapper tags-column"}
          [:div {:class "vertical-center-wrapper full-row bottom-border with-children for-multiple depth-1"}
           [:component {:key [(:item-id fred) [:comment [nil :tag]] rid]}
            [item-DOM
             fred [[:comment [nil :tag]] rid]
             #{fred-tag}
             {:commands {:add-sibling [:do-add :template '(nil :tag)]
                         :add-row [:do-add :subject-key nil
                                   :adjacent-group-key [rid]]}}
             {:depth 0}]]]])))
  (let [[dom fred fran]
        (let-mutated [fred '("Fred" :tag)
                      fran "Fran"]
          (expr-let [dom (row-header-elements-DOM
                          {:depth 0
                           :is-tags true
                           :top-border :full
                           :bottom-border :corner}
                          [fred fran] '(nil :tag) [rid]
                          {:depth 1 :do-not-merge #{}})]
            [dom fred fran]))
        fred-tag (first (current-value (entity/elements fred)))]
    (is (check-keys dom fred))
    (is (check
         dom
         [:div
          {:class "vertical-center-wrapper full-row tags-column top-border ll-corner"
           :key [[:elements [nil :tag]] [:comment [nil :tag]] rid]
           :commands {:add-row [:do-add :subject-key nil
                                :adjacent-group-key [rid]]}}
          [:div {:class "stack"}
           [:component {:key [(:item-id fred) [:comment [nil :tag]] rid]
                        :class "vertical-separated"}
            [item-DOM
             fred [[:comment [nil :tag]] rid]
             #{fred-tag}
             {:commands {:add-sibling [:do-add :template '(nil :tag)]
                         :add-row [:do-add :subject-key nil
                                   :adjacent-group-key [rid]]}}
             {:depth 1 :do-not-merge #{}}]]
           [:component {:key [(:item-id fran) [:comment [nil :tag]] rid]
                        :class "vertical-separated"}
            [item-DOM
             fran [[:comment [nil :tag]] rid]
             #{}
             {:commands {:add-sibling [:do-add :template '(nil :tag)]
                         :add-row [:do-add :subject-key nil
                                   :adjacent-group-key [rid]]}}
             {:depth 1 :do-not-merge #{}}]]]]))))

(deftest item-DOM-test
  (let [[dom fred]
        (let-mutated [fred "Fred"]
          (expr-let [dom (item-DOM fred [] #{} {}
                                   {:depth 0 :do-not-merge #{}})]
            [dom fred]))]
    (is (check-keys dom fred))
    (is (check dom
               [:div {:class "content-text editable item"
                      :key [(:item-id fred)]
                      :commands {:set-content [:do-set-content]
                                 :add-element [:do-add
                                               :subject-key [(:item-id fred)]]
                                 :delete [:do-delete]}} "Fred"])))
  ;; Check generation of a single tag for a single item.
  (let [[dom age]
        (let-mutated [age `(39 ("doubtful"
                                ("confidence" :tag)
                                (~o1 :order :non-semantic)))]
          (expr-let [dom (item-DOM
                          age [:age] #{} {} {:depth 0 :do-not-merge #{}})]
            [dom age]))
        doubtful (first (current-value (label->elements age o1)))
        confidence (first (current-value
                           (label->elements doubtful :tag)))
        confidence-tag (first (current-value (entity/elements confidence)))
        item-key [(item-referent age) :age]
        tag-key (into [[:comment [nil :tag]] (:item-id doubtful)] item-key)]
    (is (check-keys dom age))
    (is (check
         dom
         [:div {:class "item with-elements" :key [(item-referent age) :age]}
          [:div {:class "content-text editable"
                 :key [(content-location-referent) (item-referent age) :age]
                 :commands {:set-content [:do-set-content]
                            :delete [:do-delete]
                            :add-element [:do-add
                                          :subject-key
                                          [(item-referent age) :age]]}}
           "39"]
          [:div {:class "element-table"}
           [:div {:class "element-row last-row"}
            [:div {:class "vertical-center-wrapper full-row tags-column top-border bottom-border"}
             [:component {:key (into [(item-referent confidence)] tag-key)}
              [item-DOM
               confidence tag-key
               #{confidence-tag}
               {:commands {:add-sibling [:do-add :template '(nil :tag)]
                           :add-row [:do-add
                                     :subject-key item-key
                                     :adjacent-group-key
                                     (into [(:item-id doubtful)]
                                           item-key)]}}
               {:depth 1 :do-not-merge #{}}]]]
            [:component {:key (into [(:item-id doubtful)] item-key)
                         :class "elements-column"}
             [item-DOM
              doubtful item-key
              #{confidence}
              {:commands {:add-sibling [:do-add
                                        :template '(nil ("confidence" :tag))]
                          :add-row [:do-add]}}
              {:depth 1 :do-not-merge #{}}]]]]])))
  ;; Check that we generate no-tags.
  (let [[dom age]
        (let-mutated [age `(39 ("doubtful" (~o1 :order :non-semantic)))]
          (expr-let [dom (item-DOM
                          age [:age] #{} {} {:depth 0 :do-not-merge #{}})]
            [dom age]))
        doubtful (first (current-value (label->elements age o1)))
        item-key [(item-referent age) :age]]
    (is (check-keys dom age))
    (is (check dom
           [:div {:class "item with-elements" :key item-key}
            (any vector?)
            [:div (any map?)
             [:div {:class "element-row no-tags last-row"}
              [:div {:class "editable full-row tags-column top-border bottom-border"
                     :key (into [[:elements [nil :tag]]
                                 [:comment [nil :tag]]
                                 (:item-id doubtful)] item-key)
                     :commands {:set-content [:do-create-content]
                                :add-row [:do-add
                                          :subject-key item-key
                                          :adjacent-group-key
                                          (into [(:item-id doubtful)]
                                                item-key)]}}]
              [:component {:key (into [(:item-id doubtful)] item-key)
                           :class "elements-column"}
               [item-DOM
                doubtful item-key
                #{}
                {:commands {:add-sibling [:do-add :template '(nil)]
                            :add-row [:do-add]}}
                {:depth 1 :do-not-merge #{}}]]]]])))
  ;; Test added elements, and a mutable set for do-not-merge
  (let [do-not-merge (new-mutable-set #{})
        [dom-reporter joe]
        (let-mutated [joe joe-list]
          (expr identity
            [(item-DOM joe [:joe] #{} {}  {:depth 0 :do-not-merge do-not-merge})
             joe]))
        male (first (current-value (label->elements joe o1)))
        married (first (current-value (label->elements joe o2)))
        bogus-age (first (current-value
                          (label->elements joe "doubtful")))
        bogus-age-tag (first (current-value
                              (label->elements bogus-age :tag)))
        bogus-age-tag-spec (first (current-value
                                   (entity/elements bogus-age-tag)))
        age (first (remove #{bogus-age}
                           (current-value
                            (label->elements joe "age"))))
        age-tag (first (current-value (label->elements age :tag)))
        age-tag-spec (first (current-value (entity/elements age-tag)))]
    (let [md (new-expression-manager-data)
          item-key [(item-referent joe) :joe]
          both-ages-ref [:parallel [] [(:item-id bogus-age) (:item-id age)]]
          both-ages-key (into [both-ages-ref] item-key)]
      (request dom-reporter md)
      (compute md)
      (check-propagation dom-reporter)
      (is (check-keys (reporter/value dom-reporter) joe))
      (is (check
           (reporter/value dom-reporter)
           [:div {:class "item with-elements" :key item-key}
            (any vector?)
            [:div (any map?)
             (any vector?) ; male
             (any vector?) ; married
             [:div {:class "element-row last-row"}
              [:div {:class (any string?)}
               [:component
                {:key  (->> both-ages-key
                            (prepend-to-key (comment-referent '(nil :tag)))
                            (prepend-to-key (item-referent bogus-age-tag)))}
                [item-DOM
                 bogus-age-tag
                 (prepend-to-key [:comment [nil :tag]] both-ages-key)
                 #{bogus-age-tag-spec}
                 {:commands {:add-sibling [:do-add :template '(nil :tag)]
                             :add-row [:do-add
                                       :subject-key item-key
                                       :adjacent-group-key both-ages-key]}}
                 (any map?)]]]
              [:div (any map?)
               [:component (any map?)
                [item-DOM bogus-age item-key #{bogus-age-tag}
                 {:commands {:add-sibling [:do-add
                                           :template '(nil ("age" :tag))]
                             :add-row [:do-add]}}
                 (any map?)]]
               [:component (any map?)
                [item-DOM age item-key #{age-tag}
                 {:commands {:add-sibling [:do-add
                                           :template '(nil ("age" :tag))]
                             :add-row [:do-add]}}
                 (any map?)]]]]]]))
      ;; Now, make the do-not-merge be non-trivial
      (mutable-set-swap! do-not-merge (fn [old] #{age}))
      (compute md)
      (check-propagation dom-reporter)
      (is (check-keys (reporter/value dom-reporter) joe))
      (is (check
           (reporter/value dom-reporter)
           [:div {:class "item with-elements" :key item-key}
            (any vector?)
            [:div (any)
             (any vector?) ; male
             (any vector?) ; married
             [:div {:class "element-row"}
              [:div (any map?)
               [:component (any map?)
                [item-DOM
                 bogus-age-tag (into [[:comment [nil :tag]]
                                      (:item-id bogus-age)]
                                     item-key)
                 #{bogus-age-tag-spec}
                 {:commands {:add-sibling [:do-add :template '(nil :tag)]
                             :add-row [:do-add
                                       :subject-key item-key
                                       :adjacent-group-key
                                       (into [(:item-id bogus-age)] item-key)]}}
                 (any map?)]]]
              [:component (any map?)
               [item-DOM
                bogus-age item-key
                #{bogus-age-tag}
                {:commands {:add-sibling [:do-add
                                          :template '(nil ("age" :tag))]
                            :add-row [:do-add]}}
                (any map?)]]]
             [:div {:class "element-row last-row"}
              [:div (any map?)
               [:component (any map?)
                [item-DOM
                 age-tag (into [[:comment [nil :tag]]
                                (:item-id age)]
                               item-key)
                 #{age-tag-spec}
                 {:commands {:add-sibling [:do-add :template '(nil :tag)]
                             :add-row [:do-add
                                       :subject-key item-key
                                       :adjacent-group-key
                                       (into [(:item-id age)] item-key)]}}
                 (any map?)]]]
              [:component (any map?)
               [item-DOM
                age item-key
                #{age-tag}
                {:commands {:add-sibling [:do-add
                                          :template '(nil ("age" :tag))]
                            
                            :add-row [:do-add]}}
                (any map?)]]]]]
           ))))
  ;; Test a hierarchy.
  (let [[dom-reporter joe]
        (let-mutated [joe `("Joe"
                            (~o2 :order :non-semantic)
                            ("1" (~o1 :order :non-semantic)
                                 ("L1" :tag))
                            (12 (~o2 :order :non-semantic)
                                ("L1" :tag (~o1 :order :non-semantic))
                                ("L2" :tag (~o2 :order :non-semantic)))
                            (13 (~o3 :order :non-semantic)
                                ("L1" :tag (~o1 :order :non-semantic))
                                ("L3" :tag (~o2 :order :non-semantic))))]
          (expr identity
            [(item-DOM joe [:joe] #{} {}  {:depth 0 :do-not-merge #{}}) joe]))
        v1 (first (current-value (label->elements joe o1)))
        v12 (first (current-value (label->elements joe o2)))
        v13 (first (current-value (label->elements joe o3)))
        L1 (first (current-value (label->elements v1 :tag)))
        L1-spec (first (current-value (entity/elements L1)))
        L121 (first (current-value (label->elements v12 o1)))
        L2 (first (current-value (label->elements v12 o2)))
        L2-spec (first (remove #{(first (current-value
                                         (label->elements L2 :order)))}
                               (current-value (entity/elements L2))))
        L131 (first (current-value (label->elements v13 o1)))
        L3 (first (current-value (label->elements v13 o2)))
        L3-spec (first (remove #{(first (current-value
                                         (label->elements L3 :order)))}
                               (current-value (entity/elements L3))))]
    (let [md (new-expression-manager-data)]
      (request dom-reporter md)
      (compute md))
    (check-propagation dom-reporter)
    (is (check-keys (reporter/value dom-reporter) joe))
    (is (check
         (reporter/value dom-reporter)
         (let [item-key [(item-referent joe) :joe]
               both-ages-ref [:parallel
                              []
                              [(:item-id v1) (:item-id v12) (:item-id v13)]]
               both-ages-key (into [both-ages-ref] item-key) ]
           [:div {:class "item with-elements" :key item-key}
            (any vector?)
            [:div (any map?)
             [:div {:class "element-row"}
              [:div {:class "vertical-center-wrapper full-row with-children tags-column top-border"}
               [:component
                {:key (->> both-ages-key
                           (prepend-to-key (comment-referent '(nil :tag)))
                           (prepend-to-key (item-referent L1)))}
                [item-DOM L1
                 (prepend-to-key [:comment [nil :tag]] both-ages-key)
                 #{L1-spec}
                 {:commands {:add-sibling [:do-add :template '(nil :tag)]
                             :add-row [:do-add
                                       :subject-key item-key
                                       :adjacent-group-key
                                       [[:parallel []
                                         (as-set [(:item-id v1)
                                                  (:item-id v12)
                                                  (:item-id v13)])]
                                        (item-referent joe)
                                        :joe]]}}
                 (any map?)]]]
              [:component {:key (into [(:item-id v1)] item-key)
                           :class "elements-column"}
               [item-DOM v1 item-key #{L1}
                {:commands {:add-sibling [:do-add :template [nil ["L1" :tag]]]
                            :add-row [:do-add]}}
                (any map?)]]]
             [:div {:class "element-row"}
              [:div {:class "indent-wrapper tags-column"}
               [:div {:class "vertical-center-wrapper full-row top-border depth-1"}
                [:component
                 {:key (into [(:item-id L2)
                              [:comment [nil :tag]]
                              (:item-id v12)]
                             item-key)}
                 [item-DOM
                  L2 (into [[:comment [nil :tag]] (:item-id v12)]
                           item-key)
                  #{L2-spec}
                  {:commands {:add-sibling [:do-add :template '(nil :tag)]
                              :add-row [:do-add
                                        :subject-key item-key
                                        :adjacent-group-key
                                        (into [(:item-id v12)]
                                              item-key)]}}
                  (any map?)]]]]
              [:component {:key (into [(:item-id v12)] item-key)
                           :class "elements-column"}
               [item-DOM v12 item-key #{L121 L2}
                {:commands {:add-sibling [:do-add
                                          :template (as-set [nil
                                                             ["L1" :tag]
                                                             ["L2" :tag]])]
                            :add-row [:do-add]}}
                (any map?)]]]
             [:div {:class "element-row last-row"}
              [:div {:class "indent-wrapper tags-column bottom-border"}
               [:div {:class "vertical-center-wrapper full-row top-border depth-1"}
                [:component
                 {:key (into [(:item-id L3)
                              [:comment [nil :tag]]
                              (:item-id v13)]
                             item-key)}
                 [item-DOM
                  L3 (into [[:comment [nil :tag]] (:item-id v13)]
                           item-key)
                  #{L3-spec}
                  {:commands {:add-sibling [:do-add :template '(nil :tag)]
                              :add-row [:do-add
                                        :subject-key item-key
                                        :adjacent-group-key
                                        (into [(:item-id v13)] item-key)]}}
                  (any map?)]]]]
              [:component {:key (into [(:item-id v13)] item-key)
                           :class "elements-column"}
               [item-DOM
                v13 item-key #{L131 L3}
                {:commands {:add-sibling [:do-add
                                          :template (as-set [nil
                                                             ["L3" :tag]
                                                             ["L1" :tag]])]
                            :add-row [:do-add]}}
                (any map?)]]]]]))))
  ;; Test a hierarchy with an empty content in one row and an empty
  ;; tag in another.
  (let [[dom-reporter joe]
        (let-mutated [joe `("Joe"
                            (~o2 :order :non-semantic)
                            ("a" (~o2 :order :non-semantic)
                             ("L1" :tag (~o1 :order :non-semantic))
                             ("L2" :tag (~o2 :order :non-semantic)))
                            ("b" (~o3 :order :non-semantic)
                             ("L1" :tag (~o1 :order :non-semantic))))]
          (expr identity
            [(item-DOM joe [rid] #{} {} {:depth 0 :do-not-merge #{}}) joe]))
        va (first (current-value (label->elements joe o2)))
        vb (first (current-value (label->elements joe o3)))
        La1 (first (current-value (label->elements va o1)))
        La2 (first (current-value (label->elements va o2)))
        La1-spec (first (remove #{(first (current-value
                                         (label->elements La1 :order)))}
                                (current-value (entity/elements La1))))
        La2-spec (first (remove #{(first (current-value
                                         (label->elements La2 :order)))}
                               (current-value (entity/elements La2))))
        Lb1 (first (current-value (label->elements vb o1)))
        Lb1-spec (first (remove #{(first (current-value
                                          (label->elements Lb1 :order)))}
                                (current-value (entity/elements Lb1))))]
    (let [md (new-expression-manager-data)]
      (request dom-reporter md)
      (compute md))
    (check-propagation dom-reporter)
    (is (check-keys (reporter/value dom-reporter) joe))
    (is (check
         (reporter/value dom-reporter)
         (let [item-key [(item-referent joe) rid]
               both-L1s-ref [:parallel [] [(:item-id va) (:item-id vb)]]
               both-L1s-key (into [both-L1s-ref] item-key)
               L1s-ref [:parallel
                        [(:item-id La1) [:comment [nil :tag]]]
                        [(:item-id va) (:item-id vb)]]]
           [:div {:class "item with-elements" :key item-key}
            (any vector?)
            [:div (any map?)
             [:div {:class "element-row"}
              [:div {:class "vertical-center-wrapper full-row with-children tags-column top-border"}
               [:component
                {:key (->> both-L1s-key
                           (prepend-to-key [:comment [nil :tag]])
                           (prepend-to-key (:item-id La1)))}
                [item-DOM La1 (prepend-to-key [:comment [nil :tag]] both-L1s-key)
                 #{La1-spec}
                 {:commands {:add-sibling [:do-add :template '(nil :tag)]
                             :add-row [:do-add :subject-key item-key
                                       :adjacent-group-key both-L1s-key]}}
                 (any map?)]]]
              [:div {:key (into [[:elements [nil ["L1" :tag]]]] item-key)
                     :class "editable elements-column"
                     :commands {:set-content [:do-create-content
                                              :position :before
                                              :adjacent-group-key
                                              (into [(:item-id va)] item-key)]
                                :add-row [:do-add :subject-key item-key
                                          :position :before
                                          :adjacent-key
                                          (into [(:item-id va)] item-key)]}}]]
             [:div {:class "element-row"}
              [:div {:class "indent-wrapper tags-column"}
               [:div {:class "vertical-center-wrapper full-row top-border depth-1"}
                [:component
                 {:key (into [(:item-id La2)
                              [:comment [nil :tag]]
                              (:item-id va)]
                             item-key)}
                 [item-DOM
                  La2 (into [[:comment [nil :tag]]
                             (:item-id va)]
                            item-key)
                  #{La2-spec}
                  {:commands {:add-sibling [:do-add :template '(nil :tag)]
                              :add-row [:do-add :subject-key item-key
                                        :adjacent-group-key
                                        (into [(:item-id va)] item-key)]}}
                  (any map?)]]]]
              [:component {:key (into [(:item-id va)] item-key)
                           :class "elements-column"}
               [item-DOM va item-key #{La1 La2}
                {:commands {:add-sibling
                            [:do-add :template (as-set [nil
                                                       ["L2" :tag]
                                                        ["L1" :tag]])]
                            :add-row [:do-add]}}
                (any map?)]]]
             [:div {:class "element-row last-row"}
              [:div {:class "indent-wrapper tags-column bottom-border"
                     :commands {:add-row [:do-add
                                           :subject-key item-key
                                           :adjacent-group-key
                                           (into [(:item-id vb)] item-key)]}}
               [:div {:class "editable full-row top-border depth-1"
                      :key (into[[:elements [nil :tag]]
                                  [:comment [nil :tag]]
                                  (:item-id vb)]
                                 item-key)
                      :commands {:set-content [:do-create-content]
                                  :add-row [:do-add
                                            :subject-key item-key
                                            :adjacent-group-key
                                            (into [(:item-id vb)] item-key)]}}]]
              [:component {:key (into [(:item-id vb)] item-key)
                           :class "elements-column"}
               [item-DOM
                vb item-key
                #{Lb1}
                {:commands {:add-sibling [:do-add :template '(nil ("L1" :tag))]
                            :add-row [:do-add]}}
                {:depth 1 :do-not-merge #{}}]]]]])))))

(deftest table-DOM-test
  (let [[dom table joe jane]
        (let-mutated [table `("table"
                              ((:none (:none ("age" :tag))) :row-query)
                              (:none ("age" :tag) (~o1 :order :non-semantic)
                                     (:column :non-semantic))
                              (:none ("size" :tag) (~o2 :order :non-semantic)
                                     (:column :non-semantic)))
                      joe (list* (concat joe-list
                                         ['(:top-level :non-semantic)]))
                      jane (list* (concat jane-list
                                          ['(:top-level :non-semantic)]))]
          (expr-let [dom (table-DOM table [:foo] {:depth 0 :do-not-merge #{}})]
            [dom table joe jane]))
        query (current-value (entity/label->content table :row-query))
        age (first (current-value (label->elements table o1)))
        age-tag (first
                 (current-value (label->elements age :tag)))
        age-tag-spec (first (current-value (entity/elements age-tag)))
        size (first (current-value (label->elements table o2)))
        joe-bogus-age (first (current-value
                              (label->elements joe "doubtful")))
        joe-bogus-age-tag (first (current-value
                                  (label->elements joe-bogus-age :tag)))
        joe-bogus-age-tag-spec (first (current-value
                                       (entity/elements joe-bogus-age-tag)))
        joe-age (first (remove #{joe-bogus-age}
                               (current-value
                                (label->elements joe "age"))))]
    (is (check-keys dom joe))
    (is (check
         dom
         (let [table-key [(item-referent table) :foo]
               table-parent-key [(comment-referent (item-referent table))
                                 :foo]
               joe-row-key [(item-referent joe)
                            (comment-referent (item-referent table))
                            :foo]
               query-list '(nil (nil ("age" :tag)) (:top-level :non-semantic))
               age-header-key [[:parallel
                                [[:comment '(nil :tag)]]
                                [(key-referent [(item-referent age)
                                                (item-referent table)])
                                 [:parallel
                                   [(elements-referent
                                     (item-referent age))]
                                   [(query-referent query-list)]]]]
                               :foo]]
           [:div {:class "table" :key [(item-referent table) :foo]}
            [:div {:class "column-header-sequence"}
             [:div {:class "column-header-container tags top-level"                     
                    :style {:width "150px"}}
              [:component {:key (prepend-to-key (item-referent age-tag)
                                                age-header-key)
                           :class "column-header"
                           :commands {:add-column
                                      [:do-add
                                       :subject-key table-key
                                       :adjacent-group-key
                                       [(item-referent age)
                                        (item-referent table)
                                        :foo]
                                       :template '(:none
                                                   (??? :tag)
                                                   (:column :non-semantic))]
                                      :delete [:do-delete
                                               :delete-key
                                               [(item-referent age)
                                                (item-referent table)
                                                :foo]]}}
               [item-DOM
                age-tag age-header-key #{age-tag-spec}
                {:commands {:add-sibling [:do-add :template '(nil :tag)]}}
                {:level 0, :depth 0, :do-not-merge #{}}]]]
             ;; Size column.
             (any)]
            [:component {:class "table-row"
                         :key joe-row-key}
             (evals-to
              [:div {:key joe-row-key}
               [:div {:class "stack table-cell"}
                [:component {:key (into [(:item-id joe-bogus-age)
                                         (comment-referent (item-referent age))]
                                        joe-row-key)
                             :class "vertical-separated"}
                 [item-DOM
                  joe-bogus-age (into [(comment-referent (item-referent age))]
                                      joe-row-key)
                  #{joe-bogus-age-tag}
                  {:commands {:add-sibling [:do-add
                                            :template '(nil ("age" :tag))]
                              :add-row [:do-add
                                        :subject-key table-parent-key
                                        :adjacent-key joe-row-key
                                        :template
                                        '(nil (nil ("age" :tag))
                                              (:top-level :non-semantic))]}}
                  {:depth 0, :do-not-merge #{}}]]
                [:component {:key (into [(:item-id joe-age)
                                         (comment-referent (item-referent age))]
                                        joe-row-key),
                             :class "vertical-separated"}
                 (any)]]
               [:div {:key (into [[:elements '(nil ("size" :tag))]
                                  (comment-referent (item-referent size))]
                                 joe-row-key),
                      :class "editable table-cell"
                      :commands {:set-content [:do-create-content]
                                 :add-row
                                 [:do-add :subject-key table-parent-key
                                  :adjacent-key joe-row-key
                                  :template
                                  '(nil (nil ("age" :tag))
                                        (:top-level :non-semantic))]}}]])]]))))
  ;; Test a header with two labels.
  (let [[dom table joe jane]
        (let-mutated [table `("table"
                              (:none :row-query)
                              (:none ("name" :tag (~o1 :order :non-semantic))
                                     ("id" :tag (~o2 :order :non-semantic))
                                     (~o1 :order :non-semantic)
                                     (:column :non-semantic))
                              (:none ("age" :tag)
                                     (~o2 :order :non-semantic)
                                     (:column :non-semantic)))
                      joe `("Joe"
                            (~o1 :order :non-semantic)
                            (:top-level :non-semantic)
                            ("Joe" ("name" :tag) ("id" :tag)
                             (~o1 :order :non-semantic))
                            (45 ("age" :tag) (~o2 :order :non-semantic)))
                      jane `("Jane"
                             (~o2 :order :non-semantic)
                             (:top-level :non-semantic)
                             ("Jane" ("name" :tag) (~o1 :order :non-semantic))
                             (44 ("age" :tag) (~o2 :order :non-semantic)))]
          (expr-let [dom (table-DOM table [:foo] {:depth 0 :do-not-merge #{}})]
            [dom table joe jane]))
        query (current-value (entity/label->content table :row-query))
        name-id (first (current-value (label->elements table o1)))
        name-tag (first
                  (current-value (label->elements name-id o1)))
        name-tag-order (first (current-value (label->elements name-tag :order)))
        name-tag-spec (first
                       (remove #{name-tag-order}
                               (current-value (entity/elements name-tag))))
        id-tag (first
                (current-value (label->elements name-id o2)))
        id-tag-order (first (current-value (label->elements id-tag :order)))
        id-tag-spec (first
                       (remove #{id-tag-order}
                               (current-value (entity/elements id-tag))))
        joe-name (first (current-value
                         (label->elements joe "name")))
        joe-name-tags (current-value
                       (label->elements joe-name :tag))]
    (is (check-keys dom joe))
    (is (check
         dom
         (let [table-key [(item-referent table) :foo]
               table-parent-key [(comment-referent (item-referent table))
                                 :foo]
               joe-row-key [(item-referent joe)
                        (comment-referent (item-referent table))
                        :foo]
               jane-row-key [(item-referent jane)
                             (comment-referent (item-referent table))
                             :foo]
               query-list '(nil (:top-level :non-semantic))
               name-id-header-key [[:parallel
                                    [[:comment '(nil :tag)]]
                                    [(key-referent [(item-referent name-id)
                                                    (item-referent table)])
                                     [:parallel
                                      [(elements-referent
                                        (item-referent name-id))]
                                      [(query-referent query-list)]]]]
                                   :foo]]
           [:div {:class "table" :key [(item-referent table) :foo]}
            [:div {:class "column-header-sequence"}
             [:div {:class "column-header-container tags top-level"
                    :style {:width "150px"}}
              ;; name-id header
              [:div {:class "stack column-header"}
               ;; name part of name-id header
               [:component {:key (prepend-to-key (item-referent name-tag)
                                                 name-id-header-key)
                            :class "vertical-separated"
                            :commands {:add-column
                                       [:do-add :subject-key table-key
                                        :adjacent-group-key
                                        [(item-referent name-id)
                                         (item-referent table)
                                         :foo]
                                        :template '(:none
                                                    (??? :tag)
                                                    (:column :non-semantic))]}}
                [item-DOM
                 name-tag name-id-header-key #{name-tag-spec}
                 {:commands {:add-sibling [:do-add :template '(nil :tag)]}}
                 {:level 0, :depth 0, :do-not-merge #{}}]]
               ;; id part of name-id header
               [:component {:key (prepend-to-key (item-referent id-tag)
                                                 name-id-header-key)
                            :class "vertical-separated"
                            :commands {:add-column
                                       [:do-add :subject-key table-key
                                        :adjacent-group-key
                                        [(item-referent name-id)
                                         (item-referent table)
                                         :foo]
                                        :template '(:none
                                                    (??? :tag)
                                                    (:column :non-semantic)
                                                    ("name" :tag))]}}
                [item-DOM
                 id-tag name-id-header-key #{id-tag-spec}
                 {:commands {:add-sibling [:do-add :template '(nil :tag)]}}
                 {:level 0, :depth 0, :do-not-merge #{}}]]]]
             ;; Age column
             (any)]
            ;; Joe
            [:component {:class "table-row"
                         :key joe-row-key}
             (evals-to
              [:div {:key joe-row-key}
               [:component {:key (into [(:item-id joe-name)
                                        (comment-referent
                                         (item-referent name-id))]
                                       joe-row-key)
                            :class "table-cell"}
                [item-DOM
                 joe-name (into [(comment-referent (item-referent name-id))]
                                joe-row-key)
                 (set joe-name-tags)
                 {:commands {:add-sibling [:do-add
                                           :template '(nil ("id" :tag)
                                                           ("name" :tag))]
                             :add-row [:do-add
                                       :subject-key table-parent-key
                                       :adjacent-key joe-row-key
                                       :template
                                       '(nil (:top-level :non-semantic))]}}
                 {:depth 0, :do-not-merge #{}}]]
               ;; Joe's age
               (any)])]
            ;; Jane
            [:component {:class "table-row"
                         :key jane-row-key}
             (evals-to
              [:div {:key jane-row-key}
               [:div {:key (into [[:elements (as-set
                                              '(nil ("name" :tag) ("id" :tag)))]
                                  (comment-referent (item-referent name-id))]
                                 jane-row-key),
                      :class "editable table-cell"
                      :commands {:set-content [:do-create-content]
                                 :add-row
                                 [:do-add
                                  :subject-key table-parent-key
                                  :adjacent-key jane-row-key
                                  :template
                                  '(nil (:top-level :non-semantic))]}}]
               ;; Jane's age
               (any)])]]))))
  ;; Test a hierarchical header.
  (let [[dom table joe jane]
        (let-mutated [table `("table"
                              (:none :row-query)
                              (:none ("name" :tag (~o1 :order :non-semantic))
                                     (~o1 :order :non-semantic)
                                     (:column :non-semantic))
                              (:none ("name" :tag (~o1 :order :non-semantic))
                                     ("id" :tag (~o2 :order :non-semantic))
                                     (~o2 :order :non-semantic)
                                     (:column :non-semantic)))
                      joe `("Joe"
                            (~o1 :order :non-semantic)
                            (:top-level :non-semantic)
                            ("Joseph"
                             ("name" :tag) ("id" :tag)
                             (~o1 :order :non-semantic))
                            ("Joe"
                             ("name" :tag) (~o2 :order :non-semantic))
                            (45 ("age" :tag) (~o2 :order :non-semantic)))
                      jane `("Jane"
                             (~o2 :order :non-semantic)
                             (:top-level :non-semantic)
                             ("Jane" ("name" :tag) ("id :tag")
                              (~o1 :order :non-semantic))
                             (44 ("age" :tag) (~o2 :order :non-semantic)))]
          (expr-let [dom (table-DOM table [:foo] {:depth 0 :do-not-merge #{}})]
            [dom table joe jane]))
        query (current-value (entity/label->content table :row-query))
        name (first (current-value (label->elements table o1)))
        name-tag (first (current-value (label->elements name o1)))
        name-tag-order (first (current-value (label->elements name-tag :order)))
        name-tag-spec (first
                       (remove #{name-tag-order}
                               (current-value (entity/elements name-tag))))
        name-id (first (current-value (label->elements table o2)))
        id-tag (first (current-value (label->elements name-id o2)))
        id-tag-order (first (current-value (label->elements id-tag :order)))
        id-tag-spec (first
                     (remove #{id-tag-order}
                             (current-value (entity/elements id-tag))))
        joe-id (first (current-value
                       (label->elements joe "id")))
        joe-id-tags (current-value
                     (label->elements joe-id :tag))
        joe-nickname (first (current-value
                             (label->elements joe o2)))
        joe-nickname-tags (current-value
                     (label->elements joe-nickname :tag))]
    (is (check-keys dom joe))
    (is (check
         dom
         (let [table-key [(item-referent table) :foo]
               table-parent-key [(comment-referent (item-referent table))
                                 :foo]
               joe-row-key [(item-referent joe)
                            (comment-referent (item-referent table))
                            :foo]
               jane-row-key [(item-referent jane)
                             (comment-referent (item-referent table))
                             :foo]
               query-list '(nil (:top-level :non-semantic))
               name-referents [(key-referent [(item-referent name)
                                              (item-referent table)])
                               (key-referent [(item-referent name-id)
                                              (item-referent table)])
                               [:parallel
                                [(elements-referent
                                  (item-referent name))]
                                [(query-referent query-list)]]]
               just-name-referents [(key-referent [(item-referent name)
                                                   (item-referent table)])
                                    [:parallel
                                     [[:parallel
                                       []
                                       [(elements-referent
                                         (item-referent name))]
                                       [(elements-referent
                                         (item-referent name-id))]]]
                                     [(query-referent query-list)]]]
               name-id-referents [(key-referent [(item-referent name-id)
                                                 (item-referent table)])
                                  [:parallel
                                    [(elements-referent
                                      (item-referent name-id))]
                                   [(query-referent query-list)]]]
               name-header-key [[:parallel
                                 [[:comment '(nil :tag)]]
                                 name-referents]
                                :foo]
               name-id-header-key [[:parallel
                                    [[:comment '(nil :tag)]]
                                    name-id-referents]
                                   :foo]]
           [:div {:class "table" :key [(item-referent table) :foo]}
            [:div {:class "column-header-sequence"}
             [:div {:class "column-header-stack"}
              [:div {:class
                     "column-header-container rightmost tags with-children top-level"
                     :style {:width "300px"}}
               [:component {:key (prepend-to-key (item-referent name-tag)
                                                 name-header-key)
                            :class "column-header"
                            :commands {:add-column
                                       [:do-add
                                        :subject-key table-key
                                        :adjacent-group-key
                                        [(parallel-referent
                                          []
                                          [(item-referent name)
                                           (item-referent name-id)])
                                         (item-referent table)
                                         :foo]
                                        :template
                                        '(:none (??? :tag)
                                                (:column :non-semantic))]
                                       :delete
                                       [:do-delete
                                        :delete-key [[:parallel
                                                      [(item-referent name-tag)]
                                                      name-id-referents]
                                                     :foo]]}}
                 [item-DOM
                  name-tag name-header-key #{name-tag-spec}
                  {:commands {:add-sibling [:do-add :template '(nil :tag)]}}
                  {:level 0, :depth 0, :do-not-merge #{}}]]]
              [:div {:class "column-header-sequence"}
               [:div  {:class "column-header-container empty tags"
                       :style {:width "150px"}}
                [:div  {:class "editable column-header"
                        :key [[:parallel
                               [[:elements '(nil :tag)]]
                               just-name-referents]
                              :foo]
                        :commands {:delete
                                   [:do-delete
                                    :delete-key [(item-referent name)
                                                 (item-referent table)
                                                 :foo]]
                                   :set-content [:do-create-content]
                                   :add-column
                                   [:do-add
                                    :subject-key table-key
                                    :adjacent-group-key [(item-referent name)
                                                         (item-referent table)
                                                         :foo]
                                    :template '(:none (??? :tag)
                                                      (:column :non-semantic)
                                                      ("name" :tag))]}}]]
               [:div {:class "column-header-container rightmost tags"
                      :style {:width "150px"}}
                 [:component {:key (prepend-to-key (item-referent id-tag)
                                                   name-id-header-key)
                              :class "column-header"
                              :commands {:add-column
                                         [:do-add
                                          :subject-key table-key
                                          :adjacent-group-key
                                          [(item-referent name-id)
                                           (item-referent table)
                                           :foo]
                                          :template
                                          '(:none (??? :tag)
                                                  (:column :non-semantic)
                                                  ("name" :tag))]
                                         :delete [:do-delete
                                                  :delete-key
                                                  [(item-referent name-id)
                                                   (item-referent table)
                                                   :foo]]}}
                  [item-DOM
                   id-tag name-id-header-key #{id-tag-spec}
                   {:commands {:add-sibling [:do-add :template '(nil :tag)]}}
                   {:level 1, :depth 0, :do-not-merge #{}}]]]]]]
            ;; Joe
            [:component {:class "table-row"
                         :key joe-row-key}
             (evals-to
              [:div {:key joe-row-key}
               [:component {:key (into [(:item-id joe-nickname)
                                        (comment-referent
                                         (item-referent name))]
                                       joe-row-key)
                            :class "table-cell"}
                [item-DOM
                 joe-nickname (into [(comment-referent (item-referent name))]
                                    joe-row-key)
                 (set joe-nickname-tags)
                 {:commands {:add-sibling [:do-add
                                           :template '(nil ("name" :tag))]
                             :add-row [:do-add
                                       :subject-key table-parent-key
                                       :adjacent-key joe-row-key
                                       :template
                                       '(nil (:top-level :non-semantic))]}}
                 {:depth 0, :do-not-merge #{}}]]
               ;; Joe's id
               [:component {:key (into [(:item-id joe-id)
                                        (comment-referent
                                         (item-referent name-id))]
                                       joe-row-key)
                            :class "table-cell"}
                [item-DOM
                 joe-id (into [(comment-referent (item-referent name-id))]
                              joe-row-key)
                 (set joe-id-tags)
                 {:commands {:add-sibling [:do-add
                                           :template '(nil ("id" :tag)
                                                           ("name" :tag))]
                             :add-row [:do-add
                                       :subject-key table-parent-key
                                       :adjacent-key joe-row-key
                                       :template
                                       '(nil (:top-level :non-semantic))]}}
                 {:depth 0, :do-not-merge #{}}]]])]
            ;; Jane
            [:component {:class "table-row"
                         :key jane-row-key}
             (evals-to
              [:div {:key jane-row-key}
               [:component (any) (any)]
               ;; No name-id value.
               [:div {:class "editable table-cell"
                      :key (into [(elements-referent '(nil ("id" :tag)
                                                           ("name" :tag)))
                                  (comment-referent (item-referent name-id))]
                                 jane-row-key)
                      :commands {:set-content [:do-create-content]
                                 :add-row
                                 [:do-add
                                  :subject-key table-parent-key
                                  :adjacent-key jane-row-key
                                  :template
                                  '(nil (:top-level :non-semantic))]}}]])]]))))
  (let [[dom table joe jane]
        (let-mutated [table `("table"
                              (:none :row-query)
                              (:none ("name" :tag (~o1 :order :non-semantic))
                                     ("id" :tag (~o2 :order :non-semantic))
                                     (~o1 :order :non-semantic)
                                     (:column :non-semantic))
                              (:none ("name" :tag (~o1 :order :non-semantic))
                                     (~o2 :order :non-semantic)
                                     (:column :non-semantic)))
                      joe `("Joe"
                            (~o1 :order :non-semantic)
                            (:top-level :non-semantic)
                            ("Joseph"
                             ("name" :tag) ("id" :tag)
                             (~o1 :order :non-semantic))
                            ("Joe"
                             ("name" :tag) (~o2 :order :non-semantic))
                            (45 ("age" :tag) (~o2 :order :non-semantic)))
                      jane `("Jane"
                             (~o2 :order :non-semantic)
                             (:top-level :non-semantic)
                             ("Jane" ("name" :tag) ("id :tag")
                              (~o1 :order :non-semantic))
                             (44 ("age" :tag) (~o2 :order :non-semantic)))]
          (expr-let [dom (table-DOM table [:foo] {:depth 0 :do-not-merge #{}})]
            [dom table joe jane]))
        query (current-value (entity/label->content table :row-query))
        name-id (first (current-value (label->elements table o1)))
        name (first (current-value (label->elements table o2)))
        name-tag (first (current-value (label->elements name-id o1)))
        name-tag-order (first (current-value (label->elements name-tag :order)))
        name-tag-spec (first
                       (remove #{name-tag-order}
                               (current-value (entity/elements name-tag))))
        id-tag (first (current-value (label->elements name-id o2)))
        id-tag-order (first (current-value (label->elements id-tag :order)))
        id-tag-spec (first
                     (remove #{id-tag-order}
                             (current-value (entity/elements id-tag))))
        joe-id (first (current-value
                       (label->elements joe "id")))
        joe-id-tags (current-value
                     (label->elements joe-id :tag))
        joe-nickname (first (current-value
                             (label->elements joe o2)))
        joe-nickname-tags (current-value
                           (label->elements joe-nickname :tag))]
    (is (check-keys dom joe))
    (is (check
         dom
         (let [table-key [(item-referent table) :foo]
               table-parent-key [(comment-referent (item-referent table))
                                 :foo]
               joe-row-key [(item-referent joe)
                            (comment-referent (item-referent table))
                            :foo]
               jane-row-key [(item-referent jane)
                             (comment-referent (item-referent table))
                             :foo]
               query-list '(nil (:top-level :non-semantic))
               name-referents [(key-referent [(item-referent name-id)
                                              (item-referent table)])
                               (key-referent [(item-referent name)
                                              (item-referent table)])
                               [:parallel
                                [(elements-referent
                                  (item-referent name))]
                                [(query-referent query-list)]]]
               just-name-referents [(key-referent [(item-referent name)
                                                   (item-referent table)])
                                    [:parallel
                                     [[:parallel
                                       []
                                       [(elements-referent
                                         (item-referent name))]
                                       [(elements-referent
                                         (item-referent name-id))]]]
                                     [(query-referent query-list)]]]
               name-id-referents [(key-referent [(item-referent name-id)
                                                 (item-referent table)])
                                  [:parallel
                                   [(elements-referent
                                     (item-referent name-id))]
                                   [(query-referent query-list)]]]
               name-header-key [[:parallel
                                 [[:comment '(nil :tag)]]
                                 name-referents]
                                :foo]
               name-id-header-key [[:parallel
                                    [[:comment '(nil :tag)]]
                                    name-id-referents]
                                   :foo]]
           [:div {:class "table" :key table-key}
            [:div {:class "column-header-sequence"}
             [:div {:class "column-header-stack"}
              [:div {:class
                     "column-header-container rightmost tags with-children top-level"
                     :style {:width "300px"}}
               [:component {:key (prepend-to-key (item-referent name-tag)
                                                 name-header-key)
                            :commands {:add-column
                                       [:do-add
                                        :subject-key table-key
                                        :adjacent-group-key
                                        [(parallel-referent
                                          []
                                          [(item-referent name-id)
                                           (item-referent name)])
                                         (item-referent table)
                                         :foo]
                                        :template
                                        '(:none (??? :tag)
                                                (:column :non-semantic))]
                                       :delete [:do-delete
                                                :delete-key
                                                [[:parallel
                                                  [(item-referent name-tag)]
                                                  name-id-referents]
                                                 :foo]]}
                            :class "column-header"}
                [item-DOM
                 name-tag name-header-key #{name-tag-spec}
                 {:commands {:add-sibling [:do-add :template '(nil :tag)]}}
                 {:level 0, :depth 0, :do-not-merge #{}}]]]
              [:div {:class "column-header-sequence"}
               [:div {:class "column-header-container tags"
                      :style {:width "150px"}}
                [:component {:key (prepend-to-key (item-referent id-tag)
                                                  name-id-header-key)
                             :class "column-header"
                             :commands {:add-column
                                       [:do-add
                                        :subject-key table-key
                                        :adjacent-group-key
                                        [(item-referent name-id)
                                         (item-referent table)
                                         :foo]
                                        :template
                                        '(:none (??? :tag)
                                                (:column :non-semantic)
                                                ("name" :tag))]
                                       :delete [:do-delete
                                                :delete-key
                                                [(item-referent name-id)
                                                 (item-referent table)
                                                 :foo]]}}
                 [item-DOM
                  id-tag name-id-header-key #{id-tag-spec}
                  {:commands {:add-sibling [:do-add :template '(nil :tag)]}}
                  {:level 1, :depth 0, :do-not-merge #{}}]]]
               [:div  {:class "column-header-container rightmost empty tags"
                       :style {:width "150px"}}
                [:div  {:class "editable column-header"
                        :key [[:parallel
                               [[:elements '(nil :tag)]]
                               just-name-referents]
                              :foo]
                        :commands {:set-content [:do-create-content]
                                   :add-column
                                   [:do-add
                                    :subject-key table-key
                                    :adjacent-group-key
                                    [(item-referent name)
                                     (item-referent table)
                                     :foo]
                                    :template
                                    '(:none (??? :tag)
                                            (:column :non-semantic)
                                            ("name" :tag))]
                                   :delete [:do-delete
                                            :delete-key
                                            [(item-referent name)
                                             (item-referent table)
                                             :foo]]}}]]]]]
            ;; Joe
            [:component {:class "table-row"
                         :key joe-row-key}
             (evals-to
              [:div {:key joe-row-key}
               [:component {:key (into [(:item-id joe-id)
                                        (comment-referent
                                         (item-referent name-id))]
                                       joe-row-key)
                            :class "table-cell"}
                [item-DOM
                 joe-id (into [(comment-referent (item-referent name-id))]
                              joe-row-key)
                 (set joe-id-tags)
                 {:commands {:add-sibling [:do-add
                                           :template '(nil ("id" :tag)
                                                           ("name" :tag))]
                             :add-row [:do-add
                                       :subject-key table-parent-key
                                       :adjacent-key joe-row-key
                                       :template
                                       '(nil (:top-level :non-semantic))]}}
                 {:depth 0, :do-not-merge #{}}]]
               ;; Joe's id
              [:component {:key (into [(:item-id joe-nickname)
                                        (comment-referent
                                         (item-referent name))]
                                       joe-row-key)
                            :class "table-cell"}
                [item-DOM
                 joe-nickname (into [(comment-referent (item-referent name))]
                                    joe-row-key)
                 (set joe-nickname-tags)
                 {:commands {:add-sibling [:do-add
                                           :template '(nil ("name" :tag))]
                             :add-row [:do-add
                                       :subject-key table-parent-key
                                       :adjacent-key joe-row-key
                                       :template '(nil
                                                   (:top-level :non-semantic))]}}
                 {:depth 0, :do-not-merge #{}}]]])]
            ;; Jane
            [:component {:class "table-row"
                         :key jane-row-key}
             (evals-to              
              [:div {:key jane-row-key}
               [:div {:class "editable table-cell"
                      :key (into [(elements-referent '(nil ("id" :tag)
                                                           ("name" :tag)))
                                  (comment-referent (item-referent name-id))]
                                 jane-row-key)
                      :commands {:set-content [:do-create-content]
                                 :add-row
                                 [:do-add
                                  :subject-key table-parent-key
                                  :adjacent-key jane-row-key
                                  :template '(nil
                                              (:top-level :non-semantic))]}}]
               ;; No name-id value.
               [:component (any) (any)]])]])))))

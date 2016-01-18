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
             [test-utils :refer [check any as-set let-mutated]])
            (cosheet.server
             [key :refer [item-referent comment-referent
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
(def jane-list `("Jane" (~o1 :order) "plain" "plain"))
(def joe-list `("Joe"
               (~o2 :order)
               ("male" (~o1 :order))
               ("married" (~o2 :order))
               (39 (~o3 :order)
                   ("age" ~'tag)
                   ("doubtful" "confidence"))
               (45 (~o4 :order)
                   ("age" ~'tag))))

(defn get-dom-keys
  "Return all keys in the dom"
  [dom]
  (when (sequential? dom)
    (concat (vals (select-keys (dom-attributes dom)
                               [:key :add-ajacent :row-sibling]))
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

(deftest condition-specifiers-test
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-specifiers test '(nil :a)))))
             [:a]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-specifiers test '(nil :a :c)))))
             (as-set [:a :c])))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-specifiers test '(nil :a :b)))))
             [:a]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-specifiers test '(nil :a :b)))))
             [:a]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-specifiers test '(nil :a :a :b)))))
             [:a :a]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-specifiers test '(nil :a :a :b)))))
             [:a])))

(deftest canonical-info-set-test
  (is (= (let-mutated [joe joe-list, jane jane-list]
           (canonical-info-set [joe jane]))
         {["Joe"
           {"married" 1
            "male" 1
            [39 {["age" {'tag 1}] 1, ["doubtful" {"confidence" 1}] 1}] 1
            [45 {["age" {'tag 1}] 1}] 1}]1
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
  (is (check (append-to-hierarchy [] {:a 1} :i)
             [{:hierarchy-node true :groups {:a 1} :members [:i]}]))
  ;; Identical groups.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :groups {:a 1}
                                    :members [:i]}]
                                  {:a 1} :j)
             [{:hierarchy-node true :groups {:a 1} :members [:i :j]}]))
  ;; Completely different groups.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :groups {:a 1}
                                    :members [:i]}]
                                  {:b 1} :j)
             [{:hierarchy-node true :groups {:a 1} :members [:i]}
              {:hierarchy-node true :groups {:b 1} :members [:j]}]))
  ;; New element has added group.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :groups {:a 1}
                                    :members [:i]}]
                                  {:a 1 :b 1} :j)
             [{:hierarchy-node true
               :groups {:a 1}
               :members [:i]
               :children [{:hierarchy-node true
                           :groups {:b 1}
                           :members [:j]}]}]))
  ;; New element has fewer groups.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :groups {:a 1 :b 1}
                                    :members [:i]}]
                                  {:a 1} :j)
             [{:hierarchy-node true :groups {:a 1}
               :members []
               :children [{:hierarchy-node true :groups {:b 1} :members [:i]}
                          {:hierarchy-node true :groups {}, :members [:j]}]}]))
  ;; Old and new have some in common.
  (is (check
       (append-to-hierarchy [{:hierarchy-node true
                              :groups {:a 1 :b 1}
                              :members [:i]}]
                            {:a 1 :c 1} :j)
       [{:hierarchy-node true
         :groups {:a 1}
         :members []
         :children [{:hierarchy-node true :groups {:b 1} :members [:i]}
                    {:hierarchy-node true :groups {:c 1} :members [:j]}]}]))
  ;; Must add to child of last element.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :groups {:a 1}
                                    :members [:i]
                                    :children [{:hierarchy-node true
                                                :groups {:b 1}
                                                :members [:j]}]}]
                                  {:a 1 :b 1 :c 1} :k)
             [{:hierarchy-node true
               :groups {:a 1}
               :members [:i]
               :children [{:hierarchy-node true
                           :groups {:b 1}
                           :members [:j]
                           :children [{:hierarchy-node true
                                       :groups {:c 1}
                                       :members [:k]}]}]}]))
  ;; Must close off child of last element.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :groups {:a 1}
                                    :members [:i]
                                    :children [{:hierarchy-node true
                                                :groups {:b 1}
                                                :members [:j]}]}]
                                  {:a 1} :k)
             [{:hierarchy-node true
               :groups {:a 1}
               :members [:i]
               :children [{:hierarchy-node true :groups {:b 1} :members [:j]}
                          {:hierarchy-node true :groups {} :members [:k]}]}]))
  ;; Partial sharing with last element that has children.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :groups {:a 1 :b 1}
                                    :members [:i]
                                    :children [{:groups {:c 1} :members [:j]}]}]
                                  {:a 1 :c 1} :k)
             [{:hierarchy-node true
               :groups {:a 1}
               :members []
               :children [{:hierarchy-node true
                           :groups {:b 1}
                           :members [:i]
                           :children [{:groups {:c 1} :members [:j]}]}
                          {:hierarchy-node true
                           :groups {:c 1}
                           :members [:k]}]}]))
  ;; Append empty group after empty group.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :groups {:a 1 :b 1}
                                    :members [:i]
                                    :children [{:hierarchy-node true
                                                :groups {:c 1}
                                                :members [:j]}
                                               {:hierarchy-node true
                                                :groups {}
                                                :members [:k]}]}]
                                  {:a 1 :b 1} :l)
             [{:hierarchy-node true
               :groups {:a 1 :b 1}
               :members [:i]
               :children [{:hierarchy-node true :groups {:c 1} :members [:j]}
                          {:hierarchy-node true :groups {} :members [:k]}
                          {:hierarchy-node true :groups {} :members [:l]}]}]))
  ;; Append non-empty group after empty group.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :groups {:a 1 :b 1}
                                    :members [:i]
                                    :children [{:hierarchy-node true
                                                :groups {:c 1}
                                                :members [:j]}
                                               {:hierarchy-node true
                                                :groups {}
                                                :members [:k]}]}]
                                  {:a 1 :b 1 :c 1} :l)
             [{:hierarchy-node true
               :groups {:a 1 :b 1}
               :members [:i]
               :children [{:hierarchy-node true :groups {:c 1} :members [:j]}
                          {:hierarchy-node true :groups {} :members [:k]}
                          {:hierarchy-node true :groups {:c 1} :members [:l]}]
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
        [{:group-canonicals [:a] :item `(:i (~o1 :order))}
         {:group-canonicals [:a :b] :item `(:j (~o2 :order))}
         {:group-canonicals [:a :c] :item `(:k (~o3 :order))}]
        #{})
       [{:hierarchy-node true
         :groups {:a 1}
         :members [{:group-canonicals [:a] :item `(:i (~o1 :order))}]
         :children [{:hierarchy-node true
                     :groups {:b 1}
                     :members [{:group-canonicals [:a :b]
                                :item `(:j (~o2 :order))}]}
                    {:hierarchy-node true
                     :groups {:c 1}
                     :members [{:group-canonicals [:a :c]
                                :item `(:k (~o3 :order))}]}]}])))

(deftest hierarchy-node-descendants-test
  (is (check (set (hierarchy-node-descendants
                   {:hierarchy-node true
                    :groups {:a 1}
                    :members [:i]
                    :children [{:groups {:b 1} :members [:j]}]}))
             #{:i :j})))

(deftest hierarchy-node-next-level-test
  (is (check (hierarchy-node-next-level
              {:hierarchy-node true
               :groups {:a 1 :b 1}
               :members [:i]
               :children [{:hierarchy-node true :groups {:c 1} :members [:j :k]}
                          {:hierarchy-node true :groups {} :members [:l :m ]}]})
             [:i
              {:hierarchy-node true :groups {:c 1} :members [:j :k]}
              :l :m])))

(deftest flatten-hierarchy-test
  (is (check (flatten-hierarchy
              [{:hierarchy-node true
                :groups {:a 1}
                :members [:i]
                :children [{:hierarchy-node true
                            :groups {:b 1}
                            :members [:j]
                            :children [{:hierarchy-node true
                                        :groups {:c 1}
                                        :members [:l]}]}
                           {:hierarchy-node true
                            :groups {:c 1}
                            :members [:k]}]}]
              0 {})
             [{:hierarchy-node true
               :groups {:a 1}
               :cumulative-groups {:a 1}
               :depth 0
               :members [:i]
               :children [{:hierarchy-node true
                           :groups {:b 1}
                           :members [:j]
                           :children [{:hierarchy-node true
                                       :groups {:c 1}
                                       :members [:l]}]}
                          {:hierarchy-node true
                           :groups {:c 1}
                           :members [:k]}]}
              {:hierarchy-node true
               :groups {:b 1}
               :cumulative-groups {:a 1 :b 1}
               :depth 1
               :members [:j]
               :children [{:hierarchy-node true
                           :groups {:c 1}
                           :members [:l]}]}
              {:hierarchy-node true
               :groups {:c 1}
               :cumulative-groups {:a 1 :b 1 :c 1}
               :depth 2 :members [:l]}
              {:hierarchy-node true
               :groups {:c 1}
               :cumulative-groups {:a 1 :c 1}
               :depth 1
               :members [:k]}])))

(deftest canonical-info-to-generating-items-test
  (is (check (canonical-info-to-generating-items
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
                              (label->elements bogus-age 'tag)))
        age-tag (first (current-value (label->elements age 'tag)))]
    (is (check
         (let [hierarchy (current-value
                          (items-hierarchy-by-condition
                           [gender age bogus-age] #{} '(nil tag)))]
           (flatten-hierarchy-add-row-header-border-info hierarchy))
         [{:depth 0 :top-border :full :bottom-border :corner
           :hierarchy-node true
           :groups {}
           :cumulative-groups {}
           :members [{:item gender, :group-elements '() :group-canonicals nil}]}
          {:depth 0 :for-multiple true :top-border :full :bottom-border :full
           :hierarchy-node true
           :groups {["age" {'tag 1}] 1}
           :cumulative-groups {["age" {'tag 1}] 1}
           :members [{:item bogus-age
                      :group-elements [bogus-age-tag]
                      :group-canonicals [["age" {'tag 1}]]}
                     {:item age
                      :group-elements [age-tag]
                      :group-canonicals [["age" {'tag 1}]]}]}]))))

(def t1 (add-entity (new-element-store) nil 'joe))
(def store (first t1))
(def rid :foo)
(def root (description->entity rid store))

(deftest row-header-elements-DOM-test
  (is (check (row-header-elements-DOM {:depth 0 :is-tags true}
                                      nil '(nil tag) root [rid] {})
             [:div {:class "full-row editable column tags"
                    :key [[:elements [nil 'tag]] rid]
                    :row-sibling [rid]}]))
  (let [[dom fred fred-tag]
        (let-mutated [fred '("Fred" tag)]
          (expr-let [dom (row-header-elements-DOM {:depth 1
                                                   :is-tags true
                                                   :bottom-border :indented
                                                   :for-multiple true
                                                   :with-children true}
                                                  [fred] '(nil tag) root [rid]
                                                  {:depth 0})
                     fred-elements (entity/elements fred)]
            [dom fred (first fred-elements)]))]
    (is (check-keys dom fred))
    (is (check
         dom
         [:div {:class "column tags"}
          [:div {:class "full-row bottom-border with-children for-multiple indent-1"}
           [:component {:key [(:item-id fred) [:comment [nil 'tag]] rid]
                        :sibling-elements ['tag]
                        :row-sibling [rid]}
            [item-DOM
             fred [[:comment [nil 'tag]] rid]
             #{fred-tag} {:depth 0}]]
           [:div {:class "spacer"}]]])))
  (let [[dom fred fran]
        (let-mutated [fred '("Fred" tag)
                      fran "Fran"]
          (expr-let [dom (row-header-elements-DOM
                          {:depth 0
                           :is-tags true
                           :top-border :full
                           :bottom-border :corner}
                          [fred fran] '(nil tag) root [rid]
                          {:depth 1 :do-not-merge #{}})]
            [dom fred fran]))
        fred-tag (first (current-value (entity/elements fred)))]
    (is (check-keys dom fred))
    (is (check
         dom
         [:div
          {:class "full-row column tags top-border ll-corner"
           :key [[:elements [nil 'tag]] rid]
           :row-sibling [rid]}
          [:div {:class "stack"}
           [:component {:key [(:item-id fred) [:comment [nil 'tag]] rid]
                        :style {:display "block"
                                :width "100%"}
                        :class "vertical-separated"
                        :sibling-elements ['tag]
                        :row-sibling [rid]}
            [item-DOM
             fred [[:comment [nil 'tag]] rid]
             #{fred-tag} {:depth 1 :do-not-merge #{}}]]
           [:component {:key [(:item-id fran) [:comment [nil 'tag]] rid]
                        :style  {:display "block"
                                 :width "100%"}
                        :class "vertical-separated"
                        :sibling-elements ['tag]
                        :row-sibling [rid]}
            [item-DOM
             fran [[:comment [nil 'tag]] rid]
             #{} {:depth 1 :do-not-merge #{}}]]]
          [:div {:class "spacer"}]]))))

(deftest item-DOM-test
  (let [[dom fred]
        (let-mutated [fred "Fred"]
          (expr-let [dom (item-DOM fred [] #{}
                                   {:depth 0 :do-not-merge #{}})]
            [dom fred]))]
    (is (check-keys dom fred))
    (is (check dom
               [:div {:class "item content-text editable"
                      :key [(:item-id fred)]} "Fred"])))
  ;; Check generation of a single tag for a single item.
  (let [[dom age]
        (let-mutated [age `(39 ("doubtful"
                                ("confidence" ~'tag)
                                (~o1 :order)))]
          (expr-let [dom (item-DOM age [:age] #{} {:depth 0 :do-not-merge #{}})]
            [dom age]))
        doubtful (first (current-value (label->elements age o1)))
        confidence (first (current-value
                           (label->elements doubtful 'tag)))
        confidence-tag (first (current-value (entity/elements confidence)))
        item-key [(item-referent age) :age]
        tag-key (into [[:comment [nil 'tag]] (:item-id doubtful)] item-key)]
    (is (check-keys dom age))
    (is (check
         dom
         [:div {:class "item with-elements" :key [(item-referent age) :age]}
          [:div {:style {:width "100%" :display "block"}
                 :class "content-text editable"
                 :key [(content-location-referent) (item-referent age) :age]}
           "39"]
          [:div {:style {:height "1px"
                         :display "table"
                         :table-layout "fixed"
                         :width "100%"}
                 :class "element-table"}
           [:div {:style {:display "table-row"}
                  :class "last-row"}
            [:component {:key (into [(item-referent confidence)] tag-key)
                         :style {:display "table-cell"}
                         :class "full-row column tags top-border bottom-border"
                         :sibling-elements ['tag]
                         :row-sibling (into [(:item-id doubtful)] item-key)}
             [item-DOM
              confidence tag-key
              #{confidence-tag} {:depth 1 :do-not-merge #{}}]]
            [:component {:key (into [(:item-id doubtful)] item-key)
                         :class "column"
                         :style {:display "table-cell"}
                         :sibling-elements [["confidence" 'tag]]}
             [item-DOM
              doubtful item-key
              #{confidence} {:depth 1 :do-not-merge #{}}]]]]])))
  ;; Check that we generate no-tags.
  (let [[dom age]
        (let-mutated [age `(39 ("doubtful" (~o1 :order)))]
          (expr-let [dom (item-DOM age [:age] #{} {:depth 0 :do-not-merge #{}})]
            [dom age]))
        doubtful (first (current-value (label->elements age o1)))
        item-key [(item-referent age) :age]]
    (is (check-keys dom age))
    (is (check dom
           [:div {:class "item with-elements" :key item-key}
            (any vector?)
            [:div (any map?)
             [:div {:style {:display "table-row"}
                    :class "no-tags last-row"}
              [:div {:style {:display "table-cell"}
                     :class "full-row editable column tags top-border bottom-border"
                     :key (into [[:elements [nil 'tag]]
                                 (:item-id doubtful)] item-key)
                     :row-sibling (into [(:item-id doubtful)] item-key)}]
              [:component {:key (into [(:item-id doubtful)] item-key)
                           :class "column"
                           :style {:display "table-cell"}
                           :sibling-elements nil}
               [item-DOM
                doubtful item-key
                #{} {:depth 1 :do-not-merge #{}}]]]]])))
  ;; Test added elements, and a mutable set for do-not-merge
  (let [do-not-merge (new-mutable-set #{})
        [dom-reporter joe]
        (let-mutated [joe joe-list]
          (expr identity
            [(item-DOM joe [:joe] #{} {:depth 0 :do-not-merge do-not-merge})
             joe]))
        male (first (current-value (label->elements joe o1)))
        married (first (current-value (label->elements joe o2)))
        bogus-age (first (current-value
                          (label->elements joe "doubtful")))
        bogus-age-tag (first (current-value
                              (label->elements bogus-age 'tag)))
        bogus-age-tag-spec (first (current-value
                                   (entity/elements bogus-age-tag)))
        age (first (remove #{bogus-age}
                           (current-value
                            (label->elements joe "age"))))
        age-tag (first (current-value (label->elements age 'tag)))
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
             [:div {:style {:display "table-row"}
                    :class "last-row"}
              [:div {:class (any string?)
                     :style {:display "table-cell"}}
               [:component
                {:key  (->> both-ages-key
                            (prepend-to-key (comment-referent '(nil tag)))
                            (prepend-to-key (item-referent bogus-age-tag)))
                 :sibling-elements ['tag]
                 :row-sibling both-ages-key}
                [item-DOM
                 bogus-age-tag
                 (prepend-to-key [:comment [nil 'tag]] both-ages-key)
                 #{bogus-age-tag-spec} (any map?)]]
               [:div {:class "spacer"}]]
              [:div (any map?)
               [:component (any map?)
                [item-DOM bogus-age item-key #{bogus-age-tag} (any map?)]]
               [:component (any map?)
                [item-DOM age item-key #{age-tag} (any map?)]]]]]]))
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
             [:div {:style {:display "table-row"}}
              [:component (any map?)
               [item-DOM
                bogus-age-tag (into [[:comment [nil 'tag]]
                                     (:item-id bogus-age)]
                                    item-key)
                #{bogus-age-tag-spec} (any map?)]]
              [:component (any map?)
               [item-DOM
                bogus-age item-key
                #{bogus-age-tag} (any map?)]]]
             [:div {:style {:display "table-row"}
                    :class "last-row"}
              [:component (any map?)
               [item-DOM
                age-tag (into [[:comment [nil 'tag]]
                               (:item-id age)]
                              item-key)
                #{age-tag-spec} (any map?)]]
              [:component (any map?)
               [item-DOM
                age item-key
                #{age-tag} (any map?)]]]]]
           ))))
  ;; Test a hierarchy.
  (let [[dom-reporter joe]
        (let-mutated [joe `("Joe"
                            (~o2 :order)
                            ("1" (~o1 :order)
                                 ("L1" ~'tag))
                            (12 (~o2 :order)
                                ("L1" ~'tag (~o1 :order))
                                ("L2" ~'tag (~o2 :order)))
                            (13 (~o3 :order)
                                ("L1" ~'tag (~o1 :order))
                                ("L3" ~'tag (~o2 :order))))]
          (expr identity
            [(item-DOM joe [:joe] #{} {:depth 0 :do-not-merge #{}}) joe]))
        v1 (first (current-value (label->elements joe o1)))
        v12 (first (current-value (label->elements joe o2)))
        v13 (first (current-value (label->elements joe o3)))
        L1 (first (current-value (label->elements v1 'tag)))
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
               [:div {:style {:display "table-row"}}
                [:component
                 {:key (->> both-ages-key
                            (prepend-to-key (comment-referent '(nil tag)))
                            (prepend-to-key (item-referent L1)))
                  :style {:display "table-cell"}
                  :class "full-row with-children column tags top-border"
                  :sibling-elements ['tag]
                  :row-sibling [[:parallel []
                                 (as-set [(:item-id v1)
                                          (:item-id v12)
                                          (:item-id v13)])]
                                (item-referent joe)
                                :joe]}
                 [item-DOM L1
                  (prepend-to-key [:comment [nil 'tag]] both-ages-key)
                  #{L1-spec} (any map?)]]
                [:component {:key (into [(:item-id v1)] item-key)
                             :style {:display "table-cell"}
                             :class "column"
                             :sibling-elements [["L1" 'tag]]}
                 [item-DOM v1 item-key #{L1} (any map?)]]]
               [:div {:style {:display "table-row"}}
                [:div {:class "column tags" :style {:display "table-cell"}}
                 [:component
                  {:key (into [(:item-id L2)
                               [:comment [nil 'tag]]
                               (:item-id v12)]
                              item-key)
                   :class "full-row top-border indent-1"
                   :sibling-elements ['tag]
                   :row-sibling (into [(:item-id v12)] item-key)}
                  [item-DOM
                   L2 (into [[:comment [nil 'tag]] (:item-id v12)]
                            item-key)
                   #{L2-spec} (any map?)]]]
                [:component {:key (into [(:item-id v12)] item-key)
                             :style {:display "table-cell"}
                             :class "column"
                             :sibling-elements (as-set [["L1" 'tag]
                                                        ["L2" 'tag]])}
                 [item-DOM v12 item-key #{L121 L2} (any map?)]]]
               [:div {:style {:display "table-row"} :class "last-row"}
                [:div {:class "column tags bottom-border"
                       :style {:display "table-cell"}}
                 [:component
                  {:key (into [(:item-id L3)
                               [:comment [nil 'tag]]
                               (:item-id v13)]
                              item-key)
                   :class "full-row top-border indent-1"
                   :sibling-elements ['tag]
                   :row-sibling (into [(:item-id v13)] item-key)}
                  [item-DOM
                   L3 (into [[:comment [nil 'tag]] (:item-id v13)]
                            item-key)
                   #{L3-spec} (any map?)]]]
                [:component {:key (into [(:item-id v13)] item-key)
                             :style {:display "table-cell"}
                             :class "column"
                             :sibling-elements (as-set [["L3" 'tag]
                                                        ["L1" 'tag]])}
                 [item-DOM
                  v13 item-key #{L131 L3} (any map?)]]]]]))))
  ;; Test a hierarchy with an empty content in one row and an empty
  ;; tag in another.
  (let [[dom-reporter joe]
        (let-mutated [joe `("Joe"
                            (~o2 :order)
                            ("a" (~o2 :order)
                             ("L1" ~'tag (~o1 :order))
                             ("L2" ~'tag (~o2 :order)))
                            ("b" (~o3 :order)
                             ("L1" ~'tag (~o1 :order))))]
          (expr identity
            [(item-DOM joe [rid] #{} {:depth 0 :do-not-merge #{}}) joe]))
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
                        [(:item-id La1) [:comment [nil 'tag]]]
                        [(:item-id va) (:item-id vb)]]]
           [:div {:class "item with-elements" :key item-key}
            (any vector?)
            [:div (any map?)
             [:div {:style {:display "table-row"}}
              [:component
               {:key (->> both-L1s-key
                          (prepend-to-key [:comment [nil 'tag]])
                          (prepend-to-key (:item-id La1)))
                :style {:display "table-cell"}
                :class "full-row with-children column tags top-border"
                :sibling-elements ['tag]
                :row-sibling both-L1s-key}
               [item-DOM La1 (prepend-to-key [:comment [nil 'tag]] both-L1s-key)
                #{La1-spec} (any map?)]]
              [:div {:key (into [[:elements [nil ["L1" 'tag]]]] item-key)
                     :style {:display "table-cell"}
                     :class "editable column"
                     :add-adjacent (into [(:item-id va)] item-key)
                     :add-direction :before}]]
             [:div {:style {:display "table-row"}}
              [:div {:class "column tags" :style {:display "table-cell"}}
               [:component
                {:key (into [(:item-id La2)
                             [:comment [nil 'tag]]
                             (:item-id va)]
                            item-key)
                 :class "full-row top-border indent-1"
                 :sibling-elements ['tag]
                 :row-sibling (into [(:item-id va)] item-key)}
                [item-DOM
                 La2 (into [[:comment [nil 'tag]]
                            (:item-id va)]
                           item-key)
                 #{La2-spec} (any map?)]]]
              [:component {:key (into [(:item-id va)] item-key)
                           :style {:display "table-cell"}
                           :class "column"
                           :sibling-elements (as-set [["L2" 'tag]
                                                      ["L1" 'tag]])}
               [item-DOM va item-key #{La1 La2} (any map?)]]]
             [:div {:style {:display "table-row"} :class "last-row"}
              [:div {:class "column tags bottom-border"
                     :style {:display "table-cell"}
                     :row-sibling (into [(:item-id vb)] item-key)}
               [:div {:class "full-row top-border editable indent-1"
                      :key (into [[:elements [nil 'tag]]
                                  (:item-id vb)]
                                 item-key)}]]
              [:component {:key (into [(:item-id vb)] item-key)
                           :style {:display "table-cell"}
                           :class "column"
                           :sibling-elements [["L1" 'tag]]}
               [item-DOM
                vb item-key
                #{Lb1} {:depth 1 :do-not-merge #{}}]]]]])))))

(deftest table-DOM-test
  (let [[dom table joe jane]
        (let-mutated [table `("table"
                              ((:none (:none ("age" ~'tag))) :row-query)
                              ((:none ("age" ~'tag) (~o1 :order)) :column :c1)
                              ((:none ("size" ~'tag) (~o2 :order)) :column :c2))
                      joe (list* (concat joe-list
                                         ['(:top-level :non-semantic)]))
                      jane (list* (concat jane-list
                                          ['(:top-level :non-semantic)]))]
          (expr-let [dom (table-DOM table [:foo] {:depth 0 :do-not-merge #{}})]
            [dom table joe jane]))
        query (current-value (entity/label->content table :row-query))
        age (first (current-value (label->elements table :c1)))
        age-content (current-value (entity/content age))
        age-tag (first
                 (current-value (label->elements age-content 'tag)))
        age-tag-spec (first (current-value (entity/elements age-tag)))
        size (first (current-value (label->elements table :c2)))
        joe-bogus-age (first (current-value
                              (label->elements joe "doubtful")))
        joe-bogus-age-tag (first (current-value
                                  (label->elements joe-bogus-age 'tag)))
        joe-bogus-age-tag-spec (first (current-value
                                       (entity/elements joe-bogus-age-tag)))
        joe-age (first (remove #{joe-bogus-age}
                               (current-value
                                (label->elements joe "age"))))]
    (is (check-keys dom joe))
    (is (check
         dom
         (let [joe-key [(item-referent joe) :foo]
               query-list '(nil (nil ("age" tag)) (:top-level :non-semantic))
               age-header-key [[:parallel
                                [[:comment '(nil tag)]]
                                [(key-referent [(content-referent)
                                                (item-referent age)
                                                (item-referent table)])
                                 [:parallel
                                   [(elements-referent
                                     (item-referent age-content))]
                                   [(query-referent query-list)]]]]
                               :foo]]
           [:div {:class "table" :key [(item-referent table) :foo]}
            [:div {:class "column-header-sequence"}
             [:div {:class "column-header-container tags"                     
                    :style {:width "100px"}}
              [:component {:key (prepend-to-key (item-referent age-tag)
                                                age-header-key)
                           :sibling-elements ['tag]
                           :class "column-header"}
               [item-DOM
                age-tag age-header-key #{age-tag-spec}
                {:level 0, :depth 0, :do-not-merge #{}}]]]
             ;; Size column.
             (any)]
            [:div {:class "table-row"}
             [:div {:class "table-cell"}
              [:component {:key (into [(:item-id joe-bogus-age)
                                       (comment-referent (item-referent age))]
                                      joe-key)
                           :class "vertical-separated"
                           :style {:width "100%"
                                   :display "block"}
                           :sibling-elements [["age" 'tag]]}
               [item-DOM
                joe-bogus-age (into [(comment-referent (item-referent age))]
                                    joe-key)
                #{joe-bogus-age-tag}
                {:depth 0, :do-not-merge #{}}]]
              [:component {:key (into [(:item-id joe-age)
                                       (comment-referent (item-referent age))]
                                      joe-key),
                           :class "vertical-separated"
                           :style {:width "100%"
                                   :display "block"}
                           :sibling-elements [["age" 'tag]]}
               (any)]]
             [:div {:key (into [[:elements '(nil ("size" tag))]
                                (comment-referent (item-referent size))]
                               joe-key),
                    :class "editable table-cell"}]]]))))
  ;; Test a header with two labels.
  (let [[dom table joe jane]
        (let-mutated [table `("table"
                              (:none :row-query)
                              ((:none ("name" ~'tag (~o1 :order))
                                      ("id" ~'tag (~o2 :order))
                                      (~o1 :order))
                               :column :c1)
                              ((:none ("age" ~'tag) (~o2 :order)) :column :c2))
                      joe `("Joe"
                            (~o1 :order)
                            (:top-level :non-semantic)
                            ("Joe" ("name" ~'tag) ("id" ~'tag) (~o1 :order))
                            (45 ("age" ~'tag) (~o2 :order)))
                      jane `("Jane"
                             (~o2 :order)
                             (:top-level :non-semantic)
                             ("Jane" ("name" ~'tag) (~o1 :order))
                             (44 ("age" ~'tag) (~o2 :order)))]
          (expr-let [dom (table-DOM table [:foo] {:depth 0 :do-not-merge #{}})]
            [dom table joe jane]))
        query (current-value (entity/label->content table :row-query))
        name-id (first (current-value (label->elements table :c1)))
        name-id-content (current-value (entity/content name-id))
        name-tag (first
                  (current-value (label->elements name-id-content o1)))
        name-tag-order (first (current-value (label->elements name-tag :order)))
        name-tag-spec (first
                       (remove #{name-tag-order}
                               (current-value (entity/elements name-tag))))
        joe-name (first (current-value
                         (label->elements joe "name")))
        joe-name-tags (current-value
                       (label->elements joe-name 'tag))]
    (is (check-keys dom joe))
    (is (check
         dom
         (let [joe-key [(item-referent joe) :foo]
               query-list '(nil (:top-level :non-semantic))
               name-header-key [[:parallel
                                 [[:comment '(nil tag)]]
                                 [(key-referent [(content-referent)
                                                 (item-referent name-id)
                                                 (item-referent table)])
                                  [:parallel
                                   [(elements-referent
                                     (item-referent name-id-content))]
                                   [(query-referent query-list)]]]]
                                :foo]]
           [:div {:class "table" :key [(item-referent table) :foo]}
            [:div {:class "column-header-sequence"}
             [:div {:class "column-header-container tags"
                    :style {:width "100px"}}
              [:div {:class "stack column-header"}
               [:component {:key (prepend-to-key (item-referent name-tag)
                                                 name-header-key)
                            :sibling-elements ['tag]
                            :style {:width "100%", :display "block"}
                            :class "vertical-separated"}
                [item-DOM
                 name-tag name-header-key #{name-tag-spec}
                 {:level 0, :depth 0, :do-not-merge #{}}]]
               ;; id label
               (any)]]
             ;; Age column
             (any)]
            [:div {:class "table-row"}
             [:component {:key (into [(:item-id joe-name)
                                      (comment-referent
                                       (item-referent name-id))]
                                      joe-key)
                           :class "table-cell"
                          :sibling-elements (as-set [["name" 'tag]
                                                     ["id" 'tag]])}
               [item-DOM
                joe-name (into [(comment-referent (item-referent name-id))]
                               joe-key)
                (set joe-name-tags)
                {:depth 0, :do-not-merge #{}}]]
             ;; Joe's age
             (any)]
            [:div {:class "table-row"}
             [:div {:key [[:elements (as-set
                                      '(nil ("name" tag) ("id" tag)))]
                          (comment-referent (item-referent name-id))
                          (item-referent jane)
                          :foo],
                    :class "editable table-cell"}]
             ;; Jane's age
             (any)]]))))
  ;; Test a hierarchical header.
  (let [[dom table joe jane]
        (let-mutated [table `("table"
                              (:none :row-query)
                              ((:none ("name" ~'tag (~o1 :order))
                                      (~o1 :order))
                               :column :c1)
                              ((:none ("name" ~'tag (~o1 :order))
                                      ("id" ~'tag (~o2 :order))
                                      (~o2 :order))
                               :column :c2))
                      joe `("Joe"
                            (~o1 :order)
                            (:top-level :non-semantic)
                            ("Joseph"
                             ("name" ~'tag) ("id" ~'tag) (~o1 :order))
                            ("Joe"
                             ("name" ~'tag) (~o2 :order))
                            (45 ("age" ~'tag) (~o2 :order)))
                      jane `("Jane"
                             (~o2 :order)
                             (:top-level :non-semantic)
                             ("Jane" ("name" ~'tag) ("id ~'tag") (~o1 :order))
                             (44 ("age" ~'tag) (~o2 :order)))]
          (expr-let [dom (table-DOM table [:foo] {:depth 0 :do-not-merge #{}})]
            [dom table joe jane]))
        query (current-value (entity/label->content table :row-query))
        name (first (current-value (label->elements table :c1)))
        name-content (current-value (entity/content name))
        name-tag (first (current-value (label->elements name-content o1)))
        name-tag-order (first (current-value (label->elements name-tag :order)))
        name-tag-spec (first
                       (remove #{name-tag-order}
                               (current-value (entity/elements name-tag))))
        name-id (first (current-value (label->elements table :c2)))
        name-id-content (current-value (entity/content name-id))
        id-tag (first (current-value (label->elements name-id-content o2)))
        id-tag-order (first (current-value (label->elements id-tag :order)))
        id-tag-spec (first
                     (remove #{id-tag-order}
                             (current-value (entity/elements id-tag))))
        joe-id (first (current-value
                       (label->elements joe "id")))
        joe-id-tags (current-value
                     (label->elements joe-id 'tag))
        joe-nickname (first (current-value
                             (label->elements joe o2)))
        joe-nickname-tags (current-value
                     (label->elements joe-nickname 'tag))]
    (is (check-keys dom joe))
    (is (check
         dom
         (let [joe-key [(item-referent joe) :foo]
               jane-key [(item-referent jane) :foo]
               query-list '(nil (:top-level :non-semantic))
               name-referents [(key-referent [(content-referent)
                                              (item-referent name)
                                              (item-referent table)])
                               (key-referent [(content-referent)
                                              (item-referent name-id)
                                              (item-referent table)])
                               [:parallel
                                [(elements-referent
                                  (item-referent name-content))]
                                [(query-referent query-list)]]]
               just-name-referents [(key-referent [(content-referent)
                                                   (item-referent name)
                                                   (item-referent table)])
                                    [:parallel
                                     [[:parallel
                                       []
                                       [(elements-referent
                                         (item-referent name-content))]
                                       [(elements-referent
                                         (item-referent name-id-content))]]]
                                     [(query-referent query-list)]]]
               name-id-referents [(key-referent [(content-referent)
                                                 (item-referent name-id)
                                                 (item-referent table)])
                                  [:parallel
                                    [(elements-referent
                                      (item-referent name-id-content))]
                                   [(query-referent query-list)]]]
               name-header-key [[:parallel
                                 [[:comment '(nil tag)]]
                                 name-referents]
                                :foo]
               name-id-header-key [[:parallel
                                    [[:comment '(nil tag)]]
                                    name-id-referents]
                                   :foo]]
           [:div {:class "table" :key [(item-referent table) :foo]}
            [:div {:class "column-header-sequence"}
             [:div {:class "column-header-stack"}
              [:div {:class "column-header-container tags"
                     :style {:width "200px"}}
               [:component {:key (prepend-to-key (item-referent name-tag)
                                                 name-header-key)
                            :sibling-elements ['tag]
                            :class "column-header"}
                 [item-DOM
                  name-tag name-header-key #{name-tag-spec}
                  {:level 0, :depth 0, :do-not-merge #{}}]]]
              [:div {:class "column-header-sequence"}
               [:div  {:class "column-header-container tags"
                       :style {:width "100px"}}
                [:div  {:class "editable column-header"
                        :key [[:parallel
                               [[:elements '(nil tag)]]
                               just-name-referents]
                              :foo]}]]
               [:div {:class "column-header-container tags"
                      :style {:width "100px"}}
                 [:component {:key (prepend-to-key (item-referent id-tag)
                                                   name-id-header-key)
                              :sibling-elements ['tag]
                              :class "column-header"}
                  [item-DOM
                   id-tag name-id-header-key #{id-tag-spec}
                   {:level 1, :depth 0, :do-not-merge #{}}]]]]]]
            ;; Joe
            [:div {:class "table-row"}
             [:component {:key (into [(:item-id joe-nickname)
                                      (comment-referent
                                       (item-referent name))]
                                      joe-key)
                           :class "table-cell"
                          :sibling-elements [["name" 'tag]]}
               [item-DOM
                joe-nickname (into [(comment-referent (item-referent name))]
                               joe-key)
                (set joe-nickname-tags)
                {:depth 0, :do-not-merge #{}}]]
             ;; Joe's id
             [:component {:key (into [(:item-id joe-id)
                                      (comment-referent
                                       (item-referent name-id))]
                                      joe-key)
                          :class "table-cell"
                          :sibling-elements (as-set [["id" 'tag]
                                                     ["name" 'tag]])}
               [item-DOM
                joe-id (into [(comment-referent (item-referent name-id))]
                               joe-key)
                (set joe-id-tags)
                {:depth 0, :do-not-merge #{}}]]]
            ;; Jane
            [:div {:class "table-row"}
             [:component (any) (any)]
             ;; No name-id value.
             [:div {:class "editable table-cell"
                    :key (into [(elements-referent '(nil ("id" tag)
                                                         ("name" tag)))
                                (comment-referent (item-referent name-id))]
                               jane-key)}]]]))))
  (let [[dom table joe jane]
        (let-mutated [table `("table"
                              (:none :row-query)
                              ((:none ("name" ~'tag (~o1 :order))
                                      ("id" ~'tag (~o2 :order))
                                      (~o1 :order))
                               :column :c1)
                              ((:none ("name" ~'tag (~o1 :order))
                                      (~o2 :order))
                               :column :c2))
                      joe `("Joe"
                            (~o1 :order)
                            (:top-level :non-semantic)
                            ("Joseph"
                             ("name" ~'tag) ("id" ~'tag) (~o1 :order))
                            ("Joe"
                             ("name" ~'tag) (~o2 :order))
                            (45 ("age" ~'tag) (~o2 :order)))
                      jane `("Jane"
                             (~o2 :order)
                             (:top-level :non-semantic)
                             ("Jane" ("name" ~'tag) ("id ~'tag") (~o1 :order))
                             (44 ("age" ~'tag) (~o2 :order)))]
          (expr-let [dom (table-DOM table [:foo] {:depth 0 :do-not-merge #{}})]
            [dom table joe jane]))
        query (current-value (entity/label->content table :row-query))
        name (first (current-value (label->elements table :c2)))
        name-content (current-value (entity/content name))
        name-id (first (current-value (label->elements table :c1)))
        name-id-content (current-value (entity/content name-id))
        name-tag (first (current-value (label->elements name-id-content o1)))
        name-tag-order (first (current-value (label->elements name-tag :order)))
        name-tag-spec (first
                       (remove #{name-tag-order}
                               (current-value (entity/elements name-tag))))
        id-tag (first (current-value (label->elements name-id-content o2)))
        id-tag-order (first (current-value (label->elements id-tag :order)))
        id-tag-spec (first
                     (remove #{id-tag-order}
                             (current-value (entity/elements id-tag))))
        joe-id (first (current-value
                       (label->elements joe "id")))
        joe-id-tags (current-value
                     (label->elements joe-id 'tag))
        joe-nickname (first (current-value
                             (label->elements joe o2)))
        joe-nickname-tags (current-value
                           (label->elements joe-nickname 'tag))]
    (is (check-keys dom joe))
    (is (check
         dom
         (let [joe-key [(item-referent joe) :foo]
               jane-key [(item-referent jane) :foo]
               query-list '(nil (:top-level :non-semantic))
               name-referents [(key-referent [(content-referent)
                                              (item-referent name-id)
                                              (item-referent table)])
                               (key-referent [(content-referent)
                                              (item-referent name)
                                              (item-referent table)])
                               [:parallel
                                [(elements-referent
                                  (item-referent name-content))]
                                [(query-referent query-list)]]]
               just-name-referents [(key-referent [(content-referent)
                                                   (item-referent name)
                                                   (item-referent table)])
                                    [:parallel
                                     [[:parallel
                                       []
                                       [(elements-referent
                                         (item-referent name-content))]
                                       [(elements-referent
                                         (item-referent name-id-content))]]]
                                     [(query-referent query-list)]]]
               name-id-referents [(key-referent [(content-referent)
                                                 (item-referent name-id)
                                                 (item-referent table)])
                                  [:parallel
                                   [(elements-referent
                                     (item-referent name-id-content))]
                                   [(query-referent query-list)]]]
               name-header-key [[:parallel
                                 [[:comment '(nil tag)]]
                                 name-referents]
                                :foo]
               name-id-header-key [[:parallel
                                    [[:comment '(nil tag)]]
                                    name-id-referents]
                                   :foo]]
           [:div {:class "table" :key [(item-referent table) :foo]}
            [:div {:class "column-header-sequence"}
             [:div {:class "column-header-stack"}
              [:div {:class "column-header-container tags"
                     :style {:width "200px"}}
               [:component {:key (prepend-to-key (item-referent name-tag)
                                                 name-header-key)
                            :sibling-elements ['tag]
                            :class "column-header"}
                [item-DOM
                 name-tag name-header-key #{name-tag-spec}
                 {:level 0, :depth 0, :do-not-merge #{}}]]]
              [:div {:class "column-header-sequence"}
               [:div {:class "column-header-container tags"
                      :style {:width "100px"}}
                [:component {:key (prepend-to-key (item-referent id-tag)
                                                  name-id-header-key)
                             :sibling-elements ['tag]
                             :class "column-header"}
                 [item-DOM
                  id-tag name-id-header-key #{id-tag-spec}
                  {:level 1, :depth 0, :do-not-merge #{}}]]]
               [:div  {:class "column-header-container tags"
                       :style {:width "100px"}}
                [:div  {:class "editable column-header"
                        :key [[:parallel
                               [[:elements '(nil tag)]]
                               just-name-referents]
                              :foo]}]]]]]
            ;; Joe
            [:div {:class "table-row"}
             [:component {:key (into [(:item-id joe-id)
                                      (comment-referent
                                       (item-referent name-id))]
                                     joe-key)
                          :class "table-cell"
                          :sibling-elements (as-set [["id" 'tag]
                                                     ["name" 'tag]])}
              [item-DOM
               joe-id (into [(comment-referent (item-referent name-id))]
                            joe-key)
               (set joe-id-tags)
               {:depth 0, :do-not-merge #{}}]]
             ;; Joe's id
             [:component {:key (into [(:item-id joe-nickname)
                                      (comment-referent
                                       (item-referent name))]
                                     joe-key)
                          :class "table-cell"
                          :sibling-elements [["name" 'tag]]}
              [item-DOM
               joe-nickname (into [(comment-referent (item-referent name))]
                                  joe-key)
               (set joe-nickname-tags)
               {:depth 0, :do-not-merge #{}}]]]
            ;; Jane
            [:div {:class "table-row"}
             [:div {:class "editable table-cell"
                    :key (into [(elements-referent '(nil ("id" tag)
                                                         ("name" tag)))
                                (comment-referent (item-referent name-id))]
                               jane-key)}]
             ;; No name-id value.
             [:component (any) (any)]]])))))

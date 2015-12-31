(ns cosheet.server.render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [utils :refer [multiset-diff]]
             [orderable :as orderable]
             [mutable-set :refer [new-mutable-set mutable-set-swap!]]
             [entity :as entity  :refer [to-list description->entity]]
             [reporters :as reporter]
             [expression :refer [expr expr-let expr-seq]]
             [debug :refer [current-value let-mutated envs-to-list]]
             [expression-manager :refer [new-expression-manager-data
                                         request compute]] 
             [expression-manager-test :refer [check-propagation]]
             entity-impl
             [store :refer [new-element-store]]
             store-impl
             [store-utils :refer [add-entity]]
             mutable-store-impl
             [test-utils :refer [check any as-set]])
            (cosheet.server
             [key :refer [item-referent comment-referent
                          content-location-referent query-referent
                          key-referent content-referent
                          canonicalize-list prepend-to-key]]
             [render :refer :all])
                                        ; :reload
            ))

;;; TODO: Write a key checker that walks a DOM, finds all the keys,
;;; and makes sure that item ids follow up the subject/containment hierarchy.

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
  (is (check (append-to-hierarchy [] {:a 1} :i)
             [{:info {:a 1} :members [:i]}]))
  (is (check (append-to-hierarchy [{:info {:a 1} :members [:i]}] {:a 1} :j)
             [{:info {:a 1} :members [:i :j]}]))
  (is (check (append-to-hierarchy [{:info {:a 1} :members [:i]}] {:b 1} :j)
             [{:info {:a 1} :members [:i]} {:info {:b 1} :members [:j]}]))
  (is (check (append-to-hierarchy [{:info {:a 1} :members [:i]}] {:a 1 :b 1} :j)
             [{:info {:a 1} :members [:i]
               :children [{:info {:b 1} :members [:j]}]}]))
  (is (check (append-to-hierarchy [{:info {:a 1 :b 1} :members [:i]}]
                                  {:a 1} :j)
             [{:info {:a 1} :members [] :children [{:info {:b 1} :members [:i]}
                                                   {:info {} :members [:j]}]}]))
  (is (check
       (append-to-hierarchy [{:info {:a 1 :b 1} :members [:i]}]
                            {:a 1 :c 1} :j)
       [{:info {:a 1} :members [] :children [{:info {:b 1} :members [:i]}
                                             {:info {:c 1} :members [:j]}]}]))
  (is (check (append-to-hierarchy [{:info {:a 1}
                                    :members [:i]
                                    :children [{:info {:b 1} :members [:j]}]}]
                                  {:a 1 :b 1 :c 1} :k)
             [{:info {:a 1}
               :members [:i]
               :children [{:info {:b 1}
                           :members [:j]
                           :children [{:info {:c 1} :members [:k]}]}]}]))
  (is (check (append-to-hierarchy [{:info {:a 1}
                                    :members [:i]
                                    :children [{:info {:b 1} :members [:j]}]}]
                                  {:a 1} :k)
             [{:info {:a 1}
               :members [:i]
               :children [{:info {:b 1} :members [:j]}
                          {:info {} :members [:k]}]}]))
  (is (check (append-to-hierarchy [{:info {:a 1 :b 1}
                                    :members [:i]
                                    :children [{:info {:c 1} :members [:j]}]}]
                                  {:a 1 :c 1} :k)
             [{:info {:a 1}
               :members []
               :children [{:info {:b 1}
                           :members [:i]
                           :children [{:info {:c 1} :members [:j]}]}
                          {:info {:c 1} :members [:k]}]}])))

(deftest split-by-subset-test
  (is (check (split-by-subset
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
        [{:info-canonicals [:a] :item :i}
         {:info-canonicals [:a :b] :info :j}
         {:info-canonicals [:a :c] :info :k}]
        #{})
       [{:info {:a 1}
         :members [{:info-canonicals [:a] :item :i}]
         :children [{:info {:b 1}
                     :members [{:info-canonicals [:a :b] :info :j}]}
                    {:info {:c 1}
                     :members [{:info-canonicals [:a :c] :info :k}]}]}])))

(deftest hierarchy-node-descendants-test
  (is (check (set (hierarchy-node-descendants
                   {:info {:a 1}
                    :members [:i]
                    :children [{:info {:b 1} :members [:j]}]}))
             #{:i :j})))

(deftest flatten-hierarchy-test
  (is (check (flatten-hierarchy
              [{:info {:a 1}
                :members [:i]
                :children [{:info {:b 1}
                            :members [:j]
                            :children [{:info {:c 1} :members [:l]}]}
                           {:info {:c 1}
                            :members [:k]}]}]
              0 {})
             [{:info {:a 1}
               :cumulative-info {:a 1}
               :depth 0
               :members [:i]
               :children [{:info {:b 1}
                           :members [:j]
                           :children [{:info {:c 1} :members [:l]}]}
                          {:info {:c 1}
                           :members [:k]}]}
              {:info {:b 1}
               :cumulative-info {:a 1 :b 1}
               :depth 1
               :members [:j]
               :children [{:info {:c 1} :members [:l]}]}
              {:info {:c 1}
               :cumulative-info {:a 1 :b 1 :c 1}
               :depth 2 :members [:l]}
              {:info {:c 1}
               :cumulative-info {:a 1 :c 1}
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
        gender (first (current-value (entity/label->elements joe o1)))
        bogus-age (first (current-value
                          (entity/label->elements joe "doubtful")))
        age (first (remove #{bogus-age}
                           (current-value
                            (entity/label->elements joe "age"))))
        bogus-age-tag (first (current-value
                              (entity/label->elements bogus-age 'tag)))
        age-tag (first (current-value (entity/label->elements age 'tag)))]
    (println "starting check")
    (is (check
         (let [hierarchy (current-value
                          (items-hierarchy-by-condition
                           [gender age bogus-age] #{} '(nil tag)))]
           (println "got hierarchy")
           (flatten-hierarchy-add-row-header-border-info hierarchy))
         [{:depth 0 :top-border :full :bottom-border :corner
           :info {}
           :cumulative-info {}
           :members [{:item gender, :info-elements '() :info-canonicals nil}]}
          {:depth 0 :for-multiple true :top-border :full :bottom-border :full
           :info {["age" {'tag 1}] 1}
           :cumulative-info {["age" {'tag 1}] 1}
           :members [{:item bogus-age
                      :info-elements [bogus-age-tag]
                      :info-canonicals [["age" {'tag 1}]]}
                     {:item age
                      :info-elements [age-tag]
                      :info-canonicals [["age" {'tag 1}]]}]}]))))

(def t1 (add-entity (new-element-store) nil 'joe))
(def store (first t1))
(def rid (second t1))
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
        doubtful (first (current-value (entity/label->elements age o1)))
        confidence (first (current-value
                           (entity/label->elements doubtful 'tag)))
        confidence-tag (first (current-value (entity/elements confidence)))
        item-key [(item-referent age) :age]
        tag-key (into [[:comment [nil 'tag]] (:item-id doubtful)] item-key)]
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
        doubtful (first (current-value (entity/label->elements age o1)))
        item-key [(item-referent age) :age]]
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
        male (first (current-value (entity/label->elements joe o1)))
        married (first (current-value (entity/label->elements joe o2)))
        bogus-age (first (current-value
                          (entity/label->elements joe "doubtful")))
        bogus-age-tag (first (current-value
                              (entity/label->elements bogus-age 'tag)))
        bogus-age-tag-spec (first (current-value
                                   (entity/elements bogus-age-tag)))
        age (first (remove #{bogus-age}
                           (current-value
                            (entity/label->elements joe "age"))))
        age-tag (first (current-value (entity/label->elements age 'tag)))
        age-tag-spec (first (current-value (entity/elements age-tag)))]
    (let [md (new-expression-manager-data)
          item-key [(item-referent joe) :joe]
          both-ages-ref [:parallel [] [(:item-id bogus-age) (:item-id age)]]
          both-ages-key (into [both-ages-ref] item-key)]
      (request dom-reporter md)
      (compute md)
      (check-propagation dom-reporter)
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
        v1 (first (current-value (entity/label->elements joe o1)))
        v12 (first (current-value (entity/label->elements joe o2)))
        v13 (first (current-value (entity/label->elements joe o3)))
        L1 (first (current-value (entity/label->elements v1 'tag)))
        L1-spec (first (current-value (entity/elements L1)))
        L121 (first (current-value (entity/label->elements v12 o1)))
        L2 (first (current-value (entity/label->elements v12 o2)))
        L2-spec (first (remove #{(first (current-value
                                         (entity/label->elements L2 :order)))}
                               (current-value (entity/elements L2))))
        L131 (first (current-value (entity/label->elements v13 o1)))
        L3 (first (current-value (entity/label->elements v13 o2)))
        L3-spec (first (remove #{(first (current-value
                                         (entity/label->elements L3 :order)))}
                               (current-value (entity/elements L3))))]
    (let [md (new-expression-manager-data)]
      (request dom-reporter md)
      (compute md))
    (check-propagation dom-reporter)
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
        va (first (current-value (entity/label->elements joe o2)))
        vb (first (current-value (entity/label->elements joe o3)))
        La1 (first (current-value (entity/label->elements va o1)))
        La2 (first (current-value (entity/label->elements va o2)))
        La1-spec (first (remove #{(first (current-value
                                         (entity/label->elements La1 :order)))}
                                (current-value (entity/elements La1))))
        La2-spec (first (remove #{(first (current-value
                                         (entity/label->elements La2 :order)))}
                               (current-value (entity/elements La2))))
        Lb1 (first (current-value (entity/label->elements vb o1)))
        Lb1-spec (first (remove #{(first (current-value
                                          (entity/label->elements Lb1 :order)))}
                                (current-value (entity/elements Lb1))))]
    (let [md (new-expression-manager-data)]
      (request dom-reporter md)
      (compute md))
    (check-propagation dom-reporter)
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
        age (first (current-value (entity/label->elements table :c1)))
        age-content (current-value (entity/content age))
        age-tag (first
                 (current-value (entity/label->elements age-content 'tag)))
        age-tag-spec (first (current-value (entity/elements age-tag)))
        size (first (current-value (entity/label->elements table :c2)))
        joe-bogus-age (first (current-value
                              (entity/label->elements joe "doubtful")))
        joe-bogus-age-tag (first (current-value
                                  (entity/label->elements joe-bogus-age 'tag)))
        joe-bogus-age-tag-spec (first (current-value
                                       (entity/elements joe-bogus-age-tag)))
        joe-age (first (remove #{joe-bogus-age}
                               (current-value
                                (entity/label->elements joe "age"))))]
    (is (check
         dom
         (let [item-key [(item-referent table) :foo]
               joe-key [(item-referent joe) :foo]
               age-header-key (into [[:parallel
                                      [[:comment '(nil tag)]]
                                      [(query-referent (:item-id query))
                                       (key-referent [(content-referent)
                                                      (item-referent age)
                                                      (item-referent table)])]]]
                                    item-key)]
           [:div {:class "table" :key item-key}
            [:div {:class "column_header_sequence"}
             [:div {:class "column_header_container tags"}
              [:component {:key (prepend-to-key (item-referent age-tag)
                                                age-header-key)
                           :sibling-elements ['tag]
                           :class "column_header"}
               [item-DOM
                age-tag age-header-key #{age-tag-spec}
                {:level 0, :depth 0, :do-not-merge #{}}]]]
             ;; Ignore second column.
             (any)]
            [:div {:class "table_row"}
             [:div {:class "table_cell"}
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
                    :class "editable table_cell"}]]])))))

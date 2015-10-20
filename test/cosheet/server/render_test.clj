(ns cosheet.server.render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [orderable :as orderable]
             [entity :as entity  :refer [to-list description->entity]]
             [reporters :as reporter :refer [expr expr-let expr-seq]]
             [debug :refer [current-value let-propagated envs-to-list]]
             [computation-manager :refer [new-management request compute]] 
             [computation-manager-test :refer [check-propagation]]
             entity-impl
             store-impl
             mutable-store-impl)
            [cosheet.server.render :refer :all]
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
(def jane `("Jane" (~o1 :order) "plain" "plain"))
(def joe `("Joe"
           (~o2 :order)
           ("male" (~o1 :order))
           ("married" (~o2 :order))
           (39 (~o3 :order)
               ("age" ~'tag)
               ("doubtful" "confidence"))
           (45 (~o4 :order)
               ("age" ~'tag))))

(deftest prepend-to-key-test
  (let [a (item-referent :a)
        b (item-referent :b)
        c (item-referent :c)
        p (parallel-referent [] [b])
        pp (parallel-referent [p a] [b])]
    (is (= (prepend-to-key a [b]) [a b]))
    (is (= (prepend-to-key c [p a])
           [(parallel-referent [c] [b]) a]))
    (is (= (prepend-to-key c [pp a])
           [(parallel-referent [(parallel-referent [c] [b]) a]
                               [b])
            a]))))

(deftest visible-test
  (let [visible (let-propagated [him joe]
                  (visible-to-list him))]
    (is (= (first visible) "Joe"))
    (is (= (set (map canonicalize-list (rest visible)))
           #{"male"
             "married"
             '(39 {["age" {tag 1}] 1
                   ("doubtful" {"confidence" 1}) 1})
             '(45 {["age" {tag 1}] 1})})))
  (is (= (set (map canonicalize-list
                   (let-propagated [him joe]
                     (expr-seq map to-list (visible-elements him)))))
         (set (map canonicalize-list (rest (rest joe)))))))

(deftest tag-specifiers-test
  (is (= (map canonicalize-list
              (let-propagated [test '("age" tag "not-tag")]
                (expr-seq map to-list (tag-specifiers test))))
         ['tag])))

(deftest canonical-info-set-test
  (is (= (let-propagated [him joe, her jane]
           (canonical-info-set [him her]))
         {["Joe"
           {"married" 1
            "male" 1
            [39 {["age" {'tag 1}] 1, ["doubtful" {"confidence" 1}] 1}] 1
            [45 {["age" {'tag 1}] 1}] 1}]1
            ["Jane" {"plain" 2}] 1})))

(deftest canonical-info-set-diff-test
  (is (= (canonical-info-set-diff {:a 1 :b 3 :c 3} {:b 1 :c 3 :d 4})
         [{:a 1 :b 2} {:d 4} {:b 1 :c 3}])))

(deftest append-to-hierarchy-test
  (is (= (append-to-hierarchy [] {:a 1} :i)
         [{:info {:a 1} :members [:i]}]))
  (is (= (append-to-hierarchy [{:info {:a 1} :members [:i]}] {:a 1} :j)
         [{:info {:a 1} :members [:i :j]}]))
  (is (= (append-to-hierarchy [{:info {:a 1} :members [:i]}] {:b 1} :j)
         [{:info {:a 1} :members [:i]} {:info {:b 1} :members [:j]}]))
  (is (= (append-to-hierarchy [{:info {:a 1} :members [:i]}] {:a 1 :b 1} :j)
         [{:info {:a 1} :members [:i]
           :children [{:info {:b 1} :members [:j]}]}]))
  (is (= (append-to-hierarchy [{:info {:a 1 :b 1} :members [:i]}]
                              {:a 1} :j)
         [{:info {:a 1} :members [] :children [{:info {:b 1} :members [:i]}
                                             {:info {} :members [:j]}]}]))
  (is (= (append-to-hierarchy [{:info {:a 1 :b 1} :members [:i]}]
                              {:a 1 :c 1} :j)
         [{:info {:a 1} :members [] :children [{:info {:b 1} :members [:i]}
                                             {:info {:c 1} :members [:j]}]}]))
  (is (= (append-to-hierarchy [{:info {:a 1}
                                :members [:i]
                                :children [{:info {:b 1} :members [:j]}]}]
                              {:a 1 :b 1 :c 1} :k)
         [{:info {:a 1}
           :members [:i]
           :children [{:info {:b 1}
                       :members [:j]
                       :children [{:info {:c 1} :members [:k]}]}]}]))
    (is (= (append-to-hierarchy [{:info {:a 1}
                                  :members [:i]
                                  :children [{:info {:b 1} :members [:j]}]}]
                                {:a 1} :k)
         [{:info {:a 1}
           :members [:i]
           :children [{:info {:b 1} :members [:j]}
                      {:info {} :members [:k]}]}])))

(deftest hierarchy-by-info-test
  (is (= (hierarchy-by-info [[{:a 1} :i] [{:a 1 :b 1} :j] [{:a 1 :c 1} :k]])
         [{:info {:a 1} :members [:i]
           :children [{:info {:b 1} :members [:j]}
                      {:info {:c 1} :members [:k]}]}])))

(deftest hierarchy-node-descendants-test
  (is (= (set (hierarchy-node-descendants
               {:info {:a 1}
                :members [:i]
                :children [{:info {:b 1} :members [:j]}]}))
         #{:i :j})))

(deftest flatten-hierarchy-test
  (is (= (flatten-hierarchy
          [{:info {:a 1}
            :members [:i]
            :children [{:info {:b 1}
                        :members [:j]
                        :children [{:info {:c 1} :members [:l]}]}
                       {:info {:c 1}
                        :members [:k]}]}]
          0)
         [{:info {:a 1}
           :depth 0
           :members [:i]
           :children [{:info {:b 1}
                       :members [:j]
                       :children [{:info {:c 1} :members [:l]}]}
                      {:info {:c 1}
                       :members [:k]}]}
          {:info {:b 1}
           :depth 1
           :members [:j]
           :children [{:info {:c 1} :members [:l]}]}
          {:info {:c 1}
           :depth 2 :members [:l]}
          {:info {:c 1} :depth 1 :members [:k]}])))

(deftest items-for-info-test
  (is (= (set (items-for-info {:a 1 :b 2}
                              [:a1 :a2 :a3 :b1 :b2 :b3]
                              [:a :a :a :b :b :b]))
         #{:a3 :b3 :b2})))

(deftest hierarchy-node-to-row-info-test
  (let [him (let-propagated [him joe] him)
        bogus-age (first (current-value
                          (entity/label->elements him "doubtful")))
        age (first (remove #{bogus-age}
                           (current-value
                            (entity/label->elements him "age"))))
        bogus-age-tag (first (current-value
                              (entity/label->elements bogus-age 'tag)))
        age-tag (first (current-value (entity/label->elements age 'tag))) ]
    (is (= (current-value
            (hierarchy-node-to-row-info
             {:depth 0 :top-border :x :bottom-border :y
              :info {["age" {'tag 1}] 1}
              :members [{:item bogus-age
                         :tag-items [bogus-age-tag]
                         :tag-canonicals [["age" {'tag 1}]]}
                        {:item age
                         :tag-items [age-tag]
                         :tag-canonicals [["age" {'tag 1}]]}]}))
            [{:depth 0 :top-border :x :bottom-border :y}
            [bogus-age-tag]
            [[bogus-age [bogus-age-tag]] [age [age-tag]]]
            [["age" 'tag]]
            [bogus-age age]]))))

(deftest add-border-info-test
  (is (= (add-border-info
          [{:depth 0}
           {:depth 1}
           {:depth 2}
           {:depth 2}
           {:depth 1}])
         [{:depth 0 :top-border :top-level}
          {:depth 1 :top-border :nested :bottom-border :nested}
          {:depth 2 :bottom-border :nested}
          {:depth 2}
          {:depth 1 :top-border :nested}])))

(deftest tagged-items-hierarchy-test
  (let [him (let-propagated [him joe] him)
        gender (first (current-value (entity/label->elements him o1)))
        bogus-age (first (current-value
                          (entity/label->elements him "doubtful")))
        age (first (remove #{bogus-age}
                           (current-value
                            (entity/label->elements him "age"))))
        bogus-age-tag (first (current-value
                              (entity/label->elements bogus-age 'tag)))
        age-tag (first (current-value (entity/label->elements age 'tag)))]
    (is (= (current-value (tagged-items-hierarchy [gender age bogus-age]))
           [{:depth 0 :top-border :top-level
             :info {}
             :members [{:item gender, :tag-items nil, :tag-canonicals nil}]}
            {:depth 0 :top-border :top-level :bottom-border :top-level
             :info {["age" {'tag 1}] 1}
             :members [{:item bogus-age
                        :tag-items [bogus-age-tag]
                        :tag-canonicals [["age" {'tag 1}]]}
                       {:item age
                        :tag-items [age-tag]
                        :tag-canonicals [["age" {'tag 1}]]}]}]))))

(deftest tags-DOM-test
  (is (= (tags-DOM {:depth 0} nil [:k] {}) [:div {:class "tags column editable"
                                                  :key [[:condition 'tag] :k]
                                                  :row-sibling [:k]}]))
  (let [[dom fred fred-tag]
        (let-propagated [fred '("Fred" tag)]
          (expr-let [dom (tags-DOM {:depth 0 :top-border :nested}
                                   [fred] [:k] {:depth 0})
                     fred-elements (entity/elements fred)]
            [dom fred (first fred-elements)]))]
    (is (= dom
           [:component {:key [(:item-id fred) [:condition 'tag] :k]
                        :class "tags column top-border nested"
                        :sibling-elements ['tag]
                        :row-sibling [:k]}
            [item-DOM
             fred [(:item-id fred) [:condition 'tag] :k]
             #{fred-tag} {:depth 0}]])))
  (let [[dom fred fran]
        (let-propagated [fred '("Fred" tag)
                         fran "Fran"]
          (expr-let [dom (tags-DOM {:depth 0} [fred fran] [:k] {:depth 1})]
            [dom fred fran]))
        fred-tag (first (current-value (entity/elements fred)))]
    (is (= dom
           [:div
            {:class "tags column"
             :key [[:condition 'tag] :k]
             :row-sibling [:k]}
            [:component {:key [(:item-id fred) [:condition 'tag] :k]
                         :style {:display "block"
                                 :width "100%"}
                                      :class "vertical-separated"
                         :sibling-elements ['tag]
                         :row-sibling [:k]}
             [item-DOM
              fred [(:item-id fred) [:condition 'tag] :k]
              #{fred-tag} {:depth 1}]]
            [:component {:key [(:item-id fran) [:condition 'tag] :k]
                         :style  {:display "block"
                                  :width "100%"}
                         :class "vertical-separated"
                         :sibling-elements ['tag]
                         :row-sibling [:k]}
             [item-DOM
              fran [(:item-id fran) [:condition 'tag] :k]
              #{} {:depth 1}]]]))))

(deftest item-DOM-test
  (let [[dom fred]
        (let-propagated [fred "Fred"]
          (expr-let [dom (item-DOM fred [(:item-id fred)] #{} {:depth 0})]
            [dom fred]))]
    (is (= dom
           [:div {:class "item content-text editable"
                  :key [(:item-id fred)]} "Fred"])))
  ;; Check generation of a single tag for a single item.
  (let [[dom age]
        (let-propagated [age `(39 ("doubtful"
                                   ("confidence" ~'tag)
                                   (~o1 :order)))]
          (expr-let [dom (item-DOM age [:age] #{} {:depth 0})]
            [dom age]))
        doubtful (first (current-value (entity/label->elements age o1)))
        confidence (first (current-value
                           (entity/label->elements doubtful 'tag)))
        confidence-tag (first (current-value (entity/elements confidence)))
        tag-key [(:item-id confidence)
                 [:condition 'tag]
                 (:item-id doubtful)
                 :age]]
    (is (= dom
           [:div {:class "item with-elements" :key [:age]}
            [:div {:style {:width "100%" :display "block"}
                   :class "content-text editable"
                   :key [[:content] :age]}
             "39"]
            [:div {:style {:height "1px"
                           :display "table"
                           :table-layout "fixed"
                           :width "100%"}
                   :class "element-table"}
             [:div {:style {:display "table-row"}
                    :class "last-row"}
              [:component {:key tag-key
                           :style {:display "table-cell"}
                           :class "tags column top-border bottom-border"
                           :sibling-elements ['tag]
                           :row-sibling [(:item-id doubtful) :age]}
               [item-DOM
                confidence tag-key
                #{confidence-tag} {:depth 1}]]
              [:component {:key [(:item-id doubtful) :age]
                           :class "items column"
                           :style {:display "table-cell"}
                           :sibling-elements [["confidence" 'tag]]}
               [item-DOM
                doubtful [(:item-id doubtful) :age]
                #{confidence} {:depth 1}]]]]])))
  ;; Check that we generate no-tags.
  (let [[dom age]
        (let-propagated [age `(39 ("doubtful" (~o1 :order)))]
          (expr-let [dom (item-DOM age [:age] #{} {:depth 0})]
            [dom age]))
        doubtful (first (current-value (entity/label->elements age o1)))]
    (is (= dom
           [:div {:class "item with-elements" :key [:age]}
            [:div {:style {:width "100%" :display "block"}
                   :class "content-text editable"
                   :key [[:content] :age]}
             "39"]
            [:div {:style {:height "1px"
                           :display "table"
                           :table-layout "fixed"
                           :width "100%"}
                   :class "element-table"}
             [:div {:style {:display "table-row"}
                    :class "no-tags last-row"}
              [:div {:style {:display "table-cell"}
                     :class "tags column editable top-border bottom-border"
                     :key [[:condition 'tag] (:item-id doubtful) :age]
                     :row-sibling [(:item-id doubtful) :age]}]
              [:component {:key [(:item-id doubtful) :age]
                           :class "items column"
                           :style {:display "table-cell"}
                           :sibling-elements nil}
               [item-DOM
                doubtful [(:item-id doubtful) :age]
                #{} {:depth 1}]]]]])))
  (let [[dom-reporter joe]
        (let-propagated [him joe]
          (expr identity [(item-DOM him [:joe] #{} {:depth 0}) him]))
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
    (let [m (new-management)]
      (request dom-reporter m)
      (compute m))
    (check-propagation #{} dom-reporter)
    (is (= (reporter/value dom-reporter)
           (let [both-ages-ref [:parallel
                                [(:item-id bogus-age-tag) [:condition 'tag]]
                                [(:item-id bogus-age) (:item-id age)]]]
             [:div {:class "item with-elements" :key [:joe]}
              [:div {:style {:width "100%" :display "block"}
                     :class "content-text editable"
                     :key [[:content] :joe]}
               "Joe"]
              [:div {:style {:width "100%"
                             :height "1px"
                             :display "table"
                             :table-layout "fixed"}
                     :class "element-table"}
               [:div {:style {:display "table-row"}}
                [:div {:style {:display "table-cell"}
                       :class "tags column editable top-border"
                       :key [[:condition 'tag] (:item-id male) :joe]
                       :row-sibling [(:item-id male) :joe]}]
                [:component {:key [(:item-id male) :joe]
                             :style {:display "table-cell"}
                             :class "items column"
                             :sibling-elements nil}
                 [item-DOM
                  male [(:item-id male) :joe]
                  #{} {:depth 1}]]]
               [:div {:style {:display "table-row"}}
                [:div {:style {:display "table-cell"}
                       :class "tags column editable top-border"
                       :key [[:condition 'tag] (:item-id married) :joe]
                       :row-sibling [(:item-id married) :joe]}]
                [:component {:key [(:item-id married) :joe]
                             :style {:display "table-cell"}
                             :class "items column"
                             :sibling-elements nil}
                 [item-DOM
                  married [(:item-id married) :joe]
                  #{} {:depth 1}]]]
               [:div {:style {:display "table-row"}
                      :class "last-row"}
                [:component
                 {:key [both-ages-ref :joe]
                  :style {:display "table-cell"}
                  :class "tags column top-border bottom-border for-multiple-items"
                  :sibling-elements ['tag]
                  :row-sibling [[:parallel
                                 [] [(:item-id bogus-age) (:item-id age)]]
                                :joe]}
                 [item-DOM
                  bogus-age-tag
                  [both-ages-ref :joe]
                  #{bogus-age-tag-spec} {:depth 1}]]
                [:div {:style {:display "table-cell"}
                       :class "items column"}
                 [:component {:key [(:item-id bogus-age) :joe]
                              :style {:width "100%"
                                      :display "block"}
                              :class "vertical-separated"
                              :sibling-elements [["age" 'tag]]}
                  [item-DOM
                   bogus-age
                   [(:item-id bogus-age) :joe]
                   #{bogus-age-tag} {:depth 1}]]
                 [:component {:key [(:item-id age) :joe]
                              :style {:width "100%"
                                      :display "block"}
                              :class "vertical-separated"
                              :sibling-elements [["age" 'tag]]}
                  [item-DOM
                   age [(:item-id age) :joe]
                   #{age-tag} {:depth 1}]]]]]]))))  
  ;; TODO: Test content that is an item.
  )

(ns cosheet.server.render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [orderable :as orderable]
             [entity :as entity  :refer [to-list description->entity]]
             [reporters :refer [expr expr-let expr-seq]]
             [debug :refer [current-value envs-to-list
                            let-propagated let-propagated-store]]
             entity-impl
             store-impl
             mutable-store-impl)
            [cosheet.server.render :refer :all]
            ; :reload
            ))

(defn canonicalize
  "Turn a list form of an entity into a canonical representation,
   that is insensitive to the order of elements. (We record the elements in
   a map from entity to multiplicity.)"
  [entity]
  (if (sequential? entity)
    [(first entity)
     (reduce (fn [ms entity]
               (update-in ms [(canonicalize entity)] #((fnil + 0) % 1)))
             {} (rest entity))]
    entity))

(deftest render-test
  (let [unused-orderable orderable/initial
        [o1 unused-orderable] (orderable/split unused-orderable :after)
        [o2 unused-orderable] (orderable/split unused-orderable :after)
        [o3 unused-orderable] (orderable/split unused-orderable :after)
        [o4 unused-orderable] (orderable/split unused-orderable :after)
        jane `("Jane" (~o1 :order) "plain" "plain")
        joe `("Joe"
              (~o2 :order)
              ("male" (~o1 :order))
              ("married" (~o2 :order))
              (39 (~o3 :order)
                  ("age" ~'tag)
                  ("doubtful" "confidence"))
              (45 (~o4 :order)
                  ("age" ~'tag)))]
    (let [visible (let-propagated [him joe]
                    (visible-to-list him))]
      (is (= (first visible) "Joe"))
      (is (= (set (map canonicalize-list (rest visible)))
             #{"male"
               "married"
               '(39 {["age" {tag 1}] 1
                     ("doubtful" {"confidence" 1}) 1})
               '(45 {["age" {tag 1}] 1})})))
    (is (= (set (map canonicalize
                     (let-propagated [him joe]
                       (expr-seq map to-list (visible-elements him)))))
           (set (map canonicalize (rest (rest joe))))))
    (is (= (map canonicalize
                (let-propagated [test '("age" tag "not-tag")]
                  (expr-seq map to-list (tag-specifiers test))))
           ['tag]))
    (is (= (let-propagated [him joe, her jane]
             (canonical-info-set [him her]))
           {["Joe"
             {"married" 1
              "male" 1
              [39 {["age" {'tag 1}] 1, ["doubtful" {"confidence" 1}] 1}] 1
              [45 {["age" {'tag 1}] 1}] 1}]1
            ["Jane" {"plain" 2}] 1}))
    (is (= (map canonicalize
                (let-propagated [him joe, her jane]
                  (expr-seq map to-list [her him])))
           (map canonicalize [jane joe])))
    (let-propagated [him joe]
      (expr-let [elements (visible-elements him)
                 groups (group-by-tag elements)]
        (is (= (count groups) 3))
        (is (= (set (map count groups)) #{1 2}))
        (is (= (apply + (map #(count (second (first %))) groups)) 1))))
    (is (= (tags-DOM nil [] [:k] {}) [:div {:class "tag"}]))
    (let [[dom fred fred-tag]
          (let-propagated [fred '("Fred" tag)]
            (expr-let [dom (tags-DOM [fred] :k [:k] {:depth 0})
                       fred-elements (entity/elements fred)]
              [dom fred (first fred-elements)]))]
      (is (= dom 
             [:component {:key [{:condition {:elements ['tag] :subject :k}
                                 :item fred}
                                :k]
                          :definition [item-DOM
                                       fred
                                       [{:condition {:elements ['tag]
                                                     :subject :k}
                                         :item fred}
                                        :k]
                                       #{fred-tag} {:depth 0}]
                          :attributes {:class "tag"}}])))
    (let [[dom fred fran]
          (let-propagated [fred '("Fred" tag)
                           fran "Fran"]
            (expr-let [dom (tags-DOM [fred fran] :k [:k] {:depth 1})]
              [dom fred fran]))
          fred-tag (first (current-value (entity/elements fred)))]
      (is (= dom 
             [:div
              {:class "tag"}
              [:component {:key [{:condition {:elements ['tag] :subject :k}
                                  :item fred}
                                 :k]
                           :definition [item-DOM
                                        fred
                                        [{:condition {:elements ['tag]
                                                      :subject :k}
                                          :item fred}
                                         :k]
                                        #{fred-tag} {:depth 1}]
                           :attributes
                           {:style {:display "block"
                                    :width "100%"}
                            :class "vertical-separated"}}]
              [:component {:key [{:condition {:elements ['tag] :subject :k}
                                  :item fran}
                                 :k]
                           :definition [item-DOM
                                        fran
                                        [{:condition {:elements ['tag]
                                                      :subject :k}
                                          :item fran}
                                         :k]
                                        #{} {:depth 1}]
                           :attributes
                           {:style  {:display "block"
                                     :width "100%"}
                            :class "vertical-separated"}}]]
             )))
    (let [[dom fred]
          (let-propagated [fred "Fred"]
            (expr-let [dom (item-DOM fred [fred] #{} {:depth 0})]
              [dom fred]))]
      (is (= dom
             [:div {:class "content-text editable item" :key [fred]} "Fred"]))
      (let [[dom joe]
            (let-propagated [him joe]
              (expr-let [dom (item-DOM him [him] #{} {:depth 0})]
                [dom him]))
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
        (is (= dom
               [:div {:class "item" :key [joe]}
                [:div {:style {:width "100%" :display "block"}
                       :class "content-text editable"}
                 "Joe"]
                [:div {:style {:width "100%"
                               :display "table"
                               :table-layout "fixed"}
                       :class "element-table"}
                 [:div {:style {:display "table-row"}}
                  [:div {:style {:display "table-cell"}
                         :class "tag tag-column"}]
                  [:component {:attributes {:style {:display "table-cell"}
                                            :class "item-column"}
                               :key [{:condition {:elements nil, :subject joe}
                                      :item male}
                                     joe]
                               :definition [item-DOM
                                            male
                                            [{:condition {:elements nil,
                                                          :subject joe}
                                              :item male}
                                             joe]
                                            #{} {:depth 1}]}]]
                 [:div {:style {:display "table-row"}}
                  [:div {:style {:display "table-cell"}
                         :class "tag tag-column"}]
                  [:component {:attributes {:style {:display "table-cell"}
                                            :class "item-column"}
                               :key [{:condition {:elements nil,
                                                  :subject joe}
                                      :item married}
                                     joe]
                               :definition [item-DOM
                                            married
                                            [{:condition {:elements nil,
                                                          :subject joe}
                                              :item married}
                                             joe]
                                            #{} {:depth 1}]}]]
                 [:div {:style {:display "table-row"}
                        :class "last-row"}
                  [:component {:attributes
                               {:style {:display "table-cell"}
                                :class "tag tag-column for-multiple-items"}
                               :key [{:condition {:elements ['tag]
                                                  ;; TODO: subject joe?
                                                   :subject joe}
                                      :item bogus-age-tag}
                                     ;; TODO: parent joe?
                                     joe]
                               :definition [item-DOM
                                            bogus-age-tag
                                            [{:condition {:elements ['tag],
                                                          :subject joe}
                                              :item bogus-age-tag}
                                             joe]
                                            #{bogus-age-tag-spec} {:depth 1}]}]
                  [:div {:style {:display "table-cell"}
                         :class "item-column"}
                   [:component {:attributes {:style {:width "100%"
                                                     :display "block"}
                                             :class "vertical-separated"}
                                :key [{:condition {:elements ['("age" tag)]
                                                   :subject joe}
                                       :item bogus-age}
                                      joe]
                                :definition [item-DOM
                                             bogus-age
                                             [{:condition
                                               {:elements ['("age" tag)]
                                                :subject joe}
                                               :item bogus-age}
                                              joe]
                                             #{bogus-age-tag} {:depth 1}]}]
                   [:component {:attributes {:style {:width "100%"
                                                     :display "block"}
                                             :class "vertical-separated"}
                                :key [{:condition {:elements ['("age" tag)]
                                                   :subject joe}
                                       :item age}
                                      joe]
                                :definition [item-DOM
                                             age
                                             [{:condition
                                               {:elements ['("age" tag)]
                                                :subject joe}
                                               :item age}
                                              joe]
                                             #{age-tag} {:depth 1}]}]]]]]))))
    (let [[dom age]
          (let-propagated [age `(39 ("doubtful" (~o1 :order)))]
            (expr-let [dom (item-DOM age [age] #{} {:depth 0})]
              [dom age]))
          doubtful (first (current-value (entity/label->elements age o1)))
          funny (first (current-value (entity/label->elements age o2)))]
      ;; Check that we generate no-tags.
      (is (= dom
             [:div {:class "item" :key [age]}
              [:div {:style {:width "100%" :display "block"}
                     :class "content-text editable"}
               "39"]
              [:div {:style {:display "table"
                             :table-layout "fixed"
                             :width "100%"}
                     :class "element-table"}
               [:div {:style {:display "table-row"}
                      :class "no-tags last-row"}
                [:div {:style {:display "table-cell"}
                       :class "tag tag-column"}]
                [:component {:attributes {:class "item-column"
                                           :style {:display "table-cell"}}
                              :key [{:item doubtful
                                     :condition {:subject age
                                                 :elements nil}}
                                    age]
                              :definition [item-DOM
                                           doubtful
                                           [{:item doubtful
                                             :condition {:subject age
                                                         :elements nil}}
                                            age]
                                           #{} {:depth 1}]}]]]])))))



(ns cosheet.server.render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet
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
  (let [jane '("Jane" (1 :order) "plain" "plain")
        joe '("Joe"
              (2 :order)
              ("male" (1 :order))
              ("married" (2 :order))
              (39 (3 :order)
                  ("age" tag)
                  ("doubtful" "confidence")))]
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
              [39 {["age" {'tag 1}] 1, ["doubtful" {"confidence" 1}] 1}] 1}] 1
            ["Jane" {"plain" 2}] 1}))
    (is (= (map canonicalize
                (let-propagated [him joe, her jane]
                  (expr-seq map to-list (order-items [him her]))))
           (map canonicalize [jane joe])))
    (let-propagated [him joe]
      (expr-let [elements (visible-elements him)
                 groups (group-by-tag elements)]
        (is (= (count groups) 2))
        (is (= (set (map count groups)) #{1 2}))
        (is (= (apply + (map #(count (second (first %))) groups)) 1))))
    (is (= (tags-DOM [] [:k] {}) [:div {:class "tag"}]))
    (let [[dom fred fred-tag]
          (let-propagated [fred '("Fred" tag)]
            (expr-let [dom (tags-DOM [fred] [:k] {:depth 0})
                       fred-elements (entity/elements fred)]
              [dom fred (first fred-elements)]))]
      (is (= dom 
             [:component {:key [fred :k]
                          :definition [item-DOM
                                       fred [fred :k] #{fred-tag} {:depth 0}]
                          :attributes {:class "tag"}}])))
    (let [[dom fred fran]
          (let-propagated [fred '("Fred" tag)
                           fran "Fran"]
            (expr-let [dom (tags-DOM [fred fran] [:k] {:depth 1})]
              [dom fred fran]))
          fred-tag (first (current-value (entity/elements fred)))]
      (is (= dom 
             [:div
              {:class "tag"}
              [:component {:key [fred :k]
                           :definition [item-DOM
                                        fred [fred :k] #{fred-tag} {:depth 1}]
                           :attributes
                           {:style {:display "block"
                                    :width "100%"}
                            :class "vertical-separated"}}]
              [:component {:key [fran :k]
                           :definition [item-DOM fran [fran :k] #{} {:depth 1}]
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
             [:div {:class "content-text item" :key [fred]} "Fred"])))
    (let [[dom joe]
          (let-propagated [him joe]
            (expr-let [dom (item-DOM him [him] #{} {:depth 0})]
              [dom him]))
          male (first (current-value (entity/label->elements joe 1)))
          married (first (current-value (entity/label->elements joe 2)))
          age (first (current-value (entity/label->elements joe "age")))
          age-tag (first (current-value (entity/label->elements age 'tag)))
          age-tag-spec (first (current-value (entity/elements age-tag)))]
      (is (= dom
             [:div {:class "item" :key [joe]}
              [:div {:style {:width "100%" :display "block"}
                     :class "content-text"}
               "Joe"]
              [:div {:style {:width "100%"
                             :display "table"
                             :table-layout "fixed"}
                     :class "element-table"                     }
               [:div {:style {:display "table-row"}}
                [:div {:style {:display "table-cell"}
                       :class "tag tag-column for-multiple-items"}]
                [:div {:style {:display "table-cell"}
                       :class "item-column"}
                 [:component {:attributes {:style {:width "100%"
                                                   :display "block"}
                                           :class "vertical-separated"}
                              :key [male joe]
                              :definition [item-DOM
                                           male [male joe] #{} {:depth 1}]}]
                 [:component {:attributes {:style {:width "100%"
                                                   :display "block"}
                                           :class "vertical-separated"}
                              :key [married joe]
                              :definition [item-DOM
                                           married [married joe]
                                           #{} {:depth 1}]}]]]
               [:div {:style {:display "table-row"}
                      :class "last-row"}
                [:component {:attributes {:style {:display "table-cell"}
                                          :class "tag tag-column"}
                             :key [age-tag joe]
                             :definition [item-DOM
                                          age-tag [age-tag joe]
                                          #{age-tag-spec} {:depth 1}]}]
                [:component {:attributes {:style {:display "table-cell"}
                                          :class "item-column"}
                             :key [age joe]
                             :definition [item-DOM
                                          age [age joe]
                                          #{age-tag} {:depth 1}]}]]]])))    
    (let [[dom age]
          (let-propagated [age '(39 ("doubtful" (1 :order))
                                    ("funny" (2 :order)))]
            (expr-let [dom (item-DOM age [age] #{} {:depth 0})]
              [dom age]))
          doubtful (first (current-value (entity/label->elements age 1)))
          funny (first (current-value (entity/label->elements age 2)))]
      (is (= dom
             [:div {:class "item" :key [age]}
              [:div {:style {:width "100%" :display "block"}
                     :class "content-text"}
               "39"]
              [:div {:style {:display "table"
                             :table-layout "fixed"
                             :width "100%"}
                     :class "element-table"}
               [:div {:style {:display "table-row"}
                      :class "no-tags last-row"}
                [:div {:style {:display "table-cell"}
                       :class "tag tag-column for-multiple-items"}]
                [:div {:class "item-column"
                       :style {:display "table-cell"}}
                 [:component {:attributes {:class "vertical-separated"
                                           :style {:width "100%"
                                                   :display "block"}}
                              :key [doubtful age]
                              :definition [item-DOM
                                           doubtful [doubtful age]
                                           #{} {:depth 1}]}]
                 [:component {:attributes {:class "vertical-separated"
                                           :style {:width "100%"
                                                   :display "block"}}
                              :key [funny age]
                              :definition [item-DOM
                                           funny [funny age]
                                           #{} {:depth 1}]}]]]]])))))



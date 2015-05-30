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
    (is (= (-> {}
               (into-attributes {:class "foo"
                                 :style {:font "bold"}
                                 :other "abc"})
               (into-attributes {:class "bar"
                                 :style {:text "large"}
                                 :misc "junk"})
               (into-attributes {:class "baz"
                                 :style {:color "red"}}))
           {:class "foo bar baz"
            :style {:font "bold" :text "large" :color "red"}
            :other "abc"
            :misc "junk"}))
    (is (= (-> [:component {:definition [:foo :bar]}]
               (add-attributes {:class "foo"
                                :style {:font "bold"}
                                :other "abc"})
               (add-attributes {:class "bar"
                                :style {:text "large"}
                                :misc "junk"}))
           [:component {:definition [:foo :bar]
                        :attributes {:class "foo bar"
                                     :style {:font "bold" :text "large"}
                                     :other "abc"
                                     :misc "junk"}}]))
    (is (= (-> [:div "hi there"]
               (add-attributes {:class "foo"
                                :style {:font "bold"}
                                :other "abc"})
               (add-attributes {:class "bar"
                                :style {:text "large"}
                                :misc "junk"}))
           [:div {:class "foo bar"
                  :style {:font "bold" :text "large"}
                  :other "abc"
                  :misc "junk"}
            "hi there"]))
    (let-propagated [him joe]
      (expr-let [elements (visible-elements him)
                 groups (group-by-tag elements)]
        (is (= (count groups) 2))
        (is (= (set (map count groups)) #{1 2}))
        (is (= (apply + (map #(count (second (first %))) groups)) 1))))
    (is (= (tags-DOM [] {}) [:div {:style {:color "#66FFFF"}}]))
    (let [[dom fred fred-tag]
          (let-propagated [fred '("Fred" tag)]
            (expr-let [dom (tags-DOM [fred] {})
                       fred-elements (entity/elements fred)]
              [dom fred (first fred-elements)]))]
      (is (= dom 
             [:component {:sibling-key fred
                          :definition [item-DOM fred #{fred-tag} {}]
                          :attributes {:style {:color "#66FFFF"}}}])))
    (let [[dom fred fran]
          (let-propagated [fred '("Fred" tag)
                           fran "Fran"]
            (expr-let [dom (tags-DOM [fred fran] {})]
              [dom fred fran]))
          fred-tag (first (current-value (entity/elements fred)))]
      (is (= dom 
             [:div
              {:style {:color "#66FFFF"}}
              [:component {:sibling-key fred
                           :definition [item-DOM fred #{fred-tag} {}]
                           :attributes
                           {:style {:display "block"
                                    :width "100%"}}}]
              [:component {:sibling-key fran
                           :definition [item-DOM fran #{} {}]
                           :attributes
                           {:style  {:display "block"
                                     :width "100%"}}}]]
             )))
    (is (= (let-propagated [fred "Fred"]
             (item-DOM fred #{} {}))
           [:div "Fred"]))
    (let [[dom joe]
          (let-propagated [him joe]
            (expr-let [dom (item-DOM him #{} {})]
              [dom him]))
          male (first (current-value (entity/label->elements joe 1)))
          married (first (current-value (entity/label->elements joe 2)))
          age (first (current-value (entity/label->elements joe "age")))
          age-tag (first (current-value (entity/label->elements age 'tag)))
          age-tag-spec (first (current-value (entity/elements age-tag)))]
      (is (= dom
             [:div
              [:div {:style {:width "100%" :display "block"}} "Joe"]
              [:div {:style {:width "100%" :display "table"}}
               [:colgroup
                [:col {:style {:width "30%"}}]
                [:col {:style {:width "70%"}}]]
               [:div {:style {:display "table-row"}}
                [:div {:style {:color "#66FFFF"
                               :display "table-cell"}}]
                [:div {:style {:display "table-cell"}}
                 [:component {:attributes {:style {:width "100%"
                                                   :display "block"}}
                              :sibling-key male
                              :definition [item-DOM male #{} {}]}]
                 [:component {:attributes {:style {:width "100%"
                                                   :display "block"}}
                              :sibling-key married
                              :definition [item-DOM married #{} {}]}]]]
               [:div {:style {:display "table-row"}}
                [:component {:attributes {:style {:color "#66FFFF"
                                                  :display "table-cell"}}
                             :sibling-key age-tag
                             :definition [item-DOM age-tag #{age-tag-spec} {}]}]
                [:component {:attributes {:style {:display "table-cell"}}
                             :sibling-key age
                             :definition [item-DOM age #{age-tag} {}]}]]]]))
      )))



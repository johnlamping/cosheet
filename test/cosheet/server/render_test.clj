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

(deftest group-by-tag-test
  (let-propagated [him joe]
    (expr-let [elements (visible-elements him)
               groups (group-by-tag elements)]
      (is (= (count groups) 3))
      (is (= (set (map count groups)) #{1 2}))
      (is (= (apply + (map #(count (second (first %))) groups)) 1)))))

(deftest tags-DOM-test
  (is (= (tags-DOM nil [:k] {}) [:div {:class "tag-column editable"
                                       :key [[:condition 'tag] :k]}]))
  (let [[dom fred fred-tag]
        (let-propagated [fred '("Fred" tag)]
          (expr-let [dom (tags-DOM [fred] [:k] {:depth 0})
                     fred-elements (entity/elements fred)]
            [dom fred (first fred-elements)]))]
    (is (= dom
           [:component {:key [(:item-id fred) :k]
                        :definition [item-DOM
                                     fred [(:item-id fred) :k]
                                     #{fred-tag} {:depth 0}]
                        :attributes {:class "tag-column"
                                     :sibling-elements ['tag]}}])))
  (let [[dom fred fran]
        (let-propagated [fred '("Fred" tag)
                         fran "Fran"]
          (expr-let [dom (tags-DOM [fred fran] [:k] {:depth 1})]
            [dom fred fran]))
        fred-tag (first (current-value (entity/elements fred)))]
    (is (= dom
           [:div
            {:class "tag-column"
             :key [[:condition 'tag] :k]}
            [:component {:key [(:item-id fred) :k]
                         :definition [item-DOM
                                      fred [(:item-id fred) :k]
                                      #{fred-tag} {:depth 1}]
                         :attributes {:style {:display "block"
                                              :width "100%"}
                                      :class "vertical-separated"
                                      :sibling-elements ['tag]}}]
            [:component {:key [(:item-id fran) :k]
                         :definition [item-DOM
                                      fran [(:item-id fran) :k]
                                      #{} {:depth 1}]
                         :attributes {:style  {:display "block"
                                               :width "100%"}
                                      :class "vertical-separated"
                                      :sibling-elements ['tag]}}]]))))

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
           [:div {:class "item" :key [:age]}
            [:div {:style {:width "100%" :display "block"}
                   :class "content-text editable"
                   :key [[:content] :age]}
             "39"]
            [:div {:style {:display "table"
                           :table-layout "fixed"
                           :width "100%"}
                   :class "element-table"}
             [:div {:style {:display "table-row"}
                    :class "last-row"}
              [:component {:attributes
                           {:style {:display "table-cell"}
                            :class "tag-column"
                            :sibling-elements ['tag]}
                           :key tag-key
                           :definition [item-DOM
                                        confidence tag-key
                                        #{confidence-tag} {:depth 1}]}]
              [:component {:attributes {:class "item-column"
                                        :style {:display "table-cell"}
                                        :sibling-elements [["confidence" 'tag]]}
                           :key [(:item-id doubtful) :age]
                           :definition [item-DOM
                                        doubtful [(:item-id doubtful) :age]
                                        #{confidence} {:depth 1}]}]]]])))
  ;; Check that we generate no-tags.
  (let [[dom age]
        (let-propagated [age `(39 ("doubtful" (~o1 :order)))]
          (expr-let [dom (item-DOM age [:age] #{} {:depth 0})]
            [dom age]))
        doubtful (first (current-value (entity/label->elements age o1)))]
    (is (= dom
           [:div {:class "item" :key [:age]}
            [:div {:style {:width "100%" :display "block"}
                   :class "content-text editable"
                   :key [[:content] :age]}
             "39"]
            [:div {:style {:display "table"
                           :table-layout "fixed"
                           :width "100%"}
                   :class "element-table"}
             [:div {:style {:display "table-row"}
                    :class "no-tags last-row"}
              [:div {:style {:display "table-cell"}
                     :class "tag-column editable"
                     :key [[:condition 'tag] (:item-id doubtful) :age]}]
              [:component {:attributes {:class "item-column"
                                        :style {:display "table-cell"}
                                        :sibling-elements nil}
                           :key [(:item-id doubtful) :age]
                           :definition [item-DOM
                                        doubtful [(:item-id doubtful) :age]
                                        #{} {:depth 1}]}]]]])))
  (let [[dom joe]
        (let-propagated [him joe]
          (expr-let [dom (item-DOM him [:joe] #{} {:depth 0})]
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
           (let [both-ages-ref [:parallel
                                [(:item-id bogus-age-tag)]
                                [(:item-id bogus-age) (:item-id age)]]]
             [:div {:class "item" :key [:joe]}
              [:div {:style {:width "100%" :display "block"}
                     :class "content-text editable"
                     :key [[:content] :joe]}
               "Joe"]
              [:div {:style {:width "100%"
                             :display "table"
                             :table-layout "fixed"}
                     :class "element-table"}
               [:div {:style {:display "table-row"}}
                [:div {:style {:display "table-cell"}
                       :class "tag-column editable"
                       :key [[:condition 'tag] (:item-id male) :joe]}]
                [:component {:attributes {:style {:display "table-cell"}
                                          :class "item-column"
                                          :sibling-elements nil}
                             :key [(:item-id male) :joe]
                             :definition [item-DOM
                                          male [(:item-id male) :joe]
                                          #{} {:depth 1}]}]]
               [:div {:style {:display "table-row"}}
                [:div {:style {:display "table-cell"}
                       :class "tag-column editable"
                       :key [[:condition 'tag] (:item-id married) :joe]}]
                [:component {:attributes {:style {:display "table-cell"}
                                          :class "item-column"
                                          :sibling-elements nil}
                             :key [(:item-id married) :joe]
                             :definition [item-DOM
                                          married [(:item-id married) :joe]
                                          #{} {:depth 1}]}]]
               [:div {:style {:display "table-row"}
                      :class "last-row"}
                [:component
                 {:attributes
                  {:style {:display "table-cell"}
                   :class "tag-column for-multiple-items"
                   :sibling-elements ['tag]}
                  :key [both-ages-ref :joe]
                  :definition [item-DOM
                               bogus-age-tag
                               [both-ages-ref :joe]
                               #{bogus-age-tag-spec} {:depth 1}]}]
                [:div {:style {:display "table-cell"}
                       :class "item-column"}
                 [:component {:attributes {:style {:width "100%"
                                                   :display "block"}
                                           :class "vertical-separated"
                                           :sibling-elements [["age" 'tag]]}
                              :key [(:item-id bogus-age) :joe]
                              :definition [item-DOM
                                           bogus-age
                                           [(:item-id bogus-age) :joe]
                                           #{bogus-age-tag} {:depth 1}]}]
                 [:component {:attributes {:style {:width "100%"
                                                   :display "block"}
                                           :class "vertical-separated"
                                           :sibling-elements [["age" 'tag]]}
                              :key [(:item-id age) :joe]
                              :definition [item-DOM
                                           age [(:item-id age) :joe]
                                           #{age-tag} {:depth 1}]}]]]]]))))  
  ;; TODO: Test content that is an item.
  )




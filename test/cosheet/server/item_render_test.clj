(ns cosheet.server.item-render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [orderable :as orderable]
             [query :refer [matching-elements]]
             [expression :refer [expr expr-let expr-seq]]
             [expression-manager :refer [current-value]]
             [debug :refer [envs-to-list simplify-for-print]]
             entity-impl
             [store :refer [make-id]]
             store-impl
             mutable-store-impl
             [test-utils :refer [check any as-set
                                 let-mutated item->immutable]])
            (cosheet.server
             [referent :refer [item-referent parallel-union-referent]]
             [item-render :refer :all])
            ; :reload
            ))

(def orderables (reduce (fn [os _]
                          (vec (concat (pop os)
                                       (orderable/split (peek os) :after))))
                        [orderable/initial]
                        (range 6)))
(def o1 (nth orderables 0))
(def o2 (nth orderables 1))
(def o3 (nth orderables 2))
(def o4 (nth orderables 3))
(def o5 (nth orderables 4))
(def o6 (nth orderables 5))
(def unused-orderable (nth orderables 6))

(def base-inherited {:priority 0
                     :parent-key [:root]
                     :subject (make-id "root")})
(deftest item-DOM-R-test-simple
  ;; Test a simple cell
  (let [[dom fred] (let-mutated [fred "Fred"]
                     (let [my-inherited
                           (assoc
                            base-inherited :selectable-attributes
                            {:expand {:item-referent (item-referent fred)}})]
                       (expr-let [dom (item-DOM-R fred [] my-inherited)]
                         [dom (item->immutable fred)])))]
    (is (check dom
               [:div {:class "content-text editable item"
                      :key [:root (:item-id fred) :content]
                      :target {:item-referent (item-referent fred)}
                      :expand {:item-referent (item-referent fred)}}
                "Fred"]))))

(deftest item-DOM-R-test-one-column
  ;; Test a one-column element hierarchy
  (let [age-as-list `(39 (:root :non-semantic)
                         (~o3 :order :non-semantic)
                         ("one" ; One tag.
                          ("confidence"
                           :tag (~o1 :order :non-semantic))
                          (~o1 :order :non-semantic))
                         ("another" ; Second with same tag.
                          ("confidence"
                           :tag (~o1 :order :non-semantic))
                          (~o2 :order :non-semantic))
                         ("two" ; Two tags, one matching.
                          ("confidence"
                           :tag (~o1 :order :non-semantic))
                          ("probability"
                           :tag (~o2 :order :non-semantic))
                          (~o3 :order :non-semantic))
                         ("none" ; No tag.
                          (~o4 :order :non-semantic)))
        inherited (into base-inherited
                        {:width 0.5
                         :template "foo"
                         :selectable-attributes {:added-by-test {1 2}}})
        [dom age] (let-mutated [age age-as-list]
                    (expr-let [dom (item-DOM-R age [] inherited)]
                      [dom age]))
        one (first (current-value (matching-elements "one" age)))
        confidence1 (first (current-value
                            (matching-elements "confidence" one)))
        confidence1-tag (first (current-value
                                (matching-elements :tag confidence1)))
        another (first (current-value (matching-elements "another" age)))
        two (first (current-value (matching-elements "two" age)))
        confidence2 (first (current-value
                            (matching-elements "confidence" two)))
        probability (first (current-value
                            (matching-elements "probability" two)))
        probability-tag (first (current-value
                                (matching-elements :tag probability)))
        none (first (current-value (matching-elements "none" age)))
        age-key [:root (:item-id age)]
        tags-key (conj age-key (:item-id one) :outside)
        none-key (conj age-key (:item-id none))
        one-another-two-referent (parallel-union-referent
                                  [(item-referent one)
                                   (item-referent another)
                                   (item-referent two)])]
    (is (check
         dom
         [:div {:class "item with-elements"}
          [:div {:class "content-text editable"
                 :key (conj age-key :content)
                 :target {:item-referent (item-referent age)
                          :template "foo"}
                 :added-by-test {1 2}}
           "39"]
          [:div {:class "stack"}
           ;; Everything with "confidence"
           [:div {:class "wrapped-element tag"}
            [:component {:key (conj tags-key (:item-id confidence1))
                         :class "tag"}
             [item-without-labels-DOM-R confidence1 [confidence1-tag]
              {:priority 1
               :width 0.5
               :parent-key tags-key
               :subject one-another-two-referent
               :template '(nil :tag)
               :selectable-attributes
               {:sibling {:subject-referent (item-referent age)
                          :adjacent-groups-referent one-another-two-referent
                          :parent-key age-key}}}]]
            [:div {:class "indent-wrapper"}
             [:div {:class "stack"}
              ;; One Another
              [:div {:class "item-stack"}
               ;; One
               [:component {:key (conj age-key (:item-id one))}
                [item-without-labels-DOM-R one [confidence1]
                 {:priority 1
                  :width 0.5
                  :parent-key age-key
                  :subject (item-referent age)
                  :template '(nil ("confidence" :tag))
                  :selectable-attributes
                  {:sibling {:subject-referent (item-referent age)
                             :adjacent-groups-referent one-another-two-referent
                             :parent-key age-key}}}]]
               ;; Another
               [:component {:key (conj age-key (:item-id another))}
                [item-without-labels-DOM-R another [(any)]
                 (any)]]]
              ;; Two (must be nested)
              [:div {:class "wrapped-element tag"}
               [:component {:key (conj age-key (:item-id two)
                                       :outside (:item-id probability))
                            :class "tag"}
                [item-without-labels-DOM-R probability [probability-tag]
                 {:priority 1
                  :width 0.5
                  :parent-key (conj age-key (:item-id two) :outside)
                  :subject (item-referent two)
                  :template '(nil :tag)
                  :selectable-attributes
                  {:sibling {:subject-referent (item-referent age)
                             :adjacent-groups-referent (item-referent two)
                             :template '(nil ("confidence" :tag))
                             :parent-key age-key}}}]]
               [:div {:class "indent-wrapper"}
                [:component {:key (conj age-key (:item-id two))}
                 [item-without-labels-DOM-R two (as-set [confidence2 probability])
                  {:priority 1
                   :width 0.5
                   :parent-key age-key
                   :subject (item-referent age)
                   :template (as-set '(nil ("confidence" :tag)
                                           ("probability" :tag)))
                   :selectable-attributes
                   {:sibling {:subject-referent (item-referent age)
                              :adjacent-groups-referent (item-referent two)
                              :template '(nil ("confidence" :tag))
                              :parent-key age-key}}}]]]]]]]
           ;; None
           [:div {:class "horizontal-tags-element narrow"}
            [:div {:class "editable tag"
                   :key (conj none-key :outside :tags)
                   :target {:subject-referent (item-referent none)
                            :adjacent-referent (item-referent none)
                            :position :after
                            :template '(nil :tag)}}]
            [:component {:key none-key}
             [item-without-labels-DOM-R none nil
              {:priority 1
               :width 0.5
               :parent-key age-key
               :subject (item-referent age)
               :template '(nil)}]]]]]))))

(deftest item-DOM-R-test-two-column  
  ;; Test two column element hierarchy.
  (let [age-as-list `(39 (:root :non-semantic)
                         (~o3 :order :non-semantic)
                         ("pair" ; Two tags.
                          ("confidence"
                           :tag (~o1 :order :non-semantic))
                          ("likelihood"
                           :tag (~o2 :order :non-semantic))
                          (~o1 :order :non-semantic))
                         ("double" ; Second with same tags.
                          ("confidence"
                           :tag (~o1 :order :non-semantic))
                          ("likelihood"
                           :tag (~o2 :order :non-semantic))
                          (~o2 :order :non-semantic))
                         ("two" ; Two tags, only one matching.
                          ("confidence"
                           :tag (~o1 :order :non-semantic))
                          ("probability"
                           :tag (~o2 :order :non-semantic))
                          (~o3 :order :non-semantic))
                         ("one"
                          ("confidence"
                           :tag (~o1 :order :non-semantic))
                          (~o4 :order :non-semantic)))
        inherited (assoc base-inherited :width 1.0)
        [dom age] (let-mutated [age age-as-list]
                    (expr-let [dom (item-DOM-R age [] inherited)]
                      [dom age]))
        pair (first (current-value (matching-elements "pair" age)))
        confidence1 (first (current-value
                            (matching-elements "confidence" pair)))
        confidence1-tag (first (current-value
                                (matching-elements :tag confidence1)))
        likelihood (first (current-value
                           (matching-elements "likelihood" pair)))
        likelihood-tag (first (current-value
                               (matching-elements :tag likelihood)))
        double (first (current-value (matching-elements "double" age)))
        two (first (current-value (matching-elements "two" age)))
        confidence2 (first (current-value
                            (matching-elements "confidence" two)))
        probability (first (current-value
                            (matching-elements "probability" two)))
        
        probability-tag (first (current-value
                                (matching-elements :tag probability)))
        one (first (current-value (matching-elements "one" age)))
        confidence3 (first (current-value
                            (matching-elements "confidence" one)))
        age-key [:root (:item-id age)]
        tags-key (conj age-key (:item-id pair) :outside)
        one-key (conj age-key (:item-id one))
        likelihoods-referent (parallel-union-referent
                              [(item-referent pair) (item-referent double)])
        all-elements-referent (parallel-union-referent
                               [(item-referent pair) (item-referent double)
                                (item-referent two) (item-referent one)])]
    (is (check
         dom
         [:div {:class "item with-elements"}
          [:div (any map?) "39"]
          [:div {:class "stack"}
           [:div {:class "horizontal-tags-element wide"}
            ;; Group with empty item.
            [:div {:class "tag horizontal-header top-border"}
             [:component {:key (conj tags-key (:item-id confidence1))
                          :class "tag"}
              [item-without-labels-DOM-R confidence1 [confidence1-tag]
               {:priority 1
                :width 0.25
                :parent-key tags-key
                :subject all-elements-referent
                :template '(nil :tag)
                :selectable-attributes
                {:sibling {:subject-referent (item-referent age)
                           :adjacent-groups-referent all-elements-referent
                           :parent-key age-key}}}]]]
            [:div {:class "editable"
                   :key (conj age-key :example-element (:item-id confidence1))
                   :target {:subject-referent (item-referent age)
                            :adjacent-referent (item-referent pair)
                            :position :before
                            :template '(nil ("confidence" :tag))}
                   :sibling {:subject-referent (item-referent age)
                             :adjacent-groups-referent all-elements-referent
                             :parent-key age-key}}]]
           ;; Group for confidence and likelihood.
           [:div {:class "horizontal-tags-element wide"}
            [:div {:class "tag horizontal-header indent"}
             [:div {:class "tag horizontal-header top-border bottom-border"}
              [:component {:key (conj tags-key (:item-id likelihood))
                           :class "tag"}
               [item-without-labels-DOM-R likelihood [likelihood-tag]
                {:priority 1
                 :width 0.25
                 :parent-key tags-key
                 :subject likelihoods-referent
                 :template '(nil :tag)
                 :selectable-attributes
                 {:sibling {:subject-referent (item-referent age)
                            :adjacent-groups-referent likelihoods-referent
                            :template '(nil ("confidence" :tag))
                            :parent-key age-key}}}]]]]
            [:div {:class "item-stack"}
             ;; Pair
             [:component {:key (conj age-key (:item-id pair))}
              [item-without-labels-DOM-R pair (as-set [confidence1 likelihood])
               {:priority 1
                :width 0.6875
                :parent-key age-key
                :subject (item-referent age)
                :template (as-set '(nil ("confidence" :tag)
                                        ("likelihood" :tag)))
                :selectable-attributes
                {:sibling {:subject-referent (item-referent age)
                           :adjacent-groups-referent likelihoods-referent
                           :template '(nil ("confidence" :tag))
                           :parent-key age-key}}}]]
             ;; Double
             [:component {:key (conj age-key (:item-id double))}
              [item-without-labels-DOM-R double (any)
               (any)]]]]
           ;; Group for confidence and probability
           [:div {:class "horizontal-tags-element wide"}
            [:div {:class "tag horizontal-header indent"}
             [:div {:class "tag horizontal-header top-border bottom-border"}
              [:component {:key (conj age-key (:item-id two)
                                      :outside (:item-id probability))
                           :class "tag"}
               [item-without-labels-DOM-R probability [probability-tag]
                {:priority 1
                 :width 0.25
                 :parent-key (conj age-key (:item-id two) :outside)
                 :subject (item-referent two)
                 :template '(nil :tag)
                 :selectable-attributes
                 {:sibling {:subject-referent (item-referent age)
                            :adjacent-groups-referent (item-referent two)
                            :template '(nil ("confidence" :tag))
                            :parent-key age-key}}}]]]]
            [:component {:key (conj age-key (:item-id two))}
             [item-without-labels-DOM-R two (as-set [confidence2 probability])
              {:priority 1
               :width 0.6875
               :parent-key age-key
               :subject (item-referent age)
               :template (as-set '(nil ("confidence" :tag)
                                       ("probability" :tag)))
               :selectable-attributes
               {:sibling {:subject-referent (item-referent age)
                          :adjacent-groups-referent (item-referent two)
                          :template '(nil ("confidence" :tag))
                          :parent-key age-key}}}]]]
           ;; Group for confidence
           [:div {:class "horizontal-tags-element wide"}
            [:div {:class "tag horizontal-header indent bottom-border"}
             (any)]
            [:div {:class "horizontal-value-last"}
             [:component {:key one-key}
              [item-without-labels-DOM-R one [confidence3]
               {:priority 1
                :width 0.6875
                :parent-key age-key
                :subject (item-referent age)
                :template '(nil ("confidence" :tag))}]]]]]]))))

;;; Test an item that needs to be wrapped in labels.
(deftest item-DOM-R-test-labels
  ;; First, test when there is a label.
  (let [element-as-list `(39
                          ("age" :tag (~o1 :order :non-semantic))
                          ("Ke"
                           ("according-to" :tag (~o1 :order :non-semantic))
                           (~o1 :order :non-semantic)))
        inherited  (assoc base-inherited :width 1.0)
        [dom element] (let-mutated [element element-as-list]
                        (expr-let [dom (item-DOM-R element [] inherited)]
                          [dom element]))]
    (let [element-key [:root (:item-id element)]
          age (first (current-value (matching-elements "age" element)))
          age-tag (first (current-value (matching-elements :tag age)))
          age-key (conj element-key (:item-id age))]
      (is (check dom
                 [:div {:class "wrapped-element tag"}
                  [:component {:key age-key :class "tag"}
                   [item-without-labels-DOM-R age [age-tag]
                    {:priority 0 :parent-key element-key
                     :subject (item-referent element)
                     :width 1.0
                     :template '(nil :tag)
                     :selectable-attributes
                     {:expand {:item-referent (item-referent element)}}}]]
                  [:div {:class "indent-wrapper tag"}
                   [:div {:class "item with-elements"}
                    [:div {:class "content-text editable"
                           :target {:item-referent (item-referent element)}
                           :key (conj element-key :content)}
                     "39"]
                    [:div {:class "horizontal-tags-element wide"}
                     (any)
                     [:component {:key (any)}
                      [item-without-labels-DOM-R (any) [(any)] (any)]]]]]]))))
  ;; Then test when there is no label, but labels must be shown.
  (let [item-as-list `(39 ("Ke"
                           ("according-to" :tag (~o1 :order :non-semantic))
                           (~o1 :order :non-semantic)))
        inherited  (assoc base-inherited :width 1.0)
        [dom item] (let-mutated [item item-as-list]
                     (expr-let [dom (must-show-label-item-DOM-R
                                     item (item-referent item) [] inherited)]
                          [dom item]))]
    (let [item-key [:root (:item-id item)]]
      (is (check dom
                 [:div {:class "horizontal-tags-element narrow"}
                  [:div {:class "editable tag" :key (conj item-key :tags)
                         :expand {:item-referent (item-referent item)}
                         :target {:template '(nil :tag)
                                  :adjacent-referent (item-referent item)
                                  :position :after
                                  :subject-referent (item-referent item)}}]
                  [:div {:class "item with-elements"}
                    [:div {:class "content-text editable"
                           :target {:item-referent (item-referent item)}
                           :key (conj item-key :content)}
                     "39"]
                    [:div {:class "horizontal-tags-element wide"}
                     (any)
                     [:component {:key (any)}
                      [item-without-labels-DOM-R (any) [(any)] (any)]]]]])))))

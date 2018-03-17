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
             [referent :refer [item-referent union-referent
                               parallel-union-referent virtual-referent]]
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
                     :key-prefix [:root]
                     :subject-referent nil})
(deftest item-DOM-R-test-simple
  ;; Test a simple cell
  (let [[dom fred] (let-mutated [fred "Fred"]
                     (let [my-inherited
                           (assoc base-inherited
                                  :attributes
                                  [[#{:content}
                                    {:expand {:referent
                                              (item-referent fred)}}]])]
                       (expr-let [dom (item-DOM-R fred [] my-inherited)]
                         [dom (item->immutable fred)])))]
    (is (check dom
               [:div {:class "content-text editable item"
                      :key [:root (:item-id fred) :content]
                      :target {:referent (item-referent fred)}
                      :expand {:referent (item-referent fred)}}
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
        [dom age] (let-mutated [age age-as-list]
                    (expr-let [ae (matching-elements "another" age)
                               ne (matching-elements "none" age)
                               another (first ae)
                               none (first ne)
                               inherited
                               (into base-inherited
                                     {:width 0.5
                                      :template "foo"
                                      :selector-category :some-category
                                      :attributes
                                      [[#{:content} {:added-by-test {1 2}}]
                                       [(:item-id another) {:class "added1"}]
                                       [(:item-id none) {:class "added2"}]]})]
                      (expr-let [dom (item-DOM-R age [] inherited)]
                        [dom age])))
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
        tags-key (conj age-key :label)
        none-key (conj age-key (:item-id none))
        one-another-two-parallel-referent (parallel-union-referent
                                           [(item-referent one)
                                            (item-referent another)
                                            (item-referent two)])]
    (is (check
         dom
         [:div {:class "item with-elements"}
          [:div {:class "content-text editable"
                 :key (conj age-key :content)
                 :selector-category :some-category
                 :target {:referent (item-referent age)
                          :template "foo"}
                 :added-by-test {1 2}}
           "39"]
          [:div {:class "vertical-stack"}
           ;; Everything with "confidence"
           [:div {:class "wrapped-element tag"}
            [:component {:key (conj tags-key (:item-id confidence1))
                         :class "tag"}
             [item-without-labels-DOM-R confidence1 [confidence1-tag]
              {:priority 1
               :width 0.5
               :key-prefix tags-key
               :subject-referent one-another-two-parallel-referent
               :template '(nil :tag)
               :attributes
               [[(:item-id another) {:class "added1"}]
                [(:item-id none) {:class "added2"}]
                [#{:label :optional} #{:content}
                  {:add-sibling {:referent (virtual-referent
                                            'nil (item-referent age)
                                            one-another-two-parallel-referent
                                            :selector :first-group) 
                                 :select-pattern (conj age-key [:pattern])}}]]
               :selector-category :some-category}]]
            [:div {:class "indent-wrapper"}
             [:div {:class "vertical-stack"}
              ;; One Another
              [:div {:class "vertical-stack"}
               ;; One
               [:component {:key (conj age-key (:item-id one))}
                [item-without-labels-DOM-R one [confidence1]
                 {:priority 1
                  :width 0.5
                  :key-prefix age-key
                  :subject-referent (item-referent age)
                  :template '(nil ("confidence" :tag))
                  :attributes
                  [[(:item-id another) {:class "added1"}]
                   [(:item-id none) {:class "added2"}]
                   [#{:label :optional} #{:content}
                     {:add-sibling {:referent
                                    (virtual-referent
                                     'nil (item-referent age)
                                     one-another-two-parallel-referent
                                    :selector :first-group) 
                                    :select-pattern (conj age-key [:pattern])}
                      }]]
                  :selector-category :some-category}]]
               ;; Another
               [:component {:key (conj age-key (:item-id another))}
                [item-without-labels-DOM-R another [(any)]
                 (any)]]]
              ;; Two (must be nested)
              [:div {:class "wrapped-element tag"}
               [:component {:key (conj age-key :label (:item-id probability))
                            :class "tag"}
                [item-without-labels-DOM-R probability [probability-tag]
                 {:priority 1
                  :width 0.5
                  :key-prefix (conj age-key :label)
                  :subject-referent (item-referent two)
                  :template '(nil :tag)
                  :attributes
                  [[(:item-id another) {:class "added1"}]
                   [(:item-id none) {:class "added2"}]
                   [#{:label :optional} #{:content}
                     {:add-sibling {:referent (virtual-referent
                                               '(nil ("confidence" :tag))
                                               (item-referent age)
                                               (item-referent two)
                                               :selector :first-group) 
                                    :select-pattern (conj age-key [:pattern])}
                      }]]
                  :selector-category :some-category}]]
               [:div {:class "indent-wrapper"}
                [:component {:key (conj age-key (:item-id two))}
                 [item-without-labels-DOM-R
                  two (as-set [confidence2 probability])
                  {:priority 1
                   :width 0.5
                   :key-prefix age-key
                   :subject-referent (item-referent age)
                   :template (as-set '(nil ("confidence" :tag)
                                           ("probability" :tag)))
                   :attributes
                   [[(:item-id another) {:class "added1"}]                    
                    [(:item-id none) {:class "added2"}]
                    [#{:label :optional} #{:content}
                      {:add-sibling {:referent (virtual-referent
                                                '(nil ("confidence" :tag))
                                                (item-referent age)
                                                (item-referent two)
                                                :selector :first-group) 
                                     :select-pattern (conj age-key [:pattern])}
                       }]]
                   :selector-category :some-category}]]]]]]]
           ;; None
           [:div {:class "horizontal-tags-element tag narrow added2"}
            [:div {:class "editable tag"
                   :key (conj (conj age-key :label (:item-id none)) :virtual)
                   :selector-category :some-category
                   :target {:referent (virtual-referent
                                       '(nil :tag) (item-referent none)
                                       nil
                                       :selector :first-group)
                            :select-pattern (conj age-key :label [:pattern])}
                   :add-sibling {:referent (virtual-referent
                                             nil
                                             (item-referent age)
                                             (item-referent none)
                                             :selector :first-group) 
                                 :select-pattern (conj age-key [:pattern])}}]
            [:component {:key none-key}
             [item-without-labels-DOM-R none nil
              {:priority 1
               :width 0.5
               :key-prefix age-key
               :subject-referent (item-referent age)
               :template '(nil)
               :selector-category :some-category
               :attributes
               [[(:item-id another) {:class "added1"}]
                [#{:label :optional} #{:content}
                  {:add-sibling {:referent (virtual-referent
                                            nil
                                            (item-referent age)
                                            (item-referent none)
                                            :selector :first-group) 
                                 :select-pattern (conj age-key [:pattern])}
                   }]]}]]]]]))))

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
                          (~o4 :order :non-semantic))
                         ("unique"
                          ("certainty"
                           :tag (~o1 :order :non-semantic))
                          (~o5 :order :non-semantic)))
        [dom age] (let-mutated [age age-as-list]
                    (expr-let [de (matching-elements "double" age)
                               ue (matching-elements "unique" age)
                               double (first de)
                               unique (first ue)
                               inherited
                               (assoc base-inherited
                                      :width 1.0
                                      :attributes
                                      [[(:item-id double) {:class "added1"}]
                                       [(:item-id unique) {:class "added2"}]])]
                      (expr-let [dom (item-DOM-R age [] inherited)]
                        [dom age])))
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
        tags-key (conj age-key :label)
        one-key (conj age-key (:item-id one))
        unique (first (current-value (matching-elements "unique" age)))
        certainty (first (current-value (matching-elements "certainty" unique)))
        likelihoods-parallel-referent (parallel-union-referent
                                       [(item-referent pair)
                                        (item-referent double)])
        all-elements-parallel-referent (parallel-union-referent
                                        [(item-referent pair)
                                         (item-referent double)
                                         (item-referent two)
                                         (item-referent one)])]
    (is (check
         dom
         [:div {:class "item with-elements"}
          [:div (any map?) "39"]
          [:div {:class "vertical-stack"}
           [:div {:class "horizontal-tags-element tag wide"}
            ;; Group with empty item.
            [:div {:class "tag horizontal-header top-border"}
             [:component {:key (conj tags-key (:item-id confidence1))
                          :class "tag"}
              [item-without-labels-DOM-R confidence1 [confidence1-tag]
               {:priority 1
                :width 0.25
                :key-prefix tags-key
                :subject-referent all-elements-parallel-referent
                :template '(nil :tag)
                :attributes
                [[(:item-id double) {:class "added1"}]
                 [(:item-id unique) {:class "added2"}]
                 [#{:label :optional} #{:content}
                  {:add-sibling {:referent (virtual-referent
                                            nil (item-referent age)
                                            all-elements-parallel-referent) 
                                 :select-pattern (conj age-key [:pattern])}}]]}]]]
            [:div {:class "editable"
                   :key (conj age-key
                              :example-element (:item-id confidence1) :virtual)
                   :target {:referent (virtual-referent
                                       '(nil ("confidence" :tag))
                                       (item-referent age)
                                       (item-referent pair)
                                       :position :before)
                            :select-pattern (conj age-key [:pattern])}
                   :add-sibling {:referent (virtual-referent
                                            nil (item-referent age)
                                            all-elements-parallel-referent) 
                                 :select-pattern (conj age-key [:pattern])}}]]
           ;; Group for confidence and likelihood.
           [:div {:class "horizontal-tags-element tag wide"}
            [:div {:class "tag horizontal-header indent"}
             [:div {:class "tag horizontal-header top-border bottom-border"}
              [:component {:key (conj tags-key (:item-id likelihood))
                           :class "tag"}
               [item-without-labels-DOM-R likelihood [likelihood-tag]
                {:priority 1
                 :width 0.25
                 :key-prefix tags-key
                 :subject-referent likelihoods-parallel-referent
                 :template '(nil :tag)
                 :attributes
                 [[(:item-id double) {:class "added1"}]
                  [(:item-id unique) {:class "added2"}]
                  [#{:label :optional} #{:content}
                   {:add-sibling {:referent (virtual-referent
                                             '(nil ("confidence" :tag))
                                             (item-referent age)
                                             likelihoods-parallel-referent) 
                                  :select-pattern (conj age-key [:pattern])}}]]}]]]]
            [:div {:class "vertical-stack"}
             ;; Pair
             [:component {:key (conj age-key (:item-id pair))}
              [item-without-labels-DOM-R pair (as-set [confidence1 likelihood])
               {:priority 1
                :width 0.6875
                :key-prefix age-key
                :subject-referent (item-referent age)
                :template (as-set '(nil ("confidence" :tag)
                                        ("likelihood" :tag)))
                :attributes
                [[(:item-id double) {:class "added1"}]
                 [(:item-id unique) {:class "added2"}]
                 [#{:label :optional} #{:content}
                  {:add-sibling {:referent (virtual-referent
                                            '(nil ("confidence" :tag))
                                            (item-referent age)
                                            likelihoods-parallel-referent) 
                                 :select-pattern (conj age-key [:pattern])}}]]}]]
             ;; Double
             [:component {:key (conj age-key (:item-id double))}
              [item-without-labels-DOM-R double (any)
               (any)]]]]
           ;; Group for confidence and probability
           [:div {:class "horizontal-tags-element tag wide"}
            [:div {:class "tag horizontal-header indent"}
             [:div {:class "tag horizontal-header top-border bottom-border"}
              [:component {:key (conj age-key :label (:item-id probability))
                           :class "tag"}
               [item-without-labels-DOM-R probability [probability-tag]
                {:priority 1
                 :width 0.25
                 :key-prefix (conj age-key :label)
                 :subject-referent (item-referent two)
                 :template '(nil :tag)
                 :attributes
                 [[(:item-id double) {:class "added1"}]
                  [(:item-id unique) {:class "added2"}]
                  [#{:label :optional} #{:content}
                   {:add-sibling {:referent (virtual-referent
                                             '(nil ("confidence" :tag))
                                             (item-referent age)
                                             (item-referent two)) 
                                  :select-pattern (conj age-key [:pattern])}}]]}]]]]
            [:component {:key (conj age-key (:item-id two))}
             [item-without-labels-DOM-R two (as-set [confidence2 probability])
              {:priority 1
               :width 0.6875
               :key-prefix age-key
               :subject-referent (item-referent age)
               :template (as-set '(nil ("confidence" :tag)
                                       ("probability" :tag)))
               :attributes
               [[(:item-id double) {:class "added1"}]
                [(:item-id unique) {:class "added2"}]
                [#{:label :optional} #{:content}
                 {:add-sibling {:referent (virtual-referent
                                           '(nil ("confidence" :tag))
                                           (item-referent age)
                                           (item-referent two)) 
                                :select-pattern (conj age-key [:pattern])}}]]}]]]
           ;; Group for confidence
           [:div {:class "horizontal-tags-element tag wide"}
            [:div {:class "tag horizontal-header indent bottom-border"}
             (any)]
            [:div {:class "horizontal-value-last"}
             [:component {:key one-key}
              [item-without-labels-DOM-R one [confidence3]
               {:priority 1
                :width 0.6875
                :key-prefix age-key
                :subject-referent (item-referent age)
                :template '(nil ("confidence" :tag))
                :attributes
                [[(:item-id double) {:class "added1"}]
                 [(:item-id unique) {:class "added2"}]
                 [#{:label :optional} #{:content}
                  {:add-sibling {:referent (virtual-referent
                                            '(nil ("confidence" :tag))
                                            (item-referent age)
                                            (item-referent one)) 
                                 :select-pattern (conj age-key [:pattern])}}]]}]]]]
           ;; Group for unique
           [:div {:class "horizontal-tags-element tag wide added2"}
            [:div {:class "tag horizontal-header top-border bottom-border"}
             [:component {:key (conj age-key :label (:item-id certainty))
                          :class "tag"}
              [item-without-labels-DOM-R certainty (any)
               {:priority 1
                :key-prefix (conj age-key :label)
                :subject-referent (item-referent unique)
                :width 0.25
                :attributes
                [[(:item-id double) {:class "added1"}]
                 [(:item-id unique) {:class "added2"}]
                 [#{:label :optional} #{:content}
                  {:add-sibling {:referent (virtual-referent
                                            nil
                                            (item-referent age)
                                            (item-referent unique)) 
                                 :select-pattern (conj age-key [:pattern])}}]]
                :template '(nil :tag)}]]]
            [:component {:key (conj age-key (:item-id unique))}
             [item-without-labels-DOM-R unique [certainty]
              {:priority 1
               :key-prefix age-key
               :subject-referent (item-referent age)
               :width 0.6875
               :attributes
               [[(:item-id double) {:class "added1"}]
                [#{:label :optional} #{:content}
                 {:add-sibling {:referent (virtual-referent
                                           nil
                                           (item-referent age)
                                           (item-referent unique)) 
                                :select-pattern (conj age-key [:pattern])}}]]
               :template '(nil ("certainty" :tag))}]]]]]))))

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
                    {:priority 0 :key-prefix element-key
                     :subject-referent (item-referent element)
                     :width 1.0
                     :template '(nil :tag)
                     :attributes [[#{:content}
                                   {:expand {:referent
                                             (item-referent element)}}]]}]]
                  [:div {:class "indent-wrapper"}
                   [:div {:class "item with-elements"}
                    [:div {:class "content-text editable"
                           :target {:referent (item-referent element)}
                           :key (conj element-key :content)}
                     "39"]
                    [:div {:class "horizontal-tags-element tag wide"}
                     (any)
                     [:component {:key (any)}
                      [item-without-labels-DOM-R (any) [(any)] (any)]]]]]]))))
  ;; Then test when there is no label, but labels must be shown.
  (let [item-as-list `(39 ("Ke"
                           ("according-to" :tag (~o1 :order :non-semantic))
                           (~o1 :order :non-semantic)))
        inherited  (assoc base-inherited :width 1.0)
        [dom item] (let-mutated [item item-as-list]
                     (expr-let [dom (item-DOM-R
                                     item [] inherited :must-show-label true)]
                       [dom item]))]
    (let [item-key [:root (:item-id item)]]
      (is (check dom
                 [:div {:class "horizontal-tags-element tag narrow"}
                  [:div {:class "editable tag"
                         :key (conj item-key :tags :virtual)
                         :expand {:referent (item-referent item)}
                         :target {:referent (virtual-referent
                                             '(nil :tag)
                                             (item-referent item)
                                             nil
                                             :position :after)
                                  :select-pattern (conj item-key
                                                        :tags [:pattern])}}]
                  [:div {:class "item with-elements"}
                   [:div {:class "content-text editable"
                          :target {:referent (item-referent item)}
                          :key (conj item-key :content)}
                    "39"]
                   [:div {:class "horizontal-tags-element tag wide"}
                    (any)
                    [:component {:key (any)}
                     [item-without-labels-DOM-R (any) [(any)] (any)]]]]])))))

;;; TODO: Add a test where some of the arguments are true.
(deftest labels-and-elements-DOM-R-test
  (let [element-as-list `(39
                          ("age" :tag (~o1 :order :non-semantic))
                          ("Ke"
                           ("according-to" :tag (~o1 :order :non-semantic))
                           (~o1 :order :non-semantic)))

        element (let-mutated [element element-as-list]
                  element)
        element-key [:root (:item-id element)]
        element-referent (item-referent element)
        age (first (current-value (matching-elements "age" element)))
        age-tag (first (current-value (matching-elements :tag age)))
        age-key (conj element-key (:item-id age))
        qualifier (first (current-value (matching-elements "Ke" element)))
        according-to (first (current-value (matching-elements
                                            "according-to" qualifier)))
        according-to-id (:item-id according-to)
        inherited  (assoc base-inherited
                          :width 1.0
                          :key-prefix element-key
                          :subject-referent element-referent
                          :attributes [[#{:content} {:class "placeholder"}]])
        dom (current-value
              (labels-and-elements-DOM-R
               [age qualifier] false false false :vertical inherited))]
    (is (check dom
               [:div {:class "wrapped-element tag"}
                [:component {:key age-key :class "tag"}
                 [item-without-labels-DOM-R age [age-tag]
                  {:priority 0 :key-prefix element-key
                   :subject-referent (item-referent element)
                   :width 1.0
                   :template '(nil :tag)}]]
                [:div {:class "indent-wrapper"}
                 [:div {:class "item elements-wrapper"}
                  [:div {:class "horizontal-tags-element tag wide"}
                   [:div {:class
                          "tag horizontal-header top-border bottom-border"}
                    [:component {:key (conj element-key :label according-to-id)
                                 :class "tag"}
                     [item-without-labels-DOM-R
                      according-to [(any)] (any)]]]
                   [:component {:key (conj element-key (:item-id qualifier))}
                    [item-without-labels-DOM-R
                     qualifier [(any)] (any)]]]]]]))))


(deftest item-content-and-elements-DOM-R-test
  (let [element-as-list `(39
                          ("age" :tag (~o1 :order :non-semantic))
                          ("Ke"
                           ("according-to" :tag (~o1 :order :non-semantic))
                           (~o1 :order :non-semantic)))

        element (let-mutated [element element-as-list]
                  element)
        element-key [:root (:item-id element)]
        element-referent (item-referent element)
        age (first (current-value (matching-elements "age" element)))
        age-tag (first (current-value (matching-elements :tag age)))
        age-key (conj element-key (:item-id age))
        qualifier (first (current-value (matching-elements "Ke" element)))
        according-to (first (current-value (matching-elements
                                            "according-to" qualifier)))
        according-to-id (:item-id according-to)
        inherited  (assoc base-inherited
                          :width 1.0
                          :key-prefix element-key
                          :subject-referent element-referent
                          :attributes [[#{:content} {:class "placeholder"}]])
        dom (current-value
             (item-content-and-elements-DOM-R
              39 [age qualifier] false inherited))]
    (is (check dom
               [:div {:class "wrapped-element tag"}
                [:component {:key age-key :class "tag"}
                 [item-without-labels-DOM-R age [age-tag]
                  {:priority 0 :key-prefix element-key
                   :subject-referent (item-referent element)
                   :width 1.0
                   :template '(nil :tag)}]]
                [:div {:class "indent-wrapper"}
                 [:div {:class "item with-elements"}
                  [:div {:class "placeholder content-text editable"
                         :target {:referent element-referent}
                         :key (conj element-key :content)}
                   "39"]
                  [:div {:class "horizontal-tags-element tag wide"}
                   [:div {:class
                          "tag horizontal-header top-border bottom-border"}
                    [:component {:key (conj element-key
                                            :label according-to-id)
                                 :class "tag"}
                     [item-without-labels-DOM-R
                      according-to [(any)] (any)]]]
                   [:component {:key (conj element-key (:item-id qualifier))}
                    [item-without-labels-DOM-R
                     qualifier [(any)] (any)]]]]]]))))

(ns cosheet.server.render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [orderable :as orderable]
             [entity :as entity  :refer [to-list description->entity
                                         label->elements]]
             [query :refer [matching-elements matching-items]]
             [reporters :as reporter]
             [expression :refer [expr expr-let expr-seq]]
             [debug :refer [current-value envs-to-list simplify-for-print]]
             [expression-manager :refer [new-expression-manager-data
                                         request compute]] 
             [expression-manager-test :refer [check-propagation]]
             entity-impl
             [store :refer [new-element-store id->content id->subject
                            make-id current-store]]
             store-impl
             [store-utils :refer [add-entity]]
             mutable-store-impl
             [dom-utils :refer [dom-attributes]]
             [test-utils :refer [check any as-set evals-to
                                 let-mutated item->immutable]])
            (cosheet.server
             [referent :refer [item-referent union-referent difference-referent
                           query-referent elements-referent
                          canonicalize-list]]
             [render :refer :all]
             [hierarchy :refer [items-hierarchy-by-elements]])
                                        ; :reload
            ))

(deftest condition-satisfiers-R-test
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil :a)))))
             [:a]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil :a :c)))))
             (as-set [:a :c])))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil :a :b)))))
             [:a]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil :a :b)))))
             [:a]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil :a :a :b)))))
             [:a :a]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" :a (:b 1) :c)]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil :a :a :b)))))
             [:a])))

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

(deftest item-DOM-R-test
  (let [root-id (make-id "root")
        inherited {:priority 0
                   :parent-key [:root]
                   :subject root-id}]
    ;; Test a simple cell
    (let [[dom fred] (let-mutated [fred "Fred"]
                   (expr-let [dom (item-DOM-R fred [] inherited)]
                     [dom (item->immutable fred)]))]
      (is (check dom
                 [:div {:class "content-text editable item"
                        :key [:root (:item-id fred)]
                        :target {:item-referent (item-referent fred)}
                        :commands {:set-content nil
                                   :add-element nil
                                   :add-sibling nil
                                   :delete nil}}
                  "Fred"])))
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
          my-inherited (into inherited
                             {:width 0.5
                              :template "foo"
                              :selectable-attributes {:commands {:foo nil}}})
          [dom age] (let-mutated [age age-as-list]
                      (expr-let [dom (item-DOM-R age [] my-inherited)]
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
          one-another-two-referent (union-referent [(item-referent one)
                                                    (item-referent another)
                                                    (item-referent two)])]
      (is (check
           dom
           [:div {:class "item with-elements" :key age-key}
            [:div {:class "content-text editable"
                   :key (conj age-key :content)
                   :target {:item-referent (item-referent age)
                            :template "foo"}
                   :commands {:set-content nil
                              :add-element nil
                              :add-sibling nil
                              :delete nil
                              :foo nil}}
             "39"]
            [:div {:class "stack"}
             ;; Everything with "confidence"
             [:div {:class "wrapped-element tag"}
              [:component {:key (conj tags-key (:item-id confidence1))
                           :class "tag"}
               [item-DOM-R confidence1 [confidence1-tag]
                {:priority 1
                 :width 0.5
                 :parent-key tags-key
                 :subject one-another-two-referent
                 :template '(nil :tag)}]]
              [:div {:class "indent-wrapper"}
               [:div {:class "stack"}
                ;; One Another
                [:div {:class "item-stack"}
                 ;; One
                 [:component {:key (conj age-key (:item-id one))}
                  [item-DOM-R one [confidence1]
                   {:priority 1
                    :width 0.5,
                    :parent-key age-key
                    :subject (item-referent age)
                    :template '(nil ("confidence" :tag))
                    :selectable-attributes
                    {:commands {:add-row nil}
                     :row {:subject-referent (item-referent age)
                           :adjacents-referent one-another-two-referent}}}]]
                 ;; Another
                 [:component {:key (conj age-key (:item-id another))}
                  [item-DOM-R another [(any)]
                   (any)]]]
                ;; Two (must be nested)
                [:div {:class "wrapped-element tag"}
                 [:component {:key (conj age-key (:item-id two)
                                         :outside (:item-id probability))
                              :class "tag"}
                  [item-DOM-R probability [probability-tag]
                   {:priority 1
                    :width 0.5
                    :parent-key (conj age-key (:item-id two) :outside)
                    :subject (item-referent two)
                    :template '(nil :tag)
                    :selectable-attributes
                    {:commands {:add-row nil}
                     :row {:subject-referent (item-referent age)
                           :adjacents-referent one-another-two-referent}}}]]
                 [:div {:class "indent-wrapper"}
                  [:component {:key (conj age-key (:item-id two))}
                   [item-DOM-R two (as-set [confidence2 probability])
                    {:priority 1
                     :width 0.5,
                     :parent-key age-key
                     :subject (item-referent age)
                     :template (as-set '(nil ("confidence" :tag)
                                             ("probability" :tag)))
                     :selectable-attributes
                     {:commands {:add-row nil}
                      :row {:subject-referent (item-referent age)
                            :adjacents-referent (item-referent two)
                            :template '(nil ("confidence" :tag))}}}]]]]]]]
             ;; None
             [:div {:class "horizontal-tags-element narrow"}
              [:div {:class "editable tag"
                     :key (conj none-key :outside :tags)
                     :commands {:set-content nil}
                     :target {:subject-referent (item-referent none)
                              :adjacents-referent (item-referent none)
                              :position :after
                              :template '(nil :tag)}}]
              [:component {:key none-key}
               [item-DOM-R none nil
                {:priority 1,
                 :width 0.5,
                 :parent-key age-key
                 :subject (item-referent age)}]]]]])))
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
          wide-inherited (assoc inherited :width 1.0)
          [dom age] (let-mutated [age age-as-list]
                      (expr-let [dom (item-DOM-R age [] wide-inherited)]
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
          likelihoods-referent (union-referent [(item-referent pair)
                                                 (item-referent double)])
          all-elements-referent (union-referent [(item-referent pair)
                                                 (item-referent double)
                                                 (item-referent two)
                                                 (item-referent one)])]
      (is (check
           dom
           [:div {:class "item with-elements" :key age-key}
            [:div (any map?) "39"]
            [:div {:class "stack"}
             [:div {:class "horizontal-tags-element wide"}
              ;; Row with empty item.
              [:div {:class "tag horizontal-header top-border"}
               [:component {:key (conj tags-key (:item-id confidence1))
                            :class "tag"}
                [item-DOM-R confidence1 [confidence1-tag]
                 {:priority 1
                  :width 0.25
                  :parent-key tags-key
                  :subject all-elements-referent
                  :template '(nil :tag)}]]]
              [:div {:class "editable"
                     :key (conj age-key :example-element (:item-id confidence1))
                     :commands {:set-content nil
                                :add-row nil},
                     :target {:subject-referent (:item-id age)
                              :adjacents-referent (:item-id pair)
                              :position :before,
                              :template '(nil ("confidence" :tag))}
                     :row {:subject-referent (:item-id age)
                           :adjacents-referent all-elements-referent}}]]
             ;; Row for confidence and likelihood.
             [:div {:class "horizontal-tags-element wide"}
              [:div {:class "tag horizontal-header indent"}
               [:div {:class "tag horizontal-header top-border bottom-border"}
                [:component {:key (conj tags-key (:item-id likelihood))
                             :class "tag"}
                 [item-DOM-R likelihood [likelihood-tag]
                  {:priority 1
                   :width 0.25
                   :parent-key tags-key
                   :subject likelihoods-referent
                   :template '(nil :tag)}]]]]
              [:div {:class "item-stack"}
               ;; Pair
               [:component {:key (conj age-key (:item-id pair))}
                [item-DOM-R pair (as-set [confidence1 likelihood])
                 {:priority 1
                  :width 0.6875,
                  :parent-key age-key
                  :subject (item-referent age)
                  :template (as-set '(nil ("confidence" :tag)
                                          ("likelihood" :tag)))
                  :selectable-attributes
                  {:commands {:add-row nil}
                   :row {:subject-referent (item-referent age)
                         :adjacents-referent likelihoods-referent
                         :template '(nil ("confidence" :tag))}}}]]
               ;; Double
               [:component {:key (conj age-key (:item-id double))}
                [item-DOM-R double (any)
                 (any)]]]]
             ;; Row for confidence and probability
             [:div {:class "horizontal-tags-element wide"}
              [:div {:class "tag horizontal-header indent"}
               [:div {:class "tag horizontal-header top-border bottom-border"}
                [:component {:key (conj age-key (:item-id two)
                                        :outside (:item-id probability))
                             :class "tag"}
                 [item-DOM-R probability [probability-tag]
                  {:priority 1
                   :width 0.25
                   :parent-key (conj age-key (:item-id two) :outside)
                   :subject (item-referent two)
                   :template '(nil :tag)
                   ;; TODO: need add-row command.
                   }]]]]
              [:component {:key (conj age-key (:item-id two))}
               [item-DOM-R two (as-set [confidence2 probability])
                {:priority 1
                 :width 0.6875,
                 :parent-key age-key
                 :subject (item-referent age)
                 :template (as-set '(nil ("confidence" :tag)
                                         ("probability" :tag)))
                 :selectable-attributes
                 {:commands {:add-row nil}
                  :row {:subject-referent (item-referent age)
                        :adjacents-referent (item-referent two)
                        :template '(nil ("confidence" :tag))}}}]]]
             ;; Row for confidence
             [:div {:class "horizontal-tags-element wide"}
              [:div {:class "tag horizontal-header indent bottom-border"}
               (any)]
              ;; TODO: Needs new-row command
              [:component {:key one-key}
               [item-DOM-R one [confidence3]
                {:priority 1,
                 :width 0.6875,
                 :parent-key age-key
                 :subject (item-referent age)
                 :template '(nil ("confidence" :tag))}]]]]])))
    ))

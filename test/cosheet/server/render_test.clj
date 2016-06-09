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
        initial {:priority 0
                 :narrow true
                 :parent-key [:root]
                 :subject-referent root-id}]
    ;; Test a simple cell
    (let [[dom fred] (let-mutated [fred "Fred"]
                   (expr-let [dom (item-DOM-R fred [] initial)]
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
    ;; Test when there are elements.
    (let [[dom age] (let-mutated [age `(39 (:root :non-semantic)
                                           (~o3 :order :non-semantic)
                                           ("one" ; One tag.
                                            ("confidence"
                                             :tag (~o1 :order :non-semantic))
                                            (~o1 :order :non-semantic))
                                           ("two" ; Two tags, one matching.
                                            ("confidence"
                                             :tag (~o1 :order :non-semantic))
                                            ("probability"
                                             :tag (~o2 :order :non-semantic))
                                            (~o2 :order :non-semantic))
                                           ("none" ; No tag.
                                            (~o3 :order :non-semantic)))]
                      (expr-let [dom (item-DOM-R age [] initial)]
                        [dom age]))
          one (first (current-value (matching-elements "one" age)))
          confidence1 (first (current-value
                              (matching-elements "confidence" one)))
          confidence1-tag (first (current-value
                                  (matching-elements :tag confidence1)))
          two (first (current-value (matching-elements "two" age)))
          confidence2 (first (current-value
                              (matching-elements "confidence" two)))
          confidence2-tag (first (current-value
                                  (matching-elements :tag confidence2)))
          probability (first (current-value
                              (matching-elements "probability" two)))
          probability-tag (first (current-value
                                  (matching-elements :tag probability)))
          none (first (current-value (matching-elements "none" age)))
          age-key [:root (:item-id age)]
          tags-key (conj age-key (:item-id one) :outside)
          none-key (conj age-key (:item-id none))
          one-two-referent (union-referent [(item-referent one)
                                            (item-referent two)])]
      (is (check
           dom
           [:div {:class "item with-elements" :key age-key}
            [:div {:class "content-text editable"
                   :key (conj age-key :content)
                   :target {:item-referent (item-referent age)}
                   :commands {:set-content nil
                              :add-element nil
                              :add-sibling nil
                              :delete nil}}
             "39"]
            [:div {:class "stack"}
             ;; Everything with "confidence"
             [:div {:class "wrapped-element tags"}
              [:component {:key (conj tags-key (:item-id confidence1))
                           :class "tag"}
               [item-DOM-R confidence1 [confidence1-tag]
                {:priority 1
                 :narrow true
                 :parent-key tags-key
                 :subject-referent one-two-referent
                 :template '(nil :tag)}]]
              [:div {:class "indent-wrapper"}
               [:div {:class "stack"}
                ;; One
                [:component {:key (conj age-key (:item-id one))}
                 [item-DOM-R one [confidence1]
                  {:priority 1
                   :narrow true,
                   :parent-key age-key
                   :subject-referent (item-referent age)
                   :selectable-attributes
                   {:commands {:add-row nil}
                    :row {:subject-referent (item-referent age)
                          :adjacents-referent one-two-referent}}}]]
                ;; Two (must be nested)
                [:div {:class "wrapped-element tags"}
                 [:component {:key (conj age-key (:item-id two)
                                         :outside (:item-id probability))
                              :class "tag"}
                  [item-DOM-R probability [probability-tag]
                   {:priority 1
                    :narrow true
                    :parent-key (conj age-key (:item-id two) :outside)
                    :subject-referent (item-referent two)
                    :template '(nil :tag)
                    :selectable-attributes
                    {:commands {:add-row nil}
                     :row {:subject-referent (item-referent age)
                           :adjacents-referent one-two-referent}}}]]
                 [:div {:class "indent-wrapper"}
                  [:component {:key (conj age-key (:item-id two))}
                   [item-DOM-R two (as-set [confidence2 probability])
                    {:priority 1
                     :narrow true,
                     :parent-key age-key
                     :subject-referent (item-referent age)
                     :selectable-attributes
                     {:commands {:add-row nil}
                      :row {:subject-referent (item-referent age)
                            :adjacents-referent (item-referent two)
                            :template '(nil ("confidence" :tag))}}}]]]]]]]
             ;; None
             [:div {:class "horizontal-tags-element narrow"}
              [:div {:class "editable tags"
                     :key (conj none-key :outside [:template '(nil :tag)])
                     :commands {:set-content nil}
                     :target {:subject-referent (item-referent none)
                              :adjacents-referent (item-referent none)
                              :position :after
                              :template '(nil :tag)}}]
              [:component {:key none-key}
               [item-DOM-R none nil
                {:priority 1,
                 :narrow true,
                 :parent-key age-key
                 :subject-referent (item-referent age)}]]]]])))
    ))


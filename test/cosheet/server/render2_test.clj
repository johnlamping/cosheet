(ns cosheet.server.render2-test
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
             [render2 :refer :all]
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
    ;; Test a single element.
    (let [[dom age] (let-mutated [age `(39 ("doubtful"
                                            ("confidence" :tag)
                                            (~o1 :order :non-semantic)))]
                      (expr-let [dom (item-DOM-R age [] initial)]
                        [dom age]))
          doubtful (first (current-value (matching-elements "doubtful" age)))
          confidence (first (current-value
                             (matching-elements "confidence" doubtful)))
          confidence-tag (first (current-value (entity/elements confidence)))
          item-key [:root (:item-id age)]]
      (is (check
           dom
           [:div {:class "item with-elements"
                  :key item-key
                  :target {:item-referent (item-referent age)}}
            [:div {:class "content-text editable"
                   :key (conj item-key :content)
                   :target {:item-referent (item-referent age)}
                   :commands {:set-content nil
                              :add-element nil
                              :add-sibling nil
                              :delete nil}}
             "39"]
            [:div {:class "wrapped-element tags"}
             [:component {:key (conj item-key (:item-id confidence))
                          :class "tag"}
              [item-DOM-R confidence nil
               {:priority 1
                :narrow true
                :parent-key item-key
                :subject-referent (item-referent doubtful)
                :selectable-attributes {}
                :template '(nil :tag)}]]
             [:div {:class "indent-wrapper"}
              [:component {:key (conj item-key (:item-id doubtful))}
               [item-DOM-R doubtful [confidence]
                {:priority 1
                 :narrow true,
                 :parent-key item-key
                 :subject-referent (item-referent age)
                 :selectable-attributes
                 {:commands {:add-row nil}
                  :row {:subject-referent (item-referent age)
                        :adjacents-referent (item-referent doubtful)}}}]]]]]
           )))))

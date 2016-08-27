(ns cosheet.server.table-render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [orderable :as orderable]
             [entity :as entity  :refer [label->elements]]
             [expression :refer [expr expr-let expr-seq]]
             [debug :refer [current-value envs-to-list simplify-for-print]]
             entity-impl
             [test-utils :refer [check any as-set evals-to let-mutated]])
            (cosheet.server
             [referent :refer [item-referent union-referent
                               query-referent elements-referent]]
             [item-render :refer [item-without-labels-DOM-R
                                  item-DOM-R]]
             [table-render :refer :all])
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

(deftest table-DOM-test
  (let [inherited {:priority 1
                   :width 3.0
                   :parent-key [:foo]}
        joe-list `("Joe"
                   (:top-level :non-semantic)
                   (~o2 :order :non-semantic)
                   ("male" (~o1 :order :non-semantic))
                   ("married" (~o2 :order :non-semantic))
                   (39 (~o3 :order :non-semantic)
                       ("age" :tag)
                       ("doubtful" "confidence"))
                   (45 (~o4 :order :non-semantic)
                       ("age" :tag))
                   ("Joe" (~o5 :order :non-semantic)
                          ("name" :tag))
                   ("Joseph" (~o6 :order :non-semantic)
                             ("name" :tag (~o1 :order :non-semantic))
                             ("id" :tag (~o2 :order :non-semantic))))
        jane-list `("Jane"
                    (:top-level :non-semantic)
                    (~o1 :order :non-semantic)
                    "plain" "plain")]
    (let [table-list `("table"
                       (~'anything (~'anything ("age" :tag))
                                   (:row-condition :non-semantic))
                       (:none ("single" :tag (~o1 :order :non-semantic))
                              (~o1 :order :non-semantic)
                              (:column :non-semantic))
                       (:none ("name" :tag (~o1 :order :non-semantic))
                              (~o2 :order :non-semantic)
                              (:column :non-semantic))
                       (:none ("name" :tag (~o1 :order :non-semantic))
                              ("id" :tag (~o2 :order :non-semantic))
                              (~o3 :order :non-semantic)
                              (:column :non-semantic))
                       (:none ("name" :tag (~o1 :order :non-semantic))
                              (~o4 :order :non-semantic)
                              (:column :non-semantic))
                       (:none ("age" :tag (~o1 :order :non-semantic))
                              ("id" :tag (~o2 :order :non-semantic))
                              (~o5 :order :non-semantic)
                              (:column :non-semantic)))
          [dom table joe jane] (let-mutated [table table-list
                                             joe joe-list
                                             jane jane-list]
                                 (expr-let [dom (table-DOM-R table inherited)]
                                   [dom table joe jane]))
          query (first (current-value (entity/label->elements
                                       table :row-condition)))
          c1 (first (current-value (label->elements table o1)))
          single (first (current-value (label->elements c1 :tag)))
          single-tag-spec (first (current-value (entity/elements single)))
          c2 (first (current-value (label->elements table o2)))
          name2 (first (current-value (label->elements c2 :tag)))
          name2-tag-spec (first (current-value (entity/elements name2)))
          c3 (first (current-value (label->elements table o3)))
          id3 (first (current-value (label->elements c2 o2)))
          table-key [:foo (:item-id table)]
          row-template '(nil (nil ("age" :tag))
                             (:top-level :non-semantic))
          row-condition '(nil (nil :order :non-semantic)
                              (nil (nil :order :non-semantic) ("age" :tag))
                              (:top-level :non-semantic))
          rows-referent (query-referent row-condition)
          first-column-referent (union-referent
                                 [(item-referent c1)
                                  (elements-referent c1 rows-referent)])
          tag-pattern '[:pattern (nil (:variable (:v :name)
                                                 ((nil :tag) :condition)
                                                 (true :reference)))]]
      (is (check
           dom
           [:div {:class "table"}
            [:div {:class "table-top tag"}
             [:div {:class "table-corner"}]
             (any)]
            [:div {:class "table-body"}
             [:div {:class "table-indent tag"}]
             [:div {:class "table-main"}
              [:div {:class "column-header-sequence"}
               [:component {:key (conj table-key (:item-id single))
                            :class "tag top-level column-header"
                            :style {:width "150px"}}
                [item-without-labels-DOM-R single [single-tag-spec]
                 {:priority 1
                  :width 0.75
                  :parent-key table-key
                  :subject first-column-referent
                  :template '(nil :tag)
                  :selector true
                  :selectable-attributes
                  {:commands {:delete {:delete-referent (item-referent c1)}
                              :add-column {:select-pattern (conj table-key
                                                                 tag-pattern)}
                              :expand {:item-referent first-column-referent}}
                   :column {:adjacent-groups-referent (item-referent c1)
                            :subject-referent (union-referent
                                               [(item-referent table)])
                            :position :after
                            :template '(anything (:column :non-semantic)
                                                 (??? :tag))}}}]]
               (any)
               (any)]
              [:component {:key (conj table-key (:item-id joe))
                           :class "table-row"}
               [table-row-DOM-R
                joe (conj table-key (:item-id joe)) row-template
                [{:column-item c1 :template '(nil ("single" :tag))
                  :exclusions '()}
                 {:column-item c2 :template '(nil ("name" :tag))
                  :exclusions '((nil ("name" :tag) ("id" :tag)))}
                 {:column-item c3 :template '(nil ("name" :tag) ("id" :tag))
                  :exclusions ()}
                 (any)
                 (any)]
                {:priority 1 :width 3.0 :parent-key table-key}]]]]]))
      (let [table-body (nth dom 3)
            table-main (nth table-body 3)
            row-component (nth table-main 3)
            row-command (nth row-component 2)
            row-dom (current-value
                     (apply (first row-command) (rest row-command)))]
        (is (check
             row-dom
             [:div {}
              [:div {:commands {:add-row nil :set-content nil}
                     :row {:item-referent (item-referent joe)
                           :template '(nil (nil ("age" :tag))
                                           (:top-level :non-semantic))}
                     :class "editable table-cell has-border"
                     :key (conj table-key (:item-id joe) (:item-id c1))
                     :target {:template '(nil ("single" :tag))
                              :adjacent-referent (item-referent joe)
                              :position :after
                              :subject-referent (item-referent joe)}}]
              [:component {:key (conj table-key
                                      (:item-id joe) (:item-id c2) (any))
                           :class "table-cell has-border"}
               [item-without-labels-DOM-R (any) [(any)]
                {:priority 1
                 :width 0.75
                 :parent-key (conj table-key (:item-id joe) (any))
                 :subject (item-referent joe)
                 :template '(nil ("name" :tag))
                 :selectable-attributes
                 {:commands {:add-row nil}
                  :row {:item-referent (item-referent joe)
                        :template '(nil (nil ("age" :tag))
                                        (:top-level :non-semantic))}}}]]
              (any)
              (any)
              (any)]))))))

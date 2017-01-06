(ns cosheet.server.table-render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [orderable :as orderable]
             [entity :as entity  :refer [label->elements]]
             [expression :refer [expr expr-let expr-seq]]
             [expression-manager :refer [current-value]]
             [debug :refer [envs-to-list simplify-for-print]]
             entity-impl
             [test-utils :refer [check any as-set evals-to let-mutated]])
            (cosheet.server
             [referent :refer [item-referent union-referent exemplar-referent
                               query-referent elements-referent
                               virtual-referent]]
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
                       (~'anything ("single" :tag (~o1 :order :non-semantic))
                                   (~o1 :order :non-semantic)
                                   (:column :non-semantic))
                       (~'anything ("name" :tag (~o1 :order :non-semantic))
                                   (~o2 :order :non-semantic)
                                   (:column :non-semantic))
                       (~'anything ("name" :tag (~o1 :order :non-semantic))
                                   ("id" :tag (~o2 :order :non-semantic))
                                   (~o3 :order :non-semantic)
                                   (:column :non-semantic))
                       (~'anything ("name" :tag (~o1 :order :non-semantic))
                                   (~o4 :order :non-semantic)
                                   (:column :non-semantic))
                       (~'anything ("age" :tag (~o1 :order :non-semantic))
                                   ("id" :tag (~o2 :order :non-semantic))
                                   (~o5 :order :non-semantic)
                                   (:column :non-semantic))
                       (~'anything ("6-2" (~o1 :order :non-semantic)
                                    ("height" :tag (~o2 :order :non-semantic)))
                                   (~o6 :order :non-semantic)
                                   (:column :non-semantic)))
          [dom table joe jane] (let-mutated [table table-list
                                             joe joe-list
                                             jane jane-list]
                                 (expr-let [dom (table-DOM-R table inherited)]
                                   [dom table joe jane]))
          query (first (current-value (entity/label->elements table 'anything)))
          c1 (first (current-value (label->elements table o1)))
          single (first (current-value (label->elements c1 :tag)))
          single-tag-spec (first (current-value (entity/elements single)))
          c2 (first (current-value (label->elements table o2)))
          name2 (first (current-value (label->elements c2 :tag)))
          name2-tag-spec (first (current-value (entity/elements name2)))
          c3 (first (current-value (label->elements table o3)))
          c6 (first (current-value (label->elements table o6)))
          table-key [:foo (:item-id table)]
          row-template '("" ("" ("age" :tag))
                             (:top-level :non-semantic))
          row-condition (list (item-referent query)
                              '(:top-level :non-semantic))
          rows-referent (query-referent row-condition)
          first-column-referent (union-referent
                                 [(item-referent c1)
                                  (elements-referent c1 rows-referent)])
          tag-pattern '[:pattern (nil (:variable (:v :name)
                                                 ((nil :tag) :condition)
                                                 (true :reference)))]]
      (is (check
           dom
           [:div {:class "table selector-scope"
                  :key table-key}
            [:div {:class "table-top selectors"}
             [:div {:class "table-corner"}]
             (any)]
            [:div {:class "table-body"}
             [:div {:class "table-indent"}]
             [:div {:class "table-main selecteds selector-scope"}
              [:div {:class "column-header-sequence selectors"}
               [:component {:key (conj table-key (:item-id single))
                            :class "tag top-level column-header"
                            :style {:width "150px"}}
                [item-without-labels-DOM-R single [single-tag-spec]
                 {:priority 1
                  :width 0.75
                  :parent-key table-key
                  :subject-referent first-column-referent
                  :template '(nil :tag)
                  :selector-category :table-header
                  :alternate-target true
                  :selectable-attributes
                  {:expand {:referent first-column-referent}
                   :add-column {:referent (virtual-referent
                                           '(anything-immutable
                                             (:column :non-semantic)
                                             (??? :tag))
                                           (union-referent
                                            [(item-referent table)])
                                           (item-referent c1)
                                           :selector :first-group) 
                                :select-pattern (conj table-key tag-pattern)}
                   :delete {:referent first-column-referent}
                   :add-element {:referent first-column-referent}}}]]
               (any)
               (any)
               (any)
               [:div {:selector-category :table-header
                      :class "editable column-header"
                      :key (conj table-key :virtualColumn)
                      :target {:referent
                               (virtual-referent
                                ["" :tag]
                                (virtual-referent
                                 '(anything-immutable (:column :non-semantic))
                                 (item-referent table)
                                 (item-referent c6)
                                 :selector :first-group)
                                (item-referent c6)
                                :selector :first-group)
                               :alternate true}
                      :style {:width "35px"}}]]
              [:div {:class "table-rows selecteds"}
               [:component {:key (conj table-key (:item-id joe))
                            :class "table-row"}
                [table-row-DOM-R
                 joe (conj table-key (:item-id joe)) row-template
                 [{:column-id (:item-id c1) :template '(nil ("single" :tag))
                   :exclusions '()}
                  {:column-id (:item-id c2) :template '(nil ("name" :tag))
                   :exclusions '((nil ("name" :tag) ("id" :tag)))}
                  {:column-id (:item-id c3)
                   :template '(nil ("name" :tag) ("id" :tag))
                   :exclusions ()}
                  (any)
                  (any)
                  (any)
                  {:column-id :virtualColumn
                   :template (virtual-referent
                              '(anything-immutable (??? :tag)
                                                   (:column :non-semantic))
                              (item-referent table)
                              (item-referent c6)
                              :selector :first-group)
                   :exclusions nil}]
                 {:priority 1 :width 3.0 :parent-key table-key}]]
               [:component {:key (conj table-key :virtualRow)
                            :class "table-row"}
                [table-virtual-row-DOM
                 (conj table-key :virtualRow)
                 '("" ("" ("age" :tag)) (:top-level :non-semantic))
                 (item-referent joe)
                 [{:column-id (:item-id c1) :template '(nil ("single" :tag))
                   :exclusions '()}
                  (any) (any) (any) (any) (any)]
                 {:priority 1 :width 3.0 :parent-key table-key}]]]]]]))
      (let [table-body (nth dom 3)
            table-main (nth table-body 3)
            table-rows (nth table-main 3)
            row-component (nth table-rows 2)
            row-command (nth row-component 2)
            row-dom (current-value
                     (apply (first row-command) (rest row-command)))]
        (is (check
             row-dom
             [:div {:key (conj table-key (:item-id joe))}
              [:div {:add-row {:referent (virtual-referent
                                          '("" ("" ("age" :tag))
                                            (:top-level :non-semantic))
                                          nil (item-referent joe)) }
                     :class "editable table-cell has-border"
                     :key (conj table-key (:item-id joe) (:item-id c1))
                     :target {:referent (virtual-referent
                                         '(nil ("single" :tag))
                                         (item-referent joe)
                                         (item-referent joe)
                                         :position :after)}
                     :delete {:referent (item-referent joe)}}]
              [:component {:key (conj table-key
                                      (:item-id joe) (:item-id c2) (any))
                           :class "table-cell has-border"}
               [item-without-labels-DOM-R (any) [(any)]
                {:priority 1
                 :width 0.75
                 :parent-key (conj table-key (:item-id joe) (any))
                 :subject-referent (item-referent joe)
                 :template '(nil ("name" :tag))
                 :selectable-attributes
                 {:add-row {:referent (virtual-referent
                                       '("" ("" ("age" :tag))
                                         (:top-level :non-semantic))
                                       nil (item-referent joe)) }}}]]
              (any)
              (any)
              (any)
              (any)
              [:div {:class "editable table-cell virtual-column has-border"
                     :key (conj table-key (:item-id joe) :virtualColumn)
                     :target {:referent
                              (virtual-referent
                               (virtual-referent
                                '(anything-immutable
                                  (??? :tag) (:column :non-semantic))
                                (item-referent table)
                                (item-referent c6)
                                :selector :first-group)
                               (item-referent joe)
                               (item-referent joe))}}]]))))))

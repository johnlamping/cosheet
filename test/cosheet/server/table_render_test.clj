(ns cosheet.server.table-render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [orderable :as orderable]
             [query :refer [matching-items matching-elements]]
             [entity :as entity  :refer [label->elements elements]]
             [expression :refer [expr expr-let expr-seq]]
             [expression-manager :refer [current-value]]
             [debug :refer [envs-to-list simplify-for-print]]
             entity-impl
             [test-utils :refer [check any as-set evals-to let-mutated]])
            (cosheet.server
             [model-utils :refer [immutable-semantic-to-list]]
             [referent :refer [item-referent union-referent
                               query-referent virtual-referent]]
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
                   :key-prefix [:foo]}
        joe-list `("Joe"
                   :top-level
                   (~o2 :order)
                   ("male" (~o1 :order))
                   ("married" (~o2 :order))
                   (39 (~o3 :order)
                       ("age" :tag)
                       ("doubtful" "confidence"))
                   (45 (~o4 :order)
                       ("age" :tag))
                   ("Joe" (~o5 :order)
                          ("name" :tag))
                   ("Joseph" (~o6 :order)
                             ("name" :tag (~o1 :order))
                             ("id" :tag (~o2 :order))))
        jane-list `("Jane"
                    :top-level
                    (~o1 :order)
                    "plain" "plain")]
    (let [table-list
          `("table"
            (~'anything
             (~'anything ("age" :tag))
             :row-condition
             (~'anything ("single" :tag (~o1 :order))
              (~o1 :order)
              :column)
             (~'anything ("name" :tag (~o1 :order))
              (~o2 :order)
              :column)
             (~'anything ("name" :tag (~o1 :order))
              ("id" :tag (~o2 :order))
              (~o3 :order)
              :column)
             (~'anything ("name" :tag (~o1 :order))
              (~o4 :order)
              :column)
             (~'anything ("age" :tag (~o1 :order))
              ("id" :tag (~o2 :order))
              (~o5 :order)
              :column)
             (~'anything ("6-2" (~o1 :order)
                          ("height" :tag (~o2 :order)))
              (~o6 :order)
              :column)))
          [dom table joe jane] (let-mutated [table table-list
                                             joe joe-list
                                             jane jane-list]
                                 (expr-let [dom (table-DOM-R table inherited)]
                                   [dom table joe jane]))
          query (first (current-value (entity/label->elements
                                       table :row-condition)))
          c1 (first (current-value (label->elements query o1)))
          single (first (current-value (label->elements c1 :tag)))
          single-tag-spec (first (current-value (entity/elements single)))
          c2 (first (current-value (label->elements query o2)))
          name2 (first (current-value (label->elements c2 :tag)))
          name2-tag-spec (first (current-value (entity/elements name2)))
          c3 (first (current-value (label->elements query o3)))
          c6 (first (current-value (label->elements query o6)))
          table-key [:foo (:item-id table)]
          row-template '(anything (anything ("age" :tag)) :top-level)
          row-condition (list (item-referent query)
                              :top-level)
          rows-referent (query-referent row-condition)
          first-column-referent (union-referent [(item-referent c1)])
          delete-column-referent (item-referent c1)
          tag-pattern '[:pattern (nil (:variable (:v :name)
                                                 ((nil :tag) :condition)
                                                 (true :reference)))]
          first-column-add {:referent (virtual-referent
                                       '(??? :tag)
                                       (virtual-referent
                                        '(anything
                                          :column)
                                        (item-referent query)
                                        (item-referent c1))
                                       nil) 
                            :select-pattern (conj table-key
                                                  [:pattern :subject]
                                                  [:pattern])}]
      ;; First, test batch-edit-pattern.
      (let [immutable-query (entity/current-version query)]
        ;; When the item is part of the row condition.
        (is (check (batch-edit-pattern
                    (first (filter #(= (immutable-semantic-to-list %)
                                        `(~'anything ("age" :tag)))
                                   (elements immutable-query)))
                    immutable-query)
                   `(~'anything
                     (~'anything
                      ("age" :tag (~(any) :order))
                      (~(any) :order))
                     (~(any) :order))))
        ;; When the item is part of a column header.
        (is (check (batch-edit-pattern (first (matching-elements
                                           `(~'anything ("single" :tag))
                                           immutable-query))
                                   immutable-query)
               `(~'anything
                 (~'anything
                  ("age" :tag (~(any) :order))
                  (~(any) :order))
                 (~'anything
                  ("single" :tag (~(any) :order))
                  (~(any) :order))
                 (~(any) :order))))
        ;; When the item is an element in the table.
        ;; In this case, the item is a refinement of a table condition,
        ;; so it should replace the condition.
        (is (check (batch-edit-pattern (first (matching-items
                                               '(45 ("age" :tag))
                                               (:store immutable-query)))
                                       immutable-query)
                   `(~'anything
                     (45
                      ("age" :tag (~(any) :order))
                      (~(any) :order))
                     (~(any) :order)))))
      (is (check
           dom
           [:div {:class "table"}
            [:div {:class "query-holder tag"}
             [:div {:class "query-indent tag"}]
             ;; TODO: Add test here too.
             (any)]
            [:div {:class "query-result-wrapper"}
             [:div {:class "query-result-indent tag"}]
             [:div {:class "table-main"}
              [:div {:class "column-header-sequence"}
               [:div {:class "wrapped-element tag column-header leaf"} 
                [:component {:class "tag"
                             :key (conj table-key
                                        (:item-id c1) (:item-id single))}
                 [item-without-labels-DOM-R single [single-tag-spec]
                  {:priority 2
                   :width 0.75
                   :key-prefix (conj table-key (:item-id c1))
                   :subject-referent first-column-referent
                   :template '(anything :tag)
                   :attributes
                   [[#{:label :element :recursive :optional} #{:content}
                     {:add-column first-column-add
                      :delete-column {:referent first-column-referent}}]
                    [#{:content}
                     {:expand {:referent first-column-referent}
                      :delete {:referent nil}}]]}]]
                [:div {:class "indent-wrapper"}
                 [:div {:delete-column {:referent first-column-referent},
                        :delete {:referent nil}
                        :add-column first-column-add
                        :add-twin {:referent nil}
                        :class "placeholder content-text editable item"
                        :target {:referent first-column-referent
                                 :template '(anything :tag)}
                        :key (conj table-key (:item-id c1) :content)}
                  " ..."]]]
               (any)
               (any)
               (any)
               [:div {:class "editable column-header virtual-column"
                      :key (conj table-key :virtualColumn :virtual)
                      :target {:referent
                               (virtual-referent
                                ""
                                (virtual-referent
                                 '(anything
                                   :column)
                                 (item-referent query)
                                 (item-referent c6))
                                (item-referent c6))
                               :select-pattern (conj table-key
                                                     [:pattern :subject]
                                                     [:pattern] )}}]]
              [:div {:class "table-rows"}
               [:component {:key (conj table-key (:item-id joe))
                            :class "table-row"}
                [table-row-DOM-R
                 joe (conj table-key (:item-id joe)) row-template
                 [{:column-id (:item-id c1)
                   :query '(nil ("single" :tag))
                   :template '("" ("single" :tag))
                   :exclusions '()}
                  {:column-id (:item-id c2)
                   :query '(nil ("name" :tag))
                   :template '("" ("name" :tag))
                   :exclusions '((nil ("name" :tag) ("id" :tag)))}
                  {:column-id (:item-id c3)
                   :query '(nil ("name" :tag) ("id" :tag))
                   :template '("" ("name" :tag) ("id" :tag))
                   :exclusions ()}
                  (any)
                  (any)
                  (any)
                  {:column-id :virtualColumn
                   :template (virtual-referent
                              '(anything
                                :column
                                (???))
                              (item-referent query)
                              (item-referent c6))
                   :exclusions nil}]
                 {:priority 3 :width 3.0 :key-prefix table-key}]]
               [:component {:key (conj table-key :virtualRow)
                            :class "table-row"}
                [table-virtual-row-DOM
                 (conj table-key :virtualRow)
                 '(anything (anything ("age" :tag)) :top-level)
                 (item-referent joe)
                 [{:column-id (:item-id c1)
                   :query '(nil ("single" :tag))
                   :template '("" ("single" :tag))
                   :exclusions '()}
                  (any) (any) (any) (any) (any)]
                 {:priority 1 :width 3.0 :key-prefix table-key}]]]]]]))
      (let [table-body (nth dom 3)
            table-main (nth table-body 3)
            table-rows (nth table-main 3)
            row-component (nth table-rows 2)
            row-command (nth row-component 2)
            row-dom (current-value
                     (apply (first row-command) (rest row-command)))]
        (println (simplify-for-print [c1 c2 c3]))
        (is (check
             row-dom
             [:div {}
              [:div {:row {:referent (item-referent joe)
                           :key (conj (vec table-key) (item-referent joe))
                           :template '(anything (anything ("age" :tag))
                                                :top-level)}
                     :column {:referent (item-referent c1)}
                     :class "editable table-cell has-border"
                     :key (conj table-key (:item-id joe) (:item-id c1) :virtual)
                     :target {:referent (virtual-referent
                                         '("" ("single" :tag))
                                         (item-referent joe)
                                         nil
                                         :position :after)
                              :select-pattern (conj table-key
                                                    (:item-id joe) (:item-id c1)
                                                    [:pattern])}}]
              [:component {:key (conj table-key
                                      (:item-id joe) (:item-id c2) (any))
                           :class "table-cell has-border"}
               [item-without-labels-DOM-R (any) [(any)]
                {:priority 4
                 :width 0.75
                 :key-prefix (conj table-key (:item-id joe) (any))
                 :subject-referent (item-referent joe)
                 :template '("" ("name" :tag))
                 :attributes
                 [[#{:label :element :recursive :optional} #{:content}
                   {:row {:referent (item-referent joe)
                          :key (conj (vec table-key) (item-referent joe))
                          :template '(anything (anything ("age" :tag))
                                               :top-level)}}]
                  [#{:label :element :recursive :optional} #{:content}
                   {:column {:referent (item-referent c2)}}]]}]]
              (any)
              (any)
              (any)
              (any)
              [:div {:class "editable table-cell virtual-column has-border"
                     :key (conj table-key (:item-id joe) :virtualColumn :virtual)
                     :row {:referent (item-referent joe)
                           :key (conj (vec table-key) (item-referent joe))
                           :template '(anything (anything ("age" :tag))
                                                :top-level)}
                     :target {:referent
                              (virtual-referent
                               (virtual-referent
                                '(anything
                                  :column
                                  (???))
                                (item-referent query)
                                (item-referent c6))
                               (item-referent joe))
                              :select-pattern (conj table-key
                                                    (:item-id joe)
                                                    [:pattern 1]
                                                    [:pattern])}}]]))))))

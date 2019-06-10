(ns cosheet.server.table-render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [orderable :as orderable]
             [query :refer [matching-items matching-elements not-query]]
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
                        (range 8)))
(def o1 (nth orderables 0))
(def o2 (nth orderables 1))
(def o3 (nth orderables 2))
(def o4 (nth orderables 3))
(def o5 (nth orderables 4))
(def o6 (nth orderables 5))
(def o7 (nth orderables 6))
(def o8 (nth orderables 7))

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
                       ("age" :tag (~o3 :order))
                       ("doubtful" (~o1 :order) ("confidence" (~o3 :order))))
                   (45 (~o4 :order)
                       ("age" :tag (~o3 :order)))
                   ("Joe" (~o5 :order)
                          ("name" :tag (~o3 :order)))
                   ("Joseph" (~o6 :order)
                             ("name" :tag (~o1 :order))
                             ("id" :tag (~o2 :order))))
        jane-list `("Jane"
                    :top-level
                    (~o1 :order)
                    ("plain" (~o2 :order)) ("plain" (~o3 :order)))
        test-list `("TEST"
                    :top-level
                    :test
                    (~o3 :order)
                    ;; Real data won't have 'anything as content, but we want
                    ;; something that is less specific than the table condition
                    ;; to test that it will cause the condition to be
                    ;; eliminated in batch edits.
                    (~'anything (~o3 :order) ("age" :tag (~o3 :order))))]
    (let [table-list
          `("table"
            (~'anything
             :row-condition
             (~'anything
              ("age" :tag (~o1 :order))
              (~o8 :order))
             (~'anything ("single" :tag (~o1 :order))
              (~o1 :order)
              :column)
             (~'anything
              ("name" :tag (~o1 :order))
              (~o2 :order)
              :column)
             (~'anything
              ("name" :tag (~o1 :order))
              ("id" :tag (~o2 :order))
              (~o3 :order)
              :column)
             (~'anything ("name" :tag (~o1 :order))
              (~o4 :order)
              :column)
             (~'anything
              ("age" :tag (~o1 :order))
              ("id" :tag (~o2 :order))
              (~o5 :order)
              :column)
             (~'anything ("6-2" (~o1 :order)
                          ("height" :tag (~o2 :order)))
              (~o6 :order)
              :column)
             ("something" ("child" (~o1 :order))
              (~o7 :order)
              :column)))
          [dom table joe jane test] (let-mutated [table table-list
                                             joe joe-list
                                             jane jane-list
                                             test test-list]
                                 (expr-let [dom (table-DOM-R table inherited)]
                                   [dom table joe jane test]))
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
          c7 (first (current-value (label->elements query o7)))
          table-key [:foo (:item-id table)]
          row-template '(anything (anything ("age" :tag)) :top-level)
          row-condition (list (item-referent query)
                              :top-level)
          first-column-referent (item-referent c1)
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
      ;; First, test batch-edit-selector.
      (let [immutable-query (entity/current-version query)]
        ;; When the item is part of the row condition.
        (is (check (batch-edit-selectors
                    (first (filter #(= (immutable-semantic-to-list %)
                                        `(~'anything ("age" :tag)))
                                   (elements immutable-query)))
                    nil
                    immutable-query)
                   [`(~'anything
                      (~'anything
                       ("age" :tag (~(any) :order))
                       (~(any) :order))
                       :batch-row-selector
                      (~(any) :order)
                      :batch-selector :selector)]))
        ;; When the item is part of a column header. In that case,
        ;; it is presented in the explicit list of batch edit items.
        (is (check (batch-edit-selectors nil
                                         [(first (matching-elements
                                           `(~'anything ("single" :tag))
                                           immutable-query))]
                                         immutable-query)
                   [`(~'anything
                      (~'anything
                       ("age" :tag (~(any) :order))
                       (~(any) :order))
                      :batch-row-selector
                      (~(any) :order)
                      :batch-selector :selector)
                    `(~'anything
                      (~'anything
                       ("single" :tag (~(any) :order))
                       (~(any) :order))
                      :batch-elements
                      (~(any) :order)
                      :batch-selector :selector)]))
        ;; When the item is an element in the table.
        (is (check (batch-edit-selectors (first (matching-items
                                               '(45 ("age" :tag))
                                               (:store immutable-query)))
                                         nil
                                         immutable-query)
                   [`(~'anything
                      (~'anything
                       ("age" :tag (~(any) :order))
                       (~(any) :order))
                      :batch-row-selector
                      (~(any) :order)
                      :batch-selector :selector)
                    `(~'anything
                      (45
                       ("age" :tag (~(any) :order))
                       (~(any) :order))
                      :batch-elements
                      (~(any) :order)
                      :batch-selector :selector)])))
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
                [:div
                 {:key (conj table-key (:item-id single) :content)
                  :class "content-text editable item tag"
                  :target {:template '(anything :tag)
                           :referent (item-referent single)}
                  :add-column first-column-add
                  :batch-edit-ids [(:item-id c1)]
                  :delete-column {:referent first-column-referent}}
                 "single"]
                [:div {:class "indent-wrapper"}
                 [:div {:delete-column {:referent first-column-referent},
                        :batch-edit-ids [(:item-id c1)]
                        :delete {:clear-only true}
                        :add-column first-column-add
                        :add-twin {:referent nil}
                        :class "placeholder content-text editable item"
                        :target {:referent first-column-referent
                                 :template '(anything :column)}
                        :key (conj table-key (:item-id c1) :content)}
                  " ..."]]]
               (any)
               (any)
               (any)
               ;; A column with no label, but content and a sub-element.
               [:div {:class (str "tag wrapped-element"
                                  " virtual-wrapper column-header leaf")}
                ;; The virtual label
                [:div {:add-column (any)
                       :delete-column (any)
                       :class "editable tag"
                       :key (conj table-key (:item-id c7) :virtual)
                       :target (any)
                       :batch-edit-ids [(:item-id c7)]}]
                ;;; The content and sub-element.
                [:div {:class "indent-wrapper tag"}
                 [:div {:class "item with-elements"
                        :key (conj table-key (:item-id c7))}
                  [:div (any) "something"]
                  [:div {:class (str "horizontal-tags-element tag"
                                     " virtual-wrapper narrow")}
                   [:div (any)]
                   [:div (any) "child"]]]]]
               [:div {:class "editable column-header virtual-column tag"
                      :key (conj table-key :virtualColumn :virtual)
                      :target {:referent
                               (virtual-referent
                                '(anything :tag)
                                (virtual-referent
                                 '(anything
                                   :column)
                                 (item-referent query)
                                 (item-referent c7))
                                (item-referent c7))
                               :select-pattern (conj table-key
                                                     [:pattern :subject]
                                                     [:pattern])}}]]
              [:div {:class "table-rows"}
               [:component {:key (conj table-key (:item-id joe))
                            :class "table-row"}
                [table-row-DOM-R
                 joe (conj table-key (:item-id joe)) row-template
                 [{:column-id (:item-id c1)
                   :query `(nil ("single" :tag) ~(not-query :tag) (nil :order))
                   :template '("" ("single" :tag))
                   :exclusions '()}
                  {:column-id (:item-id c2)
                   :query `(nil ("name" :tag) ~(not-query :tag) (nil :order))
                   :template '("" ("name" :tag))
                   :exclusions `((nil ("name" :tag) ("id" :tag)
                                      ~(not-query :tag) (nil :order)))}
                  {:column-id (:item-id c3)
                   :query `(nil ("name" :tag) ("id" :tag)
                                ~(not-query :tag) (nil :order))
                   :template '("" ("name" :tag) ("id" :tag))
                   :exclusions ()}
                  (any)
                  (any)
                  (any)
                  (any)
                  {:column-id :virtualColumn
                   :template (virtual-referent
                              '(anything
                                :column
                                (??? :tag))
                              (item-referent query)
                              (item-referent c7))
                   :exclusions nil}]
                 {:priority 3 :width 3.0 :key-prefix table-key}]]
               (any) ; The test row.
               [:component {:key (conj table-key :virtualRow)
                            :class "table-row"}
                [table-virtual-row-DOM
                 (conj table-key :virtualRow)
                 '(anything (anything ("age" :tag)) :top-level)
                 (item-referent test)
                 [{:column-id (:item-id c1)
                   :query `(nil ("single" :tag) ~(not-query :tag) (nil :order))
                   :template '("" ("single" :tag))
                   :exclusions '()}
                  (any) (any) (any) (any) (any) (any)]
                 {:priority 1 :width 3.0 :key-prefix table-key}]]]]]]))
      (let [table-body (nth dom 3)
            table-main (nth table-body 3)
            table-rows (nth table-main 3)
            row-component (nth table-rows 2)
            row-command (nth row-component 2)
            row-dom (current-value
                     (apply (first row-command) (rest row-command)))]
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
              [:div
               {:key (conj table-key
                           (:item-id joe) (:item-id c2) (any) :content)
                :class "content-text editable item table-cell has-border"
                :target {:template '("" ("name" :tag))
                         :referent (any)}
                :column {:referent (:item-id c2)}
                :row {:referent (item-referent joe)
                          :key (conj (vec table-key) (item-referent joe))
                          :template '(anything (anything ("age" :tag))
                                               :top-level)}}
               "Joe"]
              (any)
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
                                  (??? :tag))
                                (item-referent query)
                                (item-referent c7))
                               (item-referent joe))
                              :select-pattern (conj table-key
                                                    (:item-id joe)
                                                    [:pattern 1]
                                                    [:pattern])}}]]))))))

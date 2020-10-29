(ns cosheet2.server.table-render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet2
             [orderable :as orderable]
             [store :refer [new-element-store]]
             store-impl
             [store-utils :refer [add-entity]]
             [query :refer [matching-items matching-elements not-query]]
             [entity :as entity  :refer [description->entity
                                         label->elements elements to-list]]
             [expression :refer [expr expr-let expr-seq]]
             [debug :refer [simplify-for-print]]
             entity-impl
             [test-utils :refer [check any as-set]])
            (cosheet2.server
             [item-render :refer [render-virtual-DOM
                                  get-virtual-DOM-rendering-data]]
             [action-data :refer [composed-get-action-data
                                  get-item-or-exemplar-action-data
                                  get-item-or-exemplar-action-data-for-ids
                                  get-content-only-action-data
                                  get-column-action-data
                                  get-row-action-data
                                  get-virtual-action-data]]
             [hierarchy :refer [hierarchy-by-labels]]
             [order-utils :refer [order-entities]]
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

(def virt-DOM render-virtual-DOM)
(def cell-DOM render-table-cell-DOM)
(def cell-RD get-table-cell-DOM-rendering-data)

(def comp-AD composed-get-action-data)
(def ids-AD get-item-or-exemplar-action-data-for-ids)
(def item-AD get-item-or-exemplar-action-data)
(def virt-AD get-virtual-action-data)
(def col-AD get-column-action-data)
(def row-AD get-row-action-data)

(def virt-RD get-virtual-DOM-rendering-data)

(deftest table-DOM-test
  (let [specification {:width 3.0
                       :elements-template 'anything}
          joe-list `("Joe"
                     :top-level
                     (~o2 :order)
                     ("male" (~o1 :order))
                     ("married" (~o2 :order))
                     (39 (~o3 :order)
                         ("age" :label (~o3 :order))
                         ("doubtful" (~o1 :order) ("confidence" (~o3 :order))))
                     (45 (~o4 :order)
                         ("age" :label (~o3 :order)))
                     ("Joe" (~o5 :order)
                      ("name" :label (~o3 :order)))
                     ("Joseph" (~o6 :order)
                      ("name" :label (~o1 :order))
                      ("id" :label (~o2 :order))))
          jane-list `("Jane"
                      :top-level
                      (~o1 :order)
                      ("plain" (~o2 :order)) ("plain" (~o3 :order)))
          test-list `("TEST"
                      :top-level
                      :test
                      (~o3 :order)
                      ;; Real data won't have 'anything as content,
                      ;; but we want something that is less specific
                      ;; than the table condition to test that it will
                      ;; cause the condition to be eliminated in batch
                      ;; edits.
                      (~'anything (~o3 :order) ("age" :label (~o3 :order))))
        table-list `("table"
                       (~'anything
                        :row-condition
                        (~'anything
                         ("age" :label (~o1 :order))
                         (~o8 :order))
                        (~'anything ("single" :label (~o1 :order))
                         (~o1 :order)
                         :column)
                        (~'anything
                         ("name" :label (~o1 :order))
                         (~o2 :order)
                         :column)
                        (~'anything
                         ("name" :label (~o1 :order))
                         ("other" :label (~o2 :order))
                         (~o3 :order)
                         :column)
                        (~'anything ("name" :label (~o1 :order))
                         (~o4 :order)
                         :column)
                        (~'anything
                         ("age" :label (~o1 :order))
                         ("other" :label (~o2 :order))
                         (~o5 :order)
                         :column)
                        (~'anything ("6-2" (~o1 :order)
                                     ("height" :label (~o2 :order)))
                         (~o6 :order)
                         :column)
                        ("something" ("child" (~o1 :order))
                         (~o7 :order)
                         :column)))
        [s1 joe-id] (add-entity (new-element-store) nil joe-list)
        [s2 jane-id] (add-entity s1 nil jane-list)
        [s3 test-id] (add-entity s2 nil test-list)
        [store table-id] (add-entity s3 nil table-list)
        joe (description->entity joe-id store)
        table (description->entity table-id store)
        row-condition (first (entity/label->elements table :row-condition))
        row-condition-id (:item-id row-condition)
        rc1 (first (matching-elements `(nil ~o8) row-condition))
        rc1-id (:item-id rc1)
        c1 (first (matching-elements `(nil ~o1) row-condition))
        c1-id (:item-id c1)
        c2 (first (matching-elements `(nil ~o2) row-condition))
        c2-id (:item-id c2)
        c2-name (first (matching-elements "name" c2))
        c2-name-id (:item-id c2-name)
        c3 (first (matching-elements `(nil ~o3) row-condition))
        c3-id (:item-id c3)
        c3-name (first (matching-elements "name" c3))
        c3-name-id (:item-id c3-name)
        c4 (first (matching-elements `(nil ~o4) row-condition))
        c4-id (:item-id c4)
        c5 (first (matching-elements `(nil ~o5) row-condition))
        c5-id (:item-id c5)
        c6 (first (matching-elements `(nil ~o6) row-condition))
        c6-id (:item-id c6)
        c7 (first (matching-elements `(nil ~o7) row-condition))
        c7-id (:item-id c7)
        columns (order-entities
                 (label->elements row-condition :column))
        hierarchy (hierarchy-by-labels columns)
        column-descriptions (concat
                             (mapcat table-hierarchy-node-column-descriptions
                                     hierarchy)
                             [{:column-id :virtualColumn
                                :query '(anything :column)}])
        joe-row-component (table-row-DOM-component
                           joe-id '("" :top-level ("age" :label))
                           column-descriptions {:priority 1})]
    (is (check
         (table-condition-DOM {:relative-id row-condition-id} store)
         [:div {:class "query-holder label"}
          [:div {:class "query-indent label"}]
          [:div {:class "horizontal-labels-element label query-condition"}
           ;; A virtual label for the condition
           [:component {:template '(anything :label)
                        :relative-id :virtual-label
                        :class "label"
                        :render-dom render-virtual-DOM
                        :get-rendering-data get-virtual-DOM-rendering-data
                        :get-action-data [get-virtual-action-data
                                          :template '(anything :label)
                                          :position :after]}]
           ;; The condition element.
           [:div {:class "horizontal-stack"}
            [:div {:class "vertical-labels-element label"}
             [:component {:template '("" :label)
                          :get-action-data
                          [composed-get-action-data
                           [get-item-or-exemplar-action-data-for-ids [rc1-id]]
                           get-item-or-exemplar-action-data]
                          :class "label"
                          :excluded-element-ids [(any)]
                          :relative-id (any)}]
             [:component {:template '(anything ("age" :label))
                          :excluded-element-ids [(any)]
                          :relative-id rc1-id}]]
            ;; A virtual element for more condition.
            [:div {:class "vertical-labels-element label"}
             [:component {:relative-id :virtual-label
                          :get-action-data [composed-get-action-data
                                            [get-virtual-action-data
                                             :template 'anything]
                                            [get-virtual-action-data
                                             :template '("" :label)
                                             :position :before]]
                          :class "label"
                          :render-dom render-virtual-DOM
                          :get-rendering-data get-virtual-DOM-rendering-data}]
             [:component {:template 'anything
                          :relative-id :virtual
                          :render-dom render-virtual-DOM
                          :get-rendering-data get-virtual-DOM-rendering-data
                          :get-action-data [get-virtual-action-data
                                            :template 'anything]}]]]]]))
    (is (check
         (table-header-DOM row-condition-id hierarchy)
         [:div {:class "column-header-sequence"}
          ;; A single column.
          [:component {:get-column-action-data
                       [col-AD row-condition-id [c1-id]]
                       :width 0.75
                       :template :singular
                       :relative-id c1-id
                       :class "column-header leaf"}]
          ;; Three columns.
          [:div {:class "column-header label"}
           ;; The label for the three columns
           [:component {:get-column-action-data
                        [col-AD row-condition-id [c2-id c3-id c4-id]]
                        :width 2.25
                        :template '(anything :label)
                        :get-action-data
                        [comp-AD [ids-AD [c2-id c3-id c4-id]] item-AD]
                        :relative-id c2-name-id
                        :class "label with-children"
                        :excluded-element-ids [(any)]}]
           [:div {:class "column-header-sequence"}
            ;; A column with only a virtual label
            [:div {:class (str "label wrapped-element virtual-wrapper"
                               " merge-with-parent column-header leaf")}
             [:component {:get-column-action-data
                          [col-AD row-condition-id [c2-id]]
                          :width 0.75
                          :template '(anything :label)
                          :get-action-data
                          [comp-AD [ids-AD [c2-id]]
                           [virt-AD :template '(anything :label)]]
                          :relative-id [c2-id :nested]
                          :class "label merge-with-parent"
                          :render-dom virt-DOM
                          :get-rendering-data virt-RD}]
             [:div {:class "indent-wrapper label"}
              [:component {:get-column-action-data
                           [col-AD row-condition-id [c2-id]]
                           :width 0.75
                           :template :singular
                           :relative-id c2-id
                           :excluded-element-ids [c2-name-id]}]]]
            ;; A column with an additional label
            [:component {:get-column-action-data
                         [col-AD row-condition-id [c3-id]]
                         :width 0.75
                         :template :singular
                         :relative-id c3-id
                         :excluded-element-ids [c3-name-id]
                         :class "column-header leaf"}]
            ;; A column with only a virtual label
            [:div {:class (str "label wrapped-element virtual-wrapper"
                               " merge-with-parent column-header leaf")}
             [:component {:get-column-action-data
                          [col-AD row-condition-id [c4-id]]
                          :width 0.75
                          :template '(anything :label)
                          :get-action-data
                          [comp-AD [ids-AD [c4-id]]
                           [virt-AD :template '(anything :label)]]
                          :relative-id [c4-id :nested]
                          :class "label merge-with-parent"
                          :render-dom virt-DOM
                          :get-rendering-data virt-RD}]
             [:div {:class "indent-wrapper label"}
              [:component {:get-column-action-data
                           [col-AD row-condition-id [c4-id]]
                           :width 0.75
                           :template :singular
                           :relative-id c4-id
                           :excluded-element-ids [(any)]}]]]]]
          ;; One column with two labels
          [:component {:get-column-action-data
                       [col-AD row-condition-id [c5-id]]
                       :width 0.75
                       :template :singular
                       :relative-id c5-id
                       :class "column-header leaf"}]
          ;; One column with no labels
          [:div {:class (str "label wrapped-element virtual-wrapper"
                             " column-header leaf")}
           [:component {:get-column-action-data
                        [col-AD row-condition-id [c6-id]]
                        :width 0.75
                        :template '(anything :label)
                        :get-action-data
                        [comp-AD [ids-AD [c6-id]]
                         [virt-AD :template '(anything :label)]]
                        :class "label"
                        :relative-id [c6-id :nested]
                        :render-dom virt-DOM
                        :get-rendering-data virt-RD}]
           [:div {:class "indent-wrapper label"}
            [:component {:get-column-action-data
                         [col-AD row-condition-id [c6-id]]
                         :width 0.75
                         :template :singular
                         :relative-id c6-id}]]]
          ;; One column with no labels and non-empty content.
          [:div {:class (str "label wrapped-element virtual-wrapper"
                             " column-header leaf")}
           [:component {:get-column-action-data
                        [col-AD row-condition-id [c7-id]]
                        :width 0.75
                        :template '(anything :label)
                        :get-action-data
                        [comp-AD [ids-AD [c7-id]]
                         [virt-AD :template '(anything :label)]]
                        :class "label"
                        :relative-id [c7-id :nested]
                        :render-dom virt-DOM
                        :get-rendering-data virt-RD}]
           [:div {:class "indent-wrapper label"}
            [:component {:get-column-action-data
                         [col-AD row-condition-id [c7-id]]
                         :width 0.75
                         :template :singular
                         :relative-id c7-id}]]]
          ;; The virtual column.
          [:div {:class "horizontal-labels-element label"}
           [:component {:relative-id :virtual-label
                        :get-action-data
                        [comp-AD [ids-AD [c7-id]]
                         [virt-AD :template '(??? :column)
                          :sibling true]
                         [virt-AD :template '("" :label)
                          :position :before]]
                        :class "label"
                        :render-dom virt-DOM
                        :get-rendering-data virt-RD}]
           [:component {:relative-id :virtual-column
                        :template '(??? :column)
                        :class "column-header virtual-column"
                        :get-action-data
                        [comp-AD [ids-AD [c7-id]]
                         [virt-AD :template '(??? :column)
                          :sibling true]]
                        :render-dom virt-DOM
                        :get-rendering-data virt-RD}]]]))
    (is (check
         column-descriptions
         [{:column-id c1-id
           :query '(nil ("single" :label)
                        (:cosheet2.query/special-form
                         (:not :cosheet2.query/type)
                         (:label :cosheet2.query/sub-query))
                        (nil :order))}
          {:column-id c2-id
           :query '(nil ("name" :label)
                        (:cosheet2.query/special-form
                         (:not :cosheet2.query/type)
                         (:label :cosheet2.query/sub-query))
                        (nil :order))
           :exclusions '((nil ("name" :label)
                              ("other" :label)
                              (:cosheet2.query/special-form
                               (:not :cosheet2.query/type)
                               (:label :cosheet2.query/sub-query))
                              (nil :order)))}
          {:column-id c3-id
           :query '(nil ("name" :label)
                        ("other" :label)
                        (:cosheet2.query/special-form
                         (:not :cosheet2.query/type)
                         (:label :cosheet2.query/sub-query))
                        (nil :order))}
          (any) (any) (any) (any) (any)]))
    (is (check
         joe-row-component
         [:component {:priority 1
                      :relative-id joe-id
                      :column-descriptions column-descriptions
                      :render-dom render-table-row-DOM
                      :get-row-action-data
                      [get-row-action-data
                       nil '("" :top-level ("age" :label))]}]))
    (is (check
         (render-table-row-DOM (second joe-row-component) store)
         [:div {}
          [:component {:priority 2
                       :relative-id c1-id
                       :template '("" ("single" :label))
                       :render-dom cell-DOM
                       :get-rendering-data cell-RD
                       :items nil}]
          [:component {:priority 2,
                       :relative-id c2-id
                       :template '("" ("name" :label))
                       :render-dom cell-DOM
                       :get-rendering-data cell-RD
                       :items (as-set
                               [(first (matching-elements "Joe" joe))
                                (first (matching-elements "Joseph" joe))])}]
          [:component {:priority 2
                       :relative-id c3-id
                       :template '("" ("name" :label)
                                      ("other" :label))
                       :render-dom cell-DOM
                       :get-rendering-data cell-RD
                       :items nil}]
          (any) (any) (any) (any)
          [:component {:priority 2
                       :relative-id joe-id
                       :template '(anything :column)
                       :render-dom virt-DOM
                       :get-rendering-data virt-RD
                       :get-action-data
                       ;; TODO: The action data needs to start with adding
                       ;; something to the header, then adding to the row.
                       [virt-AD :template '(anything :column)]
                       :class "table-cell virtual-column has-border"}]]))))

(comment
  (deftest table-DOM-test
    (let [inherited {:priority 1
                     :width 3.0
                     :key-prefix [:foo]
                     :elements-template 'anything}
          joe-list `("Joe"
                     :top-level
                     (~o2 :order)
                     ("male" (~o1 :order))
                     ("married" (~o2 :order))
                     (39 (~o3 :order)
                         ("age" :label (~o3 :order))
                         ("doubtful" (~o1 :order) ("confidence" (~o3 :order))))
                     (45 (~o4 :order)
                         ("age" :label (~o3 :order)))
                     ("Joe" (~o5 :order)
                      ("name" :label (~o3 :order)))
                     ("Joseph" (~o6 :order)
                      ("name" :label (~o1 :order))
                      ("id" :label (~o2 :order))))
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
                      (~'anything (~o3 :order) ("age" :label (~o3 :order))))]
      (let [table-list
            `("table"
              (~'anything
               :row-condition
               (~'anything
                ("age" :label (~o1 :order))
                (~o8 :order))
               (~'anything ("single" :label (~o1 :order))
                (~o1 :order)
                :column)
               (~'anything
                ("name" :label (~o1 :order))
                (~o2 :order)
                :column)
               (~'anything
                ("name" :label (~o1 :order))
                ("id" :label (~o2 :order))
                (~o3 :order)
                :column)
               (~'anything ("name" :label (~o1 :order))
                (~o4 :order)
                :column)
               (~'anything
                ("age" :label (~o1 :order))
                ("id" :label (~o2 :order))
                (~o5 :order)
                :column)
               (~'anything ("6-2" (~o1 :order)
                            ("height" :label (~o2 :order)))
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
            rc1 (first (current-value (label->elements query o8)))
            c1 (first (current-value (label->elements query o1)))
            single (first (current-value (label->elements c1 :label)))
            single-label-spec (first (current-value (entity/elements single)))
            c2 (first (current-value (label->elements query o2)))
            name2 (first (current-value (label->elements c2 :label)))
            name2-label-spec (first (current-value (entity/elements name2)))
            c3 (first (current-value (label->elements query o3)))
            c4 (first (current-value (label->elements query o4)))
            c6 (first (current-value (label->elements query o6)))
            c7 (first (current-value (label->elements query o7)))
            c2-c3-c4-ids [(:item-id c2) (:item-id c3) (:item-id c4)]
            table-key [:foo (:item-id table)]
            row-template '(anything (anything ("age" :label)) :top-level)
            row-condition (list (item-referent query)
                                :top-level)
            first-column-referent (item-referent c1)
            delete-column-referent (item-referent c1)
            label-pattern '[:pattern (nil (:variable (:v :name)
                                                   ((nil :label) :condition)
                                                   (true :reference)))]
            first-column-add {:referent (virtual-referent
                                         '(??? :label)
                                         (virtual-referent
                                          '(anything
                                            :column)
                                          (item-referent query)
                                          (item-referent c1))
                                         nil) 
                              :select-pattern (conj table-key
                                                    [:pattern :subject]
                                                    [:pattern])}]
        ;; First, test batch-edit-containment-path
        (let [immutable-query (entity/current-version query)]
          ;; When the item is part of the row condition.
          (let [rc1 (entity/current-version rc1)]
            (is (check (batch-edit-containment-path rc1)
                       [[rc1] nil])))
          ;; When the item is part of a column header. In that case,
          ;; it is presented in the explicit list of batch edit items.
          (let [c1 (entity/current-version c1)]
            (is (check (batch-edit-containment-path c1)
                       [[c1] true])))
          ;; When the item is an element in the table.
          (let [a45 (first (matching-items
                            '(45 ("age" :label))
                            (:store immutable-query)))
                a45a (first (label->elements a45 :label))]
            (is (check (batch-edit-containment-path a45a)
                       [[a45 a45a] true])))
          ;; Now, check batch-edit-selectors
          ;; First, with no elements
          (is (check (batch-edit-selectors immutable-query nil)
                     [`(~'anything
                        (~'anything
                         ("age" :label (~(any) :order))
                         (~(any) :order))
                        :batch-row-selector
                        (~(any) :order)
                        :batch-selector :selector)]))
          ;; Then with an element
          (is (check (batch-edit-selectors immutable-query
                                           [(first (matching-elements
                                                    `(~'anything ("single" :label))
                                                    immutable-query))])
                     [`(~'anything
                        (~'anything
                         ("age" :label (~(any) :order))
                         (~(any) :order))
                        :batch-row-selector
                        (~(any) :order)
                        :batch-selector :selector)
                      `(~'anything
                        (~'anything
                         ("single" :label (~(any) :order))
                         (~(any) :order))
                        :batch-elements
                        (~(any) :order)
                        :batch-selector :selector)])))
        (is (check
             dom
             [:div {:class "table"}
              [:div {:class "query-holder label"}
               [:div {:class "query-indent label"}]
               ;; TODO: Add test here too.
               (any)]
              [:div {:class "query-result-wrapper"}
               [:div {:class "query-result-indent label"}]
               [:div {:class "table-main"}
                [:div {:class "column-header-sequence"}
                 [:div {:class "wrapped-element label column-header leaf"}
                  [:div
                   {:key (conj table-key (:item-id single) :content)
                    :class "content-text editable item label"
                    :target {:template '(anything :label)
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
                                   :template '("" ("single" :label))}
                          :key (conj table-key (:item-id c1) :content)}
                    " ..."]]]
                 [:div
                  {:class "column-header label"}
                  [:div {:batch-edit-ids (as-set c2-c3-c4-ids)
                         :add-column (any)
                         :class "content-text editable item label with-children",
                         :target (any)
                         :key (any)}
                   "name"]
                  [:div {:class "column-header-sequence"}
                   [:div {:class (str "label wrapped-element virtual-wrapper"
                                      " merge-with-parent column-header leaf")}
                    [:div {:batch-edit-ids (as-set c2-c3-c4-ids)
                           :add-column (any)
                           :delete-column (any)
                           :class "editable label merge-with-parent"
                           :key (any)
                           :target (any)}]
                    [:div {:class "indent-wrapper label"}
                     [:div {:batch-edit-ids (as-set c2-c3-c4-ids)
                            :add-column (any)
                            :delete-column (any)
                            :add-twin (any)
                            :delete {:clear-only true}
                            :class "placeholder content-text editable item"
                            :target (any)
                            :key (any)}
                      " ..."]]]
                   [:div {:class "wrapped-element label column-header leaf"}
                    [:div {:batch-edit-ids [(:item-id c3)]
                           :add-column (any)
                           :delete-column (any)
                           :class "content-text editable item label"
                           :target (any)
                           :key (any)}
                     "id"]
                    [:div {:class "indent-wrapper"}
                     [:div {:batch-edit-ids [(:item-id c3)]
                            :add-column (any)
                            :delete-column (any)
                            :add-twin (any)
                            :delete (any)
                            :class "placeholder content-text editable item"
                            :target (any)
                            :key (any)}
                      " ..."]]]
                   [:div {:class (str "label wrapped-element virtual-wrapper"
                                      " merge-with-parent column-header leaf")}
                    [:div {:batch-edit-ids (as-set c2-c3-c4-ids)
                           :add-column (any)
                           :delete-column (any)
                           :class "editable label merge-with-parent"
                           :key (any)
                           :target (any)}]
                    [:div {:class "indent-wrapper label"}
                     [:div {:batch-edit-ids (as-set c2-c3-c4-ids)
                            :add-column (any)
                            :delete-column (any)
                            :add-twin {:referent nil}
                            :delete {:clear-only true}
                            :class "placeholder content-text editable item"
                            :target (any)
                            :key (any)}
                      " ..."]]]]]
                 (any)
                 (any)
                 ;; A column with no label, but content and a sub-element.
                 [:div {:class (str "label wrapped-element"
                                    " virtual-wrapper column-header leaf")}
                  ;; The virtual label
                  [:div {:add-column (any)
                         :delete-column (any)
                         :class "editable label"
                         :key (conj table-key (:item-id c7) :virtual)
                         :target (any)
                         :batch-edit-ids [(:item-id c7)]}]
                ;;; The content and sub-element.
                  [:div {:class "indent-wrapper label"}
                   [:div {:class "item with-elements"
                          :key (conj table-key (:item-id c7))}
                    [:div (any) "something"]
                    [:div {:class (str "horizontal-labels-element label"
                                       " virtual-wrapper narrow")}
                     [:div (any)]
                     [:div (any) "child"]]]]]
                 [:div {:class "editable column-header virtual-column label"
                        :key (conj table-key :virtualColumn :virtual)
                        :target {:referent
                                 (virtual-referent
                                  '(anything :label)
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
                     :query `(nil ("single" :label) ~(not-query :label) (nil :order))
                     :template '("" ("single" :label))
                     :exclusions '()}
                    {:column-id (:item-id c2)
                     :query `(nil ("name" :label) ~(not-query :label) (nil :order))
                     :template '("" ("name" :label))
                     :exclusions `((nil ("name" :label) ("id" :label)
                                        ~(not-query :label) (nil :order)))}
                    {:column-id (:item-id c3)
                     :query `(nil ("name" :label) ("id" :label)
                                  ~(not-query :label) (nil :order))
                     :template '("" ("name" :label) ("id" :label))
                     :exclusions ()}
                    (any)
                    (any)
                    (any)
                    (any)
                    {:column-id :virtualColumn
                     :template (virtual-referent
                                '(anything
                                  :column
                                  (??? :label))
                                (item-referent query)
                                (item-referent c7))
                     :exclusions nil}]
                   (assoc inherited :priority 3 :key-prefix table-key)]]
                 (any) ; The test row.
                 [:component {:key (conj table-key :virtualRow)
                              :class "table-row"}
                  [table-virtual-row-DOM
                   (conj table-key :virtualRow)
                   '(anything (anything ("age" :label)) :top-level)
                   (item-referent test)
                   [{:column-id (:item-id c1)
                     :query `(nil ("single" :label) ~(not-query :label) (nil :order))
                     :template '("" ("single" :label))
                     :exclusions '()}
                    (any) (any) (any) (any) (any) (any)]
                   (assoc inherited :key-prefix table-key)]]]]]]))
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
                             :template '(anything (anything ("age" :label))
                                                  :top-level)}
                       :column {:referent (item-referent c1)}
                       :class "editable table-cell has-border"
                       :key (conj table-key (:item-id joe) (:item-id c1) :virtual)
                       :target {:referent (virtual-referent
                                           '("" ("single" :label))
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
                  :target {:template '("" ("name" :label))
                           :referent (any)}
                  :column {:referent (:item-id c2)}
                  :row {:referent (item-referent joe)
                        :key (conj (vec table-key) (item-referent joe))
                        :template '(anything (anything ("age" :label))
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
                             :template '(anything (anything ("age" :label))
                                                  :top-level)}
                       :target {:referent
                                (virtual-referent
                                 (virtual-referent
                                  '(anything
                                    :column
                                    (??? :labe))
                                  (item-referent query)
                                  (item-referent c7))
                                 (item-referent joe))
                                :select-pattern (conj table-key
                                                      (:item-id joe)
                                                      [:pattern 1]
                                                      [:pattern])}}]])))))))

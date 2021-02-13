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
                                  get-virtual-DOM-rendering-data
                                  get-item-rendering-data]]
             [action-data :refer [composed-get-action-data
                                  parallel-items-get-action-data
                                  get-pass-through-action-data
                                  get-id-action-data
                                  get-item-or-exemplar-action-data
                                  get-item-do-batch-edit-action-data
                                  parallel-items-get-do-batch-edit-action-data
                                  get-virtual-action-data]]
             [hierarchy :refer [hierarchy-by-labels
                                replace-hierarchy-leaves-by-nodes] :as hierarchy]
             [order-utils :refer [ordered-entities add-order-elements]]
             [model-utils :refer [semantic-to-list]]
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

;;; We make functions that abbreviate the common functions that can be
;;; embedded in components.
;;; (We use functions, rather than constants, so this file doesn't have
;;; to be reloaded if any of the files that defines the underlying
;;; functions is reloaded.)

(defn virt-DOM [] render-virtual-DOM)
(defn cell-DOM [] render-table-cell-DOM)

(defn cell-RD [] get-table-cell-rendering-data)
(defn virt-RD [] get-virtual-DOM-rendering-data)

(defn comp-AD [] composed-get-action-data)
(defn pass-AD [] get-pass-through-action-data)
(defn parallel-AD [] parallel-items-get-action-data)
(defn id-AD [] get-id-action-data)
(defn item-AD [] get-item-or-exemplar-action-data)
(defn virt-AD [] get-virtual-action-data)
(defn col-AD [] get-column-action-data)
(defn row-AD [] get-row-action-data)
(defn table-head-do-batch-AD [] get-table-header-do-batch-edit-action-data)
(defn table-cell-do-batch-AD [] get-table-cell-do-batch-edit-action-data)
(defn table-cell-item-do-batch-AD []
  get-table-cell-item-do-batch-edit-action-data)
(defn item-do-batch-AD []
  get-item-do-batch-edit-action-data)
(defn parallel-do-batch-AD []
  parallel-items-get-do-batch-edit-action-data)

(defn run-renderer
  "run the renderer on the output of the data getter, thus testing
  that they work together correctly."
  [renderer spec data-getter store]
  (apply renderer spec (map first (data-getter spec store))))

(deftest get-virtual-column-cell-action-data-test
  (let [[s1 header-id] (add-entity (new-element-store) nil
                                   (add-order-elements
                                    '(:x (anything ("c1" :label) :column)
                                         (anything ("c2" :label) :column))))
        [store row-id] (add-entity s1 nil (add-order-elements
                                           '(anything (1 ("c1" :label))
                                                      (2 ("c2" :label)))))
        data (get-virtual-column-cell-action-data
              {} {:target-ids [row-id]} nil store header-id)
        new-store (:store data)
        new-id (first (:target-ids data))]
    (is (= (semantic-to-list (description->entity new-id new-store))
           '("" (" A" :label))))))

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
        joe-id (:item-id joe)
        joe-joe (first (matching-elements "Joe" joe))
        joe-joe-id (:item-id joe-joe)
        joe-joseph (first (matching-elements "Joseph" joe))
        joe-joseph-id (:item-id joe-joseph)
        table (description->entity table-id store)
        header (first (entity/label->elements table :row-condition))
        header-id (:item-id header)
        rc1 (first (matching-elements `(nil ~o8) header))
        rc1-id (:item-id rc1)
        c1 (first (matching-elements `(nil ~o1) header))
        c1-id (:item-id c1)
        c2 (first (matching-elements `(nil ~o2) header))
        c2-id (:item-id c2)
        c2-name (first (matching-elements "name" c2))
        c2-name-id (:item-id c2-name)
        c3 (first (matching-elements `(nil ~o3) header))
        c3-id (:item-id c3)
        c3-name (first (matching-elements "name" c3))
        c3-name-id (:item-id c3-name)
        c4 (first (matching-elements `(nil ~o4) header))
        c4-id (:item-id c4)
        c5 (first (matching-elements `(nil ~o5) header))
        c5-id (:item-id c5)
        c6 (first (matching-elements `(nil ~o6) header))
        c6-id (:item-id c6)
        c7 (first (matching-elements `(nil ~o7) header))
        c7-id (:item-id c7)
        columns (ordered-entities
                 (label->elements header :column))
        hierarchy (replace-hierarchy-leaves-by-nodes
                   (hierarchy-by-labels columns))
        column-descriptions (concat
                             (mapcat #(table-hierarchy-node-column-descriptions
                                       header-id nil %)
                                     hierarchy)
                             [{:column-id :virtualColumn
                               :width 0.75
                               :header-id header-id}])
        joe-row-component (table-row-component
                           joe-id '("" :top-level ("age" :label))
                           {:column-descriptions-R column-descriptions})
        joe-row (run-renderer
                 render-table-row-DOM (second joe-row-component)
                 get-table-row-rendering-data store)]

    ;; Check get-table-condition-do-batch-edit-action-data
    (is (check (get-table-condition-do-batch-edit-action-data
                {:header-id header-id} {} nil store)
               {:batch-edit-ids [rc1-id]
                :stack-selector-index 1}))

    ;; Check get-table-header-do-batch-edit-action-data
    (is (check (get-table-header-do-batch-edit-action-data
                {:header-id header-id
                 :item-id c2-id
                 :descendant-ids [c1-id c2-id c3-id]
                 :competing-ids [c4-id]}
                {} nil store)
              {:batch-edit-ids [rc1-id c1-id c2-id c3-id]
               :stack-selector-index 1
               :selected-index 2}))
    (is (check (get-table-header-do-batch-edit-action-data
                {:header-id header-id
                 :item-id c2-id
                 :descendant-ids [c2-id]
                 :competing-ids [c4-id]}
                {} nil store)
              {:batch-edit-ids [rc1-id c4-id c2-id]
               :stack-selector-index 1
               :selected-index 2}))

    ;; Check get-table-cell-do-batch-edit-action-data
    (is (check (get-table-cell-do-batch-edit-action-data
                    {:header-id header-id
                     :competing-ids [c4-id]}
                    {} nil store)
               {:batch-edit-ids [rc1-id c4-id]
                :stack-selector-index 1}))

    ;; Check get-table-cell-item-do-batch-edit-action-data
    (is (check (get-table-cell-item-do-batch-edit-action-data
                {:item-id joe-joe-id}
                {:batch-edit-ids [rc1-id c4-id]
                 :stack-selector-index 1}
                nil store)
               {:batch-edit-ids [rc1-id c4-id joe-joe-id]
                :stack-selector-index 1
                :selected-index 2}))

    ;; Check the top level condition
    (is (check
         (run-renderer
          render-table-condition-DOM {:header-id header-id}
          get-table-condition-rendering-data store)
         [:div {:class "horizontal-labels-element query-condition"}
           ;; A virtual label for the condition
          [:component {:template '(anything :label)
                       :position :after
                       :relative-id :virtual-label
                       :width 0.75
                       :class "label"
                       :render-dom render-virtual-DOM
                       :get-rendering-data get-virtual-DOM-rendering-data
                       :get-action-data (virt-AD)}]
           ;; The condition element.
           [:div {:class "horizontal-stack"}
            [:div {:class "wrapped-element label"}
             [:component {:template '(anything :label)
                          :width 0.75
                          :parallel-ids [rc1-id]
                          :class "label"
                          :excluded-element-ids [(any)]
                          :relative-id (any)}]
             [:div {:class "indent-wrapper"}
              [:component {:template '(anything ("age" :label))
                           :width 0.75
                           :excluded-element-ids [(any)]
                           :relative-id rc1-id}]]]
            ;; A virtual element for more condition.
            [:div {:class "wrapped-element label virtual-column"}
             [:component {:relative-id :virtual-label
                          :item-id rc1-id
                          :width 0.75
                          :sibling true
                          :template ['anything '(anything :label)]
                          :get-action-data [composed-get-action-data
                                            (item-AD)
                                            (virt-AD)]
                          :class "label"
                          :render-dom render-virtual-DOM
                          :get-rendering-data get-virtual-DOM-rendering-data}]
             [:div  {:class "indent-wrapper"}
              [:component {:template 'anything
                           :sibling true
                           :width 0.75
                           :relative-id :virtual
                           :item-id rc1-id
                           :render-dom render-virtual-DOM
                           :get-rendering-data get-virtual-DOM-rendering-data
                           :get-action-data [composed-get-action-data
                                            (item-AD)
                                             (virt-AD)]}]]]]]))

    ;; Check the header
    (is (check
         (run-renderer
          render-table-header-DOM {:header-id header-id
                                   :hierarchy-R hierarchy}
          get-table-header-rendering-data store)
         [:div {:class "column-header-sequence table-header"}
          ;; A single column.
          [:component {:get-do-batch-edit-action-data (table-head-do-batch-AD)
                       :get-column-action-data (col-AD)  
                       :header-id header-id
                       :descendant-ids [c1-id]
                       :width 0.75
                       :template :singular
                       :relative-id c1-id
                       :class "column-header leaf"}]
          ;; Three columns.
          [:div {:class "column-header label"}
           ;; The label for the three columns
           [:component {:get-column-action-data (col-AD) 
                        :header-id header-id
                        :descendant-ids [c2-id c3-id c4-id]
                        :width 2.25
                        :template '(anything :label)
                        :parallel-ids [c2-id c3-id c4-id]
                        :get-do-batch-edit-action-data
                        [(comp-AD)
                         [(parallel-do-batch-AD)
                          (table-head-do-batch-AD)]
                         (item-do-batch-AD)]
                        :relative-id c2-name-id
                        :class "label with-children"
                        :excluded-element-ids [(any)]}]
           [:div {:class "column-header-sequence"}
            ;; A column with only a virtual label
            [:div {:class (str "label wrapped-element virtual-wrapper"
                               " merge-with-parent column-header leaf")}
             [:component
              {:get-column-action-data (col-AD) 
               :header-id header-id
               :descendant-ids [c2-id]
               :competing-ids [c3-id]
               :width 0.75
               :template '(anything :label)
               :parallel-ids [c2-id]
               :get-action-data [(comp-AD)
                                 [(parallel-AD) (item-AD)]
                                 (virt-AD)]
               :relative-id [c2-id :nested]
               :class "label merge-with-parent"
               :render-dom (virt-DOM)
               :get-rendering-data (virt-RD)}]
             [:div {:class "indent-wrapper label"}
              [:component
               {:get-do-batch-edit-action-data (table-head-do-batch-AD)
                :get-column-action-data (col-AD)
                :header-id header-id
                :descendant-ids [c2-id]
                :competing-ids [c3-id]
                :width 0.75
                :template :singular
                :relative-id c2-id
                :excluded-element-ids [c2-name-id]}]]]
            ;; A column with an additional label
            [:component
             {:get-do-batch-edit-action-data (table-head-do-batch-AD)
              :get-column-action-data (col-AD) 
              :header-id header-id
              :descendant-ids [c3-id]
              :width 0.75
              :template :singular
              :relative-id c3-id
              :excluded-element-ids [c3-name-id]
              :class "column-header leaf"}]
            ;; A column with only a virtual label
            [:div {:class (str "label wrapped-element virtual-wrapper"
                               " merge-with-parent column-header leaf")}
             [:component
              {:get-column-action-data (col-AD) 
               :header-id header-id
               :descendant-ids [c4-id]
               :competing-ids [c3-id]
               :width 0.75
               :template '(anything :label)
               :parallel-ids [c4-id]
               :get-action-data [(comp-AD)
                                 [(parallel-AD) (item-AD)]
                                 (virt-AD)]
               :relative-id [c4-id :nested]
               :class "label merge-with-parent"
               :render-dom (virt-DOM)
               :get-rendering-data (virt-RD)}]
             [:div {:class "indent-wrapper label"}
              [:component
               {:get-do-batch-edit-action-data (table-head-do-batch-AD)
                :get-column-action-data (col-AD)
                :header-id header-id
                :descendant-ids [c4-id]
                :competing-ids [c3-id]
                :width 0.75
                :template :singular
                :relative-id c4-id
                :excluded-element-ids [(any)]}]]]]]
          ;; One column with two labels
          [:component {:get-do-batch-edit-action-data (table-head-do-batch-AD)
                       :get-column-action-data (col-AD)
                       :header-id header-id
                       :descendant-ids [c5-id]
                       :width 0.75
                       :template :singular
                       :relative-id c5-id
                       :class "column-header leaf"}]
          ;; One column with no labels
          [:div {:class (str "label wrapped-element virtual-wrapper"
                             " column-header leaf")}
           [:component {:get-column-action-data (col-AD)
                        :header-id header-id
                        :descendant-ids [c6-id]
                        :width 0.75
                        :template '(anything :label)
                        :parallel-ids [c6-id]
                        :get-action-data [(comp-AD)
                                          [(parallel-AD) (item-AD)]
                                          (virt-AD)]
                        :class "label"
                        :relative-id [c6-id :nested]
                        :render-dom (virt-DOM)
                        :get-rendering-data (virt-RD)}]
           [:div {:class "indent-wrapper label"}
            [:component {:get-do-batch-edit-action-data (table-head-do-batch-AD)
                         :get-column-action-data (col-AD)
                         :header-id header-id
                         :descendant-ids [c6-id]
                         :width 0.75
                         :template :singular
                         :relative-id c6-id}]]]
          ;; One column with no labels and non-empty content.
          [:div {:class (str "label wrapped-element virtual-wrapper"
                             " column-header leaf")}
           [:component {:get-column-action-data (col-AD)
                        :header-id header-id
                        :descendant-ids [c7-id]
                        :width 0.75
                        :template '(anything :label)
                        :parallel-ids [c7-id]
                        :get-action-data [(comp-AD)
                                          [(parallel-AD) (item-AD)]
                                          (virt-AD)]
                        :class "label"
                        :relative-id [c7-id :nested]
                        :render-dom (virt-DOM)
                        :get-rendering-data (virt-RD)}]
           [:div {:class "indent-wrapper label"}
            [:component {:get-do-batch-edit-action-data (table-head-do-batch-AD)
                         :get-column-action-data (col-AD)
                         :header-id header-id
                         :descendant-ids [c7-id]
                         :width 0.75
                         :template :singular
                         :relative-id c7-id}]]]
          ;; The virtual column.
          [:div {:class "wrapped-element label column-header virtual-column"}
           [:component {:relative-id :virtual-label
                        :template ['(anything :column) '(anything :label)]
                        :sibling true
                        :width 0.75
                        :item-id c7-id
                        :get-action-data [(comp-AD) (item-AD) (virt-AD)]
                        :class "label"
                        :render-dom (virt-DOM)
                        :get-rendering-data (virt-RD)}]
           [:div {:class "indent-wrapper"}
            [:component {:relative-id :virtual-column
                         :template '(anything :column)
                         :width 0.75
                         :sibling true
                         :item-id c7-id
                         :get-action-data [(comp-AD) (item-AD) (virt-AD)]
                         :render-dom (virt-DOM)
                         :get-rendering-data (virt-RD)}]]]]))

    ;; Check the column descriptions
    (is (check
         column-descriptions
         [{:header-id header-id
           :column-id c1-id
           :width 0.75
           :query '(nil ("single" :label)
                        (:cosheet2.query/special-form
                         (:not :cosheet2.query/type)
                         (:label :cosheet2.query/sub-query))
                        (nil :order))}
          {:header-id header-id
           :column-id c2-id
           :competing-ids [c3-id]
           :disqualifications '((nil ("name" :label)
                                     ("other" :label)
                                     (:cosheet2.query/special-form
                                      (:not :cosheet2.query/type)
                                      (:label :cosheet2.query/sub-query))
                                     (nil :order)))
           :width 0.75
           :query '(nil ("name" :label)
                        (:cosheet2.query/special-form
                         (:not :cosheet2.query/type)
                         (:label :cosheet2.query/sub-query))
                        (nil :order))}
          {:header-id header-id
           :column-id c3-id
           :width 0.75
           :query '(nil ("name" :label)
                        ("other" :label)
                        (:cosheet2.query/special-form
                         (:not :cosheet2.query/type)
                         (:label :cosheet2.query/sub-query))
                        (nil :order))}
          (any) (any) (any) (any) (any)]))

   ;; Check making one row component
    (is (check
         joe-row-component
         [:component {:relative-id joe-id
                      :class "table-row"
                      :column-descriptions-R column-descriptions
                      :render-dom render-table-row-DOM
                      :get-rendering-data get-table-row-rendering-data
                      :get-action-data [(id-AD) joe-id]
                      :row-template '("" :top-level ("age" :label))
                      :get-row-action-data (row-AD)}]))

    ;; Check rendering the list of rows.
    (is (check
         (run-renderer render-table-rows-DOM
                       {:relative-id :body
                        :header-id header-id
                        :column-descriptions-R column-descriptions
                        :row-template-R 'foo
                        :row-ids-R [joe-id]}
                       get-table-rows-rendering-data store)
         [:div {:class "table-rows"}
          [:component {:relative-id joe-id
                       :class "table-row"
                       :header-id header-id
                       :column-descriptions-R column-descriptions
                       :render-dom render-table-row-DOM
                       :get-rendering-data get-table-row-rendering-data
                       :get-action-data [(id-AD) joe-id]
                       :row-template 'foo
                       :get-row-action-data (row-AD)}]
          [:component {:relative-id :virtual-row
                       :class "table-row"
                       :column-descriptions-R (any)
                       :render-dom render-table-virtual-row-DOM
                       :get-rendering-data get-table-virtual-row-rendering-data
                       :template 'foo
                       :item-id joe-id
                       :sibling true
                       :get-action-data [(comp-AD) (item-AD) (virt-AD) ]}]]))

    ;; Check rendering a row
    (is (check
         joe-row
         [:div {}
          [:component {:header-id header-id
                       :width 0.75
                       :class "table-cell"
                       :relative-id c1-id
                       :row-id joe-id
                       :query '(nil ("single" :label)
                                    (:cosheet2.query/special-form
                                     (:not :cosheet2.query/type)
                                     (:label :cosheet2.query/sub-query))
                                    (nil :order))
                       :render-dom (cell-DOM)
                       :get-rendering-data (cell-RD)
                       :get-action-data (pass-AD)
                       :get-do-batch-edit-action-data (table-cell-do-batch-AD)}]
          [:component {:header-id header-id
                       :width 0.75
                       :class "table-cell"
                       :relative-id c2-id
                       :row-id joe-id
                       :query '(nil ("name" :label)
                                    (:cosheet2.query/special-form
                                     (:not :cosheet2.query/type)
                                     (:label :cosheet2.query/sub-query))
                                    (nil :order))
                       :competing-ids [c3-id]
                       :disqualifications '((nil ("name" :label)
                                                 ("other" :label)
                                                 (:cosheet2.query/special-form
                                                  (:not :cosheet2.query/type)
                                                  (:label
                                                   :cosheet2.query/sub-query))
                                                 (nil :order)))
                       :render-dom (cell-DOM)
                       :get-rendering-data (cell-RD)
                       :get-action-data (pass-AD)
                       :get-do-batch-edit-action-data (table-cell-do-batch-AD)}]
          [:component {:header-id header-id
                       :width 0.75
                       :class "table-cell"
                       :relative-id c3-id
                       :row-id joe-id
                       :query '(nil ("name" :label)
                                    ("other" :label)
                                    (:cosheet2.query/special-form
                                     (:not :cosheet2.query/type)
                                     (:label :cosheet2.query/sub-query))
                                    (nil :order))
                       :render-dom (cell-DOM)
                       :get-rendering-data (cell-RD)
                       :get-action-data (pass-AD)
                       :get-do-batch-edit-action-data (table-cell-do-batch-AD)}]
          (any) (any) (any) (any)
          [:component {:width 0.75
                       :relative-id :virtual
                       :template ""
                       :render-dom (virt-DOM)
                       :get-rendering-data (virt-RD)
                       :get-action-data
                       [get-virtual-column-cell-action-data header-id]
                       :class "table-cell has-border virtual-column"}]]))

    ;; Check a rendering cells in a row
    (is (check
         (run-renderer
          render-table-cell-DOM (second (nth joe-row 2))
          get-table-cell-rendering-data store)
         [:component
          {:width 0.75
           :relative-id :virtual
           :template '("" ("single" :label))
           :render-dom (virt-DOM)
           :get-rendering-data (virt-RD)
           :get-action-data (virt-AD)}]))
    (is (check
         (run-renderer
          render-table-cell-DOM (second (nth joe-row 3))
          get-table-cell-rendering-data store)
         [:div
          {:class "vertical-stack"}
          [:div {:class (str "horizontal-labels-element virtual-wrapper"
                             " narrow")}
           [:component
            {:width 0.75
             :template '(anything :label)
             :relative-id [(any) :virtual-label]
             :parallel-ids [joe-joe-id]
             :get-action-data [(comp-AD)
                               [(parallel-AD) (item-AD)]
                               (virt-AD)]
             :render-dom (virt-DOM)
             :get-rendering-data (virt-RD)
             :class "label"}]
           [:component
            {:relative-id joe-joe-id
             :template '("" ("name" :label))
             :width 0.75
             :excluded-element-ids
             [(:item-id (first (matching-elements
                                "name" joe-joe)))]
             :get-do-batch-edit-action-data
             (table-cell-item-do-batch-AD)}]]
          [:div {:class "wrapped-element label"}
           [:component {:width 0.75
                        :template '(anything :label)
                        :parallel-ids [joe-joseph-id]
                        :get-do-batch-edit-action-data
                        [(comp-AD)
                         [(parallel-do-batch-AD)
                          (table-cell-item-do-batch-AD)]
                         (item-do-batch-AD)]
                        :class "label"
                        :excluded-element-ids [(any)]
                        :relative-id (any)}]
            [:div {:class "indent-wrapper"}
             [:component
              {:relative-id joe-joseph-id
               :template '("" ("name" :label) ("id" :label))
               :width 0.75
               :excluded-element-ids
               (as-set
                [(:item-id (first (matching-elements
                                   "name" joe-joseph)))
                 (:item-id (first (matching-elements
                                   "id" joe-joseph)))])
               :get-do-batch-edit-action-data
               (table-cell-item-do-batch-AD)}]]]]))

    ;; Check rendering the virtual row
    (is (check
         (render-table-virtual-row-DOM {:template'(:row :top-level)}
                                       column-descriptions)
         [:div {:class "table-row"}
          [:component {:relative-id c1-id
                       :class "table-cell"
                       :render-dom (virt-DOM)
                       :get-rendering-data (virt-RD)
                       :template '("" ("single" :label))
                       :get-action-data (virt-AD)
                       :width 0.75}]
          (any) (any) (any) (any) (any) (any)]))

    ;; Check rendering the overall table, given the header id.
    (is (check
          (run-renderer
           render-ready-table-DOM {:relative-id header-id}
           get-ready-table-rendering-data store)
          [:div {:class "table"}
           [:component {:relative-id :condition
                        :header-id header-id
                        :render-dom render-table-condition-DOM
                        :get-rendering-data get-table-condition-rendering-data
                        :get-action-data (pass-AD)
                        :get-do-batch-edit-action-data
                        get-table-condition-do-batch-edit-action-data}]
           [:div {:class "table-main"}
            [:component
             {:relative-id :header
              :header-id header-id
              :hierarchy-R
              [{:cosheet2.server.hierarchy/hierarchy-node true
                :leaves (any)
                :properties {["single" {:label 1}] 1}
                :cumulative-properties {["single" {:label 1}] 1}}
               (any) (any) (any) (any)]
              :render-dom render-table-header-DOM
              :get-rendering-data get-table-header-rendering-data
              :get-action-data (pass-AD)}]
            [:component
             {:relative-id :body
              :header-id header-id
              :column-descriptions-R (any)
              :row-template-R '(anything (anything ("age" :label)) :top-level)
              :row-ids-R [(any) (any)]
              :render-dom render-table-rows-DOM
              :get-rendering-data get-table-rows-rendering-data
              :get-action-data (pass-AD)}]]]))

    ;; Check getting the header id.
    (is (check
         (run-renderer
          render-table-DOM {:relative-id joe-id}
          get-item-rendering-data store)
         [:div {}]))
    (is (check
         (run-renderer
          render-table-DOM {:relative-id table-id}
          get-item-rendering-data store)
         [:component {:relative-id header-id
                      :render-dom render-ready-table-DOM
                      :get-rendering-data get-ready-table-rendering-data}]))))

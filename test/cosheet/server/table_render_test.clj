(ns cosheet.server.table-render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [orderable :as orderable]
             [store :refer [new-element-store]]
             store-impl
             [store-utils :refer [add-element add-object
                                  add-universal-objects
                                  add-link-type-object]]
             [query :refer [matching-items matching-elements not-query]]
             [entity :as entity  :refer [id->element id->object
                                         label->elements elements to-list
                                         make-object-list
                                         link-type name-label]]
             [calculator :refer [make-calculator-data computation-value]]
             [task-queue :refer [make-priority-task-queue]]
             [store :refer [->ItemId]]
             [debug :refer [simplify-for-print]]
             entity-impl
             [test-utils :refer [check any as-set]])
            (cosheet.server
             [item-render :refer [render-item-DOM-R
                                  render-virtual-DOM
                                  get-virtual-DOM-rendering-data]]
             [action-data :refer [default-get-action-data
                                  composed-get-action-data
                                  parallel-items-get-action-data
                                  get-pass-through-action-data
                                  get-id-action-data
                                  get-item-or-exemplar-action-data
                                  get-item-do-batch-edit-action-data
                                  parallel-items-get-do-batch-edit-action-data
                                  get-virtual-action-data]]
             [hierarchy :refer [hierarchy-by-labels
                                replace-hierarchy-leaves-by-nodes]
                        :as hierarchy]
             [render-utils :refer [make-virtual-label-template
                                   make-sequential-template
                                   ensure-label-object]]
             [order-utils :refer [ordered-entities add-order-elements]]
             [model-utils :refer [semantic-to-list semantic-elements
                                  table-row-condition-object]]
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

(def label-object-template (ensure-label-object 'anything :link-type))
(def label-template `(~label-object-template))
(def virtual-label-template (make-virtual-label-template 'anything :link-type))

;;; We make functions that abbreviate the common functions that can be
;;; embedded in components.
;;; (We use functions, rather than constants, so this file doesn't have
;;; to be reloaded if any of the files that define the underlying
;;; functions is reloaded.)

(defn virt-DOM [] render-virtual-DOM)
(defn cell-DOM [] render-table-cell-DOM-R)

(defn default-AD [] default-get-action-data)
(defn comp-AD [] composed-get-action-data)
(defn pass-AD [] get-pass-through-action-data)
(defn parallel-AD [] parallel-items-get-action-data)
(defn id-AD [] get-id-action-data)
(defn item-AD [] get-item-or-exemplar-action-data)
(defn virt-AD [] get-virtual-action-data)
(defn table-head-do-batch-AD [] get-table-header-do-batch-edit-action-data)
(defn table-cell-do-batch-AD [] get-table-cell-do-batch-edit-action-data)
(defn table-cell-item-do-batch-AD []
  get-table-cell-item-do-batch-edit-action-data)
(defn item-do-batch-AD []
  get-item-do-batch-edit-action-data)
(defn parallel-do-batch-AD []
  parallel-items-get-do-batch-edit-action-data)

(defn run-renderer
  "Run the renderer, then run the resulting dom-R, to get the final
  dom."
  [renderer spec mutable-store]
  (let [queue (make-priority-task-queue 0)
        cd (make-calculator-data queue)
        dom-R (renderer spec mutable-store)]
    (computation-value dom-R cd)))

(deftest get-virtual-column-cell-action-data-test
  (let [s (add-universal-objects (new-element-store))
        [s1 c1-label-oid] (add-link-type-object s "c1")
        [s2 c2-label-oid] (add-link-type-object s1 "c2")
        c1-object (id->object c1-label-oid s2)
        c2-object (id->object c2-label-oid s2)
        [s3 table-id] (add-element s2 nil
                                   (add-order-elements
                                    `(""
                                      (:x :row-condition
                                          ~'anything)
                                      (:x :column-headers
                                          (~'anything (~c1-object))
                                          (~'anything (~c2-object))))))
        [store row-id] (add-element s3 nil (add-order-elements
                                            `(~'anything
                                              (1 (~c1-object))
                                              (2 (~c2-object)))))
        data (get-virtual-column-cell-action-data
              {}
              {:table-id table-id :subject-ids [row-id]}
              nil store)
        new-store (:store data)
        new-id (first (:subject-ids data))]
    ;; TODO: !!! This shouldn't be a :label here, but an object list
    (is (= (semantic-to-list (id->element new-id new-store))
           '("" (" A" :label))))))

(deftest table-DOM-test
  (let [specification {:width 3.0
                       :elements-template 'anything}
        s (add-universal-objects (new-element-store))
        [sa age-oid] (add-link-type-object s "age")
        [sb nickname-oid] (add-link-type-object sa "nickname")
        [sc id-oid] (add-link-type-object sb "id")
        [sd single-oid] (add-link-type-object sc "single")
        [se height-oid] (add-link-type-object sd "height")
        [sf other-oid] (add-link-type-object se "other")
        temp-age-label-object (id->object age-oid sf)
        temp-name-label-object (id->object nickname-oid sf)
        temp-id-label-object (id->object id-oid sf)
        temp-single-label-object (id->object single-oid sf)
        temp-height-label-object (id->object height-oid sf)
        temp-other-label-object (id->object other-oid sf)
        joe-list (make-object-list
                  [`(~o2 :order)
                   `("male" (~o1 :order))
                   `("married" (~o2 :order))
                   `(39 (~o3 :order)
                        (~temp-age-label-object (~o3 :order))
                        ("doubtful" (~o1 :order)
                                    ("confidence" (~o3 :order))))
                   `(45 (~o4 :order)
                        (~temp-age-label-object (~o3 :order)))
                   `("Joe" (~o5 :order)
                     (~temp-name-label-object (~o3 :order)))
                   `("Joseph" (~o6 :order)
                     (~temp-name-label-object (~o1 :order))
                     (~temp-id-label-object (~o2 :order)))])
        jane-list (make-object-list
                   [`(~o1 :order)
                    `("plain" (~o2 :order))
                    `("plain" (~o3 :order))])
        test-list (make-object-list
                   [:test
                    `(~o3 :order)
                    ;; Real data won't have 'anything as content,
                    ;; but we want something that is less specific
                    ;; than the table condition to test that it will
                    ;; cause the condition to be eliminated in batch
                    ;; edits.
                    `(~'anything (~o3 :order) (~temp-age-label-object
                                               (~o3 :order)))])
        table-list `("table"
                     (~(make-object-list
                        [`(~'anything
                           (~temp-age-label-object (~o1 :order))
                           (~o8 :order))])
                      :row-condition)
                     (~'anything
                      :column-headers
                      (~'anything (~temp-single-label-object (~o1 :order))
                       (~o1 :order))
                      (~'anything
                       (~temp-name-label-object (~o1 :order))
                       (~o2 :order))
                      (~'anything
                       (~temp-name-label-object (~o1 :order))
                       (~temp-other-label-object (~o2 :order))
                       (~o3 :order))
                      (~'anything (~temp-name-label-object (~o1 :order))
                       (~o4 :order))
                      (~'anything
                       (~temp-age-label-object (~o1 :order))
                       (~temp-other-label-object (~o2 :order))
                       (~o5 :order))
                      (~'anything ("6-2" (~o1 :order)
                                   (~temp-height-label-object (~o2 :order)))
                       (~o6 :order))
                      ("something" ("child" (~o1 :order))
                       (~o7 :order))))
        [s1 joe-id] (add-object sf joe-list)
        [s2 jane-id] (add-object s1 jane-list)
        [s3 test-id] (add-object s2 test-list)
        [store table-id] (add-element s3 nil table-list)
        age-label-object (id->object age-oid store)
        name-label-object (id->object nickname-oid store)
        id-label-object (id->object id-oid store)
        single-label-object (id->object single-oid store)
        height-label-object (id->object height-oid store)
        other-label-object (id->object other-oid store)
        joe (id->object joe-id store)
        joe-id (:item-id joe)
        joe-joe (first (matching-elements "Joe" joe))
        joe-joe-id (:item-id joe-joe)
        joe-joseph (first (matching-elements "Joseph" joe))
        joe-joseph-id (:item-id joe-joseph)
        table (id->element table-id store)
        row-condition (table-row-condition-object table)
        row-condition-id (:item-id row-condition)
        rc1 (first (matching-elements `(nil ~o8) row-condition))
        rc1-id (:item-id rc1)
        column-headers (entity/label->element table :column-headers)
        column-headers-id (:item-id column-headers)
        c1 (first (matching-elements `(nil ~o1) column-headers))
        c1-id (:item-id c1)
        c2 (first (matching-elements `(nil ~o2) column-headers))
        c2-id (:item-id c2)
        c2-name (first (matching-elements `(~name-label-object) c2))
        c2-name-id (:item-id c2-name)
        c3 (first (matching-elements `(nil ~o3) column-headers))
        c3-id (:item-id c3)
        c3-name (first (matching-elements `(~name-label-object) c3))
        c3-name-id (:item-id c3-name)
        c4 (first (matching-elements `(nil ~o4) column-headers))
        c4-id (:item-id c4)
        c5 (first (matching-elements `(nil ~o5) column-headers))
        c5-id (:item-id c5)
        c6 (first (matching-elements `(nil ~o6) column-headers))
        c6-id (:item-id c6)
        c7 (first (matching-elements `(nil ~o7) column-headers))
        c7-id (:item-id c7)
        columns (ordered-entities (semantic-elements column-headers))
        hierarchy (replace-hierarchy-leaves-by-nodes
                   (hierarchy-by-labels columns))
        column-descriptions (concat
                             (mapcat #(table-hierarchy-node-column-descriptions
                                       nil %)
                                     hierarchy)
                             [{:column-id :virtualColumn
                               :width 0.75
                               :row-condition-id row-condition-id}])
        joe-row-component (table-row-component
                           joe-id
                           {:column-descriptions-R column-descriptions})
        joe-row (run-renderer
                 render-table-row-DOM-R (second joe-row-component) store)]

    ;; Check get-table-condition-do-batch-edit-action-data
    (is (check (get-table-condition-do-batch-edit-action-data
                {} {:table-id table-id} nil store)
               {:table-id table-id
                :query-ids [rc1-id]
                :stack-ids [rc1-id]
                :must-show-label true}))

    ;; Check get-table-header-do-batch-edit-action-data
    (is (check (get-table-header-do-batch-edit-action-data
                {:auxiliary-item-id c2-id
                 :column-ids [c1-id c2-id c3-id]
                 :competing-ids [c4-id]}
                {:table-id table-id} nil store)
               {:table-id table-id
                :query-ids [rc1-id]
                :stack-ids [c1-id c2-id c3-id]
                :selected-index 1}))
    (is (check (get-table-header-do-batch-edit-action-data
                {:auxiliary-item-id c2-id
                 :column-ids [c2-id]
                 :competing-ids [c4-id]}
                {:table-id table-id} nil store)
               {:table-id table-id
                :query-ids [rc1-id]
                :stack-ids [c4-id c2-id]
                :selected-index 1}))

    ;; Check get-table-cell-do-batch-edit-action-data
    (is (check (get-table-cell-do-batch-edit-action-data
                    {:competing-ids [c4-id]}
                    {:table-id table-id} nil store)
               {:table-id table-id
                :query-ids [rc1-id]
                :stack-ids [c4-id]}))

    ;; Check get-table-cell-item-do-batch-edit-action-data
    (is (check (get-table-cell-item-do-batch-edit-action-data
                {:item-id joe-joe-id}
                {:query-ids [rc1-id]
                 :stack-ids [c4-id]}
                nil store)
               {:query-ids [rc1-id]
                :stack-ids [c4-id joe-joe-id]
                :selected-index 1}))

    ;; Check the top level condition
    (is (check
         (run-renderer
          render-table-condition-DOM-R {:relative-id row-condition-id} store)
         [:div {:class "horizontal-labels-element query-condition"}
          ;; A virtual link-type for the condition.
          [:component {:template virtual-label-template
                       :is-object-name true
                       :position :after
                       :relative-id :virtual-label
                       :width 0.75
                       :class "link-type"
                       :omit-universal-elements true
                       :render-dom render-virtual-DOM
                       :get-action-data (virt-AD)}]
           ;; The condition element.
           [:div {:class "horizontal-stack"}
            [:div {:class "wrapped-element link-type"}
             [:component {:template label-template
                          :width 0.75
                          :parallel-ids [rc1-id]
                          :class "link-type"
                          :omit-universal-elements true
                          :relative-id (any)
                          :render-dom render-item-DOM-R
                          :get-action-data (default-AD)}]
             [:div {:class "indent-wrapper left-indent"}
              [:component {:template `(~'anything (~age-label-object))
                           :width 0.75
                           :exclude-elements-by-ids [(any)]
                           :relative-id rc1-id
                           :render-dom render-item-DOM-R
                           :get-action-data (default-AD)}]]]
            ;; A virtual element for more condition.
            [:div {:class "wrapped-element link-type virtual-column"}
             [:component {:relative-id :virtual-label
                          :auxiliary-item-id rc1-id
                          :width 0.75
                          :sibling true
                          :template (make-sequential-template
                                     ['anything
                                      '("")
                                      (make-object-list [`(~link-type)
                                                         `("" (~name-label))])])
                          :is-object-name true
                          :position :after
                          :get-action-data [composed-get-action-data
                                            (item-AD)
                                            (virt-AD)]
                          :class "link-type"
                          :render-dom render-virtual-DOM}]
             [:div  {:class "indent-wrapper left-indent"}
              [:component {:template 'anything
                           :sibling true
                           :width 0.75
                           :relative-id :virtual
                           :auxiliary-item-id rc1-id
                           :render-dom render-virtual-DOM
                           :get-action-data [composed-get-action-data
                                            (item-AD)
                                             (virt-AD)]}]]]]]))

    ;; Check the header
    (is (check
         (run-renderer
          render-table-header-DOM-R {:hierarchy-R hierarchy} store)
         [:div {:class "column-header-sequence table-header"}
          ;; A single column.
          [:component {:get-do-batch-edit-action-data (table-head-do-batch-AD)  
                       :column-ids [c1-id]
                       :width 0.75
                       :template :singular
                       :render-dom render-item-DOM-R
                       :get-action-data (default-AD)
                       :relative-id c1-id
                       :class "column-header leaf"}]
          ;; Three columns.
          [:div {:class "column-header link-type"}
           ;; The link-type for the three columns
           [:component {:column-ids [c2-id c3-id c4-id]
                        :width 2.25
                        :template label-template
                        :parallel-ids [c2-id c3-id c4-id]
                        :get-do-batch-edit-action-data
                        [(comp-AD)
                         [(parallel-do-batch-AD)
                          (table-head-do-batch-AD)]
                         (item-do-batch-AD)]
                        :relative-id c2-name-id
                        :render-dom render-item-DOM-R
                        :get-action-data (default-AD)
                        :class "link-type with-children"}]
           [:div {:class "column-header-sequence"}
            ;; A column with only a virtual label
            [:div {:class (str "link-type wrapped-element virtual-wrapper"
                               " merge-with-parent column-header leaf")}
             [:component
              {:column-ids [c2-id]
               :competing-ids [c3-id]
               :width 0.75
               :template virtual-label-template
               :is-object-name true
               :position :after
               :parallel-ids [c2-id]
               :get-action-data [(comp-AD)
                                 [(parallel-AD) (item-AD)]
                                 (virt-AD)]
               :relative-id [c2-id :nested]
               :class "link-type merge-with-parent"
               :render-dom (virt-DOM)}]
             [:div {:class "indent-wrapper link-type"}
              [:component
               {:get-do-batch-edit-action-data (table-head-do-batch-AD)
                :column-ids [c2-id]
                :competing-ids [c3-id]
                :width 0.75
                :template :singular
                :relative-id c2-id
                :render-dom render-item-DOM-R
                :get-action-data (default-AD)
                :exclude-elements-by-ids [c2-name-id]}]]]
            ;; A column with an additional label
            [:component
             {:get-do-batch-edit-action-data (table-head-do-batch-AD)
              :column-ids [c3-id]
              :width 0.75
              :template :singular
              :relative-id c3-id
              :render-dom render-item-DOM-R
              :get-action-data (default-AD)
              :exclude-elements-by-ids [c3-name-id]
              :class "column-header leaf"}]
            ;; A column with only a virtual label
            [:div {:class (str "link-type wrapped-element virtual-wrapper"
                               " merge-with-parent column-header leaf")}
             [:component
              {:column-ids [c4-id]
               :competing-ids [c3-id]
               :width 0.75
               :template virtual-label-template
               :is-object-name true
               :position :after
               :parallel-ids [c4-id]
               :get-action-data [(comp-AD)
                                 [(parallel-AD) (item-AD)]
                                 (virt-AD)]
               :relative-id [c4-id :nested]
               :class "link-type merge-with-parent"
               :render-dom (virt-DOM)}]
             [:div {:class "indent-wrapper link-type"}
              [:component
               {:get-do-batch-edit-action-data (table-head-do-batch-AD)
                :column-ids [c4-id]
                :competing-ids [c3-id]
                :width 0.75
                :template :singular
                :render-dom render-item-DOM-R
                :get-action-data (default-AD)
                :relative-id c4-id
                :exclude-elements-by-ids [(any)]}]]]]]
          ;; One column with two labels
          [:component {:get-do-batch-edit-action-data (table-head-do-batch-AD)
                       :column-ids [c5-id]
                       :width 0.75
                       :template :singular
                       :render-dom render-item-DOM-R
                       :get-action-data (default-AD)
                       :relative-id c5-id
                       :class "column-header leaf"}]
          ;; One column with no labels
          [:div {:class (str "link-type wrapped-element virtual-wrapper"
                             " column-header leaf")}
           [:component {:column-ids [c6-id]
                        :width 0.75
                        :template virtual-label-template
                        :is-object-name true
                        :position :after
                        :parallel-ids [c6-id]
                        :get-action-data [(comp-AD)
                                          [(parallel-AD) (item-AD)]
                                          (virt-AD)]
                        :class "link-type"
                        :relative-id [c6-id :nested]
                        :render-dom (virt-DOM)}]
           [:div {:class "indent-wrapper link-type"}
            [:component {:get-do-batch-edit-action-data (table-head-do-batch-AD)
                         :column-ids [c6-id]
                         :width 0.75
                         :template :singular
                         :relative-id c6-id
                         :render-dom render-item-DOM-R
                         :get-action-data (default-AD)}]]]
          ;; One column with no labels and non-empty content.
          [:div {:class (str "link-type wrapped-element virtual-wrapper"
                             " column-header leaf")}
           [:component {:column-ids [c7-id]
                        :width 0.75
                        :template virtual-label-template
                        :is-object-name true
                        :position :after
                        :parallel-ids [c7-id]
                        :get-action-data [(comp-AD)
                                          [(parallel-AD) (item-AD)]
                                          (virt-AD)]
                        :class "link-type"
                        :relative-id [c7-id :nested]
                        :render-dom (virt-DOM)}]
           [:div {:class "indent-wrapper link-type"}
            [:component {:get-do-batch-edit-action-data (table-head-do-batch-AD)
                         :column-ids [c7-id]
                         :width 0.75
                         :template :singular
                         :relative-id c7-id
                         :render-dom render-item-DOM-R
                         :get-action-data (default-AD)}]]]
          ;; The virtual column.
          [:div {:class (str "wrapped-element link-type"
                             " column-header virtual-column")}
           [:component {:relative-id :virtual-label
                        :template (make-sequential-template
                                   ['anything
                                    '("")
                                    (make-object-list [`(~link-type)
                                                       `("" (~name-label))])])
                        :is-object-name true
                        :position :after
                        :sibling true
                        :width 0.75
                        :auxiliary-item-id c7-id
                        :get-action-data [(comp-AD) (item-AD) (virt-AD)]
                        :class "link-type"
                        :render-dom (virt-DOM)}]
           [:div {:class "indent-wrapper left-indent"}
            [:component {:relative-id :virtual-column
                         :template 'anything
                         :width 0.75
                         :sibling true
                         :auxiliary-item-id c7-id
                         :get-action-data [(comp-AD) (item-AD) (virt-AD)]
                         :render-dom (virt-DOM)}]]]]))

    ;; Check the column descriptions
    (is (check
         column-descriptions
         [{:column-id c1-id
           :width 0.75
           :query `(nil (~single-label-object)
                        (nil :order))}
          {:column-id c2-id
           :competing-ids [c3-id]
           :disqualifications `(~(as-set
                                  `(nil (~name-label-object)
                                        (~other-label-object)
                                        (nil :order))))
           :width 0.75
           :query (as-set `(nil (~name-label-object)
                                (nil :order)))}
          {:column-id c3-id
           :width 0.75
           :query (as-set `(nil (~name-label-object)
                                (~other-label-object)
                                (nil :order)))}
          (any) (any) (any) (any) (any)]))

   ;; Check making one row component
    (is (check
         joe-row-component
         [:component {:relative-id joe-id
                      :row-id joe-id
                      :class "table-row"
                      :column-descriptions-R column-descriptions
                      :render-dom render-table-row-DOM-R
                      :get-action-data [(id-AD) joe-id]}]))

    ;; Check rendering the list of rows.
    (is (check
         (run-renderer render-table-rows-DOM-R
                       {:relative-id :body
                        :column-descriptions-R column-descriptions
                        :row-template-R 'foo
                        :row-ids-R [joe-id]}
                       store)
         [:div {:class "table-rows"}
          [:component {:relative-id joe-id
                       :class "table-row"
                       :row-id joe-id
                       :column-descriptions-R column-descriptions
                       :render-dom render-table-row-DOM-R
                       :get-action-data [(id-AD) joe-id]}]
          [:component {:relative-id :virtual-row
                       :class "table-row"
                       :column-descriptions-R (any)
                       :render-dom render-table-virtual-row-DOM-R
                       :template 'foo
                       :sibling true
                       :get-action-data [(comp-AD)
                                         [(id-AD) joe-id]
                                         (virt-AD)]}]]))

    ;; Check rendering a row
    (is (check
         joe-row
         [:div {}
          [:component {:column-ids [c1-id]
                       :width 0.75
                       :class "table-cell"
                       :relative-id c1-id
                       :row-id joe-id
                       :query `(nil (~single-label-object)
                                    (nil :order))
                       :render-dom (cell-DOM)
                       :get-action-data (pass-AD)
                       :get-do-batch-edit-action-data (table-cell-do-batch-AD)}]
          [:component {:column-ids [c2-id]
                       :width 0.75
                       :class "table-cell"
                       :relative-id c2-id
                       :row-id joe-id
                       :query (as-set `(nil (~name-label-object)
                                            (nil :order)))
                       :competing-ids [c3-id]
                       :disqualifications `(~(as-set
                                              `(nil (~name-label-object)
                                                    (~other-label-object)
                                                    (nil :order))))
                       :render-dom (cell-DOM)
                       :get-action-data (pass-AD)
                       :get-do-batch-edit-action-data (table-cell-do-batch-AD)}]
          [:component {:column-ids [c3-id]
                       :width 0.75
                       :class "table-cell"
                       :relative-id c3-id
                       :row-id joe-id
                       :query (as-set `(nil (~name-label-object)
                                            (~other-label-object)
                                            (nil :order)))
                       :render-dom (cell-DOM)
                       :get-action-data (pass-AD)
                       :get-do-batch-edit-action-data (table-cell-do-batch-AD)}]
          (any) (any) (any) (any)
          [:component {:width 0.75
                       :relative-id :virtual
                       :row-id joe-id
                       :template ""
                       :render-dom (virt-DOM)
                       :get-action-data
                       get-virtual-column-cell-action-data
                       :class "table-cell has-border virtual-column"}]]))

    ;; Check a rendering cells in a row
    (is (check
         (run-renderer
          render-table-cell-DOM-R (second (nth joe-row 2)) store)
         [:component
          {:width 0.75
           :relative-id :virtual
           :template `("" (~single-label-object))
           :render-dom (virt-DOM)
           :get-action-data (virt-AD)}]))
    (is (check
         (run-renderer
          render-table-cell-DOM-R (second (nth joe-row 3)) store)
         [:div {:class "vertical-stack"}
          [:div {:class (str "horizontal-labels-element virtual-wrapper"
                             " narrow")}
           [:component
            {:width 0.75
             :template virtual-label-template
             :is-object-name true
             :relative-id [(any) :virtual-label]
             :parallel-ids [joe-joe-id]
             :get-action-data [(comp-AD)
                               [(parallel-AD) (item-AD)]
                               (virt-AD)]
             :render-dom (virt-DOM)
             :class "link-type"
             :position :after
             :omit-universal-elements true}]
           [:component
            {:relative-id joe-joe-id
             :render-dom render-item-DOM-R
             :get-action-data (default-AD)
             :template `("" (~name-label-object))
             :width 0.75
             :exclude-elements-by-ids
             [(:item-id (first (matching-elements
                                `(~name-label-object) joe-joe)))]
             :get-do-batch-edit-action-data
             (table-cell-item-do-batch-AD)}]]
          [:div {:class "wrapped-element link-type"}
           [:component {:width 0.75
                        :template label-template
                        :parallel-ids [joe-joseph-id]
                        :render-dom render-item-DOM-R
                        :get-action-data (default-AD)
                        :get-do-batch-edit-action-data
                        [(comp-AD)
                         [(parallel-do-batch-AD)
                          (table-cell-item-do-batch-AD)]
                         (item-do-batch-AD)]
                        :class "link-type"
                        :omit-universal-elements true
                        :relative-id (any)}]
            [:div {:class "indent-wrapper left-indent"}
             [:component
              {:relative-id joe-joseph-id
               :render-dom render-item-DOM-R
               :get-action-data (default-AD)
               :template `("" (~name-label-object) (~id-label-object))
               :width 0.75
               :exclude-elements-by-ids
               (as-set
                [(:item-id (first (matching-elements
                                   `(~name-label-object) joe-joseph)))
                 (:item-id (first (matching-elements
                                   `(~id-label-object) joe-joseph)))])
               :get-do-batch-edit-action-data
               (table-cell-item-do-batch-AD)}]]]]))

    ;; Check rendering the virtual row
    (is (check
         (run-renderer
          render-table-virtual-row-DOM-R
          {:column-descriptions-R column-descriptions}
          store)
         [:div {:class "table-row"}
          [:component {:relative-id c1-id
                       :column-ids [c1-id]
                       :class "table-cell"
                       :render-dom (virt-DOM)
                       :template `("" (~single-label-object))
                       :get-action-data (virt-AD)
                       :width 0.75}]
          (any) (any) (any) (any) (any) (any)]))

    ;; Check rendering the overall table, given the necessary ids.
    (is (check
          (run-renderer
           render-table-DOM-R
           {:relative-id table-id :table-id table-id}
           store)
          [:div {:class "table"}
           [:component {:relative-id row-condition-id
                        :render-dom render-table-condition-DOM-R
                        :get-action-data (default-AD)
                        :get-do-batch-edit-action-data
                        get-table-condition-do-batch-edit-action-data}]
           [:div {:class "table-main"}
            [:component
             {:relative-id column-headers-id
              :hierarchy-R
              [{:cosheet.server.hierarchy/hierarchy-node true
                :leaves (any)
                :properties {[:source single-label-object {}] 1}
                :cumulative-properties {[:source single-label-object {}] 1}}
               (any) (any) (any) (any)]
              :render-dom render-table-header-DOM-R
              :get-action-data (default-AD)}]
            [:component
             {:relative-id :body
              :alternate-row-sibling column-headers-id
              :column-descriptions-R (any)
              :row-template-R (make-object-list
                               [`(~'anything (~age-label-object))])
              :row-ids-R [(any) (any)]
              :render-dom render-table-rows-DOM-R
              :get-action-data (pass-AD)}]]]))

    ;; Check getting the subsidiary ids.
    (is (check
         (run-renderer
          render-table-DOM-R
          {:relative-id joe-id :table-id joe-id}
          store)
         [:div {}]))))

(ns cosheet.server.tabs-render-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet
             [orderable :refer [split initial]]
             [entity :refer [label->elements make-tree-object]]
             [store :refer [new-element-store]]
             store-impl
             [store-utils :refer [add-element link-type-object
                                  object-type-object]]
             [entity :refer [id->element]]
             [query :refer [matching-elements]]
             [debug :refer [simplify-for-print]]
             entity-impl
             [test-utils :refer [check any as-set]])
            (cosheet.server
             [item-render :refer [render-item-DOM-R
                                  render-virtual-DOM
                                  get-virtual-DOM-rendering-data]]
             [render-utils :refer [make-sequential-template]]
             [action-data :refer [default-get-action-data
                                  get-item-or-exemplar-action-data
                                  get-item-do-batch-edit-action-data
                                  get-virtual-action-data
                                  composed-get-action-data
                                  parallel-items-get-action-data
                                  parallel-items-get-do-batch-edit-action-data]]
             [tabs-render :refer :all])
             ; :reload
            ))

(defn virt-DOM [] render-virtual-DOM)
(defn tab-DOM [] render-tab-elements-DOM)

(defn virt-RD [] get-virtual-DOM-rendering-data)
(defn tab-RD [] get-tab-elements-rendering-data)

(defn item-AD [] get-item-or-exemplar-action-data)
(defn default-AD [] default-get-action-data)
(defn parallel-AD [] parallel-items-get-action-data)
(defn item-do-batch-AD [] get-item-do-batch-edit-action-data)
(defn parallel-do-batch-AD [] parallel-items-get-do-batch-edit-action-data)
(defn virt-AD [] get-virtual-action-data)
(defn comp-AD [] composed-get-action-data)

(def orderables (reduce (fn [os _]
                          (vec (concat (pop os)
                                       (split (peek os) :after))))
                        [initial]
                        (range 3)))
(def o1 (nth orderables 0))
(def o2 (nth orderables 1))
(def o3 (nth orderables 2))
(def unused-orderable (nth orderables 3))

(deftest tabs-DOM-test
  (let [specification {:priority 1
                       :width 3.0}
        ;; TODO: !!! Figure out why the :blank keywords below can be "".
        tabs-list `(""
                    ("" "foo"
                     :tab
                     (:blank :table)
                     (~o1 :order))
                    ("" "foo" "bar"
                     :tab 
                     (:blank :table)
                     (~o2 :order))
                    ("" "baz" "bletch"
                     :tab
                     (:blank :table)
                     (~o3 :order)))
        [store tabs-id] (add-element (new-element-store) nil tabs-list)
        tabs (id->element tabs-id store)
        t1 (first (matching-elements `(nil ~o1) tabs))
        t2 (first (matching-elements `(nil ~o2) tabs))
        t3 (first (matching-elements `(nil ~o3) tabs))
        t1-foo  (first (matching-elements "foo" t1))
        t2-foo (first (matching-elements "foo" t2))
        t2-bar (first (matching-elements "bar" t2))
        t3-baz (first (matching-elements "baz" t3))
        t3-bletch (first (matching-elements "bletch" t3))
        spec {:relative-id tabs-id
              :chosen-tab-id (:item-id t1)}
        dom (render-tabs-DOM spec store)
        tabs-dom (nth dom 4)
        virt-tab-dom (nth tabs-dom 3)
        tab3-dom (nth tabs-dom 4)
        tab12-dom (nth tabs-dom 5)
        tab2-dom (nth (nth tab12-dom 3) 2)
        tab1-dom (nth (nth tab12-dom 3) 3)]  
    (is (check
         dom
         [:div {:class "tabs-wrapper"}
          [:div#batch-edit.tool
           [:img {:src "../icons/edit.gif"}]
           [:div.tooltip "batch edit (C-B)"]]
          [:div.toolgap]
          [:div {:class "tabs-holder"}
           [:div]
           [:component
            {:relative-id :virtual-tab
             :auxiliary-item-id (:item-id t3)
             :class "tab virtualTab"
             :template (make-sequential-template
                        `(""
                          :tab
                          ("" :tab-topic :table
                           (~(make-tree-object [`(~(object-type-object '???))
                                                :selector])
                            :row-condition)
                           (~'anything
                            :column-headers :selector
                            (~'anything (~(link-type-object '???))))))
                        'anything)
             :sibling true
             :use-bigger true
             :render-dom (virt-DOM)
             :get-action-data [(comp-AD) (item-AD) (virt-AD)]}]
           [:component
            {:relative-id (:item-id t3)
             :width 0.75
             :template `("" :tab ""
                         ("" :tab-topic :table
                                 (~(make-tree-object [`(~(object-type-object '???)) :selector])
                                  :row-condition)
                                 (~'anything
                                  :column-headers :selector
                                  (~'anything (~(link-type-object '???))))))
             :render-dom (tab-DOM)
             :get-action-data (item-AD)
             :example-element-ids (as-set [(:item-id t3-baz)
                                           (:item-id t3-bletch)])
             :tab-id (:item-id t3)
             :class "tab"}]
           [:div {:class "tab-tree"}
            [:component
             {:relative-id (:item-id t1)
              :width 1.5
              :template `("" :tab ""
                          ("" :tab-topic :table
                                  (~(make-tree-object [`(~(object-type-object '???)) :selector])
                                   :row-condition)
                                  (~'anything
                                   :column-headers :selector
                                   (~'anything (~(link-type-object '???))))))
              :render-dom (tab-DOM)
              :example-element-ids [(:item-id t1-foo)]
              :parallel-ids [(:item-id t1) (:item-id t2)]
              :get-action-data [(parallel-AD) (item-AD)]
              :get-do-batch-edit-action-data [(parallel-do-batch-AD)
                                              (item-do-batch-AD)]
              :class "multi-tab"}]
            [:div {:class "tab-sequence"}
             [:component
              {:relative-id [(:item-id t2) :D1]
               :width 0.75
               :template `("" :tab ""
                           ("" :tab-topic :table
                                   (~(make-tree-object [`(~(object-type-object '???)) :selector])
                                    :row-condition)
                                   (~'anything
                                    :column-headers :selector
                                    (~'anything (~(link-type-object '???)))))
                           "foo")
               :render-dom (tab-DOM)
               :example-element-ids [(:item-id t2-bar)]
               :parallel-ids [(:item-id t2)]
               :get-action-data [(parallel-AD) (item-AD)]
               :get-do-batch-edit-action-data [(parallel-do-batch-AD)
                                               (item-do-batch-AD)]
               :tab-id (:item-id t2)
               :class "tab"}]
             [:component
              {:relative-id [(:item-id t1) :D1]
               :width 0.75
               :template (any)
               :render-dom (tab-DOM)
               :example-element-ids []
               :parallel-ids [(:item-id t1)]
               :get-action-data [(parallel-AD) (item-AD)]
               :get-do-batch-edit-action-data [(parallel-do-batch-AD)
                                               (item-do-batch-AD)]
               :tab-id (:item-id t1)
               :class "chosen tab"}]]]]]))
    (is (check
         (render-tab-elements-DOM (second tab3-dom) store)
         (as-set [:div {:class "vertical-stack"}
                  [:component {:relative-id (:item-id t3-baz)
                               :render-dom render-item-DOM-R
                               :get-action-data (default-AD)
                               :width 0.75
                               :template (any)}]
                  [:component {:relative-id (:item-id t3-bletch)
                               :render-dom render-item-DOM-R
                               :get-action-data (default-AD)
                               :width 0.75
                               :template (any)}]])))
    (is (check
         (render-virtual-DOM (second virt-tab-dom) store)
         [:div {:class "tab virtualTab editable virtual"}]))
    (is (check
         (render-tab-elements-DOM (second tab1-dom) store)
         [:component {:relative-id :virtual
                      :class "empty-child"
                      :render-dom (virt-DOM)
                      :template 'anything
                      :get-action-data (virt-AD)}]))))

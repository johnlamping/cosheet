(ns cosheet2.server.tabs-render-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet2
             [orderable :refer [split initial]]
             [entity :refer [label->elements]]
             [store :refer [new-element-store]]
             store-impl
             [store-utils :refer [add-entity]]
             [entity :refer [description->entity]]
             [query :refer [matching-elements]]
             [debug :refer [simplify-for-print]]
             entity-impl
             [test-utils :refer [check any as-set]])
            (cosheet2.server
             [item-render :refer [render-virtual-DOM
                                  get-virtual-DOM-rendering-data]]
             [model-utils :refer [new-tab-elements semantic-to-list]]
             [action-data :refer [get-item-or-exemplar-action-data
                                  get-tab-action-data get-virtual-action-data
                                  composed-get-action-data
                                  multiple-items-get-action-data]]
             [tabs-render :refer :all])
             ; :reload
            ))

(defn virt-DOM [] render-virtual-DOM)
(defn tab-DOM [] render-tab-elements-DOM)

(defn virt-RD [] get-virtual-DOM-rendering-data)
(defn tab-RD [] get-tab-elements-rendering-data)

(defn item-AD [] get-item-or-exemplar-action-data)
(defn mult-items-AD [] multiple-items-get-action-data)
(defn tab-AD [] get-tab-action-data)
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
        [store tabs-id] (add-entity (new-element-store) nil tabs-list)
        tabs (description->entity tabs-id store)
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
             :item-id (:item-id t3)
             :class "tab virtualTab"
             :template ['("" :tab
                          (:blank :tab-topic :table
                                  (anything (??? :label)
                                            (anything :column (??? :label))
                                            :row-condition :selector)))
                        'anything]
             :sibling true
             :use-bigger true
             :render-dom (virt-DOM)
             :get-rendering-data (virt-RD)
             :get-action-data
             [(comp-AD)
              (item-AD)
              (virt-AD)]}]
           [:component
            {:relative-id (:item-id t3)
             :width 0.75
             :template '("" "" :tab
                         (:blank :tab-topic :table
                                 (anything (??? :label)
                                           (anything :column
                                                     (??? :label))
                                           :row-condition :selector)))
             :render-dom (tab-DOM)
             :get-rendering-data (tab-RD)
             :example-element-ids [(:item-id t3-baz) (:item-id t3-bletch)]
             :get-tab-action-data [(tab-AD) (:item-id t3)]
             :class "tab"}]
           [:div {:class "tab-tree"}
            [:component
             {:relative-id (:item-id t1)
              :width 1.5
              :template '("" "" :tab
                          (:blank :tab-topic :table
                                  (anything (??? :label)
                                            (anything :column
                                                      (??? :label))
                                            :row-condition :selector)))
              :render-dom (tab-DOM)
              :get-rendering-data (tab-RD)
              :example-element-ids [(:item-id t1-foo)]
              :item-ids [(:item-id t1) (:item-id t2)]
              :get-action-data [(mult-items-AD) (item-AD)]
              :class "multi-tab"}]
            [:div {:class "tab-sequence"}
             [:component
              {:relative-id [(:item-id t2) :D1]
               :width 0.75
               :template '("" "" :tab
                           (:blank :tab-topic :table
                                   (anything (??? :label)
                                             (anything :column
                                                       (??? :label))
                                             :row-condition :selector))
                           "foo")
               :render-dom (tab-DOM)
               :get-rendering-data (tab-RD)
               :example-element-ids [(:item-id t2-bar)]
               :item-ids [(:item-id t2)]
               :get-action-data [(mult-items-AD) (item-AD)]
               :get-tab-action-data [(tab-AD) (:item-id t2)]
               :class "tab"}]
             [:component
              {:relative-id [(:item-id t1) :D1]
               :width 0.75
               :template (any)
               :render-dom (tab-DOM)
               :get-rendering-data (tab-RD)
               :example-element-ids []
               :item-ids [(:item-id t1)]
               :get-action-data [(mult-items-AD) (item-AD)]
               :get-tab-action-data [(tab-AD) (:item-id t1)]
               :class "chosen tab"}]]]]]))
    (is (check
         (render-tab-elements-DOM (second tab3-dom) store)
         [:div {:class "vertical-stack"}
          [:component {:relative-id (:item-id t3-baz)
                       :width 0.75}]
          [:component {:relative-id (:item-id t3-bletch)
                       :width 0.75}]]))
    (is (check
         (render-virtual-DOM (second virt-tab-dom))
         [:div {:class "tab virtualTab editable virtual"}]))
    (is (check
         (render-tab-elements-DOM (second tab1-dom) store)
         [:component {:relative-id :virtual
                      :class "empty-child"
                      :render-dom (virt-DOM)
                      :get-rendering-data (virt-RD)
                      :template 'anything
                      :get-action-data (virt-AD)}]))))

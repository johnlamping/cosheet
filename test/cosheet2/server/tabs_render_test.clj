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
             [action-data :refer [get-item-or-exemplar-action-data-for-ids
                                  get-tab-action-data get-virtual-action-data]]
             [tabs-render :refer :all])
             ; :reload
            ))

(defn virt-DOM [] render-virtual-DOM)
(defn tab-DOM [] render-tab-elements-DOM)

(defn virt-RD [] get-virtual-DOM-rendering-data)
(defn tab-RD [] get-tab-elements-rendering-data)

(defn ids-AD [] get-item-or-exemplar-action-data-for-ids)
(defn tab-AD [] get-tab-action-data)
(defn virt-AD [] get-virtual-action-data)



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
        client-state {:root-id (:item-id t1)}
        spec {:relative-id tabs-id
              :client-state client-state}
        dom (render-tabs-DOM spec store client-state)
        tabs-dom (nth dom 4)
        virt-tab-dom (nth tabs-dom 3)
        tab1-dom (nth tabs-dom 4)
        tab23-dom (nth tabs-dom 5)]  
                  
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
             :class "tab virtualTab"
             :render-dom (virt-DOM)
             :get-rendering-data (virt-RD)
             :get-action-data
             [(virt-AD)
              {:template '("" "" :tab
                           (:blank :tab-topic :table
                                   (anything (??? :label)
                                             (anything :column (??? :label))
                                             :row-condition :selector)))
               :sibling (:item-id t1)
               :position :before
               :use-bigger true}]}]
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
              :get-action-data [(ids-AD) [(:item-id t1) (:item-id t2)]]
              :class "multi-tab"}]
            [:div {:class "tab-sequence"}
             [:component
              {:relative-id [(:item-id t2) "1"]
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
               :get-action-data [(ids-AD) [(:item-id t2)]]
               :get-tab-action-data [(tab-AD) (:item-id t2)]
               :class "tab"}]
             [:component
              {:relative-id [(:item-id t1) "1"]
               :width 0.75
               :template (any)
               :render-dom (tab-DOM)
               :get-rendering-data (tab-RD)
               :example-element-ids []
               :get-action-data [(ids-AD) [(:item-id t1)]]
               :get-tab-action-data [(tab-AD) (:item-id t1)]
               :class "tab"}]]]]]))
    (is (check
         (render-tab-elements-DOM (second tab1-dom) store)
         [:div {:class "vertical-stack"}
          [:component {:relative-id (:item-id t3-baz)
                       :width 0.75}]
          [:component {:relative-id (:item-id t3-bletch)
                       :width 0.75}]]))
    (is (check
         (render-virtual-DOM (second virt-tab-dom))
         [:div {:class "tab virtualTab editable virtual"}]))))

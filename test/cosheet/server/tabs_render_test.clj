(ns cosheet.server.tabs-render-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet
             [orderable :as orderable]
             [entity :as entity  :refer [label->elements]]
             [query :refer [matching-elements]]
             [expression :refer [expr expr-let expr-seq]]
             [expression-manager :refer [current-value]]
             [debug :refer [envs-to-list simplify-for-print]]
             entity-impl
             [test-utils :refer [check any as-set evals-to let-mutated]])
            (cosheet.server
             [referent :refer [item-referent union-referent exemplar-referent
                               query-referent elements-referent
                               virtual-referent referent?]]
             [item-render :refer [item-without-labels-DOM-R
                                  item-DOM-R]]
             
             [model-utils :refer [new-tab-elements]]
             [tabs-render :refer :all])
             ; :reload
            ))

(def orderables (reduce (fn [os _]
                          (vec (concat (pop os)
                                       (orderable/split (peek os) :after))))
                        [orderable/initial]
                        (range 3)))
(def o1 (nth orderables 0))
(def o2 (nth orderables 1))
(def o3 (nth orderables 2))
(def unused-orderable (nth orderables 3))

(deftest tabs-DOM-test
  (let [inherited {:priority 1
                   :width 3.0
                   :key-prefix [:foo]}
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
        [dom tabs t1 t2 t3] (let-mutated [tabs tabs-list]
                              (expr-let [t1s (label->elements tabs o1)
                                         t2s (label->elements tabs o2)
                                         t3s (label->elements tabs o3)
                                         dom (tabs-DOM-R tabs (first t2s)
                                                         inherited)]
                                [dom tabs
                                 (first t1s) (first t2s) (first t3s)]))
        t1-foo  (first (current-value (matching-elements "foo" t1)))
        t2-foo (first (current-value (matching-elements "foo" t2)))
        t2-bar (first (current-value (matching-elements "bar" t2)))
        t3-baz (first (current-value (matching-elements "baz" t3)))
        t3-bletch (first (current-value (matching-elements "bletch" t3)))
        virtual-tab-referent (fn [elements adjacent-items]
                               (virtual-referent
                                (cons "" elements)
                                (item-referent tabs)
                                (map item-referent adjacent-items)))
        items-referent (fn [& items] (union-referent (map item-referent items)))
        starting-inherited {:priority 1
                            :width 3.0
                            :key-prefix [:foo]
                            :template '(anything)}] 
    (is (check
         dom
         [:div {:class "tabs-wrapper"}
          [:div#batch-edit.tool
           [:img {:src "../icons/edit.gif"}]
           [:div.tooltip "batch edit (C-B)"]]
          [:div.toolgap]
          [:div {:class "tabs-holder"}
           [:div]
           [:div {:class "editable tab virtualTab"
                  :key [:foo :virtual]
                  :target {:referent (virtual-referent
                                      "" (virtual-tab-referent
                                          new-tab-elements nil))
                           :select-pattern [:foo [:pattern]]}
                  :selected {:special :new-tab}}]
           [:div {:class "vertical-stack tab"}
            [:div {:class "content-text editable item"
                   :key [:foo (item-referent t3-baz) :content]
                   :target {:template '(anything)
                            :referent (item-referent t3-baz)}
                   :add-column (any)
                   :selected {:referent (item-referent t3) :special :tab}}
             "baz"]
            [:div (any) (any)]]
           [:div {:class "tab-tree chosen"}
            (any)
            [:div {:class "tab-sequence"}
             (any)
             [:div {:class "editable empty-child tab"
                    :key [:foo :nested (item-referent t1) :virtual]
                    :target {:referent (virtual-referent
                                        '(anything) (item-referent t1)
                                        (item-referent t1))
                             :select-pattern [:foo :nested [:pattern]]}
                    :add-column {:referent
                                 (virtual-tab-referent
                                  (concat [""] new-tab-elements ["foo"])
                                  [t1])}
                    :selected {:referent (item-referent t1)
                               :special :tab}
                    :delete {:referent (item-referent t1)}}]]]]]))))

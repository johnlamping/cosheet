(ns cosheet2.server.item-render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet2
             [orderable :as orderable]
             [query :refer [matching-elements]]
             [debug :refer [envs-to-list simplify-for-print]]
             [entity :refer [description->entity to-list]]
             entity-impl
             [store :refer [make-id new-element-store]]
             store-impl
             mutable-store-impl
             [store-utils :refer [add-entity]]
             [test-utils :refer [check any as-set]])
            (cosheet2.server
             [model-utils :refer [semantic-label-elements]]
             [hierarchy :refer [item-maps-by-elements
                                hierarchy-by-canonical-info]]
             [render :refer [basic-dom-specification]]
             [action-data :refer [composed-get-action-data
                                  get-item-or-exemplar-action-data
                                  get-item-or-exemplar-action-data-for-ids
                                  get-content-only-action-data
                                  get-virtual-action-data]]
             [item-render :refer :all])
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

(deftest render-item-DOM-test-simple
  ;; Test a simple cell
  (let [[store fred-id] (add-entity (new-element-store) nil "Fred")
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id fred-id)
                             store)]
    (is (check dom
               [:div {:class "content-text editable item"} "Fred"])))
  ;; Test a cell with a couple of labels, one excluded.
  (let [[store fred-id] (add-entity (new-element-store) nil
                                    `("Fred"
                                      (1 :label (~o1 :order))
                                      (2 :label (~o2 :order))))
        fred (description->entity fred-id store)
        id1 (:item-id (first (matching-elements 1 fred)))
        id2 (:item-id (first (matching-elements 2 fred)))
        id-tag2 (:item-id (first (matching-elements
                                  :label (description->entity id2 store))))
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id fred-id
                                    :excluded-element-ids [id1])
                             store)]
    (is (check dom
               [:div {:class "wrapped-element label item"}
                [:component {:template '("" :label)
                             :relative-id id2
                             :excluded-element-ids [id-tag2]
                             :class "label"
                             :width 1.5}]
                [:div {:class "indent-wrapper"}
                 [:component {:template '("" (1 :label) (2 :label))
                              :relative-id :content
                              :item-id fred-id
                              :render-dom render-content-only-DOM
                              :get-action-data get-content-only-action-data
                              :width 1.5}]]])))
  ;; Test must-show-label.
  (let [[store fred-id] (add-entity (new-element-store) nil
                                    "Fred")
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id fred-id
                                    :must-show-label true)
                             store)]
    (is (check
         dom
         [:div
          {:class "horizontal-labels-element label virtual-wrapper narrow item"}
          [:component {:template '("" :label)
                       :relative-id :virtual-label
                       :class "label"
                       :get-rendering-data get-virtual-DOM-rendering-data
                       :render-dom render-virtual-DOM
                       :get-action-data [get-virtual-action-data
                                         :template '("" :label)
                                         :position :after]
                       :width 1.5}]
          [:component {:template ""
                       :relative-id :content
                       :item-id fred-id
                       :render-dom render-content-only-DOM
                       :get-action-data get-content-only-action-data
                       :width 1.5}]]))))

(deftest item-DOM-R-test-one-column
  ;; Try a couple of elements with no labels
  (let [[store fred-id] (add-entity (new-element-store) nil
                                    `("Fred"
                                      (2 (~o2 :order))
                                      (1 (~o1 :order))))
        fred (description->entity fred-id store)
        id1 (:item-id (first (matching-elements 1 fred)))
        id2 (:item-id (first (matching-elements 2 fred)))
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id fred-id
                                    :width 0.9)
                             store)]
    (is (check dom
               [:div {:class "with-elements item"}
                [:component {:template ""
                             :width 0.9
                             :relative-id :content
                             :item-id fred-id
                             :render-dom render-content-only-DOM
                             :get-action-data get-content-only-action-data}]
                [:div {:class "vertical-stack"}
                 [:div {:class (str "horizontal-labels-element label"
                                    " virtual-wrapper narrow")}
                  [:component
                   {:width 0.9
                    :template '("" :label)
                    :relative-id [id1 :virtual-label]
                    :get-rendering-data get-virtual-DOM-rendering-data
                    :render-dom render-virtual-DOM
                    :get-action-data
                    [composed-get-action-data
                     [get-item-or-exemplar-action-data-for-ids [id1]]
                     [get-virtual-action-data :template '("" :label)]]
                    :class "label"}]
                  [:component {:width 0.9
                               :template ""
                               :relative-id id1}]]
                 [:div {:class (str "horizontal-labels-element label"
                                    " virtual-wrapper narrow")}
                  [:component
                   {:width 0.9
                    :template '("" :label)
                    :relative-id [id2 :virtual-label]
                    :get-rendering-data get-virtual-DOM-rendering-data
                    :render-dom render-virtual-DOM
                    :get-action-data
                    [composed-get-action-data
                     [get-item-or-exemplar-action-data-for-ids [id2]]
                     [get-virtual-action-data :template '("" :label)]]
                    :class "label"}]
                  [:component {:width 0.9
                               :template ""
                               :relative-id id2}]]]])))
  ;; Test an item with two elements, each with one distinct label.
  (let [[store fred-id] (add-entity (new-element-store) nil
                                    `("Fred"
                                      (2 ("two" :label) (~o2 :order))
                                      (1 ("one" :label) (~o1 :order))))
        fred (description->entity fred-id store)
        item1 (first (matching-elements 1 fred))
        label1 (first (matching-elements "one" item1))
        tag1 (first (matching-elements :label label1))
        id1 (:item-id item1)
        id-label1 (:item-id label1)
        id-tag1 (:item-id tag1)
        item2 (first (matching-elements 2 fred))
        label2 (first (matching-elements "two" item2))
        tag2 (first (matching-elements :label label2))
        id2 (:item-id item2)
        id-label2 (:item-id label2)
        id-tag2 (:item-id tag2)
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id fred-id
                                    :width 0.9)
                             store)]
    (is (check dom
               [:div {:class "with-elements item"}
                [:component {:template ""
                             :width 0.9
                             :relative-id :content
                             :item-id fred-id
                             :render-dom render-content-only-DOM
                             :get-action-data get-content-only-action-data}]
                [:div {:class "vertical-stack"}
                 [:div {:class "wrapped-element label"}
                  [:component {:width 0.9
                               :template '("" :label)
                               :get-action-data
                               [composed-get-action-data
                                [get-item-or-exemplar-action-data-for-ids [id1]]
                                get-item-or-exemplar-action-data]
                               :class "label"
                               :excluded-element-ids [id-tag1]
                               :relative-id id-label1}]
                  [:div {:class "indent-wrapper"}
                   [:component {:width 0.9
                                :template '("" ("one" :label))
                                :excluded-element-ids [id-label1]
                                :relative-id id1}]]]
                 [:div {:class "wrapped-element label"}
                  [:component {:width 0.9
                               :template '("" :label)
                               :get-action-data
                               [composed-get-action-data
                                [get-item-or-exemplar-action-data-for-ids [id2]]
                                get-item-or-exemplar-action-data]
                               :class "label"
                               :excluded-element-ids [id-tag2]
                               :relative-id id-label2}]
                  [:div {:class "indent-wrapper"}
                   [:component {:width 0.9
                                :template '("" ("two" :label))
                                :excluded-element-ids [id-label2]
                                :relative-id id2}]]]]])))
  ;; Test an item with four elements, with label sharing among them.
  (let [[store fred-id] (add-entity (new-element-store) nil
                                    `("Fred"
                                      (0 ("zero" :label) (~o1 :order))
                                      (2 ("two" :label (~o1 :order))
                                         ("both" :label (~o2 :order))
                                         (~o3 :order))
                                      (1 ("one" :label (~o1 :order))
                                         ("both" :label (~o2 :order)))
                                      (3 (~o4 :order))))
        fred (description->entity fred-id store)
        item0 (first (matching-elements 0 fred))
        label0 (first (matching-elements "zero" item0))
        tag0 (first (matching-elements :label label0))
        id0 (:item-id item0)
        id-label0 (:item-id label0)
        id-tag0 (:item-id tag0)
        item1 (first (matching-elements 1 fred))
        label1one (first (matching-elements "one" item1))
        tag1one (first (matching-elements :label label1one))
        label1both (first (matching-elements "both" item1))
        tag1both (first (matching-elements :label label1both))
        id1 (:item-id item1)
        id-label1one (:item-id label1one)
        id-tag1one (:item-id tag1one)
        id-label1both (:item-id label1both)
        id-tag1both (:item-id tag1both)
        item2 (first (matching-elements 2 fred))
        label2two (first (matching-elements "two" item2))
        tag2two (first (matching-elements :label label2two))
        label2both (first (matching-elements "both" item2))
        tag2both (first (matching-elements :label label2both))
        id2 (:item-id item2)
        id-label2two (:item-id label2two)
        id-tag2two (:item-id tag2two)
        id-label2both (:item-id label2both)
        id-tag2both (:item-id tag2both)
        item3 (first (matching-elements 3 fred))
        id3 (:item-id item3)
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id fred-id
                                    :width 0.9)
                             store)]
    (is (check dom
               [:div {:class "with-elements item"}
                [:component {:template ""
                             :width 0.9
                             :relative-id :content
                             :item-id fred-id
                             :render-dom render-content-only-DOM
                             :get-action-data get-content-only-action-data}]
                [:div {:class "vertical-stack"}
                 [:div {:class "wrapped-element label"}
                  [:component {:width 0.9
                               :template '("" :label)
                               :get-action-data
                               [composed-get-action-data
                                [get-item-or-exemplar-action-data-for-ids [id0]]
                                get-item-or-exemplar-action-data]
                               :class "label"
                               :excluded-element-ids [id-tag0]
                               :relative-id id-label0}]
                  [:div {:class "indent-wrapper"}
                   [:component {:width 0.9
                                :template '("" ("zero" :label))
                                :excluded-element-ids [id-label0]
                                :relative-id id0}]]]
                 [:div {:class "wrapped-element label"}
                  [:component {:width 0.9
                               :template '("" :label)
                               :get-action-data
                               [composed-get-action-data
                                [get-item-or-exemplar-action-data-for-ids
                                 [id1 id2]]
                                get-item-or-exemplar-action-data]
                               :class "label"
                               :excluded-element-ids [id-tag1both]
                               :relative-id id-label1both}]
                  [:div {:class "indent-wrapper"}
                   [:div {:class "vertical-stack"}
                    [:div {:class "wrapped-element label"}
                     [:component {:width 0.9
                                  :template '("" :label)
                                  :get-action-data
                                  [composed-get-action-data
                                   [get-item-or-exemplar-action-data-for-ids
                                    [id1]]
                                   get-item-or-exemplar-action-data]
                                  :class "label"
                                  :excluded-element-ids [id-tag1one]
                                  :relative-id id-label1one}]
                     [:div {:class "indent-wrapper"}
                      [:component {:width 0.9
                                   :template '("" ("both" :label)
                                                    ("one" :label))
                                   :excluded-element-ids [id-label1both
                                                          id-label1one]
                                   :relative-id id1}]]]
                    [:div {:class "wrapped-element label"}
                     [:component {:width 0.9
                                  :template '("" :label)
                                  :get-action-data
                                  [composed-get-action-data
                                   [get-item-or-exemplar-action-data-for-ids
                                    [id2]]
                                   get-item-or-exemplar-action-data]
                                  :class "label"
                                  :excluded-element-ids [id-tag2two]
                                  :relative-id id-label2two}]
                     [:div {:class "indent-wrapper"}
                      [:component {:width 0.9
                                   :template '("" ("both" :label)
                                                    ("two" :label))
                                   :excluded-element-ids [id-label2both
                                                          id-label2two]
                                   :relative-id id2}]]]]]]
                [:div {:class (str "horizontal-labels-element label"
                                   " virtual-wrapper narrow")}
                 [:component {:width 0.9
                              :template '("" :label)
                              :get-action-data
                              [composed-get-action-data
                               [get-item-or-exemplar-action-data-for-ids [id3]]
                               [get-virtual-action-data :template '("" :label)]]
                              :relative-id [id3 :virtual-label]
                              :get-rendering-data get-virtual-DOM-rendering-data
                              :render-dom render-virtual-DOM
                              :class "label"}]
                 [:component {:width 0.9
                              :template ""
                              :relative-id id3}]]]])))
  )

(deftest item-DOM-R-test-two-column
  ;; Try a couple of elements with no labels
  (let [[store fred-id] (add-entity (new-element-store) nil
                                    `("Fred"
                                      (2 (~o2 :order))
                                      (1 (~o1 :order))))
        fred (description->entity fred-id store)
        id1 (:item-id (first (matching-elements 1 fred)))
        id2 (:item-id (first (matching-elements 2 fred)))
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id fred-id
                                    :width 1.5
                                    :must-show-labels :wide)
                             store)]
    (is (check
         dom
         [:div {:class "with-elements item"}
          [:component {:template ""
                       :width 1.5
                       :relative-id :content
                       :item-id fred-id
                       :render-dom render-content-only-DOM
                       :get-action-data get-content-only-action-data}]
          [:div {:class "vertical-stack"}
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class "label horizontal-header top-border bottom-border"}
             [:component {:width 0.375
                          :template '("" :label)
                          :relative-id [id1 :virtual-label]
                          :get-action-data
                          [composed-get-action-data
                           [get-item-or-exemplar-action-data-for-ids [id1]]
                           [get-virtual-action-data :template '("" :label)]]
                          :get-rendering-data get-virtual-DOM-rendering-data
                          :render-dom render-virtual-DOM
                          :class "label"}]]
            [:component {:width 1.03125
                         :template ""
                         :relative-id id1}]]
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class "label horizontal-header top-border bottom-border"}
             [:component {:width 0.375
                          :template '("" :label)
                          :relative-id [id2 :virtual-label]
                          :get-action-data
                          [composed-get-action-data
                           [get-item-or-exemplar-action-data-for-ids [id2]]
                           [get-virtual-action-data :template '("" :label)]]
                          :get-rendering-data get-virtual-DOM-rendering-data
                          :render-dom render-virtual-DOM
                          :class "label"}]]
            [:component {:width 1.03125
                         :template ""
                         :relative-id id2}]]]])))
  ;; Test an item with two elements, each with one distinct label.
  (let [[store fred-id] (add-entity (new-element-store) nil
                                    `("Fred"
                                      (2 ("two" :label) (~o2 :order))
                                      (1 ("one" :label) (~o1 :order))))
        fred (description->entity fred-id store)
        item1 (first (matching-elements 1 fred))
        label1 (first (matching-elements "one" item1))
        tag1 (first (matching-elements :label label1))
        id1 (:item-id item1)
        id-label1 (:item-id label1)
        id-tag1 (:item-id tag1)
        item2 (first (matching-elements 2 fred))
        label2 (first (matching-elements "two" item2))
        tag2 (first (matching-elements :label label2))
        id2 (:item-id item2)
        id-label2 (:item-id label2)
        id-tag2 (:item-id tag2)
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id fred-id
                                    :width 1.5)
                             store)]
    (is (check
         dom
         [:div {:class "with-elements item"}
          [:component {:template ""
                       :width 1.5
                       :relative-id :content
                       :item-id fred-id
                       :render-dom render-content-only-DOM
                       :get-action-data get-content-only-action-data}]
          [:div {:class "vertical-stack"}
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class (str "label horizontal-header"
                               " top-border bottom-border")}
             [:component {:width 0.375, :template '("" :label)
                          :get-action-data
                          [composed-get-action-data
                           [get-item-or-exemplar-action-data-for-ids [id1]]
                           get-item-or-exemplar-action-data]
                          :class "label"
                          :excluded-element-ids [id-tag1]
                          :relative-id id-label1}]]
            [:component {:width 1.03125
                         :template '("" ("one" :label))
                         :excluded-element-ids [id-label1]
                         :relative-id id1}]]
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class (str "label horizontal-header"
                               " top-border bottom-border")}
             [:component {:width 0.375, :template '("" :label)
                          :get-action-data
                          [composed-get-action-data
                           [get-item-or-exemplar-action-data-for-ids [id2]]
                           get-item-or-exemplar-action-data]
                          :class "label"
                          :excluded-element-ids [id-tag2]
                          :relative-id id-label2}]]
            [:component {:width 1.03125
                         :template '("" ("two" :label))
                         :excluded-element-ids [id-label2]
                         :relative-id id2}]]]])))
  ;; Test an item with four elements, with label sharing among them.
  (let [[store fred-id] (add-entity (new-element-store) nil
                                    `("Fred"
                                      (0 ("zero" :label) (~o1 :order))
                                      (2 ("two" :label (~o1 :order))
                                         ("both" :label (~o2 :order))
                                         (~o3 :order))
                                      (1 ("one" :label (~o1 :order))
                                         ("both" :label (~o2 :order)))
                                      (3 (~o4 :order))))
        fred (description->entity fred-id store)
        item0 (first (matching-elements 0 fred))
        label0 (first (matching-elements "zero" item0))
        tag0 (first (matching-elements :label label0))
        id0 (:item-id item0)
        id-label0 (:item-id label0)
        id-tag0 (:item-id tag0)
        item1 (first (matching-elements 1 fred))
        label1one (first (matching-elements "one" item1))
        tag1one (first (matching-elements :label label1one))
        label1both (first (matching-elements "both" item1))
        tag1both (first (matching-elements :label label1both))
        id1 (:item-id item1)
        id-label1one (:item-id label1one)
        id-tag1one (:item-id tag1one)
        id-label1both (:item-id label1both)
        id-tag1both (:item-id tag1both)
        item2 (first (matching-elements 2 fred))
        label2two (first (matching-elements "two" item2))
        tag2two (first (matching-elements :label label2two))
        label2both (first (matching-elements "both" item2))
        tag2both (first (matching-elements :label label2both))
        id2 (:item-id item2)
        id-label2two (:item-id label2two)
        id-tag2two (:item-id tag2two)
        id-label2both (:item-id label2both)
        id-tag2both (:item-id tag2both)
        item3 (first (matching-elements 3 fred))
        id3 (:item-id item3)
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id fred-id
                                    :width 1.5)
                             store)]
    (is (check
         dom
         [:div {:class "with-elements item"}
          [:component {:template ""
                       :width 1.5
                       :relative-id :content
                       :item-id fred-id
                       :render-dom render-content-only-DOM
                       :get-action-data get-content-only-action-data}]
          [:div {:class "vertical-stack"}
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class "label horizontal-header top-border bottom-border"}
             [:component {:width 0.375
                          :template '("" :label)
                          :get-action-data
                          [composed-get-action-data
                           [get-item-or-exemplar-action-data-for-ids [id0]]
                           get-item-or-exemplar-action-data]
                          :class "label"
                          :excluded-element-ids [id-tag0]
                          :relative-id id-label0}]]
            [:component {:width 1.03125
                         :template '("" ("zero" :label))
                         :excluded-element-ids [id-label0]
                         :relative-id id0}]]
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class "label horizontal-header top-border"}
             [:component {:width 0.375
                          :template '("" :label)
                          :get-action-data
                          [composed-get-action-data
                           [get-item-or-exemplar-action-data-for-ids [id1 id2]]
                           get-item-or-exemplar-action-data]
                          :class "label"
                          :excluded-element-ids [id-tag1both]
                          :relative-id id-label1both}]]
            [:component {:width 1.03125
                         :template '("" ("both" :label))
                         :relative-id :virtual
                         :get-rendering-data get-virtual-DOM-rendering-data
                         :render-dom render-virtual-DOM
                         :get-action-data [get-virtual-action-data
                                           :template '("" ("both" :label))
                                           :adjacent-id id1
                                           :position :after]}]]
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class "label horizontal-header indent"}
             [:div {:class "label horizontal-header top-border bottom-border"}
              [:component {:width 0.375
                           :template '("" :label)
                           :get-action-data
                           [composed-get-action-data
                            [get-item-or-exemplar-action-data-for-ids [id1]]
                            get-item-or-exemplar-action-data]
                           :class "label"
                           :excluded-element-ids [id-tag1one]
                           :relative-id id-label1one}]]]
            [:component {:width 1.03125
                         :template '("" ("both" :label) ("one" :label))
                         :excluded-element-ids [id-label1both id-label1one]
                         :relative-id id1}]]
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class "label horizontal-header indent bottom-border"}
             [:div {:class "label horizontal-header top-border bottom-border"}
              [:component {:width 0.375
                           :template '("" :label)
                           :get-action-data
                           [composed-get-action-data
                            [get-item-or-exemplar-action-data-for-ids [id2]]
                            get-item-or-exemplar-action-data]
                           :class "label"
                           :excluded-element-ids [id-tag2two]
                           :relative-id id-label2two}]]]
            [:div {:class "horizontal-value-last"}
             [:component {:width 1.03125
                          :template '("" ("both" :label) ("two" :label))
                          :excluded-element-ids [id-label2both id-label2two]
                          :relative-id id2}]]]
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class "label horizontal-header top-border bottom-border"}
             [:component {:width 0.375
                          :template '("" :label)
                          ;; TODO: This breaks the relative id convention.
                          :relative-id [id3 :virtual-label]
                          :get-action-data
                          [composed-get-action-data
                           [get-item-or-exemplar-action-data-for-ids [id3]]
                           [get-virtual-action-data :template '("" :label)]]
                          :get-rendering-data get-virtual-DOM-rendering-data
                          :render-dom render-virtual-DOM
                          :class "label"}]]
            [:component {:width 1.03125
                         :template ""
                         :relative-id id3}]]]]))))

(deftest render-virtual-DOM-test
  (is (check (render-virtual-DOM {:class "foo"})
             [:div {:class "foo editable"}])))

(deftest horizontal-label-hierarchy-node-DOM-test
  (let [[s4 joe-id] (add-entity (new-element-store) nil "Joe")
        [s5 joe-test-id] (add-entity s4 joe-id "test")
        [s6 joe-test-label-id] (add-entity s5 joe-test-id :label)
        [s7 joe-foo-id] (add-entity s6 joe-id "foo")
        [s8 joe-foo-label-id] (add-entity s7 joe-foo-id :label)
        [s9 jane-id] (add-entity s8 nil "Jane")
        [s10 jane-test-id] (add-entity s9 jane-id "test")
        [store jane-test-label-id] (add-entity s10 jane-test-id :label)
        joe (description->entity joe-id store)
        jane (description->entity jane-id store)
        ordered-entities [joe jane]
        labelses (map semantic-label-elements ordered-entities)
        item-maps (item-maps-by-elements ordered-entities labelses)
        hierarchy (hierarchy-by-canonical-info item-maps)
        node (first hierarchy)]
    ;; A node with no leaves.
    (is (check
         (horizontal-label-hierarchy-node-DOM node {})
         [:component
          {:template '("" :label)
           :get-action-data [composed-get-action-data
                             [get-item-or-exemplar-action-data-for-ids
                              [joe-id jane-id]]
                             get-item-or-exemplar-action-data]
           :relative-id joe-test-id
           :class "label"
           :excluded-element-ids [joe-test-label-id]}]))
    ;; A node with a leaf,  properties, and no children
    (is (check
         (horizontal-label-hierarchy-node-DOM (first (:child-nodes node)) {})
         [:component {:relative-id joe-id
                      :excluded-element-ids [joe-test-id]}]))
    ;; A node with leaves, no properties, and no children
    (is (check
         (horizontal-label-hierarchy-node-DOM (second (:child-nodes node)) {})
         [:div {:class "tag wrapped-element virtual-wrapper merge-with-parent"}
          [:component {:template '("" :label)
                       :get-action-data
                       [composed-get-action-data
                        [get-item-or-exemplar-action-data-for-ids [jane-id]]
                        [get-virtual-action-data :template '("" :label)]]
                       :relative-id :nested
                       :class "tag merge-with-parent"
                       :render-dom render-virtual-DOM
                       :get-rendering-data get-virtual-DOM-rendering-data}]
          [:div {:class "indent-wrapper tag"}
           [:component {:relative-id jane-id
                        :excluded-element-ids [jane-test-id]}]]]))))

(comment
  (deftest item-DOM-R-test-one-column
    ;; Test a one-column element hierarchy
    (let [age-as-list `(39 (:root :non-semantic)
                           (~o3 :order :non-semantic)
                           ("one" ; One tag.
                            ("confidence"
                             :tag (~o1 :order :non-semantic))
                            (~o1 :order :non-semantic))
                           ("another" ; Second with same tag.
                            ("confidence"
                             :tag (~o1 :order :non-semantic))
                            (~o2 :order :non-semantic))
                           ("two" ; Two tags, one matching.
                            ("confidence"
                             :tag (~o1 :order :non-semantic))
                            ("probability"
                             :tag (~o2 :order :non-semantic))
                            (~o3 :order :non-semantic))
                           ("none" ; No tag.
                            (~o4 :order :non-semantic)))
          [dom age] (let-mutated [age age-as-list]
                      (expr-let [ae (matching-elements "another" age)
                                 ne (matching-elements "none" age)
                                 another (first ae)
                                 none (first ne)
                                 inherited
                                 (into base-inherited
                                       {:width 0.5
                                        :template "foo"
                                        :attributes
                                        [[#{:content} {:added-by-test {1 2}}]
                                         [(:item-id another) {:class "added1"}]
                                         [(:item-id none) {:class "added2"}]]})]
                        (expr-let [dom (item-DOM-R age [] inherited)]
                          [dom age])))
          one (first (current-value (matching-elements "one" age)))
          confidence1 (first (current-value
                              (matching-elements "confidence" one)))
          confidence1-tag (first (current-value
                                  (matching-elements :tag confidence1)))
          another (first (current-value (matching-elements "another" age)))
          two (first (current-value (matching-elements "two" age)))
          confidence2 (first (current-value
                              (matching-elements "confidence" two)))
          probability (first (current-value
                              (matching-elements "probability" two)))
          probability-tag (first (current-value
                                  (matching-elements :tag probability)))
          none (first (current-value (matching-elements "none" age)))
          age-key [:root (:item-id age)]
          none-key (conj age-key (:item-id none))
          one-another-two-items [(item-referent one)
                                 (item-referent another)
                                 (item-referent two)]
          one-another-two-referent (union-referent one-another-two-items)]
      (is (check
           dom
           [:div {:class "item with-elements"}
            [:div {:class "content-text editable"
                   :key (conj age-key :content)
                   :target {:referent (item-referent age)
                            :template "foo"}
                   :added-by-test {1 2}}
             "39"]
            [:div {:class "vertical-stack"}
             ;; Everything with "confidence"
             [:div {:class "wrapped-element tag"}
              [:div {:key (conj age-key (:item-id confidence1) :content)
                     :class "content-text editable item tag"
                     :target {:template '(anything :tag)
                              :referent [:exemplar
                                         (item-referent confidence1)
                                         one-another-two-referent]}
                     :add-sibling
                     {:referent (virtual-referent
                                 '(anything) (item-referent age)
                                 one-another-two-items) 
                      :select-pattern (conj age-key [:pattern])}}
               "confidence"]
              [:div {:class "indent-wrapper"}
               [:div {:class "vertical-stack"}
                ;; One Another
                [:div {:class "vertical-stack"}
                 [:div (any) "one"]
                 [:div (any) "another"]]
                ;; Two (must be nested)
                [:div {:class "wrapped-element tag"}
                 [:div
                  {:key (conj age-key (:item-id probability) :content)
                   :class "content-text editable item tag"
                   :target {:template '(anything :tag)
                            :referent (item-referent probability)}
                   :add-sibling
                   {:referent (virtual-referent
                               '(anything ("confidence" :tag))
                               (item-referent age)
                               (item-referent two)) 
                    :select-pattern (conj age-key [:pattern])}}
                  "probability"]         
                 [:div {:class "indent-wrapper"}
                  [:div (any) "two"]]]]]]
             ;; None
             [:div {:class "horizontal-tags-element tag virtual-wrapper narrow added2"}
              [:div {:class "editable tag"
                     :key (conj (conj age-key (:item-id none)) :virtual)
                     :target {:referent (virtual-referent
                                         '(anything :tag)
                                         (item-referent none) nil)
                              :select-pattern (conj age-key [:pattern])}
                     :add-sibling {:referent (virtual-referent
                                              '(anything)
                                              (item-referent age)
                                              (item-referent none)) 
                                   :select-pattern (conj age-key [:pattern])}}]
              [:div (any) "none"]]]]))))

  (deftest item-DOM-R-test-two-column  
    ;; Test two column element hierarchy.
    (let [age-as-list `(39 (:root :non-semantic)
                           (~o3 :order :non-semantic)
                           ("pair" ; Two tags.
                            ("confidence"
                             :tag (~o1 :order :non-semantic))
                            ("likelihood"
                             :tag (~o2 :order :non-semantic))
                            (~o1 :order :non-semantic))
                           ("double" ; Second with same tags.
                            ("confidence"
                             :tag (~o1 :order :non-semantic))
                            ("likelihood"
                             :tag (~o2 :order :non-semantic))
                            (~o2 :order :non-semantic))
                           ("two" ; Two tags, only one matching.
                            ("confidence"
                             :tag (~o1 :order :non-semantic))
                            ("probability"
                             :tag (~o2 :order :non-semantic))
                            (~o3 :order :non-semantic))
                           ("one"
                            ("confidence"
                             :tag (~o1 :order :non-semantic))
                            (~o4 :order :non-semantic))
                           ("unique"
                            ("certainty"
                             :tag (~o1 :order :non-semantic))
                            (~o5 :order :non-semantic)))
          [dom age] (let-mutated [age age-as-list]
                      (expr-let [de (matching-elements "double" age)
                                 ue (matching-elements "unique" age)
                                 double (first de)
                                 unique (first ue)
                                 inherited
                                 (assoc base-inherited
                                        :width 1.0
                                        :attributes
                                        [[(:item-id double) {:class "added1"}]
                                         [(:item-id unique) {:class "added2"}]])]
                        (expr-let [dom (item-DOM-R age [] inherited)]
                          [dom age])))
          pair (first (current-value (matching-elements "pair" age)))
          confidence1 (first (current-value
                              (matching-elements "confidence" pair)))
          confidence1-tag (first (current-value
                                  (matching-elements :tag confidence1)))
          likelihood (first (current-value
                             (matching-elements "likelihood" pair)))
          likelihood-tag (first (current-value
                                 (matching-elements :tag likelihood)))
          double (first (current-value (matching-elements "double" age)))
          two (first (current-value (matching-elements "two" age)))
          confidence2 (first (current-value
                              (matching-elements "confidence" two)))
          probability (first (current-value
                              (matching-elements "probability" two)))
          
          probability-tag (first (current-value
                                  (matching-elements :tag probability)))
          one (first (current-value (matching-elements "one" age)))
          confidence3 (first (current-value
                              (matching-elements "confidence" one)))
          age-key [:root (:item-id age)]
          one-key (conj age-key (:item-id one))
          unique (first (current-value (matching-elements "unique" age)))
          certainty (first (current-value (matching-elements "certainty" unique)))
          likelihoods [(item-referent pair)
                       (item-referent double)]
          likelihoods-referent (union-referent likelihoods)
          all-elements [(item-referent pair)
                        (item-referent double)
                        (item-referent two)
                        (item-referent one)]
          all-elements-referent (union-referent all-elements)]
      (is (check
           dom
           [:div {:class "item with-elements"}
            [:div (any map?) "39"]
            [:div {:class "vertical-stack"}
             [:div {:class "horizontal-tags-element tag wide"}
              ;; Group with empty item.
              [:div {:class "tag horizontal-header top-border"}
               [:div {:key (conj age-key (:item-id confidence1) :content)
                      :class "content-text editable item tag"
                      :target {:template '(anything :tag)
                               :referent [:exemplar
                                          (:item-id confidence1)
                                          all-elements-referent]}
                      :add-sibling {:referent (virtual-referent
                                               '(anything) (item-referent age)
                                               all-elements) 
                                    :select-pattern (conj age-key [:pattern])}}
                "confidence"]]
              [:div {:class "editable"
                     :key (conj age-key
                                :example-element (:item-id confidence1) :virtual)
                     :target {:referent (virtual-referent
                                         '(anything ("confidence" :tag))
                                         (item-referent age)
                                         (item-referent pair)
                                         :position :before)
                              :select-pattern (conj age-key [:pattern])}
                     :add-sibling {:referent (virtual-referent
                                              '(anything) (item-referent age)
                                              all-elements) 
                                   :select-pattern (conj age-key [:pattern])}}]]
             ;; Group for confidence and likelihood.
             [:div {:class "horizontal-tags-element tag wide"}
              [:div {:class "tag horizontal-header indent"}
               [:div {:class "tag horizontal-header top-border bottom-border"}
                [:div (any) "likelihood"]]]
              [:div {:class "vertical-stack"}
               [:div (any) "pair"]
               [:div (any) "double"]]]
             ;; Group for confidence and probability
             [:div {:class "horizontal-tags-element tag wide"}
              [:div {:class "tag horizontal-header indent"}
               [:div {:class "tag horizontal-header top-border bottom-border"}
                [:div (any) "probability"]]]
              [:div (any) "two"]]
             ;; Group for confidence
             [:div {:class "horizontal-tags-element tag wide"}
              [:div {:class "tag horizontal-header indent bottom-border"}
               (any)]
              [:div {:class "horizontal-value-last"}
               [:div (any) "one"]]]
             ;; Group for unique
             [:div {:class "horizontal-tags-element tag wide added2"}
              [:div {:class "tag horizontal-header top-border bottom-border"}
               [:div
                {:key (conj age-key (item-referent certainty) :content)
                 :class "content-text editable item tag"
                 :target {:template '(anything :tag)
                          :referent (item-referent certainty)}
                 :add-sibling {:referent (virtual-referent
                                          '(anything)
                                          (item-referent age)
                                          (item-referent unique)) 
                               :select-pattern (conj age-key [:pattern])}}
                "certainty"]]
              [:div
               {:key (conj age-key (item-referent unique) :content)
                :class "content-text editable item"
                :target {:template '(anything ("certainty" :tag))
                         :referent (item-referent unique)}
                :add-sibling {:referent (virtual-referent
                                         '(anything)
                                         (item-referent age)
                                         (item-referent unique)) 
                              :select-pattern (conj age-key [:pattern])}}
               "unique"]]]]))))

;;; Test an item that needs to be wrapped in labels.
  (deftest item-DOM-R-test-labels
    ;; First, test when there is a label.
    (let [element-as-list `(39
                            ("age" :tag (~o1 :order :non-semantic))
                            ("Ke"
                             ("according-to" :tag (~o1 :order :non-semantic))
                             (~o1 :order :non-semantic)))
          inherited  (assoc base-inherited :width 1.0)
          [dom element] (let-mutated [element element-as-list]
                          (expr-let [dom (item-DOM-R element [] inherited)]
                            [dom element]))]
      (let [element-key [:root (:item-id element)]
            age (first (current-value (matching-elements "age" element)))
            age-tag (first (current-value (matching-elements :tag age)))
            age-key [:root (:item-id age)]]
        (is (check dom
                   [:div {:class "wrapped-element tag"}
                    [:div
                     {:key (conj age-key :content)
                      :class "content-text editable item tag"
                      :target {:template '(anything :tag)
                               :referent (item-referent age)}
                      :expand {:referent (item-referent element)}}
                     "age"]
                    [:div {:class "indent-wrapper"}
                     [:div {:class "item with-elements"}
                      [:div {:class "content-text editable"
                             :target {:referent (item-referent element)
                                      :template ""}
                             :key (conj element-key :content)}
                       "39"]
                      [:div {:class "horizontal-tags-element tag wide"}
                       (any)
                       [:div (any) "Ke"]]]]]))))
    ;; Then test when there is no label, but labels must be shown.
    (let [item-as-list `(39 ("Ke"
                             ("according-to" :tag (~o1 :order :non-semantic))
                             (~o1 :order :non-semantic)))
          inherited  (assoc base-inherited :width 1.0)
          [dom item] (let-mutated [item item-as-list]
                       (expr-let [dom (item-DOM-R
                                       item [] inherited :must-show-label true)]
                         [dom item]))]
      (let [item-key [:root (:item-id item)]]
        (is (check dom
                   [:div {:class
                          "horizontal-tags-element tag virtual-wrapper narrow"}
                    [:div {:class "editable tag"
                           :key (conj item-key :tags :virtual)
                           :expand {:referent (item-referent item)}
                           :target {:referent (virtual-referent
                                               '(anything :tag)
                                               (item-referent item)
                                               nil
                                               :position :after)
                                    :select-pattern (conj item-key
                                                          :tags [:pattern])}}]
                    [:div {:class "item with-elements"}
                     [:div {:class "content-text editable"
                            :target {:referent (item-referent item)
                                     :template ""}
                            :key (conj item-key :content)}
                      "39"]
                     [:div {:class "horizontal-tags-element tag wide"}
                      (any)
                      [:div (any) "Ke"]]]])))))

;;; TODO: Add a test where some of the arguments are true.
  (deftest labels-and-elements-DOM-R-test
    (let [element-as-list `(39
                            ("age" :tag (~o1 :order :non-semantic))
                            ("Ke"
                             ("according-to" :tag (~o1 :order :non-semantic))
                             (~o1 :order :non-semantic)))

          element (let-mutated [element element-as-list]
                    element)
          element-key [:root (:item-id element)]
          element-referent (item-referent element)
          age (first (current-value (matching-elements "age" element)))
          age-tag (first (current-value (matching-elements :tag age)))
          age-key (conj element-key (:item-id age))
          qualifier (first (current-value (matching-elements "Ke" element)))
          according-to (first (current-value (matching-elements
                                              "according-to" qualifier)))
          according-to-id (:item-id according-to)
          inherited  (assoc base-inherited
                            :width 1.0
                            :key-prefix element-key
                            :subject-referent element-referent
                            :attributes [[#{:content} {:class "placeholder"}]])
          dom (current-value
               (labels-and-elements-DOM-R
                [age qualifier] false false false :vertical inherited))]
      (is (check dom
                 [:div {:class "wrapped-element tag"}
                  [:div {:key (conj age-key :content)
                         :class "content-text editable item tag"
                         :target {:template '(anything :tag)
                                  :referent (item-referent age)}}
                   "age"]
                  [:div {:class "indent-wrapper"}
                   [:div {:class "item elements-wrapper"}
                    [:div {:class "horizontal-tags-element tag wide"}
                     [:div {:class
                            "tag horizontal-header top-border bottom-border"}
                      [:div (any) "according-to"]]
                     [:div (any) "Ke"]]]]]))))


  (deftest item-content-and-elements-DOM-R-test
    (let [element-as-list `(39
                            ("age" :tag (~o1 :order :non-semantic))
                            ("Ke"
                             ("according-to" :tag (~o1 :order :non-semantic))
                             (~o1 :order :non-semantic)))

          element (let-mutated [element element-as-list]
                    element)
          element-key [:root (:item-id element)]
          element-referent (item-referent element)
          age (first (current-value (matching-elements "age" element)))
          age-tag (first (current-value (matching-elements :tag age)))
          age-key (conj element-key (:item-id age))
          qualifier (first (current-value (matching-elements "Ke" element)))
          according-to (first (current-value (matching-elements
                                              "according-to" qualifier)))
          according-to-id (:item-id according-to)
          inherited  (assoc base-inherited
                            :width 1.0
                            :key-prefix element-key
                            :subject-referent element-referent
                            :attributes [[#{:content} {:class "placeholder"}]])
          dom (current-value
               (item-content-and-elements-DOM-R
                age 39 [age qualifier] false inherited))]
      (is (check dom
                 [:div {:class "wrapped-element tag"}
                  (any)
                  [:div {:class "indent-wrapper"}
                   [:div {:class "item with-elements"}
                    [:div {:class "placeholder content-text editable"
                           :target {:referent element-referent
                                    :template ""}
                           :key (conj element-key :content)}
                     "39"]
                    [:div {:class "horizontal-tags-element tag wide"}
                     [:div {:class
                            "tag horizontal-header top-border bottom-border"}
                      [:div (any) "according-to"]]                   
                     [:div (any) "Ke"]]]]]))))
  )

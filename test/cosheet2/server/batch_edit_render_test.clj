(ns cosheet2.server.batch-edit-render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet2
             [orderable :as orderable]
             [reporter :refer [new-reporter set-value! reporter-value]]
             [task-queue :refer [new-priority-task-queue]]
             [calculator :refer [new-calculator-data request compute]]
             [entity :refer [elements]]
             [store :refer [new-element-store new-mutable-store store-reset!
                            id->element-ids id->subject]]
             store-impl
             [store-utils :refer [add-entity]]
             [query :refer [matching-items matching-elements not-query
                            extended-by?]]
             [entity :as entity  :refer [description->entity
                                         label->elements elements to-list]]
             [expression :refer [expr-let]]
             [debug :refer [simplify-for-print]]
             entity-impl
             [test-utils :refer [check any as-set]])
            (cosheet2.server
             [item-render :refer [render-virtual-DOM
                                  get-virtual-DOM-rendering-data
                                  get-item-rendering-data]]
             [action-data :refer [get-pass-through-action-data
                                  get-virtual-action-data
                                  composed-get-action-data
                                  multiple-items-get-action-data
                                  get-item-or-exemplar-action-data]]
             [order-utils :refer [ordered-entities add-order-elements]]
             [model-utils :refer [semantic-to-list]]
             [batch-edit-render :refer :all])
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
(defn virt-RD [] get-virtual-DOM-rendering-data)

(defn pass-AD [] get-pass-through-action-data)
(defn virt-AD [] get-virtual-action-data)
(defn item-AD [] get-item-or-exemplar-action-data)
(defn comp-AD [] composed-get-action-data)
(defn mult-items-AD [] multiple-items-get-action-data)
(defn batch-query-AD [] get-batch-edit-query-element-action-data)


(defn run-renderer
  "run the renderer on the output of the data getter, thus testing
  that they work together correctly."
  ([spec store]
   (run-renderer (:render-dom spec) spec (:get-rendering-data spec) store))
  ([renderer spec data-getter store]
   (let [ms (new-mutable-store store)
         data (data-getter spec ms)
         cd (new-calculator-data (new-priority-task-queue 0))]
     (doseq [[rep dep] data]
       (request rep cd))
       (compute cd)
       (apply renderer spec (map #(reporter-value (first %)) data)))))

(def t0 (add-entity (new-element-store) nil
                    (add-order-elements
                     '(:x ("s1" :label)
                          (anything ("c1" :label) :column)
                          (anything ("c2" :label) :column)
                          :row-condition))))
(def h1 (second t0))
(def t1 (add-entity (first t0) nil (add-order-elements
                                    '(""
                                      (2 ("c1" :label))
                                      (2 ("c2" :label))
                                      :top-level))))
(def r1 (second t1))
(def t2 (add-entity (first t1) nil (add-order-elements
                                    '(""
                                      (2 ("c1" :label))
                                      (3 ("c2" :label))
                                      :top-level))))
(def r2 (second t2))
(def t3 (add-entity (first t2) nil (add-order-elements
                                    '(anything (anything ("c1" :label))))))
(def q1 (second t3))
(def t4 (add-entity (first t3) nil (add-order-elements
                                    '(anything 2 (anything ("c1" :label))))))
(def q2 (second t4))
(def t5 (add-entity (first t4) nil (add-order-elements
                                    '(anything (anything ("c1" :label))))))
(def stk1 (second t5))
(def s (first t5))

(deftest match-count-R-test
  (let [mutable-store (new-mutable-store s)
        query-R (new-reporter :value '(nil (nil ("c1" :label))))
        count-R (match-count-R query-R :top-level mutable-store)
        cd (new-calculator-data (new-priority-task-queue 0))]
    (request count-R cd)
    (compute cd)
    (is (= (reporter-value count-R) 2))
    (set-value! query-R '(nil (2 ("c2" :label))))
    (compute cd)
    (is (= (reporter-value count-R) 1))
    (store-reset! mutable-store (new-element-store))
    (compute cd)
    (is (= (reporter-value count-R) 0))))

(deftest render-batch-count-DOM-test
  (let [dom (run-renderer (second (batch-count-component q1)) s)]
    (is (check dom
               [:div {:class "batch-query-match-counts"}
                "2 row matches.  1 table header matches."]))))


(deftest get-batch-edit-query-element-action-data-test
  (let [q1-entity (description->entity q1 s)
        q1-element (first (matching-elements '(nil "c1") q1-entity))
        action-data (get-batch-edit-query-element-action-data
                     {:relative-id (:item-id q1-element)}
                     {} nil s
                     (description->entity q1 s) (description->entity stk1 s))
        target-ids (:target-ids action-data)]
    (is (= (set (map #(id->subject s %) target-ids))
           #{h1 r1 r2 q1 stk1}))
    (doseq [id target-ids]
      (is (extended-by? '(nil ("c1" :label)) (description->entity id s)))))
  ;; Query 2 requires two elements: 2 and one with (nil ("c1" :label))
  ;; It's '(nil "c1") matches in the first row, query 2, itself, and
  ;; the stack selector (because the match in the stack selector does
  ;; not require the entire query matching..
  (let [q2-entity (description->entity q2 s)
        q2-element (first (matching-elements '(nil "c1") q2-entity))
        action-data (get-batch-edit-query-element-action-data
                     {:relative-id (:item-id q2-element)}
                     {} nil s
                     (description->entity q2 s) (description->entity stk1 s))
        target-ids (:target-ids action-data)]
    (is (= (set (map #(id->subject s %) target-ids))
           #{r1 q2 stk1}))
    (doseq [id target-ids]
      (is (extended-by? '(nil ("c1" :label)) (description->entity id s))))))

(deftest batch-query-DOM-test
  (let [q2-entity (description->entity q2 s)
        stk1-entity (description->entity stk1 s)
        q2-2 (:item-id (first (matching-elements 2 q2-entity)))
        q2-c1-entity (first (matching-elements '(nil "c1") q2-entity))
        q2-c1 (:item-id q2-c1-entity)
        q2-c1-l (:item-id (first (matching-elements "c1" q2-c1-entity)))
        dom (batch-query-DOM q2-entity stk1-entity)]
    (is (check
         dom
         ;; TODO: !!! The action data for the virtuals is all wrong. There needs
         ;;       to be an overall component, which sets the target
         ;;       ids to everything the query matches. Then the virtuals
         ;;       will get the right action data.
         [:div {:class "horizontal-labels-element label"}
          ;; A virtual label, because we are required to show some label.
          [:component
           {:relative-id :virtual-label
            :template '(anything :label)
            :get-action-data [(comp-AD) [(batch-query-AD)
                                         q2-entity stk1-entity]
                              [(virt-AD) {:template '(anything :label)
                                          :position :after}]]
            :class "label"
            :render-dom (virt-DOM)
            :get-rendering-data (virt-RD)}]
          [:div {:class "horizontal-stack"}
           [:div {:class "horizontal-stack"}
            [:div {:class "vertical-labels-element label virtual-wrapper"}
             [:component {:template '("" :label)
                          :relative-id [q2-2 :virtual-label]
                          :get-action-data [(comp-AD)
                                            ;; TODO: !!! This sub
                                            ;; action data is wrong.
                                            ;; It should be
                                            ;; batch-query-AD.
                                            [(mult-items-AD)
                                             (list q2-2) (item-AD)]
                                            [(virt-AD) {:template '("" :label)}]]
                          :render-dom (virt-DOM)
                          :get-rendering-data (virt-RD)
                          :class "label"}]
             [:component {:relative-id q2-2
                          :template 'anything
                          :get-action-data [(batch-query-AD)
                                            q2-entity stk1-entity]}]]
            [:div {:class "wrapped-element label"}
             [:component {:template '("" :label)
                          :get-action-data [(comp-AD)
                                            ;; TODO: !!! This sub
                                            ;; action data is wrong.
                                            ;; It should be
                                            ;; batch-query-AD
                                            [(mult-items-AD)
                                             (list q2-c1) (item-AD)]
                                            (item-AD)]
                          :class "label"
                          :excluded-element-ids [(any)]
                          :relative-id q2-c1-l}]
             [:div {:class "indent-wrapper"}
              [:component {:relative-id q2-c1
                           :template '(anything ("c1" :label))
                           :get-action-data [(batch-query-AD)
                                             q2-entity stk1-entity]
                           :excluded-element-ids [q2-c1-l]}]]]]
           ;; A virtual element that is not a label.
           [:div {:class "vertical-labels-element label"}
            [:component {:template '(anything :label)
                         :relative-id :virtual-label
                         :class "label"
                         :render-dom (virt-DOM)
                         :get-rendering-data (virt-RD)
                         :get-action-data [(virt-AD) {:template '(anything
                                                                  :label)
                                                      :position :after}]}]
            [:component {:relative-id :virtual
                         :render-dom (virt-DOM)
                         :get-rendering-data (virt-RD)
                         :get-action-data [(virt-AD)
                                           {:template
                                            '(anything
                                              (anything :label))}]}]]]]))))

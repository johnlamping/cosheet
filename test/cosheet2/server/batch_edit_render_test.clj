(ns cosheet2.server.batch-edit-render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet2
             [orderable :as orderable]
             [reporter :refer [new-reporter set-value! reporter-value]]
             [task-queue :refer [new-priority-task-queue]]
             [calculator :refer [new-calculator-data request compute]]
             [store :refer [new-element-store new-mutable-store store-reset!
                            id->target]]
             store-impl
             mutable-store-impl
             [store-utils :refer [add-element]]
             [query :refer [matching-items matching-elements not-query
                            extended-by?]]
             [entity :as entity  :refer [id->entity
                                         label->elements elements]]
             [expression :refer [expr-let]]
             [debug :refer [simplify-for-print]]
             entity-impl
             [test-utils :refer [check any as-set]])
            (cosheet2.server
             [item-render :refer [render-item-DOM
                                  render-virtual-DOM
                                  get-virtual-DOM-rendering-data]]
             [action-data :refer [default-get-action-data
                                  get-pass-through-action-data
                                  get-virtual-action-data
                                  composed-get-action-data
                                  parallel-items-get-action-data
                                  get-item-or-exemplar-action-data]]
             [order-utils :refer [ordered-entities add-order-elements]]
             [render-utils :refer [ensure-label-object
                                   make-virtual-label-template
                                   make-sequential-template
                                   make-placeholder-object-template
                                   make-object-reference-template]]
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

(def label-object-template (ensure-label-object 'anything))
(def label-template `(~label-object-template))
(def virtual-label-template (make-virtual-label-template 'anything))

;;; We make functions that abbreviate the common functions that can be
;;; embedded in components.
;;; (We use functions, rather than constants, so this file doesn't have
;;; to be reloaded if any of the files that defines the underlying
;;; functions is reloaded.)

(defn virt-DOM [] render-virtual-DOM)
(defn virt-RD [] get-virtual-DOM-rendering-data)

(defn default-AD [] default-get-action-data)
(defn pass-AD [] get-pass-through-action-data)
(defn virt-AD [] get-virtual-action-data)
(defn item-AD [] get-item-or-exemplar-action-data)
(defn comp-AD [] composed-get-action-data)
(defn parallel-AD [] parallel-items-get-action-data)

(defn batch-virtual-element-AD []
  get-batch-edit-stack-virtual-element-subject-action-data)

(def t0 (add-element (new-element-store) nil
                    (add-order-elements
                     '(:x ("s1" :label)
                          (anything ("c1" :label) :column)
                          (anything ("c2" :label) :column)
                          :row-condition))))
(def h1 (second t0))
(def t1 (add-element (first t0) nil (add-order-elements
                                    '(""
                                      (2 ("c1" :label))
                                      (2 ("c2" :label))
                                      :top-level))))
(def r1 (second t1))
(def t2 (add-element (first t1) nil (add-order-elements
                                    '(""
                                      (2 ("c1" :label))
                                      (3 ("c2" :label))
                                      :top-level))))
(def r2 (second t2))
(def t3 (add-element (first t2) nil (add-order-elements
                                    '(anything (anything ("c1" :label))))))
(def q1 (second t3))
(def t4 (add-element (first t3) nil (add-order-elements
                                    '(anything 2 (anything ("c1" :label))))))
(def q2 (second t4))
(def t5 (add-element (first t4) nil (add-order-elements
                                    '(anything (anything (anything :label))))))
(def q3 (second t5))
(def t6 (add-element (first t5) nil (add-order-elements
                                    '(anything (anything ("c1" :label))))))
(def stk1 (second t6))
(def s (first t6))

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
                "2 row matches.  1 table matches."]))))

(deftest render-batch-query-DOM-test
  (let [q2-entity (id->entity q2 s)
        q2-2 (:item-id (first (matching-elements 2 q2-entity)))
        q2-c1-entity (first (matching-elements '(nil "c1") q2-entity))
        q2-c1 (:item-id q2-c1-entity)
        q2-c1-l (:item-id (first (matching-elements "c1" q2-c1-entity)))
        dom (render-batch-query-DOM {:query-id q2 :stack-id stk1} s)]
    (is (check
         dom
         [:div {:class "horizontal-stack query-condition"}
          [:component {:relative-id q2-2
                       :render-dom render-item-DOM
                       :get-action-data (default-AD)
                       :template 'anything
                       :query-id q2
                       :immutable true
                       :stack-id stk1
                       :width 0.75}]
          [:div {:class "wrapped-element label"}
           [:component
            {:template label-template
             :query-id q2
             :stack-id stk1
             :parallel-ids [q2-c1]
             :render-dom render-item-DOM
             :get-action-data (default-AD)
             :class "label"
             :omit-universal-elements true
             :excluded-element-ids [(any)]
             :relative-id q2-c1-l
             :immutable true
             :width 0.75}]
           [:div {:class "indent-wrapper"}
            [:component {:relative-id q2-c1
                         :template '(anything ("c1" :label))
                         :query-id q2
                         :stack-id stk1
                         :render-dom render-item-DOM
                         :get-action-data (default-AD)
                         :immutable true
                         :excluded-element-ids [q2-c1-l]
                         :width 0.75}]]]]))))

(deftest get-batch-edit-stack-element-action-data-test
  (let [q1-entity (id->entity q1 s)
        q1-element (first (matching-elements '(nil "c1") q1-entity))
        action-data (get-batch-edit-stack-element-action-data
                     {:relative-id (:item-id q1-element)
                      :excluding-ids nil
                      :query-id q1
                      :stack-id stk1}
                     {} nil s)
        subject-ids (:subject-ids action-data)]
    (is (= (count subject-ids) 5))
    (is (= (set (map #(id->target s %) subject-ids))
           #{h1 r1 r2 q1 stk1}))
    (doseq [id subject-ids]
      (is (extended-by? '(nil ("c1" :label)) (id->entity id s)))))
  ;; Query 2 requires two elements: 2 and one with (nil ("c1" :label))
  ;; But as a stack selector, we only require the '(nil "c1") to match.
  (let [q2-entity (id->entity q2 s)
        q2-element (first (matching-elements '(nil "c1") q2-entity))
        action-data (get-batch-edit-stack-element-action-data
                     {:relative-id (:item-id q2-element)
                      :excluding-ids nil
                      :query-id q1
                      :render-dom render-item-DOM
                      :get-action-data (default-AD)
                      :stack-id stk1}
                     {} nil s)
        subject-ids (:subject-ids action-data)]
    (is (= (count subject-ids) 5))
    (is (= (set (map #(id->target s %) subject-ids))
           #{h1 r1 r2 q1 stk1}))
    (doseq [id subject-ids]
      (is (extended-by? '(nil ("c1" :label)) (id->entity id s)))))
  ;; Test excluding ids. Neither of the rows should match, as their
  ;; (nil ("c1" :label)) elements are all have content 2
  (let [q2-entity (id->entity q2 s)
        q2-element (first (matching-elements '(nil "c1") q2-entity))
        q2-2 (first (matching-elements 2 q2-entity))
        action-data (get-batch-edit-stack-element-action-data
                     {:relative-id (:item-id q2-element)
                      :excluding-ids [(:item-id q2-2)]
                      :render-dom render-item-DOM
                      :get-action-data (default-AD)
                      :query-id q1
                      :stack-id stk1}
                     {} nil s)
        subject-ids (:subject-ids action-data)]
    (is (= (count subject-ids) 3))
    (is (= (set (map #(id->target s %) subject-ids))
           #{h1 q1 stk1}))
    (doseq [id subject-ids]
      (is (extended-by? '(nil ("c1" :label)) (id->entity id s)))))
  ;; Test a query that matches multiple elements in some rows.
  (let [q3-entity (id->entity q3 s)
        q3-element (first (matching-elements '(nil (nil :label)) q3-entity))
        action-data (get-batch-edit-stack-element-action-data
                     {:relative-id (:item-id q3-element)
                      :render-dom render-item-DOM
                      :get-action-data (default-AD)
                      :excluding-ids nil
                      :query-id q3
                      :stack-id stk1}
                     {} nil s)
        subject-ids (:subject-ids action-data)]
    (is (= (count subject-ids) 8))
    (is (= (set (map #(id->target s %) subject-ids))
           #{h1 r1 r2 q3 stk1}))
    (doseq [id subject-ids]
      (is (extended-by? '(nil (nil :label)) (id->entity id s))))))

;;; TODO: Test a query element that doesn't match the stack element.
(deftest stack-DOM-test
  (let [stk1-entity (id->entity stk1 s)
        stk1-element (first (matching-elements '(nil "c1") stk1-entity))
        dom (stack-DOM {:query-id q1 :stack-id stk1} s)]
    (let [template (-> dom (nth 4) (nth 2) (nth 1) :template)
          template-sequence (:template-sequence template)
          inner-template (:template (nth template-sequence 1))
          inner-template-sequence (:template-sequence inner-template)]
      (println "XXXX" template-sequence
               "YYYY" inner-template
               "ZZZZ" inner-template-sequence))
    (is (check
         dom
         [:div {:class "horizontal-labeled-element-list batch-stack"}
          [:div {}]
          [:component {:query-id q1
                       :stack-id stk1
                       :get-action-data get-batch-edit-stack-element-action-data
                       :relative-id (:item-id stk1-element)
                       :render-dom render-item-DOM
                       :class "batch-stack label leaf"
                       :width 0.75}]
          [:div {:class "vertical-labels-element label"}
           [:component {:relative-id :stack-virtual-label
                        :query-id q1
                        :stack-id stk1 :class "label"
                        :render-dom (virt-DOM)
                        :get-action-data [(comp-AD)
                                          (batch-virtual-element-AD) 
                                          (virt-AD)]
                        ;; TODO: !!! The first template of the
                        ;; sequence should be an actual object, not a
                        ;; placeholder. The object reference template
                        ;; shouldn't have a sequence inside.
                        :template (make-sequential-template
                                   [(make-placeholder-object-template)
                                    (make-object-reference-template
                                     (make-sequential-template
                                      ['anything
                                       (ensure-label-object 'anything)]))])
                        :position :after
                        :do-not-match-query true}]
           [:component {:relative-id :stack-virtual
                        :query-id q1
                        :stack-id stk1
                        :render-dom (virt-DOM)
                        :get-action-data [(comp-AD)
                                          (batch-virtual-element-AD) 
                                          (virt-AD)]
                        :template '(anything (anything :label))
                        :do-not-match-query true}]]]))))

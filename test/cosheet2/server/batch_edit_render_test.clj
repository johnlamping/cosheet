(ns cosheet2.server.batch-edit-render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet2
             [orderable :as orderable]
             [reporter :refer [new-reporter set-value! reporter-value]]
             [task-queue :refer [new-priority-task-queue]]
             [calculator :refer [new-calculator-data request compute]]
             [store :refer [new-element-store new-mutable-store store-reset!]]
             store-impl
             [store-utils :refer [add-entity]]
             [query :refer [matching-items matching-elements not-query]]
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
                                  get-virtual-action-data]]
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

(defn run-renderer
  "run the renderer on the output of the data getter, thus testing
  that they work together correctly."
  [renderer spec data-getter store]
  (apply renderer spec (map first (data-getter spec store))))

(def t0 (add-entity (new-element-store) nil
                    (add-order-elements
                     '(:x ("s1" :label)
                          (anything ("c1" :label) :column)
                          (anything ("c2" :label) :column)
                          :row-condition))))
(def h1 (second t0))
(def t1 (add-entity (first t0) nil (add-order-elements
                                    '(""
                                      (1 ("c1" :label))
                                      (2 ("c2" :label))
                                      :top-level))))
(def r1 (second t1))
(def t2 (add-entity (first t1) nil (add-order-elements
                                    '(""
                                      (1 ("c1" :label))
                                      (3 ("c2" :label))
                                      :top-level))))
(def r2 (second t2))
(def s (first t2))

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


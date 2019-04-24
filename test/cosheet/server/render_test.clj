(ns cosheet.server.render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [expression :refer [expr expr-let expr-seq]]
             [expression-manager :refer [current-value]]
             [debug :refer [envs-to-list simplify-for-print]]
             [orderable :as orderable]
             [query :refer [matching-elements]]
             entity-impl
             [store :refer [make-id]]
             store-impl
             mutable-store-impl
             [test-utils :refer [check any as-set
                                 let-mutated item->immutable]])
            (cosheet.server
             [referent :refer [item-referent]]
             [render :refer :all])
            ; :reload
            ))

(def orderables (reduce (fn [os _]
                          (vec (concat (pop os)
                                       (orderable/split (peek os) :after))))
                        [orderable/initial]
                        (range 6)))
(def o1 (nth orderables 0))
(def unused-orderable (nth orderables 6))

(deftest key<->string-test
  (let [key [:root (make-id "a")]]
    (is (= (-> key key->string string->key) key))))

(deftest top-level-item-DOM-R-test
  ;; Test a case where the subject is explicitly provided.
  (let [subject-ref (make-id "subject")
        [dom fred] (let-mutated [fred "Fred"]
                     (expr-let [dom (top-level-item-DOM-R
                                     fred (item-referent fred)
                                     {:subject-referent subject-ref})]
                       [dom (item->immutable fred)]))]
    (is (check dom
               [:div {:class "horizontal-tags-element tag virtual-wrapper narrow"}
                (any)
                [:div {:expand {:referent subject-ref}
                       :class "content-text editable item"
                       :target {:referent (item-referent fred)
                                :template ""}
                       :key [(:item-id fred) :content]}
                 "Fred"]])))

  ;; Test a case where the subject has to be computed.
  (let [element-as-list `(39
                          (~o1 :order :non-semantic)
                          ("age" :tag (~o1 :order :non-semantic)))
        element (let-mutated [element element-as-list] element)
        age (first (current-value
                    (matching-elements "age" element)))]
    (expr-let [dom (top-level-item-DOM-R age (item-referent age) {})]
      (is (check dom
                 [:div {:expand {:referent (item-referent element)}
                        :class "content-text editable item tag"
                        :target {:referent (item-referent age)},
                        :key [(item-referent age) :content]}
                  "age"])))))

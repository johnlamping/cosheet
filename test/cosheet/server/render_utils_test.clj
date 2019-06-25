(ns cosheet.server.render-utils-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [store :refer [make-id new-element-store]]
             [entity :as entity  :refer [to-list description->entity]]
             [expression :refer [expr expr-let expr-seq]]
             [canonical :refer [canonicalize-list]]
             [store-utils :refer [add-entity]]
             [debug :refer [envs-to-list simplify-for-print]]
             [test-utils :refer [check any as-set evals-to
                                 let-mutated item->immutable]])
            (cosheet.server
             [render-utils :refer :all]
             [referent :refer [elements-referent exemplar-referent
                               exclusive-elements-referent
                               union-referent]])
            ; :reload
            ))

(deftest condition-satisfiers-R-test
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" "A" ("B" 1) "C")]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil "A")))))
             ["a"]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" "A" ("B" 1) "C")]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil "A" "C")))))
             (as-set ["a" "c"])))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" "A" ("B" 1) "C")]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil "A" "B")))))
             ["a"]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" "A" "A" ("B" 1) "C")]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil "A" "B")))))
             ["a"]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" "A" "A" ("B" 1) "C")]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil "A" "A" "B")))))
             ["a" "a"]))
  (is (check (map canonicalize-list
                  (let-mutated [test '("age" "A" ("B" 1) "C")]
                    (expr-seq map to-list
                              (condition-satisfiers-R test '(nil "A" "A" "B")))))
             ["a"])))

(deftest transform-inherited-attributes-test
  (is (check (transform-inherited-attributes
              {:attributes [{:a 1}
                            [#{:label} {:b 2}]
                            [#{:label :element :recursive} #{:content} {:c 3}]
                            [#{:content} {:d 4}]
                            [#{:element :optional} #{:label} {:e 5}]
                            [#{:label :optional :recursive} #{:label} {:f 6}]
                            [(make-id "a") {:item "yup"}]]
               :x 9}
              :label)
             {:attributes [{:b 2}
                           [#{:label :element :recursive :optional} #{:content}
                            {:c 3}]
                           {:e 5}
                           [#{:label :optional :recursive} #{:label} {:f 6}]
                           {:f 6}
                           [(make-id "a") {:item "yup"}]]
              :x 9})))

(deftest remove-inherited-for-item-test
  (is (check (remove-inherited-for-item
              {:attributes [{:a 1}
                            [#{:label} {:b 2}]
                            [(make-id "a") {:item "yup"}]
                            [(make-id "b") {:item "nope"}]]
               :x 9}
              (description->entity (make-id "a") (new-element-store)))
             {:attributes [{:a 1}
                            [#{:label} {:b 2}]
                            [(make-id "b") {:item "nope"}]]
              :x 9})))

(deftest remove-inherited-attribute=test
  (is (check (remove-inherited-attribute
              {:attributes [{:a 1 :b 2}
                            [#{:label} {:b 5}]
                            [#{:label :element :recursive} #{:content} {:a 3}]]
               :x 9}
              :a)
             {:attributes [{:b 2}
                            [#{:label} {:b 5}]]
              :x 9})))

(deftest split-descriptors-by-currency-test
  (is (check (split-descriptors-by-currency
              [{:a 1 :b 2}
               [#{:label :optional} {:b 5}]
               [#{:label :element :recursive} #{:content} {:a 3}]
               {:c 3}])
             [{:a 1 :b 2 :c 3}
              [[#{:label :optional} {:b 5}]
               [#{:label :element :recursive} #{:content} {:a 3}]]])))

(deftest inherited-attributes-test
  (is (check (inherited-attributes
              {:attributes [{:class "foo"}
                            {:class "bar"}
                            {:other "hi"}
                            [#{:label} {:b 2}]
                            [#{:label :optional} {:more "there"}]
                            [(make-id "a") {:item "yup"}]
                            [(make-id "a") #{:label :optional} {:item "nope"}]
                            [(make-id "b") {:item "nope"}]]
               :x 9}
              (description->entity (make-id "a") (new-element-store)))
             {:class "foo bar"
              :other "hi"
              :more "there"
              :item "yup"})))

(deftest content-attributes-test
  (is (check (content-attributes
              {:attributes [{:class "foo"}
                            {:class "bar"}
                            {:other "hi"}
                            [#{:label} {:b 2}]
                            [#{:content} {:class "baz"}]]
               :x 9})
             {:class "baz"})))

(deftest item-or-content-attributes-test
  (is (check (item-or-content-attributes
              {:attributes [{:class "foo"}
                            {:class "bar"}
                            {:other "hi"}
                            [#{:label} {:b 2}]
                            [#{:content} {:class "baz"}]]
               :x 9})
             {:class "foo bar baz"
              :other "hi"})))

(deftest competing-siblings-test
  (let [[s1 joe-id] (add-entity
                    (new-element-store) nil
                    '("joe"
                      anything (anything 1) (anything 1)
                      (2 1) (2 3)))
        [s2 item-a1-id] (add-entity s1 joe-id '(anything 1))
        [s3 item-b3-id] (add-entity s2 joe-id '("" 3))
        [store item-21-id] (add-entity s3 joe-id '(2 1))
        joe (description->entity joe-id store)
        item-a1 (description->entity item-a1-id store)
        item-b3 (description->entity item-b3-id store)
        item-21 (description->entity item-21-id store)]
    (let [competing (competing-siblings item-a1)]
      (is (check (map entity/to-list competing)
                 (as-set ['(anything 1) '(2 1) '(2 3) '("" 3)])))
      (is (not-any? #(= % item-a1) competing)))
    (let [competing (competing-siblings item-21)]
      (is (check (map entity/to-list competing)
                 (as-set ['(2 1) '(2 3)])))
      (is (not-any? #(= % item-21) competing)))
    (let [competing (competing-siblings item-b3)]
      (is (check (map entity/to-list competing)
                 ['(anything 1)])))))

(deftest item-referent-given-inherited-test
  (let [s0 (new-element-store)
        [s1 joe-id] (add-entity s0 nil "joe")
        [store joe-age-id] (add-entity s1 joe-id '(50 ("age" :tag)))
        joe (description->entity joe-id store)
        joe-age (description->entity joe-age-id store)]
    (is (check (item-referent-given-inherited joe-age {})
               joe-age-id))
    (is (check (item-referent-given-inherited joe-age {:match-multiple :all})
               joe-age-id))
    (is (check (item-referent-given-inherited
                joe-age {:match-multiple :all :subject-referent joe-id})
               (elements-referent joe-age-id joe-id)))
    (is (check (item-referent-given-inherited
                joe-age  {:subject-referent (union-referent [joe-id])})
               (exemplar-referent joe-age-id (union-referent [joe-id]))))
    ;; Check that we go to an exclusive-elements referent if there is a
    ;; sibling that is not just more general.
    (let [[store joe-fake-age-id]
          (add-entity store joe-id '(50 ("age" :tag) ("fake" :tag)))
          ;; A sibling that never competes with the age ones.
          [store _] (add-entity store joe-id "Hi")]
      ;; match-all doesn't care about competitors
      (is (check (item-referent-given-inherited
                  (description->entity joe-age-id store)
                  {:match-multiple :all :subject-referent joe-id})
                 (elements-referent joe-age-id joe-id)))
      ;; match-all-exclusive does care about competitors.
      (is (check (item-referent-given-inherited
                  (description->entity joe-age-id store)
                  {:match-multiple :exclusive :subject-referent joe-id})
                 (exclusive-elements-referent
                  joe-age-id joe-id [joe-fake-age-id])))
      ;; Check that the less specific is not competing with the more specific
      (is (check (item-referent-given-inherited
                  (description->entity joe-fake-age-id store)
                  {:match-multiple :exclusive :subject-referent joe-id})
                 (elements-referent joe-fake-age-id joe-id)))
      ;; Check that the sibling is still competing if neither sibling is
      ;; more specific.
      (let [[store _] (add-entity store joe-age-id '("true" :tag))]
        (is (check (item-referent-given-inherited
                    (description->entity joe-age-id store)
                    {:match-multiple :exclusive :subject-referent joe-id})
                   (exclusive-elements-referent
                  joe-age-id joe-id [joe-fake-age-id]))))))) 


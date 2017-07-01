(ns cosheet.server.hierarchy-test
  (:require [clojure.test :refer [deftest is assert-expr do-report]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [utils :refer [multiset-diff]]
                     [test-utils :refer [check let-mutated]]
                     [canonical :refer [canonicalize-list]])
            (cosheet.server [hierarchy :refer :all])
             ; :reload
            ))

(deftest append-to-hierarchy-test
  ;; Append to empty.
  (is (check (append-to-hierarchy [] :i {:a 1} {})
             [{:hierarchy-node true
               :members [:i]
               :properties {:a 1}
               :cumulative-properties {:a 1}}]))
  ;; Identical properties.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :members [:i]
                                    :properties {:a 1}
                                    :cumulative-properties {:a 1}}]
                                  :j {:a 1} {})
             [{:hierarchy-node true
               :members [:i :j]
               :properties {:a 1}
               :cumulative-properties {:a 1}}]))
  ;; Completely different properties.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1}
                                    :cumulative-properties {:a 1}
                                    :members [:i]}]
                                  :j {:b 1} {})
             [{:hierarchy-node true
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :members [:i]}
              {:hierarchy-node true
               :properties {:b 1}
               :cumulative-properties {:b 1}
               :members [:j]}]))
  ;; New element has added group.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :members [:i]
                                    :properties {:a 1}
                                    :cumulative-properties {:a 1}}]
                                  :j {:a 1 :b 1} {})
             [{:hierarchy-node true
               :members [:i]
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :child-nodes [{:hierarchy-node true
                              :properties {:b 1}
                              :cumulative-properties {:a 1 :b 1}
                              :members [:j]}]}]))
  ;; New element has fewer properties.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1 :b 1}
                                    :cumulative-properties {:a 1 :b 1}
                                    :members [:i]}]
                                  :j {:a 1} {})
             [{:hierarchy-node true
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :members []
               :child-nodes [{:hierarchy-node true
                              :properties {:b 1}
                              :cumulative-properties {:a 1 :b 1}
                              :members [:i]}
                             {:hierarchy-node true
                              :properties {}
                              :cumulative-properties {:a 1}
                              :members [:j]}]}]))
  ;; Old and new have some in common.

  (is (check
       (append-to-hierarchy [{:hierarchy-node true
                              :properties {:a 1 :b 1}
                              :cumulative-properties {:a 1 :b 1 :d 2}
                              :members [:i]}]
                            :j {:a 1 :c 1} {:d 2})
       [{:hierarchy-node true
         :properties {:a 1}
         :cumulative-properties {:a 1 :d 2}
         :members []
         :child-nodes [{:hierarchy-node true
                        :properties {:b 1}
                        :cumulative-properties {:a 1 :b 1 :d 2}
                        :members [:i]}
                       {:hierarchy-node true
                        :properties {:c 1}
                        :cumulative-properties {:a 1 :c 1 :d 2}
                        :members [:j]}]}]))
  ;; Must add to child of last element.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1}
                                    :cumulative-properties {:a 1}
                                    :members [:i]
                                    :child-nodes [{:hierarchy-node true
                                                   :properties {:b 1}
                                                   :cumulative-properties {:a 1
                                                                           :b 1}
                                                   :members [:j]}]}]
                                  :k {:a 1 :b 1 :c 1} {})
             [{:hierarchy-node true
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :members [:i]
               :child-nodes [{:hierarchy-node true
                              :properties {:b 1}
                              :cumulative-properties {:a 1 :b 1}
                              :members [:j]
                              :child-nodes [{:hierarchy-node true
                                             :properties {:c 1}
                                             :cumulative-properties {:a 1 :b 1
                                                                     :c 1}
                                             :members [:k]}]}]}]))
  ;; Must close off child of last element.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1}
                                    :cumulative-properties {:a 1}
                                    :members [:i]
                                    :child-nodes [{:hierarchy-node true
                                                   :properties {:b 1}
                                                   :cumulative-properties {:a 1
                                                                           :b 1}
                                                   :members [:j]}]}]
                                  :k {:a 1} {})
             [{:hierarchy-node true
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :members [:i]
               :child-nodes [{:hierarchy-node true
                              :properties {:b 1}
                              :cumulative-properties {:a 1 :b 1}
                              :members [:j]}
                             {:hierarchy-node true
                              :properties {}
                              :cumulative-properties {:a 1}
                              :members [:k]}]}]))
  ;; Partial sharing with last element that has children.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1 :b 1}
                                    :cumulative-properties {:a 1 :b 1}
                                    :members [:i]
                                    :child-nodes [{:properties {:c 1}
                                                   :cumulative-properties
                                                   {:a 1 :b 1 :c 1}
                                                   :members [:j]}]}]
                                  :k {:a 1 :c 1} {})
             [{:hierarchy-node true
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :members []
               :child-nodes [{:hierarchy-node true
                              :properties {:b 1}
                              :cumulative-properties {:a 1 :b 1}
                              :members [:i]
                              :child-nodes [{:properties {:c 1}
                                             :cumulative-properties {:a 1 :b 1
                                                                     :c 1}
                                             :members [:j]}]}
                             {:hierarchy-node true
                              :properties {:c 1}
                              :cumulative-properties {:a 1 :c 1}
                              :members [:k]}]}]))
  ;; Append two empty properties.
  (is (check
       (append-to-hierarchy [{:hierarchy-node true
                              :properties {}
                              :cumulative-properties {}
                              :members [:i]}]
                            :j {} {})
       [{:hierarchy-node true
         :cumulative-properties {}
         :properties {}
         :members [:i]}
        {:hierarchy-node true
         :properties {}
         :cumulative-properties {}
         :members [:j]}]))
  ;; Append empty group after empty group while nested.
  (is (check
       (append-to-hierarchy [{:hierarchy-node true
                              :properties {:a 1 :b 1}
                              :cumulative-properties {:a 1 :b 1}
                              :members [:i]
                              :child-nodes [{:hierarchy-node true
                                             :properties {:c 1}
                                             :cumulative-properties
                                             {:a 1 :b 1 :c 1}
                                             :members [:j]}
                                            {:hierarchy-node true
                                             :properties {}
                                             :cumulative-properties {:a 1 :b 1}
                                             :members [:k]}]}]
                            :l {:a 1 :b 1} {})
       [{:hierarchy-node true
         :properties {:a 1 :b 1}
         :cumulative-properties {:a 1 :b 1}
         :members [:i]
         :child-nodes [{:hierarchy-node true
                        :properties {:c 1}
                        :cumulative-properties {:a 1 :b 1 :c 1}
                        :members [:j]}
                       {:hierarchy-node true
                        :properties {}
                        :cumulative-properties {:a 1 :b 1}
                        :members [:k :l]}]}]))
  ;; Append non-empty group after empty group.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1 :b 1}
                                    :cumulative-properties {:a 1 :b 1}
                                    :members [:i]
                                    :child-nodes [{:hierarchy-node true
                                                   :properties {:c 1}
                                                   :cumulative-properties
                                                   {:a 1 :b 1 :c 1}
                                                   :members [:j]}
                                                  {:hierarchy-node true
                                                   :properties {}
                                                   :cumulative-properties
                                                   {:a 1 :b 1}
                                                   :members [:k]}]}]
                                  :l {:a 1 :b 1 :c 1} {})
             [{:hierarchy-node true
               :properties {:a 1 :b 1}
               :cumulative-properties {:a 1 :b 1}
               :members [:i]
               :child-nodes [{:hierarchy-node true
                              :properties {:c 1}
                              :cumulative-properties {:a 1 :b 1 :c 1}
                              :members [:j]}
                             {:hierarchy-node true
                              :properties {}
                              :cumulative-properties {:a 1 :b 1}
                              :members [:k]}
                             {:hierarchy-node true
                              :properties {:c 1}
                              :cumulative-properties {:a 1 :b 1 :c 1}
                              :members [:l]}]
               }])))

(deftest hierarchy-node-descendants-test
  (is (check (set (hierarchy-node-descendants
                   {:hierarchy-node true
                    :properties {:a 1}
                    :members [:i]
                    :child-nodes [{:hierarchy-node true
                                   :properties {:b 1}
                                   :members [:j]}]}))
             #{:i :j}))
  (is (check (hierarchy-node-descendants :foo)
             [:foo])))

(deftest hierarchy-node-next-level-test
  (is (check (hierarchy-node-next-level
              {:hierarchy-node true
               :properties {:a 1 :b 1}
               :members [:i]
               :child-nodes [{:hierarchy-node true
                              :properties {:c 1}
                              :members [:j :k]}
                             {:hierarchy-node true
                              :properties {}
                              :members [:l :m ]}]})
             [:i
              {:hierarchy-node true :properties {:c 1} :members [:j :k]}
              :l :m]))
  (is (check (hierarchy-node-next-level :foo)
             [:foo])))

(def jane-list `("Jane" (1 :order :non-semantic) "plain" "plain"))
(def joe-list `("Joe"
               (2 :order :non-semantic)
               ("male" (1 :order :non-semantic))
               ("married" (2 :order :non-semantic))
               (39 (3 :order :non-semantic)
                   ("age" :tag)
                   ("doubtful" "confidence"))
               (45 (4 :order :non-semantic)
                   ("age" :tag))))

(deftest canonical-info-set-test
  (is (= (let-mutated [joe joe-list, jane jane-list]
           (canonical-info-set [joe jane]))
         {["joe"
           {"married" 1
            "male" 1
            [39 {["age" {:tag 1}] 1, ["doubtful" {"confidence" 1}] 1}] 1
            [45 {["age" {:tag 1}] 1}] 1}]1
            ["jane" {"plain" 2}] 1})))

(deftest hierarchy-by-canonical-info-test
  (is (check
       (hierarchy-by-canonical-info
        [{:property-canonicals [:a] :item `(:i (1 :order :non-semantic))}
         {:property-canonicals [:a :b] :item `(:j (2 :order :non-semantic))}
         {:property-canonicals [:a :c] :item `(:k (3 :order :non-semantic))}])
       [{:hierarchy-node true
         :properties {:a 1}
         :cumulative-properties {:a 1}
         :members [{:property-canonicals [:a]
                    :item `(:i (1 :order :non-semantic))}]
         :child-nodes [{:hierarchy-node true
                        :properties {:b 1}
                        :cumulative-properties {:b 1 :a 1}
                        :members [{:property-canonicals [:a :b]
                                   :item `(:j (2 :order :non-semantic))}]}
                       {:hierarchy-node true
                        :properties {:c 1}
                        :cumulative-properties {:c 1 :a 1}
                        :members [{:property-canonicals [:a :c]
                                   :item `(:k (3 :order :non-semantic))}]}]}])))



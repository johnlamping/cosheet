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
               :leaves [:i]
               :properties {:a 1}
               :cumulative-properties {:a 1}}]))
  ;; Identical properties.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :leaves [:i]
                                    :properties {:a 1}
                                    :cumulative-properties {:a 1}}]
                                  :j {:a 1} {})
             [{:hierarchy-node true
               :leaves [:i :j]
               :properties {:a 1}
               :cumulative-properties {:a 1}}]))
  ;; Completely different properties.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1}
                                    :cumulative-properties {:a 1}
                                    :leaves [:i]}]
                                  :j {:b 1} {})
             [{:hierarchy-node true
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :leaves [:i]}
              {:hierarchy-node true
               :properties {:b 1}
               :cumulative-properties {:b 1}
               :leaves [:j]}]))
  ;; New element has added group.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :leaves [:i]
                                    :properties {:a 1}
                                    :cumulative-properties {:a 1}}]
                                  :j {:a 1 :b 1} {})
             [{:hierarchy-node true
               :leaves [:i]
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :child-nodes [{:hierarchy-node true
                              :properties {:b 1}
                              :cumulative-properties {:a 1 :b 1}
                              :leaves [:j]}]}]))
  ;; New element has fewer properties.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1 :b 1}
                                    :cumulative-properties {:a 1 :b 1}
                                    :leaves [:i]}]
                                  :j {:a 1} {})
             [{:hierarchy-node true
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :leaves []
               :child-nodes [{:hierarchy-node true
                              :properties {:b 1}
                              :cumulative-properties {:a 1 :b 1}
                              :leaves [:i]}
                             {:hierarchy-node true
                              :properties {}
                              :cumulative-properties {:a 1}
                              :leaves [:j]}]}]))
  ;; Old and new have some in common.

  (is (check
       (append-to-hierarchy [{:hierarchy-node true
                              :properties {:a 1 :b 1}
                              :cumulative-properties {:a 1 :b 1 :d 2}
                              :leaves [:i]}]
                            :j {:a 1 :c 1} {:d 2})
       [{:hierarchy-node true
         :properties {:a 1}
         :cumulative-properties {:a 1 :d 2}
         :leaves []
         :child-nodes [{:hierarchy-node true
                        :properties {:b 1}
                        :cumulative-properties {:a 1 :b 1 :d 2}
                        :leaves [:i]}
                       {:hierarchy-node true
                        :properties {:c 1}
                        :cumulative-properties {:a 1 :c 1 :d 2}
                        :leaves [:j]}]}]))
  ;; Must add to child of last element.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1}
                                    :cumulative-properties {:a 1}
                                    :leaves [:i]
                                    :child-nodes [{:hierarchy-node true
                                                   :properties {:b 1}
                                                   :cumulative-properties {:a 1
                                                                           :b 1}
                                                   :leaves [:j]}]}]
                                  :k {:a 1 :b 1 :c 1} {})
             [{:hierarchy-node true
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :leaves [:i]
               :child-nodes [{:hierarchy-node true
                              :properties {:b 1}
                              :cumulative-properties {:a 1 :b 1}
                              :leaves [:j]
                              :child-nodes [{:hierarchy-node true
                                             :properties {:c 1}
                                             :cumulative-properties {:a 1 :b 1
                                                                     :c 1}
                                             :leaves [:k]}]}]}]))
  ;; Must close off child of last element.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1}
                                    :cumulative-properties {:a 1}
                                    :leaves [:i]
                                    :child-nodes [{:hierarchy-node true
                                                   :properties {:b 1}
                                                   :cumulative-properties {:a 1
                                                                           :b 1}
                                                   :leaves [:j]}]}]
                                  :k {:a 1} {})
             [{:hierarchy-node true
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :leaves [:i]
               :child-nodes [{:hierarchy-node true
                              :properties {:b 1}
                              :cumulative-properties {:a 1 :b 1}
                              :leaves [:j]}
                             {:hierarchy-node true
                              :properties {}
                              :cumulative-properties {:a 1}
                              :leaves [:k]}]}]))
  ;; Partial sharing with last element that has children.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1 :b 1}
                                    :cumulative-properties {:a 1 :b 1}
                                    :leaves [:i]
                                    :child-nodes [{:properties {:c 1}
                                                   :cumulative-properties
                                                   {:a 1 :b 1 :c 1}
                                                   :leaves [:j]}]}]
                                  :k {:a 1 :c 1} {})
             [{:hierarchy-node true
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :leaves []
               :child-nodes [{:hierarchy-node true
                              :properties {:b 1}
                              :cumulative-properties {:a 1 :b 1}
                              :leaves [:i]
                              :child-nodes [{:properties {:c 1}
                                             :cumulative-properties {:a 1 :b 1
                                                                     :c 1}
                                             :leaves [:j]}]}
                             {:hierarchy-node true
                              :properties {:c 1}
                              :cumulative-properties {:a 1 :c 1}
                              :leaves [:k]}]}]))
  ;; Append two empty properties.
  (is (check
       (append-to-hierarchy [{:hierarchy-node true
                              :properties {}
                              :cumulative-properties {}
                              :leaves [:i]}]
                            :j {} {})
       [{:hierarchy-node true
         :cumulative-properties {}
         :properties {}
         :leaves [:i]}
        {:hierarchy-node true
         :properties {}
         :cumulative-properties {}
         :leaves [:j]}]))
  ;; Append empty group after empty group while nested.
  (is (check
       (append-to-hierarchy [{:hierarchy-node true
                              :properties {:a 1 :b 1}
                              :cumulative-properties {:a 1 :b 1}
                              :leaves [:i]
                              :child-nodes [{:hierarchy-node true
                                             :properties {:c 1}
                                             :cumulative-properties
                                             {:a 1 :b 1 :c 1}
                                             :leaves [:j]}
                                            {:hierarchy-node true
                                             :properties {}
                                             :cumulative-properties {:a 1 :b 1}
                                             :leaves [:k]}]}]
                            :l {:a 1 :b 1} {})
       [{:hierarchy-node true
         :properties {:a 1 :b 1}
         :cumulative-properties {:a 1 :b 1}
         :leaves [:i]
         :child-nodes [{:hierarchy-node true
                        :properties {:c 1}
                        :cumulative-properties {:a 1 :b 1 :c 1}
                        :leaves [:j]}
                       {:hierarchy-node true
                        :properties {}
                        :cumulative-properties {:a 1 :b 1}
                        :leaves [:k :l]}]}]))
  ;; Append non-empty group after empty group.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1 :b 1}
                                    :cumulative-properties {:a 1 :b 1}
                                    :leaves [:i]
                                    :child-nodes [{:hierarchy-node true
                                                   :properties {:c 1}
                                                   :cumulative-properties
                                                   {:a 1 :b 1 :c 1}
                                                   :leaves [:j]}
                                                  {:hierarchy-node true
                                                   :properties {}
                                                   :cumulative-properties
                                                   {:a 1 :b 1}
                                                   :leaves [:k]}]}]
                                  :l {:a 1 :b 1 :c 1} {})
             [{:hierarchy-node true
               :properties {:a 1 :b 1}
               :cumulative-properties {:a 1 :b 1}
               :leaves [:i]
               :child-nodes [{:hierarchy-node true
                              :properties {:c 1}
                              :cumulative-properties {:a 1 :b 1 :c 1}
                              :leaves [:j]}
                             {:hierarchy-node true
                              :properties {}
                              :cumulative-properties {:a 1 :b 1}
                              :leaves [:k]}
                             {:hierarchy-node true
                              :properties {:c 1}
                              :cumulative-properties {:a 1 :b 1 :c 1}
                              :leaves [:l]}]
               }])))

(deftest replace-hierarchy-leaves-by-nodes-test
  (let [hierarchy
        [{:hierarchy-node true
          :properties {:a 1 :b 1}
          :cumulative-properties {:a 1 :b 1}
          :leaves [:i]
          :child-nodes [;; Only one leaf, and no children
                        {:hierarchy-node true
                         :properties {:c 1}
                         :cumulative-properties {:a 1 :b 1 :c 1}
                         :leaves [:j]}
                        ;; Children with no properties
                        {:hierarchy-node true
                         :properties {:d 1}
                         :cumulative-properties {:a 1 :b 1 :d 1}
                         :leaves [:k]
                         :child-nodes [{:hierarchy-node true
                                        :properties {:e 1}
                                        :cumulative-properties {:a 1 :b 1
                                                                :d 1 :e 1}
                                        :leaves [:j]}
                                       {:hierarchy-node true
                                        :properties {}
                                        :cumulative-properties {:a 1 :b 1 :d 1}
                                        :leaves [:l :m]}]}
                        ;; A child with no leaves.
                        {:hierarchy-node true
                         :properties {:e 1}
                         :cumulative-properties {:a 1 :b 1 :e 1}
                         :child-nodes [{:hierarchy-node true
                                        :properties {:f 1}
                                        :cumulative-properties {:a 1 :b 1
                                                                :d 1 :f 1}
                                        :leaves [:n]}
                                       {:hierarchy-node true
                                        :properties {:g 1}
                                        :cumulative-properties {:a 1 :b 1
                                                                :d 1 :g 1}
                                        :leaves [:q :r]}]}]}]]
    (is (check
         (replace-hierarchy-leaves-by-nodes hierarchy)
         [{:hierarchy-node true
           :properties {:a 1 :b 1}
           :cumulative-properties {:a 1 :b 1}
           :child-nodes [{:hierarchy-node true
                          :leaves [:i]
                          :cumulative-properties {:a 1 :b 1}}
                         {:hierarchy-node true
                          :properties {:c 1}
                          :cumulative-properties {:a 1 :b 1 :c 1}
                          :leaves [:j]}
                         {:hierarchy-node true
                          :properties {:d 1}
                          :cumulative-properties {:a 1 :b 1 :d 1}
                          :child-nodes
                          [{:hierarchy-node true
                            :leaves [:k]
                            :cumulative-properties {:a 1 :b 1 :d 1}}
                           {:hierarchy-node true
                            :properties {:e 1}
                            :cumulative-properties {:a 1 :b 1 :d 1 :e 1}
                            :leaves [:j]}
                           {:hierarchy-node true
                            :leaves [:l]
                            :cumulative-properties {:a 1 :b 1 :d 1}}
                           {:hierarchy-node true
                            :leaves [:m]
                            :cumulative-properties {:a 1 :b 1 :d 1}}]}
                         {:hierarchy-node true
                          :properties {:e 1}
                          :cumulative-properties {:a 1 :b 1 :e 1}
                          :child-nodes
                          [{:hierarchy-node true
                            :properties {:f 1}
                            :cumulative-properties {:a 1 :b 1 :d 1 :f 1}
                            :leaves [:n]}
                           {:hierarchy-node true
                            :properties {:g 1}
                            :cumulative-properties {:a 1 :b 1 :d 1 :g 1}
                            :child-nodes
                            [{:hierarchy-node true
                              :leaves [:q]
                              :cumulative-properties {:a 1 :b 1 :d 1 :g 1}}
                             {:hierarchy-node true
                              :leaves [:r]
                              :cumulative-properties {:a 1 :b 1 :d 1 :g 1}}
                             ]}]}]}]))))

(deftest hierarchy-node-descendants-test
  (is (check (set (hierarchy-node-descendants
                   {:hierarchy-node true
                    :properties {:a 1}
                    :leaves [:i]
                    :child-nodes [{:hierarchy-node true
                                   :properties {:b 1}
                                   :leaves [:j]}]}))
             #{:i :j}))
  (is (check (hierarchy-node-descendants :foo)
             [:foo])))

(deftest hierarchy-node-next-level-test
  (is (check (hierarchy-node-next-level
              {:hierarchy-node true
               :properties {:a 1 :b 1}
               :leaves [:i]
               :child-nodes [{:hierarchy-node true
                              :properties {:c 1}
                              :leaves [:j :k]}
                             {:hierarchy-node true
                              :properties {}
                              :leaves [:l :m ]}]})
             [:i
              {:hierarchy-node true :properties {:c 1} :leaves [:j :k]}
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

(deftest hierarchy-by-canonical-info-test
  (is (check
       (hierarchy-by-canonical-info
        [{:property-canonicals [:a] :item `(:i (1 :order :non-semantic))}
         {:property-canonicals [:a :b] :item `(:j (2 :order :non-semantic))}
         {:property-canonicals [:a :c] :item `(:k (3 :order :non-semantic))}])
       [{:hierarchy-node true
         :properties {:a 1}
         :cumulative-properties {:a 1}
         :leaves [{:property-canonicals [:a]
                    :item `(:i (1 :order :non-semantic))}]
         :child-nodes [{:hierarchy-node true
                        :properties {:b 1}
                        :cumulative-properties {:b 1 :a 1}
                        :leaves [{:property-canonicals [:a :b]
                                   :item `(:j (2 :order :non-semantic))}]}
                       {:hierarchy-node true
                        :properties {:c 1}
                        :cumulative-properties {:c 1 :a 1}
                        :leaves [{:property-canonicals [:a :c]
                                   :item `(:k (3 :order :non-semantic))}]}]}])))



(ns cosheet.server.hierarchy-test
  (:require [clojure.test :refer [deftest is assert-expr do-report]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [utils :refer [multiset-diff]]
                     [test-utils :refer [check let-mutated]]
                     [orderable :as orderable])
            (cosheet.server
             [hierarchy :refer :all]
             [key :refer [canonicalize-list]])
             ; :reload
            ))

(deftest append-to-hierarchy-test
  ;; Append to empty.
  (is (check (append-to-hierarchy [] :i {:a 1})
             [{:hierarchy-node true
               :members [:i]
               :properties {:a 1}
               :cumulative-properties {:a 1}}]))
  ;; Identical properties.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :members [:i]
                                    :properties {:a 1}
                                    :cumulative-properties {:a 1}}]
                                  :j {:a 1})
             [{:hierarchy-node true
               :members [:i :j]
               :properties {:a 1}
               :cumulative-properties {:a 1}}]))
  ;; Completely different properties.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1}
                                    :cumulative-properties {:a 1}
                                    :members [:i]}]
                                  :j {:b 1})
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
                                  :j {:a 1 :b 1})
             [{:hierarchy-node true
               :members [:i]
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :children [{:hierarchy-node true
                           :properties {:b 1}
                           :cumulative-properties {:a 1 :b 1}
                           :members [:j]}]}]))
  ;; New element has fewer properties.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1 :b 1}
                                    :cumulative-properties {:a 1 :b 1}
                                    :members [:i]}]
                                  :j {:a 1})
             [{:hierarchy-node true
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :members []
               :children [{:hierarchy-node true
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
         :children [{:hierarchy-node true
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
                                    :children [{:hierarchy-node true
                                                :properties {:b 1}
                                                :cumulative-properties {:a 1
                                                                        :b 1}
                                                :members [:j]}]}]
                                  :k {:a 1 :b 1 :c 1})
             [{:hierarchy-node true
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :members [:i]
               :children [{:hierarchy-node true
                           :properties {:b 1}
                           :cumulative-properties {:a 1 :b 1}
                           :members [:j]
                           :children [{:hierarchy-node true
                                       :properties {:c 1}
                                       :cumulative-properties {:a 1 :b 1 :c 1}
                                       :members [:k]}]}]}]))
  ;; Must close off child of last element.
  (is (check (append-to-hierarchy [{:hierarchy-node true
                                    :properties {:a 1}
                                    :cumulative-properties {:a 1}
                                    :members [:i]
                                    :children [{:hierarchy-node true
                                                :properties {:b 1}
                                                :cumulative-properties {:a 1
                                                                        :b 1}
                                                :members [:j]}]}]
                                  :k {:a 1})
             [{:hierarchy-node true
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :members [:i]
               :children [{:hierarchy-node true
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
                                    :children [{:properties {:c 1}
                                                :cumulative-properties
                                                {:a 1 :b 1 :c 1}
                                                :members [:j]}]}]
                                  :k {:a 1 :c 1})
             [{:hierarchy-node true
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :members []
               :children [{:hierarchy-node true
                           :properties {:b 1}
                           :cumulative-properties {:a 1 :b 1}
                           :members [:i]
                           :children [{:properties {:c 1}
                                       :cumulative-properties {:a 1 :b 1 :c 1}
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
                            :j {})
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
                              :children [{:hierarchy-node true
                                          :properties {:c 1}
                                          :cumulative-properties
                                          {:a 1 :b 1 :c 1}
                                          :members [:j]}
                                         {:hierarchy-node true
                                          :properties {}
                                          :cumulative-properties {:a 1 :b 1}
                                          :members [:k]}]}]
                            :l {:a 1 :b 1})
       [{:hierarchy-node true
         :properties {:a 1 :b 1}
         :cumulative-properties {:a 1 :b 1}
         :members [:i]
         :children [{:hierarchy-node true
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
                                    :children [{:hierarchy-node true
                                                :properties {:c 1}
                                                :cumulative-properties
                                                {:a 1 :b 1 :c 1}
                                                :members [:j]}
                                               {:hierarchy-node true
                                                :properties {}
                                                :cumulative-properties
                                                {:a 1 :b 1}
                                                :members [:k]}]}]
                                  :l {:a 1 :b 1 :c 1})
             [{:hierarchy-node true
               :properties {:a 1 :b 1}
               :cumulative-properties {:a 1 :b 1}
               :members [:i]
               :children [{:hierarchy-node true
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
                    :children [{:properties {:b 1} :members [:j]}]}))
             #{:i :j})))

(deftest hierarchy-node-next-level-test
  (is (check (hierarchy-node-next-level
              {:hierarchy-node true
               :properties {:a 1 :b 1}
               :members [:i]
               :children [{:hierarchy-node true :properties {:c 1} :members [:j :k]}
                          {:hierarchy-node true :properties {} :members [:l :m ]}]})
             [:i
              {:hierarchy-node true :properties {:c 1} :members [:j :k]}
              :l :m])))

(deftest flatten-hierarchy-test
  (is (check (flatten-hierarchy
              [{:hierarchy-node true
                :properties {:a 1}
                :cumulative-properties {:a 1}
                :members [:i]
                :children [{:hierarchy-node true
                            :properties {:b 1}
                            :cumulative-properties {:a 1 :b 1}
                            :members [:j]
                            :children [{:hierarchy-node true
                                        :properties {:c 1}
                                        :cumulative-properties {:a 1 :b 1 :c 1}
                                        :members [:l]}]}
                           {:hierarchy-node true
                            :properties {:c 1}
                            :cumulative-properties {:a 1 :c 1}
                            :members [:k]}]}]
              0)
             [{:hierarchy-node true
               :properties {:a 1}
               :cumulative-properties {:a 1}
               :depth 0
               :members [:i]
               :children [{:hierarchy-node true
                           :properties {:b 1}
                           :cumulative-properties {:a 1 :b 1}
                           :members [:j]
                           :children [{:hierarchy-node true
                                       :properties {:c 1}
                                       :cumulative-properties {:a 1 :b 1 :c 1}
                                       :members [:l]}]}
                          {:hierarchy-node true
                           :properties {:c 1}
                           :cumulative-properties {:a 1 :c 1}
                           :members [:k]}]}
              {:hierarchy-node true
               :properties {:b 1}
               :cumulative-properties {:a 1 :b 1}
               :depth 1
               :members [:j]
               :children [{:hierarchy-node true
                           :properties {:c 1}
                           :cumulative-properties {:a 1 :b 1 :c 1}
                           :members [:l]}]}
              {:hierarchy-node true
               :properties {:c 1}
               :cumulative-properties {:a 1 :b 1 :c 1}
               :depth 2 :members [:l]}
              {:hierarchy-node true
               :properties {:c 1}
               :cumulative-properties {:a 1 :c 1}
               :depth 1
               :members [:k]}])))

(def orderables (reduce (fn [os _]
                          (vec (concat (pop os)
                                       (orderable/split (peek os) :after))))
                        [orderable/initial]
                        (range 4)))
(def o1 (nth orderables 0))
(def o2 (nth orderables 1))
(def o3 (nth orderables 2))
(def o4 (nth orderables 3))
(def jane-list `("Jane" (~o1 :order :non-semantic) "plain" "plain"))
(def joe-list `("Joe"
               (~o2 :order :non-semantic)
               ("male" (~o1 :order :non-semantic))
               ("married" (~o2 :order :non-semantic))
               (39 (~o3 :order :non-semantic)
                   ("age" :tag)
                   ("doubtful" "confidence"))
               (45 (~o4 :order :non-semantic)
                   ("age" :tag))))

(deftest canonical-info-set-test
  (is (= (let-mutated [joe joe-list, jane jane-list]
           (canonical-info-set [joe jane]))
         {["Joe"
           {"married" 1
            "male" 1
            [39 {["age" {:tag 1}] 1, ["doubtful" {"confidence" 1}] 1}] 1
            [45 {["age" {:tag 1}] 1}] 1}]1
            ["Jane" {"plain" 2}] 1})))

(deftest canonical-to-list-test
  (let [starting [joe-list jane-list jane-list]
        canonical (canonicalize-list [starting])]
    (is (= (canonicalize-list (canonical-to-list canonical))
           canonical))))

(deftest canonical-info-set-diff-test
  (is (= (multiset-diff {:a 1 :b 3 :c 3} {:b 1 :c 3 :d 4})
         [{:a 1 :b 2} {:d 4} {:b 1 :c 3}])))

(deftest split-by-do-not-merge-subset-test
  (is (check (split-by-do-not-merge-subset
              (map (fn [x] {:item x}) [:i :j :k :l :m :n :o])
              #{:i :l :m :o})
             [[{:item :i}]
              [{:item :j} {:item :k}]
              [{:item :l}]
              [{:item :m}]
              [{:item :n}]
              [{:item :o}]])))

(deftest hierarchy-by-canonical-info-test
  (is (check
       (hierarchy-by-canonical-info
        [{:property-canonicals [:a] :item `(:i (~o1 :order :non-semantic))}
         {:property-canonicals [:a :b] :item `(:j (~o2 :order :non-semantic))}
         {:property-canonicals [:a :c] :item `(:k (~o3 :order :non-semantic))}]
        #{})
       [{:hierarchy-node true
         :properties {:a 1}
         :cumulative-properties {:a 1}
         :members [{:property-canonicals [:a]
                    :item `(:i (~o1 :order :non-semantic))}]
         :children [{:hierarchy-node true
                     :properties {:b 1}
                     :cumulative-properties {:b 1 :a 1}
                     :members [{:property-canonicals [:a :b]
                                :item `(:j (~o2 :order :non-semantic))}]}
                    {:hierarchy-node true
                     :properties {:c 1}
                     :cumulative-properties {:c 1 :a 1}
                     :members [{:property-canonicals [:a :c]
                                :item `(:k (~o3 :order :non-semantic))}]}]}])))



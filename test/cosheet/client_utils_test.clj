(ns cosheet.client-utils-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [client-utils :refer :all])
            ; :reload
            ))

(deftest set-difference-test
  (is (= (set-difference #{1 2} #{2 3}) #{1})))

(deftest replace-in-struct-test
  (is (= (replace-in-struct {:a 1 :b 2}
                            {'(3 :a) [{:a '(:d :b)} :c]
                             4 '([] {} () :a)})
          {'(3 1) [{1 '(:d 2)} :c]
           4 '([] {} () 1)})))

(deftest subcomponent-ids-test
  (is (= (set (subcomponent-ids [:div
                                 [component :message]
                                 [component :clock]]))
         #{:message :clock})))

(deftest into-atom-map-test
  (let [a1 (atom [:div {:id :a1 :data-version 1} 1])
        a2 (atom [:div {:id :a2 :data-version 3} 2])
        a3 (atom [:div {:id :a3 :data-version 1} 3])
        am (atom {:a1 a1
                  :a2 a2
                  :a3 a3})]
    (into-atom-map am [[:div {:id :a1 :data-version 2} 11]
                       [:div {:id :a2 :data-version 2} 22]
                       [:div {:id :a4 :data-version 4} 44]])
    (is (= (count @am) 3))
    (is (= (@am :a1) a1))
    (is (= (@am :a2) a2))
    (is (= (@am :a3) a3))
    (is (= @a1 [:div {:id :a1 :data-version 2} 11]))
    (is (= @a2 [:div {:id :a2 :data-version 3} 2]))
    (is (= @a3 [:div {:id :a3 :data-version 1} 3])))
  (let [am (atom {:root (atom [:div {:id :root :data-version 0}])})]
    (into-atom-map am [[:div
                        {:id :root :data-version 1}
                        [component :message]
                        [component :clock]]])
    (is (= (set (keys @am)) #{:root :message :clock}))
    (into-atom-map am [[:div {:id :message :data-version 1} "hi"]
                       [:div {:id :clock :data-version 1} "now"]
                       [:div {:id :a4 :data-version 4} 44]])
    (is (= (set (keys @am)) #{:root :message :clock}))
    (= @(@am :message) [:div {:id :message :data-version 1} "hi"])
    (into-atom-map am [[:div
                        {:id :root :data-version 2}
                        [component :message]
                        [component :tick]]
                       [:div {:id :tick :data-version 2} "tock"]])
    (is (= (set (keys @am)) #{:root :message :tick}))
    (= @(@am :tick) [:div {:id :message :data-version 2} "tock"]))
  )


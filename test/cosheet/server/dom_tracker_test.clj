(ns cosheet.server.dom-tracker-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            [clojure.data.priority-map :as priority-map]
            (cosheet
             [utils :refer [dissoc-in]]
             [entity :as entity :refer [to-list description->entity]]
             [reporters :as reporter :refer [new-reporter]]
             [computation-manager :refer [new-management compute]]
             [debug :refer [current-value envs-to-list
                            let-propagated let-propagated-store]]
             entity-impl
             store-impl
             mutable-store-impl)
            (cosheet.server
             [dom-tracker :refer :all]
             [render :refer [item-DOM]])
            ; :reload
            ))

(deftest action-test
  (let [a (atom {})
        action (fn [atm extra]
                 (is (= atm a))
                 (is (= @a {1 2}))
                 (is (= extra 3))
                 (swap! a (constantly 4)))]
    (swap-and-act a #(-> %
                         (assoc 1 2)
                         (update-new-pending-action action 3)))
    (is (= @a 4))))

(deftest make-key-test
  (is (= (make-key nil 1) [1]))
  (is (= (make-key [1] 2) [1 2])))

(deftest subcomponent->component-map-test
  (is (= (subcomponent->component-map {:sibling-key 1 :definition 2} [0] 3)
         {:key [0 1] :definition 2 :depth 4})))

(deftest dom->subcomponents-test
  (is (= (set  (dom->subcomponents
                [:div
                 [:component {:foo :bar}]
                 [:component {:bletch 1}]
                 [:div "hi" [:component {1 2}]]]))
         #{{:foo :bar} {:bletch 1} {1 2}})))

(deftest adjust-subcomponents-for-client-test
  (is (= (adjust-subcomponents-for-client
          {:components {[:p :a] {:id 2}
                        [:p :b] {:id 3}}}
          [:p]
          [:div
           [:component {:sibling-key :a :attributes {:width 5}}]
           [:div  [:component {:sibling-key :b}]]])
         [:div
          [:component {:width 5} 2]
          [:div  [:component nil 3]]])))

(deftest dom-for-client-test
  (is (= (dom-for-client
          {:components {[:p] {:id 1
                              :key [:p]
                              :dom [:div
                                    [:component {:sibling-key :a
                                                 :attributes {:width 4}}]
                                    [:div  [:component {:sibling-key :b}]]]
                              :version 5}
                        [:p :a] {:id 2}
                        [:p :b] {:id 3}}}
          [:p])
         [:div
          {:id 1 :version 5}
          [:component {:width 4} 2]
          [:div  [:component nil 3]]])))

(deftest response-doms-test
  (is (= (response-doms {} 3) []))
  (is (= (set (response-doms
               {:components {[:p] {:id "id1"
                                   :key [:p]
                                   :dom [:div
                                         [:component {:sibling-key :a}]
                                         [:div  [:component
                                                 {:attributes {:width 9}
                                                  :sibling-key :b}]]]
                                   :attributes {:width 4}
                                   :version 3}
                             [:p :a] {:id "id2"
                                      :dom [:div "hi"]
                                      :version 5}
                             [:p :b] {:id "id3"
                                      :dom [:div "there"]
                                      :version 7
                                      :attributes {:width 9}}}
                :out-of-date-keys (-> (priority-map/priority-map)
                                      (assoc [:p] 0)
                                      (assoc [:p :a] 2)
                                      (assoc [:p :b] 1))}
               2))
         #{[:div
            {:id "id1" :version 3}
            [:component nil "id2"]
            [:div  [:component {:width 9} "id3"]]]
           [:div {:id "id3" :version 7} "there"]})))

(deftest update-acknowledgements-test
  (let [data {:components {[:p] {:id "id1"
                                 :key [:p]
                                 :version 3}
                           [:p :a] {:id "id2"
                                    :key [:p :a]
                                    :version 5}
                           [:p :b] {:id "id3"
                                    :key [:p :b]
                                    :version 7}}
              :out-of-date-keys (priority-map/priority-map
                                  [:p] 1 [:p :a] 2 [:p :b] 2)
              :id->key {"id1" [:p] "id2" [:p :a] "id3" [:p :b]}}]
    (is (= (update-acknowledgements data {"id1" 3 "id2" 4})
           (assoc data :out-of-date-keys (priority-map/priority-map
                                          [:p :a] 2 [:p :b] 2))))
    (let [tracker (atom data)]
      (process-acknowledgements tracker {"id1" 3 "id2" 4})
      (is (= @tracker
             (assoc data :out-of-date-keys (priority-map/priority-map
                                            [:p :a] 2 [:p :b] 2)))))))

(deftest update-unnedded-subcomponents-test
  (is (= (update-unneeded-subcomponents
          {:components {1 2, 3 4, 5 6}}
          {:subcomponents #{1 3}}
          {:subcomponents #{1 7}}))
      {:components {1 2, 5 6}}))

(comment (deftest update-dom-test
           (is (= (update-dom {:components {:a {:key :a}}}
                              :a
                              [])))))

(deftest update-attending-test
  (let [r (reporter/new-reporter)
        component-map {:key :a :reporter r}
        a (atom {})]
    (swap-and-act a #(-> %
                         (assoc-in [:components :a] component-map)
                         (update-request-set-attending component-map))) 
    (is (= (:attendees (reporter/data r))
           {[:dom-request :a] [dom-callback a]}))
    (swap-and-act a #(-> %
                         (dissoc-in [:components :a])
                         (update-request-set-attending component-map)))
    (is (= (:attendees (reporter/data r)) nil))))

(deftest update-ensure-component-test
  (is (= (update-ensure-component {:components {:a 1}} :a))
      {:components {:a 1}})
  (let [tracker @(new-dom-tracker nil)]
    (let [id "root"]
      (is (= (update-ensure-component tracker :b id)
             (-> tracker
                 (assoc :components {:b {:key :b
                                         :version 0
                                         :depth 0
                                         :id id}})
                 (assoc :id->key {id :b})))))
    (let [tracker @(new-dom-tracker nil)
          id-num (:next-id tracker)
          id (str "id" id-num)]
      (is (= (update-ensure-component tracker :b)
             (-> tracker
                 (assoc :components {:b {:key :b
                                         :version 0
                                         :depth 0
                                         :id id}})
                 (assoc :id->key {id :b})
                 (assoc :next-id (inc id-num))))))))

(deftest update-clear-component-test
  (let [tracker @(new-dom-tracker nil)
        id (:next-id tracker)]
    (is (= (-> tracker
               (update-ensure-component :b)
               (update-clear-component :b))
           (-> tracker
               (assoc :next-id (inc id)))))))

(deftest update-set-component-test
  (let [management (new-management)
        tracker (new-dom-tracker management)
        reporter (new-reporter :value "hi")
        c-map {:key [:k]
               :id "root"
               :definition [item-DOM reporter #{} {}]
               :attributes {:style {:color "blue"}}}
        alt-c-map (assoc-in c-map [:attributes :style :color] "black")
        deep-c-map {:key [:d]
                    :definition
                    [(fn [value]
                       (into [:div
                              [:component
                               {:sibling-key "s"
                                :definition [item-DOM reporter #{} {}]
                                :attributes {:width 1}}]]
                             (map (fn [c]
                                    [:component
                                     {:sibling-key (str c)
                                      :definition [item-DOM (str c) #{} {}]}])
                                  value)))
                     reporter]}]
    (swap-and-act tracker #(update-set-component % c-map))
    (compute management)
    (let [data @tracker
          component (get-in data [:components [:k]])]
      (is (= (:dom component) [:div  {:class "item"} "hi"]))
      (is (= (:id component) "root"))
      (is (= (:version component) 1))
      (is (= (:depth component) 0))
      (is (= (:components @tracker {[:k] component})))
      (is (= (:id->key data {"root" [:k]})))
      (is (= (:next-id data) 0))
      (is (= (set (:out-of-date-keys data)) #{[[:k] 0]}))
      ;; Try some updates that don't change the definition.
      (swap! tracker #(assoc % :out-of-date-keys (priority-map/priority-map)))
      (compute management)
      (let [data @tracker]
        (swap-and-act tracker #(update-set-component % c-map))
        (compute management)
        (is (= @tracker data))
        (swap-and-act tracker #(update-set-component % alt-c-map))
        (compute management)
        (is (= (get-in @tracker [:components [:k]])
               (assoc component :attributes (:attributes alt-c-map))))
        (is (= (set (:out-of-date-keys @tracker)) #{})))
      ;; Change the value of the reporter, and make sure the dom updates.
      (swap! tracker #(assoc % :out-of-date-keys (priority-map/priority-map)))
      (reporter/set-value! reporter "ho")
      (compute management)
      (is (= (get-in @tracker [:components [:k] :dom])
             [:div  {:class "item"} "ho"]))
      (is (= (set (:out-of-date-keys @tracker)) #{[[:k] 0]})))
    (swap-and-act tracker #(update-set-component % deep-c-map))
    (is (nil? (get-in @tracker [:components [:d] :dom])))
    (compute management)
    (reporter/set-value! reporter "hi")
    (compute management)
    (is (= (into {} (map (fn [[key component]] [key (:dom component)])
                               (get-in @tracker [:components])))
           {[:k] [:div  {:class "item"} "hi"]
            [:d] [:div
                  [:component {:sibling-key "s"
                               :definition [item-DOM reporter  #{} {}]
                               :attributes {:width 1}}]
                  [:component {:sibling-key (str "h")
                               :definition [item-DOM "h" #{} {}]}]
                  [:component {:sibling-key (str "i")
                               :definition [item-DOM "i" #{} {}]}]]
            [:d "s"] [:div  {:class "item"} "hi"]
            [:d "h"] [:div  {:class "item"} "h"]
            [:d "i"] [:div  {:class "item"} "i"]}))
    (is (= (get-in @tracker [:components [:d "s"] :attributes] {:width 1})))
    ))

(deftest add-dom-test
  (let [management (new-management)
        tracker (new-dom-tracker management)
        reporter (new-reporter :value "hi")]
    (add-dom tracker "root" [item-DOM reporter #{} {}])
    (compute management)
    (let [data @tracker
          component (get-in data [:components ["root"]])]
      (is (= (:dom component) [:div {:class "item"} "hi"]))
      (is (= (:id component) "root"))
      (is (= (:version component) 1))
      (is (= (:depth component) 0))
      (is (= (:components @tracker {[:k] component})))
      (is (= (:id->key data {"root" [:k]})))
      (is (= (:next-id data) 0))
      (is (= (set (:out-of-date-keys data)) #{[["root"] 0]})))))


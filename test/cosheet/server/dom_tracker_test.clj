(ns cosheet.server.dom-tracker-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [utils :refer [dissoc-in]]
             [entity :as entity :refer [to-list description->entity]]
             [reporters :as reporter :refer [new-reporter]]
             [computation-manager :as computation-manager
              :refer [new-management]]
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

(deftest contextualize-subcomponent-test
  (is (= (contextualize-subcomponent {:sibling-key 1 :foo 2} [0] 3)
         {:key [0 1] :depth 4 :foo 2})))

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
           [:component {:sibling-key :a}]
           [:div  [:component {:sibling-key :b}]]])
         [:div
          [:component 2]
          [:div  [:component 3]]])))

(deftest dom-for-client-test
  (is (= (dom-for-client
          {:components {[:p] {:id 1
                              :key [:p]
                              :dom [:div
                                    [:component {:sibling-key :a}]
                                    [:div  [:component {:sibling-key :b}]]]
                              :attributes {:width 4}}
                        [:p :a] {:id 2}
                        [:p :b] {:id 3}}}
          [:p])
         [:div
          {:id 1 :width 4}
          [:component 2]
          [:div  [:component 3]]])))

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
                         (update-request-attending component-map))) 
    (is (= (:attendees (reporter/data r))
           {[:dom-request :a] [dom-callback a]}))
    (swap-and-act a #(-> %
                         (dissoc-in [:components :a])
                         (update-request-attending component-map)))
    (is (= (:attendees (reporter/data r)) nil))))

(deftest update-ensure-component-test
  (is (= (update-ensure-component {:components {:a 1}} :a))
      {:components {:a 1}})
  (let [tracker @(new-dom-tracker nil)
        id (:next-id tracker)]
    (is (= (update-ensure-component tracker :b)
           (-> tracker
               (assoc :components {:b {:key :b :version 0 :depth 0 :id id}})
               (assoc :id->key {id :b})
               (assoc :next-id (inc id)))))))

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
               :definition [item-DOM reporter  #{} {}]
               :attributes {:style {:color "blue"}}}
        alt-c-map (assoc-in c-map [:attributes :style :color] "black")
        deep-c-map {:key [:d]
                    :definition
                    [(fn [value]
                       (into [:div
                              [:component
                               {:sibling-key "s"
                                :definition [item-DOM reporter  #{} {}]
                                :attributes {:width 1}}]]
                             (map (fn [c]
                                    [:component
                                     {:sibling-key (str c)
                                      :definition [item-DOM (str c) #{} {}]}])
                                  value)))
                     reporter]}]
    (swap-and-act tracker #(update-set-component % c-map))
    (computation-manager/compute management)
    (let [data @tracker
          component (get-in data [:components [:k]])]
      (is (= (:dom component) [:div "hi"]))
      (is (= (:id component) 0))
      (is (= (:version component) 1))
      (is (= (:depth component) 0))
      (is (= (:components @tracker {[:k] component})))
      (is (= (:id->key data {0 [:k]})))
      (is (= (:next-id data) 1))
      (is (= (set (:out-of-date-ids data)) #{[0 0]}))
      ;; Try some updates that don't change the definition.
      (swap! tracker #(update-in % [:out-of-date-ids]
                                 (fn [items] (dissoc items 0))))
      (computation-manager/compute management)
      (let [data @tracker]
        (swap-and-act tracker #(update-set-component % c-map))
        (computation-manager/compute management)
        (is (= @tracker data))
        (swap-and-act tracker #(update-set-component % alt-c-map))
        (computation-manager/compute management)
        (is (= (get-in @tracker [:components [:k]])
               (assoc component :attributes (:attributes alt-c-map))))
        (is (= (set (:out-of-date-ids @tracker)) #{[0 0]})))
      ;; Change the value of the reporter, and make sure the dom updates.
      (swap! tracker #(update-in % [:out-of-date-ids]
                                 (fn [items] (dissoc items 0))))
      (reporter/set-value! reporter "ho")
      (computation-manager/compute management)
      (is (= (get-in @tracker [:components [:k] :dom]) [:div "ho"]))
      (is (= (set (:out-of-date-ids @tracker)) #{[0 0]})))
    (swap-and-act tracker #(update-set-component % deep-c-map))
    (is (nil? (get-in @tracker [:components [:d] :dom])))
    (computation-manager/compute management)
    (reporter/set-value! reporter "hi")
    (computation-manager/compute management)
    (is (= (into {} (map (fn [[key component]] [key (:dom component)])
                               (get-in @tracker [:components])))
           {[:k] [:div "hi"]
            [:d] [:div
                  [:component {:sibling-key "s"
                               :definition [item-DOM reporter  #{} {}]
                               :attributes {:width 1}}]
                  [:component {:sibling-key (str "h")
                               :definition [item-DOM "h" #{} {}]}]
                  [:component {:sibling-key (str "i")
                               :definition [item-DOM "i" #{} {}]}]]
            [:d "s"] [:div "hi"]
            [:d "h"] [:div "h"]
            [:d "i"] [:div "i"]}))
    (is (= (get-in @tracker [:components [:d "s"] :attributes] {:width 1})))
    ))


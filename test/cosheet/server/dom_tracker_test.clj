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
                         (update-new-further-action action 3)))
    (is (= @a 4))))

(deftest subcomponent->component-map-test
  (is (= (subcomponent->component-map {:key [0 1] :definition 2} [0] 3)
         {:key [0 1] :definition 2 :depth 4})))

(deftest dom->subcomponents-test
  (is (= (set  (dom->subcomponents
                [:div
                 [:component {:foo :bar}]
                 [:component {:bletch 1}]
                 [:div "hi" [:component {1 2}]]]))
         #{{:foo :bar} {:bletch 1} {1 2}})))

(deftest adjust-attributes-for-client-test
  (is (= (adjust-attributes-for-client
          {:key->id {:a 1 :b 2}}
          [:div {:key :a :sibling-elements :c :foo :f}
           [:div {:key :b :sibling-elements :d}]
           [:div "foo"]])
         [:div {:id 1 :foo :f}
          [:div {:key :b :sibling-elements :d}]
          [:div "foo"]])))

(deftest adjust-dom-for-client-test
  (is (= (adjust-dom-for-client
          {:key->id {[:p] 1
                     [:p :a] 2
                     [:p :b] 3}}
          [:div
           [:component {:key [:p :a] :attributes {:width 5}}]
           [:div {:key [:p]} [:component {:key [:p :b]}]]])
         [:div
          [:component {:width 5} 2]
          [:div {:id 1} [:component nil 3]]])))

(deftest dom-for-client-test
  (is (= (dom-for-client
          {:components {[:p] {:key [:p]
                              :dom [:div
                                    {:key [:p]}
                                    [:component {:key [:p :a]
                                                 :attributes {:width 4}}]
                                    [:div [:component {:key [:p :b]}]]]
                              :version 5}}
           :key->id {[:p] 1
                     [:p :a] 2
                     [:p :b] 3}}
          [:p])
         [:div
          {:id 1 :version 5}
          [:component {:width 4} 2]
          [:div [:component nil 3]]])))

(deftest response-doms-test
  (is (= (response-doms {} 3) []))
  (is (= (set (response-doms
               {:components {[:p] {:key [:p]
                                   :dom [:div
                                         {:key [:p]}
                                         [:component {:key [:p :a]}]
                                         [:div  [:component
                                                 {:attributes {:width 9}
                                                  :key [:p :b]}]]]
                                   :attributes {:width 4}
                                   :version 3}
                             [:p :a] {:dom [:div {:key [:p :a]}"hi"]
                                      :version 5}
                             [:p :b] {:dom [:div {:key [:p :b]} "there"]
                                      :version 7
                                      :attributes {:width 9}}}
                :out-of-date-keys (-> (priority-map/priority-map)
                                      (assoc [:p] 0)
                                      (assoc [:p :a] 2)
                                      (assoc [:p :b] 1))
                :key->id {[:p] "id1"
                          [:p :a] "id2"
                          [:p :b] "id3"}}
               2))
         #{[:div
            {:id "id1" :version 3}
            [:component nil "id2"]
            [:div [:component {:width 9} "id3"]]]
           [:div {:id "id3" :version 7} "there"]})))

(deftest update-acknowledgements-test
  (let [data {:components {[:p] {:key [:p]
                                 :version 3}
                           [:p :a] {:key [:p :a]
                                    :version 5}
                           [:p :b] {:key [:p :b]
                                    :version 7}}
              :out-of-date-keys (priority-map/priority-map
                                  [:p] 1 [:p :a] 2 [:p :b] 2)
              :id->key {"id1" [:p] "id2" [:p :a] "id3" [:p :b]}
              :key->id {[:p] "id1" [:p :a] "id2" [:p :b] "id3"}}]
    (is (= (update-acknowledgements data {"id1" 3 "id2" 4})
           (assoc data :out-of-date-keys (priority-map/priority-map
                                          [:p :a] 2 [:p :b] 2))))
    (let [tracker (atom data)]
      (process-acknowledgements tracker {"id1" 3 "id2" 4})
      (is (= @tracker
             (assoc data :out-of-date-keys (priority-map/priority-map
                                            [:p :a] 2 [:p :b] 2)))))))

(deftest update-associate-key-to-id-test
  (is (= (update-associate-key-to-id
          {:key->id {:a 1}
           :id->key {1 :1}}
          :b 2)
         {:key->id {:a 1 :b 2}
          :id->key {1 :1 2 :b}})))

(deftest update-ensure-id-for-key-test
  (is (= (update-ensure-id-for-key
          {:key->id {:a 1}
           :id->key {1 :1}
           :next-id 2}
          :a)
         {:key->id {:a 1}
          :id->key {1 :1}
          :next-id 2}))
  (is (= (update-ensure-id-for-key
          {:key->id {:a 1}
           :id->key {1 :1}
           :next-id 2}
          :b)
         {:key->id {:a 1 :b "id2"}
          :id->key {1 :1 "id2" :b}
          :next-id 3})))

(deftest update-ensure-ids-for-keys-test
  (is (= (update-ensure-ids-for-keys
          {:key->id {:a 1}
           :id->key {1 :1}
           :next-id 2}
          [:a :b])
         {:key->id {:a 1 :b "id2"}
          :id->key {1 :1 "id2" :b}
          :next-id 3})))

(deftest update-unnedded-subcomponents-test
  (is (= (update-unneeded-subcomponents
          {:components {1 2, 3 4, 5 6}}
          {:subcomponents #{1 3}}
          {:subcomponents #{1 7}}))
      {:components {1 2, 5 6}}))

;;; TODO: code update-dom-test
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
  (let [tracker @(new-dom-tracker nil)
        id-num (:next-id tracker)
        id (str "id" id-num)]
    (is (= (update-ensure-component tracker :b)
           (-> tracker
               (assoc :components {:b {:key :b
                                       :version 0
                                       :depth 0}})
               (assoc :id->key {id :b})
               (assoc :key->id {:b id})
               (assoc :next-id (inc id-num)))))
    (let [tracker (-> @(new-dom-tracker nil)
                      (update-associate-key-to-id :b :id))]
      (is (= (update-ensure-component tracker :b)
             (-> tracker
                 (assoc :components {:b {:key :b
                                         :version 0
                                         :depth 0}})))))))

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
               :definition [item-DOM reporter [:k] #{} {:depth 0}]
               :attributes {:style {:color "blue"}}}
        alt-c-map (assoc-in c-map [:attributes :style :color] "black")
        deep-c-map {:key [:d]
                    :definition
                    [(fn [value]
                       (into [:div {:key [:d]}
                              [:component
                               {:key ["s" :d]
                                :definition [item-DOM
                                             reporter ["s" :d]
                                             #{} {:depth 1}]
                                :attributes {:width 1}}]]
                             (map
                              (fn [c]
                                [:component
                                 {:key [(str c) :d]
                                  :definition [item-DOM
                                               (str c) [(str c) :d]
                                               #{} {:depth 1}]}])
                              value)))
                     reporter]}]
    (swap-and-act tracker #(-> %
                               (update-associate-key-to-id [:k] "root")
                               (update-set-component c-map)))
    (compute management)
    (let [data @tracker
          component (get-in data [:components [:k]])]
      (is (= (:dom component) [:div {:key [:k]
                                     :class "content-text editable item"}
                               "hi"]))
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
             [:div  {:key [:k] :class "content-text editable item"} "ho"]))
      (is (= (set (:out-of-date-keys @tracker)) #{[[:k] 0]})))
    (swap-and-act tracker #(update-set-component % deep-c-map))
    (is (nil? (get-in @tracker [:components [:d] :dom])))
    (compute management)
    (reporter/set-value! reporter "hi")
    (compute management)
    (is (= (into {} (map (fn [[key component]] [key (:dom component)])
                               (get-in @tracker [:components])))
           {[:k] [:div  {:key [:k] :class "content-text editable item"} "hi"]
            [:d] [:div {:key [:d]}
                  [:component {:key ["s" :d]
                               :definition [item-DOM
                                            reporter ["s" :d] #{} {:depth 1}]
                               :attributes {:width 1}}]
                  [:component {:key [(str "h") :d]
                               :definition [item-DOM
                                            "h" ["h" :d] #{} {:depth 1}]}]
                  [:component {:key [(str "i") :d]
                               :definition [item-DOM
                                            "i" ["i" :d] #{} {:depth 1}]}]]
            ["s" :d] [:div  {:key ["s" :d]
                             :class "content-text editable item"} "hi"]
            ["h" :d] [:div  {:key ["h" :d]
                             :class "content-text editable item"} "h"]
            ["i" :d] [:div  {:key ["i" :d]
                             :class "content-text editable item"} "i"]}))
    (is (= (get-in @tracker [:components [:d "s"] :attributes] {:width 1})))
    ))

(deftest id->key-test
  (let [management (new-management)
        tracker (new-dom-tracker management)]
    (swap-and-act tracker #(update-associate-key-to-id % [:k] "root"))
    (is (= (id->key tracker "root")
           [:k]))))

(deftest add-dom-test
  (let [management (new-management)
        tracker (new-dom-tracker management)
        reporter (new-reporter :value "hi")]
    (add-dom tracker "root" [:root] [item-DOM reporter [:root] #{} {:depth 0}])
    (compute management)
    (let [data @tracker
          component (get-in data [:components [:root]])]
      (is (= (:dom component) [:div {:key [:root]
                                     :class "content-text editable item"}
                               "hi"]))
      (is (= (:version component) 1))
      (is (= (:depth component) 0))
      (is (= (:components @tracker) {[:root] component}))
      (is (= (:id->key data) {"root" [:root]}))
      (is (= (:key->id data  {[:root] "root"})))
      (is (= (:next-id data) 0))
      (is (= (set (:out-of-date-keys data)) #{[[:root] 0]})))))


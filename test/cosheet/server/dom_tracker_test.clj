(ns cosheet.server.dom-tracker-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            [clojure.data.priority-map :as priority-map]
            (cosheet
             [utils :refer [dissoc-in]]
             [entity :as entity :refer [to-list description->entity]]
             [reporters :as reporter :refer [new-reporter]]
             [expression-manager :refer [new-expression-manager-data compute]]
             [debug :refer [current-value envs-to-list]]
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
  (is (= (component->component-map [:component {:key [0 1]} 2] 3)
         {:key [0 1] :definition 2 :depth 4 :attributes {}})))

(deftest dom->subcomponent-maps-test
  (is (= (set (dom->subcomponent-maps
               [:div
                [:component {:key :a} :b]
                [:component {:key :c} :d]
                [:div "hi" [:component {:key :e} :f]]]
               4))
         #{{:key :a :definition :b :depth 5 :attributes {}}
           {:key :c :definition :d :depth 5 :attributes {}}
           {:key :e :definition :f :depth 5 :attributes {}}})))

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
           [:component {:key [:p :a] :width 5} :foo]
           [:div {:key [:p]} [:component {:key [:p :b]} :bar]]])
         [:div
          [:component {:width 5 :id 2}]
          [:div {:id 1} [:component {:id 3}]]])))

(deftest dom-for-client-test
  (is (= (dom-for-client
          {:components {[:p] {:key [:p]
                              :version 5}}
           :key->id {[:p] 1
                     [:p :a] 2
                     [:p :b] 3}
           :key->dom {[:p] [:div
                            {:key [:p]}
                            [:component {:key [:p :a]
                                         :width 4}]
                            [:div [:component {:key [:p :b]}]]]}}
          [:p])
         [:div
          {:id 1 :version 5}
          [:component {:width 4 :id 2}]
          [:div [:component {:id 3}]]])))

(deftest response-doms-test
  (is (= (response-doms {} 3) []))
  (is (= (set (response-doms
               {:components {[:p] {:key [:p]
                                   :attributes {:width 4}
                                   :version 3}
                             [:p :a] {:version 5}
                             [:p :b] {:version 7
                                      :attributes {:width 9}}}
                :out-of-date-keys (-> (priority-map/priority-map)
                                      (assoc [:p] 0)
                                      (assoc [:p :a] 2)
                                      (assoc [:p :b] 1))
                :key->id {[:p] "id1"
                          [:p :a] "id2"
                          [:p :b] "id3"}
                :key->dom {[:p] [:div
                                 {:key [:p]}
                                 [:component {:key [:p :a]}]
                                 [:div  [:component
                                         {:width 9
                                          :key [:p :b]}]]]
                           [:p :a] [:div {:key [:p :a]}"hi"]
                           [:p :b] [:div {:key [:p :b]} "there"]}}
               2))
         #{[:div
            {:id "id1" :version 3}
            [:component {:id "id2"}]
            [:div [:component {:width 9 :id "id3"}]]]
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
  (let [md (new-expression-manager-data)
        tracker (new-dom-tracker md)
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
                               {:key ["s" :d] :width 1}
                               [item-DOM reporter ["s" :d] #{} {:depth 1}]]]
                             (map
                              (fn [c]
                                [:component
                                 {:key [(str c) :d]}
                                 [item-DOM
                                  (str c) [(str c) :d] #{} {:depth 1}]])
                              value)))
                     reporter]}]
    (swap-and-act tracker #(-> %
                               (update-associate-key-to-id [:k] "root")
                               (update-set-component c-map)))
    (compute md)
    (let [data @tracker
          component (get-in data [:components [:k]])]
      (is (= (:version component) 1))
      (is (= (:depth component) 0))
      (is (= (:components @tracker) {[:k] component}))
      (is (= (:id->key data) {"root" [:k]}))
      (is (= (:key->dom data) {[:k] [:div {:key [:k]
                                           :class "item content-text editable"}
                                     "hi"]}))
      (is (= (:next-id data) 0))
      (is (= (set (:out-of-date-keys data)) #{[[:k] 0]}))
      ;; Try some updates that don't change the definition.
      (swap! tracker #(assoc % :out-of-date-keys (priority-map/priority-map)))
      (compute md)
      (let [data @tracker]
        (swap-and-act tracker #(update-set-component % c-map))
        (compute md)
        (is (= @tracker data))
        (swap-and-act tracker #(update-set-component % alt-c-map))
        (compute md)
        (is (= (get-in @tracker [:components [:k]])
               (assoc component :attributes (:attributes alt-c-map))))
        (is (= (set (:out-of-date-keys @tracker)) #{})))
      ;; Change the value of the reporter, and make sure the dom updates.
      (swap! tracker #(assoc % :out-of-date-keys (priority-map/priority-map)))
      (reporter/set-value! reporter "ho")
      (compute md)
      (is (= (get-in @tracker [:key->dom [:k]])
             [:div  {:key [:k] :class "item content-text editable"} "ho"]))
      (is (= (set (:out-of-date-keys @tracker)) #{[[:k] 0]})))
    (swap-and-act tracker #(update-set-component % deep-c-map))
    (is (nil? (get-in @tracker [:key->dom [:d]])))
    (compute md)
    (reporter/set-value! reporter "hi")
    (compute md)
    (is (= (into {} (map (fn [[key component]]
                           [key (get-in @tracker [:key->dom key])])
                         (get-in @tracker [:components])))
           {[:k] [:div  {:key [:k] :class "item content-text editable"} "hi"]
            [:d] [:div {:key [:d]}
                  [:component {:key ["s" :d]
                               :width 1}
                   [item-DOM reporter ["s" :d] #{} {:depth 1}]]
                  [:component {:key [(str "h") :d]}
                   [item-DOM "h" ["h" :d] #{} {:depth 1}]]
                  [:component {:key [(str "i") :d]}
                   [item-DOM "i" ["i" :d] #{} {:depth 1}]]]
            ["s" :d] [:div  {:key ["s" :d]
                             :class "item content-text editable"} "hi"]
            ["h" :d] [:div  {:key ["h" :d]
                             :class "item content-text editable"} "h"]
            ["i" :d] [:div  {:key ["i" :d]
                             :class "item content-text editable"} "i"]}))
    (is (= (get-in @tracker [:components [:d "s"] :attributes] {:width 1})))
    ))

(deftest id->key-test
  (let [md (new-expression-manager-data)
        tracker (new-dom-tracker md)]
    (swap-and-act tracker #(update-associate-key-to-id % [:k] "root"))
    (is (= (id->key tracker "root") [:k]))))

(deftest key->id-test
  (let [md (new-expression-manager-data)
        tracker (new-dom-tracker md)]
    (swap-and-act tracker #(update-associate-key-to-id % [:k] "root"))
    (is (= (key->id tracker [:k]) "root"))))

(deftest key->attributes-test
  (let [md (new-expression-manager-data)
        tracker (new-dom-tracker md)
        reporter (new-reporter :value "hi")]
    (add-dom tracker "root" [:root]
             [identity [:div {:key [:root] :other :bar}
                        [:component {:key [:foo :root]
                                     :other :foo}
                         [identity [:div]]]]])
    (is (= (key->attributes tracker [:root]) {:key [:root] :other :bar}))
    (is (= (key->attributes tracker [:foo :root]) {:other :foo}))))

(deftest add-dom-test
  (let [md (new-expression-manager-data)
        tracker (new-dom-tracker md)
        reporter (new-reporter :value "hi")]
    (add-dom tracker "root" [:root] [item-DOM reporter [:root] #{} {:depth 0}])
    (compute md)
    (let [data @tracker
          component (get-in data [:components [:root]])]
      (is (= (:version component) 1))
      (is (= (:depth component) 0))
      (is (= (:components @tracker) {[:root] component}))
      (is (= (:id->key data) {"root" [:root]}))
      (is (= (:key->id data  {[:root] "root"})))
      (is (= (:key->dom data)
             {[:root] [:div {:key [:root]
                             :class "item content-text editable"}
                       "hi"]}))
      (is (= (:next-id data) 0))
      (is (= (set (:out-of-date-keys data)) #{[[:root] 0]})))))

(deftest request-client-refresh-test
  (let [md (new-expression-manager-data)
        tracker (new-dom-tracker md)
        reporter (new-reporter :value "hi")]
    (add-dom tracker "root" [:root]
             [identity [:div {:key [:root] :other :bar}
                        [:component {:key [:foo :root]
                                     :other :foo}
                         [identity [:div]]]]])
    (compute md)
    (let [old-out-of-date (:out-of-date-keys @tracker)]
      (swap! tracker #(assoc % :out-of-date-keys (priority-map/priority-map)))
      (is (not= old-out-of-date (:out-of-date-keys @tracker)))
      (request-client-refresh tracker)
      (is (= old-out-of-date (:out-of-date-keys @tracker))))))


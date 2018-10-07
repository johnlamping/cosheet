(ns cosheet.server.dom-tracker-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            [clojure.data.priority-map :as priority-map]
            (cosheet
             [debug :refer [simplify-for-print]]
             orderable
             [utils :refer [dissoc-in]]
             [test-utils :refer [check any as-set let-mutated]]
             [entity :as entity :refer [to-list description->entity]]
             [reporter :as reporter :refer [new-reporter]]
             [expression-manager :refer [new-expression-manager-data compute]]
             [debug :refer [envs-to-list]]
             entity-impl
             [store :refer [update-content!
                            new-element-store new-mutable-store]]
             store-impl
             mutable-store-impl)
            (cosheet.server
             [dom-tracker :refer :all]
             [render :refer [DOM-for-client-R]]
             [item-render :refer [item-DOM-R]]
             [order-utils :refer [update-add-entity-with-order-and-temporary]]
             [referent :refer [item-referent referent->string]]
             [session-state :refer [create-client-state]])
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
          {}
          [:div {:key [:a] :foo :f}
           [:div {:key [:b]}]
           [:div "foo"]])
         [:div {:id ":a" :foo :f}
          [:div {:key [:b]}]
          [:div "foo"]])))

(deftest adjust-dom-for-client-test
  (is (= (adjust-dom-for-client
          {:key->id {[:p] "root"}}
          [:div
           [:component {:key [:p :a] :width 5} :foo]
           [:div {:key [:p]} [:component {:key [:p :b]} :bar]]])
         [:div
          [:component {:width 5 :id ":p_:a"}]
          [:div {:id "root"} [:component {:id ":p_:b"}]]])))

(deftest dom-for-client-test
  (is (= (dom-for-client
          {:components {[:p] {:key [:p]
                              :version 5}}
           :key->id {[:p] "root"}
           :key->dom {[:p] [:div
                            {:key [:p]}
                            [:component {:key [:p :a]
                                         :width 4}]
                            [:div [:component {:key [:p :b]}]]]}}
          [:p])
         [:div
          {:id "root" :version 5}
          [:component {:width 4 :id ":p_:a"}]
          [:div [:component {:id ":p_:b"}]]])))

(deftest response-doms-test
  (is (= (response-doms {} 3) []))
  (is (check (response-doms
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
               :key->id {[:p] "root"}
               :key->dom {[:p] [:div
                                {:key [:p]}
                                [:component {:key [:p :a]}]
                                [:div  [:component
                                        {:width 9
                                         :key [:p :b]}]]]
                          [:p :a] [:div {:key [:p :a]}"hi"]
                          [:p :b] [:div {:key [:p :b]} "there"]}}
              2)
             (as-set [[:div
                       {:id "root" :version 3}
                       [:component {:id ":p_:a"}]
                       [:div [:component {:width 9 :id ":p_:b"}]]]
                      [:div {:id ":p_:b" :version 7} "there"]]))))

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
    (is (= (update-acknowledgements data {":p" 3 ":p_:a" 4})
           (assoc data :out-of-date-keys (priority-map/priority-map
                                          [:p :a] 2 [:p :b] 2))))
    (let [tracker (atom data)]
      (process-acknowledgements tracker {":p" 3 ":p_:a" 4})
      (is (= @tracker
             (assoc data :out-of-date-keys (priority-map/priority-map
                                            [:p :a] 2 [:p :b] 2)))))))

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
  (let [r (new-reporter)
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
  (let [tracker @(new-dom-tracker nil)
        updated (update-ensure-component tracker [:b])]
    (is (check updated
               (-> tracker
                   (assoc :components {[:b] {:key [:b]
                                             :version 1
                                             :depth 0}})
                   (update-in [:next-version] inc))))
    (is (check (update-ensure-component updated [:b])
               updated))))

(deftest update-clear-component-test
  (let [tracker @(new-dom-tracker nil)]
    (is (check (-> tracker
                   (update-ensure-component [:b])
                   (update-clear-component [:b]))
               (-> tracker
                   (assoc :next-version (inc (:next-version tracker))))))))

(deftest update-set-component-test
  (let [md (new-expression-manager-data)
        tracker (new-dom-tracker md)
        [joe jane jill] (let-mutated [joe "Joe", jane "Jane", jill "Jill"]
                          [joe jane jill])
        joe-key [:k (item-referent joe)]
        reporter (new-reporter :value jane) ;; Will change to jill.
        c-map {:key joe-key
               :definition [item-DOM-R joe [] {:priority 0
                                               :key-prefix [:k]
                                               :subject-referent nil}]
               :attributes {:style {:color "blue"}}}
        alt-c-map (assoc-in c-map [:attributes :style :color] "black")
        deep-c-map {:key [:d]
                    :definition
                    [(fn [item]
                       [:div {:key [:d]}
                        [:component
                         {:key [:d (item-referent joe)] :width 1}
                         [item-DOM-R joe [] {:priority 0
                                             :key-prefix [:d]
                                             :subject-referent nil}]]
                        [:component
                         {:key [:d (item-referent item)]}
                         [item-DOM-R item [] {:priority 0
                                              :key-prefix [:d]
                                              :subject-referent nil}]]])
                     reporter]}]
    (swap-and-act
     tracker #(update-set-component % c-map))
    (compute md)
    (let [data @tracker
          component (get-in data [:components joe-key])]
      (is (= (:version component) 2))
      (is (= (:depth component) 0))
      (is (= (:components @tracker) {joe-key component}))
      (is (check (:key->dom data)
                 {joe-key [:div {:key joe-key
                                 :class (any)
                                 :target {:referent (:item-id joe)}}
                       "Joe"]}))
      (is (= (:next-version data) 3))
      (is (= (set (:out-of-date-keys data)) #{[joe-key 0]}))
      ;; Try some updates that don't change the definition.
      (swap! tracker #(assoc % :out-of-date-keys (priority-map/priority-map)))
      (compute md)
      (let [data @tracker]
        (swap-and-act tracker #(update-set-component % c-map))
        (compute md)
        (is (= @tracker data))
        (swap-and-act tracker #(update-set-component % alt-c-map))
        (compute md)
        (is (= (get-in @tracker [:components joe-key])
               (assoc component :attributes (:attributes alt-c-map))))
        (is (= (set (:out-of-date-keys @tracker)) #{})))
      ;; Change the value of the store, and make sure the dom updates.
      (swap! tracker #(assoc % :out-of-date-keys (priority-map/priority-map)))
      (let [store (:store joe)]
        (update-content! store (:item-id joe) "Joe's"))
      (compute md)
      (is (check (get-in @tracker [:key->dom joe-key])
                 [:div  {:key joe-key
                         :class (any) :target (any)}
                  "Joe's"]))
      (is (= (set (:out-of-date-keys @tracker)) #{[joe-key 0]})))
    (swap-and-act tracker #(update-set-component % deep-c-map))
    (is (nil? (get-in @tracker [:key->dom [:d]])))
    (compute md)
    (reporter/set-value! reporter jane)
    (compute md)
    (is (check
         (into {} (map (fn [[key component]]
                         [key (get-in @tracker [:key->dom key])])
                       (get-in @tracker [:components])))
         {joe-key [:div {:key joe-key
                         :target {:referent (:item-id joe)}
                         :class (any)}
                   "Joe's"]
          [:d] [:div {:key [:d]}
                [:component {:key [:d (item-referent joe)]
                             :width 1}
                 [item-DOM-R joe [] {:priority 0
                                     :key-prefix [:d]
                                     :subject-referent nil}]]
                [:component {:key [:d (item-referent jane)]}
                 [item-DOM-R jane [] {:priority 0
                                      :key-prefix [:d]
                                      :subject-referent nil}]]]
          [:d (item-referent joe)] [:div  {:key [:d (item-referent joe)]
                                            :target {:referent
                                                     (:item-id joe)}
                                           :class (any)}
                                    "Joe's"]
          [:d (item-referent jane)] [:div  {:key [:d (item-referent jane)]
                                            :target {:referent
                                                     (:item-id jane)}
                                            :class (any)}
                                     "Jane"]}))
    (is (= (get-in @tracker [:components [:d "s"] :attributes] {:width 1})))))

(deftest id->key-test
  (let [md (new-expression-manager-data)
        tracker (new-dom-tracker md)]
    (swap-and-act tracker #(-> %
                               (assoc-in [:id->key "root"] [:foo])
                               (assoc-in [:key->dom [:k]] [:div])))
    (is (= (id->key tracker "root") [:foo]))
    (is (= (id->key tracker ":k") [:k]))))

(deftest key->id-test
  (let [md (new-expression-manager-data)
        tracker (new-dom-tracker md)]
    (swap-and-act tracker #(assoc-in % [:key->id [:foo]] "root"))
    (is (= (key->id tracker [:foo]) "root"))
    (is (= (key->id tracker [:k]) ":k"))))

(deftest dom-for-key?-test
  (let [md (new-expression-manager-data)
        tracker (new-dom-tracker md)]
    (swap-and-act tracker #(assoc-in % [:key->dom [:foo]] [:div {} "root"]))
    (is (dom-for-key? tracker [:foo]))
    (is (not (dom-for-key? tracker [:k])))))

(deftest key->attributes-test
  (let [md (new-expression-manager-data)
        tracker (new-dom-tracker md)]
    (add-dom tracker "root" [:root]
             [identity [:div {:key [:root] :other :bar}
                        [:component {:key [:foo :root]
                                     :other :foo}
                         [identity [:div {:key [:foo :root]}]]]]])
    (is (= (key->attributes tracker [:root]) {:key [:root] :other :bar}))
    (is (= (key->attributes tracker [:foo :root]) {:key [:foo :root]
                                                   :other :foo}))))

(deftest add-dom-test
  (let [md (new-expression-manager-data)
        tracker (new-dom-tracker md)
        joe (let-mutated [joe "Joe"] joe)
        joe-key [:root (item-referent joe)]]
    (add-dom tracker "root" joe-key
             [item-DOM-R joe [] {:priority 0
                                 :key-prefix [:root]
                                 :subject-referent nil}])
    (compute md)
    (let [data @tracker
          component (get-in data [:components joe-key])]
      (is (= (:version component) 2))
      (is (= (:depth component) 0))
      (is (= (:components @tracker) {joe-key component}))
      (is (= (:id->key data) {"root" joe-key}))
      (is (= (:key->id data  {joe-key "root"})))
      (is (check (:key->dom data)
                 {joe-key [:div {:key joe-key
                                 :class (any) :target (any)}
                       "Joe"]}))
      (is (= (:next-version data) 3))
      (is (= (set (:out-of-date-keys data)) #{[joe-key 0]})))))

(deftest remove-all-doms-test
  (let [md (new-expression-manager-data)
        tracker (new-dom-tracker md)
        [store joe _] (update-add-entity-with-order-and-temporary
                       (new-element-store) nil '("Joe" (39 ("age" :tag)))
                       cosheet.orderable/initial :after true)
        ms (new-mutable-store store)
        client-state (create-client-state ms (referent->string joe))]
    (add-dom tracker "root" [:root]
             [DOM-for-client-R ms nil client-state nil])
    (compute md)
    (is (>= (count (:subscriptions @(:manager-data ms))) 4))
    (let [reporters (keep :reporter (vals (:components @tracker)))]
      (assert (= (count reporters) 4))
      (doseq [reporter reporters]
        (= (count (:attendees @(:data reporter))) 1))
      (remove-all-doms tracker)
      (doseq [reporter reporters]
        (= (count (:attendees @(:data reporter))) 0))
      (is (= (count (vals (:components @tracker)))  0))
      (is (= (count (:subscriptions @(:manager-data ms))) 0)))))

(deftest request-client-refresh-test
  (let [md (new-expression-manager-data)
        tracker (new-dom-tracker md)]
    (add-dom tracker "root" [:root]
             [identity [:div {:key [:root] :other :bar}
                        [:component {:key [:foo :root]
                                     :other :foo}
                         [identity [:div {:key [:foo :root]}]]]]])
    (compute md)
    (let [old-out-of-date (:out-of-date-keys @tracker)]
      (swap! tracker #(assoc % :out-of-date-keys (priority-map/priority-map)))
      (is (not= old-out-of-date (:out-of-date-keys @tracker)))
      (request-client-refresh tracker)
      (is (= old-out-of-date (:out-of-date-keys @tracker))))))


(ns cosheet2.server.dom-manager-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            [clojure.data.priority-map :as priority-map]
            (cosheet2
             [debug :refer [simplify-for-print]]
             orderable
             [utils :refer [dissoc-in]]
             [test-utils :refer [check any as-set]]
             [entity :as entity :refer [to-list description->entity]]
             [reporter :as reporter :refer [new-reporter reporter-data]]
             [calculator :refer [new-calculator-data compute]]
             [debug :refer [envs-to-list]]
             entity-impl
             [store :refer [new-element-store new-mutable-store make-id]]
             store-impl
             mutable-store-impl
             [task-queue :refer [new-priority-task-queue
                                 run-all-pending-tasks]])
            (cosheet2.server
             [dom-manager :refer :all]
             [item-render :refer [render-item-DOM]]
             [order-utils :refer [update-add-entity-with-order-and-temporary]])
            ; :reload
            ))

(def id1 (make-id "foo"))
(def id2 (make-id "bar"))
(def s2 {:relative-id id2
         :render-dom (fn [spec store] [:div 3])})
(def s1 {:relative-id id1
         :client-id :root-id
         :render-dom (fn [spec store] [:div 2 [:component s2]])})

(deftest get-id->subcomponent-specifications-test
    (is (= (get-id->subcomponent-specifications
            [:div [:component {:relative-id :foo}]
             [:div [:component {:relative-id :bar :misc 1}]]])
           {:foo {:relative-id :foo}
            :bar {:relative-id :bar :misc 1}})))

(deftest reuse-or-make-component-atom-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager cd ms)
        c1 (reuse-or-make-component-atom s1 manager nil nil)
        c1-reused (reuse-or-make-component-atom s1 manager nil c1)
        c2 (reuse-or-make-component-atom s2 manager c1 c1)]
    (is (= (:dom-specification @c1) s1))
    (is (= (:depth @c1) 1))
    (is (= (:dom-version @c1) 1))
    (is (= c1 c1-reused))
    (is (= (:dom-specification @c2) s2))
    (is (= (:containing-component @c2) c1))
    (is (= (:depth @c2) 2))))

(deftest activate-disable-component-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager cd ms)
        c2 (reuse-or-make-component-atom s2 manager nil nil)]
    (activate-component c2)
    (is (check @manager
               {:root-components {}
                :highest-version 0
                :client-ready-dom {}
                :calculator-data cd
                :mutable-store ms
                :further-actions nil}))
    (is (check (:attendees (reporter-data ms))
               {c2 [1 [id2] reporter-changed-callback]}))
    (compute cd)
    (is (check @manager
               {:root-components {}
                :highest-version 0
                :client-ready-dom {c2 1}
                :calculator-data cd
                :mutable-store ms
                :further-actions nil}))
    (is (check @c2
               {:reporters [ms]
                :id->subcomponent {}
                :dom-manager manager
                :client-needs-dom true
                :dom-specification s2
                :client-id nil
                :dom-version 2
                :containing-component nil
                :depth 1
                :dom [:div 3]
                :further-actions nil}))
    (disable-component c2)
    (is (= @c2
           (map->ComponentData
            {:id->subcomponent {}
             :dom-manager manager
             :depth 1})))))

(deftest update-dom-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager cd ms)
        c1 (reuse-or-make-component-atom s1 manager nil nil)
        updated (update-dom @c1 c1 [:div 2 [:component s2]])]
    (is (check updated
               {:reporters nil
                :further-actions [[note-dom-ready-for-client manager c1]
                                  [activate-component (any)]]
                :id->subcomponent {id2 (any)}
                :client-id nil
                :dom-manager manager
                :client-needs-dom true
                :dom-specification s1
                :dom-version 2
                :containing-component nil
                :depth 1
                :dom [:div 2 [:component s2]]}))))

(deftest compute-dom-if-old-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager cd ms)
        c1 (reuse-or-make-component-atom s1 manager nil nil)]
    (activate-component c1)
    (is (check (:tasks @(:queue cd))
               {[compute-dom-unless-newer c1 1] 1}))
    (compute cd)
    ;; This should run compute-data, which should put compute-data of
    ;; the subcomponent on the task queue, which should then get run.
    (let [c2 ((:id->subcomponent @c1) id2)]
      (is (check @c1
                 {:reporters [ms]
                  :further-actions nil
                  :id->subcomponent {id2 (any)}
                  :client-id nil
                  :dom-manager manager
                  :client-needs-dom true
                  :dom-specification s1
                  :dom-version 2
                  :containing-component nil
                  :depth 1
                  :dom [:div 2 [:component s2]]}))
      (is (check @c2
                 {:reporters [ms]
                  :further-actions nil
                  :id->subcomponent {}
                  :client-id nil
                  :dom-manager manager
                  :client-needs-dom true
                  :dom-specification s2
                  :dom-version 2
                  :containing-component c1
                  :depth 2
                  :dom [:div 3]}))
      (is (check @manager
                 {:root-components {}
                  :highest-version 0
                  :client-ready-dom {c1 1
                                     c2 2}
                  :calculator-data cd
                  :mutable-store ms
                  :further-actions nil}))
      ;; Check that nothing happens if we have a newer dom than asked for.
      (compute-dom-unless-newer c1 1)
      (is (= (:dom-version @c1) 2)))))

(deftest mark-component-tree-as-needed-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager cd ms)
        c1 (reuse-or-make-component-atom s1 manager nil nil)]
    (let [ready (mark-component-tree-as-needed c1 (:queue cd))]
      (is (= ready [])))
    (is (check (:tasks @(:queue cd))
               {[compute-dom-unless-newer c1 1] 1}))
    (activate-component c1)
    (compute cd)
    (let [c2 ((:id->subcomponent @c1) id2)
          ready (mark-component-tree-as-needed c1 (:queue cd))]
      (is (= ready [[c1 1] [c2 2]]))
      (is (check (:tasks @(:queue cd))
                 {})))))

(deftest component->client-id-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager cd ms)
        c1 (reuse-or-make-component-atom s1 manager nil nil)]
    (activate-component c1)
    (compute cd)
    (let [c2 ((:id->subcomponent @c1) id2)
          ready (mark-component-tree-as-needed c1 (:queue cd))]
      (is (= (component->client-id c1)
             ":root-id")
          (= (component->client-id c2)
             ":root-id_Ibar")))))

(deftest client-id->component-test
  ;; Also tests note-dom-ready-for-client
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager cd ms)]
    (add-root-dom manager :alt-client-id (dissoc s1 :client-id))
    (let [c1 (client-id->component @manager ":alt-client-id")]
      (activate-component c1)
      (compute cd)
      (let [c2 ((:id->subcomponent @c1) id2)]
        (is (= (client-id->component @manager ":alt-client-id_Ibar")
               c2))
        (is (check (:client-ready-dom @manager)
                   {c1 1
                    c2 2}))))))

(deftest get-response-doms-test
  ;; Also tests prepare-dom-for-client and adjust-subdom-for-client
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager cd ms)]
    (add-root-dom manager :alt-client-id (dissoc s1 :client-id))
    (let [c1 (client-id->component @manager ":alt-client-id")]
      (activate-component c1)
      (compute cd)
      [:div {:id ":alt-client-id_Ibar", :version 3}]
      (is (:highest-version @manager) 1)
      (is (check (get-response-doms manager 3)
                 (as-set [[:div {:id ":alt-client-id" :version 2}
                           [:component {:id ":alt-client-id_Ibar"}]]
                          [:div {:id ":alt-client-id_Ibar", :version 3}]])))
      (is (:highest-version @manager) 3)
      (is (check (get-response-doms manager 1)
                 [[:div {:id ":alt-client-id" :version 2}
                   [:component {:id ":alt-client-id_Ibar"}]]]))
      (is (:highest-version @manager) 3))))

(comment

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
          component-map {:key :a :reporter r :depth 2}
          a (atom {})]
      (swap-and-act a #(-> %
                           (assoc-in [:components :a] component-map)
                           (update-request-set-attending component-map))) 
      (is (= (:attendees (reporter/data r))
             {[:dom-request :a] [2000 dom-callback a]}))
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
    (let [md (new-expression-manager-data (new-priority-task-queue 0))
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
          (update-content! store (:item-id joe) "Joe's")
          (run-all-pending-tasks (store-queue store)))
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
    (let [md (new-expression-manager-data (new-priority-task-queue 0))
          tracker (new-dom-tracker md)]
      (swap-and-act tracker #(-> %
                                 (assoc-in [:id->key "root"] [:foo])
                                 (assoc-in [:key->dom [:k]] [:div])))
      (is (= (id->key tracker "root") [:foo]))
      (is (= (id->key tracker ":k") [:k]))))

  (deftest key->id-test
    (let [md (new-expression-manager-data (new-priority-task-queue 0))
          tracker (new-dom-tracker md)]
      (swap-and-act tracker #(assoc-in % [:key->id [:foo]] "root"))
      (is (= (key->id tracker [:foo]) "root"))
      (is (= (key->id tracker [:k]) ":k"))))

  (deftest dom-for-key?-test
    (let [md (new-expression-manager-data (new-priority-task-queue 0))
          tracker (new-dom-tracker md)]
      (swap-and-act tracker #(assoc-in % [:key->dom [:foo]] [:div {} "root"]))
      (is (dom-for-key? tracker [:foo]))
      (is (not (dom-for-key? tracker [:k])))))

  (deftest key->attributes-test
    (let [md (new-expression-manager-data (new-priority-task-queue 0))
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
    (let [md (new-expression-manager-data (new-priority-task-queue 0))
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
    (let [md (new-expression-manager-data (new-priority-task-queue 0))
          tracker (new-dom-tracker md)
          [store joe _] (update-add-entity-with-order-and-temporary
                         (new-element-store) nil '("Joe" (39 ("age" :tag)))
                         cosheet.orderable/initial :after true)
          queue (new-priority-task-queue 0)
          ms (new-mutable-store store queue)
          client-state (create-client-state ms (referent->string joe) queue)]
      (add-dom tracker "root" [:root]
               [DOM-for-client-R ms nil client-state])
      (compute md)
      (is (>= (count (:subscriptions @(:manager-data ms))) 4))
      (let [reporters (keep :reporter (vals (:components @tracker)))]
        (assert (= (count reporters) 2))
        (doseq [reporter reporters]
          (= (count (:attendees @(:data reporter))) 1))
        (remove-all-doms tracker)
        (doseq [reporter reporters]
          (= (count (:attendees @(:data reporter))) 0))
        (is (= (count (vals (:components @tracker)))  0))
        (compute md)
        (stop-speculative-tracking ms)
        (is (= (count (:subscriptions @(:manager-data ms))) 0)))))

  (deftest request-client-refresh-test
    (let [md (new-expression-manager-data (new-priority-task-queue 0))
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

  )

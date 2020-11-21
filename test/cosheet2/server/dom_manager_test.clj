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
             [reporter :as reporter :refer [new-reporter
                                            reporter-data reporter-value]]
             [calculator :refer [new-calculator-data compute]]
             [debug :refer [envs-to-list]]
             entity-impl
             [store :refer [new-element-store new-mutable-store make-id
                            id->element-ids]]
             [store-impl :refer [string->id]]
             mutable-store-impl
             [store-utils :refer [add-entity]]
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
(def s1- {:relative-id id2
          :client-id :wrapper
          :render-dom (fn [spec store] [:component s1])})
(def s2- {:relative-id id2
          :render-dom (fn [spec store] [:component s2])})

(deftest valid-relative-id?-test
  (is (valid-relative-id? (string->id "23")))
  (is (valid-relative-id? :i1-32B))
  (is (not (valid-relative-id? :i1_32B)))
  (is (not (valid-relative-id? :i1.32B)))
  (is (not (valid-relative-id? :I1-32B)))
  (is (not (valid-relative-id? :1-32B)))
  (is (not (valid-relative-id? :i1-32B!))))

(deftest client-id<->relative-ids-test
  (let [client-id "root_1.b"
        relative-ids (client-id->relative-ids client-id)]
    (is (check relative-ids
               [:root [(string->id "1") :b]]))
    (is (check (relative-ids->client-id relative-ids)
               client-id))))

(deftest get-id->subcomponent-specifications-test
    (is (= (get-id->subcomponent-specifications
            [:div [:component {:relative-id :foo}]
             [:div [:component {:relative-id :bar :misc 1}]]])
           {:foo {:relative-id :foo}
            :bar {:relative-id :bar :misc 1}})))

(deftest reuse-or-make-component-atom-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager ms cd)
        c1 (reuse-or-make-component-atom s1 manager nil false :foo nil)
        c1-reused (reuse-or-make-component-atom s1 manager nil false :foo c1)
        c2 (reuse-or-make-component-atom s2 manager c1 true nil c1)]
    (is (= (:dom-specification @c1) s1))
    (is (= (:depth @c1) 1))
    (is (= (:dom-version @c1) 1))
    (is (not (:elided c1)))
    (is (= c1 c1-reused))
    (is (= (:dom-specification @c2) s2))
    (is (= (:containing-component @c2) c1))
    (is (:elided @c2))
    (is (= (:depth @c2) 2))
    (is (= (type @c1) cosheet2.server.dom_manager.ComponentData))
    (is (= (type @c2) cosheet2.server.dom_manager.ComponentData))))

(deftest activate-disable-component-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager ms cd)
        c2 (reuse-or-make-component-atom s2 manager nil false :foo nil)]
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
                :client-id :foo
                :dom-version 2
                :containing-component nil
                :elided false
                :depth 1
                :dom [:div 3]
                :further-actions nil}))
    (disable-component c2)
    (is (= @c2
           (map->ComponentData
            {:id->subcomponent {}
             :dom-manager manager
             :client-id :foo
             :elided false
             :depth 1})))
    (is (= (type @c2) cosheet2.server.dom_manager.ComponentData))))

(deftest update-dom-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager ms cd)]
    (let [c1 (reuse-or-make-component-atom s1 manager nil false :foo nil)
          updated (update-dom @c1 c1 [:div 2 [:component s2]])]
      (is (= (type updated) cosheet2.server.dom_manager.ComponentData))
      (is (check updated
                 {:reporters nil
                  :further-actions [[note-dom-ready-for-client manager c1]
                                    [activate-component (any)]]
                  :id->subcomponent {id2 (any)}
                  :client-id :foo
                  :elided false
                  :dom-manager manager
                  :client-needs-dom true
                  :dom-specification s1
                  :dom-version 2
                  :containing-component nil
                  :depth 1
                  :dom [:div 2 [:component s2]]})))
    (let [c2- (reuse-or-make-component-atom s2- manager nil false :foo nil)
          updated- (update-dom @c2- c2- [:component s2])
          c2 (first (vals (:id->subcomponent updated-)))]
      (is (= (type updated-) cosheet2.server.dom_manager.ComponentData))
      (is (check @c2
                 {:reporters nil
                  :further-actions nil
                  :client-id nil
                  :elided true
                  :dom-manager manager
                  :client-needs-dom false
                  :dom-specification s2
                  :dom-version 1
                  :id->subcomponent nil
                  :containing-component c2-
                  :depth 2
                  :dom nil})))))

(deftest compute-dom-if-old-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager ms cd)
        c1 (reuse-or-make-component-atom s1 manager nil false :foo nil)]
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
                  :client-id :foo
                  :dom-manager manager
                  :client-needs-dom true
                  :dom-specification s1
                  :dom-version 2
                  :containing-component nil
                  :elided false
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
                  :elided false
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
      (is (= (:dom-version @c1) 2))
      (is (= (type @c1) cosheet2.server.dom_manager.ComponentData))
      (is (= (type @c2) cosheet2.server.dom_manager.ComponentData)))))

(deftest mark-component-tree-as-needed-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager ms cd)
        c1 (reuse-or-make-component-atom s1 manager nil false :foo nil)]
    (let [ready (mark-component-tree-as-needed c1)]
      (is (= ready [])))
    (is (check (:tasks @(:queue cd))
               {[compute-dom-unless-newer c1 1] 1}))
    (activate-component c1)
    (compute cd)
    (let [c2 ((:id->subcomponent @c1) id2)
          ready (mark-component-tree-as-needed c1)]
      (is (= ready [[c1 1] [c2 2]]))
      (is (check (:tasks @(:queue cd))
                 {}))
      (is (= (type @c1) cosheet2.server.dom_manager.ComponentData))
      (is (= (type @c2) cosheet2.server.dom_manager.ComponentData)))))

(deftest component->client-id-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager ms cd)
        c1 (reuse-or-make-component-atom s1 manager nil false :foo nil)]
    (activate-component c1)
    (compute cd)
    (let [c2 ((:id->subcomponent @c1) id2)
          ready (mark-component-tree-as-needed c1)]
      (is (= (type @c1) cosheet2.server.dom_manager.ComponentData))
      (is (= (type @c2) cosheet2.server.dom_manager.ComponentData))
      (is (= (component->client-id c1)
             "foo")
          (= (component->client-id c2)
             "foo_Ibar")))))

(deftest client-id->action-data-test
  ;; Also tests client-id->component and note-dom-ready-for-client
  (let [[s1 id1] (add-entity (new-element-store) nil "foo")
        [s2 id2] (add-entity s1 id1 "bar")
        [s id3] (add-entity s2 id2 "end")
        client1 "alt-client-id"
        client3 (str client1 "_" (:id id3))
        ms (new-mutable-store s)
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager ms cd)]
    (add-root-dom
     manager
     :alt-client-id
     {:relative-id id1
      :render-dom (fn [spec store]
                    ;; This component has an elided subcomponent.
                    [:component
                     {:relative-id id2
                      :render-dom (fn [spec store]
                                    ;; Here, a non-elided subcomponent.
                                    [:div [:component
                                           {:relative-id id3
                                            :render-dom (fn [spec store]
                                                          [:div 3])}]])}])
      :get-action-data [(fn [s c a i extra]
                          (is (= extra "test"))
                          {:target-ids [id1 id1]})
                        "test"]})
    (let [c1 (client-id->component @manager client1)
          ad1 (client-id->action-data
               @manager client1 nil (reporter-value ms))]
      (is (check ad1 {:component c1
                     :target-ids [id1 id1]}))
      (compute cd)
      (let [c2 (first (vals (:id->subcomponent @c1)))
            c3 (client-id->component @manager client3)
            ad1 (client-id->action-data
               @manager client1 nil (reporter-value ms))
            ad3 (client-id->action-data
                @manager client3 nil (reporter-value ms))]
        ;; The containing component should refer its actions to its contained.
        (is (check ad1 {:component c2
                       :target-ids [id2 id2]}))
        (is (= c3 ((:id->subcomponent @c2) id3)))
        (is (check ad3 {:component c3
                        :target-ids [id3 id3]}))
        ;; The elided dom should not need to go to the manager.
        (is (check (:client-ready-dom @manager)
                   {c1 1 c3 3}))))))

(deftest get-response-doms-and-process-acknowledgements-test
  ;; Also tests add-root-dom, request-client-refresh,
  ;; remove-all-doms, prepare-dom-for-client and adjust-subdom-for-client
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager ms cd)]
    (add-root-dom manager :alt-client-id (dissoc s1- :client-id))
    (let [c1- (client-id->component @manager "alt-client-id")]
      (activate-component c1-)
      (compute cd)
      (let [c1 (first (vals (:id->subcomponent @c1-)))
            c2 (first (vals (:id->subcomponent @c1)))]
        (is (:highest-version @manager) 1)
        (is (check (get-response-doms manager [id2] 3)
                   [(as-set [[:div {:id "alt-client-id" :version 3}
                              2
                              [:component {:id "alt-client-id_Ibar"}]]
                             [:div {:id "alt-client-id_Ibar" :version 3}
                              3]])
                    "alt-client-id_Ibar"]))
        (is (:highest-version @manager) 3)
        (is (check (get-response-doms manager [id2] 1)
                   [[[:div {:id "alt-client-id" :version 3}
                      2
                      [:component {:id "alt-client-id_Ibar"}]]]
                    "alt-client-id"]))
        (is (:highest-version @manager) 3)
        (is (:client-needs-dom @c1-))
        (is (:client-needs-dom @c2))
        ;; The client doesn't need to know about the elided dom.
        (is (not (:client-needs-dom @c1)))
        (is (check (:client-ready-dom @manager)
                   {c1- 1 c2 3}))
        (process-acknowledgements manager {"alt-client-id" 1})
        (is (:client-needs-dom @c1-))
        (is (check (:client-ready-dom @manager)
                   {c1- 1  c2 3}))
        (process-acknowledgements manager {"alt-client-id" 3
                                           "alt-client-id_Ibar" 2})
        (is (not (:client-needs-dom @c1-)))
        (is (:client-needs-dom @c2))
        (is (check (:client-ready-dom @manager)
                   {c2 3}))
        (process-acknowledgements manager {"alt-client-id" 2
                                           "alt-client-id_Ibar" 3})
        (is (not (:client-needs-dom @c1-)))
        (is (not (:client-needs-dom @c2)))
        (is (check (:client-ready-dom @manager)
                   {}))
        (is (= (type @c1-) cosheet2.server.dom_manager.ComponentData))
        (is (= (type @c1) cosheet2.server.dom_manager.ComponentData))
        (request-client-refresh manager)
        (is (= (:client-ready-dom @manager)
               {c1- 1  c2 3}))
        (is (check (keys (:attendees @(:data ms)))
                   (as-set [c1- c1 c2])))
        (remove-all-doms manager)
        (is (empty? (keys (:attendees @(:data ms)))))
        (is (nil? (:dom-specification @c1-)))
        (is (nil? (:dom-specification @c1)))))))


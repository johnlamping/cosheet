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
        c1 (reuse-or-make-component-atom s1 manager nil :foo nil)
        c1-reused (reuse-or-make-component-atom s1 manager nil :foo c1)
        c2 (reuse-or-make-component-atom s2 manager c1 nil c1)]
    (is (= (:dom-specification @c1) s1))
    (is (= (:depth @c1) 1))
    (is (= (:dom-version @c1) 1))
    (is (= c1 c1-reused))
    (is (= (:dom-specification @c2) s2))
    (is (= (:containing-component @c2) c1))
    (is (= (:depth @c2) 2))
    (is (= (type @c1) cosheet2.server.dom_manager.ComponentData))
    (is (= (type @c2) cosheet2.server.dom_manager.ComponentData))))

(deftest activate-disable-component-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager cd ms)
        c2 (reuse-or-make-component-atom s2 manager nil :foo nil)]
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
                :depth 1
                :dom [:div 3]
                :further-actions nil}))
    (disable-component c2)
    (is (= @c2
           (map->ComponentData
            {:id->subcomponent {}
             :dom-manager manager
             :client-id :foo
             :depth 1})))
    (is (= (type @c2) cosheet2.server.dom_manager.ComponentData))))

(deftest update-dom-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager cd ms)
        c1 (reuse-or-make-component-atom s1 manager nil :foo nil)
        updated (update-dom @c1 c1 [:div 2 [:component s2]])]
    (is (check updated
               {:reporters nil
                :further-actions [[note-dom-ready-for-client manager c1]
                                  [activate-component (any)]]
                :id->subcomponent {id2 (any)}
                :client-id :foo
                :dom-manager manager
                :client-needs-dom true
                :dom-specification s1
                :dom-version 2
                :containing-component nil
                :depth 1
                :dom [:div 2 [:component s2]]}))
    (is (= (type updated) cosheet2.server.dom_manager.ComponentData))))

(deftest compute-dom-if-old-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager cd ms)
        c1 (reuse-or-make-component-atom s1 manager nil :foo nil)]
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
      (is (= (:dom-version @c1) 2))
      (is (= (type @c1) cosheet2.server.dom_manager.ComponentData))
      (is (= (type @c2) cosheet2.server.dom_manager.ComponentData)))))

(deftest mark-component-tree-as-needed-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager cd ms)
        c1 (reuse-or-make-component-atom s1 manager nil :foo nil)]
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
        manager (new-dom-manager cd ms)
        c1 (reuse-or-make-component-atom s1 manager nil :foo nil)]
    (activate-component c1)
    (compute cd)
    (let [c2 ((:id->subcomponent @c1) id2)
          ready (mark-component-tree-as-needed c1)]
      (is (= (type @c1) cosheet2.server.dom_manager.ComponentData))
      (is (= (type @c2) cosheet2.server.dom_manager.ComponentData))
      (is (= (component->client-id c1)
             ":foo")
          (= (component->client-id c2)
             ":foo_Ibar")))))

(deftest client-id->action-data-test
  ;; Also tests client-id->component and note-dom-ready-for-client
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager cd ms)]
    (add-root-dom manager :alt-client-id (dissoc s1 :client-id))
    (let [c1 (client-id->component @manager ":alt-client-id")
          d1 (client-id->action-data @manager ":alt-client-id" nil :store)]
      (is (check d1 {:component c1
                     :target [id1]}))
      (activate-component c1)
      (compute cd)
      (let [c2 (client-id->component @manager ":alt-client-id_Ibar")
            d2 (client-id->action-data
                @manager ":alt-client-id_Ibar" nil :store)]
        (is (= c2 ((:id->subcomponent @c1) id2)))
        (is (check d2 {:component c2
                       :target [id2]}))
        (is (check (:client-ready-dom @manager)
                   {c1 1  c2 2}))))))

(deftest get-response-doms-and-process-acknowledgements-test
  ;; Also tests add-root-dom, request-client-refresh,
  ;; remove-all-doms, prepare-dom-for-client and adjust-subdom-for-client
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager cd ms)]
    (add-root-dom manager :alt-client-id (dissoc s1 :client-id))
    (let [c1 (:component
              (client-id->action-data @manager ":alt-client-id" nil :store))]
      (activate-component c1)
      (compute cd)
      (let [c2 ((:id->subcomponent @c1) id2)]
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
        (is (:highest-version @manager) 3)
        (is (:client-needs-dom @c1))
        (is (:client-needs-dom @c2))
        (is (check (:client-ready-dom @manager)
                   {c1 1  c2 2}))
        (process-acknowledgements manager {":alt-client-id" 1})
        (is (:client-needs-dom @c1))
        (is (check (:client-ready-dom @manager)
                   {c1 1  c2 2}))
        (process-acknowledgements manager {":alt-client-id" 2
                                           ":alt-client-id_Ibar" 2})
        (is (not (:client-needs-dom @c1)))
        (is (:client-needs-dom @c2))
        (is (check (:client-ready-dom @manager)
                   {c2 2}))
        (process-acknowledgements manager {":alt-client-id" 2
                                           ":alt-client-id_Ibar" 3})
        (is (not (:client-needs-dom @c1)))
        (is (not (:client-needs-dom @c2)))
        (is (check (:client-ready-dom @manager)
                   {}))
        (is (= (type @c1) cosheet2.server.dom_manager.ComponentData))
        (is (= (type @c2) cosheet2.server.dom_manager.ComponentData))
        (request-client-refresh manager)
        (is (= (:client-ready-dom @manager)
               {c1 1  c2 2}))
        (is (check (keys (:attendees @(:data ms)))
                   (as-set [c1 c2])))
        (remove-all-doms manager)
        (is (empty? (keys (:attendees @(:data ms)))))
        (is (nil? (:dom-specification @c1)))
        (is (nil? (:dom-specification @c2)))))))


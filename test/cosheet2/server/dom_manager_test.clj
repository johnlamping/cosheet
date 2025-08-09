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
             [calculator :as calculator :refer [new-calculator-data compute]]
             [application-calculator :as application-calculator]
             [expression :refer [expr]]
             entity-impl
             [store :refer [new-element-store new-mutable-store make-item-id
                            string->id]]
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

(defn make-fixed-dom-renderer
  "Make a dom renderer that when called returns a reporter that appears
  to depend on the store, but actually returns the fixed value."
  [result]
  (fn [spec store] (expr (fn [store] result) store)))
(def id1 (make-item-id "foo"))
(def id2 (make-item-id "bar"))
(def s2 {:relative-id id2
         :render-dom (make-fixed-dom-renderer [:div 3])})
(def s1 {:relative-id id1
         :render-dom (make-fixed-dom-renderer [:div 2 [:component s2]])})
(def s1- {:relative-id :root
          :render-dom (make-fixed-dom-renderer [:component s1])})
(def s2- {:relative-id id2
          :render-dom (make-fixed-dom-renderer [:component s2])})

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
        c1 (reuse-or-make-component-atom s1 manager "c1" 2 nil nil)
        c1-reused (reuse-or-make-component-atom s1 manager "c1" 2 nil c1)
        c2 (reuse-or-make-component-atom s2 manager "c2" 2 c1 c1)]
    (is (= (:dom-specification @c1) s1))
    (is (= (:depth @c1) 2))
    (is (= (:dom-version @c1) nil)) ; Not activated yet.
    (is (not (:elided c1)))
    (is (= c1 c1-reused))
    (is (= (:dom-specification @c2) s2))
    (is (= (:containing-component @c2) nil))
    (is (= (:elided-from @c2) c1))
    (is (= (:depth @c2) 2))
    (is (component-atom? c1))
    (is (component-atom? c2))))

(deftest update-dom-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager ms cd)]
    (let [c1 (reuse-or-make-component-atom s1 manager "c1" 1 nil nil)]
      ;; Make c1 look like it has been activated.
      (swap! c1 #(assoc % :dom-R true :dom-version 1))
      (let [updated (update-dom @c1 c1 [:div 2 [:component s2]])]
        (is (component-data? updated))
        (is (check updated
                   {:further-actions [[process-dom-ready-for-client manager c1]
                                      [activate-component (any)]]
                    :id->subcomponent {id2 (any)}
                    :client-id "c1"
                    :obsolete-components nil
                    :elided-from nil
                    :dom-manager manager
                    :client-needs-dom true
                    :dom-specification s1
                    :dom-version 2
                    :depth 1
                    :dom-R (any)})))
      (let [c2- (reuse-or-make-component-atom s2- manager "c2" 3 nil nil)]
        ;; Make c2- look like it has been activated.
        (swap! c2- #(assoc % :dom-R true :dom-version 1))
        (let [updated- (update-dom @c2- c2- [:component s2])
              c2 (first (vals (:id->subcomponent updated-)))]
          (is (component-data? updated-))
          (is (check @c2
                     {:further-actions nil
                      :id->subcomponent nil
                      :client-id "c2_Ibar"
                      :obsolete-components nil
                      :elided-from c2-
                      :dom-manager manager
                      :client-needs-dom false
                      :dom-specification s2
                      :dom-version nil
                      :depth 4
                      :dom-R nil})))))))

(deftest activate-deactivate-component-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager ms cd)
        c2 (reuse-or-make-component-atom s2 manager "c2" 1 nil nil)]
    (activate-component c2)
    (is (check @manager
               {:highest-version 0
                :obsolete-components nil
                :components-to-send {}
                :calculator-data cd
                :mutable-store ms
                :root-components {}
                :further-actions nil}))
    (is (check (:attendees (reporter-data ms))
               nil))
    (compute cd)
    (is (check @c2
               {:client-id "c2"
                :id->subcomponent {}
                :dom-manager manager
                :client-needs-dom true
                :dom-specification s2
                :dom-R (any)
                :dom-version 2
                :elided-from nil
                :depth 1
                :obsolete-components nil
                :further-actions nil}))
    (let [dom-R (:dom-R @c2)]
      (is (check
           (:attendees (reporter-data ms))
           {dom-R [11
                   [reporter/universal-category]
                   application-calculator/copy-subordinate-callback]}))
      (is (check @manager
                 {:root-components {}
                  :highest-version 0
                  :obsolete-components nil
                  :components-to-send {c2 1}
                  :calculator-data cd
                  :mutable-store ms
                  :further-actions nil}))
      (deactivate-component c2)
      (is (check @c2
                 {:client-id "c2"
                  :id->subcomponent nil
                  :dom-manager manager
                  :client-needs-dom nil
                  :dom-specification nil
                  :dom-R nil
                  :dom-version 2
                  :elided-from nil
                  :depth 1
                  :obsolete-components nil
                  :further-actions nil}))
      (is (component-atom? c2))
      (is (check
           (:attendees (reporter-data ms))
           {dom-R [11
                   [reporter/universal-category]
                   application-calculator/copy-subordinate-callback]}))
      (is (check @manager
                 {:root-components {}
                  :highest-version 0
                  :obsolete-components nil
                  :components-to-send {}
                  :calculator-data cd
                  :mutable-store ms
                  :further-actions nil}))
      (compute cd)
      (is (check
           (:attendees (reporter-data ms))
           nil)))))

(deftest mark-component-tree-as-needed-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager ms cd)
        c1 (reuse-or-make-component-atom s1 manager "c1" 1 nil nil)]
    (let [ready (mark-component-tree-as-needed c1)]
      (is (= ready [])))
    (is (check (:tasks @(:queue cd))
               {}))
    (activate-component c1)
    (is (check (:tasks @(:queue cd))
               {[application-calculator/do-application-calculate
                 (:dom-R @c1) cd]
                10}))
    (compute cd)
    (let [c2 ((:id->subcomponent @c1) id2)
          ready (mark-component-tree-as-needed c1)]
      (is (= ready [[c1 1] [c2 2]]))
      (is (check (:tasks @(:queue cd))
                 {}))
      (is (component-atom? c1))
      (is (component-atom? c2)))))

(deftest client-id-test
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager ms cd)
        c1 (reuse-or-make-component-atom s1 manager "c1" 1 nil nil)]
    (activate-component c1)
    (compute cd)
    (let [c2 ((:id->subcomponent @c1) id2)
          ready (mark-component-tree-as-needed c1)]
      (is (component-atom? c1))
      (is (component-atom? c2))
      (is (= (:client-id @c1)
             "c1"))
      (is (= (:client-id @c2)
             "c1_Ibar")))))

(deftest client-id->action-data-test
  ;; Also tests client-id->component and note-dom-ready-for-client
  (let [[s1 id1] (add-entity (new-element-store) nil "foo")
        [s2 id2] (add-entity s1 id1 "bar")
        [s id3] (add-entity s2 id2 "end")
        client1 "root"
        client3 (str client1 "_" (:id id3))
        ms (new-mutable-store s)
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager ms cd)]
    (add-root-dom
     manager
     {:relative-id :root
      :render-dom (make-fixed-dom-renderer
                   ;; This component has an elided subcomponent.
                   [:component
                    {:relative-id id2
                     :render-dom (make-fixed-dom-renderer
                                  ;; Here, a non-elided subcomponent.
                                  [:div [:component
                                         {:relative-id id3
                                          :render-dom (make-fixed-dom-renderer
                                                       [:div 3])}]])}])
      :get-action-data [(fn [s c a i extra]
                          (is (= extra "test"))
                          {:subject-ids [id1 id1]})
                        "test"]})
    (let [c1 (client-id->component @manager client1)
          ad1 (client-id->action-data
               @manager client1 nil (reporter-value ms))]
      (is (check ad1 {:component c1
                     :subject-ids [id1 id1]}))
      (compute cd)
      (let [c2 (first (vals (:id->subcomponent @c1)))
            c3 (client-id->component @manager client3)
            ad1 (client-id->action-data
               @manager client1 nil (reporter-value ms))
            ad3 (client-id->action-data
                @manager client3 nil (reporter-value ms))]
        ;; The containing component should refer its actions to its contained.
        (is (check ad1 {:component c2
                       :subject-ids [id2 id2]}))
        (is (= c3 ((:id->subcomponent @c2) id3)))
        (is (check ad3 {:component c3
                        :subject-ids [id3 id3]}))
        ;; The elided dom should not need to go to the manager.
        (is (check (:components-to-send @manager)
                   {c1 1 c3 3}))))))

(deftest get-response-doms-and-process-acknowledgements-test
  ;; Also tests add-root-dom, request-client-refresh,
  ;; remove-all-doms, prepare-dom-for-client and adjust-subdom-for-client
  (let [ms (new-mutable-store (new-element-store))
        cd (new-calculator-data (new-priority-task-queue 0))
        manager (new-dom-manager ms cd)]
    (add-root-dom manager s1-)
    (let [c1- (client-id->component @manager "root")]
      (activate-component c1-)
      (compute cd)
      (let [c1 (first (vals (:id->subcomponent @c1-)))
            c2 (first (vals (:id->subcomponent @c1)))]
        (is (:highest-version @manager) 1)
        (is (check (get-response-doms manager [id2] 3)
                   [(as-set [[:div {:id "root" :version 4}
                              2
                              [:component {:id "root_Ibar"}]]
                             [:div {:id "root_Ifoo_Ibar" :version 3}
                              3]])
                    "root_Ifoo_Ibar"]))
        (is (:highest-version @manager) 3)
        (is (check (get-response-doms manager [id2] 1)
                   [[[:div {:id "root" :version 4}
                      2
                      [:component {:id "root_Ibar"}]]]
                    nil]))
        (is (:highest-version @manager) 3)
        (is (:client-needs-dom @c1-))
        (is (:client-needs-dom @c2))
        ;; The client doesn't need to know about the elided dom.
        (is (not (:client-needs-dom @c1)))
        (is (check (:components-to-send @manager)
                   {c1- 1 c2 3}))
        ;; An out of date acknowledgement should do nothing.
        (process-acknowledgements manager {"root" 1})
        (is (:client-needs-dom @c1-))
        (is (check (:components-to-send @manager)
                   {c1- 1  c2 3}))
        (process-acknowledgements manager {"root" 4
                                           "root_Ifoo_Ibar" 3})
        (is (not (:client-needs-dom @c1-)))
        (is (:client-needs-dom @c2))
        (is (check (:components-to-send @manager)
                   {c2 3}))
        (process-acknowledgements manager {"root" 2
                                           "root_Ibar" 3})
        (is (not (:client-needs-dom @c1-)))
        (is (not (:client-needs-dom @c2)))
        (is (check (:components-to-send @manager)
                   {}))
        (is (component-atom? c1))
        (is (component-atom? c1-))
        (request-client-refresh manager)
        (is (= (:components-to-send @manager)
               {c1- 1  c2 3}))
        (is (check (keys (:attendees @(:data ms)))
                   (as-set [c1- c1 c2])))
        (remove-all-doms manager)
        (is (empty? (keys (:attendees @(:data ms)))))
        (is (nil? (:dom-specification @c1-)))
        (is (nil? (:dom-specification @c1)))))))


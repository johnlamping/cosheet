(ns cosheet2.server.dom-manager-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            [clojure.data.priority-map :as priority-map]
            (cosheet2
             [debug :refer [simplify-for-print]]
             orderable
             [utils :refer [dissoc-in with-latest-value swap-control-return!]]
             [test-utils :refer [check any as-set]]
             [reporter :as reporter :refer [new-reporter set-value!
                                            reporter-data reporter-value
                                            reporter-value-when-valid
                                            reporter-atom data-attended?]]
             [calculator :as calculator :refer [new-calculator-data compute
                                                current-value]]
             [application-calculator :as application-calculator]
             [expression :refer [app-R let-R]]
             entity-impl
             [store :refer [new-element-store new-mutable-store make-item-id
                            string->id]]
             mutable-store-impl
             [store-utils :refer [add-element]]
             [hiccup-utils :refer [dom-attributes add-attributes]]
             [task-queue :refer [new-priority-task-queue
                                 run-all-pending-tasks]])
            (cosheet2.server
             [dom-manager :refer :all]
             [item-render :refer [render-item-DOM]]
             [action-data :refer [default-get-action-data]])
            ; :reload
            ))

(defn make-fixed-dom-renderer
  "Make a dom renderer that when called returns a reporter that appears
  to depend on the store, but actually returns the fixed value."
  [result]
  (fn [spec store] (app-R (fn [store] result) store)))
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
                :further-actions nil
                :client-lock (any)}))
    (is (check (:attendees (reporter-data ms))
               nil))
    (compute cd)
    (is (check @c2
               {:client-id "c2"
                :id->subcomponent {}
                :dom-manager manager
                :dom-specification s2
                :dom-R (any)
                :dom-version nil
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
                  :further-actions nil
                  :client-lock (any)}))
      (deactivate-component c2)
      (is (check @c2
                 {:client-id "c2"
                  :id->subcomponent nil
                  :dom-manager manager
                  :dom-specification nil
                  :dom-R nil
                  :dom-version nil
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
                  :further-actions nil
                  :client-lock (any)}))
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
  (let [[s1 id1] (add-element (new-element-store) nil "foo")
        [s2 id2] (add-element s1 id1 "bar")
        [s id3] (add-element s2 id2 "end")
        client1 "root"
        client3 (str client1 "_" (:id id2) "_" (:id id3))
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
                     :get-action-data default-get-action-data
                     :render-dom (make-fixed-dom-renderer
                                  ;; Here, a non-elided subcomponent.
                                  [:div [:component
                                         {:relative-id id3
                                          :get-action-data default-get-action-data
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
                        :subject-ids [id2 id2]
                        :past-subject-ids [[id1 id1]]}))
        (is (= c3 ((:id->subcomponent @c2) id3)))
        (is (check ad3 {:component c3
                        :subject-ids [id3 id3]
                        :past-subject-ids [[id2 id2]
                                           [id1 id1]]}))
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
                   [(as-set [[:div {:id "root" :version 2}
                              2
                              [:component {:id "root_Ifoo_Ibar"}]]
                             [:div {:id "root_Ifoo_Ibar" :version 2}
                              3]])
                    "root_Ifoo_Ibar"]))
        (is (:highest-version @manager) 3)
        (is (check (get-response-doms manager [id2] 1)
                   [[[:div {:id "root" :version 2}
                      2
                      [:component {:id "root_Ifoo_Ibar"}]]]
                    nil]))
        (is (:highest-version @manager) 3)
        ;; The client doesn't need to know about the elided dom.
        (is (check (:components-to-send @manager)
                   {c1- 1 c2 3}))
        ;; An out of date acknowledgement should do nothing.
        (process-acknowledgements manager {"root" 1})
        (is (check (:components-to-send @manager)
                   {c1- 1  c2 3}))
        (process-acknowledgements manager {"root" 4
                                           "root_Ifoo_Ibar" 1})
        (is (check (:components-to-send @manager)
                   {c2 3}))
        (process-acknowledgements manager {"root" 2
                                           "root_Ifoo_Ibar" 3})
        (is (check (:components-to-send @manager)
                   {}))
        (is (component-atom? c1))
        (is (component-atom? c1-))
        (request-client-refresh manager)
        (is (= (:components-to-send @manager)
               {c1- 1  c2 3}))
        (is (check (keys (:attendees @(:data ms)))
                   (as-set [(:dom-R @c1-) (:dom-R @c1) (:dom-R @c2)])))
        (remove-all-doms manager)
        (compute cd)
        (is (empty? (:attendees @(:data ms))))
        (is (nil? (:dom-specification @c1-)))
        (is (nil? (:dom-specification @c1)))))))

(deftest asynchronous-test
  ;; Creates width base reporters, then a series layers of lookups
  ;; that use the value at the previous layer as an index into another
  ;; value at that layer. Then makes a bunch of components whose
  ;; values depend on that stack. Each has a level and a position
  ;; among the reporters at its level. Each takes one or more values
  ;; at its level near its position. If it is not at the lowest level,
  ;; it creates subsidiary components at the next lower level at the
  ;; position of those values.  Components at the lowest level, just
  ;; return the values in their dom.
  
  ;; The code then sets up a thread that tries to get ready components
  ;; for the client, and process acknowledgements, along with a thread
  ;; that mutates the bottom reporters. After running the mutation for
  ;; a limited number of times, we check that the doms the client has
  ;; been given match the current doms of the reporters.
  (let [width 17
        depth 4
        trials 10 ; 10000
        changes-per-trial 300
        base (vec (for [i (range width)]
                    (new-reporter :name [0 i]
                                  :value (mod (inc i) width))))
        reporters (loop [d 1
                         prev base
                         reporters [base]]
                    (if (= d depth)
                      (vec reporters)
                      (let [current
                            (mapv (fn [i]
                                    (app-R ^{:name [d i]}
                                        nth prev (nth prev i)))
                                  (range width))]
                        (recur (+ d 1) current (conj reporters current)))))
        ;; These count how many times we have calculated a dom for the
        ;; specific location.
        calculation-counters (doall (map (fn [depth]
                                           (doall (map (fn [pos] (atom 0))
                                                       (range width))))
                                         (range depth)))
        cd (new-calculator-data (new-priority-task-queue 4))
        ms (new-mutable-store (new-element-store))
        dm (new-dom-manager ms cd)
        client-lock (atom 0)
        doms-not-acknowledged (atom 0)
        repeat-doms-received (atom 0)
        ;; A set of all active dom-R reporters.
        active-dom-Rs (atom #{})
        ;; A map from client id to the latest dom the client has.
        client-copy (atom {})]
    (letfn [(layer-reporter [level position]
              (-> reporters (nth level) (nth position)))
            (subdom-values-R [level position]
              (let-R [value (layer-reporter level position)]
                (let [num (max 1 (int (/ width (+ value 2))))]
                  (map #(mod % width)
                       (range value (+ value num))))))
            (value->keyword [value] (keyword (str "root" value)))
            (value->id [value] (make-item-id (str value)))
            (id->value [id] (Integer. (:id id)))
            (specification-for-dom [level]
              {:level level
               :render-dom render-dom
               :get-action-data get-action-data})
            (dom-for-position-R [level position]
              (let-R [subdom-values (subdom-values-R level position)]
                (let [parts (map 
                             (if (= 0 level)
                               (fn [value] [:div (str value)])
                               (fn [value]
                                 [:component
                                  (assoc (specification-for-dom (- level 1))
                                         :relative-id (value->id value))]))
                             subdom-values)
                      calculation-number (swap! (nth (nth calculation-counters
                                                          level)
                                                     position)
                                                inc)]
                  (add-attributes
                   ;; If we have only one part beneath us, then half
                   ;; the time, return just it. That gives us eliding.
                   (if (and (= (count parts) 1)
                            (= (first (first parts)) :component)
                            (= (mod position 2) 0))
                     (first parts)
                     (into [:div {}] parts))
                   ;; Add information that lets us detect if calculation
                   ;; has gotten ahead of the dom manager
                   {:calculation-number calculation-number
                    :location [level position]}))))
            ;; Make a wrapper for a reporter's calculator that keeps
            ;; it in our active-dom-Rs set iff it has demand.
            (calculator-wrapper [calculator]
              (fn [reporter cd]
                (with-latest-value [attended
                                    (data-attended? (reporter-data reporter))]
                  (if attended
                    (swap! active-dom-Rs #(conj % reporter))
                    (swap! active-dom-Rs #(disj % reporter))))
                (calculator reporter cd)))
            (render-dom [{:keys [relative-id item-id level]} store]
              (let [position (id->value (or item-id relative-id))
                    dom-R (dom-for-position-R level position)]
                ;; Hook our wrapper into the DOM reporter's calculator.
                (swap! (reporter-atom dom-R)
                       (fn [reporter-data]
                         (update reporter-data :calculator
                                 #(calculator-wrapper %))))
                dom-R))
            (get-action-data
              [specification inherited-action-data action immutable-store]
              {})
            (dom-version [dom] (:version (dom-attributes dom)))
            (dom-location [dom] (:location (dom-attributes dom)))
            (dom-calculation-number [dom]
              (:calculation-number (dom-attributes dom)))
            (record-doms [for-client]
              (doseq [dom for-client]
                (let [{:keys [id version]} (dom-attributes dom)]
                  (when (swap-control-return!
                         client-copy
                         (fn [data]
                           (let [matches
                                 (if-let [current (data id)]
                                   (let [our-version (:version
                                                      (dom-attributes current))]
                                     (assert (>= version our-version))
                                     (if (= version our-version)
                                       (do (assert (= current dom))
                                           true)
                                       (= current dom)))
                                   false)]
                             [(cond-> data (not matches) (assoc id dom))
                              matches])))
                    (swap! repeat-doms-received inc)))))
            (acknowledge-doms [for-client]
              (let [acknowledgements
                    (map (fn [[dom position]]
                           (let [{:keys [id version]} (dom-attributes dom)]
                             [id
                              ;; Sometimes ack an outdated version.
                              (- version (if (= (mod position 3) 2) 1 0))]))
                         (map vector
                              for-client
                              (range (count for-client))))]
                
                (swap! doms-not-acknowledged
                       #(+ % (int(/ (count acknowledgements) 3))))
                (process-acknowledgements dm acknowledgements)))
            ;; Get client ready doms, and acknowledge them.
            ;; Return the number of doms gotten.
            (get-and-acknowledge-doms []
              ;; Make sure that only one fetch is active at a time.
              (locking client-lock
                (let [for-client (first (get-response-doms dm nil 20))]
                  (record-doms for-client)
                  (acknowledge-doms for-client)
                  (count for-client))))
            (get-and-acknowledge-all-doms []
              (loop []
                (let [num-processed (get-and-acknowledge-doms)]
                  (when (> num-processed 0)
                    (recur)))))
            ;; Check that our version of the dom of one id is correct.
            ;; If require latest is true, we assume that we are caught up
            ;; with the manager, and require that the doms match.
            ;; Otherwise, we allow a couple of extenuating circumstances:
            ;;    The manager's version is ahead of ours
            ;;    The manager's dom is from a later computation than ours.
            ;;    The manager's dom was elided from a different client id
            ;;                  than ours.
            ;; We return true if the doms matched, or we required them to.
            (check-one-id [id our-dom require-latest]
              (let [component (client-id->component @dm id)]
                (when (and component
                           (not (:elided-from @component)))
                  (let [[dom monitored]
                        (prepare-dom-for-client component nil)]
                    (when dom ; We might have a client-id for an
                              ; obsolete component.
                      (if require-latest
                        (do (is (check our-dom dom))
                            true)
                        (let [our-version (dom-version our-dom)
                              their-version (dom-version dom)]
                          ;; The manager may have a new component for
                          ;; this id, that hasn't sent us a dom
                          ;; yet. In which case, its dom-version will
                          ;; still be nil.
                          (assert (or (nil? their-version)
                                      (>= their-version our-version)))
                          (when (= their-version our-version)
                            (if (= our-dom dom)
                              true
                              ;; The dom manager's dom may have
                              ;; gotten ahead of its dom
                              ;; version. Confirm that by checking
                              ;; that the calculation number we
                              ;; recorded for it is higher than the
                              ;; one we last heard about. Sometimes,
                              ;; that test fails because a
                              ;; containing dom has switched which
                              ;; doms it is eliding from. Allow that
                              ;; case, too.
                              (do (is (or (> (dom-calculation-number dom)
                                             (dom-calculation-number our-dom))
                                          (not= (dom-location dom)
                                                (dom-location our-dom))))
                                  false))))))))))
            (check-one-id-and-subcomponents [id client-data require-latest]
              (if-let [our-dom (client-data id)]
                (let [matched (if (check-one-id id our-dom require-latest)
                                1 0)
                      sub-specs (subcomponent-specifications our-dom)
                      sub-ids (map #(:id %) sub-specs)
                      sub-matches (map #(check-one-id-and-subcomponents
                                         % client-data require-latest)
                                       sub-ids)]
                  (apply + matched sub-matches))
                0))
            ;; Check that all of the information we have recorded on
            ;; the reachable components accords with the manager. If
            ;; require-latest is true, require an exact
            ;; match. Otherwise, allow for the manager to be ahead of
            ;; us.
            ;; We return the number of doms that matched.
            (check-client-copy [require-latest]
              (let [client-data @client-copy]
                (apply + (map #(check-one-id-and-subcomponents
                                (id-subpart->client-id-subpart
                                 (value->keyword %))
                                client-data require-latest)
                              (range width)))))
            (component-and-subcomponent-ids [id]
              ;; We can't use our cache, because we want to get the
              ;; elided ids too. So we go to the manager.
              (when-let [component (client-id->component @dm id)]
                (when-let [dom-R (:dom-R @component)]
                  (when-let [dom (reporter-value-when-valid dom-R)]
                    (apply concat
                           [id]
                           (->> (subcomponent-specifications dom)
                                (map #(subcomponent-client-id
                                       id (:relative-id %)))
                                (map component-and-subcomponent-ids)))))))
            (all-dom-ids []
              (let [client-data @client-copy]
                (apply concat (->> (range width)
                                   (map #(id-subpart->client-id-subpart
                                          (value->keyword %)))
                                   (map component-and-subcomponent-ids)))))]
      (doseq [position (range width)]
        (add-root-dom dm (assoc (specification-for-dom (- depth 1))
                                :item-id (value->id position)
                                :relative-id (value->keyword position))))
      ;; First see if everything is starting out right.
      (compute cd)
      (get-and-acknowledge-all-doms)
      (is (>= (check-client-copy true)
              (* width (- depth 1))))
      (doseq [i (range trials)]
        (when (= (mod i 100) 0)
          (println "starting dom-manager asynchronous trial" i))
        (doseq [j (range changes-per-trial)]
          (set-value! (base (mod (* (inc i) j) width))
                      (mod (* j j) width))
          (when (zero? (mod j 17))
            (future (compute cd (mod i 34))))
          (when (zero? (mod j (+ 10 (mod i 50))))
            (future (get-and-acknowledge-doms)))
          (when (zero? (mod j (+ 20 (mod i 100))))
            (check-client-copy false)))
        (compute cd)
        (get-and-acknowledge-all-doms)
        (let [dom-count (check-client-copy true)]
          ;; This test is only probabilistic. Fewer doms can be
          ;; checked if there is lots of elision. But it virtually
          ;; certain to succeed if the width is at least 7.
          (is (>= dom-count
                  (* width (- depth 1)))))
        (let [known-ids (all-dom-ids)
              active-ids (->> @active-dom-Rs
                              (map #(keys (:attendees (reporter-data %))))
                              (apply concat)
                              (map #(:client-id @%)))
              excess-active (clojure.set/difference (set active-ids)
                                                    (set known-ids))
              excess-known (clojure.set/difference (set known-ids)
                                                   (set active-ids))]
          (is (empty? excess-active))
          (is (empty? excess-known))
          ;; If we fail either of the above two tests, we will
          ;; probably keep failing on successive trials, since the
          ;; mismatch continues across trials. So stop the test now if
          ;; either of the above failes.
          (assert (empty? excess-active))
          (assert (empty? excess-known))))
      ;; The main reason we receive a dom more than once is because we
      ;; didn't acknowledge it. But benign races in the manager can
      ;; occasionally cause us to get a dom an extra time. Make sure
      ;; this didn't happen very often.
      (assert (< @repeat-doms-received (+ 2 ; for noise tolerance on small runs
                                          (* 1.001 @doms-not-acknowledged))))
      (println "doms test did not acknowledge" @doms-not-acknowledged
               "repeat doms received" @repeat-doms-received))))

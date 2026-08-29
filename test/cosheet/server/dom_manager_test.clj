(ns cosheet.server.dom-manager-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [clojure.data.priority-map :as priority-map]
            (cosheet
             [debug :refer [simplify-for-print]]
             orderable
             [utils :refer [dissoc-in with-latest-value swap-control-return!
                            swap-and-act! swap-and-act-control-return!]]
             [test-utils :refer [check any as-set]]
             [reporter :as reporter :refer [make-reporter set-value!
                                            reporter-data reporter-value-or-invalid
                                            reporter-value-when-valid
                                            reporter-atom data-attended?]]
             [calculator :as calculator :refer [make-calculator-data compute
                                                current-value]]
             [application-calculator :as application-calculator]
             [reporter-macros :refer [app-R let-R]]
             [map-reporter :refer [make-map-reporter map-reporter-get
                                   map-reporter-set-value!]]
             entity-impl
             [store :refer [new-element-store new-mutable-store make-item-id
                            string->id]]
             mutable-store-impl
             [store-utils :refer [add-element]]
             [hiccup-utils :refer [dom-attributes add-attributes]]
             [task-queue :refer [make-priority-task-queue
                                 run-all-pending-tasks
                                 finished-all-tasks?]])
            (cosheet.server
             [dom-manager :refer :all]
             [action-data :refer [default-get-action-data]])
            ; :reload
            )
  (:import (cosheet.server.dom_manager ComponentData DOMManagerData)))

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
        cd (make-calculator-data (make-priority-task-queue 0))
        manager (make-dom-manager ms cd)
        c1 (reuse-or-make-component-atom s1 manager nil "c1" 2 nil nil)
        c1-reused (reuse-or-make-component-atom s1 manager nil "c1" 2 nil c1)
        c2 (reuse-or-make-component-atom s2 manager nil "c2" 2 c1 c1)]
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
        cd (make-calculator-data (make-priority-task-queue 0))
        manager (make-dom-manager ms cd)]
    (let [c1 (reuse-or-make-component-atom s1 manager nil "c1" 1 nil nil)]
      ;; Make c1 look like it has been activated.
      (swap! c1 #(assoc % :dom-R true :dom-version 1))
      (let [updated (update-dom @c1 c1 [:div 2 [:component s2]])]
        (is (component-data? updated))
        (is (check updated
                   {:further-actions [[process-dom-ready-for-client manager c1]
                                      [activate-component (any)]]
                    :id->subcomponent {id2 (any)}
                    :client-id "c1"
                    :dismantling nil
                    :salvage-recipient nil
                    :elided-from nil
                    :dom-manager manager
                    :parent nil
                    :dismantling-state nil
                    :dom-specification s1
                    :dom-version 2
                    :depth 1
                    :dom-R (any)})))
      (let [c2- (reuse-or-make-component-atom s2- manager nil "c2" 3 nil nil)]
        ;; Make c2- look like it has been activated.
        (swap! c2- #(assoc % :dom-R true :dom-version 1))
        (let [updated- (update-dom @c2- c2- [:component s2])
              c2 (first (vals (:id->subcomponent updated-)))]
          (is (component-data? updated-))
          (is (check @c2
                     {:further-actions nil
                      :id->subcomponent nil
                      :client-id "c2_Ibar"
                      :dismantling nil
                      :salvage-recipient nil
                      :elided-from c2-
                      :dom-manager manager
                      :parent c2-
                      :dismantling-state nil
                      :dom-specification s2
                      :dom-version nil
                      :depth 4
                      :dom-R nil})))))))

(deftest activate-component-test
  (let [ms (new-mutable-store (new-element-store))
        cd (make-calculator-data (make-priority-task-queue 0))
        manager (make-dom-manager ms cd)
        c2 (reuse-or-make-component-atom s2 manager nil "c2" 1 nil nil)]
    (activate-component c2)
    (is (check @manager
               {:highest-version 0
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
                :parent nil
                :dismantling-state nil
                :dom-specification s2
                :dom-R (any)
                :dom-version nil
                :elided-from nil
                :depth 1
                :dismantling nil
                :salvage-recipient nil
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
                  :components-to-send {c2 1}
                  :calculator-data cd
                  :mutable-store ms
                  :further-actions nil
                  :client-lock (any)})))))

;;; A controllable test harness for the dom-manager. Every component's
;;; dom is looked up, by its relative-id, from a single map-reporter
;;; that holds a map from relative-id to dom. Getting a component's dom
;;; with map-reporter-get yields a reporter that only fires when that
;;; component's own entry changes, so changing one entry drives a dom
;;; update for exactly that component (and nothing else recomputes),
;;; and lets us drive precise structural changes (adding, dropping,
;;; replacing, and moving subcomponents). Each dom reporter's
;;; calculator is wrapped so
;;; that the set of dom reporters that currently have demand (are
;;; active) can be enumerated, which is what lets us check the global
;;; invariant that the active components are exactly those in the DOM
;;; tree.

(defn track-demand
  "Wrap the reporter's calculator so that active-dom-Rs holds it iff
  it has demand."
  [dom-R active-dom-Rs]
  (swap! (reporter-atom dom-R)
         (fn [rd]
           (update rd :calculator
                   (fn [calc]
                     (fn [reporter cd]
                       (with-latest-value
                         [attended (data-attended? (reporter-data reporter))]
                         (swap! active-dom-Rs (if attended conj disj) reporter))
                       (calc reporter cd))))))
  dom-R)

(defn make-harness []
  (let [ms (new-mutable-store (new-element-store))
        cd (make-calculator-data (make-priority-task-queue 0))]
    {:ms ms
     :cd cd
     :manager (make-dom-manager ms cd)
     :control-R (make-map-reporter {})
     :active-dom-Rs (atom #{})}))

(defn spec
  "A component spec whose dom is looked up, by relative-id, from the
  harness's map-reporter. The resulting dom reporter only fires when
  this relative-id's entry changes."
  [harness relative-id]
  {:relative-id relative-id
   :get-action-data default-get-action-data
   :render-dom (fn [_spec _store]
                 (track-demand
                  (map-reporter-get (:control-R harness) relative-id)
                  (:active-dom-Rs harness)))})

(defn set-dom!
  "Set the dom that the harness provides for relative-id."
  [harness relative-id dom]
  (map-reporter-set-value! (:control-R harness) relative-id dom))

(defn quiesce [harness] (compute (:cd harness)))

(defn start-root!
  "Populate the control reporter from id->dom, add a root component for
  root-id, and run to quiescence."
  [harness root-id id->dom]
  (doseq [[id dom] id->dom] (set-dom! harness id dom))
  (add-root-dom (:manager harness) (spec harness root-id))
  (quiesce harness))

(defn components-attending-to-dom-Rs
  "The component atoms whose dom reporter currently has demand."
  [harness]
  (set (mapcat (fn [dom-R] (keys (:attendees (reporter-data dom-R))))
               @(:active-dom-Rs harness))))

(defn reachable-components
  "All component atoms reachable from the manager's root-components by
  following id->subcomponent."
  [harness]
  (loop [to-visit (vals (:root-components @(:manager harness)))
         seen #{}]
    (if (empty? to-visit)
      seen
      (let [[c & rest-to-visit] to-visit]
        (if (seen c)
          (recur rest-to-visit seen)
          (recur (concat rest-to-visit (vals (:id->subcomponent @c)))
                 (conj seen c)))))))

(defn client-ids [components]
  (set (map #(:client-id @%) components)))

(defn component-at
  "Walk from the manager's root-components to the component reached by
  following root-id then the given subcomponent relative-ids. Returns
  nil if any step is missing, or if a component along the way has no
  subcomponents."
  [harness root-id & ids]
  (reduce (fn [c id]
            (when-let [id->subcomponent (and c (:id->subcomponent @c))]
              (id->subcomponent id)))
          (get (:root-components @(:manager harness)) root-id)
          ids))

(defn check-invariants
  "Assert the dom-manager's structural invariants. Should hold after
  the harness has run to quiescence."
  [harness]
  (let [manager (:manager harness)
        reachable (reachable-components harness)
        active (components-attending-to-dom-Rs harness)]
    ;; The active components are exactly those in the DOM tree: no
    ;; orphaned active component, and nothing in the tree left inactive.
    (is (= active reachable)
        {:excess-active (client-ids (set/difference active reachable))
         :excess-reachable (client-ids (set/difference reachable active))})
    ;; Every component attending to a dom reporter really is :active. At
    ;; quiescence a deactivated component's lingering attendee has been
    ;; removed, so anything still attending must be active.
    (doseq [c active]
      (is (= :active (component-data-state @c))
          [:attending-not-active (:client-id @c) (component-data-state @c)]))
    ;; No two active components share a client id.
    (let [cids (map #(:client-id @%) active)]
      (is (= (count cids) (count (set cids))) [:duplicate-client-ids cids]))
    (doseq [c reachable]
      (let [{:keys [dismantling parent]} @c]
        (is (= (component-data-state @c) :active)
            [:reachable-not-active (:client-id @c) (component-data-state @c)])
        (is (or (nil? dismantling) (set? dismantling))
            [:dismantling-not-a-set (:client-id @c) (type dismantling)])
        ;; At quiescence all salvaging/finalizing is done, so an active
        ;; component's dismantling must be empty.
        (is (empty? dismantling)
            [:dismantling-not-empty (:client-id @c) (count dismantling)])
        (is (or (nil? parent) (component-atom? parent))
            [:parent-not-a-component (:client-id @c)])
        ;; The parent link agrees with where the component actually
        ;; lives: a root (nil parent) is in the manager's
        ;; root-components, and any other component is its parent's
        ;; subcomponent at its relative-id.
        (is (if parent
              (= (get (:id->subcomponent @parent)
                      (:relative-id (:dom-specification @c))) c)
              (contains? (set (vals (:root-components @manager))) c))
            [:parent-back-pointer (:client-id @c)])))
    ;; Only active components are queued to send to the client.
    (doseq [[c _] (:components-to-send @manager)]
      (is (= (component-data-state @c) :active)
          [:stale-in-components-to-send (:client-id @c)]))))

(defn record-double-active-client-ids!
  "A non-quiescent invariant: no two components that are actually
  :active share a client id. This must hold at every instant, since
  the client can't cope with two doms for one id. We use the component
  state, not the active-dom-Rs attendee set, because a just-deactivated
  component's dom-R attendee lingers until its deferred removal runs.
  Any violation is recorded in the violations atom rather than
  asserted, because this runs on the cooperative worker threads, where
  clojure.test's reporting is not bound."
  [harness violations]
  (let [actives (filter #(= :active (component-data-state @%))
                        (components-attending-to-dom-Rs harness))
        cids (map #(:client-id @%) actives)]
    (when (not= (count cids) (count (set cids)))
      (swap! violations conj (vec cids)))))

(deftest harness-smoke-test
  (let [h (make-harness)
        child (make-item-id "child")]
    (start-root! h :root
                 {:root [:div [:component (spec h child)]]
                  child [:div "child"]})
    (check-invariants h)
    (is (= (count (components-attending-to-dom-Rs h)) 2))))

;;; Scenario tests: drive structural dom changes and verify the tree,
;;; reuse, and the global invariants after each change.

(deftest drop-subcomponent-test
  (let [h (make-harness)
        a (make-item-id "a")
        a-spec (spec h a)]
    (start-root! h :root
                 {:root [:div [:component a-spec]]
                  a [:div "a"]})
    (check-invariants h)
    (let [a-comp (component-at h :root a)
          a-dom-R (:dom-R @a-comp)]
      (is (= :active (component-data-state @a-comp)))
      ;; Drop the subcomponent.
      (set-dom! h :root [:div "no children"])
      (quiesce h)
      (check-invariants h)
      (is (nil? (component-at h :root a)))
      (is (= :defunct (component-data-state @a-comp)))
      (is (not (contains? @(:active-dom-Rs h) a-dom-R))))))

(deftest add-subcomponent-test
  (let [h (make-harness)
        a (make-item-id "a")
        b (make-item-id "b")
        a-spec (spec h a)
        b-spec (spec h b)]
    (start-root! h :root
                 {:root [:div [:component a-spec]]
                  a [:div "a"]
                  b [:div "b"]})
    (check-invariants h)
    (is (= 2 (count (reachable-components h))))
    ;; Add b alongside a.
    (set-dom! h :root [:div [:component a-spec] [:component b-spec]])
    (quiesce h)
    (check-invariants h)
    (is (= 3 (count (reachable-components h))))
    (is (= :active (component-data-state @(component-at h :root b))))))

(deftest reuse-subcomponent-on-sibling-change-test
  (let [h (make-harness)
        a (make-item-id "a")
        a-spec (spec h a)]
    (start-root! h :root
                 {:root [:div 1 [:component a-spec]]
                  a [:div "a"]})
    (check-invariants h)
    (let [a-comp (component-at h :root a)
          a-dom-R (:dom-R @a-comp)]
      ;; Change only the root's own content, keeping the same child spec.
      (set-dom! h :root [:div 2 [:component a-spec]])
      (quiesce h)
      (check-invariants h)
      ;; a is the very same atom, still active, with its original reporter.
      (is (= a-comp (component-at h :root a)))
      (is (= :active (component-data-state @a-comp)))
      (is (= a-dom-R (:dom-R @a-comp))))))

(deftest replace-subcomponent-spec-test
  ;; Same relative-id, different spec, no grandchild: the old leaf is
  ;; finalized and a new one activated.
  (let [h (make-harness)
        a (make-item-id "a")
        ;; These two specs are different because Clojure thinks their
        ;; :render-dom are different functions - even though they give
        ;; the same result.
        a-spec1 (spec h a)
        a-spec2 (spec h a)]
    (start-root! h :root
                 {:root [:div [:component a-spec1]]
                  a [:div "a"]})
    (check-invariants h)
    (let [old-a (component-at h :root a)]
      (set-dom! h :root [:div [:component a-spec2]])
      (quiesce h)
      (check-invariants h)
      (let [new-a (component-at h :root a)]
        (is (not= old-a new-a))
        (is (= :defunct (component-data-state @old-a)))
        (is (= :active (component-data-state @new-a)))))))

(deftest salvage-transfers-subcomponent-test
  ;; The core reuse path: a subcomponent's spec is replaced (same id),
  ;; so a new component is made for it, and its still-active grandchild
  ;; is salvaged/transferred to the replacement and reused.
  (let [h (make-harness)
        a (make-item-id "a")
        g (make-item-id "g")
        a-spec1 (spec h a)
        a-spec2 (spec h a)
        g-spec (spec h g)]
    (start-root! h :root
                 {:root [:div [:component a-spec1]]
                  a [:div [:component g-spec]]
                  g [:div "g"]})
    (check-invariants h)
    (let [old-a (component-at h :root a)
          g-comp (component-at h :root a g)
          g-dom-R (:dom-R @g-comp)]
      (is (= :active (component-data-state @g-comp)))
      ;; Replace a's spec, forcing a new a component.
      (set-dom! h :root [:div [:component a-spec2]])
      (quiesce h)
      (check-invariants h)
      (let [new-a (component-at h :root a)]
        (is (not= old-a new-a))
        (is (= :defunct (component-data-state @old-a)))
        (is (= :active (component-data-state @new-a)))
        ;; g was transferred to the new a and reused: same atom, same
        ;; reporter, still active.
        (is (= g-comp (component-at h :root a g)))
        (is (= :active (component-data-state @g-comp)))
        (is (= g-dom-R (:dom-R @g-comp)))))))

;;; Direct lifecycle-function tests.

(deftest direct-finalize-test
  ;; finalize on an active subtree tears down the whole subtree and
  ;; removes it from its parent.
  (let [h (make-harness)
        a (make-item-id "a")
        g (make-item-id "g")
        a-spec (spec h a)
        g-spec (spec h g)]
    (start-root! h :root
                 {:root [:div [:component a-spec]]
                  a [:div [:component g-spec]]
                  g [:div "g"]})
    (let [a-comp (component-at h :root a)
          g-comp (component-at h :root a g)]
      (finalize a-comp (component-at h :root))
      (quiesce h)
      (check-invariants h)
      (is (nil? (component-at h :root a)))
      (is (= :defunct (component-data-state @a-comp)))
      (is (= :defunct (component-data-state @g-comp))))))

;;; Teardown test.

(deftest remove-all-doms-teardown-test
  (let [h (make-harness)
        a (make-item-id "a")
        b (make-item-id "b")
        g (make-item-id "g")
        a-spec (spec h a)
        b-spec (spec h b)
        g-spec (spec h g)]
    (start-root! h :root
                 {:root [:div [:component a-spec] [:component b-spec]]
                  a [:div [:component g-spec]]
                  b [:div "b"]
                  g [:div "g"]})
    (check-invariants h)
    (let [root-comp (get (:root-components @(:manager h)) :root)
          a-comp (component-at h :root a)
          b-comp (component-at h :root b)
          g-comp (component-at h :root a g)]
      (is (= :active (component-data-state @root-comp)))
      (is (= 4 (count (reachable-components h))))
      (remove-all-doms (:manager h))
      (quiesce h)
      ;; The tree is empty, so the invariants hold trivially.
      (check-invariants h)
      ;; The whole tree, including the root, is finalized.
      (is (= :defunct (component-data-state @root-comp)))
      (is (= :defunct (component-data-state @a-comp)))
      (is (= :defunct (component-data-state @b-comp)))
      (is (= :defunct (component-data-state @g-comp)))
      ;; The manager is emptied and every dom reporter released.
      (is (empty? (:root-components @(:manager h))))
      (is (empty? (:components-to-send @(:manager h))))
      (is (empty? (components-attending-to-dom-Rs h)))
      (is (empty? (:attendees (reporter-data (:control-R h))))))))

;;; A deterministic pseudo-random stress test. Each step regenerates a
;;; random acyclic dom for one node and checks the invariants after
;;; running to quiescence, so a failure points at the exact step.

(deftest deterministic-stress-test
  (let [h (make-harness)
        n 6
        ids (mapv #(make-item-id (str "n" %)) (range n))
        specs (mapv #(spec h %) ids)
        rng (java.util.Random. 12345)
        ;; A random dom for node i: a div (with a changing marker so a
        ;; recompute is always seen) containing a random subset of the
        ;; strictly-higher nodes as children, which keeps the graph
        ;; acyclic.
        random-dom (fn [i]
                     (into [:div {:v (.nextInt rng 1000000)}]
                           (for [j (range (inc i) n)
                                 :when (.nextBoolean rng)]
                             [:component (specs j)])))]
    (doseq [i (range n)] (set-dom! h (ids i) (random-dom i)))
    (set-dom! h :root [:div [:component (specs 0)]])
    (add-root-dom (:manager h) (spec h :root))
    (quiesce h)
    (check-invariants h)
    (dotimes [_ 300]
      (let [i (.nextInt rng n)]
        (set-dom! h (ids i) (random-dom i))
        (quiesce h)
        (check-invariants h)))))

;;; A cooperative scheduler for deterministically interleaving several
;;; concurrent activities. Each activity runs on its own thread, but a
;;; turn-lock (a per-task semaphore, handed off under a monitor) means
;;; exactly one thread runs at a time, and a seeded rng picks which
;;; ready task gets the turn next. swap-and-act! is redefined to run its
;;; cascade inline (so a swap-and-act!'s follow-on actions all complete
;;; before the code after it, matching production) but to yield the turn
;;; before each follow-on action, so another activity can interleave
;;; between a strand's actions. Interleaving is thus at swap-and-act!
;;; (CAS) boundaries between concurrent activities, which is where the
;;; real system's atoms serialize. Reporter atoms (plain-map data, not a
;;; record) keep the normal inline, non-yielding behavior.

;; A map {:seq <semaphore>} belonging to the currently running task.
(def ^:dynamic *coop-task* nil)

(defn make-coop
  "Make a cooperative scheduler driven by rng. The options, if given:
    :mid-check  a thunk run on about one swap in ten to check
                invariants while the system is not quiescent.
    :injector   a thunk run on about one swap in ten to interject an
                additional activity into the running cascade.
  rng picks the next ready task and gates the mid-check and injector. A
  single stream is as deterministic as several would be, since every
  draw happens on the one thread that holds the turn."
  ([rng] (make-coop rng {}))
  ([rng {:keys [mid-check injector]}]
   {:monitor (Object.)
    :ready (atom []) ; A sequence of ready actions, each represented by
                     ; a map {:seq <semaphore>} holding the semaphore
                     ; it is waiting on.
    :rng rng
    :mid-check mid-check
    :injector injector}))

(defn- coop-pass-turn!
  "Give the turn to a random ready task. Must be called holding the
  monitor."
  [{:keys [ready rng]}]
  (when (seq @ready)
    (let [i (.nextInt rng (count @ready))
          token (nth @ready i)]
      (swap! ready #(into (subvec % 0 i) (subvec % (inc i))))
      (.release ^java.util.concurrent.Semaphore (:sem token)))))

(defn coop-yield!
  "The current task yields the turn, then blocks until it is picked
  again."
  [{:keys [monitor ready] :as coop}]
  (let [me *coop-task*]
    (locking monitor
      (swap! ready conj me)
      (coop-pass-turn! coop))
    (.acquire ^java.util.concurrent.Semaphore (:sem me))))

(defn- coop-swap-and-run!
  "Core of the coop swap-and-act stand-ins. f returns a
  [new-data return-value] pair, whose new-data may carry a
  :further-actions field. Commits the swap, then runs the follow-on
  actions. For a dom-manager atom inside a cooperative task it yields
  the turn before the swap, before each action, and after the whole
  cascade completes (just before returning), so another activity can
  interleave at every step; the whole cascade still completes before
  this returns. For a reporter atom (plain-map data), or outside a
  task, it behaves exactly like the normal swap-and-act variants.
  Returns return-value."
  [coop cell f]
  (let [{:keys [rng mid-check injector]} coop]
    (when (and mid-check
               (zero? (.nextInt ^java.util.Random rng 10)))
      (mid-check))
    (when (and injector
               (zero? (.nextInt ^java.util.Random rng 10)))
      (injector)))
  (let [in-task (and *coop-task*
                     (or (instance? ComponentData @cell)
                         (instance? DOMManagerData @cell)))]
    (when in-task (coop-yield! coop))
    (let [[actions return-value]
          (swap-control-return!
           cell
           (fn [data]
             (let [[new-data return-value] (f data)
                   actions (:further-actions new-data)]
               [(if actions (assoc new-data :further-actions nil) new-data)
                [actions return-value]])))]
      (doseq [action actions]
        (when in-task (coop-yield! coop))
        (apply (first action) (rest action)))
      (when in-task (coop-yield! coop))
      return-value)))

(defn coop-swap-and-act!
  "A stand-in for swap-and-act!, whose f returns just the new data."
  [coop cell f]
  (coop-swap-and-run! coop cell (fn [data] [(f data) nil])))

(defn coop-swap-and-act-control-return!
  "A stand-in for swap-and-act-control-return!, whose f returns a
  [new-data return-value] pair."
  [coop cell f]
  (coop-swap-and-run! coop cell f))

(defn coop-run-all-pending-tasks
  "A cooperative stand-in for run-all-pending-tasks: instead of
  busy-waiting for a task that is running on another (parked)
  cooperative thread, it yields the turn so that thread can finish."
  [coop task-queue]
  (loop []
    (if (#'cosheet.task-queue/run-pending-task task-queue false)
      (recur)
      (when-not (finished-all-tasks? task-queue)
        (when *coop-task* (coop-yield! coop))
        (recur)))))

(defn run-coop-tasks!
  "Run each thunk as a cooperative task, interleaving them at their
  yield points, and return when all have finished."
  [{:keys [monitor ready] :as coop} thunks]
  (let [latch (java.util.concurrent.CountDownLatch. (count thunks))]
    (doseq [thunk thunks]
      (let [token {:sem (java.util.concurrent.Semaphore. 0)}]
        (locking monitor (swap! ready conj token))
        (.start (Thread.
                 (fn []
                   (.acquire ^java.util.concurrent.Semaphore (:sem token))
                   (binding [*coop-task* token] (thunk))
                   (.countDown latch)
                   (locking monitor (coop-pass-turn! coop)))))))
    (locking monitor (coop-pass-turn! coop))
    (.await latch)))

(deftest coop-interleaved-stress-test
  (let [h (make-harness)
        cd (:cd h)
        ;; Records any errors spotted by the mid-check running
        ;; mid-cascade. The debug facilities might not be available to
        ;; the thread running mid-check, so it puts the violations
        ;; here, and we check that there are none.
        violations (atom [])
        ;; Additional dom-changing thunks, interjected one at a time
        ;; into the running cascade by the injector.
        delayed-dom-changes (atom [])
        ;; This rng drives all of the test's scheduling: which ready coop
        ;; task runs next, whether the mid-check or injector fires, and
        ;; (below) the tie-breaker among equal-priority queued tasks.
        coop-rng (java.util.Random. 24680)
        coop (make-coop
              coop-rng
              {:mid-check (fn []
                            (record-double-active-client-ids! h violations))
               :injector (fn []
                           (when-let [t (first @delayed-dom-changes)]
                             (swap! delayed-dom-changes (comp vec rest))
                             (t)))})
        n 30 ; number of doms
        k 16 ; number of concurrent tasks
        ids (mapv #(make-item-id (str "n" %)) (range n))
        ;; Every call to spec generates a distinct spec, because it
        ;; creates a new function for the spec's render-dom. So we cache
        ;; one spec to reference each id, letting a node's subcomponents
        ;; be reused across dom changes rather than salvaged every time.
        specs (mapv #(spec h %) ids)
        ;; Generates the doms. Unlike coop-rng, dom-rng is drawn only
        ;; while building the thunks on this thread, never during the
        ;; scheduled interleaving, so its sequence is deterministic.
        dom-rng (java.util.Random. 13579)
        orig-add-task cosheet.task-queue/add-task-with-priority
        ;; Include each potential child with probability
        ;; (min 1/2 3/(n-i)), so a node's expected number of children
        ;; stays bounded (about 3) rather than growing with n. That
        ;; keeps the tree sparse, so the test's cost scales far better.
        random-dom (fn [i]
                     (let [p (min 1/2 (/ 3 (- n i)))]
                       (into [:div {:v (.nextInt dom-rng 1000000)}]
                             (for [j (range (inc i) n)
                                   :when (< (.nextDouble dom-rng) p)]
                               [:component (nth specs j)]))))
        ;; A dom-changing thunk: usually sets a random node's dom;
        ;; occasionally (only for t 0) replaces node 0's spec, salvaging
        ;; its subtree. It just perturbs the dom, as an interjected
        ;; activity does mid-cascade.
        change-thunk (fn [t]
                       (if (and (zero? t) (zero? (.nextInt dom-rng 3)))
                         (let [new-spec (spec h (ids 0))]
                           (fn [] (set-dom! h :root
                                            [:div [:component new-spec]])))
                         (let [i (.nextInt dom-rng n)
                               dom (random-dom i)]
                           (fn [] (set-dom! h (ids i) dom)))))
        ;; Make a thunk that calls its argument, then runs to quiescence.
        act-and-compute-thunk (fn [thunk] (fn []
                                            (thunk)
                                            (compute cd)))]
    (with-redefs [swap-and-act! (fn [cell f] (coop-swap-and-act! coop cell f))
                  swap-and-act-control-return!
                  (fn [cell f]
                    (coop-swap-and-act-control-return! coop cell f))
                  run-all-pending-tasks (fn [tq]
                                          (coop-run-all-pending-tasks coop tq))
                  ;; Give every queued task a random tie-breaker, so
                  ;; equal-priority tasks run in a reproducible (but
                  ;; seed-varied) order rather than the production
                  ;; priority-map's identity-hash order.
                  cosheet.task-queue/add-task-with-priority
                  (fn [task-queue priority & task]
                    (apply orig-add-task task-queue
                           [priority (.nextInt coop-rng)] task))]
      ;; Setup runs inline (no *coop-task*).
      (doseq [i (range n)] (set-dom! h (ids i) (random-dom i)))
      (set-dom! h :root [:div [:component (nth specs 0)]])
      (add-root-dom (:manager h) (spec h :root))
      (compute cd)
      (check-invariants h)
      (is (empty? @violations)
          [:transient-duplicate-active-client-ids @violations])
      (reset! violations [])
      (dotimes [_ 10]
        ;; Fire k concurrent dom changes. Each has to be followed by
        ;; (compute cd), because there is no guarantee which will run
        ;; first, and we need to have a strand running compute once
        ;; all changes are active.
        (let [thunks (vec (for [t (range k)]
                            (act-and-compute-thunk (change-thunk t))))]
          ;; Queue up k more dom changes for the injector to interject
          ;; into the cascade as it runs, so new changes arrive while
          ;; old ones are in flight.
          (reset! delayed-dom-changes
                  (vec (for [t (range k)] (change-thunk t))))
          (run-coop-tasks! coop thunks)
          ;; Run any interjections that never got injected, then quiesce.
          (when-let [delayed-changes @delayed-dom-changes]
            (doseq [t delayed-changes] (t))
            (reset! delayed-dom-changes [])
            (compute cd)
            (run-coop-tasks! coop thunks))
          (check-invariants h)
          (is (empty? @violations)
              [:transient-duplicate-active-client-ids @violations])
          (reset! violations []))))))

(deftest mark-component-tree-as-needed-test
  (let [ms (new-mutable-store (new-element-store))
        cd (make-calculator-data (make-priority-task-queue 0))
        manager (make-dom-manager ms cd)
        c1 (reuse-or-make-component-atom s1 manager nil "c1" 1 nil nil)]
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
        cd (make-calculator-data (make-priority-task-queue 0))
        manager (make-dom-manager ms cd)
        c1 (reuse-or-make-component-atom s1 manager nil "c1" 1 nil nil)]
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
        cd (make-calculator-data (make-priority-task-queue 0))
        manager (make-dom-manager ms cd)]
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
               @manager client1 nil (reporter-value-or-invalid ms))]
      (is (check ad1 {:component c1
                     :subject-ids [id1 id1]}))
      (compute cd)
      (let [c2 (first (vals (:id->subcomponent @c1)))
            c3 (client-id->component @manager client3)
            ad1 (client-id->action-data
               @manager client1 nil (reporter-value-or-invalid ms))
            ad3 (client-id->action-data
                @manager client3 nil (reporter-value-or-invalid ms))]
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

(deftest preferred-selection-test
  ;; Prefer the candidate with the longer common prefix with current-selection.
  (is (= (preferred-selection "abcde" "abcx" "abxy") "abcx"))
  (is (= (preferred-selection "abcde"  "abxy" "abcx") "abcx"))
  ;; On equal prefix length, prefer the longer string.
  (is (= (preferred-selection "abc" "abx" "abyz") "abyz"))
  (is (= (preferred-selection "abc" "abyz" "abx") "abyz"))
  (is (= (preferred-selection nil "xy" "abc") "abc"))
  ;; nil arguments are treated as zero-length.
  (is (= (preferred-selection nil nil "a") "a"))
  (is (= (preferred-selection nil "a" nil) "a"))
  (is (nil? (preferred-selection "abc" nil nil))))

(deftest get-response-doms-and-process-acknowledgements-test
  ;; Also tests add-root-dom, request-client-refresh,
  ;; prepare-dom-for-client and adjust-subdom-for-client
  (let [ms (new-mutable-store (new-element-store))
        cd (make-calculator-data (make-priority-task-queue 0))
        manager (make-dom-manager ms cd)]
    (add-root-dom manager s1-)
    (let [c1- (client-id->component @manager "root")]
      (activate-component c1-)
      (compute cd)
      (let [c1 (first (vals (:id->subcomponent @c1-)))
            c2 (first (vals (:id->subcomponent @c1)))]
        (is (:highest-version @manager) 1)
        (is (check (get-response-doms manager [id2] nil 3)
                   [(as-set [[:div {:id "root" :version 2}
                              2
                              [:component {:id "root_Ifoo_Ibar"}]]
                             [:div {:id "root_Ifoo_Ibar" :version 2}
                              3]])
                    "root_Ifoo_Ibar"]))
        (is (:highest-version @manager) 3)
        (is (check (get-response-doms manager [id2] nil 1)
                   [[[:div {:id "root" :version 2}
                      2
                      [:component {:id "root_Ifoo_Ibar"}]]]
                    nil]))
        ;; With two monitored ids, preferred-selection uses current-selection
        ;; to pick between candidates. :root monitors "root" (via its elided
        ;; subcomponent), id2 monitors "root_Ifoo_Ibar" directly. The deeper
        ;; component has a longer prefix overlap with a current-selection that
        ;; matches it.
        (is (= (second (get-response-doms manager [:root id2]
                                          "root_Ifoo" 3))
               "root_Ifoo_Ibar"))
        ;; With nil current-selection, tie breaks by longer string,
        ;; which still gives the same answer.
        (is (= (second (get-response-doms manager [:root id2] nil 3))
               "root_Ifoo_Ibar"))
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
                   (as-set [(:dom-R @c1-) (:dom-R @c1) (:dom-R @c2)])))))))

(deftest asynchronous-client-interaction-test
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
                    (make-reporter :name [0 i]
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
        cd (make-calculator-data (make-priority-task-queue 4))
        ms (new-mutable-store (new-element-store))
        dm (make-dom-manager ms cd)
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
                (let [for-client (first (get-response-doms dm nil nil 20))]
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
                  (let [dom (prepare-dom-for-client component)]
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
            (check-one-id-and-subcomponents
              [id client-data require-latest being-checked]
              (if (being-checked id)
                ;; A higher call is already considering this id. There
                ;; is a race. Return success.
                0
                (if-let [our-dom (client-data id)]
                  (let [matched (if (check-one-id id our-dom require-latest)
                                  1 0)
                        sub-specs (subcomponent-specifications our-dom)
                        sub-ids (map #(:id %) sub-specs)
                        sub-matches (map #(check-one-id-and-subcomponents
                                           % client-data require-latest
                                           (conj being-checked id))
                                         sub-ids)]
                    (apply + matched sub-matches))
                  0)))
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
                                client-data require-latest #{})
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

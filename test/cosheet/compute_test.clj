(ns cosheet.compute-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [mutable-map :as mm]
                     [task-queue :as task-queue]
                     [state :refer :all]
                     [compute :refer :all]
                     [compute-impl :refer :all])
            ; :reload
            ))

(deftest update-valid-test
  (is (= (update-valid {:value-depends-changed true})
         {:value-depends-changed true}))
  (is (= (update-valid {:uncertain-depends #{5}})
         {:uncertain-depends #{5}}))
  (is (= (update-valid {})
         {:visible {:valid true}})))

(deftest update-add-action-test
  (is (= (update-add-action {1 2} :f 5 6)
         {1 2 :pending-actions [[:f 5 6]]})))

(deftest update-remove-state-test
  (is (= (update-remove-state {}) {}))
  (is (= (update-remove-state {:state 1})
         {:pending-actions [[register-different-state 1]]})))

(deftest update-value-test
  (is (= (update-value {} 1) {:visible {:value 1}}))
  (is (= (update-value {:visible {:value 1}} 2) {:visible {:value 2}})))

(deftest update-result-test
  (let [state (new-state :value 1)
        different-state (new-state :value 2)]
    (is (= (update-result {:visible {:value 1}
                           :value-depends-changed true
                           :state state} 2)
           {:visible {:value 2 :valid true}
            :pending-actions [[register-different-state state]]}))
    (is (= (update-result {:visible {:value 1 :valid true} :state state
                           :pending-actions [1]}
                          different-state)
           {:visible {:value 2 :valid true}
            :state different-state
            :pending-actions
            [1
             [register-different-state state]
             [register-different-state different-state]]}))
    (is (= (update-result {:visible {:value 1}
                           :value-depends-changed true
                           :pending-actions [1]}
                          (expr 1 2 3))
           {:visible {:value 1}
            :value-depends-changed true
            :expression '(1 2 3)
            :depends-info {2 nil 3 nil}
            :uncertain-depends #{2 3}
            :unused-depends #{2 3}
            :pending-actions
            [1 [register-different-depends #{2 3}]]}))
    ;; Now check an expression when there is already depends info
    (is (= (update-result {:visible {:value 1}
                           :depends-info {2 4 8 9}}
                          (expr 1 2 3))
           {:visible {:value 1}
            :expression '(1 2 3)
            :depends-info {2 4 3 nil 8 9}
            :uncertain-depends #{3}
            :unused-depends #{3}
            :pending-actions
            [[register-different-depends #{3}]]}))))

(deftest update-expression-while-ready-test
  (is (= (update-run-expression-while-ready
          {:expression 1 :uncertain-depends #{1}})
         {:expression 1 :uncertain-depends #{1}}))
  (is (= (update-run-expression-while-ready
          {})
         {}))
  (is (= (update-run-expression-while-ready
          {:depends-info {:a {:value 2} :b {:value 3}}
           :expression [(fn [arg] (expr (fn [arg2] [arg arg2])
                                                :a))
                         :b]})
         {:visible {:value [3 2] :valid true}
          :depends-info {:a {:value 2} :b {:value 3}}})))

(deftest update-start-evaluation-test
  (is (= (update-start-evaluation {} [+ 2 3])
         {:visible {:value 5 :valid true}})))

(deftest update-initialize-if-needed-test
  (is (= (update-initialize-if-needed {} [+ 1 2])) {})
  (is (= (update-initialize-if-needed nil [+ 1 2])) {:value 3}))

(deftest update-depends-on-visible-test
  (let [info {:depends-info {:a {:value 2 :valid true}
                             :b {:value 3}
                             :c {:value 5 :valid true}}
              :visible {:value 3}
              :uncertain-depends #{:b}
              :unused-depends #{:a}
              :expression [1 2]}]
    (is (= (update-depends-on-visible {} :a :b (constantly {})) {}))
    (is (= (update-depends-on-visible
            info [3 :d] :a (constantly {:value 4 :valid true}))
           {:depends-info {:a {:value 4 :valid true}
                           :b {:value 3}
                           :c {:value 5 :valid true}}
            :visible {:value 3}
            :uncertain-depends #{:b}
            :unused-depends #{:a}
            :expression [1 2]}))
    (is (= (update-depends-on-visible
            info [3 :d] :a (constantly {:value 4}))
           {:depends-info {:a {:value 4}
                           :b {:value 3}
                           :c {:value 5 :valid true}}
            :visible {:value 3}
            :uncertain-depends #{:b :a}
            :unused-depends #{:a}
            :expression [1 2]}))
    (is (= (update-depends-on-visible
            info [3 :d] :b  (constantly {:value 3 :valid true}))
           {:depends-info {:a {:value 2 :valid true}
                           :b {:value 3 :valid true}
                           :c {:value 5 :valid true}}
            :visible {:value 3 :valid true}
            :unused-depends #{:a}
            :expression [1 2]}))
    (is (= (update-depends-on-visible
            info [(fn [] (expr 3 :d))] :c (constantly {:value 3}))
           {:depends-info {:d nil}
            :visible {:value 3}
            :value-depends-changed true
            :uncertain-depends #{:d}
            :unused-depends #{:d}
            :expression [3 :d]
            :pending-actions
            [[register-different-depends '(:c :b :a)]
             [register-different-depends #{:d}]]}))))

(deftest change-and-schedule-propagation-test
  (let [s (new-approximating-scheduler)
        history (atom [])
        f5 (fn [] 5)
        r1 (fn [scheduler reference arg]
             (swap! history #(conj % :r1))
             (is (= scheduler s))
             (is (= reference :a))
             (is (= arg 3))
             (mm/update! (:references scheduler) :a
                         #(assoc % :expression [f5])))]
    (mm/assoc-in! (:references s) [:a] {:visible {:value 0 :valid true}
                                         :using-references #{:x}})
    (change-and-schedule-propagation
     s :a update-add-action r1 3)
    (is (= (mm/current-contents (:references s))
           {:a {:visible {:value 0 :valid true}
                :expression [f5]
                :using-references #{:x}}}))
    (is (= @history [:r1]))
    (let [pending (first @(:pending s))]
      (is (= (count pending) 0)))
    (comment
      (is (= (first (peek pending)) [handle-depends-on-info-changed :x :a])))))

(deftest register-added-depends-test
  (let [s (new-approximating-scheduler)
        e-mm (:references s)
        not-ready-fn (fn [] (expr +))]
    (mm/assoc-in!
     e-mm [[identity 1]]
     (update-start-evaluation {} [identity 1]))
    (mm/assoc-in! e-mm [[identity 1] :using-references] #{:b})
    (mm/assoc-in!
     e-mm [:a]
     (update-start-evaluation
      {} [(fn [] (expr + [identity 1] [not-ready-fn]))]))
    (register-different-depends s :a [[identity 1] [not-ready-fn]])
    (is (= (mm/get-in! e-mm [[identity 1] :using-references]) #{:a :b}))
    (is (= (mm/get-in! e-mm [[not-ready-fn] :using-references]) #{:a}))
    (let [pending (first @(:pending s))]
      (is (= (count pending) 1))
      (is (= (first (peek pending))
             [copy-visible-to-user [not-ready-fn] :a])))))

(deftest register-removed-depends-test
  (let [s (new-approximating-scheduler)
        e-mm (:references s)]
    (mm/assoc-in! e-mm [:a :depends-info] {:b 1 :c 1})
    (mm/assoc-in! e-mm [:b :using-references] #{:a :e})
    (mm/assoc-in! e-mm [:d :using-references] #{:a :f})
    (register-different-depends s :a [:b :d])
    (is (= (mm/get-in! e-mm [:b :using-references]) #{:a :e}))
    (is (= (mm/get-in! e-mm [:d :using-references]) #{:f}))))

(deftest register-added-removed-state-test
  (let [s (new-approximating-scheduler)
        st1 (new-state :value 1)
        st2 (new-state :value 2)
        e-mm (:references s)]
    (mm/assoc-in! e-mm [:a :state] st1)
    (register-different-state s :a st2)
    (is (empty? @(:subscriptions st1)))
    (register-different-state s :a st1)
    (let [subscriptions @(:subscriptions st1)]
      (is (= (count subscriptions) 1))
      (is (= (rest (first subscriptions)) [s :a]))
      (register-different-state s :a st1)
      (is (= @(:subscriptions st1) subscriptions))
      (mm/assoc-in! e-mm [:a :state] st2)
      (register-different-state s :a st1)
      (is (empty? @(:subscriptions st1))))))

(deftest copy-visible-to-user-test
  (let [s (new-approximating-scheduler)]
    (change-and-schedule-propagation
     s :a (fnil update-start-evaluation {})
     [(fn [] (expr inc [identity 2]))])
    (copy-visible-to-user s [identity 2] :a)
    (let [info (mm/get! (:references s) :a)]
      (is (= (:visible info) {:value 3 :valid true})))))

(deftest copy-state-value-test
  (let [s (new-approximating-scheduler)
        state (new-state :value 1)
        exp [(fn [] state)]]
    (change-and-schedule-propagation s exp update-initialize-if-needed exp)
    (is (= (mm/get-in! (:references s) [exp :visible :value]) 1))
    (state-set state 2)
    (run-all-pending s)
    (is (= (mm/get-in! (:references s) [exp :visible :value]) 2))))

(deftest notifier-value-test
  (let [s (new-approximating-scheduler)]
    (letfn [(fib [n] (if (<= n 1)
                       1
                       (eval-let [f1 [fib (- n 1)]
                                  f2 [fib (- n 2)]]
                                 (+  f1 f2))))]
      (is (= (notifier-value s [fib 6]) 13)))))

(deftest current-value-test
  (let [state (new-state :value 1)]
    (letfn [(fib [n] (if (<= n 1)
                       state
                       (eval-let [f1 [fib (- n 1)]
                                  f2 [fib (- n 2)]]
                                 (+  f1 f2))))]
      (is (= (current-value [fib 6]) 13)))))

(deftest asynchronous-test
  ;; Creates width states, then a series layers of lookups that use the value
  ;; at the previous layer as an index into another value at that
  ;; layer. Makes sure the right results are returned, then starts
  ;; changing the states in one thread, while another thread
  ;; propagates, and makes sure that the final answer is still right
  
  (let [width 13
        depth 7
        trials 100000
        changes-per-trial 1000
        states (vec (for [i (range width)]
                      (new-state :value (mod (inc i) width))))
        s (new-approximating-scheduler)]
    (letfn [(indexer [d pos]
              (if (zero? d)
                (states pos)
                (eval-let [index [indexer (- d 1) pos]
                           value [indexer (- d 1) index]]
                          value)))
            (expected [d pos]
              (if (zero? d)
                (state-value (states pos))
                (expected (- d 1) (expected (- d 1) pos))))
            (right-results? []
              (doseq [d (range depth)
                      pos (range width)]
                (is (= (notifier-value s [indexer d pos])
                       (expected d pos)))))]
      (doseq [pos (range width)
              d (range depth)]
        (request s [indexer d pos]))
      (run-all-pending s)
      (right-results?)
      (doseq [i (range trials)]
        (when (= (mod i 1000) 0)
          (println "starting trial" i))
        (doseq [j (range changes-per-trial)]
          (state-set (states (mod (* i j) width)) (mod (* j j) width))
          (when (zero? (mod j 17))
            (future (run-all-pending s))))
        (future (run-all-pending s))
        (task-queue/wait-until-finished (:pending s))
        (right-results?)))))

(comment "How to do local redefinitions in a rest"
  (def ^:dynamic counter 0)
  (deftest conflicts-mutable-map-test
    (let [mm (new-mutable-map)
          orig-update-non-nil update-non-nil
          orig-add-mutable-value add-mutable-value]
      (binding [counter 0]
        (with-redefs [update-non-nil
                      (fn [va f]
                        (set! counter (inc counter))
                        (if (not= (mod counter 5) 0)
                          (swap! va (fn [x] nil)))
                        (orig-update-non-nil va f))
                      add-mutable-value
                      (fn [mm key value]
                        (set! counter (inc counter))
                        (if (not= (mod counter 6) 0)
                          (swap! mm (fn [m] (assoc m key (atom 3)))))
                        (orig-add-mutable-value mm key value))]
          (update! mm :foo (fn [x] 5))
          (is (= (get! mm :foo)) 5)
          (is (< 5 counter 15)))))))




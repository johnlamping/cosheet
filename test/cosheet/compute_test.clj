(ns cosheet.compute-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [mutable-map :as mm]
                     [task-queue :as task-queue]
                     [state :refer :all]
                     [utils :refer :all]
                     [compute :refer :all]
                     [compute-impl :refer :all])
            ; :reload
            ))

(deftest update-add-action-test
  (is (= (update-add-action {1 2} :f 5 6)
         {1 2 :requested-actions [[:f 5 6]]})))

(deftest update-value-test
  (is (= (update-value {} 1)
         {:visible {:value 1 :valid true}
          :requested-actions [[schedule-copy-visible-to-all-users]]}))
  (is (= (update-value {:visible {:value 1 :valid true}} 2)
         {:visible {:value 2 :valid true}
          :requested-actions [[schedule-copy-visible-to-all-users]]}))
  (is (= (update-value {:visible {:value 2 :valid true}} 2)
         {:visible {:value 2 :valid true}})))

(deftest update-invalid-test
  (is (= (update-invalid {:visible {:value 1 :valid true}})
         {:visible {:value 1}
          :requested-actions [[schedule-copy-visible-to-all-users]]}))
    (is (= (update-invalid {:visible {:value 1}})
         {:visible {:value 1}})))

(deftest update-reference-test
  (let [expression (expr :a (expr :b))
        reference [:a :b]
        added (update-reference {1 2} expression reference)]
    (is (= added
           {1 2
            :expressions {expression {:reference reference}}
            :depends-info {reference nil}
            :using-expressions {reference #{expression}}
            :requested-actions [[register-different-depends [reference]]]}))
    (is (= (update-reference added expression nil)
           {1 2
            :requested-actions [[register-different-depends [reference]]
                              [register-different-depends [reference]]]}))))

(deftest update-initialize-test
  (let [reference [:a :b]
        subexpression (make-expression nil :b)
        expression (make-expression nil :a subexpression)
        expression-fn (constantly expression)
        state (new-state :value 1)
        state-fn (constantly state)
        value "value"
        value-fn (constantly value)]
    (is (= (update-initialize {1 2} [value-fn])
           {1 2
            :visible {:value value :valid true}
            :requested-actions [[schedule-copy-visible-to-all-users]]}))
    (is (= (update-initialize {1 2} [state-fn])
           {1 2
            :visible {:value 1 :valid true}
            :state state
            :requested-actions [[schedule-copy-visible-to-all-users]
                              [register-different-state state]]}))
    (is (= (update-initialize {1 2} [expression-fn])
           {1 2 :expression expression
            :expressions {expression {:uncertain-depends #{subexpression}}
                          subexpression {:reference [:b]
                                         :using-expressions #{expression}}}
            :depends-info {[:b] nil}
            :using-expressions {[:b] #{subexpression}}
            :requested-actions [[register-different-depends [[:b]]]]}))))

(deftest update-using-reference-test
  (is (= (update-using-reference
        {1 2} [:a :b] [:c :d] {[:a :b] nil})
       {1 2 :using-references #{[:c :d]}}))
  (let [starting {1 2 :using-references #{[:e :f]}}
        added (update-using-reference
               starting [:a :b] [:c :d] {[:a :b] nil})]
    (is (= added
           {1 2 :using-references #{[:e :f] [:c :d]}}))
    (is (= (update-using-reference
            added [:a :b] [:c :d] {})
           starting))
    (is (= (update-using-reference
            starting [:a :b] [:e :f] {})
           {1 2}))))

(deftest update-depends-on-visible-test
  (let [subexpression (make-expression nil :b)
        expression (make-expression nil :a subexpression)
        info (update-initialize nil [(constantly expression)])
        update1 (update-depends-on-visible
                 info [:b]  {:value :b :valid true})
        update2 (update-depends-on-visible
                 update1 [:a :b] {:value 5 :valid true})]
    (is (= update1
           (-> info
               (assoc-in [:depends-info [:b]] {:value :b :valid true})
               (assoc-in [:depends-info [:a :b]] nil)
               (assoc-in [:using-expressions [:a :b]]
                         #{expression})
               (assoc-in [:expressions expression :reference] [:a :b])
               (dissoc-in [:expressions expression :uncertain-depends])
               (update-in [:requested-actions]
                          #(conj % [register-different-depends [[:a :b]]])))))
    (is (= update2
           (-> update1
               (assoc-in [:visible] {:value 5 :valid true})
               (assoc-in [:depends-info [:a :b]] {:value 5 :valid true})
               (update-in [:requested-actions]
                          #(conj %
                                 [schedule-copy-visible-to-all-users])))))
    (is (= (update-depends-on-visible
            update2 [:a :b] {:value 5})
           (-> update1
               (assoc-in[:depends-info [:a :b]] {:value 5})
               (assoc-in [:visible]  {:value 5})
               (update-in [:requested-actions]
                          #(conj %
                                 [schedule-copy-visible-to-all-users]
                                 [schedule-copy-visible-to-all-users])))))
    (is (= (update-depends-on-visible
            update2 [:b]  {:value :b})
           (-> info
               (assoc-in [:depends-info [:b]] {:value :b})
               (assoc-in [:visible]  {:value 5})
               (update-in [:requested-actions]
                          #(conj %
                                 [register-different-depends [[:a :b]]]
                                 [schedule-copy-visible-to-all-users]
                                 [register-different-depends [[:a :b]]]
                                 [schedule-copy-visible-to-all-users])))))
    (is (= (update-depends-on-visible
            update2 [:b] {:value :c :valid true})
           (-> update1
               (assoc-in [:visible]  {:value 5})
               (assoc-in [:depends-info [:b]] {:value :c :valid true})
               (assoc-in [:depends-info [:a :c]] nil)
               (dissoc-in [:depends-info [:a :b]])
               (assoc-in [:using-expressions [:a :c]]
                         #{expression})
               (dissoc-in [:using-expressions [:a :b]])
               (assoc-in [:expressions expression :reference] [:a :c])
               (update-in [:requested-actions]
                          #(conj %
                                 [schedule-copy-visible-to-all-users]
                                 [register-different-depends [[:a :b]]]
                                 [register-different-depends [[:a :c]]]
                                 [schedule-copy-visible-to-all-users]))))))
  ;; Now try a repeated subexpression, and with deeper nesting.
  (let [repeated (make-expression nil :b)
        intermediate (make-expression nil :a repeated)
        expression (make-expression nil intermediate repeated)
        info (update-initialize nil [(constantly expression)])
        update1 (update-depends-on-visible
                 info [:b] {:value :b :valid true})
        update2 (update-depends-on-visible
                 update1 [:a :b] {:value :a :valid true})]
    (is (= update1
           (-> info
               (assoc-in [:depends-info [:b]] {:value :b :valid true})
               (assoc-in [:depends-info [:a :b]] nil)
               (assoc-in [:using-expressions [:a :b]]
                         #{intermediate})
               (assoc-in [:expressions intermediate :reference] [:a :b])
               (dissoc-in [:expressions intermediate :uncertain-depends])
               (assoc-in [:expressions expression :uncertain-depends]
                         #{intermediate})
               (update-in [:requested-actions]
                          #(conj % [register-different-depends [[:a :b]]])))))
    (is (= update2
           (-> update1
               (assoc-in [:using-expressions [:a :b]]
                         #{intermediate expression})
               (assoc-in [:expressions expression :reference] [:a :b])
               (assoc-in [:visible] {:value :a :valid true})
               (assoc-in [:depends-info [:a :b]] {:value :a :valid true})
               (dissoc-in [:expressions expression :uncertain-depends])
               (update-in [:requested-actions]
                          #(conj % [schedule-copy-visible-to-all-users])))))
    (is (= (update-depends-on-visible update2 [:b] {:value :b})
           (-> info
               (assoc-in [:depends-info [:b]] {:value :b})
               (assoc-in [:visible]  {:value :a})
               (update-in [:requested-actions]
                          #(conj %
                                 [register-different-depends [[:a :b]]]
                                 [schedule-copy-visible-to-all-users]
                                 [register-different-depends [[:a :b]]]
                                 [schedule-copy-visible-to-all-users])))))))

(deftest update-initialize-if-needed-test
  (is (= (update-initialize-if-needed {} [+ 1 2])) {})
  (is (= (update-initialize-if-needed nil [+ 1 2])) {:value 3}))

(deftest change-and-run-requested-actions-test
  (let [s (new-approximating-scheduler)
        history (atom [])
        r1 (fn [scheduler reference arg]
             (swap! history #(conj % :r1))
             (is (= scheduler s))
             (is (= reference :a))
             (is (= arg 3))
             (mm/update! (:references scheduler) :a
                         update-value 0))]
    (change-and-run-requested-actions
     s :a update-add-action r1 3)
    (is (= (mm/current-contents (:references s))
           {:a {:visible {:value 0 :valid true}}}))
    (is (= @history [:r1]))
    (let [pending (first @(:pending s))]
      (is (= (count pending) 0)))))

(deftest register-added-depends-test
  (let [s (new-approximating-scheduler)
        r-mm (:references s)]
    (mm/assoc-in! r-mm [[identity 1]]
                  (-> (update-initialize {} [identity 1])
                      (dissoc :requested-actions)))
    (mm/assoc-in! r-mm [[:foo]] {:visible {:value 1}})
    (mm/assoc-in! r-mm [[identity 1] :using-references] #{:b})
    (mm/assoc-in! r-mm [:a :depends-info] {[identity 1] nil
                                           [:foo] nil})
    (register-different-depends s :a [[identity 1] [:foo]])
    (is (= (mm/get-in! r-mm [[identity 1] :using-references]) #{:a :b}))
    (is (= (mm/get-in! r-mm [[:foo] :using-references]) #{:a}))
    (let [pending (first @(:pending s))]
      (is (= (count pending) 2))
      (is (= (set pending)
             #{[[copy-visible-to-user [identity 1] :a] 0]
               [[copy-visible-to-user [:foo] :a] 0]})))))

(deftest register-removed-depends-test
  (let [s (new-approximating-scheduler)
        r-mm (:references s)]
    (mm/assoc-in! r-mm [:a :depends-info] {:b 1 :c 1})
    (mm/assoc-in! r-mm [:b :using-references] #{:a :e})
    (mm/assoc-in! r-mm [:d :using-references] #{:a :f})
    (register-different-depends s :a [:b :d])
    (is (= (mm/get-in! r-mm [:b :using-references]) #{:a :e}))
    (is (= (mm/get-in! r-mm [:d :using-references]) #{:f}))))

(deftest register-added-removed-state-test
  (let [s (new-approximating-scheduler)
        st1 (new-state :value 1)
        st2 (new-state :value 2)
        r-mm (:references s)]
    (mm/assoc-in! r-mm [:a :state] st1)
    (register-different-state s :a st2)
    (is (empty? @(:subscriptions st1)))
    (register-different-state s :a st1)
    (let [subscriptions @(:subscriptions st1)]
      (is (= (count subscriptions) 1))
      (is (= (rest (first subscriptions)) [s :a]))
      (register-different-state s :a st1)
      (is (= @(:subscriptions st1) subscriptions))
      (mm/assoc-in! r-mm [:a :state] st2)
      (register-different-state s :a st1)
      (is (empty? @(:subscriptions st1))))))

(deftest copy-visible-to-user-test
  (let [s (new-approximating-scheduler)]
    (change-and-run-requested-actions
     s :a update-initialize [(fn [] (expr identity 2))])
    (copy-visible-to-user s [identity 2] :a)
    (let [info (mm/get! (:references s) :a)]
      (is (= (:visible info) {:value 2 :valid true})))))

(deftest copy-state-value-test
  (let [s (new-approximating-scheduler)
        state (new-state :value 1)
        ref [(fn [] state)]]
    (change-and-run-requested-actions s ref update-initialize-if-needed ref)
    (is (= (mm/get-in! (:references s) [ref :visible :value]) 1))
    (state-set state 2)
    (run-all-pending s)
    (is (= (mm/get-in! (:references s) [ref :visible :value]) 2))))

(deftest macros-test
  (letfn [(clean-tracers [exp]
            (if (expression? exp)
              (do (assert (= ((expression-tracer exp) (constantly "test"))
                             "test"))
                  (apply make-expression nil (clean-tracers (expression-fn exp))
                         (map clean-tracers (expression-args exp))))
              exp))]
    (is (= (clean-tracers (expr 1 2 3)) (make-expression nil 1 2 3)))
    (let [test-exp (expr-let [a (expr 2)] (a 3))]
      (is (= (map clean-tracers (expression-args test-exp))
             [(clean-tracers (expr 2))]))
      (is (= (clean-tracers ((expression-fn test-exp) inc)) 4)))
    (is (= (clean-tracers (expr-map :f [1 2]))
           (clean-tracers (expr vector (expr :f 1) (expr :f 2)))))))

(deftest notifier-value-test
  (let [s (new-approximating-scheduler)
        state (new-state :value 0)]
    (letfn [(fib [n] (if (<= n 1)
                       (expr identity state)
                       (expr-let [f1 (expr fib (- n 1))
                                  f2 (expr fib (- n 2))]
                                 (+  f1 f2))))]
      ;; Since the state is 0, fib should be 0 everywhere, and since
      ;; the computations should be cached, this should be fast.
      (is (= (notifier-value s [fib 37]) 0))
      (state-set state 1)
      ;; Now it should be the right value.
      (is (= (notifier-value s [fib 37]) 39088169)))))

(deftest current-value-test
  (let [state (new-state :value 1)]
    (letfn [(fib [n] (if (<= n 1)
                       state
                       (expr + (expr fib (- n 1)) (expr fib (- n 2)))))]
      (is (= (current-value [fib 6]) 13)))))

(deftest asynchronous-test
  ;; Creates width states, then a series layers of lookups that use the value
  ;; at the previous layer as an index into another value at that
  ;; layer. Makes sure the right results are returned, then starts
  ;; changing the states in one thread, while another thread
  ;; propagates, and makes sure that the final answer is still right
  
  (let [width 13
        depth 7
        trials 10; 0000
        changes-per-trial 1000
        states (vec (for [i (range width)]
                      (new-state :value (mod (inc i) width))))
        s (new-approximating-scheduler)]
    (letfn [(indexer [d pos]
              (if (zero? d)
                (states pos)
                (expr-let [index (expr indexer (- d 1) pos)
                           value (expr indexer (- d 1) index)]
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




(ns cosheet.compute-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [mutable-map :as mm]
                     [compute :refer :all]
                     [compute-impl :refer :all])
            :reload))

(defrecord TestState
    [value subscriptions]
  State
  (state-value [this] @value)
  (subscribe [this callback]
    (swap! subscriptions #(conj % (if (fn? callback) [callback] callback))))
  (unsubscribe [this callback]
    (swap! subscriptions #(disj % (if (fn? callback) [callback] callback)))))

(defn new-test-state [value]
  (->TestState (atom value) (atom #{})))

(deftest valid?-test
  (is (valid? {:value-depends-unchanged true}))
  (is (not (valid? {})))
  (is (not (valid? nil)))
  (is (not (valid? {:value-depends-unchanged true
                    :uncertain-depends #{5}}))))

(deftest equal-propagated-info?-test
  (is (equal-propagated-info? {:value 1}
                              {:value 1
                               :uncertain-depends #{3 4}}))
  (is (not (equal-propagated-info? {:value 2
                                    :value-depends-unchanged true}
                                   {:value 1
                                    :value-depends-unchanged true})))
  (is (not (equal-propagated-info? {:value 1
                                    :value-depends-unchanged true}
                                   {:value 1
                                    :value-depends-unchanged true
                                    :uncertain-depends #{3 4}})))
  (is (not (equal-propagated-info? {:value 1}
                                   {:value 1
                                    :value-depends-unchanged true}))))

(deftest update-add-registration-test
  (is (= (update-add-registration {1 2} [3 4] :f 5 6)
         {1 2 :pending-registrations [[[3 4] :f 5 6]]})))

(deftest update-remove-state-test
  (is (= (update-remove-state {}) {}))
  (is (= (update-remove-state {:state 1})
         {:pending-registrations [[[:state] register-removed-state 1]]})))

(deftest update-value-test
  (is (= (update-value {} 1) {:value 1}))
  (is (= (update-value {:value 2} 1) {:value 1})))

(deftest update-value-from-state-test
  (let [state (new-test-state 1)]
    (is (= (update-value-from-state {:state state})
           {:value 1 :state state}))))

(deftest update-result-test
  (let [state (new-test-state 1)
        new-state (new-test-state 2)]
    (is (= (update-result {:value 1 :state state} 2)
           {:value 2
            :value-depends-unchanged true
            :pending-registrations [[[:state] register-removed-state state]]}))
    (is (= (update-result {:value 1 :state state
                           :value-depends-unchanged true
                           :pending-registrations [1]}
                          new-state)
           {:value 2
            :state new-state
            :value-depends-unchanged true
            :pending-registrations
            [1
             [[:state] register-removed-state state]
             [[:state] register-added-state new-state]]}))
    (is (= (update-result {:value 1
                           :pending-registrations [1]}
                          (application 1 2 3))
           {:value 1
            :application '(1 2 3)
            :depends-info {2 nil 3 nil}
            :uncertain-depends #{2 3}
            :unused-depends #{2 3}
            :pending-registrations
            [1 [[:depends-info] register-added-depends #{2 3}]]}))
    ;; Now check an application when there is already depends info
    (is (= (update-result {:value 1
                           :depends-info {2 4 8 9}}
                          (application 1 2 3))
           {:value 1
            :application '(1 2 3)
            :depends-info {2 4 3 nil 8 9}
            :uncertain-depends #{3}
            :unused-depends #{3}
            :pending-registrations
            [[[:depends-info] register-added-depends #{3}]]}))))

(deftest update-application-while-ready-test
  (is (= (update-run-application-while-ready
          {:application 1 :uncertain-depends #{1}})
         {:application 1 :uncertain-depends #{1}}))
  (is (= (update-run-application-while-ready
          {:application 1 :pending-registrations [1]})
         {:application 1 :pending-registrations [1]}))
  (is (= (update-run-application-while-ready
          {})
         {}))
  (is (= (update-run-application-while-ready
          {:depends-info {:a {:value 2} :b {:value 3}}
           :application [(fn [arg] (application (fn [arg2] [arg arg2])
                                                :a))
                         :b]})
         {:value [3 2]
          :value-depends-unchanged true
          :depends-info {:a {:value 2} :b {:value 3}}})))

(deftest update-start-evaluation-test
  (is (= (update-start-evaluation {} [+ 2 3])
         {:value 5 :value-depends-unchanged true})))

(deftest update-initialize-if-needed-test
  (is (= (update-initialize-if-needed {} [+ 1 2])) {})
  (is (= (update-initialize-if-needed nil [+ 1 2])) {:value 3}))

(deftest update-depends-on-info-changed-test
  (let [info {:depends-info {:a {:value 2 :value-depends-unchanged true}
                             :b {:value 3 :value-depends-unchanged true}
                             :c {:value 5 :value-depends-unchanged true}}
              :value 3
              :value-depends-unchanged true
              :uncertain-depends #{:b}
              :unused-depends #{:a}
              :application [1 2]}]
    (is (= (update-depends-on-info-changed {} :a :b {}) {}))
    (is (= (update-depends-on-info-changed
            info [3 :d] :a {:value 4 :value-depends-unchanged true})
           {:depends-info {:a {:value 4 :value-depends-unchanged true}
                           :b {:value 3 :value-depends-unchanged true}
                           :c {:value 5 :value-depends-unchanged true}}
            :value 3
            :value-depends-unchanged true
            :uncertain-depends #{:b}
            :unused-depends #{:a}
            :application [1 2]}))
    (is (= (update-depends-on-info-changed
            info [3 :d] :a {:value 4 :value-depends-unchanged true
                            :uncertain-depends #{3}})
           {:depends-info {:a {:value 4 :value-depends-unchanged true
                               :uncertain-depends #{3}}
                           :b {:value 3 :value-depends-unchanged true}
                           :c {:value 5 :value-depends-unchanged true}}
            :value 3
            :value-depends-unchanged true
            :uncertain-depends #{:b :a}
            :unused-depends #{:a}
            :application [1 2]}))
    (is (= (update-depends-on-info-changed
            info [3 :d] :b  {:value 3 :value-depends-unchanged true})
           {:depends-info {:a {:value 2 :value-depends-unchanged true}
                           :b {:value 3 :value-depends-unchanged true}
                           :c {:value 5 :value-depends-unchanged true}}
            :value 3
            :value-depends-unchanged true
            :unused-depends #{:a}
            :application [1 2]}))
    (is (= (update-depends-on-info-changed
            info [(fn [] (application 3 :d))] :c {:value 3})
           {:depends-info {:d nil}
            :value 3
            :uncertain-depends #{:d}
            :unused-depends #{:d}
            :application [3 :d]
            :pending-registrations
            [[[:depends-info] register-removed-depends '(:c :b :a)]
             [[:depends-info] register-added-depends #{:d}]]}))))

(deftest change-and-schedule-propagation-test
  (let [s (new-approximating-scheduler)
        history (atom [])
        r2 (fn [current scheduler expression arg]
             (swap! history #(conj % [:r2 current]))
             (is (= current 4))
             (is (= scheduler s))
             (is (= expression :a))
             (is (= arg 3))
             (mm/update! (:expressions scheduler) :a
                         #(assoc % :application [(fn [] 5)])))
        r1 (fn [current scheduler expression arg]
             (swap! history #(conj % [:r1 current]))
             (is (or (= current 0) (= current 4)))
             (is (= scheduler s))
             (is (= expression :a))
             (is (= arg 2))
             (mm/update! (:expressions scheduler) :a
                         #(-> %
                              (update-value 4)
                              (update-add-registration [:value] r2 3))))]
    (mm/assoc-in! (:expressions s) [:a] {:value 0 :using-expressions #{:x}})
    (change-and-schedule-propagation
     s :a update-add-registration [:value] r1 2)
    (is (= (mm/current-contents (:expressions s))
           {:a {:value 5
                :value-depends-unchanged true
                :using-expressions #{:x}}}))
    (is (= history) [[:r1 0] [:r1 4] [:r2 4]])
    (let [pending @(:pending s)]
      (is (= (count pending) 1))
      (is (= (first (peek pending)) [handle-depends-on-info-changed :x :a])))))

(deftest register-added-depends-test
  (let [s (new-approximating-scheduler)
        e-mm (:expressions s)
        not-ready-fn (fn [] (application +))]
    (mm/assoc-in!
     e-mm [[identity 1]]
     (update-start-evaluation {} [identity 1]))
    (mm/assoc-in! e-mm [[identity 1] :using-expressions] #{:b})
    (mm/assoc-in!
     e-mm [:a]
     (update-start-evaluation
      {} [(fn [] (application + [identity 1] [not-ready-fn]))]))
    (register-added-depends (:depends-info (mm/get! e-mm :a)) s :a
                            [[identity 1] [not-ready-fn]])
    (is (= (mm/get-in! e-mm [[identity 1] :using-expressions]) #{:a :b}))
    (is (= (mm/get-in! e-mm [[not-ready-fn] :using-expressions]) #{:a}))
    (let [pending @(:pending s)]
      (is (= (count pending) 2))
      (is (= #{(first (peek pending))
               (first (peek (pop pending)))}
             #{[handle-depends-on-info-changed :a [identity 1]]
               [handle-depends-on-info-changed :a [not-ready-fn]]})))))

(deftest register-removed-depends-test
  (let [s (new-approximating-scheduler)
        e-mm (:expressions s)]
    (mm/assoc-in! e-mm [:a :depends-info] {:b 1 :c 1})
    (mm/assoc-in! e-mm [:b :using-expressions] #{:a :e})
    (mm/assoc-in! e-mm [:d :using-expressions] #{:a :f})
    (register-removed-depends (mm/get-in! e-mm [:a :depends-info]) s :a
                              [:b :d])
    (is (= (mm/get-in! e-mm [:b :using-expressions]) #{:a :e}))
    (is (= (mm/get-in! e-mm [:d :using-expressions]) #{:f}))))

(deftest register-added-removed-state-test
  (let [s (new-approximating-scheduler)
        st1 (new-test-state 1)
        st2 (new-test-state 2)
        e-mm (:expressions s)]
    (mm/assoc-in! e-mm [:a :state] st1)
    (register-added-state (mm/get-in! e-mm [:a :state]) s :a st2)
    (is (empty? @(:subscriptions st1)))
    (register-added-state (mm/get-in! e-mm [:a :state]) s :a st1)
    (let [subscriptions @(:subscriptions st1)]
      (is (= (count subscriptions) 1))
      (is (= (rest (first subscriptions)) [s :a]))
      (register-removed-state (mm/get-in! e-mm [:a :state]) s :a st1)
      (is (= @(:subscriptions st1) subscriptions))
      (mm/assoc-in! e-mm [:a :state] st2)
      (register-removed-state (mm/get-in! e-mm [:a :state]) s :a st1)
      (is (empty? @(:subscriptions st1))))))


(deftest handle-depends-on-info-changed-test
  (let [s (new-approximating-scheduler)]
    (change-and-schedule-propagation
     s :a (fnil update-start-evaluation {})
     [(fn [] (application inc [identity 2]))])
    (handle-depends-on-info-changed s :a [identity 2])
    (let [info (mm/get! (:expressions s) :a)]
      (is (= (:value info) 3))
      (is (valid? info)))))

;;; TODO: write handle-state-changed-test

(deftest current-value-test
  (let [s (new-approximating-scheduler)]
    (letfn [(fib [n] (if (<= n 1)
                       1
                       (application + [fib (- n 1)] [fib (- n 2)])))]
      (is (= (current-value s [fib 6]) 13)))))

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




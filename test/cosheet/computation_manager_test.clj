(ns cosheet.computation-manager-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [mutable-map :as mm]
                     [task-queue :as task-queue]
                     [reporter :as reporter]
                     [utils :refer :all]
                     [computation-manager :refer :all])
            ; :reload
            ))

(deftest modify-and-act-test
  (let [r (reporter/new-reporter :test 10)
        a (atom 1)]
    (modify-and-act r (fn [data]
                        (-> data
                            (update-in [:test] inc)
                            (assoc :pending-actions [[swap! a inc]]))))
    (is (= (:test (reporter/data r)) 11))
    (is (= @a 2))))

(deftest copy-value-test
  (let [r1 (reporter/new-reporter :value :v)
        r2 (reporter/new-reporter :value-source r1)]
    (register-copy-value r1 r2)
    (is (= (reporter/value r2) :v))
    (reporter/set-value! r1 :w)
    (is (= (reporter/value r2) :w))
    (swap! (reporter/data-atom r2) dissoc :value-source)
    (register-copy-value r1 r2)
    (reporter/set-value! r1 :x)
    (is (= (reporter/value r2) :w))))

(deftest copy-subordinate-test
  (let [r0 (reporter/new-reporter :name :r0 :value :r0)
        r1 (reporter/new-reporter :name :r1 :value :v)
        r2 (reporter/new-reporter :name :r2
                                  :value :x
                                  :needed-values #{r1}
                                  :subordinate-values {})
        r3 (reporter/new-reporter :name :r3 :value-source r2)
        m (new-management)]
    ;; Register, and check that the information is copied.
    (register-copy-subordinate r1 r2 m)
    (is (= (:needed-values (reporter/data r2)) #{}))
    (is (= (:subordinate-values (reporter/data r2)) {r1 :v}))
    (is (= (reporter/value r2) :x))
    (is (= (task-queue/current-tasks (:queue m))
           [[eval-expression-if-ready r2 m]]))
    ;; Change the subordinate value, and make sure it got propagated.
    (reset! (:queue m) @(task-queue/new-priority-task-queue))
    (reporter/set-value! r1 :v1)
    (is (= (:needed-values (reporter/data r2)) #{}))
    (is (= (:subordinate-values (reporter/data r2)) {r1 :v1}))
    (is (= (reporter/value r2) :x))
    (is (= (task-queue/current-tasks (:queue m))
           [[eval-expression-if-ready r2 m]]))
    ;; Send a spurious update, and make sure things are unchanged.
    (reset! (:queue m) @(task-queue/new-priority-task-queue))
    (reporter/inform-attendees r1)
    (is (= (:needed-values (reporter/data r2)) #{}))
    (is (= (:subordinate-values (reporter/data r2)) {r1 :v1}))
    (is (= (reporter/value r2) :x))
    (is (= (task-queue/current-tasks (:queue m)) ()))
    ;; Now pretend that we did the eval and got a value-source,
    ;; then change the input to undefined, and check all the consequences.
    (reset! (:queue m) @(task-queue/new-priority-task-queue))
    (swap! (reporter/data-atom r2) assoc :value-source r0)
    (register-copy-value r0 r2)
    (register-copy-value r2 r3)
    (is (= (reporter/value r3) :r0))
    (reporter/set-value! r1 reporter/invalid)
    (is (= (reporter/value r2) reporter/invalid))
    (is (= (reporter/value r3) reporter/invalid))
    (is (not (contains? (reporter/data r2) :value-source)))
    (is (= (:attendees (reporter/data r0)) nil))
    (is (= (task-queue/current-tasks (:queue m)) ()))))

(deftest eval-expression-if-ready-test
  (let [r0 (reporter/new-reporter  :name :r0 :value 1)
        r1 (reporter/new-reporter :name :r1
                                  :value 3
                                  :expression [inc 2]
                                  :manager-type :eval)
        r (reporter/new-reporter :name :r
                                 :expression [inc r0]
                                 :needed-values #{r1}
                                 :subordinate-values {r0 1})
        rc (reporter/new-reporter :name :rc :value-source r)
        m (new-management)]
    (register-copy-value r rc)
    ;; Try when the expression is not ready.
    (eval-expression-if-ready r m)
    (is (= (reporter/value r) reporter/invalid))
    (swap! (reporter/data-atom r) assoc :needed-values #{})
    ;; Try when it is ready and computes a constant.
    (eval-expression-if-ready r m)
    (is (= (reporter/value r) 2))
    (is (= (reporter/value rc) 2))
    ;; Try when it is ready and computes a reporter.
    (swap! (reporter/data-atom r)
           #(-> %
                (assoc :subordinate-values {r0 (fn [] r1)})
                (assoc :expression [r0])
                (assoc :value-source r0)))
    (register-copy-value r0 r)
    (is (= (:attendees (reporter/data r0))
           {[:copy-value r] [copy-value-callback]}))
    (is (= (:manager (reporter/data r1)) nil))
    (eval-expression-if-ready r m)
    (is (= (reporter/value r) 3))
    (is (= (reporter/value rc) 3))
    (is (= (:attendees (reporter/data r0)) nil))
    (is (= (:manager (reporter/data r1)) [eval-manager m]))))

(deftest eval-manager-test
  (let [r0 (reporter/new-reporter :value 1)
        r (reporter/new-reporter :expression [inc r0]
                                 :value 2
                                 :manager-type :eval)
        rc (reporter/new-reporter :value-source r)
        m (new-management)]
    ;; Run manager when there is no interest.
    (eval-manager r m)
    (is (= (reporter/value r) reporter/invalid))
    ;; Run when there is interest.
    (register-copy-value r rc)
    (eval-manager r m)
    (is (= (:needed-values (reporter/data r)) #{}))
    (is (= (:subordinate-values (reporter/data r)) {r0 1}))
    ;; Run when there is no interest again.
    (swap! (reporter/data-atom rc) dissoc :value-source)
    (register-copy-value r rc)
    (eval-manager r m)
    (is (not (contains? (reporter/data r) :needed-values)))
    (is (not (contains? (reporter/data r) :subordinate-values)))
    (is (empty? (:attendees (reporter/data r0))))))




(ns cosheet2.application-calculator-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet2 [mutable-map :as mm]
                      [task-queue :refer [current-tasks
                                          new-priority-task-queue]]
                      [reporter :refer [invalid new-reporter
                                        reporter-data reporter-value
                                        reporter-atom set-value! set-attendee!
                                        remove-attendee! inform-attendees
                                        universal-category]]
                      [calculator :refer [new-calculator-data current-value
                                          compute request]]
                      [expression :refer [expr]]
                      [utils :refer :all]
                      [application-calculator :refer :all]
                      [test-utils :refer [check any]]
                      [propagation-test-utils :refer [check-propagation]])
            ; :reload
            ))

(deftest copy-value-test
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        r1 (new-reporter :value :v)
        r2 (new-reporter :value-source r1
                         :value-source-priority-delta 1
                         :calculator-data cd)
        r3 (new-reporter :old-value-source r1
                         :value-source-priority-delta 1
                         :calculator-data cd)]
    (register-copy-value r2 r1 cd)
    (compute cd)
    (is (= (reporter-value r2) :v))
    (set-value! r1 :w)
    (compute cd)
    (is (= (reporter-value r2) :w))
    (swap! (reporter-atom r2) dissoc :value-source)
    (register-copy-value r2 r1 cd)
    (compute cd)
    (set-value! r1 :x)
    (compute cd)
    (is (= (reporter-value r2) :w))
    (register-demand-old-value r3 r1 cd)
    (compute cd)
    ;; r3 had r1 as its old source, so the copy value should throw away
    ;; the result.
    (is (= (reporter-value r3) invalid))
    (is (check
         ((:attendees (reporter-data r1)) [:demand-old-value r3])
         [Double/MAX_VALUE [universal-category] null-callback]))))

(deftest copy-subordinate-test
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        r0 (new-reporter :name :r0 :value :r0)
        r1 (new-reporter :name :r1 :value :v :dependent-depth 1)
        r2 (new-reporter :name :r2
                         :value :x
                         :dependent-depth 1
                         :needed-values #{r1}
                         :application [identity r1]
                         :subordinate-values {}
                         :calculator-data cd)
        r3 (new-reporter :name :r3
                         :value-source r2
                         :value-source-priority-delta 1
                         :calculator-data cd)
        ;; Since cd is immutable, we can't replace its queue, but we
        ;; can set the content of its queue to the content of a fresh queue.
        clear-cd-queue #(reset! (:queue cd) @(new-priority-task-queue 0))]
    ;; Give the ultimate reporters demand.
    (set-attendee! r2 :k 0 (constantly nil))
    (set-attendee! r3 :k 0 (constantly nil))
    ;; Register, and check that the information is copied.
    (register-copy-subordinate r2 r1 cd)
    (compute cd)
    (is (= (:needed-values (reporter-data r2)) #{}))
    (is (check (:subordinate-values (reporter-data r2)) {r1 [:v (any)]}))
    (is (= (reporter-value r2) :v))
    (set-value! r1 :v1)
    (compute cd)
    (is (= (:needed-values (reporter-data r2)) #{}))
    (is (check (:subordinate-values (reporter-data r2)) {r1 [:v1 (any)]}))
    (is (= (reporter-value r2) :v1))1
    (inform-attendees r1)
    (is (= (:needed-values (reporter-data r2)) #{}))
    (is (check (:subordinate-values (reporter-data r2)) {r1 [:v1 (any)]}))
    (is (= (reporter-value r2) :v1))
    ;; Now pretend that we did the eval and got a value-source,
    ;; then change the input to undefined, and check all the consequences.
    (clear-cd-queue)
    (swap! (reporter-atom r2)
           assoc :value-source r0 :value-source-priority-delta 1)
    (register-copy-value r2 r0 cd)
    (register-copy-value r3 r2 cd)
    (compute cd)
    (is (= (reporter-value r3) :r0))
    (set-value! r1 invalid)
    (compute cd)
    (is (= (reporter-value r2) invalid))
    (is (= (reporter-value r3) invalid))
    (is (= (:value-source (reporter-data r2)) nil))
    (is (= (:old-value-source (reporter-data r2)) r0)) ;; Last known source.
    (is (= (current-tasks (:queue cd)) ()))
    ;; Now set the value back to the original value, and check the consequences.
    ;; We don't need to compute, because it is just value copying.
    (set-value! r1 :v1)
    (compute cd)
    (is (= (:value-source (reporter-data r2)) r0)) ;; Same source.
    (is (= (reporter-value r2) :r0))
    (is (= (reporter-value r3) :r0))))

(deftest run-application-if-ready-test
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        r0 (new-reporter :name :r0 :value 1)
        r1 (new-reporter :name :r1
                         :calculator application-calculator
                         :value 3
                         :dependent-depth 0
                         :application [inc 2])
        r (new-reporter :name :r
                        :application [inc r0]
                        :needed-values #{r0}
                        :subordinate-values {r0 [1 0]}
                        :calculator-data cd)
        rc (new-reporter :name :rc
                         :value-source r
                         :value-source-priority-delta 1
                         :calculator-data cd)]
    ;; Give rc priority 6
    (set-attendee! rc :test 6 (fn [& _] nil))
    (register-copy-value rc r cd)
    (is (= (:priority (reporter-data r)) 7))
   ;; Try when the application is not ready.
    (run-application-if-ready r cd)
    (is (= (reporter-value r) invalid))
    (is (= (:dependent-depth (reporter-data r)) nil))
    (swap! (reporter-atom r) assoc :needed-values #{})
    ;; Try when it is ready and computes a constant.
    (run-application-if-ready r cd)
    (is (= (reporter-value r) 2))
    (is (= (:dependent-depth (reporter-data r)) 1))
    (compute cd)
    (is (= (reporter-value rc) 2))
    (is (= (:dependent-depth (reporter-data r)) 1))    
    ;; Try when it is ready and computes a reporter.
    (swap! (reporter-atom r0)
           #(into % {:value (fn [] r1)}))
    (swap! (reporter-atom r)
           #(into % {:subordinate-values {r0 [(fn [] r1) 0]}
                     :application [r0]
                     :value-source r1
                     :value-source-priority-delta 2}))
    (register-copy-value r r1 cd)
    (is (= (:calculator-data (reporter-data r1)) nil))
    (compute cd)
    (is (= (:dependent-depth (reporter-data r)) 2))    
    (is (check (:attendees (reporter-data r1))
               {[:copy-value r] [(+ 6 3) [universal-category] (any)]}))
    (swap! (reporter-atom r)
           #(into % {:value-source nil
                     :value invalid}))
    (run-application-if-ready r cd)
    ;; r won't get a value yet, because it doesn't have the value from r1.
    (is (= (reporter-value r) invalid))
    ;; Propagate the value
    (compute cd)
    (is (= (reporter-value rc) 3))
    (is (= (:attendees (reporter-data r0)) nil))
    (is (= (:calculator-data (reporter-data r1)) cd))
    (run-application-if-ready r1 cd)
    (is (= (reporter-value r) 3))
    (compute cd)
    (is (= (reporter-value rc) 3))))

(deftest application-calculator-test
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        r0 (new-reporter :name :r0
                         :value 1)
        r (new-reporter :name :r
                        :application [inc r0]
                        :calculator application-calculator
                        :calculator-data cd)
        rc (new-reporter :name :rc
                         :value 2
                         :application [identity r]
                         :value-source r
                         :value-source-priority-delta 1
                         :calculator application-calculator
                         :calculator-data cd)]
    ;; Run manager when there is no interest.
    (do-application-calculate r cd)
    (compute cd)
    (is (= (reporter-value r) invalid))
    ;; Run when there is interest.
    ;; Give rc priority 6
    (set-attendee! rc :test 6 (fn [& _] nil))
    (register-copy-value rc r cd)
    (do-application-calculate r cd)
    (compute cd)
    (is (= (:needed-values (reporter-data r)) #{}))
    (is (= (:subordinate-values (reporter-data r)) {r0 [1 0]}))
    (run-application-if-ready r cd)
    (is (= (reporter-value r) 2))
    (is (= (:dependent-depth (reporter-data r)) 1))    
    ;; Run when there is no interest again.
    (remove-attendee! rc :test)
    (register-copy-value rc r cd)
    (do-application-calculate r cd)
    (compute cd)
    (is (not (contains? (reporter-data r) :needed-values)))
    (is (not (contains? (reporter-data r) :subordinate-values)))
    (is (empty? (:attendees (reporter-data r0))))))

(deftest nil-value-test
  ;; There had been a bug with nil values throwing off propagation.
  ;; This tests that it is fixed.
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        r0 (new-reporter :name :r0)
        r1 (expr identity r0)
        r2 (expr identity r1)]
    (request r2 cd)
    (compute cd)
    (is (= (reporter-value r2) invalid))
    (set-value! r0 nil)
    (compute cd)
    (is (= (reporter-value r2) nil))
    (set-value! r0 1)
    (compute cd)
    (is (= (reporter-value r2) 1))))

(deftest asynchronous-test
  ;; Creates width base reporters, then a series layers of lookups
  ;; that use the value at the previous layer as an index into another
  ;; value at that layer. Makes sure the right results are returned,
  ;; then starts changing the base values in one thread, while another
  ;; thread propagates, and makes sure that the final answer is still
  ;; right.
  (let [width 37
        depth 11
        trials 10 ;100000
        changes-per-trial 1000
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
                                    (expr ^{:name [d i]}
                                        nth prev (nth prev i)))
                                  (range width))]
                        (recur (+ d 1) current (conj reporters current)))))
        cd (new-calculator-data (new-priority-task-queue 4))
        evals (atom 0)]
    (letfn [(answers [arguments]
               (reduce
                (fn [answers-map [d pos]]
                  (assoc answers-map [d pos]
                         (if (zero? d)
                           (reporter-value (base pos))
                           (answers-map [(- d 1)
                                         (answers-map [(- d 1) pos])]))))
                {}
                arguments))
            (right-results? [requests answers-map]
              (doseq [[[d pos] reporter] requests]
                (is (= (reporter-value reporter)
                       (answers-map [d pos])))))
            (check [requests answers-map]
              (right-results? requests answers-map)
              ;; Note: almost all of the time of this test occurs
              ;; here, not in the computation, because each (is ...)
              ;; in the check does a transaction on the test counter.
              (check-propagation (vals requests)))]
      (let [arguments (for [d (range depth)
                            pos (range width)]
                        [d pos])
            requests (into {} (for [[d pos] arguments]
                                [[d pos]
                                 (request (nth (nth reporters d) pos)
                                          cd)]))]
        (compute cd 1000000)
        (check requests (answers arguments))
        (doseq [i (range trials)]
          (when (= (mod i 100) 0)
            (println "starting trial" i))
          (doseq [j (range changes-per-trial)]
            (set-value! (base (mod (* (inc i) j) width))
                                 (mod (* j j) width))
            (when (zero? (mod j 17))
              (future (compute cd (mod i 34)))))
          (compute cd)
          (check requests (answers arguments)))))))



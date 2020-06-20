(ns cosheet2.application-calculator-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet2 [mutable-map :as mm]
                      [task-queue :refer [current-tasks
                                          new-priority-task-queue]]
                      [reporter :as reporter]
                      [calculator :refer [new-calculator-data current-value
                                          compute request]]
                      [expression :refer [expr expr-seq expr-let invalid]]
                      [utils :refer :all]
                      [application-calculator :refer :all]
                      [test-utils :refer [check any]])
            ; :reload
            ))

;;; NOTE: The next three functions are not currently used, but have been
;;; useful in avoiding huge print-outs in debugging.

(defn pretty-expression
  [expr show-prev]
  (if (reporter/reporter? expr)
    (let [desc
          (for [key [:name :value]]
            [key (key (reporter/data expr))])]
      (if (nil? (second (first desc)))
        (let [[key [priority & callback]]
              (first (:attendees (reporter/data expr)))]
          (let [to (if (sequential? key) (second key) key)]
            (if (reporter/reporter? to)
              (concat [[:name [(:name (reporter/data to))]]]
                      (rest desc))
              desc)))
        desc))
    (if (sequential? expr)
      (for [e expr]
        (pretty-expression e show-prev))
      expr)))

(defn print-reporter
  [r]
  (doseq [key [:name :calculator :value]]
    (let [v (key (reporter/data r))]
      (when v
        (println " " key v))))
  (let [source (:value-source (reporter/data r))]
    (when source
      (println " " :value-source (:name (reporter/data source)))))
  (doseq [[rep value] (:subordinate-values (reporter/data r))]
    (println "    Stored" value
             "for" (pretty-expression rep false) (reporter/value rep)))
  (doseq [key (keys (:attendees (reporter/data r)))]
    (println "    callback" (pretty-expression key true))))

(defn print-sources
  [r]
  (print-reporter r)
  (let [source (:value-source (reporter/data r))]
    (when source
      (print "It's source:")
      (print-sources source))))

(defn fib [n s]
  (if (<= n 1)
    s   
    (expr + (fib (- n 1) s) (fib (- n 2) s))))

;;; These functions check that exactly the right callbacks are in
;;; place, and that all copied information is up to date.

(defn check-source-propagation
  [need-checking reporter]
  (let [data (reporter/data reporter)
        source (:value-source data)]
    (if source
      (do (is (= (:value data) (reporter/value source)))
          (is (check (get-in (reporter/data source)
                             [:attendees `(:copy-value ~reporter)])
                     [(any #(> % (:priority data)))
                      [reporter/universal-category]
                      (any)]))
          (conj need-checking source))
      need-checking)))

(defn check-old-source-propagation
  [need-checking reporter]
  (let [data (reporter/data reporter)
        old-source (:old-value-source data)]
    (if old-source
      (do (is (= (:value data) invalid))
          (is (check (get-in (reporter/data old-source)
                             [:attendees `(:copy-value ~reporter)])
                     [(any)
                      [reporter/universal-category]
                      (any)]))
          (conj need-checking old-source))
      need-checking)))

(defn check-subordinate-propagation
  [need-checking reporter]
  (let [data (reporter/data reporter)]
    (reduce
     (fn [need-checking [subordinate [value dependent-depth]]]
       (if (contains? (:needed-values data) subordinate)
         need-checking
         (do
           (is (= value (reporter/value subordinate)))
           (is (contains? (:attendees (reporter/data subordinate)) reporter))
           (conj need-checking subordinate))))
     need-checking
     (:subordinate-values data))))

(defn check-needed-propagation
  [need-checking reporter]
  (let [data (reporter/data reporter)]
    (reduce
     (fn [checked needed]
       (is (not (reporter/valid? (reporter/value needed))))
       (is (contains? (:attendees (reporter/data needed)) reporter))
       (conj need-checking needed))
     need-checking
     (:needed-values data))))

(defn check-attendees
  [need-checking reporter]
  (reduce
   (fn [need-checking [key _]]
     (cond
       (reporter/reporter? key)
       (let [data (reporter/data key)]
         (is (or
              (contains? (:subordinate-values data) reporter)
              (contains? (:needed-values data) reporter)))
         (conj need-checking key))
       (list? key)
       (let [user (second key)
             data (reporter/data user)]
         (is (= (first key) :copy-value))
         (is (reporter/reporter? user))
         (is (or (= reporter (:value-source data))
                 (= reporter (:old-value-source data))))
         (conj need-checking user))
       :else
       need-checking))
   need-checking
   (:attendees (reporter/data reporter))))

(defn check-propagation-for-one-reporter
  "Check that this reporter reflects everything it should,
  and return a list of reporters reachable from this one."
  [reporter]
  (let [data (reporter/data reporter)]
      (is (= (set (filter reporter/reporter? (:application data)))
             (clojure.set/union (set (keys (:subordinate-values data)))
                                (:needed-values data))))
      (is (not (empty? (:attendees data))))
      (-> []
          (check-source-propagation reporter)
          (check-old-source-propagation reporter)
          (check-subordinate-propagation reporter)
          (check-needed-propagation reporter)
          (check-attendees reporter))))

(defn check-propagation
  "Check that all the right information has been propagated to any reporter
  reachable from this one."
  ;; We can't do a simple recursion that checks reporters as we hear
  ;; about them, without possibly blowing out the stack. So, instead,
  ;; as we check, we accumulate a list of reporters we hear about, and
  ;; use a loop to check them until we have no more to check.
  [reporter-or-reporters]
  (loop [already-checked #{}
         need-to-check (if (reporter/reporter? reporter-or-reporters)
                         [reporter-or-reporters]
                         reporter-or-reporters)]
    (when (not (empty? need-to-check))
      (let [[reporter & rest] need-to-check]
        (if (contains? already-checked reporter)
          (recur already-checked rest)
          (recur (conj already-checked reporter)
                 (concat rest
                         (check-propagation-for-one-reporter reporter))))))))

(deftest modify-and-act-test
  (let [r (reporter/new-reporter :test 10)
        a (atom 1)]
    (modify-and-act r (fn [data]
                        (-> data
                            (update-in [:test] inc)
                            (assoc :further-actions [[swap! a inc]]))))
    (is (= (:test (reporter/data r)) 11))
    (is (= @a 2))))

(deftest copy-value-test
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        r1 (reporter/new-reporter :value :v)
        r2 (reporter/new-reporter :value-source r1)
        r3 (reporter/new-reporter :old-value-source r1)]
    (register-copy-value r1 r2 cd)
    (compute cd)
    (is (= (reporter/value r2) :v))
    (reporter/set-value! r1 :w)
    (compute cd)
    (is (= (reporter/value r2) :w))
    (swap! (reporter/data-atom r2) dissoc :value-source)
    (register-copy-value r1 r2 cd)
    (compute cd)
    (reporter/set-value! r1 :x)
    (compute cd)
    (is (= (reporter/value r2) :w))
    (register-copy-value r1 r3 cd)
    (compute cd)
    (is (= (reporter/value r3) invalid))
    (is (check
         ((:attendees (reporter/data r1)) [:copy-value r3])
         [Double/MAX_VALUE [reporter/universal-category] null-callback]))))

(deftest copy-subordinate-test
  (let [r0 (reporter/new-reporter :name :r0 :value :r0)
        r1 (reporter/new-reporter :name :r1 :value :v :dependent-depth 1)
        r2 (reporter/new-reporter :name :r2
                                  :value :x
                                  :dependent-depth 1
                                  :needed-values #{r1}
                                  :application [identity r1]
                                  :subordinate-values {})
        r3 (reporter/new-reporter :name :r3 :value-source r2)
        queue (new-priority-task-queue 0)
        cd (new-calculator-data queue)
        ;; Since cd is immutable, we can't replace its queue, but we
        ;; can set the content of its queue to the content of a fresh queue.
        clear-cd-queue #(reset! (:queue cd) @(new-priority-task-queue 0))]
    ;; Give the ultimate reporters demand.
    (reporter/set-attendee! r2 :k 0 (constantly nil))
    (reporter/set-attendee! r3 :k 0 (constantly nil))
    ;; Register, and check that the information is copied.
    (register-copy-subordinate r1 r2 cd)
    (compute cd)
    (is (= (:needed-values (reporter/data r2)) #{}))
    (is (check (:subordinate-values (reporter/data r2)) {r1 [:v (any)]}))
    (is (= (reporter/value r2) :v))
    (reporter/set-value! r1 :v1)
    (compute cd)
    (is (= (:needed-values (reporter/data r2)) #{}))
    (is (check (:subordinate-values (reporter/data r2)) {r1 [:v1 (any)]}))
    (is (= (reporter/value r2) :v1))1
    (reporter/inform-attendees r1)
    (is (= (:needed-values (reporter/data r2)) #{}))
    (is (check (:subordinate-values (reporter/data r2)) {r1 [:v1 (any)]}))
    (is (= (reporter/value r2) :v1))
    ;; Now pretend that we did the eval and got a value-source,
    ;; then change the input to undefined, and check all the consequences.
    (clear-cd-queue)
    (swap! (reporter/data-atom r2) assoc :value-source r0)
    (register-copy-value r0 r2 cd)
    (register-copy-value r2 r3 cd)
    (compute cd)
    (is (= (reporter/value r3) :r0))
    (reporter/set-value! r1 invalid)
    (compute cd)
    (is (= (reporter/value r2) invalid))
    (is (= (reporter/value r3) invalid))
    (is (= (:value-source (reporter/data r2)) nil))
    (is (= (:old-value-source (reporter/data r2)) r0)) ;; Last known source.
    (is (= (current-tasks (:queue cd)) ()))
    ;; Now set the value back to the original value, and check the consequences.
    ;; We don't need to compute, because it is just value copying.
    (reporter/set-value! r1 :v1)
    (compute cd)
    (is (= (:value-source (reporter/data r2)) r0)) ;; Same source.
    (is (= (reporter/value r2) :r0))
    (is (= (reporter/value r3) :r0))))

(deftest run-application-if-ready-test
  (let [r0 (reporter/new-reporter :name :r0 :value 1)
        r1 (reporter/new-reporter :name :r1
                                  :value 3
                                  :dependent-depth 0
                                  :application [inc 2]
                                  :calculator application-calculator)
        r (reporter/new-reporter :name :r
                                 :application [inc r0]
                                 :needed-values #{r0}
                                 :subordinate-values {r0 [1 0]})
        rc (reporter/new-reporter :name :rc :value-source r)
        cd (new-calculator-data (new-priority-task-queue 0))]
    ;; Give rc priority 6
    (reporter/set-attendee! rc :test 6 (fn [& _] nil))
    (register-copy-value r rc cd)
    (is (= (:priority (reporter/data r)) 7))
   ;; Try when the application is not ready.
    (run-application-if-ready r cd)
    (is (= (reporter/value r) invalid))
    (is (= (:dependent-depth (reporter/data r)) nil))
    (swap! (reporter/data-atom r) assoc :needed-values #{})
    ;; Try when it is ready and computes a constant.
    (run-application-if-ready r cd)
    (is (= (reporter/value r) 2))
    (is (= (:dependent-depth (reporter/data r)) 1))
    (compute cd)
    (is (= (reporter/value rc) 2))
    (is (= (:dependent-depth (reporter/data r)) 1))    
    ;; Try when it is ready and computes a reporter.
    (swap! (reporter/data-atom r)
           #(into % {:subordinate-values {r0 [(fn [] r1) 0]}
                     :application [r0]
                     :value-source r1}))
    (register-copy-value r1 r cd)
    (compute cd)
    (is (= (:dependent-depth (reporter/data r)) 2))    
    (is (check (:attendees (reporter/data r1))
               {[:copy-value r] [(+ 6 3) [reporter/universal-category] (any)]}))
    (is (= (:calculator-data (reporter/data r1)) nil))
    (reporter/set-value! r invalid)
    (run-application-if-ready r cd)
    ;; r won't get a value yet, because r1 is invalid.
    (is (= (reporter/value r) invalid))
    (compute cd)
    (is (= (reporter/value rc) 3))
    (is (= (:attendees (reporter/data r0)) nil))
    (is (= (:calculator-data (reporter/data r1)) cd))
    (run-application-if-ready r1 cd)
    (is (= (reporter/value r) 3))
    (compute cd)
    (is (= (reporter/value rc) 3))))

(deftest application-calculator-test
  (let [r0 (reporter/new-reporter :value 1)
        r (reporter/new-reporter :application [inc r0]
                                 :calculator application-calculator)
        rc (reporter/new-reporter :value-source r)
        cd (new-calculator-data (new-priority-task-queue 0))]
    ;; Give rc priority 6
    (reporter/set-attendee! rc :test 6 (fn [& _] nil))
    ;; Run manager when there is no interest.
    (do-application-calculate r cd)
    (compute cd)
    (is (= (reporter/value r) invalid))
    ;; Run when there is interest.
    (register-copy-value r rc cd)
    (do-application-calculate r cd)
    (compute cd)
    (is (= (:needed-values (reporter/data r)) #{}))
    (is (= (:subordinate-values (reporter/data r)) {r0 [1 0]}))
    (run-application-if-ready r cd)
    (is (= (reporter/value r) 2))
    (is (= (:dependent-depth (reporter/data r)) 1))    
    ;; Run when there is no interest again.
    (swap! (reporter/data-atom rc) dissoc :value-source)
    (register-copy-value r rc cd)
    (do-application-calculate r cd)
    (compute cd)
    (is (not (contains? (reporter/data r) :needed-values)))
    (is (not (contains? (reporter/data r) :subordinate-values)))
    (is (empty? (:attendees (reporter/data r0))))))

(deftest nil-value-test
  ;; There had been a bug with nil values throwing off propagation.
  ;; This tests that it is fixed.
  (let [cd (new-calculator-data (new-priority-task-queue 0))
        r0 (reporter/new-reporter :name :r0)
        r1 (expr identity r0)
        r2 (expr identity r1)]
    (request r2 cd)
    (compute cd)
    (is (= (reporter/value r2) invalid))
    (reporter/set-value! r0 nil)
    (compute cd)
    (is (= (reporter/value r2) nil))
    (reporter/set-value! r0 1)
    (compute cd)
    (is (= (reporter/value r2) 1))))

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
                    (reporter/new-reporter :name [0 i]
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
                           (reporter/value (base pos))
                           (answers-map [(- d 1)
                                         (answers-map [(- d 1) pos])]))))
                {}
                arguments))
            (right-results? [requests answers-map]
              (doseq [[[d pos] reporter] requests]
                (is (= (reporter/value reporter)
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
            (reporter/set-value! (base (mod (* (inc i) j) width))
                                 (mod (* j j) width))
            (when (zero? (mod j 17))
              (future (compute cd (mod i 34)))))
          (compute cd)
          (check requests (answers arguments)))))))



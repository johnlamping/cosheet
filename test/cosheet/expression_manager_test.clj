(ns cosheet.expression-manager-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [mutable-map :as mm]
                     [task-queue :refer [current-tasks new-priority-task-queue]]
                     [reporter :as reporter]
                     [expression :refer [expr expr-seq expr-let cache invalid]]
                     [utils :refer :all]
                     [expression-manager :refer :all]
                     [test-utils :refer [check any]]
                     [debug :refer (simplify-for-print)])
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
  (doseq [key [:name :manager-type :value]]
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

(deftest current-value-test
  (let [state (reporter/new-reporter :value 0)
        fib6 (fib 6 state)]
    (is (= (current-value fib6) 0))
    (reporter/set-value! state 1)
    (is (= (current-value fib6) 13))))

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
                     (into [(any #(> % (:priority data) )) ]
                           [copy-value-callback (any)])))
          (conj need-checking source))
      need-checking)))

(defn check-old-source-propagation
  [need-checking reporter]
  (let [data (reporter/data reporter)
        old-source (:old-value-source data)]
    (if old-source
      (do (is (= (:value data) invalid))
          (is (= (get-in (reporter/data old-source)
                         [:attendees `(:copy-value ~reporter)])
                 [null-callback]))
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
      (is (= (set (filter reporter/reporter? (:expression data)))
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
  ;; about them without possibly blowing out the stack, so, instead,
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
  (let [md (new-expression-manager-data (new-priority-task-queue 0))
        r1 (reporter/new-reporter :value :v)
        r2 (reporter/new-reporter :value-source r1)
        r3 (reporter/new-reporter :old-value-source r1)]
    (register-copy-value r1 r2 md)
    (compute md)
    (is (= (reporter/value r2) :v))
    (reporter/set-value! r1 :w)
    (compute md)
    (is (= (reporter/value r2) :w))
    (swap! (reporter/data-atom r2) dissoc :value-source)
    (register-copy-value r1 r2 md)
    (compute md)
    (reporter/set-value! r1 :x)
    (compute md)
    (is (= (reporter/value r2) :w))
    (register-copy-value r1 r3 md)
    (compute md)
    (is (= (reporter/value r3) invalid))
    (is (check ((:attendees (reporter/data r1)) [:copy-value r3])
               (into [Double/MAX_VALUE] [null-callback])))))

(deftest copy-subordinate-test
  (let [r0 (reporter/new-reporter :name :r0 :value :r0)
        r1 (reporter/new-reporter :name :r1 :value :v :dependent-depth 1)
        r2 (reporter/new-reporter :name :r2
                                  :value :x
                                  :dependent-depth 1
                                  :needed-values #{r1}
                                  :subordinate-values {})
        r3 (reporter/new-reporter :name :r3 :value-source r2)
        queue (new-priority-task-queue 0)
        md (new-expression-manager-data queue)
        ;; Since md is immutable, we can't replace its queue, but we
        ;; can set the content of its queue to the content of a fresh queue.
        clear-md-queue #(reset! (:queue md) @(new-priority-task-queue 0))]
    ;; Give the ultimate reporters demand.
    (reporter/set-attendee! r2 :k 0 (constantly nil))
    (reporter/set-attendee! r3 :k 0 (constantly nil))
    ;; Register, and check that the information is copied.
    (register-copy-subordinate r1 r2 md)
    (is (= (:needed-values (reporter/data r2)) #{}))
    (is (check (:subordinate-values (reporter/data r2)) {r1 [:v (any)]}))
    (is (= (reporter/value r2) invalid))
    (is (= (current-tasks (:queue md))
           [[eval-expression-if-ready r2 md]]))
    ;; Change the subordinate value, and make sure it got propagated.
    (clear-md-queue)
    (reporter/set-value! r1 :v1)
    (is (= (:needed-values (reporter/data r2)) #{}))
    (is (check (:subordinate-values (reporter/data r2)) {r1 [:v1 (any)]}))
    (is (= (reporter/value r2) invalid))
    (is (= (current-tasks (:queue md))
           [[eval-expression-if-ready r2 md]]))
    ;; Send a spurious update, and make sure things are unchanged.
    (clear-md-queue)
    (reporter/inform-attendees r1)
    (is (= (:needed-values (reporter/data r2)) #{}))
    (is (check (:subordinate-values (reporter/data r2)) {r1 [:v1 (any)]}))
    (is (= (reporter/value r2) invalid))
    (is (= (current-tasks (:queue md)) ()))
    ;; Now pretend that we did the eval and got a value-source,
    ;; then change the input to undefined, and check all the consequences.
    (clear-md-queue)
    (swap! (reporter/data-atom r2) assoc :value-source r0)
    (register-copy-value r0 r2 md)
    (register-copy-value r2 r3 md)
    (compute md)
    (is (= (reporter/value r3) :r0))
    (reporter/set-value! r1 invalid)
    (compute md)
    (is (= (reporter/value r2) invalid))
    (is (= (reporter/value r3) invalid))
    (is (= (:value-source (reporter/data r2)) nil))
    (is (= (:old-value-source (reporter/data r2)) r0)) ;; Last known source.
    (is (= (current-tasks (:queue md)) ()))
    ;; Now set the value back to the original value, and check the consequences.
    ;; We don't need to compute, because it is just value copying.
    (reporter/set-value! r1 :v1)
    (is (= (:value-source (reporter/data r2)) r0)) ;; Same source.
    (compute md)
    (is (= (reporter/value r2) :r0))
    (is (= (reporter/value r3) :r0))))

(deftest eval-expression-if-ready-test
  (let [r0 (reporter/new-reporter :name :r0 :value 1)
        r1 (reporter/new-reporter :name :r1
                                  :value 3
                                  :dependent-depth 0
                                  :expression [inc 2]
                                  :manager-type :eval)
        r (reporter/new-reporter :name :r
                                 :expression [inc r0]
                                 :needed-values #{r0}
                                 :subordinate-values {r0 [1 0]})
        rc (reporter/new-reporter :name :rc :value-source r)
        md (new-expression-manager-data (new-priority-task-queue 0))]
    ;; Give rc priority 6
    (reporter/set-attendee! rc :test 6 (fn [key r] nil))
    (register-copy-value r rc md)
    (is (= (:priority (reporter/data r)) 7))
   ;; Try when the expression is not ready.
    (eval-expression-if-ready r md)
    (is (= (reporter/value r) invalid))
    (is (= (:dependent-depth (reporter/data r)) nil))
    (swap! (reporter/data-atom r) assoc :needed-values #{})
    ;; Try when it is ready and computes a constant.
    (eval-expression-if-ready r md)
    (is (= (reporter/value r) 2))
    (is (= (:dependent-depth (reporter/data r)) 1))
    (compute md)
    (is (= (reporter/value rc) 2))
    (is (= (:dependent-depth (reporter/data r)) 1))    
    ;; Try when it is ready and computes a reporter.
    (swap! (reporter/data-atom r)
           #(into % {:subordinate-values {r0 [(fn [] r1) 0]}
                     :expression [r0]
                     :value-source r1}))
    (register-copy-value r1 r md)
    (compute md)
    (is (= (:dependent-depth (reporter/data r)) 2))    
    (is (check (:attendees (reporter/data r1))
               {[:copy-value r] (into [(+ 6 3)] [copy-value-callback md])}))
    (is (= (:manager (reporter/data r1)) nil))
    (reporter/set-value! r invalid)
    (eval-expression-if-ready r md)
    ;; r won't get a value yet, because r1 is invalid.
    (is (= (reporter/value r) invalid))
    (compute md)
    (is (= (reporter/value rc) 3))
    (is (= (:attendees (reporter/data r0)) nil))
    (is (= (:manager (reporter/data r1)) [eval-manager md]))
    (eval-expression-if-ready r1 md)
    (is (= (reporter/value r) 3))
    (compute md)
    (is (= (reporter/value rc) 3))))

(deftest eval-manager-test
  (let [r0 (reporter/new-reporter :value 1)
        r (reporter/new-reporter :expression [inc r0]
                                 :manager-type :eval)
        rc (reporter/new-reporter :value-source r)
        md (new-expression-manager-data (new-priority-task-queue 0))]
    ;; Give rc priority 6
    (reporter/set-attendee! rc :test 6 (fn [key r] nil))
    ;; Run manager when there is no interest.
    (eval-manager r md)
    (is (= (reporter/value r) invalid))
    ;; Run when there is interest.
    (register-copy-value r rc md)
    (eval-manager r md)
    (is (= (:needed-values (reporter/data r)) #{}))
    (is (= (:subordinate-values (reporter/data r)) {r0 [1 0]}))
    (eval-expression-if-ready r md)
    (is (= (reporter/value r) 2))
    (is (= (:dependent-depth (reporter/data r)) 1))    
    ;; Run when there is no interest again.
    (swap! (reporter/data-atom rc) dissoc :value-source)
    (register-copy-value r rc md)
    (eval-manager r md)
    (is (not (contains? (reporter/data r) :needed-values)))
    (is (not (contains? (reporter/data r) :subordinate-values)))
    (is (empty? (:attendees (reporter/data r0))))))

(deftest ensure-in-cache-test
  (let [md (new-expression-manager-data (new-priority-task-queue 0))
        r0 (ensure-in-cache [:a :b] "ab" md)
        r1 (ensure-in-cache [:a :c] "bc" md)
        r2 (ensure-in-cache [:a :b] "changed" md)]
    (is (= r0 r2))
    (is (= (:expression (reporter/data r0)) [:a :b]))
    (is (= (:manager-type (reporter/data r0)) :cached-eval))
    (is (not= r0 r1))))

(deftest cache-manager-test
  (let [md (new-expression-manager-data (new-priority-task-queue 0))
        r0 (reporter/new-reporter :name :r0 :value 1)
        r1 (reporter/new-reporter :name :r1
                                  :expression [inc r0]
                                  :manager-type :cache)
        r2 (reporter/new-reporter :name :r2
                                  :expression [inc r0]
                                  :manager-type :cache)
        r3 (reporter/new-reporter :name :r3 :value-source r1)
        r4 (reporter/new-reporter :name :r4 :value-source r2)]
    ;; Nothing should happen when there is no demand for r1.
    (cache-manager r1 md)
    (is (not (contains? (reporter/data r1) :value-source)))
    ;; With demand, the cached value should be created.
    (register-copy-value r1 r3 md)
    (cache-manager r1 md)
    (is (contains? (reporter/data r1) :value-source))
    ;; And we should pick it up for the other reporter with the same expr.
    (register-copy-value r2 r4 md)
    (cache-manager r2 md)
    (is (= (:value-source  (reporter/data r1))
           (:value-source  (reporter/data r2))))
    (let [orig-source (:value-source  (reporter/data r1))]
      ;; Lose interest in r1 then get it back, and the same value
      ;; source should come back.
      (swap! (reporter/data-atom r3) dissoc :value-source)
      (register-copy-value r1 r3 md)
      (cache-manager r1 md)
      (cached-eval-manager orig-source md)
      (is (not (contains? (reporter/data r1) :value-source)))
      (swap! (reporter/data-atom r3) assoc :value-source r1)
      (register-copy-value r1 r3 md)
      (cache-manager r1 md)
      (is (= (:value-source (reporter/data r1))
             orig-source))
      ;; Now, lose interest in both reporters with that expression,
      ;; and the cache should drop it.
      (swap! (reporter/data-atom r3) dissoc :value-source)
      (register-copy-value r1 r3 md)
      (cache-manager r1 md)
      (swap! (reporter/data-atom r4) dissoc :value-source)
      (register-copy-value r2 r4 md)
      (cache-manager r2 md)
      (cached-eval-manager orig-source md)
      (swap! (reporter/data-atom r3) assoc :value-source r1)
      (register-copy-value r1 r3 md)
      (cache-manager r1 md)
      (is (not= (:value-source (reporter/data r1))
                orig-source)))))

;;; Test that caching is working by doing a recursive computation that would
;;; take a very long time if it weren't cached.
(deftest fib-cache-test
  (let [md (new-expression-manager-data (new-priority-task-queue 0))
        base (reporter/new-reporter :value 0)]
    (letfn [(fib [n] (if (<= n 1)
                       base
                       (expr + (cache fib (- n 1)) (cache fib (- n 2)))))]
      ;; Since the base is 0, fib should be 0 everywhere, and since
      ;; the computations should be cached, this should be fast.
      (let [f45 (fib 45)]
        (compute md)
        (is (= (computation-value f45 md) 0))
        (check-propagation f45)
        (reporter/set-value! base 1)
        (compute md)
        ;; Now it should be the right value.
        (is (= (computation-value f45 md) 1836311903))
        (check-propagation f45)
        (reporter/set-value! base invalid)
        ;;; Now it should be invalid.
        (is (= (not (reporter/valid? (computation-value f45 md)))))
        (check-propagation f45)))))

;; Test that caching works with recomputations of subsidiary
;; computations. This tests that :old-value-source is getting kept
;; around long enough.
(deftest reuse-test
  (let [r1 (reporter/new-reporter :value 1)
        rs (reporter/new-reporter :value [1 2 3])
        counter (atom 0)
        counting-plus (fn counting-plus [x y] (swap! counter inc) (+ x y))
        dependency-introducer (fn [x] (cache counting-plus r1 x))
        r (expr-let [s1 (expr-seq map dependency-introducer rs)
                      s2 (expr-seq map dependency-introducer s1)]
             s2)
        md (new-expression-manager-data (new-priority-task-queue 0))]
    (is (= (computation-value r md) [3 4 5]))
    (is (= @counter 4))
    (reporter/set-value! rs [1 2 3 4])
    (is (= (computation-value r md) [3 4 5 6]))
    (is (= @counter 5))))

(deftest nil-value-test
  ;; There had been a bug with nil values throwing off propagation.
  ;; This tests that it is fixed.
  (let [md (new-expression-manager-data (new-priority-task-queue 0))
        r0 (reporter/new-reporter :name :r0)
        r1 (expr identity r0)
        r2 (expr identity r1)]
    (request r2 md)
    (compute md)
    (is (= (reporter/value r2) invalid))
    (reporter/set-value! r0 nil)
    (compute md)
    (is (= (reporter/value r2) nil))
    (reporter/set-value! r0 1)
    (compute md)
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
        trials 10 ;0000
        changes-per-trial 1000
        base (vec (for [i (range width)]
                    (reporter/new-reporter :name [0 i]
                                           :value (mod (inc i) width))))
        md (new-expression-manager-data (new-priority-task-queue 4))
        evals (atom 0)]
    (letfn [(indexer [d pos]
              (if (zero? d)
                (base pos)
                (cache ^{:name [d pos :cache]}
                       indexer-eval d pos)))
            (indexer-eval [d pos]
              (expr ^{:name [d pos :eval]}
                    indexer (- d 1) (indexer (- d 1) pos)))
            (answers [arguments]
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
                                [[d pos] (request (indexer d pos) md)]))]
        (compute md 1000000)
        (check requests (answers arguments))
        (doseq [i (range trials)]
          (when (= (mod i 100) 0)
            (println "starting trial" i))
          (doseq [j (range changes-per-trial)]
            (reporter/set-value! (base (mod (* (inc i) j) width))
                                 (mod (* j j) width))
            (when (zero? (mod j 17))
              (future (compute md (mod i 34)))))
          (compute md)
          (check requests (answers arguments)))))))



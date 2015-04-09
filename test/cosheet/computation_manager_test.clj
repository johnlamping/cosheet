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

(deftest ensure-in-cache-test
  (let [m (new-management)
        r0 (ensure-in-cache [:a :b] m)
        r1 (ensure-in-cache [:a :c] m)
        r2 (ensure-in-cache [:a :b] m)]
    (is (= r0 r2))
    (is (= (:expression (reporter/data r0)) [:a :b]))
    (is (= (:manager-type (reporter/data r0)) :cached-eval))
    (is (not= r0 r1))))

(deftest cache-manager-test
  (let [m (new-management)
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
    (cache-manager r1 m)
    (is (not (contains? (reporter/data r1) :value-source)))
    ;; With demand, the cached value should be created.
    (register-copy-value r1 r3)
    (cache-manager r1 m)
    (is (contains? (reporter/data r1) :value-source))
    ;; And we should pick it up for the other reporter with the same expr.
    (register-copy-value r2 r4)
    (cache-manager r2 m)
    (is (= (:value-source  (reporter/data r1))
           (:value-source  (reporter/data r2))))
    (let [orig-source (:value-source  (reporter/data r1))]
      ;; Lose interest in r1 then get it back, and the same value
      ;; source should come back.
      (swap! (reporter/data-atom r3) dissoc :value-source)
      (register-copy-value r1 r3)
      (cache-manager r1 m)
      (cached-eval-manager orig-source m)
      (is (not (contains? (reporter/data r1) :value-source)))
      (swap! (reporter/data-atom r3) assoc :value-source r1)
      (register-copy-value r1 r3)
      (cache-manager r1 m)
      (is (= (:value-source (reporter/data r1))
             orig-source))
      ;; Now, lose interest in both reporters with that expression,
      ;; and the cache should drop it.
      (swap! (reporter/data-atom r3) dissoc :value-source)
      (register-copy-value r1 r3)
      (cache-manager r1 m)
      (swap! (reporter/data-atom r4) dissoc :value-source)
      (register-copy-value r2 r4)
      (cache-manager r2 m)
      (cached-eval-manager orig-source m)
      (swap! (reporter/data-atom r3) assoc :value-source r1)
      (register-copy-value r1 r3)
      (cache-manager r1 m)
      (is (not= (:value-source (reporter/data r1))
                orig-source)))))

;;; TODO: Move these to a library.
(defmacro expr
  "Takes a function and a series of arguments, and produces an eval
   reporter with a tracing thunk. Extra information can be added as meta
   on the function."
  [& args]
  `(reporter/new-reporter :trace (fn [thunk#] (thunk#))
                          :expression [~@args]
                          :manager-type :eval
                          ~@(apply concat (seq (meta (first args))))))

(defmacro cache
  "Takes a function and a series of arguments, and produces a cache
   reporter with a tracing thunk. Extra information can be added as meta
   on the function."
  [& args]
  `(reporter/new-reporter :trace (fn [thunk#] (thunk#))
                          :expression  [~@args]
                          :manager-type :cache
                          ~@(apply concat (seq (meta (first args))))))

;;; Test that caching is working by doing a recursive computation that would
;;; take a very long time if it weren't cached.
(deftest fib-cache-test
  (let [m (new-management)
        base (reporter/new-reporter :value 0)]
    (letfn [(fib [n] (if (<= n 1)
                       base
                       (expr + (cache fib (- n 1)) (cache fib (- n 2)))))]
      ;; Since the base is 0, fib should be 0 everywhere, and since
      ;; the computations should be cached, this should be fast.
      (let [f45 (fib 45)]
        (is (= (computation-value f45 m) 0))
        (reporter/set-value! base 1)
        ;; Now it should be the right value.
        (is (= (computation-value f45 m) 1836311903))))))

(defn pretty-expression
  [expr show-prev]
  (if (reporter/reporter? expr)
    (let [desc
          (for [key [:name :value]]
            [key (key (reporter/data expr))])]
      (if (nil? (second (first desc)))
        (let [[key callback] (first (:attendees (reporter/data expr)))]
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

;;; NOTE: The following three functions are not used, but may be
;;; helpful in debugging problems.

(defn print-reporter
  [r]
  (doseq [key [:name :manager-type :value]]
    (let [v (key (reporter/data r))]
      (when v
        (println " " key v))))
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

;; TODO: Add a data structure checker to the following test

(deftest asynchronous-test
  ;; Creates width base reporters, then a series layers of lookups that
  ;; use the value
  ;; at the previous layer as an index into another value at that
  ;; layer. Makes sure the right results are returned, then starts
  ;; changing the states in one thread, while another thread
  ;; propagates, and makes sure that the final answer is still right
  (let [width 13
        depth 7
        trials 10000; 0
        changes-per-trial 1000
        base (vec (for [i (range width)]
                    (reporter/new-reporter :name [0 i]
                                           :value (mod (inc i) width))))
        m (new-management)]
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
                       (answers-map [d pos])))))]
      (let [arguments (for [d (range depth)
                            pos (range width)]
                        [d pos])
            requests (into {} (for [[d pos] arguments]
                                [[d pos] (request (expr indexer d pos) m)]))]
        (compute m)
        (right-results? requests (answers arguments))
        (doseq [i (range trials)]
          (when (= (mod i 1000) 0)
            (println "starting trial" i))
          (doseq [j (range changes-per-trial)]
            (reporter/set-value! (base (mod (* i j) width))
                                 (mod (* j j) width))
            (when (zero? (mod j 17))
              (future (compute m))))
          (compute m)
          (right-results? requests (answers arguments)))))))



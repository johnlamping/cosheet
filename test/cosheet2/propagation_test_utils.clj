(ns cosheet2.propagation-test-utils
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet2 [reporter :refer [invalid] :as reporter]
                      [test-utils :refer [check any]]
                      [application-calculator
                       :refer [copy-value-and-cleanup-callback
                               null-callback]])
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
                      (any fn?)]))
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
                      null-callback]))
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

(ns cosheet.profiling
  (:require (cosheet [reporter :refer [reporter?
                                       reporter-data reporter-value-or-invalid]]
                     [task-queue :refer [make-priority-task-queue]]
                     [calculator :refer [computation-value
                                         make-calculator-data]]
                     [debug :refer [simplified-function-name]])))

;;; Code to walk reporters whose values have been calculated, and
;;; generate a profile.  See the doc string for reporters-profile for
;;; a description of the output.  Many of these functions also take a
;;; set of reporters already seen on some other path through the
;;; dag. They will be charged only to the first path we have seen that
;;; leads to them. They also take a seq of function names of ancestors
;;; to the given reporter.

(def accumulate-profiles)

(defn- accumulate-invocations
  "Count one invocation of fun-name, under each of its ancestors, plus
  just itself."
  [acc fun-name ancestors]
  (reduce (fn [acc ancestor]
            (update-in acc [ancestor fun-name] (fnil inc 0)))
          acc (conj ancestors nil)))

(defn parse-reporter-application
  "Given a reporter's data, return its application, and its function
  name, if they are available."
  [data]
  (let [application (when (not (:cache-key data))
                      ;; We don't directly handle the application of
                      ;; forwarding reporters.  Instead, we'll do the
                      ;; application reporter they get their value
                      ;; from.  That way, we only profile the
                      ;; application once.
                      (:application data))
        fun-name (as-> (first application) fun
                   (if (reporter? fun) (reporter-value-or-invalid fun) fun)
                   (when (instance? clojure.lang.Fn fun)
                     (simplified-function-name fun)))]
    [application fun-name]))

(defn- accumulate-reporter-profile
  "Accumulate one reporter's profile information."
  [acc seen reporter ancestors]
  (if (seen reporter)
    [acc seen]
    (let [seen (conj seen reporter)
          data (reporter-data reporter)
          [application fun-name] (parse-reporter-application data)
          acc (cond-> acc
                fun-name (accumulate-invocations fun-name ancestors))
          subsidiaries (filter reporter? application)
          [acc seen] (accumulate-profiles acc seen subsidiaries ancestors)
          source (:value-source data)]
      (if source
        (accumulate-profiles acc seen [source]
                            (cond-> ancestors fun-name (conj fun-name)))
        [acc seen]))))

(defn- accumulate-profiles
  "Accumulate profile information on reporters, returning the profile
  and the set of reporters seen."
  [acc seen reporters ancestors]
  (reduce (fn [[acc seen] reporter]
            (accumulate-reporter-profile acc seen reporter ancestors))
          [acc seen] reporters))

(defn reporters-profile
  "Calculate the values of the reporters, if not already calculated, and
  return profile information on them.

  The profile is a map of maps of counts: f -> f -> n, from name of
  function to name of function heading expressions that the first
  function caused to be called. The first function name can also be
  nil, in which case the count is the total number of invocations of
  the second function.

  Notice that this is a cumulative profile, which notes the function
  calls anywhere underneath a function.  That is what is recorded in
  the reporter tree.  But it also only records functions recorded in
  reporters, not all intermediate functions."
  [reporters]
  (let [cd (or (some #(when (reporter? %)
                        (:calculator-data (reporter-data %)))
                     reporters)
               (make-calculator-data (make-priority-task-queue)))]
    (doseq [reporter reporters]
      (computation-value reporter cd))
    (first (accumulate-profiles {} #{} reporters #{}))))

(defn print-profile
  "Print a summary of a profile, showing only the max-fns most
  important functions, and, for each of them, only the most important
  max-descendants."
  [profile max-fns max-descendants]
  (let [calls (profile nil)
        num-calls (apply + (vals calls))
        top-subsidiaries (->> (seq profile)
                              (filter #(not (nil? (first %))))
                              (map (fn [[fun counts]]
                                     [fun (apply + (vals counts))]))
                              (sort-by #(+ (get calls (first %) 0) (second %)))
                              reverse
                              (take max-fns))]
    (println "Total calls:" num-calls)
    (doseq [[fun below-count] top-subsidiaries]
      (println fun "calls:" (calls fun) " has-below:" below-count)
      (let [top-descendants (->> (seq (profile fun))
                                 (sort-by second)
                                 reverse
                                 (take max-descendants))]
        (doseq [[fun count] top-descendants]
          (println "  " fun "called:" count))))))

(defn profile-and-print-reporters
  ([reporters] (profile-and-print-reporters reporters 10 10))
  ([reporters max-fns max-descendants]
   (print-profile (reporters-profile reporters) max-fns max-descendants)))

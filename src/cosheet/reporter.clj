(ns cosheet.reporter
  (:require (cosheet [utils :refer [dissoc-in swap-returning-both!
                                    swap-control-return!]])))

(defn- check-callback [callback]
  (assert (fn? (first callback))
          ["Callback doesn't start with a function." callback])
  callback)

(defn- call-callback [callback & args]
  (apply (first callback) (concat args (rest callback))))

(def invalid
  "A special value indicating that the reporter does not have a valid value"
  ::invalid)

(defn valid? [value]
  (not= value invalid))

(defrecord ReporterImpl
    ^{:doc
      "A reporter is essentially an atom that provides monitoring not only
       of its value, but also of demand for its value. A computation
       may be associated with a reporter and will be re-evaluated as necessary
       called to keep the reporter's value up to date, but only as long as
       there is demand for the reporter's value.
       A reporter is implemented as a record holding an atom with
       a map of relevant information.
       We warp the atom in a record only because that supports a special
       print method that can avoid printing circular references.
       The special value, ::invalid, indicates that there is no current
       valid value.
       One or more callbacks can attend to the value of the reporter. Each
       is contains a key, a priority, a function and optionally some
       additional arguments. The priority indicates how important it is
       for this reporter to be recomputed earlier, if multiple reporters
       are out of date (lower priority first). The effective priority
       of a reporter is the minimum of the priority of each of its attendees.
       Each attendee is guaranteed to eventually be called with
       the key, the reporter, and the attendee's additional arguments. It will
       eventually be similarly called after any change in the value, but not
       necessarily once per change.
       (We use a key, rather than just passing in a closure, because an
       identical key can be generated later, to refer to the callback when we
       want to remove it, while an identical closure can't.)
       A manager can be registered with the reporter, to be informed when
       demand for the reporter's value changes. It will be called the
       first time there are any attendees to the reporter, whenever
       there is a transition in whether there are attendees, and whenever
       the priority of the reporter changes. It is responsible
       for keeping the reporter's value up to date, doing things
       like registering for callbacks to update its state, or removing those
       callbacks when there is no more interest. It can put additional
       information on the reporter to support its functionality."}
    [;;; An atom holding a map including
     ;;;   :value     The value of the reporter.
     ;;;   :priority  The priority for recomputing this reporter (lower first)
     ;;;              This will be the minimum of the priorities of all
     ;;;              attendees. If there are no attendees, it will be
     ;;;              Double/MAX_VALUE.
     ;;;   :manager   If present, the manager for this reporter.
     ;;;   :attendees If present, a map from key [priority fn &args] of
     ;;;              attendees to the reporter.
   data]
  )

(defn reporter? [r]
  (instance? ReporterImpl r))

(defn value
  "Return the current value of the reporter. If it is not a reporter, treat
   it as a constant reporter, and return the object."
  [r]
  (if (reporter? r)
    (:value @(:data r))
    r))

(defn data-atom
  "Return the atom holding the reporter's data. If any of the standard fields
   are changed, the appropriate notifications must be done."
  [r]
  (:data r))

(defn data
  "Return all the current data for the reporter."
  [r]
  @(:data r))

(defn data-attended? [data]
  (not (empty? (:attendees data))))

(defn attended? [r]
  (data-attended? @(:data r)))

(defn inform-attendees
  "Notify the attendees that the value may have changed."
  [r]
  ;;; Since the only guarantee is eventual callback, we can fetch the
  ;;; attendees map outside of any lock, since anything that changed
  ;;; the attendees will also request callbacks if appropriate.
  ;;; This does mean that an attendee may be called after it has cancelled
  ;;; its request.
  (doseq [[key [priority & callback]] (:attendees @(:data r))]
    (call-callback callback key r)))

(defn set-value!
  "Set the value of the reporter, informing any attendees."
  [r value]
  (let [[old current]
        (swap-returning-both! (:data r) #(assoc % :value value))]
    (if (not= (:value old) (:value current))
      (inform-attendees r))))

(defn set-manager!
  "Set the manager for the reporter.
   Once set, the manager can never be changed."
  [reporter & manager]
  (check-callback manager)
  (let [[old current]
        (swap-returning-both! (:data reporter) assoc :manager manager)]
    (let [old-manager (:manager old)]
      (when old-manager
        (assert (= old-manager manager)
                (format "Cannot change manager: expression %s."
                        (:expression old)))))
    (when (data-attended? current)
      (call-callback manager reporter))))

(defn set-attendee!
  "Add an attending callback to a reporter, under a key that must be unique
   to each callback. If a callback is provided, the remaining arguments are
   a priority, a function, and added arguments to the function.
   If no callback is provided, remove any callback that is
   present. If a callback is provided, call it immediately."
  [r key & priority-and-callback]
  (let [[priority & callback] priority-and-callback]
    (if (reporter? r)
      (let [[old current]
            (swap-returning-both!
             (:data r)
             (fn [data]
               (let [prev-attendee-priority
                     (first (get-in data [:attendees key]))
                     adjusted
                     (if (nil? callback)
                       (dissoc-in data [:attendees key])
                       (do (check-callback callback)
                           (-> data
                               (assoc-in [:attendees key] priority-and-callback)
                               (update :priority #(min % priority)))))]
                 (if (or (nil? prev-attendee-priority)
                         (and (not (nil? callback))
                              (<= priority prev-attendee-priority))
                         (> prev-attendee-priority (:priority adjusted)))
                   adjusted
                   ;; We replaced the lowest priority with a higher one,
                   ;; So we have to go through all the priorities to find
                   ;; the new lowest.
                   (let [priorities-and-callbacks (vals (:attendees adjusted))
                         new-priority
                         (if (empty? priorities-and-callbacks)
                           Double/MAX_VALUE
                           (apply min (map first priorities-and-callbacks)))]
                     (assoc adjusted :priority new-priority))))))]
        (when callback
          (call-callback callback key r))
        (when (and (:manager current)
                   (or
                    (not= (data-attended? old) (data-attended? current))
                    (not= (:priority old) (:priority current))))
          (call-callback (:manager current) r)))
      (when callback
        (call-callback callback key r)))))

(defn new-reporter
  [& {manager :manager :as args}]
  (let [args (merge {:value invalid} args)]
    (let [reporter (->ReporterImpl
                    (atom (-> args
                              (dissoc :manager)
                              (assoc :priority Double/MAX_VALUE))))]
      (when manager
        (apply set-manager! reporter
               (check-callback (if (sequential? manager) manager [manager]))))
      reporter)))

(defmethod print-method ReporterImpl [s ^java.io.Writer w]
  (let [data @(:data s)]
    (.write w "<Reporter")
    (if-let [value (:value data)]
      (.write w (str " value:" value)))
    (if-let [type (:manager-type data)]
      (.write w (str " manager-type:" type)))
    (if-let [expression (:expression data)]
      (.write w (str " expression:"
                     (vec (map #(if (reporter? %) "<R>" %) expression)))))
    (.write w ">")))



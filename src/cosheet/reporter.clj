(ns cosheet.reporter
  (:require (cosheet [utils :refer [dissoc-in swap-returning-both!]])))

(defn- check-callback [callback]
  (assert (fn? (first callback)) "Callback is not a function.")
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
       We only use a record so we can have a special print method that
       can avoid printing circular references.
       The special value ::invalid indicates that there is no current
       valid value.
       One or more callbacks can attend to the value of the reporter. Each
       callback is associated with a key, and optionally some additional
       arguments. Each attendee is guaranteed to eventually be called with
       the key, the reporter, and the attendee's additional arguments. It will
       eventually be similarly called after any change in the value, but not
       necessarily once per change.
       (We use a key, rather than just passing in a closure, because an
       identical key can be generated later, to refer to the callback when we
       want to remove it, while an identical closure can't.)
       A manager can be registered with the reporter, to be informed when
       demand for the reporter's value changes. It will be called the
       first time there are any attendees to the reporter, and whenever
       there is a transition in whether there are attendees. It is responsible
       for keeping the reporter's value up to date. It can do things
       like registering for callbacks to update its state, or removing those
       callbacks when there is no more interest. It can put additional
       information on the reporter to support its functionality."}
  [;;; An atom holding a map including :value, and possibly :manager,
   ;;; and :attendees, as well as other keys.
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
  ;;; the attendees will also do callbacks if appropriate.
  (doseq [[key callback] (:attendees @(:data r))]
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
   to each callback. If no callback is provided, remove any callback that is
   present. If a callback is provided, call it."
  [r key & callback]
  (if (reporter? r)
    (let [[old current]
          (swap-returning-both!
           (:data r)
           (fn [data]
             (if (nil? callback)
               (dissoc-in data [:attendees key])
               (assoc-in data [:attendees key] (check-callback callback)))))]
      (when callback
        (call-callback callback key r))
      (when (and (:manager current)
                 (not= (data-attended? old) (data-attended? current)))
        (call-callback (:manager current) r)))
    (when callback
      (call-callback callback key r))))

(defn new-reporter
  [& {[key & callback] :attendee manager :manager :as args}]
  (let [args (merge {:value invalid} args)]
    (let [reporter (->ReporterImpl
                    (atom (dissoc args :attendee :manager)))]
      (when manager
        (apply set-manager! reporter
               (check-callback (if (sequential? manager) manager [manager]))))
      (when key
        (apply set-attendee! reporter key callback))
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



(ns cosheet.reporter
  (:require [cosheet.utils :refer [dissoc-in
                                   swap-returning-both!
                                   call-with-latest-value]]))

(defn- check-callback [callback]
  (assert (fn? (first callback)) "Callback is not a function")
  callback)

(defn- call-callback [callback & args]
  (apply (first callback) (concat args (rest callback))))

(defn- inform-attendees [reporter]
  ;;; Since the only guarantee is eventual callback, we can fetch the
  ;;; attendees map outside of the lock
  (doseq [[key callback] (:attendees @(:data reporter))]
    (call-callback callback key reporter)))

(def invalid ::invalid)

(defn valid? [value]
  (not= value invalid))

(defrecord ReporterImpl
    ^{:doc
      "A reporter object (an atom with a map of relevant information).
       We use a record only so we can have a special print method that
       can avoid printing circular references.
       A reporter monitors a computation, and records its current value, or
       the special token ::invalid.
       One or more callbacks can attend to the value of the reporter. Each
       callback is associated with a key, and optionally some additional
       arguments. Each attendee is guaranteed to eventually be called with
       the key, the reporter, and the attendee's additional arguments. It will
       eventually be similarly called after any change in the value, but not
       necessarily once per change.
       In addition to these generic functions, specific kinds of reporters will
       have additional information to support additional functionality.
       A manager function can be registered with the reporter, to implement
       that functionality. It will be called the first time there are any
       attendees to the reporter, and whenever there is a transition in
       whether there are attendees. It can do things
       like registering for callbacks to update its state, or removing those
       callbacks when there is no more interest."}
  [;;; An atom holding a map including :value, and possibly :manager,
   ;;; and :attendees, as well as other keys.
   data]
  )

(defn reporter? [r]
  (instance? ReporterImpl r))

(defn value
  "Return the current value of the reporter"
  [reporter]
  (:value @(:data reporter)))

(defn swap-data-returning-both!
  "Modify the extra data,
   which must not change the value, manager, or attendees.
   Return the old data and the revised data."
  [reporter f & args]
  (let [[old current]
        (apply swap-returning-both! (:data reporter) f args)]
    [old current]))

(defn set-value
  "Set the value of the reporter, informing any attendees."
  [reporter value]
  (let [[old current]
        (swap-returning-both! (:data reporter) assoc :value value)]
    (if (not= (:value old) (:value current))
      (inform-attendees reporter))))

(defn set-manager
  "Set the manager for the reporter.
   Once set, the manager can never be changed."
  [reporter & manager]
  (check-callback manager)
  (let [[old current]
        (swap-returning-both! (:data reporter) assoc :manager manager)]
    (assert (nil? (:manager old)) "Cannot change manager.")
    (when (not= (nil? (:attendees current)))
      (call-callback manager reporter))))

(defn set-attendee
  "Add an attending callback to a reporter, under a key that must be unique
   to each callback. If no callback is provided, remove any callback that is
   present. If a callback is provided, call it."
  [reporter key & callback]
  (let [[old current]
        (swap-returning-both!
         (:data reporter)
         (fn [data]
           (if (nil? callback)
             (dissoc-in data [:attendees key])
             (assoc-in data [:attendees key] (check-callback callback)))))]
    (when callback
      (call-callback callback key reporter))
    (when (and (:manager current)
               (not= (nil? (:attendees old)) (nil? (:attendees current))))
      (call-callback (:manager current) reporter))))

(defn new-reporter
  [& {value :value [key & callback] :attendee manager :manager
      :or {:value ::invalid} :as args}]
  (let [reporter (->ReporterImpl (atom (dissoc args :attendee :manager)))]
    (when manager
      (apply set-manager reporter
             (check-callback (if (sequential? manager) manager [manager]))))
    (when key
      (apply set-attendee reporter key callback))
    reporter))

(defmethod print-method ReporterImpl [s ^java.io.Writer w]
  (.write w "<Reporter")
  (.write w (str (dissoc @(:data s) :attendees :manager)))
  (.write w ">"))
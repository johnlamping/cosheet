(ns cosheet2.reporter
  (:require (cosheet2 [utils :refer [dissoc-in 
                                    update-in-clean-up
                                    swap-returning-both!
                                    swap-control-return!]])))

(defn check-callback [callback]
  (assert (fn? callback)
          ["Callback isn't a function." callback])
  callback)

(defn call-callback-for-undescribed-change [callback & args]
  (apply callback (concat args [:description nil :categories nil])))

(def invalid
  "A special value indicating that the reporter does not have a valid value"
  ::invalid)

(defn valid? [value]
  (not= value invalid))

(defrecord ReporterImpl
    ^{:doc
      "A reporter is essentially an atom that provides monitoring of
       its value, what parts of its value have changed, and of demand
       for its value. The special value, ::invalid, indicates that the
       value is not currently known.

       One or more callbacks can attend to the reporter. And each can
       optionally specify which categories of change it wants to be
       informed of.  When the reporter's value changes, the change can
       optionally be associated with a description of what parts
       changed, and a set of categories of those parts. Whenever the
       value changes, all attendees are notified, except that if the
       change got a description, and an attendee specified categories
       it was interested in that don't match any of the change's
       categories, it won't be notified.
       
       Each callback has a key, a priority, a function. The
       priority indicates how important it is for the callback to have
       the latest value for this reporter. If multiple reporters'
       values are out of date, recomputation of the lower priority
       numbers will come first.  The priority of a reporter is the
       minimum of the priorities of its attendees.

       Once an attendee added, it is guaranteed to eventually be
       called. It will be similarly called after any change in the
       value, (unless it has registered for categories, and none of
       the changes match). When called, it gets keyword arguments for
       the key, the reporter, the categories of the changed parts of
       the value and a description of the change. The latter two will
       be nil if they were not specified.

       The callback will not necessarily be called once per change,
       and it may not find a valid value when it is called.  But it is
       guaranteed to be called after the final valid value has been
       set. And all the change descriptions, and their categories,
       will eventually be provided, possibly in a series of callbacks.

       The callback's key identifies it so it can be unregistered
       (We use a key, rather than just passing in a closure, because
       an identical key can be generated later, to refer to the
       callback when we want to remove it, while an identical closure
       can't.)

       Typically, a reporter has a calculator registered with it. The
       calculator is in charge of keeping the reporter's value up to
       date, as long as there is demand for it. To do that, it is
       informed when there is a change to whether there is demand for
       the reporter's value. It will be called, with the reporter, the
       first time there are any attendees to the reporter, whenever
       there is a transition in whether there are attendees, and
       whenever the priority of the reporter changes. These callbacks
       let it do things like registering for callbacks to update its
       state, or cancelling those callbacks when there is no more
       interest. The calculator can put additional information on the
       reporter to support its functionality.

       A reporter is implemented as a record holding an atom with
       a map of relevant information. By wrapping the atom in a record,
       we can define a special print method that can avoid printing
       circular references.
."}
    [;;; An atom holding a map consisting of
     ;;;   :value       The value of the reporter.
     ;;;   :priority    The priority for recomputing this reporter
     ;;;                (lower first)
     ;;;                This will be the minimum of the priorities of all
     ;;;                attendees. If there are no attendees, it will be
     ;;;                Double/MAX_VALUE.
     ;;;   :calculator  If present, the calculator for this reporter.
     ;;;   :attendees   If present, a map from key to
     ;;;                [priority categories callback] 
     ;;;                for each attendee to the reporter.
     ;;;   :selections  A map from category to the set of keys of attendees
     ;;;                that have requested that category.
     ;;;                (::universal-category matches everything, and is used
     ;;;                for attendees that haven't narrowed down their
     ;;;                interest.
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
   in this atom are changed, the appropriate notifications must be done."
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
  [r description categories]
  ;;; Since the only guarantee is eventual callback, we can fetch the
  ;;; attendees map outside of any lock, since anything that changed
  ;;; the attendees will also request callbacks if appropriate.
  ;;; This does mean that an attendee may be called after it has cancelled
  ;;; its request.
  (let [data (data r) 
        ;; Avoid calling the same reporter twice if several of its
        ;; categories match
        reporter-keys (if (nil? categories)
                        (keys (:attendees data))
                        (set (mapcat (partial get (:selections data))
                                     (conj categories ::universal-category))))]
    (doseq [key reporter-keys]    
      (let [[priority classes callback] (get-in data [:attendees key])]
        (callback :key key
                  :reporter r
                  :description description
                  :categories categories)))))

(defn set-value!
  "Set the value of the reporter, informing any attendees."
  [r value]
  (let [[old current]
        (swap-returning-both! (:data r) #(assoc % :value value))]
    (if (not= (:value old) (:value current))
      (inform-attendees r nil nil))))

;; TODO: Add make-change-control-return

(defn make-change!
  "Call the function with the current value of the reporter.  It must
   return a new value, a description of the change, and the categories
   of the change. Set the value of the reporter to the new value, and
   informing any attendees that care about any of the categories of the
   change."
  [r f]
  (let [[changed description categories]
        (swap-control-return!
         (:data r)
         #(let [[value description categories] (f (:value %))]
            [(assoc % :value value)
             [(not= (:value %) value) description categories]]))]
    (if changed
      (inform-attendees r description categories))))

(defn set-calculator!
  "Set the calculator for the reporter.
   Once set, the calculator can never be changed."
  [reporter calculator]
  (check-callback calculator)
  (let [[old current]
        (swap-returning-both! (:data reporter) assoc :calculator calculator)]
    (when-let [old-calculator (:calculator old)]
      (assert (= old-calculator calculator)
              (format "Cannot change calculator: expression %s."
                      (:expression old))))
    (when (data-attended? current)
      (calculator :reporter reporter))))

(defn update-add-attendee
  "Add the described attendee to the data, which must not already have
   one with that key."
  [data key priority categories callback]
  (-> data
      (update-in [:selections]
                 (fn [selections]
                   (reduce (fn [selections category]
                             (update-in selections [category]
                                        #((fnil conj #{}) % key)))
                           selections categories)))
      (assoc-in [:attendees key] [priority categories callback])
      (update :priority #(min % priority))))

(defn update-remove-attendee
  "Remove any attendee with the given key from the data."
  [data key]
  (if-let [[priority categories _] (get-in data [:attendees key])]
    (let [data (-> data
                   (update-in-clean-up
                    [:selections]
                    (fn [selections]
                      (reduce (fn [selections category]
                                (update-in-clean-up selections [category]
                                                    #(disj % key)))
                              selections categories)))
                   (dissoc-in [:attendees key]))]
      (if (= priority (:priority data))
        ;; We took out a best priority attendee. Recompute the priority.
        (assoc data :priority
               (if-let [attendees (vals (:attendees data))]
                 (apply min (map first attendees))
                 Double/MAX_VALUE))
        data))
    data))

(defn update-and-inform-calculator!
  "Run the update on the reporter, and inform the calculator if there
   has been a change in whether it is attended to."
  [r f]
  (let [[old current] (swap-returning-both! (:data r) f)]
    (when (and (:calculator current)
               (or
                (not= (data-attended? old) (data-attended? current))
                (not= (:priority old) (:priority current))))
      ((:calculator current) r))))

(defn remove-attendee!
  "Remove the attendee with the given key."
  [r key]
  (update-and-inform-calculator! r #(update-remove-attendee % key)))

(defn set-selective-attendee!
  "Add an attending callback to a reporter, under a key that must be
   unique to each callback. If a callback is provided, the remaining
   arguments are a priority, a vector of categories, a function, and
   added arguments to the function, and the callback is called.
   If no callback is provided, remove any callback with the key."
  ([r key]
   (remove-attendee! r key))
  ([r key priority categories callback]
   (check-callback callback)
   (if (reporter? r)
     (do
       (update-and-inform-calculator!
        r #(-> %
               (update-remove-attendee key)
               (update-add-attendee key priority categories callback)))
       (call-callback-for-undescribed-change callback :key key :reporter r))
     ;; r is not a reporter, so it will never change. We just have to make
     ;; the initial call to the callback.
     (call-callback-for-undescribed-change callback :key key :reporter r))))

(defn set-attendee!
  "Add an attending callback to a reporter, under a key that must be unique
   to each callback. If a callback is provided, the remaining arguments are
   a priority, a function, and added arguments to the function, and the
   callback is called immediately.
   If no callback is provided, remove any callback .with the given key"
  ([r key]
   (remove-attendee! r key))
  ([r key priority callback]
   (set-selective-attendee! r key priority [::universal-category] callback)))

(defn new-reporter
  [& {calculator :calculator :as args}]
  (let [reporter (->ReporterImpl
                  (atom (-> (merge {:value invalid} args)
                            (dissoc :calculator)
                            (assoc :priority Double/MAX_VALUE))))]
    (when calculator
      (set-calculator! reporter calculator))
    reporter))

(defmethod print-method ReporterImpl [s ^java.io.Writer w]
  (let [data @(:data s)]
    (.write w "<Reporter")
    (if-let [name (:name data)]
      (.write w (str " name:" name)))    
    (if-let [value (:value data)]
      (.write w (str " value:" (if (seq? value) (doall value) value))))
    (if-let [type (:calculator-type data)]
      (.write w (str " calculator-type:" type)))
    (if-let [expression (:expression data)]
      (.write w (str " expression:"
                     (vec (map #(if (reporter? %) "<R>" %) expression)))))
    (if-let [application (:application data)]
      (.write w (str " application:"
                     (vec (map #(if (reporter? %) "<R>" %) application)))))
    (.write w ">")))



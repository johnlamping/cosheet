(ns cosheet2.reporter
  (:require (cosheet2 [utils :refer [update-in-clean-up
                                     assoc-in-if-non-empty
                                     swap-returning-both!
                                     swap-control-return!]])))

;;; TODO: Reporters should distinguish whether they have a valid value
;;; from whether their value has changed. Going from valid to invalid
;;; isn't a value change, and going from invalid to valid isn't a
;;; value change if the old value has become valid again. Callback
;;; descriptions need to say whether they are only a validity
;;; change. And there needs to be a way to register for only value
;;; changes, not validity changes.

;;; TODO: Reporters could keep a version number, which they report on
;;; callbacks. That would let the recipient trust the value given in
;;; the callback, if its version number was later than the last
;;; version told about. That would mean that many with-current-value
;;; calls, which require two atom dereferences, could go away.

(defprotocol Reporter
  "A protocol that indicates an object has an atom with fields expected
  for a reporter.
 
  A reporter holds a value and provides a connection between:
     * Attendees, which are callbacks that will be informed when the
       reporter's value changes.
     * A calculator that that will be informed when the demand from
       attendees changes, and that is charged with computing and updating
       the reporter's value. (The calculator is optional; any code can
       post changes. But there is usually a calculator.)
  In other words, the attendees are called when the value changes,
  while the calculator is called when the demand changes.

  It is possible for a reporter to not have a valid value. Usually,
  this is because something there has been a change to something that
  the reporter's value depends on, and the reporter hasn't recomputed
  its value yet. Some clients of a reporter only need the most recent
  valid value. And it can often be that recomputation will reveal that
  that value was the right value after all. So there is a way to
  access the last valid valud. It is possible to ask a reporter:
     * whether it has a valid value
     * for its last valid value

  Some methods that interact with the value, support a special
  value, ::invalid, that indicates that the reporter's value is not
  valid. Some methods that set a value accept ::invalid to indicate
  that the current value should be marked invalid. And some methods
  that get the current value use ::invalid to indicate that the
  current value is not valid. Reporter defines the variable invalid to
  be ::invalid.

  Changes to a reporter's value can be associated with categories -
  which can be anything that the attendees and the calculator agree
  on. For example, if a reporter's value is a map, the categories
  could be keys of the map. And the categories of a change to the
  value would be the keys whose values in the map changed.
  
  An attendee can optionally specify a set of categories of change
  that it wants to be informed of. And when the reporter's value
  changes, the change can optionally be associated with a set of what
  categories of change happened. If an update specifies its categories
  of change, then attendee that also specified a set of categories of
  change will only be notified if the update included at least one of
  the categories the expressed interest in. (Attendees that don't
  specify categories of interest will be informed of all changes. And
  updates that aren't associated with categories will be given to all
  attendees.)

  Since categories can only describe changes to the value, if the
  value didn't change, the provided categories are ignored. However,
  there is an additional category, validity-category, which is
  triggered when the value doesn't change but the validity does
  change. An attendee that registers for specific categories may also
  register for validity-category to be informed of validity changes.

  Am attendee also optionally specify a priority for how important
  that it have the the latest value for this reporter (lower priority
  numbers first). A reporter calculates its priority as the minimum of
  the priorities of its attendees. When multiple reporters' values are
  out of date, their calculators should priorities their
  recomputations based on their reporters' priorities, so that the
  recomputation of the earlier priority numbers will come first. The
  reporter functions, theselves, aren't affected the priority. They
  always complete before they return. They just calculate the priority
  for the benefit of calculators.

  Finally, each attendee has a key, which must be unique among
  attendees to its reporter. An attendee's key is how methods
  reference it when they want to remove it or modify its priority or
  categories of interest. In addition, when an attendees callback
  function is called, it is given the attendee's key.

  Once an attendee is added, it is guaranteed to eventually be called
  after any change in the value, (unless it has registered for
  categories, and none of the changes match). When called, it is given
  its key, the reporter, the categories of the changed parts of the
  value since the last valid value and a description of the
  change. The latter two will be nil if they were not specified when
  the change was made.

  The callback will not necessarily be called once per change, and it
  may not find a valid value when it is called.  But it is guaranteed
  to be called after the final valid value has been set. And all the
  change descriptions, and their categories, will eventually be
  provided, possibly in a series of callbacks, with each callback
  happenning when the value is at or past all of the changes
  described.

  A reporter typically has a calculator function, whose job it is to
  understand the information the reporter's value depends on, and keep
  that value up to date, if and only if there is demand for it. It
  may register for callbacks from other reporters that its value
  depends on. But it will typically only do recomputation when it has
  demand.

  The calculator is only activated when :calculator-data is
  present. From then on, the calculator is informed whenever there is
  a change in the nature of the demand for the reporter's value.

  It will be called the first time there are any attendees to the
  reporter. It will be called again whenever there is a change in:
      Whether or not there is demand.
      The priority of the demand.
      The categories requested.
  The calls give it the reporter and the calculator data, and let it
  do things like registering for callbacks to update its state, or
  cancelling those callbacks when there is no more interest. The
  calculator can put additional information on the reporter to support
  its functionality.

  A reporter is implemented as a record holding an atom with a map of
  relevant information. By wrapping the atom in a record, we can
  define a special print method that can avoid printing circular
  references.

  The atom must be in the field, data, and hold a map consisting of
  these fields, and that may have additional fields.
     :valid            True if the reporter has a valid value.
     :value            The value of the reporter. If the value is not
                       currently valid, this will be the last valid one.
     :priority         The priority for recomputing this reporter
                       (lower first)
                       This will be the minimum of the priorities of all
                       attendees. If there are no attendees, it will be
                       Double/MAX_VALUE.
     :attendees        If present, a map from key to
                       [priority categories callback] 
                       for each attendee to the reporter.
     :selections       A map from category to the set of keys of attendees
                       that have requested that category.
                       (::universal-category matches everything, and is
                       used for attendees that haven't narrowed down
                       their interest.)
     :calculator       The calculator for this reporter. It is set
                       when the reporter is created, and may not be changed.
     :calculator-data  If present, the auxilliary data for the
                       reporter's calculator. This is typically global
                       information shared across many reporters, like
                       a shared work queue. Once set, it may not be
                       changed."
  )

(defrecord ReporterImpl
    [data]
  Reporter
  )

(defn reporter? [r]
  (satisfies? Reporter r))

(defn reporter-atom
  "Return the atom holding the reporter's data. If any of the standard fields
   in this atom are changed, the appropriate notifications must be done."
  [r]
  (:data r))

(defn reporter-data
  "Return all the current data for the reporter."
  [r]
  @(:data r))

(def invalid
  "A special value indicating that a value is not valid"
  ::invalid)

(defn data-valid? [data]
  (:valid data))

(defn reporter-valid? [r]
  (data-valid? @(:data r)))

(defn value-valid? [value]
  (not= value invalid))

(defn data-latest-value [data]
  (:value data))

(defn reporter-latest-value [r]
  (if (reporter? r)
    (data-latest-value @(:data r))
    r))

(defn data-value-or-invalid [data]
  (if (data-valid? data)
    (data-latest-value data)
    invalid))

(defn reporter-value-or-invalid [r]
  (if (reporter? r)
    (data-value-or-invalid @(:data r))
    r))

(defn data-value-when-valid [data]
  (when (data-valid? data)
    (data-latest-value data)))

(defn reporter-value-when-valid
  "Return the current value of the reporter, if the value is
  valid. Otherwise return nil. If the argument is not a reporter,
  treat it as a constant reporter, and return it."
  [r]
  (if (reporter? r)
    (data-value-when-valid @(:data r) )
    r))

;;; TODO: !!! replace all calls to these with one of the above new
;;;           functions.
(defn data-value [data]
  (if (:valid data)
    (do (assert (not (= invalid (:value data))))
        (:value data))
    invalid))

(defn reporter-value
  "Return the current value of the reporter. If the argument is not a
  reporter, treat it as a constant reporter, and return it."
  [r]
  (if (reporter? r)
    (data-value @(:data r))
    r))

(defn valid? [r]
  "Return whether a reporter's value is valid. If given a plain value,
  this returns whether that value is valid."
  (not= (reporter-value r) invalid))

(defn data-attended? [data]
  (not (empty? (:attendees data))))

(defn attended? [r]
  (data-attended? @(:data r)))

(def universal-category
  "A special category of change that includes all changes, both to to
  values and to validity"
  ::universal-category)

(def value-category
  "A special category of chaqnge that includes all changes that alter
  the value, but doesn't include changes that affect only the
  validity. Clients that only want to see updated values should attend
  to this change."
  ::value-category)

(def validity-category
  "A special category of change that consists of changes that affect the
  validity. This means either going invalid or going valid. Clients
  that subscribe to only specific categories of change should also
  attend to this if they want to keep track of validity."
  ::validity-category)

(defn inform-attendees
  "Notify the attendees that the value may have changed."
  ([r]
   (inform-attendees r nil nil))
  ([r description categories]
  ;; Since the only guarantee is eventual callback, we can fetch the
  ;; attendees map outside of any lock, since anything that changed
  ;; the attendees will also request callbacks for new attendees.
  ;; This does mean that an attendee may be called after it has cancelled
  ;; its request.
   (let [data (reporter-data r) 
         reporter-keys (if (nil? categories)
                         ;; We have to inform all attendees
                         (keys (:attendees data))
                         ;; Avoid calling the same reporter twice
                         ;; if several of its categories match.
                         (set (mapcat (partial get (:selections data))
                                      (cond->
                                          (conj categories universal-category)
                                        (not= categories [validity-category])
                                        ;; The value changed.
                                        (conj value-category)))))]
     (doseq [key reporter-keys]    
       (let [[_ classes callback] (get-in data [:attendees key])]
         (callback :key key
                   :reporter r
                   :description description
                   :categories categories))))))

(defn update-value
  [data value]
  (if (value-valid? value)
    (assoc data :value value :valid true)
    (assoc data :valid false)))

(defn update-to-invalid
  [data]
  (assoc data :valid false))

(defn same-state?
  [data1 data2]
  (and (= (:value data1) (:value data2))
       (= (:valid data1) (:valid data2))))

(defn add-validity-category-when-appropriate
  "Given the old and new state of the reporter data, and the category
  changes the user provided, if they provided them, add the validity
  category to the categories of change if necessary.
  Also, check that the change satisfies the requirements on changes."
  [old-data new-data categories]
  (let [value-changed (not= (:value old-data) (:value new-data))
        validity-changed (not= (:valid old-data) (:valid new-data))]
    ;; We should only be called when something changed.
    (assert (or value-changed validity-changed))
    ;; If the value changed, we are required to be valid. Otherwise
    ;; the change is effectively changing the last valid value,
    ;; without giving a valid value.
    (when value-changed (assert (:valid new-data)))
    (if validity-changed
      (if value-changed
        ;; If the categories were nil, they already encompass changes
        ;; to validity.
        (when (seq categories)
          (conj categories validity-category))
        ;; The validity changed, but the value didn't change.
        [validity-category])
      categories)))

(defn set-value!
  "Set the value of the reporter, informing all attendees."
  [r value]
  (let [[old current]
        (swap-returning-both! (:data r) #(update-value % value))]
    (if (not (same-state? old current))
      (inform-attendees r nil (add-validity-category-when-appropriate
                               old current nil)))))

(defn change-data-control-return!
  "This is the most general function for updating a reporter. But it
  must not change the demand information.  
  Call the function with the current data map of the reporter.  It must
  return a new data map, a description of its change since the last
  valid value, the categories of the change, and the return value it
  wants.  Set the data of the reporter to the new map, and inform any
  attendees that care about any of the categories of the change.
  Return the specified value."
  [r f]
  (let [[old-data new-data description categories return-value]
        (swap-control-return!
         (:data r)
         #(let [[data description categories return-value] (f %) ]
            [data
             [% data description categories return-value]]))]
    (if (not (same-state? old-data new-data))
      (inform-attendees r description (add-validity-category-when-appropriate
                                       old-data new-data categories)))
    return-value))

(defn change-data!
  "This is the most general function for updating a reporter without
  also controlling the return value. But it must not change the demand
  information.
  Call the function with the current data map of the reporter.  It must
  return a new data map, a description of its change since the last
  valid value, and the categories of the change.  Set the data of the
  reporter to the new map, and inform any attendees that care about
  any of the categories of the change."
    [r f]
  (let [[old-data new-data description categories]
        (swap-control-return!
         (:data r)
         #(let [[data description categories] (f %)]
            [data
             [% data description categories]]))]
    (if (not (same-state? old-data new-data))
      (inform-attendees r description (add-validity-category-when-appropriate
                                       old-data new-data categories)))))

(defn change-value!
  "Call the function with the current value of the reporter.  It must
  return a new value, a description of the change since the last valid
  value, and the categories of the change. Set the value of the
  reporter to the new value, and inform any attendees that care about
  any of the categories of the change."
  [r f]
  (let [[old-data new-data description categories]
        (swap-control-return!
         (:data r)
         #(let [[value description categories] (f (:value %))
                data (update-value % value)]
            [data
             [% data description categories]]))]
    (if (not (same-state? old-data new-data))
      (inform-attendees r description (add-validity-category-when-appropriate
                                       old-data new-data categories)))))

(defn set-calculator-data-if-needed!
  "If the calculator data is not already present, set it
   and call the calculator if there is any demand.
   Calling this function activates the reporter.
   Once set, the calculator data may never be changed."
  [reporter calculator-data]
  (assert (not (nil? calculator-data)))
  (when
      (swap-control-return!
       (:data reporter)
       (fn [data]
         (if (and (nil? (:calculator-data data))
                  (not (nil? (:calculator data))))
           [(assoc data :calculator-data calculator-data)
            true]
           [data
            false])))
    (let [data (reporter-data reporter)]
      (when (data-attended? data)
        ((:calculator data) reporter (:calculator-data data))))))

(defn set-calculator-data!
  "Set the calculator data for the reporter, and call the calculator
   if there is any demand.
   Calling this function activates the reporter.
   Once set, the calculator data may never be changed."
  [reporter calculator-data]
  (let [data (reporter-data reporter)]
    (assert (nil? (:calculator-data data)))
    (assert (not (nil? (:calculator data)))))
  (set-calculator-data-if-needed! reporter calculator-data))

(defn update-attendee
  "Make the attendee under the given key be as described. This
  encompases adding a new attendee, removing one, or changing the
  priority or categories of an existing one."
  [data key priority categories callback]
  (let [[old-priority old-categories _ ] (get-in data [:attendees key])
        ;; If there is no callback, we are not interested in any categories.
        categories (when callback categories)
        priority (if callback priority Double/MAX_VALUE)
        old-priority (or old-priority Double/MAX_VALUE)
        dropped-categories (clojure.set/difference (set old-categories)
                                                   (set categories))
        added-categories (clojure.set/difference (set categories)
                                                 (set old-categories))]
    (let [data
          (-> data
              (update-in-clean-up
               [:selections]
               (fn [selections]
                 (as-> selections selections
                   (reduce (fn [selections category]
                             (update-in-clean-up selections [category]
                                                 #(disj % key)))
                           selections dropped-categories)
                   (reduce (fn [selections category]
                             (update-in selections [category]
                                        #((fnil conj #{}) % key)))
                           selections added-categories))))
              (assoc-in-if-non-empty [:attendees key]
                                     (when callback
                                       [priority categories callback])))]
      (if (or (<= priority old-priority)
              (< (:priority data) old-priority))
        (update data :priority #(min % priority))
        ;; We took out a best priority attendee. Recompute the priority.
        (assoc data :priority
               (if-let [attendees (vals (:attendees data))]
                 (apply min (map first attendees))
                 Double/MAX_VALUE))))))

(defn- change-and-inform-calculator!
  "Run the change on the reporter, and inform the calculator if there
  has been a change in the nature of the demand.
  The change must not affect the value or validity."
  [r f]
  (let [[old current] (swap-returning-both! (:data r) f)
        calculator (:calculator current)
        calculator-data (:calculator-data current)]
    ;; Some calculators, like application-calculator, can generate
    ;; demand for a reporter they create before they activate it.
    ;; So we may have to wait on calling the calculator until we
    ;; are activated.
    (when (and calculator
               calculator-data
               (or
                (not= (data-attended? old)
                      (data-attended? current))
                (not= (set (keys (:selections old)))
                      (set (keys (:selections current))))
                (not= (:priority old) (:priority current))))
      (calculator r calculator-data))))

(defn- check-callback [callback]
  (assert (fn? callback)
          ["Callback isn't a function." callback])
  callback)

(defn- call-callback-for-undescribed-change [callback & args]
  (apply callback (concat args [:description nil :categories nil])))

(defn remove-attendee!
  "Remove the attendee with the given key."
  [r key]
  (when (reporter? r)
    (change-and-inform-calculator! r #(update-attendee
                                       % key Double/MAX_VALUE [] nil))))

(defn set-attendee!
  "Add an attending callback to a reporter, under a key that must be
  unique to each callback. If a callback is provided, it is the last
  argument. It is preceeded by a priority, and optionally the
  categories of interest.  If no callback is provided, or the callback
  is nil, remove any callback with the given key."
  ([r key]
   (remove-attendee! r key))
  ([r key priority callback]
   (set-attendee! r key priority [universal-category] callback))
  ([r key priority categories callback]
   (when callback (check-callback callback))
   ;; You can't subscribe to only validity changes.
   ;; This makes value changes that don't specify a category match
   ;; all callbacks.
   (assert (not= categories [validity-category]))
   (when (reporter? r)
     (change-and-inform-calculator! r #(update-attendee
                                        % key priority categories callback)))))

(defn set-attendee-and-call!
  "Add an attending callback, and call it immediately."
  ([r key priority callback]
   (set-attendee-and-call! r key priority [universal-category] callback))
  ([r key priority categories callback]
   (set-attendee! r key priority categories callback)
   (when callback
     (call-callback-for-undescribed-change callback :key key :reporter r))))

(defn set-attendee-and-call-if-valid!
  "Add an attending callback, and if the reporter has a valid value,
  call the attendee immediately."
  ([r key priority callback]
   (set-attendee-and-call-if-valid! r key priority [universal-category]
                                    callback))
  ([r key priority categories callback]
   (set-attendee! r key priority categories callback)
   ;; It's OK the reporter goes invalid before we get here, because
   ;; when it goes valid again, the callback will be called.
   (when (and callback (reporter-valid? r))
     (call-callback-for-undescribed-change callback :key key :reporter r))))

(defn new-reporter-data
  [& {:as args}]
  (merge {:valid (value-valid? (get args :value invalid))
          :value invalid
          :priority Double/MAX_VALUE}
         args))

(defn new-reporter
  [& {:as args}]
  (when-let [calculator (:calculator args)] (check-callback calculator))
  (->ReporterImpl
   (atom (new-reporter-data args))))

(defmethod print-method ReporterImpl [s ^java.io.Writer w]
  (let [data @(:data s)]
    (.write w "<Reporter")
    (if-let [name (:name data)]
      (.write w (str " name:" name)))    
    (if-let [value (:value data)]
      (.write w (str " value:" (if (seq? value) (doall value) value))))
    (if-let [application (:application data)]
      (.write w (str " application:"
                     (vec (map #(if (reporter? %) "<R>" %) application)))))
    (.write w ">")))

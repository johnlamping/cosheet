(ns cosheet.state
  (:require [cosheet.utils :refer [swap-returning-both!
                                   call-with-latest-value]]))

;;; TODO: write test.

(defn new-state
  "Create a new state object. The call can provide the initial value,
   a map of additional information to be incorporated into the state,
   and a callback to call when the first subscription is added,
   or the last removed. The callback will be called with,
   whether or not there are any subscriptions,
   and any additional arguments specified with the callback."
  [& {:keys [value info callback] :as args}]
  (into (if (nil? info) {} info)
        (into
         {::value (atom value) ; The current value.
          :subscriptions (atom #{})} ; subscriptions, in the form  [fn arg ...]
         (when callback ; The subscription callback, in the form [fn arg ...]
           {:callback (if (fn? callback) [callback] callback)}))))

(defn state? [state]
  (not (nil? (::value state))))

(defn state-value
  "The current value of the state"
  [state]
  @(::value state))

(defn state-set
  "Make the new value be the value, and let the callbacks know."
  [state value]
  (let [[old new] (swap-returning-both! (::value state) (constantly value))]
    (if (not= old new)
      (doseq [[fn & args] @(:subscriptions state)]
        (apply call-with-latest-value #(identity @(::value state)) fn args)))))

(defn- inform-callback [state]
  (if-let [[fn & args] (:callback state)]
    (apply call-with-latest-value #(not (empty? @(:subscriptions state)))
           fn args)))

(defn subscribe
  "Returns the current value, or nil if it is currently unknown. If
   the current value changes, the callback will eventually be called.
   It must be a function or a sequence of a function and
   arguments. The function will be passed the new value and the
   additional arguments, if any. The function may not be called for every
   change, but will eventually be called after any change."
  [state callback & args]
  (let [[old new] (swap-returning-both! (:subscriptions state)
                                        #(conj % (cons callback args)))]
    (when (empty? old)
      (inform-callback state))
    @(::value state)))

(defn unsubscribe
  "Removes the specified subscription."
  [state callback & args]
  (let [[old new] (swap-returning-both! (:subscriptions state)
                                        #(disj % (cons callback args)))]
    (when (and (empty? new) (not (empty? old)))
      (inform-callback state))))


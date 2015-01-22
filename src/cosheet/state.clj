(ns cosheet.state
  (:require [cosheet.utils :refer [swap-returning-both!
                                   call-with-latest-value]]))

;;; TODO: write test.

(defn new-state
  "Create a new state object. The call can provide the initial value,
   a map of additional information to be incorporated into the state,
   and a callback to call when the first subscription is added,
   or the last removed. The callback will be called with:
   whether or not there are any subscriptions, the state object,
   and any additional arguments specified with the callback."
  [& {:keys [value callback additional] :as args}]
  (into (if (nil? additional) {} additional)
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

(defn state-update
  "Call the function with the current value,
  and set the value of the state to the result of the function."
  [state f & args]
  (let [[old new] (apply swap-returning-both! (::value state) f args)]
    (if (not= old new)
      (doseq [[f & args] @(:subscriptions state)]
        (apply call-with-latest-value (fn [] @(::value state))
               f state args)))))

(defn state-set
  "Make the new value be the value, and let the callbacks know."
  [state value]
  (state-update state (constantly value)))

(defn- inform-provider-callback
  "Tell the provider of our values whether we are interested in updates."
  [state]
  (if-let [[fn & args] (:callback state)]
    (apply call-with-latest-value #(not (empty? @(:subscriptions state)))
           fn state args)))

(defn subscribe
  "Returns the current value, or nil if it is currently unknown. If
   the current value changes, the callback will eventually be called.
   It must be a function or a sequence of a function and
   arguments. The function will be passed the new value, the state object, and
   the additional arguments, if any. The function may not be called for every
   change, but will eventually be called after any change."
  [state callback & args]
  (let [[old new] (swap-returning-both! (:subscriptions state)
                                        #(conj % (cons callback args)))]
    (when (empty? old)
      (inform-provider-callback state))
    @(::value state)))

(defn unsubscribe
  "Removes the specified subscription."
  [state callback & args]
  (let [[old new] (swap-returning-both! (:subscriptions state)
                                        #(disj % (cons callback args)))]
    (when (and (empty? new) (not (empty? old)))
      (inform-provider-callback state))))


(ns cosheet.mutable-map)

;;; Utilities for making clean maps. Maybe don't belong here.

(defn dissoc-in
  "Remove (get-in map keys), and if that creates an empty map one
  level up, keep removing."
  [map keys]
  (if (empty? keys)
    nil
    (let [key (first keys)
          lower (dissoc-in (get map key) (rest keys))]
      (if (empty? lower)
        (dissoc map key)
        (assoc map key lower)))))

(defn update-in-clean-up
  "Like update-in, but removes empty collections."
  [map keys fn]
  (let [result (fn (get-in map keys))]
      (if (or (nil? result) (and (coll? result) (empty? result)))
        (dissoc-in map keys)
        (assoc-in map keys result))))

(defn swap-returning-both! [atom f & args]
  (loop []
    (let [old @atom
          new (apply f old args)]
      (if (compare-and-set! atom old new)
        [old new]
        (recur)))))

;;; Functions that implement mutable maps.
;;; By hiding mutable maps behind this abstraction, we can switch the
;;; implementation if we want.
;;; This implementation is a vector containing atoms of maps. A key is
;;; stored in the map at the index corresponding to its hash.

(defn new-mutable-map [] (vec (map (fn [i] (atom {})) (range 10))))

(defn- mm-ref [mm key]
  (mm (mod (hash key) (count mm))))

(defn- mm! [fun mm key & args]
  (apply fun @(mm-ref mm key) key args))

(defn- mm-in! [fun mm keys & args]
  (apply fun @(mm-ref mm (first keys)) keys args))

(def get! (partial mm! get))
(def get-in! (partial mm-in! get-in))

(defn- mm-swap! [fun mm key & args]
  (swap! (mm-ref mm key) #(apply fun % key args)))

(defn- mm-swap-in! [fun mm keys & args]
  (swap! (mm-ref mm (first keys)) #(apply fun % keys args)))

(defn- mm-swap-in-returning-both! [fun mm keys & args]
  (for [info (swap-returning-both! (mm-ref mm (first keys))
                                  #(apply fun % keys args))]
    (get-in info keys)))

(def update! (partial mm-swap! (fn [map key f] (update-in map [key] f))))
(def update-in! (partial mm-swap-in! update-in))
(def update-in-returning-both!
  (partial mm-swap-in-returning-both! update-in))
(def assoc-in! (partial mm-swap-in! assoc-in))
(def dissoc-in! (partial mm-swap-in! dissoc-in))
(def update-in-clean-up! (partial mm-swap-in! update-in-clean-up))

;;; Methods for reflecting the state of part of a mutable map
;;; somewhere else, either elsewhere in the map, or elsewhere
;;; entirely.

;;; This is used for propagating updated information to users of the
;;; information without using transactions, just atomic operations.
;;; The principal is that anyplace that uses information makes a
;;; record of what it used. Then, it can be notified of changes at any
;;; time, possibly with some changes skipped, as long as it is
;;; guaranteed that it will eventually be notified of the latest
;;; information.

;;; In typical use, after a use of information is recorded, a
;;; task is set up to make a subscription for that information. When
;;; that task runs, it makes the state of there being a subscription
;;; agree with whether a subscription is still wanted, and if the
;;; information had changed before the subscription was created, it
;;; does a notification as if it had come from the subscription.

(defn call-with-latest-value-in!
  "Call the function with the current value of the path, and the other
   arguments, repeating until the value of the path after the call
   matches the value used in the call. This is good for registering the
   current state with some other place that needs to track it."
  [mm path f & args]
  (loop [value (get-in! mm path)]
    (apply f value args)
    (let [latest-value (get-in! mm path)]
      (when (not= value latest-value) (recur latest-value)))))

(defn call-and-clear-in!
  "Clear the value at the path, and if there had been a value, call the
   function with that value and the other arguments, repeating until
   there is no value on the path after the call. This is good for
   processing indications of pending work until all work is done."
  [mm path f & args]
  (when (get-in! mm path) ; Optimization to avoid a swap!
                             ; if there is no data.
    (loop []
      (let [[value _] (update-in-returning-both! mm path (constantly nil))]
        (when value
          (apply f value args)
          (recur))))))


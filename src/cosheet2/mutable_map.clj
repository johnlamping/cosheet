(ns cosheet2.mutable-map
  (:require [cosheet.utils :refer :all]))

;;; Functions that implement mutable maps.
;;; By hiding mutable maps behind this abstraction, we can switch the
;;; implementation if we want.
;;; This implementation is a vector containing atoms of maps. A key is
;;; stored in the map at the index corresponding to its hash.

(defn new-mutable-map
  ([] (new-mutable-map 10))
  ([parallelism] (vec (map (fn [i] (atom {})) (range parallelism)))))

(defn- mm-ref [mm key]
  (mm (mod (hash key) (count mm))))

(defn mm-get [mm key] (get @(mm-ref mm key) key))
(defn mm-get-in [mm keys] (get-in @(mm-ref mm (first keys)) keys))

;;; We reverse the usual order of function and map in the swap functions,
;;; so we can use partial to bind the function.
(defn- mm-swap! [fun mm key & args]
  (get (swap! (mm-ref mm key) #(apply fun % key args))
       key))
(defn- mm-swap-in! [fun mm keys & args]
  (get-in (swap! (mm-ref mm (first keys)) #(apply fun % keys args))
          keys))
(defn- mm-swap-in-returning-both! [fun mm keys & args]
  (for [info (swap-returning-both! (mm-ref mm (first keys))
                                   #(apply fun % keys args))]
    (get-in info keys)))

;;; The following functions do the named thing to the map.
(def update! (partial mm-swap! update))
(def update-in! (partial mm-swap-in! update-in))
(def update-in-returning-both!
  (partial mm-swap-in-returning-both! update-in))

(def assoc-in! (partial mm-swap-in! assoc-in))
(def dissoc-in! (partial mm-swap-in! dissoc-in))
(def update-in-clean-up! (partial mm-swap-in! update-in-clean-up))
(def update-in-clean-up-returning-both!
  (partial mm-swap-in-returning-both! update-in-clean-up))

(defn current-contents [mm]
  (reduce (fn [m part] (into m @part)) {} mm))

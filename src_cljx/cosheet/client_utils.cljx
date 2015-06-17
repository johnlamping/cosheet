(ns cosheet.client-utils
  (:require #+cljs [reagent.core :as special-atom]
            #+clj [clojure.core :as special-atom]
            [cosheet.dom-utils :refer [into-attributes]])
  )

(defn set-difference
  "Implement set difference ourselves, because I can't figure out how to
   get clojure.set available in Chrome."
  [s1 s2]
  (set (remove s2 s1)))

(defn replace-in-struct
  "Replace items in the structure that match keys in the map
   with the result in the map"
  [smap struct]
  (letfn [(r [struct]
            (if (contains? smap struct)
              (smap struct)
              (cond (map? struct) (into {} (map r struct))
                    (vector? struct) (vec (map r struct))
                    ;; We need the or below, because (list* nil) = nil.
                    (list? struct) (or (list* (map r struct)) '())
                    :else struct)))]
    (r struct)))

(defn remove-keys
  "Remove the given ids from the map."
  [map keys]
  (apply dissoc map keys))

(defn add-keys
  "Add entries for each of the keys to the map,
   creating the values by calling the creator with each key."
  [map ids creator]
  (reduce (fn [map id] (assoc map id (creator id)))
          map ids))

;;; A map from component id to atoms holding the current dom of that component.
(def components (atom {}))

(defn component [extra-attributes name]
  (let [[tag dom-attributes & rest] @(@components name)]
    (into
     [tag (into-attributes (dissoc dom-attributes :version) extra-attributes)]
     rest)))

(defn subcomponent-ids
  "Return a seq of the ids of the subcomponents of the dom."
  [dom]
  (if (vector? dom)
    (if (= (first dom) component)
      [(nth dom 2)]
      (reduce (fn [subcomponents dom]
                (into subcomponents (subcomponent-ids dom)))
              [] dom))
    []))

(defn into-atom-map
  "Incorporate an update of new doms into an atom containing a map of atoms,
  creating or deleting atoms as called for by the new doms."
  [amap update]
  (swap!
   amap
   (fn [amap]
     (reduce
      (fn [amap dom]
        (let [{:keys [id version]} (second dom)]
          (if (contains? amap id)
            (let [old-dom @(amap id)
                  old-version (:version (second old-dom))]
              (if (> version old-version)
                (let [old-subcomponents (set (subcomponent-ids old-dom))
                      subcomponents (set (subcomponent-ids dom))]
                  (do (reset! (amap id) dom)
                      (-> amap
                          (remove-keys
                           (set-difference old-subcomponents
                                           subcomponents))
                          (add-keys
                           (set-difference subcomponents
                                           old-subcomponents)
                           #(special-atom/atom [:div {:id % :version -1}])))))
                amap))
            amap)))
      amap update))))

(def pending-actions
  (atom {;;; The number of the next action we will tell the server about.
         :next-number 0
         ;;; A map from number to action.
         :waiting-actions {}}))

(defn update-add-action
  "Add an action to inform the server about."
  [pending action]
  (-> pending
      (assoc-in [:waiting-actions (:next-number pending)] action)
      (update-in [:next-number] inc)))

(defn add-pending-action
  "Add the action to the pending actions."
  [action]
  (swap! pending-actions update-add-action action))

(defn update-actions-acknowledged
  "Remove actions that are in the list of acknowledged ids."
  [pending acknowledged]
  (update-in pending [:waiting-actions] #(remove-keys % acknowledged)))

(defn process-acknowledged-actions
  "Remove actions that are in the list of acknowledged ids."
  [response]
  (let [acknowledged (:acknowledge response)]
    (when acknowledged
      (swap! pending-actions update-actions-acknowledged acknowledged))))

(defn include-pending-actions
  "Given a set of parameters to send to the server,
   include any pending actions."
  [params]
  (let [waiting (:waiting-actions @pending-actions)]
    (if (= waiting {}) params (into params {:actions waiting}))))

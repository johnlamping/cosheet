(ns cosheet.client-utils
  (:require #?(:cljs [reagent.core :as special-atom]
               :clj [clojure.core :as special-atom]) 
            [cosheet.dom-utils :refer [into-attributes]])
  )

(defn set-difference
  "Implement set difference ourselves, because I can't figure out how to
   get clojure.set available in Chrome."
  [s1 s2]
  (set (remove s2 s1)))

;;; Copied from utils.clj, so we don't have to import the whole thing yet.
;;; TODO: Try moving utils.clj to src_cljc.
(defn swap-returning-both!
  "Swap, and return a vector of the old and new values"
  [cell f & args]
  (loop []
    (let [old @cell
          new (apply f old args)]
      (if (compare-and-set! cell old new)
        [old new]
        (recur)))))

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

;;; The next code deals with our copy of the hiccup style dom that is
;;; fed to reagent.

;;; A map from component id to atom holding the current dom of that component.
(def components (atom {}))

(defn component [attributes]
  (let [id (:id attributes)
        [tag dom-attributes & rest] @(@components id)]
    (into
     [tag (into-attributes (dissoc dom-attributes :version :id) attributes)]
     rest)))

(defn subcomponent-ids
  "Return a seq of the ids of the subcomponents of the dom."
  [dom]
  (if (vector? dom)
    (if (= (first dom) component)
      [(:id (second dom))]
      (reduce (fn [subcomponents dom]
                (into subcomponents (subcomponent-ids dom)))
              [] dom))
    []))

(defn into-atom-map
  "Incorporate an update of new doms into an atom containing a map of atoms,
  creating or deleting atoms as called for by the new doms."
  ;; TODO: If performance is a problem, try adding a :key property
  ;;       that matches the id. The React documentation claims
  ;;       that speeds up re-rendering of sequences of many items.  
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

;;; The next code deals with the information we need to send to the server.

(defn new-pending-for-server
  "Make the information that holds the information that needs to be sent
   the server, or that has not been acknowledged yet by the server."
  []
  (atom {;; The number of the next action we will tell the server about.
         :next-action-number 0
         ;; A map from number to action.
         :actions {}
         ;; The ids and versions of doms that we have received from
         ;; the server and not acknowledged to it.
         :acknowledgments {}}))

(def pending-for-server (new-pending-for-server))

(defn update-add-action
  "Add an action to inform the server about."
  [pending action]
  (-> pending
      (assoc-in [:actions (:next-action-number pending)] action)
      (update-in [:next-action-number] inc)))

(defn update-remove-actions-acknowledged
  "Remove actions that are in the list of acknowledged ids."
  [pending acknowledged]
  (update-in pending [:actions] #(remove-keys % acknowledged)))

(defn update-add-dom-acknowledgments
  "Add dom id -> version pairs that we need to acknowledge to the server"
  [pending doms]
  (update-in pending [:acknowledgments]
             #(into % (map (fn [[tag {:keys [id version]} &rest]]
                             [id version])
                           doms))))

(defn update-for-response
  "Add the need to acknowledge doms we received, and
   remove outgoing actions that were acknowledged."
  [pending response]
  (let [{:keys [acknowledge doms]} response]
    (-> pending
        (update-add-dom-acknowledgments doms)
        (update-remove-actions-acknowledged acknowledge))))

(defn add-pending-action
  "Add the action to the pending actions."
  [action]
  (swap! pending-for-server update-add-action action))

(defn process-response-for-pending
  "Do the processing for a response."
  [response]
  (swap! pending-for-server update-for-response response))

(defn take-pending-params
  "Return any pending information for the server, and for information
   that will not be acknowledged by the server, remove it from the pending
   information."
  []
  (let [[pending _] (swap-returning-both! pending-for-server
                                          #(assoc % :acknowledgments {}))
        {:keys [actions acknowledgments]} pending]
    (cond-> {}
      (not= actions {}) (into {:actions actions})
      (not= acknowledgments {}) (into {:acknowledge acknowledgments}))))

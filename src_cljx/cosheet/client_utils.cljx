(ns cosheet.client-utils
  (:require #+cljs [reagent.core :as reagent :refer [atom]]
            [cosheet.dom-utils :refer [into-attributes]])
  )

(def components (clojure.core/atom {}))

(defn component [attributes name]
  (let [[tag dom-attributes & rest] @(@components name)]
    (into
     [tag (into-attributes (dissoc dom-attributes :version) attributes)]
     rest)))

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

(defn remove-ids
  "Remove the given ids from the map."
  [map ids]
  (reduce (fn [map id] (dissoc map id))
          map ids))

(defn add-ids
  "Add atoms for the given ids to the map."
  [map ids]
  (reduce (fn [map id] (assoc map id (atom [:div {:id id :version -1}])))
          map ids))

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
                          (remove-ids
                           (set-difference old-subcomponents
                                           subcomponents))
                          (add-ids
                           (set-difference subcomponents
                                           old-subcomponents)))))
                amap))
            amap)))
      amap update))))


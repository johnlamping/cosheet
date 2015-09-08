(ns cosheet.server.actions
  (:require
   [hiccup.page :refer [html5 include-js include-css]]
   [ring.util.response :refer [response]]
   (cosheet
    [debug :refer [simplify-for-print]]
    [store :refer [update-content do-update!]]
    store-impl
    mutable-store-impl
    [entity :refer [StoredEntity description->entity content elements]]
    [query :refer [query-matches]])
   (cosheet.server
    [dom-tracker :refer [id->key]]
    [render :refer [visible-to-list canonicalize-list]])))

;;; TODO: validate the data coming in, so nothing can cause us to
;;; crash.

;;; TODO: Once we can handle :content keys, change the render to
;;; provide them.

(defn item-referents
  "Return the elements of a key that refer to items."
  [key]
  (vec (filter #(or (not (sequential? %))
                    (= (first %) :parallel))
               key)))

(defn item->canonical-visible
  "Return the canonical form of the visible information for the item."
  [item]
  (canonicalize-list (visible-to-list item)))

(defn id->canonical-visible
  "Return the canonical form of the visible information
  for the id in the store."
  [store id]
  (item->canonical-visible (description->entity id store)))

(defn visible-matching-element
  "Given the list form of visible information and an item,
  find an element of the item that matches the visible information.
  Return nil if there is no matching element."
  [store visible-info item]
  (first (filter #(= (item->canonical-visible %) visible-info)
                 (elements item))))

(defn instantiate-item-id
  "Given the id of an exemplar item and a regular item, find an element
   of the item that matches the visible information of the exemplar.
  Return nil if there is no matching element."
  [store exemplar-id item]
  (visible-matching-element
   store (id->canonical-visible store exemplar-id) item))

(defn instantiate-exemplar
  "Given a store, an exemplar, and an item, instantiate the exemplar
  to the item, returning the sequence of items matched.
  The exemplar must have been pruned to only referents that refer to items."
  [store exemplar item]
  (assert (vector? exemplar))  ; So peek and pop take from end.
  (if (empty? exemplar)
    [item]
    (let [last-referent (peek exemplar)
          remainder (pop exemplar)]
      (if (sequential? last-referent)
        (let [[type exemplar item-ids] last-referent
              exemplar-referents (item-referents exemplar)]
          (assert (= type :parallel))  ; Other complex referents filtered.
          (assert (empty? remainder))  ; Parallel referents must be first.
            (mapcat (partial instantiate-exemplar store exemplar-referents)
                    (remove nil? (map #(instantiate-item-id store % item)
                                      item-ids))))
        (let [exemplar-item (instantiate-item-id store last-referent item)]
          (if (nil? exemplar-item)
            []
            (instantiate-exemplar store remainder exemplar-item)))))))

(defn key->items
  "Return the list of items that a key describes."
  [store key]
  (let [referent (first (item-referents key))]
    (cond
      (nil? referent)
      []
      (sequential? referent)
      (let [[type exemplar item-ids] referent
            exemplar-referents (item-referents exemplar)]
        (assert (= type :parallel))
        (mapcat (partial instantiate-exemplar store exemplar-referents)
                (map #(description->entity % store) item-ids)))
      true
      [(description->entity referent store)])))

(defn set-content-handler
  [store dom-tracker id from to]
  ;; TODO: Handle adding and deleting. (In the former case, the key
  ;; will be a :condition.)
  (let [key (id->key dom-tracker id)
        items (key->items store key)]
    (println "set id:" id " with key:" (simplify-for-print key))
    (println "total items:" (count items))
    (println "from:" from " to:" to)
    (reduce (fn [store item]
              (if (= (content item) from)
                (update-content store (:item-id item) to)
                store))
            store items)))

;;; TODO: Undo functionality should be added here. It shouldn't be
;;; hard, because the updated store already has a list of changed ids.
;;; Support could even be incorporated into immutable stores.
(defn do-action
  [mutable-store dom-tracker [action-type & action-args]]
  (let [handler (case action-type
                  :set-content set-content-handler
                  nil)]
    (if handler
      (do-update! mutable-store
                  (fn [store] (or (apply handler store dom-tracker action-args)
                                  store)))
      (println "unknown action type:" action-type))))

(defn do-actions
  [mutable-store dom-tracker actions]
  (let [keys (sort (keys actions))]
    (doseq [key keys]
      (let [action (actions key)]
        (do-action mutable-store dom-tracker action)))))

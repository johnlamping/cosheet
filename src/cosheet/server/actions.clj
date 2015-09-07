(ns cosheet.server.actions
  (:require
   [hiccup.page :refer [html5 include-js include-css]]
   [ring.util.response :refer [response]]
   (cosheet
    [store :refer [update-content!]]
    store-impl
    mutable-store-impl
    [entity :refer [StoredEntity content]]
    [query :refer [query-matches]])
   (cosheet.server
    [dom-tracker :refer [id->key]])))

;;; TODO: validate the data coming in, so nothing can cause us to
;;; crash.

;;; TODO: add mutation methods to mutable entities, so we can do
;;; operations to them, rather than to the store.

(defn set-content
  [store dom-tracker id from to]
  ;;; TODO: Make this work for tags that apply to multiple items.
  ;;; TODO: Check the key and the type of item
  (let [key (id->key dom-tracker id)
        item (first key)]
    (println "key " key "item " item "id " (:item-id item))
    (println "from " from "  to " to )
    (when (satisfies? StoredEntity item)
      ;; TODO: check that the current value is what the user expected.
      (update-content! store (:item-id item) to))))

(defn do-action
  [store dom-tracker action]
  (case (first action)
    :set-content (apply set-content store dom-tracker (rest action))
    (println "unknown action type " (first action))))

(defn do-actions
  [store dom-tracker actions]
  (let [keys (sort (keys actions))]
    (doseq [key keys]
      (let [action (actions key)]
        (case (first action)
          :set-content (apply set-content store dom-tracker (rest action))
          (println "unknown action type " (first action)))))))

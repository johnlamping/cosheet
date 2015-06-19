(ns cosheet.server.views
  (:require
   [hiccup.page :refer [html5 include-js include-css]]
   [ring.util.response :refer [response]]
   (cosheet
    [store :refer [new-element-store new-mutable-store current-store]]
    store-impl
    mutable-store-impl
    [entity :refer [description->entity]]
    entity-impl
    [store-utils :refer [add-entity]]
    [query :refer [query-matches]]
    query-impl
    [computation-manager :refer [new-management compute]])
   (cosheet.server
    [render :refer [item-DOM]]
    [dom-tracker :refer [new-dom-tracker add-dom
                         process-acknowledgements response-doms]]
    [actions :refer [do-actions]])))

(defn initial-page []
  (html5
    [:head
     [:title "Hello World"]
     (include-js "/js/main.js")
     (include-css "style.css")]
    [:body
     [:div#app "Root"]
     [:div#edit_holder [:input#edit_input]]
     [:script "cosheet.client.run();"]]))

(defn create-store
  []
  (let [starting-item '("Joe"
                        (:root :invisible)
                        (2 :order)
                        ("male" (1 :order))
                        ("married" (2 :order))
                        (39 (3 :order)
                            ("age" tag)
                            ("doubtful" "confidence")))
        [store id] (add-entity (new-element-store) nil starting-item)]
    (new-mutable-store store)))

(defonce store (create-store))

(defonce management (new-management 1))

(defn create-tracker
  [mutable-store]
  (let [immutable-root-item (:v (first (cosheet.query-impl/query-matches
                                        '(:variable (:v :name)
                                                    ((nil :root) :condition)
                                                    (true :reference))
                                        (current-store store))))
        root-item (description->entity (:item-id immutable-root-item)
                                       mutable-store)
        definition [item-DOM root-item #{} {}]
        tracker (new-dom-tracker management)]
    (add-dom tracker "root" definition)
    tracker))

;;; TODO: this needs to be separate for each session.
(def dom-tracker (atom nil))

(defn ajax-response [request]
  (let [params (:params request)]
    (println "request params" params)
    (when (or (:initialize params) (nil? @dom-tracker))
      (println "initializing")
      (reset! dom-tracker (create-tracker store)))
    (println "process acknowledgements" (:acknowledge params))
    (process-acknowledgements @dom-tracker (:acknowledge params))
    (let [actions (:actions params)]
      (when actions
        (do-actions store @dom-tracker actions)
        ;; TODO: rather than compute everything, wait a little while,
        ;; then take whatever we have.
        (compute management))
      ;; Note: We must get the doms after doing the actions, we can
      ;; immediately show the response to the actions.
      (let [doms (response-doms @@dom-tracker 2)
            answer (cond-> {}
                     (> (count doms) 0) (assoc :doms doms)
                     (> (count actions) 0) (assoc :acknowledge
                                                  (vec (keys actions))))]
        (println "response" answer)
        (response answer)))))


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

;;; TODO: this needs to be separate for each web page.
(def dom-tracker (atom nil))

(defn ajax-response [request]
  (let [params (:params request)
        {:keys [actions acknowledge initialize]} params
        must-initialize (or initialize (nil? @dom-tracker))]
    (println "request params" params)
    (when must-initialize
      (println "initializing")
      (reset! dom-tracker (create-tracker store))
      (compute management 100))
    (println "process acknowledgements" acknowledge)
    (process-acknowledgements @dom-tracker acknowledge)    
    (when actions
      (do-actions store @dom-tracker actions)
      (compute management 100))
    ;; Note: We must get the doms after doing the actions, so we can
    ;; immediately show the response to the actions.
    (let [doms (response-doms @@dom-tracker 10)
          answer (cond-> {}
                   (> (count doms) 0) (assoc :doms doms)
                   actions (assoc :acknowledge (vec (keys actions))))]
      (println "response" answer)
      (response answer))))

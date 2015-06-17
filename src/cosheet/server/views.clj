(ns cosheet.server.views
  (:require
   [hiccup.page :refer [html5 include-js include-css]]
   [ring.util.response :refer [response]]
   (cosheet
    [store :refer [new-element-store new-mutable-store]]
    store-impl
    mutable-store-impl
    [entity :refer [description->entity]]
    entity-impl
    [store-utils :refer [add-entity]]
    [computation-manager :refer [new-management compute]])
   (cosheet.server
    [render :refer [item-DOM]]
    [dom-tracker :refer [new-dom-tracker add-dom
                         process-acknowledgements response-doms]])))

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

(defn create-tracker
  []
  (let [starting-item '("Joe"
                        (2 :order)
                        ("male" (1 :order))
                        ("married" (2 :order))
                        (39 (3 :order)
                            ("age" tag)
                            ("doubtful" "confidence")))
        [store id] (add-entity (new-element-store) nil starting-item)
        mutable-store (new-mutable-store store)
        item (description->entity id mutable-store)
        definition [item-DOM item #{} {}]
        management (new-management)
        tracker (new-dom-tracker management)]
    (add-dom tracker "root" definition)
    (compute management)
    tracker))

(def dom-tracker (atom nil))

(defn ajax-response [request]
  (let [params (:params request)]
    (println "request params" params)
    (when (or (:initialize params) (nil? @dom-tracker))
      (println "initializing")
      (reset! dom-tracker (create-tracker)))
    (println "process acknowledgements" (:acknowledge params))
    (process-acknowledgements @dom-tracker (:acknowledge params))
    (let [doms (response-doms @@dom-tracker 2)]
      (println "doms" doms)
      (response (if (> (count doms) 0) {:doms doms} {})))))


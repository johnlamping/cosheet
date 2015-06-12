(ns cosheet.server.views
  (:require
   [hiccup.page :refer [html5 include-js]]
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

(defn index-page []
  (html5
    [:head
      [:title "Hello World"]
      (include-js "/js/main.js")]
    [:body
     [:div#app "Root"]
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
      (reset! dom-tracker (create-tracker))
      (println "component keys" (keys (:components @@dom-tracker))))
    (comment (case (:params request)
               {:initialize true}
               (response [[:div {:id "root" :version 1}
                           "Hello " [:cosheet/component {} :new]
                           " world, the time is now"
                           [:cosheet/component {} :clock]]
                          [:span {:id :new :version 1} "new"]])
               {:acknowledge {"root" 1 :new 1}}
               (response [[:div {:id :clock :version 1} "now"]])
               ({} {:acknowledge {:clock 1}})
               (response [])
               (do (println "unknown request" request)
                   (response []))))
    (println "process acknowledgements" (:acknowledge params))
    (process-acknowledgements @dom-tracker (:acknowledge params))
    (let [doms (response-doms @@dom-tracker 2)]
      (println "doms" doms)
      (response doms))))


(ns cosheet.server.views
  (:require
   [hiccup.page :refer [html5 include-js]]
   [ring.util.response :refer [response]]))

(defn index-page []
  (html5
    [:head
      [:title "Hello World"]
      (include-js "/js/main.js")]
    [:body
     [:div#app "Hello World"]
     [:script "cosheet.client.run();"]]))

(defn ajax-response [request]
  (println "request params" (:params request))
  (case (:params request)
    {:initialize true}
    (response [[:div {:id :root :version 1}
                "Hello " [:cosheet/component {} :new]
                " world, the time is now"
                [:cosheet/component {} :clock]]
               [:span {:id :new :version 1} "new"]])
    {:acknowledge {:root 1 :new 1}}
    (response [[:div {:id :clock :version 1} "now"]])
    ({} {:acknowledge {:clock 1}})
    (response [])
    (do (println "unknown request" request)
        (response []))))


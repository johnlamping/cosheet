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
  (if (= (:params request) {})
    (response [[:div {:id :message :version 1}
                "Helllo " [:cosheet/component {}  :new]
                " world, the time is now"]
               [:span {:id :new :version 1} "new"]])
    (response [])))


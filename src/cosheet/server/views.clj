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

(defn ajax-response []
  (response {:message [:div "Hello " [:cosheet/component :new]
                       " world, the time is now"]
             :new [:span "new"]}))


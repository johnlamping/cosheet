(ns cosheet.server.views
  (:require
    [hiccup.page :refer [html5 include-js]]))

(defn index-page []
  (html5
    [:head
      [:title "Hello World"]
      (include-js "/js/main.js")]
    [:body
      [:h1 "Hello World"]]))

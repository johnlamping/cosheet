(ns cosheet.server.routes
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response]
            [compojure.core :refer [GET defroutes]]
            [hiccup.middleware :refer [wrap-base-url]]
            [cosheet.server.views :refer [index-page]]))

(defroutes main-routes
  (GET "/" [] (index-page))
  (route/resources "/")
  (route/not-found "Page not found"))

;;; This allows the server to be run from lein with
;;;    lein ring server-headless 3000
;;; Running the server from lein makes it automatically update
;;; whenever a source file is changed.
(def app
  (-> (handler/site main-routes)
      (wrap-base-url)))



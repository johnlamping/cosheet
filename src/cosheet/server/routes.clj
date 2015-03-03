(ns cosheet.server.routes
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response]
            [compojure.core :refer [GET POST defroutes]]
            [hiccup.middleware :refer [wrap-base-url]]
            [ring.middleware.transit :refer [wrap-transit-response]]
            [cosheet.server.views :refer [index-page ajax-response]]))

(defroutes main-routes
  (GET "/" [] (index-page))
  (POST "/ajax-request" [] (ajax-response))
  (route/resources "/")
  (route/not-found "Page not found"))

;;; This allows the server to be run from lein with
;;;    lein ring server-headless 3000
;;; Running the server from lein makes it automatically update
;;; whenever a source file is changed.
(def app
  (-> main-routes ; (handler/site main-routes)
      (wrap-transit-response {:encoding :json, :opts {}})
      (wrap-base-url)))



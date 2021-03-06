(ns cosheet.server.routes
  (:gen-class) ;; So we don't need lein ring.
  (:require [compojure.route :as route]
            [compojure.core :refer [GET POST defroutes]]
            [hiccup.middleware :refer [wrap-base-url]]
            [ring.middleware.transit :refer [wrap-transit-response
                                             wrap-transit-params]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.adapter.jetty :refer [run-jetty]]
            [cosheet.server.views :refer [initial-page handle-ajax]]))

(defroutes main-routes
  (GET "/cosheet/:path{.*}" [path referent selector]
       (initial-page (str "/cosheet/" path) referent selector))
  (GET ".+//:path{.*}" [path referent selector]
       (initial-page (str "//" path) referent selector))
  (GET "/~/:path{.*}" [path referent selector]
       (initial-page (str "/~/" path) referent selector))
  (POST "/ajax-request/:id" request (handle-ajax request))
  (route/resources "/")
  (route/not-found "Page not found"))

;;; Note: It is probably not necessary, but you may have to first run
;;;   lein cljsbuild once
;;; This allows the server to be run from lein with
;;;    lein ring server-headless 3000
;;; Running the server from lein makes it automatically update
;;; whenever a source file is changed.
(def app
  (-> main-routes
      (wrap-params)
      (wrap-transit-params)
      (wrap-transit-response {:encoding :json, :opts {}})
      (wrap-base-url)))

;; So we don't need lein ring.
;; Pass the handler to Jetty on port 3000
(defn -main []
  (run-jetty app {:port 3000}))

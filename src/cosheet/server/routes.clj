(ns cosheet.server.routes
  (:gen-class) ;; So we don't need lein ring.
  (:require [compojure.route :as route]
            [compojure.core :refer [GET POST defroutes]]
            [hiccup.middleware :refer [wrap-base-url]]
            [ring.middleware.transit :refer [wrap-transit-response
                                             wrap-transit-params]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.adapter.jetty :refer [run-jetty]]
            [cosheet.server.views :refer [initial-page handle-ajax list-user-files]]
            [cosheet.server.db :refer [get-user-pwdhash get-all-users]]

            [buddy.auth :refer [authenticated? throw-unauthorized]]
            [buddy.auth.backends.session :refer [session-backend]]
            [buddy.auth.middleware :refer [wrap-authentication wrap-authorization]]
            [buddy.hashers :as hashers]

            [ring.util.response :refer [response redirect content-type]]
            [ring.middleware.session :refer [wrap-session]]

            [compojure.response :refer [render]]
            [clojure.java.io :as io]
            [ring.adapter.jetty :as jetty])
(:gen-class))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Authentication                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Authentication Handler
;; Used to respond to POST requests to /login.

(defn login-authenticate
  "Check request username and password against user credentials dB
  username and passwords.

  On successful authentication, set appropriate user
  into the session and redirect to the value of
  (:next (:query-params request)). On failed
  authentication, renders the login page."
  [request]
  ;;(println "login-authenticate request " request)

  (let [username (get-in request [:form-params "username"])
        password (get-in request [:form-params "password"])
        session (:session request)
        found-pwdhash (get (get-user-pwdhash username) (keyword "pwdhash"))]
    ;;(println "login-authenticate username:" username " pwdhash: " found-pwdhash)
    ;;(println "pwd" password " hash " (hashers/encrypt password))
    (if (and found-pwdhash (hashers/check password found-pwdhash))
      (let [next-url (get-in request [:query-params "next"] "/")
            updated-session (assoc session :identity username)]
        (-> (redirect next-url)
            (assoc :session updated-session)))
      (let [content (slurp (io/resource "public/login.html"))]
        (render content request)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Controllers                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn get-initial-page
  [request path referent selector]
  (if-not (authenticated? request)
    (throw-unauthorized)
    ;;
    ;; user data is stored in ~/cosheet/userdata/<user-id>/<filename>
    ;; TODO use global variable for "~/cosheet/userdata"
    (let [user-id (get-in request [:session :identity] "unknown")]
      (initial-page (str "/~/cosheet/userdata/" user-id "/" path) referent selector)
    )
  ))

;; Login page controller
;; It returns a login page on get requests.

(defn login
  [request]
  (let [content (slurp (io/resource "public/login.html"))]
    (render content request)))

;; Logout handler
;; Responsible for clearing the session.

(defn logout
  [request]
  (-> (redirect "/login")
      (assoc :session {})))

(defn list-files
  [request]
  (if-not (authenticated? request)
    (throw-unauthorized)
    (let [user-id (get-in request [:session :identity] "unknown")]
      (list-user-files (str "/~/cosheet/userdata/" user-id))
    ;; placeholder only. TODO: generate a list of user files
    (let [content (slurp (io/resource "public/index-user-files.html"))]
      (render content request)))) )

(defn create-file
  [request]
  (if-not (authenticated? request)
    (throw-unauthorized)
    (let [filename (get-in request [:form-params "filename"])
          next-url (str "/cosheet/" filename)
          ]
      (-> (redirect next-url))
      )))

(defn ajax-request
  [request]
  (if-not (authenticated? request)
    (throw-unauthorized)
    (handle-ajax request))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routes and Middlewares                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defroutes app    ;;main-routes
  (GET "/" [] list-files)
  (POST "/" [] create-file)
  (GET "/cosheet/:path{.*}" [path referent selector :as request]
       (get-initial-page request path referent selector))
  ;;(GET ".+//:path{.*}" [path referent selector]
  ;;     (initial-page (str "//" path) referent selector))
  ;;(GET "/~/:path{.*}" [path referent selector]
  ;;     (initial-page (str "/~/" path) referent selector))
  (POST "/ajax-request/:id" [] ajax-request)
  (GET "/login" [] login)
  (POST "/login" [] login-authenticate)
  (GET "/logout" [] logout)
  (route/resources "/")
  (route/not-found "Page not found"))

;; User defined unauthorized handler
;;
;; This function is responsible for handling
;; unauthorized requests (when unauthorized exception
;; is raised by some handler)

(defn unauthorized-handler
  [request metadata]
  (cond
    ;; If request is authenticated, raise 403 instead
    ;; of 401 (because user is authenticated but permission
    ;; denied is raised).
    (authenticated? request)
    (-> (render (slurp (io/resource "public/error.html")) request)
        (assoc :status 403))
    ;; In other cases, redirect the user to login page.
    :else
    (let [current-url (:uri request)]
      (redirect (format "/login?next=%s" current-url)))))

;; Create an instance of auth backend.

(def auth-backend
  (session-backend {:unauthorized-handler unauthorized-handler}))


;;; Note: It is probably not necessary, but you may have to first run
;;;   lein cljsbuild once
;;; This allows the server to be run from lein with
;;;    lein ring server-headless 3000
;;; Running the server from lein makes it automatically update
;;; whenever a source file is changed.

;; So we don't need lein ring.
;; Pass the handler to Jetty on port 3000
(defn -main
  [& args]
  (as-> app $
        (wrap-authorization $ auth-backend)
        (wrap-authentication $ auth-backend)
        (wrap-params $)
        (wrap-session $)
        (wrap-transit-params $)
        (wrap-transit-response $ {:encoding :json, :opts {}})
        (wrap-base-url $)
        (jetty/run-jetty $ {:port 3000})))

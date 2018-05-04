(ns cosheet.server.routes
  (:gen-class) ;; So we don't need lein ring.
  (:require [compojure.route :as route]
            [compojure.core :refer [GET POST defroutes]]
            [hiccup.middleware :refer [wrap-base-url]]
            [ring.middleware.transit :refer [wrap-transit-response
                                             wrap-transit-params]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.adapter.jetty :refer [run-jetty]]
            [cosheet.server.views :refer [initial-page handle-ajax list-user-files
                                          admin-page create-user delete-user-view]]
            [cosheet.server.session-state :refer [isAdmin get-userdata-path url-path-to-file-path]]
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
        found-pwdhash ((get-user-pwdhash username) :pwdhash)]
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

;; cosheet initial page
;;
(defn get-initial-page
  [request path referent selector]
  (if-not (authenticated? request)
    (throw-unauthorized)
    ;;
    ;; user data is stored in ~/cosheet/userdata/<user-id>/<filename>
    (let [user-id (get-in request [:session :identity] "unknown")]
      (initial-page (str (get-userdata-path user-id) "/" path) referent selector)
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

;; Per user's home page
;; list files in the user's directory
(defn list-files
  [request]
  (if-not (authenticated? request)
    (throw-unauthorized)
    ;; call list-user-files
    (let [user-id (get-in request [:session :identity] "unknown")]
      (list-user-files (get-userdata-path user-id) user-id))
    ;; static html response - deprecated
    ;(let [content (slurp (io/resource "public/index-user-files.html"))]
    ;  (render content request))
    ))

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

(defn get-admin
  [request]
  (if-not (authenticated? request)
    (throw-unauthorized)
    ;; only allow "admin" user to access admin page
    (let [user-id (get-in request [:session :identity] "unknown")]
      (if-not (isAdmin user-id)
        (throw-unauthorized)
        (admin-page user-id)
        ))
  ))

(defn post-admin
  [request]
  (if-not (authenticated? request)
    (throw-unauthorized)
    ;; only allow "admin" user to access admin page
    (let [user-id (get-in request [:session :identity] "unknown")]
      (if-not (isAdmin user-id)
        (throw-unauthorized)
        (let [username (get-in request [:form-params "username"])
              password (get-in request [:form-params "password"])
              userdata-full-path (url-path-to-file-path (get-userdata-path username))]
          (create-user username password userdata-full-path)))
  )))

(defn delete-user
  [request username]
  (if-not (authenticated? request)
    (throw-unauthorized)
    (let [user-id (get-in request [:session :identity] "unknown")]
      (if-not (isAdmin user-id)
        (throw-unauthorized)
        (if (isAdmin username)  ; can't remove "admin" user
          (throw-unauthorized)
          (delete-user-view username))))
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routes and Middlewares                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defroutes main-routes    ;;main-routes
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
  (GET "/admin" [] get-admin)
  (POST "/admin" [] post-admin)
  ;; TODO, should be PUT or POST, but temp hack for now
  (GET "/admin/delete/:username" [username :as request] (delete-user request username))
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

(def app
  (-> main-routes
      (wrap-authorization auth-backend)
      (wrap-authentication auth-backend)
      (wrap-params)
      (wrap-session)
      (wrap-transit-params)
      (wrap-transit-response {:encoding :json, :opts {}})
      (wrap-base-url)))

;;; Note: It is probably not necessary, but you may have to first run
;;;   lein cljsbuild once
;;; This allows the server to be run from lein with:
;;;    lein run
;;; or if you want the running code to update:
;;;    lein ring server-headless 3000
;;; Running the server from lein makes it automatically update
;;; whenever a source file is changed.

;; So we don't need lein ring.
;; Pass the handler to Jetty on port 3000
(defn -main
  [& args]
  (jetty/run-jetty app {:port 3000}))

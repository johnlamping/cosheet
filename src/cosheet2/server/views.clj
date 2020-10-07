(ns cosheet2.server.views
  (:require
    [hiccup.page :refer [html5 include-js include-css]]
    [ring.util.response :refer [response]]
    [clojure.java.io :as io]
    (cosheet2
      [store :refer [current-store data-to-store store-reset! id-valid?]]
      [store-impl :refer [->ItemId]]
      mutable-store-impl
      [entity :refer [description->entity in-different-store
                      content label->elements]]
      entity-impl
      [query :refer [matching-elements]]
      [debug :refer [simplify-for-print]]
      query-impl
      [expression :refer [expr expr-let]]
      [calculator :refer [new-calculator-data compute]]
      [task-queue :refer [new-priority-task-queue finished-all-tasks?]]
      [hiccup-utils :refer [dom-attributes add-attributes]]
      [reporter :as reporter]
      [map-state :refer [map-state-reset! map-state-get-current]]
      [debug :refer [profile-and-print-reporters]])
    (cosheet2.server
      [dom-manager :refer [request-client-refresh
                           process-acknowledgements get-response-doms]]
      [session-state :refer [create-session ensure-session forget-session
                             create-client-state url-path-to-file-path
                             get-userdata-path
                             remove-url-file-extension isAdmin
                             get-session-state queue-to-log update-store-file]]
      [db :refer [get-all-users add-user-to-db remove-user-from-db
                  get-user-pwdhash]]
      [actions :refer [confirm-actions do-actions]]))
  (:import (org.h2.util New)))

(defonce common-queue (new-priority-task-queue 1))
(defonce calculator-data (new-calculator-data common-queue))

(defonce time-formatter (java.text.DateFormat/getDateTimeInstance))
(defn now-string
  "Return a string representing the current time, down to milliseconds"
  []
  (let [now (java.util.Calendar/getInstance)
        datime (.format time-formatter (.getTime now))
        datime-len (count datime)
        millis (.get now java.util.Calendar/MILLISECOND)]
    ;; Put the millis after the seconds, and before the AM or PM.
    (str (subs datime 0, (- datime-len 3))
         ":"
         millis
         (subs datime, (- datime-len 3)) )))

;;; Generate html page listing all user files with href to edit file
;;; page. Also contains logout.
(defn list-user-files [user-id servlet-path]
  (when-let [directory (get-userdata-path user-id)]
    (html5
      [:head
       [:title "user files"]
       ]
      [:body
        [:h1 "Welcome " user-id]
        ;; list current files
        [:h3 "Cosheet Files:"]
        [:ul  (let [files (.list (io/file directory))]
           (for [file files]
             (if (clojure.string/ends-with? file ".cosheet")
               [:li [:a {:href (str servlet-path "/cosheet/" file)} file ] ]
             )))
         ]
        [:h3 "Create New File"]
        [:form {:action (str servlet-path "/") :method "POST"}
           ;(util/anti-forgery-field) ; prevents cross-site scripting attacks
           [:p " New file: " [:input {:type "text" :name "filename"}] [:input {:type "submit" :value "Create"}]]]
        [:p (if (isAdmin user-id) [:a {:href (str servlet-path "/admin")} "Add/Disable Users"])]
        [:a {:href (str servlet-path "/logout")} "Logout"]
      ])
  ))

;;; Administration page for managing users.
(defn admin-page [user-id servlet-path]
  (html5
    [:head
     [:title "Administration"]
     ]
    [:body
      [:h1 "Manage Users "]
      ;; list current users
     [:h3 "Active Users"]
     [:ul  (let [users (get-all-users)]
        (for [user users]
          (let [username (user :username)]
            [:li (str username "&nbsp;&nbsp;&nbsp;&nbsp;")
             [:a {:href (str servlet-path "/admin/delete/" username)}
              "disable"]]
          )))]

      [:form {:action (str servlet-path "/admin") :method "POST"}
         ;(util/anti-forgery-field) ; prevents cross-site scripting attacks
         [:h3 " Create New User"]
         [:p [:input {:type "text" :placeholder "username" :name "username"}]]
         [:p [:input {:type "text" :placeholder "password" :name "password"}]]
         [:p [:input {:type "submit" :value "Create"}]]]
      [:a {:href (str servlet-path "/logout")} "Logout"]
    ])
 )

;;; Create user
(defn create-user [user-id password servlet-path]
  (try ;; check if user-id is already in dB
    (if (get-user-pwdhash user-id)
      (throw (Exception. "User already exists")))
    (let [userdata-path (get-userdata-path user-id)]
      ;; create userdata dir
      (println "checking userdata dir " userdata-path)
      (if-not (.exists (java.io.File. userdata-path))
        (let [success (.mkdir (java.io.File. userdata-path))]
          (if-not success
            (throw (Exception. "Unable to create user data directory")))
          (println "created userdata dir at " userdata-path))
        (println "userdata dir for " user-id "already exists")
        ))
    (println "adding " user-id " to dB")
    ;; add user-id to dB
    (if-not (add-user-to-db user-id password)
      (throw (Exception. "Unable to add user to dB.")))
    ;; success
    (println "User " user-id " added successfully")
    (html5
     [:head
      [:title "Success"]
      [:body
       [:div (str "User " user-id " created successfully.")]
       [:a {:href (str servlet-path "/admin")} "Back"]
       ]])
    (catch Exception e
      (println (str "create-user caught exception: " e))
      (println (clojure.stacktrace/print-stack-trace e))
      ;; respond with error
      (html5
       [:head
        [:title "Error"]
        [:body
         [:div (str "Unable to create user. Exception " e)]]
        [:a {:href (str servlet-path "/admin")} "Back"]])
      )))

(defn delete-user-view [username servlet-path]
  (if (remove-user-from-db username)     ; TODO, better return error check
    (html5
      [:head
       [:title "Success"]
       [:body
        [:div (str "User " username " diabled successfully.")]
        [:a {:href (str servlet-path "/admin")} "Back"]
        ]])
    (      ; error case
      (html5
        [:head
         [:title "Error"]
         [:body
          [:div (str "Unable to disable " username)]
          [:a {:href (str servlet-path "admin")} "Back"]
          ]])
      ))
  )

(defn initial-page [file-path servlet-path referent-string]
  (println (now-string) "initial page"
           file-path referent-string)
  (if-let [session-id (create-session nil file-path referent-string
                                      common-queue calculator-data)]
    (do (println (now-string) "Got session")
        (html5
         [:head
          [:title (last (clojure.string/split file-path #"/"))]
          [:meta {:itemprop "session-id"
                  :content session-id}]
          (include-js "../js/main.js")
          (include-css "../style.css")]
         [:body
          [:div#toolbar.toolbar
           [:div#undo.tool
            [:img {:src "../icons/undo.gif"}]
            [:div.tooltip "undo (C-Z)"]]
           [:div#redo.tool
            [:img {:src "../icons/redo.gif"}]
            [:div.tooltip "redo (C-Y)"]]
           [:div.toolgap]
           [:div#expand.tool
            [:img {:src "../icons/expand.gif"}]
            [:div.tooltip "expand (C-E)"]]
           [:div.toolgap]
           [:div#add-twin.tool
            [:img {:src "../icons/add_twin.gif"}]
            [:Div.tooltip "add twin (C-=)"]]
           [:div#add-element.tool
            [:img {:src "../icons/add_element.gif"}]
            [:div.tooltip "add element (C->)"]]
           [:div#add-label.tool
            [:img {:src "../icons/add_label.gif"}]
            [:div.tooltip "add label (C-L)"]]
           [:div#add-sibling.tool
            [:img {:src "../icons/add_sibling.gif"}]
            [:div.tooltip "add sibling below (C-S)"]]
           [:div.toolgap]
           [:div#add-row.tool
            [:img {:src "../icons/add_row.gif"}]
            [:div.tooltip "add row below (C-_)"]]
           [:div#add-column.tool
            [:img {:src "../icons/add_column.gif"}]
            [:div.tooltip "add column right (C-|)"]]
           [:div.toolgap]
           [:div#delete-row.tool
            [:img {:src "../icons/delete_row.gif"}]
            [:div.tooltip "delete row"]]
           [:div#delete-column.tool
            [:img {:src "../icons/delete_column.gif"}]
            [:div.tooltip "delete column"]]
           [:div.toolgap]
           [:a.link {:href (str servlet-path "/logout")} "Logout"]
           [:a.link {:href (str servlet-path "/")} "Open"]]
          [:div#app "root"] ;; Client will create a component with id "root".
          [:div#select_holder.select_holder
           [:input#edit_input {"type" "text"}]]
          [:script "cosheet2.client.run();"]]))
     (html5
     [:head
      [:title "Hello World"]]
     [:body
      [:div "Invalid path."]])))

(defn read-item-sequence
  "Return a lazy seq of the sequence of items from the reader."
  [reader]
  (when-let [item (clojure.edn/read {:eof nil} reader)]
    (lazy-seq (cons item (read-item-sequence reader)))))

(defn replay-request
  [session-state request]
  (let [{:keys [actions]} request 
        action-sequence (confirm-actions actions (:client-state session-state))]
    (do-actions (:store session-state) session-state action-sequence))
  (compute calculator-data 100000))

(defn replay-item [session-state [type content]]
  (println "replaying item" type (when (not= type :store) content))
  (case type
    :store (let [store (:store session-state)]
             (let [new-store (data-to-store (current-store store) content)]
               (store-reset! store new-store))
             (compute calculator-data 100000))
    :opened (map-state-reset! (:client-state session-state) :last-action 0)
    :initialize (map-state-reset! (:client-state session-state) :last-action 0)
    :request (replay-request session-state content)
    :error nil)
  (Thread/sleep 100))

(defn do-replay [session-state replay]
  ;; First, make our own client state to run the replays in, so we get separate
  ;; numbering of actions.
  (let [{:keys [id url-path store client-state]} session-state
        client-state (create-client-state
                      store (:root-id client-state))
        session-state (assoc session-state :client-state client-state)]
    (when (clojure.string/ends-with? url-path ".history")
      (let [original-path (subs url-path 0 (- (count url-path) 8))]
        (try
          (let [items (with-open
                        [stream (clojure.java.io/input-stream
                                 (url-path-to-file-path
                                  (str original-path ".cosheetlog") id))]
                        (with-open [reader (java.io.PushbackReader.
                                            (java.io.InputStreamReader.
                                             stream))]
                          ;; We use a doall to force reading all the
                          ;; items before the with-opens exit and
                          ;; close the stream.
                          (doall (read-item-sequence reader))))]
            (println "starting replay.")
            (let [done (future (doseq [item items]
                                 (replay-item session-state item)))]
              (future (println "done replaying." @done))))
          (catch java.io.FileNotFoundException e
            nil))))))

;;; The parameters for the ajax request and response are:
;;; request:
;;;        :unload If true, the client page is about to be unloaded, and
;;;                the server should drop its state.
;;;         :clean If present, the value is the location part of the url.
;;;                The server should assume the client is
;;;                starting from scratch. No other parameters
;;;                should be present.
;;;        :replay If true, the path should end in .history, and the
;;;                history of the file withough .history will be replayed.
;;;            :id The session id of this client session. The initial html
;;;                returned to the client will have the session id
;;;                in its session-id metadata field.
;;;       :actions A map action-id -> action to be performed, where each
;;;                action looks like [action_type arg ...], and the
;;;                action types are those implemented in actions.clj.
;;;                The action ids should sort in the order in which
;;;                the actions should be done. Action ids in successive
;;;                requests should have successively higher sort orders.
;;;   :acknowledge A map component-id -> version of pairs for which
;;;                the dom of that version was received by the client.
;;; response:
;;;         :reload The server has no record of the session. The page
;;;                 should request a reload.
;;; :reset-versions The server has no record of the session. The page
;;;                 should reset its version information.
;;;           :doms A list of hiccup encoded doms of components. Their
;;;                 attributes will include a unique :id and a :version
;;;                 number that will increase for each change of the dom
;;;                 for that id. Inside the doms may be internal components
;;;                 encoded as [:component {<id and other attributes>}].
;;;         :select A list of an id to select, and an optional list of
;;;                 the only ids that may currently be selected for the
;;;                 command to take effect.
;;;           :open A url that should be opened in a new window.
;;;        :set-url A url that should be set as the current url of
;;;                 what the client is displaying. This is used to record
;;;                 the currently displayed tab in the url, so reloads
;;;                 and the like will remember it.
;;;    :acknowledge A vector of action ids of actions that have been
;;;                 performed.

(defn ajax-response [manager client-state actions client-info]
  ;; Note: We must get the doms after doing the actions, so we can
  ;; immediately show the response to the actions. Likewise, we
  ;; want to have done some computation, so if we need to send back
  ;; a select request, the dom we want to select will be going to
  ;; the client.
  (let [in-sync (map-state-get-current client-state :in-sync)
        doms (when in-sync (get-response-doms manager 100))
        answer (cond-> (select-keys client-info
                                    [:open :set-url :select])
                 (seq doms) (assoc :doms doms)
                 (not in-sync) (assoc :reset-versions true)
                 actions (assoc :acknowledge (vec (keys actions))))]
    (when (not= answer {})
      (let [stripped (update
                      answer :doms
                      #(map (fn [dom] (:id (dom-attributes dom)))
                            %))]
        (println (now-string) "response" stripped)))
    (response answer)))

(defn ensure-session-state
  [user-id params]
  (let [{:keys [id clean]} params]
    (or (get-session-state id)
        (when-let [url clean]
          (let [referent-string (second (re-find #"\?.*referent=([^&]*)" url))
                url-path (str "/" (second (re-find #"//[^/]*/([^?]*)" url)))
                file-path (url-path-to-file-path url-path user-id)]
            (ensure-session
             id file-path referent-string common-queue calculator-data))))))

(defn handle-ajax [request]
  (let [params (:params request)
        {:keys [actions replay unload clean acknowledge]}
        params
        user-id (get-in request [:session :identity] "unknown")
        session-state (ensure-session-state user-id params)]
    (if session-state
      (let [{:keys [manager file-path store client-state]} session-state]
        (when (or actions clean)
          (queue-to-log [:request (dissoc params :acknowledge)] file-path))
        (when (not= (dissoc request :id) {})
          (println (now-string) "Request" params))
        (when unload
          (println "Unloading session.")
          (forget-session (:id params)))
        (when clean
          (println "Client is clean.")
          (request-client-refresh manager)
          (map-state-reset! client-state :in-sync true)
          (map-state-reset! client-state :last-action nil))
        (when replay
          (do-replay session-state replay))
        (process-acknowledgements manager acknowledge)
        (let [action-sequence (confirm-actions actions client-state)]
          (let [client-info (cond-> (do-actions
                                     store session-state action-sequence)
                              clean (assoc :set-url
                                           (remove-url-file-extension clean)))]
            (update-store-file file-path)
            (compute calculator-data 1000)
            ;; If we have no doms ready for the client yet, try computing
            ;; some more.
            (when (empty? (get-response-doms manager 1))
              (compute calculator-data 10000))
            (ajax-response manager client-state actions client-info))))
      (response (if clean {} {:reset-versions true})))))
(ns cosheet.server.views
  (:import [java.nio.file Files CopyOption StandardCopyOption])
  (:require
   [hiccup.page :refer [html5 include-js include-css]]
   [ring.util.response :refer [response]]
   (cosheet
    [utils :refer [swap-control-return! ensure-in-atom-map! with-latest-value]]
    [orderable :as orderable]
    [store :refer [new-element-store new-mutable-store current-store
                   read-store write-store store-to-data  data-to-store
                   reset-store! id-valid?]]
    [store-impl :refer [->ItemId]]
    mutable-store-impl
    [entity :refer [description->entity in-different-store
                    content label->elements]]
    entity-impl
    [store-utils :refer [add-entity]]
    [query :refer [matching-elements]]
    [debug :refer [simplify-for-print]]
    query-impl
    [expression :refer [expr expr-let]]
    [expression-manager :refer [new-expression-manager-data compute]]
    [expression-manager-test :refer [check-propagation]]
    [task-queue :refer [finished-all-tasks?]]
    [dom-utils :refer [dom-attributes add-attributes]]
    [reporters :as reporter]
    [mutable-manager :refer [current-mutable-value]]
    [state-map :refer [new-state-map state-map-reset!]]
    [debug :refer [profile-and-print-reporters]])
   (cosheet.server
    [referent :refer [item-referent union-referent
                      referent->exemplar-and-subject
                      string->referent referent->string instantiate-to-items]]
    [render-utils :refer [make-component]]
    [order-utils :refer [order-items-R]]
    [render :refer [DOM-for-client-R
                    user-visible-item? starting-inherited]]
    [item-render :refer [item-content-DOM]]
    [tabs-render :refer [first-tab-R]]
    [dom-tracker :refer [new-dom-tracker add-dom request-client-refresh
                         process-acknowledgements response-doms
                         key->id dom-for-key?]]
    [actions :refer [update-add-blank-table-view confirm-actions do-actions]])))

(defn starting-store
  []
  (let [[store _] (add-entity (new-element-store) nil
                              (list orderable/initial :unused-orderable))
        [store _] (add-entity store nil '("tabs" (:tabs :non-semantic)))
        [store _] (update-add-blank-table-view store "tab")]
    store))

(defonce manager-data (new-expression-manager-data 0)) ;; TODO: Make it 1

;;; Store management. Stores may be shared across sessions.

;;; A map from name to a map
;;;  {:mutable <mutable-store>,
;;;   :agent <the agent responsible for saving the store, and writing log
;;;           entries. It's state is the
;;;           last version of the store written. We write the store by doing
;;;           a send to this agent, so that writes don't block interaction,
;;;           and will skip intermediate values if they get behind.>
;;;   :log-agent <the agent responsible for writing log entries.
;;;               its state is a writer opened to append to the log file.>
(def store-info (atom {}))

(defn name-to-path [name]
  (let [homedir (System/getProperty "user.home")]
    (clojure.string/join "" [homedir  "/cosheet/" name ".cst"])))

(defn path-to-Path [path]
  (java.nio.file.Paths/get
   (java.net.URI. (clojure.string/join "" ["file://" path]))))

(defn read-store-file [name]
  (with-open [stream (clojure.java.io/input-stream (name-to-path name))]
    (read-store (new-element-store) stream)))

(defn write-store-file-if-different
  "Function for running in the :agent of store-info.
   If the current immutable store is different from the written store,
   writes the current value to a file of the given name. Always returns the
   current-value."
   [written-store mutable-store name]
  ;; We write the latest value from the mutable store, rather than the value
  ;; at the time the send was done, so that we will catch up if we get behind.
  (with-latest-value [store (current-store mutable-store)]
    (when (not= written-store store)
      (let [temp-path (name-to-path (str name "_TEMP_"))]
        (clojure.java.io/delete-file temp-path true)
        (with-open [stream (clojure.java.io/output-stream temp-path)]
          (write-store store stream))
        (Files/move (path-to-Path temp-path) (path-to-Path (name-to-path name))
                    (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING,
                                            StandardCopyOption/ATOMIC_MOVE])))
      store)))

(defn write-log-entry
  "Function for running in the log-agent of store-info. Adds the entry
   to the log stream, and flushes the stream."
  [log-writer entry]
  (binding [*out* log-writer]
    (prn entry)
    (flush))
  log-writer)

(defn update-store-file [name]
  (when-let [info (@store-info name)]
    (send (:agent info) write-store-file-if-different (:mutable info) name)))

(defn queue-to-log
  "Add the entry to the queue to be written to the log."
  [entry name]
  (when-let [info (@store-info name)]
    (when-let [agent (:log-agent info)]
      (send agent write-log-entry entry))))

(defn ensure-store [name]
  ;; If there were a race to create the store, the log stream might get
  ;; opened multiple times in ensure-in-atom-map! To avoid that, we run under
  ;; a global lock.
  (locking store-info
    (ensure-in-atom-map!
     store-info name
     #(let [immutable (try (read-store-file %)
                           (catch java.io.FileNotFoundException e
                             (starting-store)))
            log-stream (try (java.io.FileOutputStream.
                             (name-to-path (str name "_LOG_")) true)
                            (catch java.io.FileNotFoundException e
                              nil))
            log-agent (when log-stream
                        (agent (clojure.java.io/writer log-stream)))]
        (when (and log-stream (= (.position (.getChannel log-stream)) 0))
          (send log-agent write-log-entry [:store (store-to-data immutable)]))
        (send log-agent write-log-entry [:opened])
        {:mutable (new-mutable-store immutable)
         :agent (agent immutable)
         :log-agent log-agent}))))

;;; Session management. There is a tracker for each session.

;;; A map from file name to session state.
;;; Session state consists of a map
;;;           :name  The file name to mirror the store.
;;;          :store  The store that holds the data.
;;;        :tracker  The tracker for the session.
;;;   :client-state  A state-map holding these keys:
;;;                 :referent  The referent for the root of the display
;;;         :subject-referent  The referent, if any for the subject of the
;;;                            root of the display.
;;;              :last-action  The id of the last action we did.
;;;                            This keeps us from repeating an action if the
;;;                            client gets impatient and repeats actions while
;;;                            we are working on them.
;;;               :alternate   Either nil or a map indicating an alternate
;;;                            interpretation of the last command. the map
;;;                            contains
;;;                     :new-store  The state of the immutable store after
;;;                                 the action.
;;;                        :action  The alternate storage update action,
;;;                                 with the store missing.
;;;                          :text  The text shown to the user.
;;; When we process client commands, we add to session-state
;;; :selector-interpretation from the client's request.
(def session-states (atom {}))

(defn create-client-state
  [store referent-string]
  (let [immutable-store (current-store store)
        [referent subject-ref]
        (or (when referent-string
              (let [referent (string->referent referent-string)]
                (println "item referent" (simplify-for-print referent))
                (when referent
                  (let [[_ subject-ref]
                        (referent->exemplar-and-subject referent)
                        item (first (instantiate-to-items
                                     referent immutable-store))]
                    (when (and item (user-visible-item? item))
                      [referent subject-ref])))))
            (let [tab (first-tab-R immutable-store)]
              [(when tab (item-referent tab)) nil]))]
    (new-state-map {:referent referent
                    :subject-referent subject-ref
                    :last-action nil
                    :alternate nil})))

(defn create-tracker
  [store client-state selector-string]
  (let [selector-category (when selector-string
                            (string->referent selector-string))
        definition [DOM-for-client-R store client-state selector-category]
        tracker (new-dom-tracker manager-data)]
    (add-dom tracker "root" [] definition)
    (println "created tracker" (simplify-for-print definition))
    tracker))

(defn check-propagation-if-quiescent [tracker]
  (let [tracker-data @tracker
        task-queue (get-in tracker-data [:manager-data :queue])]
    (when (finished-all-tasks? task-queue)
      (let [reporters (->> (vals (:components tracker-data))
                           (map :reporter)
                           (filter reporter/reporter?))]
        ;; TODO: Eventually, this needs to be turned off, both
        ;; because it is too expensive, visiting all dependencies,
        ;; and because if a second request comes in while we are checking,
        ;; the checks are likely to fail.
        (check-propagation reporters)
        ;; This can be uncommented to see what is allocating reporters.
        (comment (profile-and-print-reporters reporters))))))

(defn new-id [session-map]
  (let [id (str (rand-int 1000000))]
    (if (contains? session-map id)
      (new-id session-map)
      id)))

;;; TODO: We should keep track of how old sessions are, and dump them when
;;;       they get too old.
(defn get-session-state [session-id]
  (@session-states session-id))

(defn create-session
  [name referent-string selector-string]
  (let [store (:mutable (ensure-store name))
        id (swap-control-return!
            session-states
            (fn [session-map]
              (let [id (new-id session-map)
                    client-state (create-client-state store referent-string)]
                [(assoc session-map id
                        {:name name
                         :store store
                         :tracker (create-tracker
                                   store client-state selector-string)
                         :client-state client-state})
                 id])))]
    (compute manager-data 1000)
    (println "computed some")
    (check-propagation-if-quiescent (:tracker (get-session-state id)))
    id))

(defn initial-page [name referent-string selector-string]
  (println "initial page" name referent-string selector-string)
  (let [session-id (create-session name referent-string selector-string)]
    (html5
     [:head
      [:title "Hello World"]
      [:meta {:itemprop "session-id"
              :content session-id}]
      (include-js "../js/main.js")
      (include-css "../style.css")]
     [:body
      [:div#toolbar.toolbar
       [:div#undo.tool
        [:img {:src "../icons/undo.gif"}]
        [:div.tooltip "undo"]]
       [:div#redo.tool
        [:img {:src "../icons/redo.gif"}]
        [:div.tooltip "redo"]]
       [:div.toolgap]
       [:div#expand.tool
        [:img {:src "../icons/expand.gif"}]
        [:div.tooltip "expand"]]
       [:div.toolgap]
       [:div#add-twin.tool
        [:img {:src "../icons/add_twin.gif"}]
        [:Div.tooltip "add twin"]]
       [:div#add-element.tool
        [:img {:src "../icons/add_element.gif"}]
        [:div.tooltip "add element"]]
       [:div#add-label.tool
        [:img {:src "../icons/add_label.gif"}]
        [:div.tooltip "add label"]]
       [:div#add-sibling.tool
        [:img {:src "../icons/add_sibling.gif"}]
        [:div.tooltip "add sibling below"]]
       [:div.toolgap]
       [:div#add-row.tool
        [:img {:src "../icons/add_row.gif"}]
        [:div.tooltip "add row below"]]
       [:div#add-column.tool
        [:img {:src "../icons/add_column.gif"}]
        [:div.tooltip "add column right"]]]
      [:div#app "Root"] ;; Client will create a component with id "root".
      [:div#select_holder.select_holder
       [:input#edit_input {"type" "text"}]
       [:div#scope_holder
        [:div#broad_selector_interpretation.tool
         [:img {:src "../icons/edit.gif"}]
         [:div.tooltip "editing data"]]
        [:div#narrow_selector_interpretation.tool
         [:img {:src "../icons/select.gif"}]
         [:div.tooltip "choosing selection"]]]]
      [:div#alternate_interpretation_holder {}
       [:div " "]
       [:div#alternate_interpretation " "]
       [:div " "]]
      [:script "cosheet.client.run();"]])))

(defn read-item-sequence
  "Return a lazy seq of the sequence of items from the reader."
  [reader]
  (when-let [item (clojure.edn/read {:eof nil} reader)]
    (lazy-seq (cons item (read-item-sequence reader)))))

(defn replay-request
  [session-state request]
  (let [{:keys [actions selector-interpretation]} request 
        action-sequence (confirm-actions actions (:client-state session-state))
        augmented-state (assoc session-state :selector-interpretation
                               selector-interpretation)]
    (do-actions (:store session-state) augmented-state action-sequence))
  (compute manager-data 4000)
  ;; Turn this on if there are questions about propagation, but it
  ;; makes things really slow.
  (when true
    (check-propagation-if-quiescent (:tracker session-state))))

(defn replay-item [session-state [type content]]
  (println "replaying item" type (when (not= type :store) content))
  (case type
    :store (let [store (:store session-state)]
             (let [new-store (data-to-store (current-store store) content)]
               (reset-store! store new-store))
             (compute manager-data 4000))
    :opened (state-map-reset! (:client-state session-state) :last-action 0)
    :initialize (state-map-reset! (:client-state session-state) :last-action 0)
    :request (replay-request session-state content)))

(defn do-replay [session-state replay]
  ;; First, make our own client state to run the replays in, so we get separate
  ;; numbering of actions.
  (let [{:keys [name store client-state]} session-state
        client-state (create-client-state
                      store (referent->string (:referent client-state)))
        session-state (assoc session-state :client-state client-state)]
    (when (clojure.string/ends-with? name ".history")
      (let [log-name (str (subs name 0 (- (count name) 8)) "_LOG_")]
        (try
          (with-open [stream (clojure.java.io/input-stream
                              (name-to-path log-name))]
            (with-open [reader (java.io.PushbackReader.
                                (java.io.InputStreamReader. stream))]
              ;; For some reason, the doall below is necessary. Otherwise,
              ;; the future doesn't run.
              (let [items (doall (read-item-sequence reader))]
                (println "starting replay.")
                (let [done (future (doseq [item items]
                                     (replay-item session-state item)))]
                  (future (println "done replaying." @done))))))
          (catch java.io.FileNotFoundException e
            nil))))))

;;; The parameters for the ajax request and response are:
;;; request:
;;;    :initialize If true, the server should assume the client is
;;;                starting from scratch. No other parameters
;;;                should be present.
;;;        :replay If true, the name should end in .history, and the
;;;                history of the file withough .history will be replayed.
;;;            :id The session id of this client session. The initial html
;;;                returned to the client will have the session id
;;;                in its session-id metadata field.
;;;       :actions A map id -> action to be performed, where each
;;;                action looks like [action_type arg ...], and the
;;;                action types are those implemented in actions.clj.
;;;                The action ids should sort in the order in which
;;;                the actions should be done. Ids in successive requests
;;;                should have successively higher sort orders.
;;;   :acknowledge A map component-id -> version of pairs for which
;;;                the dom of that version was received by the client.
;;; :selector-interpretation Either :broad or :narrow, giving the interpretation
;;;                          to use when there is an alternate interpretation.
;;; response:
;;;         :reload The server has no record of the session. The page
;;;                 should request a reload.
;;;           :doms A list of hiccup encoded doms of components. Their
;;;                 attributes will include a unique :id and a :version
;;;                 number that will increase for each change of the dom
;;;                 for that id. Inside the doms may be internal components
;;;                 encoded as [:component {<id and other attributes>}].
;;;         :select A list of an id to select, and a list of only ids
;;;                 that may currently be selected for the command to
;;;                 take effect.
;;;           :open A url that should be opened in a new window.
;;; :alternate-text Either nil or a vector of two or three strings
;;;                 to display in the  alternate interpretation field.
;;;                 The second string will be the link text.
;;;    :acknowledge A vector of action ids of actions that have been
;;;                 performed.

(defn ajax-response [request]
  (let [params (:params request)
        {:keys [id actions replay initialize selector-interpretation
                acknowledge]}
        params
        session-state (get-session-state id)]
    (if session-state
      (let [{:keys [tracker name store client-state]} session-state]
        (when (or actions initialize)
          (queue-to-log [:request (dissoc params :acknowledge)] name))
        (when (or actions initialize replay acknowledge)
          (println "request" params))
        (when initialize
          (println "requesting client refresh")
          (request-client-refresh tracker)
          (state-map-reset! client-state :last-action nil))
        (when replay
          (do-replay session-state replay))
        (process-acknowledgements tracker acknowledge)
        (let [action-sequence (confirm-actions actions client-state)]
          (when (and (not-any? #{[:alternate]} action-sequence)
                     (not (empty? action-sequence))
                     (empty? acknowledge))
            (println "resetting alternate" action-sequence acknowledge)
            (state-map-reset! client-state :alternate nil))
          (let [augmented-state (assoc session-state :selector-interpretation
                                       selector-interpretation)
                client-info (do-actions store augmented-state action-sequence)]
            (update-store-file name)
            (compute manager-data 4000)
            ;; Turn this on if there are questions about propagation, but it
            ;; makes things really slow.
            (when true
              (check-propagation-if-quiescent tracker))
            ;; Note: We must get the doms after doing the actions, so we can
            ;; immediately show the response to the actions. Likewise, we
            ;; have to pass down select requests after the new dom has been
            ;; constructed, so the client has the dom we want it to select.
            (let [doms (response-doms @tracker 100)
                  select (let [[select if-selected] (:select client-info)]
                           (when select
                             (let [select-key
                                   (first (filter
                                           #(dom-for-key? tracker %)
                                           [(conj select :content) select]))
                                   select-id (when select-key
                                               (key->id tracker select-key))]
                               [select-id
                                (filter identity
                                        (map (partial key->id tracker)
                                             if-selected))])))
                  answer (cond-> (select-keys client-info
                                              [:open :alternate-text])
                           (> (count doms) 0) (assoc :doms doms)
                           select (assoc :select select)
                           actions (assoc :acknowledge (vec (keys actions))))]
              (when (not= (dissoc answer :alternate-text) {})
                (println "response" answer))
              (response answer)))))
      (response {:reload true}))))

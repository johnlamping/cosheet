(ns cosheet.server.views
  (:import [java.nio.file Files CopyOption StandardCopyOption])
  (:require
   [hiccup.page :refer [html5 include-js include-css]]
   [ring.util.response :refer [response]]
   (cosheet
    [utils :refer [swap-control-return! ensure-in-atom-map!]]
    [orderable :as orderable]
    [store :refer [new-element-store new-mutable-store current-store
                   read-store write-store id-valid?]]
    [store-impl :refer [->ItemId]]
    mutable-store-impl
    [entity :refer [description->entity in-different-store]]
    entity-impl
    [store-utils :refer [add-entity]]
    [query :refer [matching-items matching-elements]]
    [debug :refer [simplify-for-print]]
    query-impl
    [expression-manager :refer [new-expression-manager-data compute]]
    [expression-manager-test :refer [check-propagation]]
    [task-queue :refer [finished-all-tasks?]]
    [reporters :as reporter]
    [debug :refer [profile-and-print-reporters]])
   (cosheet.server
    [referent :refer [item-referent referent->exemplar-and-subject
                      string->referent instantiate-referent]]
    [render :refer [top-level-item-DOM-R]]
    [dom-tracker :refer [new-dom-tracker add-dom request-client-refresh
                         process-acknowledgements response-doms
                         key->id]]
    [actions :refer [confirm-actions do-actions]])))

(defn starting-store
  []
  (let [unused-orderable orderable/initial
        [o1 unused-orderable] (orderable/split unused-orderable :after)
        [o2 unused-orderable] (orderable/split unused-orderable :after)
        [o3 unused-orderable] (orderable/split unused-orderable :after)
        [o4 unused-orderable] (orderable/split unused-orderable :after)
        starting-item `("Joe"
                        (:root :non-semantic)
                        (:top-level :non-semantic)
                        (~o1 :order :non-semantic)
                        ("person" :tag (~o1 :order :non-semantic))
                        ("male" (~o1 :order :non-semantic))
                        ("married" (~o2 :order :non-semantic))
                        (39 (~o3 :order :non-semantic)
                            ("age" :tag (~o1 :order :non-semantic))
                            ("doubtful"
                             ("confidence" (~o1 :order :non-semantic))
                             (~o1 :order :non-semantic)))
                        (45 (~o4 :order :non-semantic)
                            ("age" :tag (~o1 :order :non-semantic))))
        [store id] (add-entity (new-element-store) nil starting-item)
        starting-table `("table"
                         (:root :non-semantic)
                         (:table :non-semantic)
                         (~'anything ("person" :tag
                                      (~o1 :order :non-semantic))
                                     (:row-condition :non-semantic))
                         (~'anything ("age" :tag (~o1 :order :non-semantic))
                                    (~o1 :order :non-semantic)
                                    (:column :non-semantic))
                         (~'anything ("size" :tag (~o1 :order :non-semantic))
                                    (~o2 :order :non-semantic)
                                    (:column :non-semantic)))
        starting-element `(39 (:root :non-semantic)
                              ("age" :tag (~o1 :order :non-semantic))
                              (~o3 :order :non-semantic)
                              ("doubtful"
                               ("confidence" :tag (~o1 :order :non-semantic))
                               (~o1 :order :non-semantic))
                              ("more"
                               (~o2 :order :non-semantic)))
        [store id] (add-entity store nil starting-table)
        [store _] (add-entity store nil (list unused-orderable
                                              :unused-orderable))]
    store))

(defonce manager-data (new-expression-manager-data 0)) ;; TODO: Make it 1

;;; Store management. Stores may be shared across sessions.

;;; A map from name to stores that we have open.
(def stores (atom {}))

(defn name-to-path [name]
  (let [homedir (System/getProperty "user.home")]
    (clojure.string/join "" [homedir  "/cosheet/" name ".cst"])))

(defn path-to-Path [path]
  (java.nio.file.Paths/get
   (java.net.URI. (clojure.string/join "" ["file://" path]))))

(defn read-store-file [name]
  (with-open [stream (clojure.java.io/input-stream (name-to-path name))]
    (read-store (new-element-store) stream)))

(defn write-store-file [immutable-store name]
  (let [temp-path (name-to-path "_TEMP_")]
    (clojure.java.io/delete-file temp-path true)
    (with-open [stream (clojure.java.io/output-stream temp-path)]
      (write-store immutable-store stream))
    (Files/move (path-to-Path temp-path) (path-to-Path (name-to-path name))
                (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING,
                                        StandardCopyOption/ATOMIC_MOVE]))))

(defn ensure-store [name]
  (ensure-in-atom-map!
   stores name
   #(new-mutable-store
     (try (read-store-file %)
          (catch java.io.FileNotFoundException e (starting-store))))))

;;; Session management. There is a tracker for each session.

;;; A map from file name to session state.
;;; Session state consists of a map
;;;           :name  The file name to mirror the store.
;;;          :store  The store that holds the data.
;;;        :tracker  The tracker for the session.
;;;    :last-action  an atom holding the id of the last action we did.
;;;                  This keeps us from repeating an action if the
;;;                  client gets impatient and repeats it while we were
;;;                  working on it.
(def session-states (atom {}))

(defn create-tracker
  [store referent-string]
  (let [immutable-store (current-store store)
        [immutable-item referent subject]
        (or (when referent-string
              (let [referent (string->referent referent-string)]
                (println "item referent" (simplify-for-print referent))
                (when referent
                  (let [[_ subject] (referent->exemplar-and-subject referent)
                        item (first (apply concat
                                     (instantiate-referent referent
                                                           immutable-store)))]
                    ;; Check that the item has an :order element,
                    ;; which indicates that it is a user visible item.
                    ;; TODO: This will also need to check for selectors
                    ;; that the user should see, even though they don't have
                    ;; :order.
                    (println "item" (simplify-for-print item))
                    (when (and item
                               (not (empty?
                                     (matching-elements '(nil :order) item))))
                      [item referent subject])))))
            (let [item (first (matching-items '(nil :root) immutable-store))]
              [item (item-referent item) nil]))
        root-item (description->entity (:item-id immutable-item) store)
        definition [top-level-item-DOM-R
                    root-item referent (if subject {:subject subject} {})]
        tracker (new-dom-tracker manager-data)]
    (add-dom tracker "root" [] definition)
    (println "created tracker")
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
  [name referent-string]
  (let [store (ensure-store name)
        id (swap-control-return!
            session-states
            (fn [session-map]
              (let [id (new-id session-map)]
                [(assoc session-map id
                        {:name name
                         :store store
                         :tracker (create-tracker store referent-string)
                         :last-action (atom nil)})
                 id])))]
    (compute manager-data 1000)
    (println "computed some")
    (check-propagation-if-quiescent (:tracker (get-session-state id)))
    id))

(defn initial-page [name referent-string]
  (println "initial page" name referent-string)
  (let [session-id (create-session name referent-string)]
    (html5
     [:head
      [:title "Hello World"]
      [:meta {:itemprop "session-id"
              :content session-id}]
      (include-js "../js/main.js")
      (include-css "../style.css")]
     [:body
      [:div#toolbar.toolbar
       [:div#undo.tool "⤺" [:div.tooltip "undo"]]
       [:div#redo.tool "⤼" [:div.tooltip "redo"]]
       [:div.toolgap]
       [:div#add-twin.tool "+" [:div.tooltip "add twin"]]
       [:div#add-element.tool "↘" [:div.tooltip "add element"]] 
       [:div#add-sibling.tool "⍗" [:div.tooltip "add sibling below"]]
       [:div#add-row.tool "↧" [:div.tooltip "add row below"]]
       [:div#add-column.tool "↦" [:div.tooltip "add column right"]]]
      [:div#app "Root"]
      [:div#edit_holder [:textarea#edit_input {"rows" 1}]]
      [:script "cosheet.client.run();"]])))

;;; The parameters for the ajax request and response are:
;;; request:
;;;    :initialize If true, the server should assume the client is
;;;                starting from scratch. No other parameters
;;;                should be present.
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
;;; response:
;;;        :reload The server has no record of the session. The page
;;;                should request a reload.
;;;          :doms A list of hiccup encoded doms of components. Their
;;;                attributes will include a unique :id and a :version
;;;                number that will increase for each change of the dom
;;;                for that id. Inside the doms may be internal components
;;;                encoded as [:component {<id and other attributes>}].
;;;        :select A list of an id to select, and a list of only ids
;;;                that may currently be selected for the command to
;;;                take effect.
;;;          :open A url that should be opened in a new window.
;;;   :acknowledge A vector of action ids of actions that have been
;;;                performed.

(defn ajax-response [request]
  (let [params (:params request)
        {:keys [id actions acknowledge initialize]} params
        session-state (get-session-state id)]
    (if session-state
      (let [{:keys [tracker name store last-action]} session-state
            original_store (current-store store)]
        (println "request" params)
        (when initialize
          (println "requesting client refresh")
          (request-client-refresh tracker)
          (reset! last-action nil))
        (println "process acknowledgements" acknowledge)
        (process-acknowledgements tracker acknowledge)
        (let [action-sequence (confirm-actions actions last-action)
              client-info (when (not (empty? action-sequence))
                            (do-actions store session-state action-sequence))]
          (let [new-store (current-store store)]
            (when (not= new-store store)
              (write-store-file new-store name)))
          (compute manager-data 4000)
          (check-propagation-if-quiescent tracker)
          ;; Note: We must get the doms after doing the actions, so we can
          ;; immediately show the response to the actions. Likewise, we
          ;; have to pass down select requests after the new dom has been
          ;; constructed, so the client has the dom we want it to select.      
          (let [doms (response-doms @tracker 10)
                select (let [[select if-selected] (:select client-info)]
                         (when select
                           (let [select-id (key->id tracker select)]
                             [select-id
                              (filter identity
                                      (map (partial key->id tracker)
                                           if-selected))])))
                answer (cond-> (select-keys client-info [:open])
                         (> (count doms) 0) (assoc :doms doms)
                         select (assoc :select select)
                         actions (assoc :acknowledge (vec (keys actions))))]
            (println "response" answer)
            (response answer))))
      (response {:reload true}))))

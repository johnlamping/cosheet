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
    [query :refer [matching-items]]
    [debug :refer [simplify-for-print]]
    query-impl
    [expression-manager :refer [new-expression-manager-data compute]]
    [expression-manager-test :refer [check-propagation]]
    [task-queue :refer [finished-all-tasks?]]
    [dom-utils :refer [dom-attributes]]
    [reporters :as reporter]
    [debug :refer [profile-and-print-reporters]])
   (cosheet.server
    [referent :refer [item-referent referent->exemplar-and-subject
                      string->referent instantiate-referent]]
    [render :refer [top-level-item-DOM-R user-visible-item?]]
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
        [o5 unused-orderable] (orderable/split unused-orderable :after)
        [o6 unused-orderable] (orderable/split unused-orderable :after)
        [o7 unused-orderable] (orderable/split unused-orderable :after)
        [o8 unused-orderable] (orderable/split unused-orderable :after)
        [o9 unused-orderable] (orderable/split unused-orderable :after)
        [o10 unused-orderable] (orderable/split unused-orderable :after)
        [o11 unused-orderable] (orderable/split unused-orderable :after)
        starting-item `(""
                        (:top-level :non-semantic)
                        (~o1 :order :non-semantic)
                        ("restaurant" :tag (~o2 :order :non-semantic))
                        ("Chef Chu's" (~o3 :order :non-semantic)
                            ("name" :tag (~o4 :order :non-semantic)))
                        ("Los Altos" (~o5 :order :non-semantic)
                            ("location" :tag (~o6 :order :non-semantic))))
        [store id] (add-entity (new-element-store) nil starting-item)
        starting-table `("table"
                         (:root :non-semantic)
                         (:table :non-semantic)
                         (~'anything ("restaurant" :tag
                                      (~o7 :order :non-semantic))
                                     (:row-condition :non-semantic))
                         (~'anything ("name" :tag (~o8 :order :non-semantic))
                                    (~o9 :order :non-semantic)
                                    (:column :non-semantic))
                         (~'anything ("location"
                                      :tag (~o10 :order :non-semantic))
                                    (~o11 :order :non-semantic)
                                    (:column :non-semantic)))
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
;;;                  client gets impatient and repeats actions while we
;;;                  are working on them.
;;;     :alternate   An atom possibly holding a map indicating an alternate
;;;                  interpretation of the last command. the map contains
;;;                  :new-store  The state of the immutable store after
;;;                              the action.
;;;                     :action  The alternate storage update action,
;;;                              with the store missing.
;;;                       :text  The text shown to the user.
;;; When we process client commands, we add to session-state
;;; :selector-interpretation from the client's request.
(def session-states (atom {}))

(defn create-tracker
  [store referent-string selector-string]
  (let [immutable-store (current-store store)
        selector-category (when selector-string
                            (string->referent selector-string))
        [immutable-item referent subject]
        (or (when referent-string
              (let [referent (string->referent referent-string)]
                (println "item referent" (simplify-for-print referent))
                (when referent
                  (let [[_ subject] (referent->exemplar-and-subject referent)
                        item (first (apply concat
                                     (instantiate-referent referent
                                                           immutable-store)))]
                    (println "item" (simplify-for-print item))
                    (when (and item (user-visible-item? item))
                      [item referent subject])))))
            (let [item (first (matching-items '(nil :root) immutable-store))]
              [item (item-referent item) nil]))
        root-item (description->entity (:item-id immutable-item) store)
        definition [top-level-item-DOM-R
                    root-item referent
                    (cond-> {}
                      subject (assoc :subject subject)
                      selector-category (assoc
                                         :selector-category selector-category
                                         :alternate-target true))]
        tracker (new-dom-tracker manager-data)]
    (add-dom tracker "root" (or (and (sequential? definition)
                                     (:key (dom-attributes definition)))
                                [])
             definition)
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
  (let [store (ensure-store name)
        id (swap-control-return!
            session-states
            (fn [session-map]
              (let [id (new-id session-map)]
                [(assoc session-map id
                        {:name name
                         :store store
                         :tracker (create-tracker
                                   store referent-string selector-string)
                         :last-action (atom nil)
                         :alternate (atom nil)})
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
       [:div#add-element.tool
        [:img {:src "../icons/add_element.gif"}]
        [:div.tooltip "add element"]]
      [:div#add-twin.tool
        [:img {:src "../icons/add_twin.gif"}]
        [:Div.tooltip "add twin"]]
       [:div#add-sibling.tool
        [:img {:src "../icons/add_sibling.gif"}]
        [:div.tooltip "add sibling below"]]
       [:div#add-row.tool
        [:img {:src "../icons/add_row.gif"}]
        [:div.tooltip "add row below"]]
       [:div#add-column.tool
        [:img {:src "../icons/add_column.gif"}]
        [:div.tooltip "add column right"]]
       [:div.toolgap] [:div.toolgap]
       [:div.selector-container
        [:div#select-broad.selection
         [:div.radio-button
          [:div.inner-radio-button]]
         [:img.selector-img {:src "../icons/select_broad.gif"}]
         [:div.tooltip "act on header descriptions"]]
        [:div#select-narrow.selection.picked
         [:div.radio-button
          [:div.inner-radio-button]]
         [:img.selector-img {:src "../icons/select_narrow.gif"}]
         [:div.tooltip "act on header selections"]]]]
      [:div#app "Root"]
      [:div#edit_holder [:textarea#edit_input {"rows" 1}]]
      [:div#alternate_interpretation_holder {}
       [:div " "]
       [:div#alternate_interpretation " "]
       [:div " "]]
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
        {:keys [id actions acknowledge initialize :selector-interpretation]}
        params
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
        (let [action-sequence (confirm-actions actions last-action)]
          (when (and (not-any? #{[:alternate]} action-sequence)
                     (not (empty? action-sequence))
                     (empty? acknowledge))
            (println "resetting alternate" action-sequence acknowledge)
            (reset! (:alternate session-state) nil))
          (let [augmented-state (assoc session-state :selector-interpretation
                                      selector-interpretation)
                client-info (do-actions store augmented-state action-sequence)]
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
                             (let [select-id
                                   ;; If there is a content item, select that.
                                   (or (key->id tracker (conj select :content))
                                       (key->id tracker select))]
                               [select-id
                                (filter identity
                                        (map (partial key->id tracker)
                                             if-selected))])))
                  answer (cond-> (select-keys client-info [:open
                                                           :alternate-text])
                           (> (count doms) 0) (assoc :doms doms)
                           select (assoc :select select)
                           actions (assoc :acknowledge (vec (keys actions))))]
              (println "response" answer)
              (response answer)))))
      (response {:reload true}))))

(ns cosheet.server.views
  (:import [java.nio.file Files CopyOption StandardCopyOption])
  (:require
   [hiccup.page :refer [html5 include-js include-css]]
   [ring.util.response :refer [response]]
   (cosheet
    [utils :refer [swap-control-return! ensure-in-atom-map!]]
    [orderable :as orderable]
    [store :refer [new-element-store new-mutable-store current-store
                   read-store write-store id-valid? call-dependent-on-id]]
    [store-impl :refer [->ItemId]]
    mutable-store-impl
    [entity :refer [description->entity in-different-store
                    content label->elements]]
    entity-impl
    [store-utils :refer [add-entity]]
    [query :refer [matching-items matching-elements]]
    [debug :refer [simplify-for-print]]
    query-impl
    [expression :refer [expr-let]]
    [expression-manager :refer [new-expression-manager-data compute]]
    [expression-manager-test :refer [check-propagation]]
    [task-queue :refer [finished-all-tasks?]]
    [dom-utils :refer [dom-attributes add-attributes]]
    [reporters :as reporter]
    [mutable-manager :refer [current-mutable-value]]
    [state-map :refer [new-state-map state-map-get-current-value
                       state-map-get state-map-reset!]]
    [debug :refer [profile-and-print-reporters]])
   (cosheet.server
    [referent :refer [item-referent union-referent
                      referent->exemplar-and-subject
                      string->referent instantiate-to-items]]
    [render-utils :refer [make-component]]
    [render :refer [top-level-item-DOM-R user-visible-item? starting-inherited]]
    [item-render :refer [item-content-DOM]]
    [table-render :refer [table-DOM-R]]
    [dom-tracker :refer [new-dom-tracker add-dom request-client-refresh
                         process-acknowledgements response-doms
                         key->id dom-for-key?]]
    [actions :refer [update-add-blank-table-view confirm-actions do-actions]])))

(defn starting-store
  []
  (let [[store _] (add-entity (new-element-store) nil
                              (list orderable/initial :unused-orderable))
        [store _] (add-entity store nil '("tabs" (:tabs :non-semantic)))
        [store _] (update-add-blank-table-view store "table")]
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
                    (println "item" (simplify-for-print item))
                    (when (and item (user-visible-item? item))
                      [referent subject-ref])))))
            (let [tabs-holder (first (matching-items '(nil :tabs)
                                                     immutable-store))
                  tab (first (label->elements tabs-holder :tab))
                  item (first (label->elements tab :tab-referent))]
              [(item-referent item) nil]))]
    ;; TODO: Put :referent and :subject-referent under :root
    (new-state-map {:referent referent
                    :subject-referent subject-ref
                    :last-action nil
                    :alternate nil})))

(defn DOM-for-client-R
  "Return a reporter giving the DOM specified by the client."
  [store client-state selector-category]
  (expr-let [referent (state-map-get client-state :referent)
             subject-referent (state-map-get client-state :subject-referent)
             immutable-item (call-dependent-on-id
                             store nil
                             (fn [immutable-store]
                               (first (instantiate-to-items
                                       referent immutable-store))))]
    (if immutable-item
      (let [item (description->entity (:item-id immutable-item) store)
            inherited (cond-> starting-inherited
                        subject-referent (assoc
                                          :subject-referent subject-referent)
                        selector-category (assoc
                                           :selector-category selector-category
                                           :alternate-target true))]
        (expr-let [table (matching-elements :table item)
                   content (content item)]
          (if (empty? table)
            (top-level-item-DOM-R item referent inherited)
            ;; Under the narrow interpretation of commands, we don't want to
            ;; affect the item, only the selection of which item,
            ;; so make the item referent have an empty first group.
            (let [regrouped-referent (union-referent [(union-referent [])
                                                      referent])]
              [:div {:class "tab-holder selector-scope"}
               ;; We need a div around the tab text, so it can have
               ;; a drop shadow that won't interfere with a selection inset
               ;; drop shadow.
               [:div {:class "tab-text-holder selectors"}
                (add-attributes
                 (item-content-DOM regrouped-referent content inherited)
                 {:class "tab"
                  :key [:tab]
                  :target {:special :tab
                           :alternate true}
                  :selector-category :tab})]
               (make-component
                {:key [:tab (:item-id item)] :class "table selecteds"}
                [table-DOM-R item
                 (assoc inherited :key-prefix [:tab])])]))))
      [:div])))

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
  (let [store (ensure-store name)
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
        [:div.tooltip "add column right"]]]
      [:div#app.selector-scope "Root"]
      [:div#select_holder.select_holder
       [:textarea#edit_input {"rows" 1}]
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
        {:keys [id actions acknowledge initialize selector-interpretation]}
        params
        session-state (get-session-state id)]
    (if session-state
      (let [{:keys [tracker name store client-state]} session-state
            original_store (current-store store)]
        (println "request" params)
        (when initialize
          (println "requesting client refresh")
          (request-client-refresh tracker)
          (state-map-reset! client-state :last-action nil))
        (println "process acknowledgements" acknowledge)
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
                  answer (cond-> (select-keys client-info [:open
                                                           :alternate-text])
                           (> (count doms) 0) (assoc :doms doms)
                           select (assoc :select select)
                           actions (assoc :acknowledge (vec (keys actions))))]
              (println "response" answer)
              (response answer)))))
      (response {:reload true}))))

(ns cosheet.server.views
  (:require
   [hiccup.page :refer [html5 include-js include-css]]
   [ring.util.response :refer [response]]
   (cosheet
    [store :refer [current-store data-to-store reset-store! id-valid?]]
    [store-impl :refer [->ItemId]]
    mutable-store-impl
    [entity :refer [description->entity in-different-store
                    content label->elements]]
    entity-impl
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
    [state-map :refer [state-map-reset!]]
    [debug :refer [profile-and-print-reporters]])
   (cosheet.server
    [referent :refer [union-referent referent->string]]
    [render-utils :refer [make-component]]
    [order-utils :refer [order-items-R]]
    [item-render :refer [item-content-DOM]]
    [dom-tracker :refer [request-client-refresh
                         process-acknowledgements response-doms
                         key->id dom-for-key?]]
    [session-state :refer [create-session create-client-state name-to-path
                           get-session-state queue-to-log update-store-file]]
    [actions :refer [confirm-actions do-actions]])))

(defonce manager-data (new-expression-manager-data 0)) ;; TODO: Make it 1

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

(defn initial-page [name referent-string selector-string]
  (println "initial page" name referent-string selector-string)
  (let [session-id (create-session
                    name referent-string manager-data selector-string)]
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
    :request (replay-request session-state content))
  (Thread/sleep 100))

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

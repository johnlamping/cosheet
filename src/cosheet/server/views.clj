(ns cosheet.server.views
  (:require
   [hiccup.page :refer [html5 include-js include-css]]
   [ring.util.response :refer [response]]
   (cosheet
    [orderable :as orderable]
    [mutable-set :refer [new-mutable-set]]
    [store :refer [new-element-store new-mutable-store current-store]]
    store-impl
    mutable-store-impl
    [entity :refer [description->entity]]
    entity-impl
    [store-utils :refer [add-entity]]
    [query :refer [matching-items]]
    query-impl
    [expression-manager :refer [new-expression-manager-data compute]]
    [expression-manager-test :refer [check-propagation]]
    [task-queue :refer [finished-all-tasks?]]
    [reporters :as reporter]
    [debug :refer [profile-and-print-reporters]])
   (cosheet.server
    [key :refer [item-referent prepend-to-key]]
    ;; TODO: Make item-DOM recognize tables, so we can just call it.
    [render :refer [item-DOM table-DOM]]
    [dom-tracker :refer [new-dom-tracker add-dom request-client-refresh
                         process-acknowledgements response-doms
                         key->id]]
    [actions :refer [confirm-actions do-actions]])))

(defn initial-page [name]
  (html5
    [:head
     [:title "Hello World"]
     (include-js "../js/main.js")
     (include-css "../style.css")]
    [:body
     [:div#toolbar.toolbar
      [:div#undo.tool "⤺" [:div.tooltip "undo"]]
      [:div#redo.tool "⤼" [:div.tooltip "redo"]]
      [:div#add-sibling.tool "+" [:div.tooltip "add item"]]
      [:div#add-row.tool "↧" [:div.tooltip "add row below"]]
      [:div#add-column.tool "↦" [:div.tooltip "add column right"]]] 
     [:div#app "Root"]
     [:div#edit_holder [:textarea#edit_input {"rows" 1}]]
     [:script "cosheet.client.run();"]]))

(defn create-store
  []
  (let [unused-orderable orderable/initial
        [o1 unused-orderable] (orderable/split unused-orderable :after)
        [o2 unused-orderable] (orderable/split unused-orderable :after)
        [o3 unused-orderable] (orderable/split unused-orderable :after)
        [o4 unused-orderable] (orderable/split unused-orderable :after)
        starting-item `("Joe"
                        ; (:root :non-semantic)
                        (:top-level :non-semantic)
                        (~o1 :order :non-semantic)
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
                         (:none :row-query)
                         (:none ("age" :tag)
                                (~o1 :order :non-semantic)
                                (:column :non-semantic))
                         (:none ("size" :tag)
                                (~o2 :order :non-semantic)
                                (:column :non-semantic)))
        [store id] (add-entity store nil starting-table)
        [store _] (add-entity store nil (list unused-orderable
                                              :unused-orderable))]
    (new-mutable-store store)))

(defonce store (create-store))

(defonce root-item
  (let [immutable-root-item (first (matching-items '(nil :root)
                                                   (current-store store)))]
    (description->entity (:item-id immutable-root-item) store)))

(defonce root-parent-key ["root"])

(defonce root-key (prepend-to-key (item-referent root-item) root-parent-key))

(defonce manager-data (new-expression-manager-data 0)) ;; TODO: Make it 1

(defn create-tracker
  [do-not-merge]
  (let [definition [table-DOM root-item root-parent-key
                    {:depth 0 :do-not-merge do-not-merge}]
        tracker (new-dom-tracker manager-data)]
    (add-dom tracker "root" root-key definition)
    tracker))

;;; TODO: this needs to be separate for each web page.
;;; Session state consists of a map
;;;        :tracker  The tracker for the session.
;;;   :do-not-merge  A set of items that should not be merged.
;;;    :last-action  The key of the last action we did.
;;;                  This keeps us from repeating an action if the
;;;                  client gets impatient and repeats it while we were
;;;                  working on it.
(def session-state-atom (atom nil))

(defn tracker [] (:tracker @session-state-atom))

(defn check-propagation-if-quiescent []
  (let [tracker-data @(tracker)
        reporter (get-in tracker-data [:components root-key :reporter])
        task-queue (get-in tracker-data [:manager-data :queue])]
    (when (finished-all-tasks? task-queue)
      ;; TODO: Eventually, this needs to be turned off, both
      ;; because it is too expensive, visiting all dependencies,
      ;; and because if a second request comes in while we are checking
      ;; the checks are likely to fail.
      (check-propagation reporter)
      ;; This can be uncommented to see what is allocating reporters.
      (comment
        (profile-and-print-reporters (->> (vals (:components @(tracker)))
                                          (map :reporter)
                                          (filter reporter/reporter?)))))))

(defn initialize-session-state-atom
  []
  (reset! session-state-atom (let [do-not-merge (new-mutable-set #{})]
                               {:tracker (create-tracker do-not-merge)
                                :do-not-merge do-not-merge
                                :last-action nil}))
  (println "created tracker")
  (compute manager-data 1000)
  (println "computed some")
  (check-propagation-if-quiescent))

;;; The parameters for the ajax request and response are:
;;; request:
;;;    :initialize If true, the server should assume the client is
;;;                starting from scratch. No other parameters
;;;                should be present.
;;;       :actions A map id -> action to be performed, where each
;;;                action looks like [action_type arg ...], and the
;;;                action types are those implemented in actions.clj.
;;;                The action ids should sort in the order in which
;;;                the actions should be done. Ids in successive
;;;                should have successively higher sort orders.
;;;   :acknowledge A map component-id -> version of pairs for which
;;;                the dom of that version was received by the client.
;;; response:
;;;          :doms A list of hiccup encoded doms of components. Their
;;;                attributes will include a unique :id and a :version
;;;                number that will increase for each change of the dom
;;;                for that id. Inside the doms may be internal components
;;;                encoded as [:component {<id and other attributes>}].
;;;        :select A list of an id to select, and a list of only ids
;;;                that may currently be selected for the command to
;;;                take efffect.
;;;   :acknowledge A vector of action ids of actions that have been
;;;                performed.
;;; TODO: Action ids should be required to be integers, and the server
;;;       should not perform out of order actions. It should
;;;       acknowledge just the highest action it has seen.

(defn ajax-response [request]
  (let [params (:params request)
        {:keys [actions acknowledge initialize]} params]
    (println "request params" params)
    (if (nil? @session-state-atom)
      (initialize-session-state-atom)
      (when initialize
        (println "requesting client refresh")
        (request-client-refresh (tracker))
        (swap! session-state-atom #(assoc % :last-action nil))))
    (println "process acknowledgements" acknowledge)
    (process-acknowledgements (tracker) acknowledge)
    (let [action-sequence (confirm-actions actions session-state-atom)
          client-info (when (not (empty? action-sequence))
                        (do-actions store @session-state-atom action-sequence))]
      (compute manager-data 4000)
      (check-propagation-if-quiescent)
      ;; Note: We must get the doms after doing the actions, so we can
      ;; immediately show the response to the actions. Likewise, we
      ;; have to pass down select requests after the new dom has been
      ;; constructed, so the client has the dom we want it to select.      
      (let [doms (response-doms @(tracker) 10)
            select (let [[select if-selected] (:select client-info)]
                     (when select
                       (let [select-id (key->id (tracker) select)]
                         [select-id
                          (filter identity
                                  (map (partial key->id (tracker))
                                       if-selected))])))
            answer (cond-> {}
                     (> (count doms) 0) (assoc :doms doms)
                     select (assoc :select select)
                     actions (assoc :acknowledge (vec (keys actions))))]
        (println "response" answer)
        (response answer)))))

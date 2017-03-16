(ns cosheet.server.session-state
  (:import [java.nio.file Files CopyOption StandardCopyOption])
  (:require
   (cosheet
    [utils :refer [swap-control-return! ensure-in-atom-map! with-latest-value]]
    [orderable :as orderable]
    [store :refer [new-element-store new-mutable-store current-store
                   read-store write-store store-to-data  data-to-store]]
    mutable-store-impl
    [store-utils :refer [add-entity]]
    [debug :refer [simplify-for-print]]
    [expression-manager :refer [compute]]
    [state-map :refer [new-state-map]])
   (cosheet.server
    [referent :refer [item-referent referent->exemplar-and-subject
                      string->referent referent->string instantiate-to-items]]
    [render :refer [DOM-for-client-R user-visible-item?]]
    [tabs-render :refer [first-tab-R]]
    [dom-tracker :refer [new-dom-tracker add-dom]]
    [actions :refer [update-add-blank-table-view]])))

(defn starting-store
  []
  (let [[store _] (add-entity (new-element-store) nil
                              (list orderable/initial :unused-orderable))
        [store _] (add-entity store nil '("tabs" (:tabs :non-semantic)))
        [store _] (update-add-blank-table-view store "tab")]
    store))

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
  [store client-state manager-data selector-string]
  (let [selector-category (when selector-string
                            (string->referent selector-string))
        definition [DOM-for-client-R store client-state selector-category]
        tracker (new-dom-tracker manager-data)]
    (add-dom tracker "root" [] definition)
    (println "created tracker" (simplify-for-print definition))
    tracker))

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
  [name referent-string manager-data selector-string]
  (let [store (:mutable (ensure-store name))
        id (swap-control-return!
            session-states
            (fn [session-map]
              (let [id (new-id session-map)
                    client-state (create-client-state store referent-string)]
                [(assoc session-map id
                        {:name name
                         :store store
                         :tracker (create-tracker store client-state
                                                  manager-data selector-string)
                         :client-state client-state})
                 id])))]
    (compute manager-data 1000)
    (println "computed some")
    id))


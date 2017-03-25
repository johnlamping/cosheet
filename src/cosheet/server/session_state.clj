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
    [query :refer [matching-items]]
    [debug :refer [simplify-for-print]]
    [expression-manager :refer [compute]]
    [state-map :refer [new-state-map]])
   (cosheet.server
    [order-utils :refer [update-add-entity-adjacent-to  order-element-for-item]]
    [referent :refer [item-referent referent->exemplar-and-subject
                      string->referent referent->string
                      instantiate-to-items specialize-template]]
    [render :refer [DOM-for-client-R user-visible-item?]]
    [tabs-render :refer [first-tab-R new-tab-elements]]
    [dom-tracker :refer [new-dom-tracker add-dom]])))

(defn update-add-blank-table-view
  "Add a blank table view with the given url path to the store, returning the
  new store and the id of the new view."
  ;; TODO: Get just the final name from the path.
  [store url-path]
  (let [name (last (clojure.string/split url-path #"/"))
        generic (cons "" (cons name new-tab-elements))
        [specialized [store _]] (specialize-template generic [store {}])
        tabs-holder (first (matching-items '(nil :tabs) store))]
    (update-add-entity-adjacent-to
     store (:item-id tabs-holder) specialized (order-element-for-item nil store)
     :after false)))

(defn starting-store
  []
  (let [[store _] (add-entity (new-element-store) nil
                              (list orderable/initial :unused-orderable))
        [store _] (add-entity store nil '("tabs" (:tabs :non-semantic)))
        [store _] (update-add-blank-table-view store "tab")]
    store))

;;; Store management. Stores may be shared across sessions.

;;; A map from url path to a map
;;;  {:mutable <mutable-store>,
;;;   :agent <the agent responsible for saving the store, and writing log
;;;           entries. It's state is the
;;;           last version of the store written. We write the store by doing
;;;           a send to this agent, so that writes don't block interaction,
;;;           and will skip intermediate values if they get behind.>
;;;   :log-agent <the agent responsible for writing log entries.
;;;               its state is a writer opened to append to the log file.>
(def store-info (atom {}))

(defn url-path-to-file-path
  [url-path]
  (let [path (cond (clojure.string/starts-with? url-path "/cosheet/")
                   (str (System/getProperty "user.home") url-path)
                   (clojure.string/starts-with? url-path "//")
                   (subs url-path 1)
                   true nil)]
    (when path (str path ".cst"))))

(defn path-to-Path [path]
  (java.nio.file.Paths/get
   (java.net.URI. (clojure.string/join "" ["file://" path]))))

(defn read-store-file [url-path]
  (with-open [stream (clojure.java.io/input-stream
                      (url-path-to-file-path url-path))]
    (read-store (new-element-store) stream)))

(defn write-store-file-if-different
  "Function for running in the :agent of store-info.
   If the current immutable store is different from the written store,
   writes the current value to a file of the given url path. Always returns
   the current-value."
   [written-store mutable-store url-path]
  ;; We write the latest value from the mutable store, rather than the value
  ;; at the time the send was done, so that we will catch up if we get behind.
  (with-latest-value [store (current-store mutable-store)]
    (when (not= written-store store)
      (let [temp-path (url-path-to-file-path (str url-path "_TEMP_"))]
        (clojure.java.io/delete-file temp-path true)
        (with-open [stream (clojure.java.io/output-stream temp-path)]
          (write-store store stream))
        (Files/move (path-to-Path temp-path)
                    (path-to-Path (url-path-to-file-path url-path))
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

(defn update-store-file [url-path]
  (when-let [info (@store-info url-path)]
    (send (:agent info) write-store-file-if-different (:mutable info) url-path)))

(defn queue-to-log
  "Add the entry to the queue to be written to the log."
  [entry url-path]
  (when-let [info (@store-info url-path)]
    (when-let [agent (:log-agent info)]
      (send agent write-log-entry entry))))

(defn ensure-store [url-path]
  ;; If there were a race to create the store, the log stream might get
  ;; opened multiple times in ensure-in-atom-map! To avoid that, we run under
  ;; a global lock.
  (locking store-info
    (ensure-in-atom-map!
     store-info url-path
     #(let [immutable (try (read-store-file %)
                           (catch java.io.FileNotFoundException e
                             (starting-store)))
            log-stream (try (java.io.FileOutputStream.
                             (url-path-to-file-path (str url-path "_LOG_")) true)
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

;;; A map from url path to session state.
;;; Session state consists of a map
;;;       :url-path  The url path corresponding to the store.
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
  [url-path referent-string manager-data selector-string]
  (let [store (:mutable (ensure-store url-path))
        id (swap-control-return!
            session-states
            (fn [session-map]
              (let [id (new-id session-map)
                    client-state (create-client-state store referent-string)]
                [(assoc session-map id
                        {:url-path url-path
                         :store store
                         :tracker (create-tracker store client-state
                                                  manager-data selector-string)
                         :client-state client-state})
                 id])))]
    (compute manager-data 1000)
    (println "computed some")
    id))


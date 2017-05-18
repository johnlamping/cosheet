(ns cosheet.server.session-state
  (:import [java.nio.file Files CopyOption StandardCopyOption])
  (:require
   [clojure-csv.core :refer [parse-csv]]
   (cosheet
    [utils :refer [swap-control-return! ensure-in-atom-map! with-latest-value
                   parse-string-as-number]]
    [orderable :as orderable]
    [store :refer [new-element-store new-mutable-store current-store
                   read-store write-store store-to-data data-to-store]]
    mutable-store-impl
    [store-utils :refer [add-entity]]
    [query :refer [matching-items]]
    [debug :refer [simplify-for-print]]
    [expression-manager :refer [compute]]
    [state-map :refer [new-state-map]])
   (cosheet.server
    [order-utils :refer [update-add-entity-adjacent-to  order-element-for-item]]
    [model-utils :refer [starting-store add-table first-tab-R new-tab-elements
                         specialize-template]]
    [referent :refer [item-referent referent->exemplar-and-subject
                      string->referent referent->string
                      instantiate-to-items]]
    [render :refer [DOM-for-client-R user-visible-item?]]
    [dom-tracker :refer [new-dom-tracker add-dom remove-all-doms]])))

;;; TODO: This needs a unit test.

(defn update-add-blank-table-view
  "Add a blank table view with the given url path to the store, returning the
  new store and the id of the new view."
  [store url-path]
  (let [name (last (clojure.string/split url-path #"/"))
        generic (cons "" (cons name new-tab-elements))
        [specialized [store _]] (specialize-template generic [store {}])
        tabs-holder (first (matching-items '(nil :tabs) store))]
    (update-add-entity-adjacent-to
     store (:item-id tabs-holder) specialized (order-element-for-item nil store)
     :after false)))

(defn path-to-Path [path]
  (java.nio.file.Paths/get
   (java.net.URI. (clojure.string/join "" ["file://" path]))))

(defn url-path-to-file-path
  "Turn a url path into a file path, returning nil if the url path is
  syntactically ill formed."
  [url-path suffix]
  (when (not (clojure.string/ends-with? url-path "/"))
    (when-let [path (cond (clojure.string/starts-with? url-path "/cosheet/")
                          (str (System/getProperty "user.home") url-path)
                          (clojure.string/starts-with? url-path "/~/")
                          (str (System/getProperty "user.home")
                               (subs url-path 2))
                          (clojure.string/starts-with? url-path "//")
                          (subs url-path 1)
                          true nil)]
      (str path suffix))))

(defn is-valid-path?
  "Return whether the file path refers to a name in an actual directory."
  [path]
  (and
   path
   (let [directory (clojure.string/join
                    "/" (butlast (clojure.string/split path #"/")))]
     (java.nio.file.Files/isDirectory
      (path-to-Path directory)
      (into-array java.nio.file.LinkOption [])))))

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

(defn read-csv
  "Read a csv file, turning it into a store."
  [path name]
  (try
    (with-open [reader (clojure.java.io/reader path)]
      (let [csved (parse-csv reader)
            parsed-rows (map (fn [csv] (map #(-> %
                                                 clojure.string/trim
                                                 parse-string-as-number)
                                            csv))
                             csved)]
        (add-table (starting-store nil) name parsed-rows)))
    (catch java.io.FileNotFoundException e nil)))

(defn read-store-file [url-path]
  (or
   (try
     (with-open [stream (clojure.java.io/input-stream
                         (url-path-to-file-path url-path ".cosheet"))]
       (read-store (new-element-store) stream))
     (catch java.io.FileNotFoundException e nil))
   (let [store
         (read-csv (url-path-to-file-path url-path ".csv")
                   (last (clojure.string/split url-path #"/")))]
     store)))

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
      (let [temp-path (url-path-to-file-path url-path "_TEMP_.cosheet")]
        (clojure.java.io/delete-file temp-path true)
        (with-open [stream (clojure.java.io/output-stream temp-path)]
          (write-store store stream))
        (Files/move (path-to-Path temp-path)
                    (path-to-Path (url-path-to-file-path url-path ".cosheet"))
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
    (send (:agent info)
          write-store-file-if-different (:mutable info) url-path)))

(defn queue-to-log
  "Add the entry to the queue to be written to the log."
  [entry url-path]
  (when-let [info (@store-info url-path)]
    (when-let [agent (:log-agent info)]
      (send agent write-log-entry entry))))

(defn ensure-store
  "Return the store-info for the given url path, creating it if necessary,
  returning nil if there is something wrong with the path."
  [url-path]
  ;; If there were a race to create the store, the log stream might get
  ;; opened multiple times in ensure-in-atom-map! To avoid that, we run under
  ;; a global lock.
  (locking store-info
    (or
     (@store-info url-path)
     ;; If the path is not valid, we don't want to put nil in the store-info,
     ;; as that would preclude fixing the path. Rather, we leave store-info
     ;; blank in that case.
     (when-let
         [info
          (when (is-valid-path? (url-path-to-file-path url-path ".cosheet"))
            (let [immutable (or (read-store-file url-path)
                                (let [name (last (clojure.string/split
                                                  url-path #"/"))]
                                  (starting-store name)))
                  log-stream (try (java.io.FileOutputStream.
                                   (url-path-to-file-path
                                    url-path ".cosheetlog")
                                   true)
                                  (catch java.io.FileNotFoundException e
                                    nil))
                  log-agent (when log-stream
                              (agent (clojure.java.io/writer log-stream)))]
              (when log-agent
                (when (= (.position (.getChannel log-stream)) 0)
                  (send log-agent
                        write-log-entry [:store (store-to-data immutable)]))
                (send log-agent write-log-entry [:opened])
                {:mutable (new-mutable-store immutable)
                 :agent (agent immutable)
                 :log-agent log-agent})))]
       (swap! store-info #(assoc % url-path info))
       info))))

;;; Session management. There is a tracker for each session.

;;; A map from id to session state.
;;; Session state consists of a map
;;;       :url-path  The url path corresponding to the store.
;;;          :store  The store that holds the data.
;;;        :tracker  The tracker for the session.
;;;   :client-state  A state-map holding these keys:
;;;                  :in-sync  True if the client is ready to accept doms.
;;;                 :referent  The referent for the root of the display or
;;;                            selected tab.
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
                    :alternate nil
                    :in-sync false})))

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
  (let [id (str (rand-int 1000000000))]
    (if (contains? session-map id)
      (new-id session-map)
      id)))

;;; TODO: We should keep track of how old sessions are, and dump them when
;;;       they get too old.
(defn get-session-state [session-id]
  (@session-states session-id))

(defn create-session
  "Create a session with the given id, or with a new id if none is given."
  [session-id url-path referent-string manager-data selector-string]
  (when-let [store (:mutable (ensure-store url-path))]
    (let [id (swap-control-return!
              session-states
              (fn [session-map]
                (let [id (or session-id (new-id session-map))
                      client-state (create-client-state store referent-string)]
                  [(assoc session-map id
                          {:url-path url-path
                           :store store
                           :tracker (create-tracker
                                     store client-state
                                     manager-data selector-string)
                           :client-state client-state})
                   id])))]
      (compute manager-data 1000)
      (println "computed some")
      id)))

(defn ensure-session
  "Make sure there is a session with the given id, and return its state."
  [session-id url-path referent-string manager-data selector-string]
  (or (get-session-state session-id)
      (do (create-session
           session-id url-path referent-string manager-data selector-string)
          (get-session-state session-id))))

(defn forget-session
  "The session is no longer needed. Forget about it."
  [session-id]
  (swap! session-states
         (fn [session-map]
           (if-let [state (session-map session-id)]
             (do (remove-all-doms (:tracker state))
                 (Thread/sleep 100)
                 (dissoc session-map session-id))
             session-map))))


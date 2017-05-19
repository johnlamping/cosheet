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
    [state-map :refer [new-state-map
                       state-map-get-current-value state-map-reset!]])
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

;;; The map holding all session state. Sessions are keyed by an id assigned
;;; to the client when it first loads. Stores may be shared across sessions,
;;; so there is a separate map holding them, keyed by the path to the store
;;; file.
;;; The contents of each map are further detailed below.
(def session-info (atom {:sessions {}
                         :stores {}}))

;;; Store management

;;; (:stores @session-info) is a map from url path to a map:
;;;  {   :store The mutable-store
;;;      :agent The agent responsible for saving the store, and writing log
;;;             entries. It's state is the
;;;             last version of the store written. We write the store by doing
;;;             a send to this agent, so that writes don't block interaction,
;;;             and will skip intermediate values if they get behind.
;;;  :log-agent The agent responsible for writing log entries.
;;;             its state is a writer opened to append to the log file.

(defn read-csv-reader
  "parse a csv stream, turning it into a store."
  [reader name]
  (let [csved (parse-csv reader)
        parsed-rows (map (fn [csv] (map #(-> %
                                             clojure.string/trim
                                             parse-string-as-number)
                                        csv))
                         csved)]
    (add-table (starting-store nil) name parsed-rows)))

(defn read-csv
  "Read a csv file, turning it into a store."
  [path name]
  (try
    (with-open [reader (clojure.java.io/reader path)]
      (read-csv-reader reader name))
    (catch java.io.FileNotFoundException e nil)))

(defn read-store-file [url-path]
  (or
   (try
     (with-open [stream (clojure.java.io/input-stream
                         (url-path-to-file-path url-path ".cosheet"))]
       (read-store (new-element-store) stream))
     (catch java.io.FileNotFoundException e nil))
   (read-csv (url-path-to-file-path url-path ".csv")
                   (last (clojure.string/split url-path #"/")))))

(defn write-store-file-if-different
  "Function for running in the :agent of (:stores @session-info).
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
  "Function for running in the log-agent of (:stores @session-info).
  Adds the entry to the log stream, and flushes the stream."
  [log-writer entry]
  (binding [*out* log-writer]
    (prn entry)
    (flush))
  log-writer)

(defn update-store-file [url-path]
  (when-let [info ((:stores @session-info) url-path)]
    (send (:agent info)
          write-store-file-if-different (:store info) url-path)))

(defn queue-to-log
  "Add the entry to the queue to be written to the log."
  [entry url-path]
  (when-let [info ((:stores @session-info) url-path)]
    (when-let [agent (:log-agent info)]
      (send agent write-log-entry entry))))

(defn ensure-store
  "Return the store info for the given url path, creating it if necessary,
  returning nil if there is something wrong with the path."
  [url-path]
  (or
   ((:stores @session-info) url-path)
   ;; If there were a race to create the store, the log stream might get
   ;; opened multiple times in ensure-in-atom-map! To avoid that, we run under
   ;; a global lock.
   (locking session-info
     ;; If the path is not valid, we don't want to put nil in
     ;; (:stores @session-info), as that would preclude fixing the path.
     ;; Rather, we leave (:stores @session-info) blank in that case.
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
                {:store (new-mutable-store immutable)
                 :agent (agent immutable)
                 :log-agent log-agent})))]
       (swap! session-info #(assoc-in % [:stores url-path] info))
       info))))

;;; Session management. There is a tracker for each session.

;;; (:sessions @session-info) is a map from id to session state.
;;; Session state consists of a map
;;;       :url-path  The url path corresponding to the store.
;;;          :store  The store that holds the data.
;;;        :tracker  The tracker for the session.
;;;   :client-state  A state-map holding these keys:
;;;                :last-time  The last time we accessed this session.
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
;;; When we process client commands, we add to the session state
;;; :selector-interpretation from the client's request.

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
    (new-state-map {:last-time (System/currentTimeMillis)
                    :referent referent
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

(defn get-session-state [session-id]
  (when-let [state ((:sessions @session-info) session-id)]
    (state-map-reset! (:client-state state) :last-time
                      (System/currentTimeMillis))
    state))

(defn prune-old-sessions [delay-millis]
  (swap!
   session-info
   (fn [session-info]
     (let [last-time-to-keep (- (System/currentTimeMillis) delay-millis)]
       (assoc session-info :sessions
              (reduce-kv (fn [accum id state]
                           (if (< (state-map-get-current-value
                                   (:client-state state) :last-time)
                                  last-time-to-keep)
                             accum
                             (assoc accum id state)))
                         {} (:sessions session-info)))))))

(defn prune-unused-stores []
  ;; We need to close the log streams of any stores we close. But we can't
  ;; do that inside a swap!, as that may be run several times, needing to
  ;; close different stores different times. Instead, we use
  ;; swap-control-return! to inform us which log streams need closing.
  (let [writers-to-close
        (swap-control-return!
         session-info
         (fn [session-info]
           (let [store-infos (:stores session-info)
                 in-use (set (map :store (vals (:sessions session-info))))
                 need-pruning (filter #(not (in-use (:store (store-infos %))))
                                      (keys store-infos))]
             [(assoc session-info :stores
                     (apply dissoc store-infos need-pruning))
              (map #(:log-agent (store-infos %)) need-pruning)])))]
    (doseq [to-close writers-to-close]
      (send to-close #(.close %)))))

(defn create-session
  "Create a session with the given id, or with a new id if none is given."
  [session-id url-path referent-string manager-data selector-string]
  (prune-old-sessions (* 60 60 1000))
  (when-let [store (:store (ensure-store url-path))]
    (let [id (swap-control-return!
              session-info
              (fn [session-info]
                (let [session-map (:sessions session-info)
                      id (or session-id (new-id session-map))
                      client-state (create-client-state store referent-string)]
                  [(assoc-in session-info [:sessions id]
                             {:url-path url-path
                              :store store
                              :tracker (create-tracker
                                        store client-state
                                        manager-data selector-string)
                              :client-state client-state})
                   id])))]
      (prune-unused-stores)
      (compute manager-data 1000)
      (println "computed some")
      id)))

(defn ensure-session
  "Make sure there is a session with the given id, and return its state."
  [session-id url-path referent-string manager-data selector-string]
  (or (get-session-state session-id)
      (let [session-id (create-session session-id url-path referent-string
                                       manager-data selector-string)]
          (get-session-state session-id))))

(defn forget-session
  "The session is no longer needed. Forget about it."
  [session-id]
  (swap! session-info
         (fn [session-info]
           (let [session-map (:sessions session-info)]
             (assoc session-info :sessions
                    (if-let [state (session-map session-id)]
                      (do (remove-all-doms (:tracker state))
                          (Thread/sleep 100)
                          (dissoc session-map session-id))
                      session-map))))))


(ns cosheet.server.session-state
  (:import [java.nio.file Files CopyOption StandardCopyOption])
  (:require
   [clojure-csv.core :refer [parse-csv]]
   (cosheet
    [utils :refer [swap-control-return! ensure-in-atom-map! with-latest-value
                   parse-string-as-number]]
    [expression-manager :refer [current-value]]
    [orderable :as orderable]
    [store :refer [new-element-store new-mutable-store current-store
                   read-store write-store store-to-data data-to-store
                   do-update-control-return! declare-temporary-id]]
    store-impl
    mutable-store-impl
    [store-utils :refer [add-entity]]
    [query :refer [matching-items]]
    [debug :refer [simplify-for-print]]
    [expression-manager :refer [compute]]
    [state-map :refer [new-state-map
                       state-map-get-current-value state-map-reset!]])
   (cosheet.server
    [order-utils :refer [update-add-entity-adjacent-to  order-element-for-item]]
    [model-utils :refer [starting-store add-table first-tab-R
                         specialize-template visible-item?-R]]
    [format-convert :refer [convert-to-current]]
    [referent :refer [item-referent referent->exemplar-and-subject
                      string->referent referent->string]]
    [instantiate :refer [instantiate-referent]]
    [render :refer [DOM-for-client-R]]
    [dom-tracker :refer [new-dom-tracker add-dom remove-all-doms]])))

;;; This is the only function that directly turns one url into another.
;;; It is used only to remove the suffix from an initial url, so the client
;;; can be asked to redirect to the clean url.
(defn remove-url-file-extension
  "If the url has a file extension, remove it."
  [url]
  (let [parts (clojure.string/split url #"\?")
        path-parts (clojure.string/split (first parts) #"/")
        file-parts (clojure.string/split (last path-parts) #"\.")]
    (if (> (count file-parts) 1)
      (let [path (clojure.string/join
                  "/" (concat (butlast path-parts) [(first file-parts)]))]
        (clojure.string/join "?" (concat [path] (rest parts))))
      url)))

;;
;; functions to retrieve hard coded global params
;; (e.g., default userdata directory path)
;;
(defn isAdmin
  [user-id]
  (= "admin" user-id))

;;; If there is a top level mount point, /cosheet, then we put our data
;;; there, otherwise in /~/cosheet/.
(defn cosheet-data-path-uncached []
  (if (.exists (clojure.java.io/file "/cosheet/"))
    ;; on server
    "/cosheet/"
    ;; on local machine
    ;; We need to get the home directory, but with /, not \, even on Windows.
    ;; We do that by turning it into a URI, then replacing the prefix with "/".
    (let [home (-> (new java.io.File (System/getProperty "user.home"))
                   (.toURI)
                   (.toString)
                   (clojure.string/replace-first #"[^:]*:/*" "/"))]
      (str home "cosheet/"))))

(def cosheet-data-path (memoize cosheet-data-path-uncached))

(defn get-db-path
  [filename]
  (str (cosheet-data-path) filename))

;;; User data is stored in <cosheet-data-path>/userdata/<user-id>/<filename>
(defn get-userdata-path
  [user-id]
  (str (cosheet-data-path) "userdata/" user-id "/"))

(defn url-path-to-file-path
  "Turn a url path into a file path, returning nil if the url path is
  syntactically ill formed."
  [url-path user-id]
  (when (and (not (clojure.string/ends-with? url-path "/"))
             (clojure.string/starts-with? url-path "/cosheet/"))
    (str (get-userdata-path user-id) (subs url-path 9))))

(defn interpret-file-path
  "Given the file path, return the path without the suffix,
  the name, and the suffix, using appropriate defaults.
  Return nil if the path is not well formed."
  [file-path]
  (when file-path
    (let [parts (clojure.string/split file-path #"/")
          directory-path (clojure.string/join "/" (butlast parts))
          filename (last parts)
          name-parts (clojure.string/split filename #"\.")
          name (first name-parts)
          without-suffix (str directory-path "/" name)]
      (cond (= (count name-parts) 1)
            ;; If there was no extension, we default to "cosheet"
            [without-suffix name ".cosheet"]
            (and (= (count name-parts) 2)
                 (#{"cosheet" "csv"} (second name-parts)))
            [without-suffix name (str "." (second name-parts))]))))

(defn path-to-Path
  "Turn a string file path to a Java file Path object, which is what various
  Java File static methods need."
   [path]
  (java.nio.file.Paths/get
   (java.net.URI. (str "file://" path))))

(defn has-valid-directory?
  "Return whether the file path refers to an actual directory."
  [path]
  (and path
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

;;; (:stores @session-info) is a map from file path (with suffix omitted)
;;; to a map:
;;;  {        :store The mutable-store.
;;;           :agent The agent responsible for saving the store. It's state
;;;                  is the last version of the store written. We write
;;;                  the store by doing a send to this agent, so that
;;;                  writes don't block interaction, and will skip
;;;                  intermediate values if they get behind.
;;;       :log-agent The agent responsible for writing log entries.
;;;                  its state is a writer opened to append to the log file.

(defn read-csv-reader
  "parse a reader over a csv file, turning it into a store."
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

(defn add-temporary-element
  "Add a root temporary element to a store.
   Return the id of the element."
  [store]
  (do-update-control-return!
   store
   (fn [store]
     (let [[store id] (add-entity store nil
                                  '("" (anything :batch-query :selector)))]
       [(declare-temporary-id store id) id]))))

(defn get-store
  "Read the store if possible; otherwise create one.
   If the store can't be made, return nil."
  [without-suffix name suffix]
  (when (and without-suffix (has-valid-directory? without-suffix))
    (let [file-path (str without-suffix suffix)]
      (when-let
        [store (cond (= suffix ".cosheet")
                     (try
                       (with-open [stream (clojure.java.io/input-stream
                                           file-path)]
                         (read-store (new-element-store) stream))
                       ;; We return the default starting store only if the
                       ;; default extension was asked for.
                       (catch java.io.FileNotFoundException e
                         (starting-store name)))
                     (= suffix ".csv")
                     (read-csv file-path name))]
        (convert-to-current store)))))

(defn write-store-file-if-different
  "Function for running in the :agent of (:stores @session-info).
   If the current immutable store is different from the written store,
   writes the current value to a file of the given url path. Always returns
   the current-value."
  [written-store mutable-store file-path]
  ;; We write the latest value from the mutable store, rather than the value
  ;; at the time the send was done, so that we will catch up if we get behind.
  (with-latest-value [store (current-store mutable-store)]
    (when (not= written-store store)
      (let [temp-path (str file-path "_TEMP_.cosheet")]
        (clojure.java.io/delete-file temp-path true)
        (with-open [stream (clojure.java.io/output-stream temp-path)]
          (write-store store stream))
        (Files/move (path-to-Path temp-path)
                    (path-to-Path (str file-path ".cosheet"))
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

(defn update-store-file [file-path]
  (when-let [info ((:stores @session-info) file-path)]
    (send (:agent info)
          write-store-file-if-different (:store info) file-path)))

(defn queue-to-log
  "Add the entry to the queue to be written to the log."
  [entry file-path]
  (when-let [info ((:stores @session-info) file-path)]
    (when-let [agent (:log-agent info)]
      (send agent write-log-entry entry))))

(defn ensure-store
  "Return the store info for the given file path, creating it if necessary.
  Also add :without-suffix to the store info returned, giving the
  file path that the store is indexed under.
  Return nil if there is something wrong with the path."
  [file-path]
  (let [[without-suffix name suffix] (interpret-file-path file-path)
        store-info
        ;; We want to make sure we don't have a race between threads to
        ;; create a store twice, open its log stream multiple times, etc.
        ;; So we run under a global lock.
        (locking session-info
          (or
           ((:stores @session-info) without-suffix)
           ;; If the path is not valid, we don't want to put nil in
           ;; (:stores @session-info), as that would preclude fixing the path.
           ;; Rather, we leave (:stores @session-info) blank in that case.
           (when-let [immutable-store
                      (get-store without-suffix name suffix)]
             (let [log-stream (try (java.io.FileOutputStream.
                                    (str without-suffix ".cosheetlog") true)
                                   (catch java.io.FileNotFoundException e
                                     nil))
                   log-agent (when log-stream
                               (agent (clojure.java.io/writer log-stream)))
                   info {:store (new-mutable-store immutable-store)
                         :agent (agent immutable-store)
                         :log-agent log-agent}]
               (when log-agent
                 (when (= (.position (.getChannel log-stream)) 0)
                   (send log-agent
                         write-log-entry
                         [:store (store-to-data immutable-store)]))
                 (send log-agent write-log-entry [:opened]))
               (swap! session-info #(assoc-in % [:stores without-suffix] info))
               info))))]
    (when store-info
      (assoc store-info :without-suffix without-suffix))))

;;; Session management. There is a tracker for each session.

;;; (:sessions @session-info) is a map from id to session state.
;;; Session state consists of a map
;;;             :id  The session id identifying the client.
;;;      :file-path  The file path (with suffix omitted) corresponding
;;;                  to the store.
;;;          :store  The store that holds the data.
;;;    :temporary-id The id of the root temporary item in the store
;;;                  for this session.
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
;;;            :batch-editing  If true, we are batch editing, and showing
;;;                            the batch edit window, rather than whatever
;;;                            referent says we should show.

(defn create-client-state
  [store referent-string]
  (let [immutable-store (current-store store)
        [referent subject-ref]
        (or (when referent-string
              (let [referent (string->referent referent-string)]
                (when referent
                  (let [[_ subject-ref]
                        (referent->exemplar-and-subject referent)
                        item (first (instantiate-referent
                                     referent immutable-store))]
                    (when (and item (current-value (visible-item?-R item)))
                      [referent subject-ref])))))
            (let [tab (first-tab-R immutable-store)]
              [(when tab (item-referent tab)) nil]))]
    (new-state-map {:last-time (System/currentTimeMillis)
                    :referent referent
                    :subject-referent subject-ref
                    :last-action nil
                    :batch-editing false
                    :in-sync false})))

(defn create-tracker
  [store temporary-id client-state manager-data]
  (let [definition [DOM-for-client-R store temporary-id client-state]
        tracker (new-dom-tracker manager-data)]
    (add-dom tracker "root" [] definition)
    (println (new java.util.Date) "created tracker"
             (simplify-for-print definition))
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
  ;; swap-control-return! and have it inform us which log streams need closing.
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
  [session-id file-path referent-string manager-data]
  (prune-old-sessions (* 60 60 1000))
  (when-let [store-info (ensure-store file-path)]
    (let [store (:store store-info)
          temporary-id (add-temporary-element store)
          id (swap-control-return!
              session-info
              (fn [session-info]
                (let [session-map (:sessions session-info)
                      id (or session-id (new-id session-map))
                      client-state (create-client-state store referent-string)]
                  [(assoc-in session-info [:sessions id]
                             {:file-path (:without-suffix store-info)
                              :id id
                              :store store
                              :temporary-id temporary-id
                              :tracker (create-tracker
                                        store temporary-id client-state
                                        manager-data)
                              :client-state client-state})
                   id])))]
      (prune-unused-stores)
      (compute manager-data 100)
      (println (new java.util.Date) "computed some")
      id)))

(defn ensure-session
  "Make sure there is a session with the given id, and return its state."
  [session-id file-path referent-string manager-data]
  (or (get-session-state session-id)
      (let [session-id (create-session session-id file-path referent-string
                                       manager-data)]
        (get-session-state session-id))))

(defn forget-session
  "The session is no longer needed. Forget about it."
  ;; TODO: Remove its temporary item from the store.
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



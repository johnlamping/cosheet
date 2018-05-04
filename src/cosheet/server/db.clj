(ns cosheet.server.db
  (:require [clojure.java.jdbc :as jdbc]
            [cosheet.server.session-state
             :refer [url-path-to-file-path get-db-path]]
            [buddy.hashers :as hashers]))

(def db-spec {:dbtype "h2"
              :dbname (url-path-to-file-path (get-db-path "cosheet-db"))})

(defn add-user-to-db
  [username password]
  (let [results (jdbc/insert! db-spec :usercredentials
                              {:username username
                               :pwdhash (hashers/encrypt password)})]
    (assert (= (count results) 1))
    (first (vals (first results)))))

(defn get-user-pwdhash
  [username]
  (let [results (jdbc/query db-spec
                            [(str "select username, pwdhash from "
                                  "usercredentials where username = ?")
                             username])]
    ;(assert (= (count results) 1))
    (first results)))

(defn remove-user-from-db
  [username]
  (let [results (jdbc/query db-spec
                            [(str "select id, username from "
                                  "usercredentials where username = ?")
                             username])]
    (if results
      (let [record-id (first (vals (first results)))]
        (if record-id
          (jdbc/execute! db-spec
                      ["delete from usercredentials where id = ?" record-id])))
      )))

(defn get-all-users
  []
  (jdbc/query db-spec "select id, username from usercredentials"))

(comment   ;; the following statement was used to create the ./cosheet-db
           ;; refer to http://clojure-doc.org/articles/tutorials/basic_web_development.html#run-your-webapp-during-development
           ;; "lein  repl" and then paste in the following

  (require '[clojure.java.jdbc :as jdbc])
  (require '[buddy.hashers :as hashers])
  (jdbc/with-db-connection [conn {:dbtype "h2" :dbname "~/cosheet/cosheet-db"}]

    (jdbc/db-do-commands conn
      (jdbc/create-table-ddl :usercredentials
        [[:id "bigint primary key auto_increment"]
         [:username "varchar(128)"]
         [:pwdhash "varchar(1024)"]]))

      (jdbc/insert! conn :usercredentials
        {:username "testuser" :pwdhash (hashers/encrypt "testpwd")})
  )
)

(comment ;; the following statement was used to add an add-user-to-db

  (require '[clojure.java.jdbc :as jdbc])
  (require '[buddy.hashers :as hashers])
  (jdbc/with-db-connection [conn {:dbtype "h2" :dbname "~/cosheet/cosheet-db"}]
      (jdbc/insert! conn :usercredentials
        {:username "testuser2" :pwdhash (hashers/encrypt "testpwd2")}))
)

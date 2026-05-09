(ns cosheet.server.db
  (:require [clojure.java.jdbc :as jdbc]
            [cosheet.server.session-state
             :refer [get-db-path]]
            [buddy.hashers :as hashers]))

(def db-spec {:dbtype "h2"
              :dbname (get-db-path "cosheet-db")})

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

;;; This function is meant to be called from the command line to
;;; create a blank db. It is not called by the server code.
(defn create-blank-db
  []
  (jdbc/with-db-connection [conn {:dbtype "h2" :dbname "~/cosheet/cosheet-db"}]
    (jdbc/db-do-commands conn
      (jdbc/create-table-ddl :usercredentials
        [[:id "bigint primary key auto_increment"]
         [:username "varchar(128)"]
         [:pwdhash "varchar(1024)"]]))
  )
)

(comment ;; Here is how to create the initial db from the command line
  ;; mkdir ~/cosheet/userdata/testuser
  ;;   or
  ;; mkdir /cosheet/userdata/testuser
  ;; cd <the project directory>
  ;; lein repl
  (cosheet.server.db/create-blank-db)
  (cosheet.server.db/add-user-to-db "testuser", "testpwd")
)


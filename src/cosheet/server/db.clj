(ns cosheet.server.db
  (:require [clojure.java.jdbc :as jdbc]
            [cosheet.server.session-state :refer [url-path-to-file-path]]
            [buddy.hashers :as hashers]))

(def db-spec {:dbtype "h2" :dbname (url-path-to-file-path "/~/cosheet/cosheet-db")})  ;; note the hard coded path

(defn add-user-to-db
  [username password]
  (let [results (jdbc/insert! db-spec :usercredentials {:username username :pwdhash (hashers/encrypt password)})]
    (assert (= (count results) 1))
    (first (vals (first results)))))

(defn get-user-pwdhash
  [username]
  (let [results (jdbc/query db-spec
                            ["select username, pwdhash from usercredentials where username = ?" username])]
    ;(assert (= (count results) 1))
    (first results)))

(defn get-all-users
  []
  (jdbc/query db-spec "select id, username from usercredentials"))

(comment   ;; the following statement was used to create the ./cosheet-db
           ;; refer to http://clojure-doc.org/articles/tutorials/basic_web_development.html#run-your-webapp-during-development

  (require '[clojure.java.jdbc :as jdbc])
  (jdbc/with-db-connection [conn {:dbtype "h2" :dbname "~/cosheet/cosheet-db"}]

    (jdbc/db-do-commands conn
      (jdbc/create-table-ddl :usercredentials
        [[:id "bigint primary key auto_increment"]
         [:username "varchar(128)"]
         [:pwdhash "varchar(1024)"]]))

      (jdbc/insert! conn :usercredentials
        {:username "jlamping" :pwdhash "bcrypt+sha512$a9bdc1803db9234ab5817dfc13230a8d$12$53748e8de5968c217fbbe7eef3d7d095b2f455a549ea34e2"})

      (jdbc/insert! conn :usercredentials
        {:username "fchu" :pwdhash "bcrypt+sha512$a9bdc1803db9234ab5817dfc13230a8d$12$53748e8de5968c217fbbe7eef3d7d095b2f455a549ea34e2"})

  )
)

(comment ;; the following statement was used to add an add-user-to-db

  (require '[clojure.java.jdbc :as jdbc])
  (jdbc/with-db-connection [conn {:dbtype "h2" :dbname "~/cosheet/cosheet-db"}]
      (jdbc/insert! conn :usercredentials
        {:username "testuser" :pwdhash "bcrypt+sha512$a9bdc1803db9234ab5817dfc13230a8d$12$53748e8de5968c217fbbe7eef3d7d095b2f455a549ea34e2"}))
)

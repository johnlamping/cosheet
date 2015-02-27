(ns cosheet.server
  (:require [ring.adapter.jetty :as jetty]
            [cosheet.server.routes :refer [app]]))

;;; This will start the server from a REPL. To stop or restart it:
;;; (.stop cosheet.server.routes/server)
;;; (.start cosheet.server.routes/server)
;;; Running the server from the REPL requires re-evaluating files to
;;; update the server's behavior.
(defn startup []
  (defonce server
    (jetty/run-jetty app {:port 3000 :join? false})))

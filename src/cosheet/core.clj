(ns cosheet.core
  (:require (cosheet utils reporters store entity query
                     mutable-map task-queue computation-manager
                     store-impl store-utils entity-impl query-impl
                     mutable_store_impl debug)
            :reload)
  (:gen-class))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

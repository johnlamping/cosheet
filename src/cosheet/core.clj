(ns cosheet.core
  (:require (cosheet utils state store entity query
                     mutable-map task-queue compute
                     store-impl entity-impl query-impl
                     compute_impl mutable_store_impl)
            :reload)
  (:gen-class))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

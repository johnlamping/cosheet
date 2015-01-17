(ns cosheet.core
  (:require (cosheet store entity query
                     mutable-map task-queue compute
                     store-impl entity-impl query-impl
                     compute_impl)
            :reload)
  (:gen-class))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

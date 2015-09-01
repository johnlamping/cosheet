(ns cosheet.core
  (:require (cosheet utils expandable-order dom-utils
                     reporters store entity query
                     mutable-map task-queue computation-manager
                     store-impl store-utils entity-impl query-impl
                     mutable_store_impl debug)
            ;; Note: the reload below is key to making reporters.clj
            ;; load successfully. Not clear why
            :reload)
  (:gen-class))

;;; Note, dom-utils is in src_cljx, so you need to run
;;; lein cljx once

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

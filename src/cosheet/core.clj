(ns cosheet.core
  (:require [cosheet.protocols :refer :all])
  (:gen-class))

(load "entity")
(load "store")
(load "query")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

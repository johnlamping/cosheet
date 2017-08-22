(ns cosheet.core
  (:require (cosheet utils orderable hiccup-utils mutable-manager 
                     reporters expression store store-impl mutable-store-impl
                     entity entity-impl canonical query query-impl
                     state-map mutable-map task-queue expression-manager
                     store-utils debug)
            ;; Note: the reload below was key to making reporters.clj
            ;; load successfully. Now it's not Not clear why.
            ; :reload
            )
  (:gen-class))

;;; Note, hiccup-utils is in src_cljx, so you need to run
;;; lein cljx once


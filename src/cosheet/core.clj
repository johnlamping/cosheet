(ns cosheet.core
  (:require (cosheet utils orderable hiccup-utils reporter mutable-manager
                     expression store store-impl mutable-store-impl
                     entity entity-impl canonical query query-impl
                     state-map mutable-map task-queue expression-manager
                     store-utils debug)
            ;; Note: the reload below was key to making reporter.clj
            ;; load successfully. Now it's not. It may have been because
            ;; of old versions of the compiled code lying around.
            ; :reload
            )
  (:gen-class))

;;; Note, hiccup-utils is in src_cljx, so you need to run
;;; lein cljx once


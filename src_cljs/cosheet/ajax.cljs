(ns cosheet.ajax
  (:require [ajax.core :refer [GET POST transit-response-format]]
            [cosheet.client-utils :refer
             [component components
              replace-in-struct into-atom-map
              process-response-for-pending take-pending-params]]
            cosheet.dom-utils
            ))

(declare ajax-handler)
(declare ajax-error-handler)

(def ajax-request-pending (atom false))

(defn ajax-request [params]
  (POST "/ajax-request"
        {:params params
         :response-format (transit-response-format)
         :handler ajax-handler
         :error-handler ajax-error-handler
         :timeout 5000})
  (reset! ajax-request-pending true))

;;; A handle to the current running ajax refresh task.
(def watch-task (atom nil))

(defn start-watch-task
  "Set the ajax watch task to run in 10 seconds."
  []
  (swap! watch-task
         (fn [handle]
           (when handle (js/clearInterval handle))
           (js/setInterval #(ajax-request (take-pending-params)) 600000))))

(defn ajax-if-pending
  "Send an ajax request if we have pending information to send the server
   and there isn't a request already in flight."
  []
  (when (not @ajax-request-pending)
    (let [params (take-pending-params)]
      (when (not= params {})
        (ajax-request params)))))

(defn ajax-handler [response]
  (reset! ajax-request-pending false)
  (when (not= response {})
    (.log js/console (str response))
    (let [doms (:doms response)]
      (when doms
        (into-atom-map components
                       ;; Turn [:component {attributes} <id>]
                       ;; into [cosheet.client/component {attributes} id]
                       (replace-in-struct {:component component} (vec doms)))))
    (process-response-for-pending response)
    (ajax-if-pending))
  (start-watch-task))

(defn ajax-error-handler [{:keys [status status-text]}]
  (reset! ajax-request-pending false)
  (.log js/console (str "ajax-error: " status " " status-text))
  (start-watch-task))

(ns cosheet.ajax
  (:require [ajax.core :refer [GET POST transit-response-format]]
            [reagent.core :as reagent]
            [cosheet.client-utils :refer
             [component components
              replace-in-struct into-atom-map
              process-response-for-pending take-pending-params]]
            [cosheet.edit-field :refer [close-edit-field edit-field-open-on
                                        select selected deselect]]
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

(defn dom-contained-in-changes?
  "Given a dom and a list of dom revisions, return true if the dom
  might be changed."
  ;; TODO: This sees through components, which is too paranoid, as
  ;; a change to a component container won't affect the component
  [dom changes]
  (some (fn [change]
          (let [{:keys [id]} (second change)]
            (and (contains? @components id)
                 (.contains (js/document.getElementById id) dom))))
        changes))

(defn deslect-if-contained
  "Given a list of dom revisions, undo the selection if it is contained 
  in one of the changes."
  [doms]
  (let [selection @selected]
    (when (and selection
               (dom-contained-in-changes? selection doms))
      (deselect))))

(defn close-edit-field-if-contained
  "Given a list of dom revisions, close the edit field if it
   is contained in one of the changes."
  [doms]
  (when (dom-contained-in-changes?
         (js/document.getElementById "edit_holder") doms)
    (close-edit-field)))

(defn handle-ajax-doms
  [response]
  (let [doms (:doms response)]
    (when doms
      (deslect-if-contained doms)
      (close-edit-field-if-contained doms)
      (into-atom-map components
                     ;; Turn [:component {attributes} <id>]
                     ;; into [cosheet.client/component {attributes} id]
                     (replace-in-struct {:component component} (vec doms))))))

(defn handle-ajax-select
  [response]
  (let [[to-select if-selected] (:select response)
        select-target (and to-select (js/document.getElementById to-select))
        current-id (and @selected (.-id @selected))]
    (when (and select-target
               (nil? @edit-field-open-on)
               (or (nil? current-id) (some #{current-id} if-selected)))
      (.log js/console "doing select.")
      (select select-target))))

(defn ajax-handler [response]
  (reset! ajax-request-pending false)
  (when (not= response {})
    (.log js/console (str response))
    (handle-ajax-doms response)
    (reagent/flush)  ;; Must update the dom before the select is processed.
    (handle-ajax-select response)
    (process-response-for-pending response)
    (ajax-if-pending))
  (start-watch-task))

(defn ajax-error-handler [{:keys [status status-text]}]
  (reset! ajax-request-pending false)
  (.log js/console (str "ajax-error: " status " " status-text))
  (start-watch-task))

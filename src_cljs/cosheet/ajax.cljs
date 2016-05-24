(ns cosheet.ajax
  (:require [ajax.core :refer [GET POST transit-response-format]]
            [reagent.core :as reagent]
            [cosheet.client-utils :refer
             [component components
              replace-in-struct into-atom-map add-pending-action
              process-response-for-pending take-pending-params]]
            [cosheet.interaction-state :refer [close-edit-field
                                               edit-field-open-on
                                               select selected deselect]]
            ))

(declare ajax-handler)
(declare ajax-error-handler)

(def ajax-request-pending (atom false))

(defn session-id []
  (-> (->> (array-seq (js/document.getElementsByTagName "meta"))
           (filter #(= (.getAttribute % "itemprop") "session-id")))
      first
      (.getAttribute "content")))

(defn ajax-request [params]
  (POST (clojure.string/join "/" ["/ajax-request" (session-id)])
          {:params params
           :response-format (transit-response-format)
           :handler ajax-handler
           :error-handler ajax-error-handler
           :timeout 5000})
  (reset! ajax-request-pending true))

;;; A handle to the current pending ajax poll task.
(def pending-poll-task (atom nil))

;;; The timeout until the next ajax poll.
(def poll-delay (atom 500))

(defn reset-poll-delay
  "Reset the poll delay to the shortest duration. 
  It will increase if nothing happens."
  []
  (reset! poll-delay 500))

(def schedule-poll-task)

(defn poll-task
  "The task to run when it is time to poll for server changes."
  []
  (swap! poll-delay #(min (* % 10) 3600000))
  (schedule-poll-task)
  (when (not @ajax-request-pending)
    (ajax-request (take-pending-params))))

(defn schedule-poll-task
  "Schedule the ajax poll task to run after the current delay."
  []
  (swap! pending-poll-task
         (fn [handle]
           (when handle (js/clearTimeout handle))
           (js/setTimeout poll-task @poll-delay))))

(defn ajax-if-pending
  "Send an ajax request if we have pending information to send the server
   and there isn't a request already in flight."
  []
  (when (not @ajax-request-pending)
    (let [params (take-pending-params)]
      (when (not= params {})
        (ajax-request params)))))

(defn request-action
  [action]
  (add-pending-action action)
  (reset-poll-delay)
  (ajax-if-pending))

(defn dom-contained-in-changes?
  "Given a dom and a list of dom revisions, return true if the dom
  might be changed."
  ;; TODO: This sees through components, which is too paranoid, as
  ;; a change to a component container won't affect the component.
  [dom changes]
  (some (fn [change]
          (let [{:keys [id]} (second change)]
            (and (contains? @components id)
                 (if-let [dom-for-id (js/document.getElementById id)]
                   (.contains dom-for-id dom)))))
        changes))

(defn deselect-if-contained
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
      (deselect-if-contained doms)
      (close-edit-field-if-contained doms)
      (into-atom-map components
                     ;; Turn [:component {attributes} <id>]
                     ;; into [cosheet.client/component {attributes} id]
                     (replace-in-struct {:component component} (vec doms))))))

(defn handle-ajax-select
  "Do the selection requested by the ajax response, or if none,
  but the old selection was temporarily cleared, restore that selection."
  [response previously-selected-id]
  (when (nil? @edit-field-open-on)
    (let [[request-id if-selected] (:select response)
          target-id (cond (or (nil? previously-selected-id)
                              (some #{previously-selected-id} if-selected))
                          request-id
                          (nil? @selected)
                          previously-selected-id)
          select-target (and target-id (js/document.getElementById target-id))]
      (when select-target
        (.log js/console "doing select.")
        (select select-target)
        ;; Tell the server that their selection request went through.
        (add-pending-action [:selected target-id])))))

(defn ajax-handler [response]
  (reset! ajax-request-pending false)
  (when (not= response {})
    (.log js/console (str response))
    (reset-poll-delay)
    (if (:reload response)
      (js/location.reload)
      (let [previously-selected-id (and @selected (.-id @selected))]
        (handle-ajax-doms response)
        (reagent/flush)  ;; Must update the dom before the select is processed.
        (handle-ajax-select response previously-selected-id)
        (process-response-for-pending response)
        (ajax-if-pending))))
  (schedule-poll-task))

(defn ajax-error-handler [{:keys [status status-text]}]
  (reset! ajax-request-pending false)
  (.log js/console (str "ajax-error: " status " " status-text))
  (schedule-poll-task))

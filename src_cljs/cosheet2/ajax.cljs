(ns cosheet2.ajax
  (:require [ajax.core :refer [GET POST transit-response-format]]
            [reagent.core :as reagent]
            [cosheet2.client-utils :refer
             [component components
              replace-in-struct into-atom-map reset-atom-map-versions!
              add-pending-action add-pending-replay add-pending-clean
              process-response-for-pending take-pending-params]]
            [cosheet2.interaction-state :refer
             [close-edit-field edit-field-open-on select deselect selected
              select-and-clear-pending
              pending-server-selection-request-id]]
            ))

(declare ajax-handler)
(declare ajax-error-handler)

(def ajax-request-open (atom false))

(defn timed-log [item]
  (let [now (js/Date.)]
    (.log js/console (str (subs (.toTimeString now) 0, 8) ":"
                          (.getMilliseconds now) " " item))))

(defn session-id []
  (-> (->> (array-seq (js/document.getElementsByTagName "meta"))
           (filter #(= (.getAttribute % "itemprop") "session-id")))
      first
      (.getAttribute "content")))

(defn ajax-request
  ([params]
   (ajax-request params 5000))
  ([params timeout]
   (timed-log "sending ajax request.")
   (POST (clojure.string/join "/" ["/ajax-request" (session-id)])
         {:params params
          :response-format (transit-response-format)
          :handler ajax-handler
          :error-handler ajax-error-handler
          :timeout timeout})
   (reset! ajax-request-open true)))

;;; A handle to the current pending ajax poll task.
(def pending-poll-task (atom nil))

;;; The timeout until the next ajax poll.
(def poll-delay (atom 500))

(defn reset-poll-delay
  "Reset the poll delay to the shortest duration. 
  It will increase if nothing happens."
  []
  (reset! poll-delay 500))

(declare schedule-poll-task)

(defn poll-task
  "The task to run when it is time to poll for server changes."
  []
  (swap! poll-delay #(min (* % 2) 3600000))
  (schedule-poll-task)
  (when (not @ajax-request-open)
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
  (when (not @ajax-request-open)
    (let [params (take-pending-params)]
      (when (not= params {})
        (ajax-request params)))))

(defn request-action
  [action]
  (add-pending-action action)
  (reset-poll-delay)
  (ajax-if-pending))

(defn request-replay
  [replay]
  (add-pending-replay replay)
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
         (js/document.getElementById "select_holder") doms)
    (close-edit-field)))

(defn handle-ajax-doms
  [response]
  (when-let [doms (:doms response)]
    (deselect-if-contained doms)
    (close-edit-field-if-contained doms)
    (into-atom-map components
                   ;; Turn [:component {attributes} <id>]
                   ;; into [cosheet.client/component {attributes} id]
                   (replace-in-struct {:component component} (vec doms)))))

;;; TODO: We shouldn't need to check for _content any more; the server
;;; adds that when appropriate.
(defn find-target
  "Find the DOM for the target id. If that doesn't exist, try the id
   with _:content appended.
   Return the id that works, in any, and the DOM that it matches."
  [target-id]
  (when target-id
    (let [target (js/document.getElementById target-id)]
      (if target
        [target-id target]
        (let [target-id (str target-id "_:content")
              target (js/document.getElementById target-id)]
          (when target
            [target-id target]))))))

(defn first-valid-target
  "Given a seq of target ids, return the triple of
     [target-id adjusted target-id target]
  of the first one that matches a target (after possible adjustment."
  [target-ids]
  (if (empty? target-ids)
    nil
    (let [[target-id & remaining-target-ids] target-ids
          [adjusted-target-id target] (find-target target-id)]
      (if target
        [target-id adjusted-target-id target]
        (first-valid-target remaining-target-ids)))))

(defn handle-ajax-select
  "Do the selection requested by the ajax response, or if none,
   but the old selection was temporarily cleared, restore that selection."
  [response previously-selected-id]
  (if (nil? @edit-field-open-on)
    (do (let [[request-id if-selected] (:select response)]
          (when (and request-id
                     (or (nil? previously-selected-id)
                         (nil? if-selected)
                         (some #{previously-selected-id} if-selected)))
            (reset! pending-server-selection-request-id request-id)))
        (let [request-id @pending-server-selection-request-id
              [target-id adjusted-target-id select-target]
              (first-valid-target [request-id previously-selected-id])]
          (when select-target
            (select select-target)
            (when (= target-id request-id)
              (reset! pending-server-selection-request-id nil)
              ;; Tell the server that we did the selection.
              (add-pending-action [:selected adjusted-target-id])))))
    (reset! pending-server-selection-request-id nil)))

(defn handle-ajax-open
  "Handle an open window request in an ajax response."
  [response]
  (when-let [path (:open response)]
    (.open js/window path "CosheetExpandPopup")))

(defn handle-ajax-set-url
  "Handle a request to set the url in an ajax response"
  [response]
  (when-let [url (:set-url response)]
    (js/history.replaceState  "" "" url)))

(defn handle-ajax-reset-versions
  "Handle a request to reset the client version information."
  [response]
  (when (:reset-versions response)
    (reset-atom-map-versions! components)
    (add-pending-clean js/window.location.href)))

(defn ajax-handler [response]
  (reset! ajax-request-open false)
  (when (not= response {})
    (timed-log response)
    (when (not= response {})
      (reset-poll-delay))
    (if (:reload response)
      (js/location.reload)
      (let [;; Remember the current selection before we mess with the DOM.
            previously-selected-id (and @selected (.-id @selected))]
        (handle-ajax-reset-versions response)
        (handle-ajax-doms response)
        (reagent/flush)  ;; Must update the dom before the select is processed.
        (handle-ajax-select response previously-selected-id)
        (handle-ajax-open response)
        (handle-ajax-set-url response)
        (process-response-for-pending response)
        (ajax-if-pending))))
  (schedule-poll-task))

(defn ajax-error-handler [{:keys [status status-text]}]
  (reset! ajax-request-open false)
  (timed-log (str "ajax-error: " status " " status-text))
  (schedule-poll-task))

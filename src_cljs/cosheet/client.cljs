(ns cosheet.client
  (:require [reagent.core :as reagent :refer [atom]]
            [ajax.core :refer [GET POST transit-response-format]]
            [goog.dom :as gdom]
            [goog.events :as gevents]
            [goog.events.KeyCodes :as key-codes]
            [goog.events.KeyHandler :as key-handler]
            [cosheet.client-utils :refer
             [component components
              replace-in-struct into-atom-map]]
            ;; Note: We seem to have to declare everything used
            ;; by our libraries in order for them to be visible to
            ;; Chrome.
            ;; TODO: Test if this is true, by requiring clojure.set,
            ;; and seeing if that works.
            cosheet.dom-utils
            ))

(reset! components {"root" (atom [:div {:id "root" :version 0}])})

(def ajax-handler)
(def ajax-error-handler)

(defn ajax-request [params]
  (POST "/ajax-request"
        {:params params
         :response-format (transit-response-format)
         :handler ajax-handler
         :error-handler ajax-error-handler}))

;;; A handle to the current running ajax refresh task.
(def watch-task (clojure.core/atom nil))

(defn clear-watch-task
  "Stop any ajax watch task from running."
  []
  (swap! watch-task (fn [handle]
                      (when handle (js/clearInterval handle))
                      nil)))

(defn start-watch-task
  "Make sure the ajax watch task is running"
  []
  (swap! watch-task (fn [handle]
                     (or handle
                         (js/setInterval #(ajax-request {}) 10000)))))

(defn ajax-acknowledge
  "Send an ajax request acknowledging recipt of the given response (which
   should a a vector of doms.
   The acknowledgement is a map from id to version."
  [response]
  (let [params (into {} (map (fn [[tag {:keys [id version]} &rest]]
                               [id version])
                             response))]
    (ajax-request {:acknowledge params})))

(defn ajax-handler [response]
  (when (not= response [])
    (.log js/console (str response))
    (into-atom-map
     components
     ;; Turn [:component {attributes} <id>]
     ;; into [cosheet.client/component {attributes} id]
     (replace-in-struct {:component component} (vec response)))
    (.log js/console (str "Components: " (keys @components)))
    (ajax-acknowledge response))
  (clear-watch-task)
  (start-watch-task))

(defn ajax-error-handler [{:keys [status status-text]}]
  (.log js/console (str "ajax-error: " status " " status-text)))

(def edit-field-open-on (clojure.core/atom nil))

(defn open-edit-field [target]
  (when (and target (not= target @edit-field-open-on))
    (let [edit-holder (js/document.getElementById "edit_holder")
          edit-input (js/document.getElementById "edit_input")
          original_value (gdom/getTextContent target)]
      (set! (.-value edit-input) original_value)
      (gdom/appendChild target edit-holder)
      (.focus edit-input)
      (.select edit-input)
      (reset! edit-field-open-on target))))

(defn close-edit-field
  "Close the edit field, without storing the value."
  []
  (when @edit-field-open-on
    (let [edit-holder (js/document.getElementById "edit_holder")
          app (js/document.getElementById "app")]
      (reset! edit-field-open-on nil)
      ;; Put it at the end, where it will be invisible, but still findable.
      (gdom/appendChild app edit-holder))))

(defn store-edit-field
  []
  (let [target @edit-field-open-on]
    (when target
      (let [edit-input (js/document.getElementById "edit_input")
            value (.-value edit-input)]
        (when (not= value (gdom/getTextContent target))
          ;; TODO: tell the server to do something here.
          (.log js/console (str "supposed to store " value
                                " into " (.-id target))))))))

(defn double-click-handler
  [event]
  (let [target (.-target event)]
    (.log js/console (str "Double click on " (.-id target) "."))
    ;; TODO: Check to see if it is editable before bringing up editor.
    (store-edit-field)
    (open-edit-field target)))

(defn keypress-handler
  [event]
  (let [ctrl (.-ctrlKey event)
        alt (.-altKey event)
        key-code (.-keyCode event)
        ]
    (when ctrl
      (when (= key-code key-codes/Z)
        ;; TODO: Put undo functionality here too.
        (when @edit-field-open-on (close-edit-field))))
    (when (not (or ctrl alt))
      (cond
        (= key-code key-codes/ESC) (close-edit-field)
        (= key-code key-codes/ENTER) (do (store-edit-field)
                                         (close-edit-field))))))

;; TODO: Put a click handler that stores the edit field too.

(defn ^:export run []
  (let [app (js/document.getElementById "app")
        edit-input (js/document.getElementById "edit_input")
        ;; The key handler makes events consistent across browsers.
        key-handler (gevents/KeyHandler. edit-input)]
    (reagent/render [component {} "root"] app)
    (gevents/listen app gevents/EventType.DBLCLICK double-click-handler)
    (gevents/listen key-handler key-handler/EventType.KEY
                    keypress-handler))
  (ajax-request {:initialize true}))

;;; TODO: Get rid of this eventually; It's just something cute.
(defonce time-updater
  (js/setInterval
   #(let [clock (@components :clock)]
      (when clock
        (let [now
              (-> (js/Date.) .toTimeString (clojure.string/split " ")  first)]
          (swap! clock (fn [old] (assoc old 2 now))))))
   1000))

; (js/alert "Hello from ClojureScript!")


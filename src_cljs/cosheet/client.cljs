(ns cosheet.client
  (:require [reagent.core :as reagent :refer [atom]]
            [ajax.core :refer [GET POST transit-response-format]]
            [cosheet.client-utils :refer
             [component components
              replace-in-struct into-atom-map]]
            ;; Note: We seem to have to declare everything used
            ;; by our libraries in order for them to be visible to
            ;; Chrome.
            ;; TODO: Test if this is true, bu requiring clojure.set,
            ;; and seeing if that works.
            cosheet.dom-utils
            ))

(reset! components {:main (atom [:div
                                 {:id :main :version 0}
                                 [component {} :message]
                                 [component {} :clock]])
                    :message (atom [:div {:id :message :version 0}
                                    "Hellllo "
                                    [component {} :old]
                                    " world, it is now"])
                    :old (atom [:div {:id :old :version 0}
                                  "old"])
                    :clock (atom [:div {:id :clock :version 0}
                                  "now"])})

(defonce time-updater
  (js/setInterval
   #(let [clock (@components :clock)]
      (when clock
        (let [now
              (-> (js/Date.) .toTimeString (clojure.string/split " ")  first)]
          (swap! clock (fn [old] (assoc old 2 now))))))
   1000))

(defn ajax-handler [response]
  (.log js/console (str response))
  (.log js/console (str "Before: " (keys @components)))
  (into-atom-map
   components
   ;; Turn [:component <id>] into [cosheet.client/component id]
   (replace-in-struct {:cosheet/component component} response))
  (.log js/console (str "After: " (keys @components)))
  (.log js/console (str "After M: " @(:message @components)))
  (clear-ajax-task)
  (start-ajax-task))

(defn ajax-error-handler [{:keys [status status-text]}]
  (.log js/console (str "ajax-error: " status " " status-text)))

(defn ajax-request [params]
  (POST "/ajax-request"
        {:params params
         :response-format (transit-response-format)
         :handler ajax-handler
         :error-handler ajax-error-handler}))

(defn ajax-acknowledge
  "Send an ajax request acknowledging recipt of the given response (which
   should a a vector of doms.
   The acknowledgement is a map from id to version."
  [response]
  (when (> 0 (count response))
    (let [params (into {} (map (fn [tag {:keys [id version]} &rest]
                                 [id version])
                               response))]
      (ajax-request params))))

;;; A handle to the current running ajax refresh task.
(def ajax-task (clojure.core/atom nil))

(defn clear-ajax-task
  "Stop any ajax refresh task from running."
  []
  (swap! ajax-task (fn [handle]
                     (when handle (js/clearInterval handle))
                     nil)))

(defn start-ajax-task
  "Make sure the ajax refresh task is running"
  []
  (swap! ajax-task (fn [handle]
                     (or handle
                         (js/setInterval #(ajax-request {}) 10000)))))

(defn ^:export run []
  (reagent/render [component {} :main]
                  (js/document.getElementById "app"))
  (js/setTimeout #(ajax-request {:test {}}) 3000))

; (js/alert "Hello from ClojureScript!")


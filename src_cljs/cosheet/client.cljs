(ns cosheet.client
  (:require [reagent.core :as reagent :refer [atom]]
            [ajax.core :refer [GET POST transit-response-format]]
            [cosheet.client-utils :refer [replace-in-struct into-atom-map]]))

(def components (clojure.core/atom {}))

(defn component [name]
  @(@components name))

(reset! components {:main (atom [:div
                                 [component :message]
                                 [component :clock]])
                    :message (atom [:div "Hello world, it is now"])
                    :clock (atom [:div "now"])})

(defonce time-updater
  (js/setInterval
   #(reset! (@components :clock)
            [:div
             (-> (js/Date.) .toTimeString (clojure.string/split " ") first)])
   1000))

(defn ajax-handler [response]
  (.log js/console (str response))
  (into-atom-map
   components
   (replace-in-struct {:cosheet/component component} response)))

(defn ajax-error-handler [{:keys [status status-text]}]
  (.log js/console (str "ajax-error: " status " " status-text)))

(defn ajax-request [params]
  (POST "/ajax-request"
        {:params params
         :response-format (transit-response-format)
         :handler ajax-handler
         :error-handler ajax-error-handler}))

(defn ^:export run []
  (reagent/render [component :main]
                  (js/document.getElementById "app"))
  (js/setTimeout #(ajax-request {}) 3000))

; (js/alert "Hello from ClojureScript!")


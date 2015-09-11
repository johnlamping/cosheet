(ns cosheet.server.views
  (:require
   [hiccup.page :refer [html5 include-js include-css]]
   [ring.util.response :refer [response]]
   (cosheet
    [orderable :as orderable]
    [store :refer [new-element-store new-mutable-store current-store]]
    store-impl
    mutable-store-impl
    [entity :refer [description->entity]]
    entity-impl
    [store-utils :refer [add-entity]]
    [query :refer [query-matches]]
    query-impl
    [computation-manager :refer [new-management compute]])
   (cosheet.server
    [render :refer [item-DOM]]
    [dom-tracker :refer [new-dom-tracker add-dom
                         process-acknowledgements response-doms]]
    [actions :refer [do-actions]])))

(defn initial-page []
  (html5
    [:head
     [:title "Hello World"]
     (include-js "/js/main.js")
     (include-css "style.css")]
    [:body
     [:div#app "Root"]
     [:div#edit_holder [:input#edit_input]]
     [:script "cosheet.client.run();"]]))

(defn create-store
  []
  (let [unused-orderable orderable/initial
        [o1 unused-orderable] (orderable/split unused-orderable :after)
        [o2 unused-orderable] (orderable/split unused-orderable :after)
        [o3 unused-orderable] (orderable/split unused-orderable :after)
        [o4 unused-orderable] (orderable/split unused-orderable :after)
        starting-item `("Joe"
                        (:root :invisible)
                        (~o1 :order)
                        ("male" (~o1 :order))
                        ("married" (~o2 :order))
                        (39 (~o3 :order)
                            ("age" ~'tag)
                            ("doubtful" "confidence"))
                        (45 (~o3 :order)
                            ("age" ~'tag)))
        [store id] (add-entity (new-element-store) nil starting-item)
        [store _] (add-entity store nil (list unused-orderable
                                              :unused-orderable))]
    (new-mutable-store store)))

(defonce store (create-store))

(defonce management (new-management 1))

(defn create-tracker
  [mutable-store]
  (let [immutable-root-item (:v (first (query-matches
                                        '(:variable (:v :name)
                                                    ((nil :root) :condition)
                                                    (true :reference))
                                        (current-store store))))
        root-item (description->entity (:item-id immutable-root-item)
                                        mutable-store)
        definition [item-DOM root-item ["root"] #{} {:depth 0}]
        tracker (new-dom-tracker management)]
    (add-dom tracker "root" ["root"] definition)
    tracker))

;;; TODO: this needs to be separate for each web page.
(def dom-tracker (atom nil))

;;; The parameters for the ajax request and response are:
;;; request:
;;;    :initialize If true, the server should assume the client is
;;;                starting from scratch. No other parameters
;;;                should be present.
;;;       :actions A map id -> action to be performed, where each
;;;                action looks like [action_type arg ...], and the
;;;                action types are those implemented in actions.clj.
;;;                The action ids should sort in the order in which
;;;                the actions should be done.
;;;   :acknowledge A map component-id -> version of pairs for which
;;;                the dom of that version was received by the client.
;;; response:
;;;          :doms A list of hiccup encoded doms of components. Their
;;;                attributes will include a unique :id and a :version
;;;                number that will increase for each change of the dom
;;;                for that id. Inside the doms may be internal components
;;;                encoded as [:component {attributes} id].
;;;   :acknowledge A vector of action ids of actions that have been
;;;                performed.
;;; TODO: Action ids should be required to be integers, and the server
;;;       should not perform out of order actions. It should
;;;       acknowledge just the highest action it has seen.

(defn ajax-response [request]
  (let [params (:params request)
        {:keys [actions acknowledge initialize]} params
        must-initialize (or initialize (nil? @dom-tracker))]
    (println "request params" params)
    (when must-initialize
      (println "initializing")
      (reset! dom-tracker (create-tracker store))
      (println "created tracker")
      (compute management 1000)
      (println "computed some"))
    (println "process acknowledgements" acknowledge)
    (process-acknowledgements @dom-tracker acknowledge)    
    (when actions
      (do-actions store @dom-tracker actions)
      (compute management 100))
    ;; Note: We must get the doms after doing the actions, so we can
    ;; immediately show the response to the actions.
    (let [doms (response-doms @@dom-tracker 10)
          answer (cond-> {}
                   (> (count doms) 0) (assoc :doms doms)
                   actions (assoc :acknowledge (vec (keys actions))))]
      (println "response" answer)
      (response answer))))

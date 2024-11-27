(ns cosheet2.server.render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet2
             [debug :refer [simplify-for-print]]
             [test-utils :refer [check any as-set]]
             [store :refer [new-mutable-store]]
             store-impl
             [reporter :refer [reporter-data reporter-value]]
             [calculator :refer [new-calculator-data computation-value]]
             [task-queue :refer [new-priority-task-queue]]
             [utils :refer [call-pseudo-closure]])
            (cosheet2.server
             [render :refer :all]
             [tabs-render :refer [render-tabs-DOM]]
             [table-render :refer [render-table-DOM]]
             [action-data :refer [get-id-action-data]]
             [model-utils :refer [starting-store]]
             [session-state :refer [update-add-session-temporary-element
                                    create-client-state]])
            ; :reload
            ))

(defn render_dom_spec
  [spec mutable-store]
  (let [queue (new-priority-task-queue 0)
        cd (new-calculator-data queue)
        renderer (dom-renderer spec)
        data-getter (rendering-data-getter spec)
        data-reporters (map first (call-pseudo-closure
                                   data-getter spec mutable-store))
        data (map #(computation-value % cd) data-reporters)]
    (apply call-pseudo-closure renderer spec data)))

(defn render_component
  [component mutable-store]
  (let [spec (second component)]
    (render_dom_spec spec mutable-store)))

(deftest initial-top-level-item-DOM-R-test
  (let [store (starting-store "Tab")
        [store temporary-id] (update-add-session-temporary-element store)
        mutable-store (new-mutable-store store)
        client-state (create-client-state mutable-store nil)
        top-level-id (top-level-id-R mutable-store client-state)
        queue (new-priority-task-queue 0)
        cd (new-calculator-data queue)
        DOM-R (top-level-DOM-R mutable-store temporary-id
                               client-state top-level-id)
        dom (computation-value DOM-R cd)]
    (is (check
         dom
         [:div {}
          [:div {:class "tabbed"}
	   [:component {:relative-id (any)
		 	 :chosen-tab-id (any)
			 :render-dom render-tabs-DOM
			 :get-action-data [get-id-action-data (any)]}]
	   [:component {:width 1.5
                        :template ""
			:relative-id (any)
			:render-dom render-table-DOM}]]]))
    ;; TODO: Try turning the second component into DOM, to see
    ;; why the app fails when you try to change a header.))

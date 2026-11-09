(ns cosheet.server.render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [debug :refer [simplify-for-print]]
             [test-utils :refer [check any as-set]]
             [store :refer [new-mutable-store]]
             [store-utils :refer [add-object]]
             [entity :refer [make-tree-object name-label]]
             store-impl
             [reporter :refer [reporter-data reporter-value-or-invalid]]
             [calculator :refer [make-calculator-data computation-value]]
             [task-queue :refer [make-priority-task-queue]]
             [utils :refer [call-pseudo-closure]])
            (cosheet.server
             [render :refer :all]
             [tabs-render :refer [render-tabs-DOM]]
             [table-render :refer [render-table-DOM-R get-table-rendering-data]]
             [item-render :refer [render-item-DOM-R]]
             [action-data :refer [default-get-action-data
                                  get-id-action-data]]
             [model-utils :refer [starting-store add-examine-tab]]
             [session-state :refer [update-add-session-ephemeral-object
                                    create-client-state]])
            ; :reload
            ))

(defn render-dom-spec
  [spec mutable-store]
  (let [queue (make-priority-task-queue 0)
        cd (make-calculator-data queue)
        renderer (dom-renderer spec)
        dom-R (renderer spec mutable-store)]
    (computation-value dom-R cd)))

(defn render-component
  [component mutable-store]
  (let [spec (second component)]
    (render-dom-spec spec mutable-store)))

(deftest initial-top-level-item-DOM-R-test
  (let [store (starting-store "Tab")
        [store ephemeral-id] (update-add-session-ephemeral-object store)
        mutable-store (new-mutable-store store)
        client-state (create-client-state mutable-store nil)
        top-level-id (top-level-id-R mutable-store client-state)
        queue (make-priority-task-queue 0)
        cd (make-calculator-data queue)
        DOM-R (top-level-DOM-R mutable-store ephemeral-id
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
	   [:component {:relative-id (any)
                        :table-id (any)
			:render-dom render-table-DOM-R
                        :get-action-data default-get-action-data}]]]))))

(deftest examine-tab-top-level-DOM-R-test
  ;; With an examine tab chosen, the top level shows the tabs and the
  ;; examined item.
  (let [store (starting-store "Tab")
        [store ephemeral-id] (update-add-session-ephemeral-object store)
        [store joe-id] (add-object store
                                   (make-tree-object [`("Joe" (~name-label))]))
        [store tab-id] (add-examine-tab store joe-id)
        mutable-store (new-mutable-store store)
        client-state (create-client-state mutable-store tab-id)
        top-level-id (top-level-id-R mutable-store client-state)
        queue (make-priority-task-queue 0)
        cd (make-calculator-data queue)
        DOM-R (top-level-DOM-R mutable-store ephemeral-id
                               client-state top-level-id)
        dom (computation-value DOM-R cd)
        item-component (nth (nth dom 2) 3)]
    (is (check
         dom
         [:div {}
          [:div {:class "tabbed"}
           [:component {:relative-id (any)
                        :chosen-tab-id tab-id
                        :render-dom render-tabs-DOM
                        :get-action-data [get-id-action-data (any)]}]
           [:component {:relative-id joe-id
                        :width 3.0
                        :template ""
                        :must-show-label true
                        :render-dom render-item-DOM-R
                        :get-action-data [get-id-action-data joe-id]}]]]))
    (is (check (render-component item-component mutable-store)
               [:div (any) (any) (any)]))))

(ns cosheet.server.batch-edit-render
  (:require (cosheet [store :refer [call-dependent-on-id]]
                     [entity :as entity]
                     [query :refer [matching-elements matching-items]]
                     [debug :refer [simplify-for-print]]
                     [hiccup-utils :refer [dom-attributes
                                           into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq]])
            (cosheet.server
             [referent :refer [item-referent elements-referent query-referent
                               virtual-referent exemplar-referent
                               union-referent instantiate-referent
                               semantic-elements-R]]
             [render-utils :refer [add-inherited-attribute
                                   virtual-referent-DOM]]
             [model-utils :refer [table-header-template]]
             [order-utils :refer [order-items-R]]
             [item-render :refer [item-DOM-R
                                  virtual-element-with-label-DOM
                                  add-labels-DOM
                                  labels-and-elements-DOM-R]])))

(defn top-level-items-referent
  "Return a referent to all the top level items that match the query item."
  [query-item]
  (query-referent (list (item-referent query-item) :top-level)))

(defn table-headers-referent
  " All table conditions matching the query."
  [query-item]
  (query-referent (list (item-referent query-item) :row-condition)))

(defn batch-edit-stack-virtual-DOM-R
  "Return the DOM for a virtual element in the edit stack part of the display,
   that is an element of the query item."
  [query-item inherited]
  (let [query-virtual-referent (virtual-referent
                                'anything (item-referent query-item))
        headers-virtual-referent (virtual-referent
                                  table-header-template
                                  (table-headers-referent query-item))
        matches-virtual-referent (virtual-referent
                                  "" (top-level-items-referent query-item))
        tag-referent (union-referent
                      [(virtual-referent
                        '(anything :tag) query-virtual-referent)
                       (virtual-referent
                        '(anything :tag) headers-virtual-referent)
                       (virtual-referent
                        '("" :tag) matches-virtual-referent)])
        ;; If we are setting the content, then don't add to headers, since
        ;; their content must always be 'anything-immutable.
        dom (virtual-referent-DOM
             (union-referent [query-virtual-referent
                              matches-virtual-referent])
             inherited)
        tag-dom (add-attributes
                 (virtual-referent-DOM
                  tag-referent
                  (-> inherited
                      (update :key-prefix #(conj % :tags))
                      (assoc :select-pattern (conj (:key-prefix inherited)
                                                   :label [:pattern]))))
                 {:class "tag"})]
    (add-labels-DOM tag-dom dom :vertical)))

(defn batch-edit-stack-DOM-R
  "Return the dom for the edit stack part of the batch edit display."
  [query-item query-elements store inherited]
  (let [top-level-matches-referent (top-level-items-referent query-item)
        table-header-matches-referent (table-headers-referent query-item)
        matches-referent (union-referent
                          [(item-referent query-item)
                           top-level-matches-referent
                           table-header-matches-referent])
        inherited-for-batch
        (-> inherited
            (assoc :selector-category :batch-stack :match-all true)
            ;; Make its doms have different keys.
            (update :key-prefix #(conj % :batch-stack)))]
    (expr-let
        [virtual-dom (batch-edit-stack-virtual-DOM-R
                      query-item inherited-for-batch)
         batch-dom
         (let [inherited
               (-> inherited-for-batch
                   (assoc :subject-referent matches-referent)
                   ;; If the selected item is the whole query, then a
                   ;; delete could remove the whole condition of a
                   ;; table. Have it just remove top level rows, instead.
                   (add-inherited-attribute
                    [#{:content}
                     {:delete {:referent top-level-matches-referent}}]))]
           ;; TODO: Add-twin is a problem here with not knowing
           ;; what template to use.
           (labels-and-elements-DOM-R
            query-elements virtual-dom true true :horizontal inherited))
         count-dom
         (call-dependent-on-id
          store nil
          (fn [store]
            (let [header (count (instantiate-referent
                                 table-header-matches-referent store))
                  non-header (count (instantiate-referent
                                     top-level-matches-referent store))]
              [:div {:class "batch-query-match-counts"}
               (str non-header " row matches.  "
                    header " table header matches.")])))]
      [count-dom
       (add-attributes batch-dom {:class "batch-stack"})])))

(defn batch-edit-DOM-R
  "Return the DOM for batch editing, given the item specifying the query,
  and the item, if any, giving the sub-part of the query to operate on
  in batch mode."
  [query-item store inherited]
  (let [inherited-for-query
        (-> inherited
            (assoc :selector-category :batch-query
                   :subject-referent (item-referent query-item))
            (update :key-prefix #(conj % :batch-query)))]
    (expr-let
        [condition-elements (semantic-elements-R query-item)
         stack-dom (batch-edit-stack-DOM-R
                    query-item condition-elements store
                    inherited)]
      [:div
       [:div#quit-batch-edit.tool
              [:img {:src "../icons/exit.gif"}]
        [:div.tooltip "exit batch edit (C-Q)"]]
       [:div  {:class "batch-holder"}
        [:div {:class "query-result-wrapper"}
         [:div {:class "query-result-indent tag"}]
         (into [:div {:class "batch-stack-wrapper"}] stack-dom)]]])))

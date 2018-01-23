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
                               virtual-referent parallel-union-referent
                               exemplar-referent
                               union-referent instantiate-to-items
                               semantic-elements-R]]
             [render-utils :refer [add-inherited-attribute
                                   virtual-referent-item-DOM]]
             [model-utils :refer [table-header-template]]
             [order-utils :refer [order-items-R]]
             [item-render :refer [item-DOM-R
                                  virtual-element-DOM-R
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

(defn referent-for-virtual-element
  "Return a referent for a new virtual element that comes after the current
  last element."
  [template subject-referent last-element-referent]
  (virtual-referent
   template
   subject-referent
   (when last-element-referent
     (exemplar-referent last-element-referent subject-referent))
   :position :after
   :selector :first-group))

(defn batch-edit-stack-virtual-DOM-R
  "Return the DOM for a virtual element, when the selected batch item
  is the query item."
  [query-item query-elements inherited]
  (expr-let [ordered-elements (order-items-R query-elements)]
    (let [last-element (last ordered-elements)
          last-element-referent (when last-element (item-referent last-element))
          non-headers-referent (referent-for-virtual-element
                                'anything
                                (union-referent
                                 [(item-referent query-item)
                                  (top-level-items-referent query-item)])
                                last-element-referent)
          headers-referent (referent-for-virtual-element
                            table-header-template
                            (union-referent
                             [(table-headers-referent query-item)])
                            last-element-referent)
          tag-referent (union-referent
                        [(virtual-referent
                          '(anything :tag) non-headers-referent nil)
                         (virtual-referent
                          '(anything :tag) headers-referent nil)])
          ;; If we are setting the content, then don't add to headers, since
          ;; their content must always be 'anything-immutable.
          dom (virtual-referent-item-DOM
               non-headers-referent (conj (:key-prefix inherited) :virtual)
               inherited)
          tag-dom (add-attributes
                   (virtual-referent-item-DOM
                    tag-referent
                    (conj (:key-prefix inherited) :virtual :label)
                    inherited)
                   {:class "tag"})]
      (add-labels-DOM tag-dom dom :vertical))))

(defn batch-edit-stack-DOM-R
  "Return the dom for the edit stack part of the batch edit display."
  [query-item query-elements store inherited]
  (println (simplify-for-print ["Batch stack" query-item query-elements]))
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
                      query-item query-elements inherited-for-batch)
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
            (let [header (count (instantiate-to-items
                                 table-header-matches-referent store))
                  non-header (count (instantiate-to-items
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
         query-virtual-element-dom (virtual-element-DOM-R
                      'anything condition-elements
                      true :vertical inherited-for-query)
         query-dom (labels-and-elements-DOM-R
                    condition-elements query-virtual-element-dom
                    true true :horizontal inherited-for-query)
         stack-dom (batch-edit-stack-DOM-R
                    query-item condition-elements store
                    inherited)]
      [:div
       [:div#quit-batch-edit.tool
              [:img {:src "../icons/exit.gif"}]
        [:div.tooltip "exit batch edit (C-Q)"]]
       [:div  {:class "batch-holder"}
        [:div {:class "query-holder tag"}
         [:div {:class "query-indent"}]
         (add-attributes query-dom {:class "query-condition"})]
        [:div {:class "query-result-wrapper"}
         [:div {:class "query-result-indent tag"}]
         (into [:div {:class "batch-stack-wrapper"}] stack-dom)]]])))

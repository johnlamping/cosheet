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

(defn table-conditions-referent
  " All table conditions matching the query."
  [query-item]
  (query-referent (list (item-referent query-item) :row-condition)))

(defn query-items-referent
  [query-item]
  (union-referent
   [(item-referent query-item)
    (top-level-items-referent query-item)
    (table-conditions-referent query-item)]))

(defn selected-batch-referent
  "Return the referent for the selected batch item and everything affected
  by it, given the query item and a referent for everything relevant
  to the query."
  [selected-batch-item query-item query-referent]
  (if (= query-item selected-batch-item)
    query-referent
    (when-let [subject (when selected-batch-item
                         (entity/subject selected-batch-item))]
      (let [subject-referent (selected-batch-referent
                              subject query-item query-referent)]
        (elements-referent (item-referent selected-batch-item)
                           subject-referent)))))

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
                             [(table-conditions-referent query-item)])
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
  [query-item query-elements selected-batch-item store inherited]
  (when selected-batch-item
    (let [selected-referent (selected-batch-referent
                             selected-batch-item query-item
                             (query-items-referent query-item))
          selected-non-header-referent
          (selected-batch-referent
           selected-batch-item query-item
           (top-level-items-referent query-item))
          inherited-for-batch
          (-> inherited
              (assoc :selector-category :batch-stack)
              ;; Make its doms have different keys.
              (update :key-prefix #(conj % :batch-stack)))]
      (expr-let
          [virtual-dom (batch-edit-stack-virtual-DOM-R
                        query-item query-elements inherited-for-batch)
           batch-dom
           (if (= query-item selected-batch-item)
             (let [query-and-rows-referent
                   (union-referent [(item-referent query-item)
                                    (top-level-items-referent query-item)])
                   conditions-referent
                   (table-conditions-referent query-item)
                   inherited
                   (-> inherited-for-batch
                       ;; We use a function for the subject, so that
                       ;; all column headers will be changed, even if
                       ;; there are several matching headers in one table.
                       (assoc
                        :subject-referent
                        (fn
                          ([] (union-referent [query-and-rows-referent
                                               conditions-referent]))
                          ([item]
                           (let [item-ref (item-referent item)]
                             (parallel-union-referent
                              [(exemplar-referent
                                item-ref query-and-rows-referent)
                               (elements-referent
                                item-ref conditions-referent)])))))
                       ;; If the selected item is the whole query, then a
                       ;; delete could remove the whole condition of a
                       ;; table. Have it just remove top level rows, instead.
                       (add-inherited-attribute
                        [#{:content}
                         {:delete
                          {:referent selected-non-header-referent}}]))]
               ;; TODO: Add-twin is a problem here with not knowing
               ;; what template to use.
               (labels-and-elements-DOM-R
                query-elements virtual-dom true true :horizontal inherited))
             (item-DOM-R
              selected-batch-item nil inherited-for-batch
              :referent selected-referent :must-show-label true))
           count-dom
           (call-dependent-on-id
            store nil
            (fn [store]
              (let [total (count (instantiate-to-items
                                  selected-referent store))
                    non-header (count (instantiate-to-items
                                       selected-non-header-referent
                                       store))]
                [:div {:class "batch-query-match-counts"}
                 (str non-header " data matches. "
                      (- total non-header 1) " header matches.")])))]
        [count-dom
         (add-attributes batch-dom {:class "batch-selected"})]))))

(defn batch-edit-DOM-R
  "Return the DOM for batch editing, given the item specifying the query,
  and the item, if any, giving the sub-part of the query to operate on
  in batch mode."
  [query-item selected-batch-item store inherited]
  (let [inherited-for-query
        (-> inherited
            (assoc :selector-category :batch-query
                   :subject-referent (item-referent query-item))
            (update :key-prefix #(conj % :batch-query))
            (add-inherited-attribute
             [(:item-id selected-batch-item) {:class "batch-selected-item"}]))]
    (expr-let
        [condition-elements (semantic-elements-R query-item)
         virtual-dom (virtual-element-DOM-R
                      'anything condition-elements
                      true :vertical inherited-for-query)
         query-dom (labels-and-elements-DOM-R
                    condition-elements virtual-dom
                    true true :horizontal inherited-for-query)
         old-query-dom (item-DOM-R
                        query-item nil inherited-for-query
                        :must-show-empty-label true)
         stack-dom (batch-edit-stack-DOM-R
                    query-item condition-elements selected-batch-item store
                    inherited)]
      [:div {:class "batch-holder"}
       [:div#quit-batch-edit.tool
              [:img {:src "../icons/exit.gif"}]
        [:div.tooltip "exit batch edit (C-Q)"]]
       [:div {:class "query-holder tag"}
        [:div {:class "query-indent"}]
        (add-attributes query-dom {:class "query-condition"})]
       [:div {:class "query-result-wrapper"}
        [:div {:class "query-result-indent tag"}]
        (into [:div {:class "batch-stack-wrapper"}] stack-dom)]])))

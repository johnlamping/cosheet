(ns cosheet.server.batch-edit-render
  (:require (cosheet [store :refer [call-dependent-on-id get-unique-number
                                    current-store]]
                     [entity :as entity]
                     [query :refer [matching-elements matching-items]]
                     [debug :refer [simplify-for-print]]
                     [hiccup-utils :refer [dom-attributes
                                           into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq]])
            (cosheet.server
             [referent :refer [item-referent item-or-exemplar-referent
                               elements-referent query-referent
                               virtual-referent exemplar-referent
                               difference-referent union-referent
                               union-referent-if-needed]]
             [instantiate :refer [instantiate-referent]]
             [hierarchy :refer [replace-hierarchy-leaves-by-nodes
                                hierarchy-node-example-elements
                                hierarchy-node-descendants
                                hierarchy-node-non-immediate-descendant-cover
                                hierarchy-by-labels-R]]
             [render-utils :refer [add-inherited-attribute
                                   virtual-referent-DOM
                                   hierarchy-node-DOM-R
                                   hierarchy-node-items-referent]]
             [model-utils :refer [semantic-elements-R table-header-template
                                  visible-non-labels-R visible-elements-R]]
             [order-utils :refer [order-items-R]]
             [item-render :refer [add-labels-DOM
                                  labels-and-elements-DOM-R
                                  element-hierarchy-child-info
                                  horizontal-label-hierarchy-node-DOM]])))

(defn top-level-items-referent
  "Return a referent to all the top level items that match the query item."
  [query-item]
  (query-referent (list (item-referent query-item) :top-level)))

(defn table-headers-referent
  "All table conditions matching the query."
  [query-item]
  (query-referent (list (item-referent query-item) :row-condition)))

(defn table-header-style-hierarchy
  ;; Return the hierarchy given the root item of a table style header.
  [item]
  (-> (entity/elements item)
      order-items-R
      hierarchy-by-labels-R
      replace-hierarchy-leaves-by-nodes))

(defn horizontal-label-subtree-DOM
  "Generate the dom for a subtree of a table header hierarchy, given
  the dom particular to the node, and doms for all the children."
  [node child-doms function-info inherited]
  (let [is-leaf (empty? child-doms)
        node-dom (horizontal-label-hierarchy-node-DOM
                  node function-info inherited)
        elements-template (cons 'anything
                           (keys (:cumulative-properties node)))
        class (cond-> "column-header tag"
                is-leaf (str " leaf"))]
    (if (empty? child-doms)
      (add-attributes node-dom {:class class})
      [:div {:class class}
       (add-attributes node-dom {:class "with-children"})
       (into [:div {:class "horizontal-label-sequence"}]
             child-doms)])))

(defn horizontal-label-child-info
  "Add :remainder-referent to the function-info, giving a referent to
   all items that match a child of the node that has no properties
   of its own."
  [node function-info inherited]
  (let [[child-info inherited] (element-hierarchy-child-info
                                node function-info inherited)
        excluding-cover (hierarchy-node-non-immediate-descendant-cover node)]
    [(if (empty? excluding-cover)
       (dissoc child-info :remainder-referent)
       (let [subject-referent (:subject-referent inherited)
             item (:item (first (hierarchy-node-descendants node)))]
         (assoc child-info :remainder-referent
                (difference-referent
                 (item-or-exemplar-referent item subject-referent)
                 (union-referent-if-needed (map item-or-exemplar-referent
                                                excluding-cover))))))
     inherited]))

(defn horizontal-label-top-level-subtree-DOM
  "Generate the dom for a top level subtree of a table header hierarchy.
  Inherited describes the column requests."
  [node inherited]
  (hierarchy-node-DOM-R
   node horizontal-label-subtree-DOM element-hierarchy-child-info
   {:top-level true} inherited))

;;; The subject referent should give the match to the rows, the headers,
;;; and the item that specifies the hierarchy.
(defn horizontal-label-DOM
  "Generate DOM for a horizontal layout of the visible elements of the item,
   with their labels shown in a hierarchy above them."
  [item inherited]
  (let [elements (order-items-R (visible-elements-R item))
        hierarchy (-> (hierarchy-by-labels-R elements)
                      replace-hierarchy-leaves-by-nodes)
        doms (map #(horizontal-label-top-level-subtree-DOM % inherited)
                  hierarchy)]
    (into [:div {:class "horizontal-label-sequence"}] doms)))

(defn batch-edit-stack-virtual-DOM-R
  "Return the DOM for a virtual element in the edit stack part of the display,
   that is an element of the query item."
  [query-item store inherited]
  (let [[unique _] (get-unique-number (current-store store))
        invisible `(~unique
                    :invisible
                    :temporary)
        query-virtual-referent (virtual-referent
                                `(~'anything ~invisible)
                                (item-referent query-item))
        headers-virtual-referent (virtual-referent
                                  (concat table-header-template [invisible])
                                  (table-headers-referent query-item))
        matches-virtual-referent (virtual-referent
                                  `("" ~invisible)
                                  (top-level-items-referent query-item))
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
  [selector-item store inherited]
  (let [top-level-matches-referent (top-level-items-referent selector-item)
        table-header-matches-referent (table-headers-referent selector-item)
        matches-referent (union-referent
                          [(item-referent selector-item)
                           top-level-matches-referent
                           table-header-matches-referent])
        inherited-for-batch
        (-> inherited
            (assoc :match-all true)
            ;; Make its doms have different keys.
            (update :key-prefix #(conj % :batch-stack)))]
    (expr-let
        [virtual-dom (batch-edit-stack-virtual-DOM-R
                      selector-item store inherited-for-batch)
         ;; We need to take the current versions of the query elements,
         ;; as :match-all only works with immutable items.
         current-query-elements
         (expr-seq map #(entity/updating-call-with-immutable % identity)
                   (semantic-elements-R selector-item))
         batch-dom
         (let [inherited
               (-> inherited-for-batch
                   (assoc :subject-referent matches-referent
                          :template 'anything)
                   ;; TODO: This should not be necessary any more,
                   ;;       as the consistency check should catch it.
                   ;; If the selected item is the whole query, then a
                   ;; delete could remove the whole condition of a
                   ;; table. Have it just remove top level rows, instead.
                   (add-inherited-attribute
                    [#{:content}
                     {:delete {:referent top-level-matches-referent}}]))]
           ;; TODO: Add-twin is a problem here with not knowing
           ;; what template to use.
           (labels-and-elements-DOM-R current-query-elements virtual-dom
                                      true true :horizontal inherited))
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
  [selector-item store inherited]
  (let [inherited-for-query
        (-> inherited
            (assoc :subject-referent (item-referent selector-item))
            (update :key-prefix #(conj % :batch-selector)))]
    (expr-let
        [stack-dom (batch-edit-stack-DOM-R selector-item store inherited)]
      [:div
       [:div#quit-batch-edit.tool
              [:img {:src "../icons/table_view.gif"}]
        [:div.tooltip "table view (C-Q)"]]
       [:div {:class "query-result-wrapper"}
        (into [:div {:class "batch-stack-wrapper"}] stack-dom)]])))

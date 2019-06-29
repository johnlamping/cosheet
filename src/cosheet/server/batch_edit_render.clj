(ns cosheet.server.batch-edit-render
  (:require (cosheet [store :refer [call-dependent-on-id get-unique-number
                                    current-store]]
                     [entity :as entity]
                     [query :refer [matching-elements matching-items
                                    not-query extended-by?]]
                     [debug :refer [simplify-for-print]]
                     [hiccup-utils :refer [dom-attributes
                                           into-attributes add-attributes]]
                     [expression-manager :refer [current-value]]
                     [expression :refer [expr expr-let expr-seq expr-filter]])
            (cosheet.server
             [referent :refer [item-referent item-or-exemplar-referent
                               element-restriction-referent
                               elements-referent query-referent
                               virtual-referent exemplar-referent
                               difference-referent union-referent
                               union-referent-if-needed]]
             [instantiate :refer [instantiate-referent]]
             [hierarchy :refer [replace-hierarchy-leaves-by-nodes
                                hierarchy-node-example-elements
                                hierarchy-node-descendants
                                hierarchy-node-logical-leaves
                                hierarchy-node-non-immediate-descendant-cover
                                hierarchy-by-labels-R]]
             [render-utils :refer [add-inherited-attribute
                                   virtual-referent-DOM
                                   hierarchy-node-DOM-R
                                   hierarchy-node-items-referent]]
             [model-utils :refer [semantic-elements-R]]
             [order-utils :refer [order-items-R]]
             [item-render :refer [add-labels-DOM
                                  item-DOM-R
                                  labels-and-elements-DOM-R
                                  element-hierarchy-child-info
                                  horizontal-label-hierarchy-node-DOM]])))

(defn top-level-items-referent
  "Return a referent to all the top level items that match the query item."
  [query-item]
  (query-referent (list (item-referent query-item) :top-level)))

(defn table-headers-referent
  "Return a union of a referent to the table condition, and to its columns.
   They are both stored in the same item, but we use a union, so that
   an exemplar referent for an element will return one of each."
  [query-item]
  (let [row-ref (query-referent
                       (list (item-referent query-item) :row-condition))]
    (union-referent
     [(element-restriction-referent '(nil :column) row-ref)
      (element-restriction-referent `(nil ~(not-query :column)) row-ref)])))

(defn table-header-style-hierarchy
  ;; Return the hierarchy given the root item of a table style header.
  [item]
  (-> (entity/elements item)
      order-items-R
      hierarchy-by-labels-R
      replace-hierarchy-leaves-by-nodes))

(defn horizontal-label-subtree-DOM
  "Generate the dom for a subtree of a table header hierarchy, given
  the doms for all the children."
  [node child-doms function-info inherited]
  (let [is-leaf (empty? child-doms)
        node-dom (horizontal-label-hierarchy-node-DOM
                  node function-info inherited)
        class (cond-> "column-header tag"
                is-leaf (str " leaf"))]
    (if (empty? child-doms)
      (add-attributes node-dom {:class class})
      [:div {:class class}
       (add-attributes node-dom {:class "with-children"})
       (into [:div {:class "horizontal-label-sequence"}]
             child-doms)])))

(defn horizontal-label-hierarchy-node-referent-f
  "Return the referent to use for the the node,
   given parent-excluding-cover, the excluding cover of its parent."
  [parent-excluding-cover node inherited]
  ;; We want to match the same items a column header would match,
  ;; which means everything, except that a column selector without its own
  ;; properties is competed with by any children.
  (let [subject-ref (:subject-referent inherited) ]
    (if (and (empty? (:properties node)) 
             (not (empty? parent-excluding-cover)))
      (difference-referent
       (elements-referent (:item (first (:leaves node))) subject-ref)
       (union-referent-if-needed
        (map #(elements-referent (:item %) subject-ref)
             parent-excluding-cover)))
      (hierarchy-node-items-referent
       node (assoc inherited :match-multiple :all)))))

(defn horizontal-label-child-info
  "Update :referent-f in the function-info, giving how to compute the
   referent of child nodes."
  [node function-info inherited]
  (let [[child-info inherited] (element-hierarchy-child-info
                                node function-info inherited)
        excluding-cover (hierarchy-node-non-immediate-descendant-cover node)]
    [(assoc child-info :referent-f
            (partial horizontal-label-hierarchy-node-referent-f
                     excluding-cover))
     inherited]))

(defn horizontal-label-top-level-subtree-DOM
  "Generate the dom for a top level subtree of a table header hierarchy.
  Inherited describes the column requests."
  [node inherited]
  (hierarchy-node-DOM-R
   node horizontal-label-subtree-DOM horizontal-label-child-info
   {:top-level true
    :referent-f (partial horizontal-label-hierarchy-node-referent-f nil)}
   inherited))

;;; The subject referent should give the match to the rows, to the headers,
;;; and to the items that specify the pattern.
(defn horizontal-label-DOM-R
  "Generate DOM for a horizontal layout of the semantic elements of the item,
   with their labels shown in a hierarchy above them."
  [item inherited]
  (entity/updating-with-immutable
   [immutable-item item]
   (let [elements (order-items-R (semantic-elements-R immutable-item))
         hierarchy (-> (hierarchy-by-labels-R elements)
                       replace-hierarchy-leaves-by-nodes)
         doms (map #(horizontal-label-top-level-subtree-DOM % inherited)
                   hierarchy)]
     (into [:div {:class "horizontal-label-sequence"}] doms))))

(defn batch-row-selector-virtual-DOM-R
  "Return the DOM for a virtual element in the row selector part of the display,
   that is an element of the query item."
  ;; TODO: Take an optional elements item, and make a copy of that,
  ;;       and of table headers.
  [query-item store inherited]
  (let [[unique _] (get-unique-number (current-store store))
        subject-referent (union-referent
                          [(item-referent query-item)
                           (table-headers-referent query-item)
                           (top-level-items-referent query-item)])
        dom (virtual-referent-DOM
             (virtual-referent '(anything (anything :tag)) subject-referent)
             inherited)
        tag-dom (add-attributes
                 (virtual-referent-DOM
                  (virtual-referent
                      '(anything :tag)
                      (virtual-referent 'anything subject-referent))
                  (-> inherited
                      (update :key-prefix #(conj % :tags))
                      (assoc :select-pattern (conj (:key-prefix inherited)
                                                   :label [:pattern]))))
                 {:class "tag"})]
    (add-labels-DOM tag-dom dom :vertical)))

(defn batch-row-selector-DOM-R
  "Return the dom row selector."
  [row-selector store inherited]
  (expr-let
      [virtual-dom (batch-row-selector-virtual-DOM-R
                    row-selector store inherited)
       ;; We need to take the current versions of the query elements,
       ;; as :match-multiple only works with immutable items.
       current-query-elements
       (expr-seq map #(entity/updating-call-with-immutable % identity)
                 (semantic-elements-R row-selector))
       batch-dom
       ;; TODO: Add-twin is a problem here with not knowing
       ;; what template to use.
       (labels-and-elements-DOM-R current-query-elements virtual-dom
                                  true true :horizontal inherited)]
    batch-dom))

(defn count-DOM-R
  "Return DOM describing the number of matches."
  [row-selector store]
  (call-dependent-on-id
   store nil
   (fn [store]
     (let [header (count (instantiate-referent
                          (table-headers-referent row-selector) store))
           non-header (count (instantiate-referent
                              (top-level-items-referent row-selector) store))]
       [:div {:class "batch-query-match-counts"}
        (str non-header " row matches.  "
             header " table header matches.")]))))

(defn batch-edit-DOM-R
  "Return the DOM for batch editing, given the item specifying the query,
  and the item, if any, giving the sub-part of the query to operate on
  in batch mode."
  [selector-items store inherited]
  (expr-let [row-selector (expr first
                            (expr-filter
                             #(extended-by? '(nil :batch-row-selector) %)
                             selector-items))
             elements-item (expr first
                             (expr-filter
                              #(extended-by? '(nil :batch-elements) %)
                              selector-items))]
    (let [top-level-matches-referent (top-level-items-referent row-selector)
          table-header-matches-referent (table-headers-referent row-selector)
          matches-referent (union-referent
                            (cond-> [;; The row selector must be first
                                     ;; so selection on new content will work.
                                     (item-referent row-selector)
                                     top-level-matches-referent
                                     table-header-matches-referent]
                              elements-item
                              (conj (item-referent elements-item))))
          inherited (-> inherited
                        (assoc :subject-referent matches-referent
                               :template "")
                        ;; Make sure our doms have unique keys.
                        (update :key-prefix #(conj % :batch)))]
      (expr-let
          [count-dom (count-DOM-R row-selector store)
           row-dom (batch-row-selector-DOM-R
                    row-selector store inherited)
           inner-dom (cond
                       elements-item
                       ;; For elements of our items, we want to match all
                       ;; exclusively matched elements.
                       (let [inherited (assoc inherited :match-multiple
                                              :exclusive)]
                         (expr-let [elements-dom (horizontal-label-DOM-R
                                                  elements-item inherited)]
                           [:div {:class "batch-stack-wrapper"}
                            count-dom
                            [:div {:class "horizontal-tags-element batch-stack"}
                             row-dom
                             [:div {:class "batch-stack"} elements-dom]]]))
                       true
                       [:div {:class "batch-stack-wrapper"}
                        count-dom
                        (add-attributes row-dom {:class "batch-stack"})])]
        [:div
         [:div#quit-batch-edit.tool
          [:img {:src "../icons/table_view.gif"}]
          [:div.tooltip "table view (C-Q)"]]
         [:div {:class "query-result-wrapper"} inner-dom]]))))

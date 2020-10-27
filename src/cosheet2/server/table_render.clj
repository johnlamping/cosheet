(ns cosheet2.server.table-render
  (:require (cosheet2 [utils :refer [replace-in-seqs multiset separate-by
                                     add-elements-to-entity-list remove-first]]
                      [entity :refer [subject content label->elements
                                      description->entity StoredEntity]]
                      [query :refer [matching-elements matching-items
                                     extended-by?]]
                      [query-calculator :refer [matching-items-R]]
                      [debug :refer [simplify-for-print]]
                      [hiccup-utils :refer [dom-attributes
                                            into-attributes add-attributes]]
                      [expression :refer [expr expr-let expr-seq]]
                      [canonical :refer [canonicalize-list canonical-to-list
                                         canonical-set-to-list
                                         common-canonical-multisets]])
            (cosheet2.server
             [hierarchy :refer [hierarchy-node? hierarchy-node-descendants
                                replace-hierarchy-leaves-by-nodes
                                hierarchy-node-leaves
                                hierarchy-node-next-level
                                hierarchy-nodes-extent
                                hierarchy-by-labels
                                hierarchy-node-example-elements
                                hierarchy-node-non-immediate-descendant-cover]]
             [order-utils :refer [ordered-ids-R]]
             [model-utils :refer [semantic-to-list
                                  semantic-elements semantic-non-label-elements
                                  table-header-template
                                  pattern-to-query query-to-template]]
             [render-utils :refer [make-component
                                   hierarchy-node-DOM]]
             [item-render :refer [virtual-DOM virtual-entity-and-label-DOM
                                  label-stack-DOM
                                  item-content-and-non-label-elements-DOM
                                  item-content-DOM
                                  labels-and-elements-DOM
                                  non-label-entities-DOM
                                  horizontal-label-hierarchy-node-DOM]]
             [action-data :refer [get-item-or-exemplar-action-data-for-ids
                                  get-column-action-data]])))

;;; The condition elements of a table are its semantic elements
;;; that are not column headers.
(defn table-condition-elements [row-condition-item]
  (expr-let
      [semantic-elements (semantic-elements row-condition-item)
       column-elements (label->elements row-condition-item :column)]
    (remove (set column-elements) semantic-elements)))

(comment

;;; The next two functions are here, rather than in actions, because they
;;; know about the structure of tables.
  (defn batch-edit-containment-path
    "Given an item, return the sequence of items that contain it, up to
  the highest level that is reflected in the batch edit selectors,
  starting from the outermost container. Also return whether the
  outermost item should appear in the batch elements (as opposed to
  in the row condition.
  Return nil if the item doesn't determine a batch edit."
    [immutable-item]
    (when immutable-item
      (loop [item immutable-item
             ;; The sequence of containing items up to and including
             ;; the current item.
             containing-items (list immutable-item)]
        (when-let [parent-item (subject item)]
          (cond
            (seq (matching-elements :top-level parent-item))
            [containing-items true] 
            (seq (matching-elements :row-condition parent-item))
            [containing-items (when (seq (matching-elements :column item)) true)]
            true
            (recur parent-item (cons parent-item containing-items)))))))

  (defn batch-edit-selectors
    "Given the row condition item, and a (possibly empty) list of
  element items, return a list containing the list form of a batch row
  selector, tagged with :batch-row-selector, and, if there are any
  element items, by a list tagged :batch-elements, where each element is
  the list form of each of the element items, and given :order
  elements."
    [immutable-row-condition-item immutable-element-items]
    ;; We walk up containing items, until we find an item that is either
    ;; column condition, an element of a row, or the entire row condition.
    ;; Then we return the appropriate thing for that situation.
    (let [row-condition
          (concat '(anything)
                  (map immutable-semantic-to-list
                       (table-condition-elements-R immutable-row-condition-item))
                  [:batch-row-selector])
          selectors (if (seq immutable-element-items)
                      [row-condition
                       (concat
                        '(anything)
                        (map immutable-semantic-to-list immutable-element-items)
                        [:batch-elements])]
                      [row-condition])]
      (map (fn [selector] (-> selector
                              add-order-elements
                              (replace-in-seqs 'anything-immutable 'anything)
                              (concat [:batch-selector :selector])
                              (#(apply list %))))
           selectors)))

  

  (defn set-batch-edit-ids
    "Set the batch edit ids in inherited to be the ones appropriate for
   items that span this node or that don't have any properties of their own."
    [node inherited]
    (let [cleaned (remove-inherited-attribute inherited :batch-edit-ids)]
      (if (and (empty? (:properties node))
               (not= (seq (:attributes cleaned)) (seq (:attributes inherited))))
        ;; We have no hierarchy properties of our own, so we refer to whatever
        ;; our sibling column's don't, and we need to keep the
        ;; batch-edit-ids that our parent set, if there was one.
        inherited
        (-> cleaned
            (add-inherited-attribute
             [#{:label :element :recursive :optional} #{:content}
              {:batch-edit-ids (map #(:item-id (:item %))
                                    (hierarchy-node-descendants node))}])))))

 

  (defn table-row-DOM-R
    "Generate dom for a table row."
    [row-item row-key new-row-template column-descriptions inherited]
    (let [row-referent (item-or-exemplar-referent
                        row-item (:subject-referent inherited))
          inherited (-> inherited
                        (assoc :key-prefix row-key
                               :subject-referent row-referent)
                        (add-inherited-attribute
                         [#{:label :element :recursive :optional} #{:content}
                          {:row {:referent row-referent
                                 :key row-key
                                 :template new-row-template}}])
                        (update :priority inc))]
      ;; We use updating-with-immutable so that we don't track
      ;; every dependency in the row. The downside is that we have to
      ;; reexamine the whole row when any part of it changes. (Much of
      ;; that still won't lead to recomputation, as we will reuse
      ;; unchanged parts.)
      (expr-let [cells (entity/updating-with-immutable
                        [immutable row-item]
                        (map #(table-cell-DOM-R immutable % inherited)
                             column-descriptions))]
        (into [:div {}] cells))))

  (defn table-row-DOM-component
    "Generate a component for a table row."
    [row-item new-row-template column-descriptions inherited]
    (let [row-key (conj (:key-prefix inherited) (:item-id row-item))]
      (make-component
       {:key row-key :class "table-row"}
       [table-row-DOM-R
        row-item row-key new-row-template column-descriptions inherited])))

  (defn table-virtual-row-cell-DOM
    "Return the dom for one cell of a virtual row of a table."
    [adjacent-referent
     {:keys [column-id template]} ;; A column header description
     inherited]
    (let [select (conj (vec (butlast (:key-prefix inherited)))
                       [:pattern :subject] ;; The new row's id
                       column-id
                       [:pattern])
          inherited (assoc inherited
                           :key-prefix (conj (:key-prefix inherited) column-id)
                           :template template
                           :select-pattern select)]
      (add-attributes
       (virtual-element-DOM adjacent-referent :after inherited)
       {:class "table-cell has-border"})))

  (defn table-virtual-row-DOM
    "Generate dom for a table's virtual row."
    [row-key new-row-template adjacent-referent column-descriptions inherited]
    (let [inherited (-> inherited
                        (assoc :key-prefix row-key)
                        (update :subject-referent
                                #(virtual-referent
                                  new-row-template % adjacent-referent)))
          cells (map #(table-virtual-row-cell-DOM
                       adjacent-referent % inherited) column-descriptions)]
      (into [:div {}] cells)))

  (defn table-virtual-row-DOM-component
    "Generate a component for a table row."
    [new-row-template adjacent-referent column-descriptions inherited]
    (let [row-key (conj (:key-prefix inherited) :virtualRow)]
      (make-component
       {:key row-key :class "table-row"}
       [table-virtual-row-DOM
        row-key new-row-template adjacent-referent column-descriptions
        inherited])))

  (defn table-DOM-R
    "Return a hiccup representation of DOM, with the given internal key,
  describing a table."
    ;; The following elements of table-item describe the table:
    ;;  :row-condition  The content is an item whose list form gives the
    ;;                  requirements for an item to appear as a row.
    ;;                  It is marked as :selector.
    ;;                  It has additional elements tagged with:
    ;;     :column  The semantics gives the requirements for an element
    ;;              of a row to appear in this column. Generally, the content
    ;;              will be the keyword 'anything, to indicate no constraint
    ;;              on the content of an element in the row, without
    ;;              breaking the rule that the database doesn't contain
    ;;              nil. The exception is the special content :other,
    ;;              which means to show everything not shown in any
    ;;              other column. (:other not yet implemented.)
    ;; TODO: Add the "other" column if a table requests it.
    [table-item inherited]
    (println "Generating DOM for table" (simplify-for-print table-item))
    (assert (satisfies? StoredEntity table-item))
    (let [store (:store table-item)
          table-referent (item-or-exemplar-referent
                          table-item (:subject-referent inherited))
          table-key (conj (:key-prefix inherited) table-referent)
          inherited (assoc inherited :key-prefix table-key)]
      (expr-let [immutable-item (entity/updating-with-immutable
                                 [immutable table-item]
                                 immutable)]
        ;; Don't do anything if we don't yet have the table information filled in.
        (when-let [row-condition-item (first (label->elements
                                              immutable-item :row-condition))]
          (let [headers-inherited (update
                                   (assoc
                                    inherited
                                    :subject-referent (item-referent
                                                       row-condition-item)
                                    :template table-header-template)
                                   :priority inc)
                columns (order-items-R
                         (label->elements row-condition-item :column))
                hierarchy (hierarchy-by-labels columns)
                headers (table-header-DOM
                         hierarchy headers-inherited)
                condition-dom (table-top-DOM-R
                               row-condition-item inherited)
                column-descriptions (mapcat
                                     table-hierarchy-node-column-descriptions
                                     hierarchy)
                new-column-template '(anything :label)]
            (expr-let
                [[row-template row-items] (row-template-and-items-R
                                           store row-condition-item)]
              (let [virtual-template (virtual-referent
                                      new-column-template
                                      (item-referent row-condition-item)
                                      (item-referent (or (last columns)
                                                         table-item)))
                    virtual-column-description {:column-id :virtualColumn
                                                :template virtual-template
                                                :exclusions nil}
                    rows (map #(table-row-DOM-component
                                % row-template (concat
                                                column-descriptions
                                                [virtual-column-description])
                                (update inherited :priority (partial + 2)))
                              row-items)
                    virtual-row (table-virtual-row-DOM-component
                                 row-template
                                 (item-referent (or (last row-items) table-item))
                                 column-descriptions inherited)]
                [:div {:class "table"}
                 condition-dom
                 [:div {:class "query-result-wrapper"}
                  [:div {:class "query-result-indent label"}]
                  [:div {:class "table-main"}
                   headers
                   (into [:div {:class "table-rows"}]
                         (concat rows [virtual-row]))]]])))))))
  )

(defn table-virtual-column-cell-DOM
  [specification]
  (add-attributes
   (virtual-DOM specification {})
   {:class "table-cell virtual-column has-border"}))

(defn get-table-cell-DOM-rendering-data
  [specification mutable-store]
  [])

(defmethod print-method
  cosheet2.server.table_render$get_table_cell_DOM_rendering_data
  [v ^java.io.Writer w]
  (.write w "cell-RD"))

(defn render-table-cell-DOM
  "We need to put each table cell in a component, so its column-id
  can be included in its client id. Otherwise, if several columns show the
  same item, we could have the same client id for both."
  [{:keys [items] :as specification}]
  (let [spec (dissoc specification :items :relative-id)
        dom (if (empty? items)
              ;; TODO: Get our left neighbor as an arg, and pass it
              ;; in the sibling for the virtual dom.
              (virtual-DOM (assoc specification :relative-id
                                  :virtual)
                           {})
              (non-label-entities-DOM
               items (:template specification) false :vertical specification))]
    (add-attributes dom {:class "table-cell has-border"})))

(defmethod print-method
  cosheet2.server.table_render$render_table_cell_DOM
  [v ^java.io.Writer w]
  (.write w "cell-DOM"))

(defn table-cell-DOM
  "Return the dom for one cell of a table, given its column description."
  [row-item
   {:keys [column-id query template exclusions] :as column-description}
   specification]
    (let [spec (assoc specification :template template)]
      (if (= column-id :virtualColumn)
        (table-virtual-column-cell-DOM spec)
        (let [matches (matching-elements query row-item)
              items (if exclusions
                         (let [do-not-show (map #(matching-elements % row-item)
                                                exclusions)]
                           (seq (clojure.set/difference
                               (set matches)
                               (set (apply concat do-not-show)))))
                         matches)]
          (make-component
           (assoc specification
                  :render-dom render-table-cell-DOM
                  :get-rendering-data get-table-cell-DOM-rendering-data
                  :relative-id column-id
                  :items items
                  :template template))))))

(defn query-for-hierarchy-leaf
  "Given a hierarchy leaf, and the node it is from,
  return the query that items in cells of the column must satisfy.
  (Not worrying about exclusions.)"
  [leaf]
  (pattern-to-query (semantic-to-list (:item leaf))))

(defn table-hierarchy-node-column-descriptions
  "Given a hierarchy node, for each column under the node,
  return a map:
       :column-id Id that identifies the column.
                  Typically the id of the column item.
           :query Query that each element of the column must satisfy.
                  For a virtual column, this will not be present.
        :template Template for new items in the column.
      :exclusions Seq of conditions that elements must not satisfy."
  [node]
  (mapcat (fn [node-or-element]
            (if (hierarchy-node? node-or-element)
              (table-hierarchy-node-column-descriptions node-or-element)
              (let [query (query-for-hierarchy-leaf node-or-element)]
                [{:column-id (:item-id (:item node-or-element))
                  :query query
                  :template (query-to-template query)
                  :exclusions (table-hierarchy-node-exclusions node)}])))
          (hierarchy-node-next-level node)))

(defn row-template
  "Given the item giving the row condition, return the template for a row."
  [row-condition-item]
  (let [condition-elements (table-condition-elements row-condition-item)
        elems-list (map semantic-to-list condition-elements) ] 
    (concat '(anything) elems-list [:top-level])))

(defn row-items-R
  "Given the item giving the row condition, return the template for a row
  and the items for the rows, in order."
  [store template]
  (let [row-query (pattern-to-query row-template)]
    (expr-let [items (matching-items-R row-query store)
               ids (map :item-id items)]
      (ordered-ids-R (ids store)))))

(defn is-label-template?
  "Return true if the template describes a label."
  [template]
  (and (sequential? template)
       (some #(= (content %) :label)
             (rest template))))

(defn table-hierarchy-node-exclusions
  "Given a hierarchy node, return a seq of conditions that immediate
  elements of the node must not satisfy, because they are covered
  by sub-nodes."
  [node]
  (map #(pattern-to-query (semantic-to-list (:item %)))
       (hierarchy-node-non-immediate-descendant-cover node)))


(defn table-header-node-specification
  "Return the specification to use for a DOM that is part of a table header."
  [header-id node]
  (let [descendant-ids (map #(:item-id (:item %))
                            (hierarchy-node-descendants node))]
    {:get-column-action-data [get-column-action-data header-id descendant-ids]
     :width (* 0.75 (count descendant-ids))
     ;; Tell add-twin and delete that they must not add or remove a column.
     :template :singular}))

(defn table-header-subtree-DOM
  "Generate the dom for a subtree of a table header hierarchy, given
  the doms for all the children."
  [header-id node child-doms specification]
  (let [spec (into specification
                   (table-header-node-specification header-id node))
        node-dom (horizontal-label-hierarchy-node-DOM
                  node spec)
        is-leaf (empty? child-doms)
        elements-template '(anything :label)
        is-label (is-label-template? elements-template)
        class (cond-> "column-header"
                is-leaf (str " leaf"))]
    (if (empty? child-doms)
      (add-attributes node-dom {:class class})
      [:div {:class (cond-> class
                      is-label (str " label"))}
       (add-attributes node-dom {:class "with-children"})
       (into [:div {:class "column-header-sequence"}]
             child-doms)])))

(defn table-header-top-level-subtree-DOM
  "Generate the dom for a top level subtree of a table header hierarchy.
  Inherited describes the column requests."
  [header-id node]
  (hierarchy-node-DOM node
                      (partial table-header-subtree-DOM header-id)
                      (fn [sub-node spec] (dissoc spec :top-level))
                      {:top-level true}))

(defn table-virtual-column-header-DOM
  [hierarchy]
  (let [spec {:relative-id :virtual-column
              :template table-header-template
              :class "column-header virtual-column"}]
    (if (empty? hierarchy)
      (virtual-entity-and-label-DOM
       spec :horizontal {})
      (let [last-column (last (hierarchy-node-descendants (last hierarchy)))
            last-column-id (:item-id (:item last-column))]
        (virtual-entity-and-label-DOM
         (assoc spec :get-action-data [get-item-or-exemplar-action-data-for-ids
                                       [last-column-id]])
         :horizontal
         {:sibling true})))))

(defn table-header-DOM
  "Generate DOM for column headers given the hierarchy.
  The column will contain those elements of the rows that match the templates
  in the hierarchy."
    [header-id hierarchy]
  (let [hierarchy (replace-hierarchy-leaves-by-nodes hierarchy)
        columns (map #(table-header-top-level-subtree-DOM header-id %)
                     hierarchy)
        virtual-header (table-virtual-column-header-DOM hierarchy)]
    (into [:div {:class "column-header-sequence"}]
          (concat columns [virtual-header]))))

(defn table-condition-DOM
  "Return a hiccup representation for the top of a table, the part that
  holds its condition. The relative-id should be for the row-condition"
  [{:keys [relative-id]} store]
  (let [row-condition-entity (description->entity relative-id store)
        condition-elements (table-condition-elements row-condition-entity)
        spec-down {:template '(anything)}
        virtual-dom
        (virtual-entity-and-label-DOM
         {:template 'anything
          :relative-id :virtual}
         :vertical
         {})
        dom (labels-and-elements-DOM
             condition-elements virtual-dom
             true true :horizontal spec-down)]
    [:div {:class "query-holder label"}
     [:div {:class "query-indent label"}]
     (add-attributes dom {:class "query-condition"})]))

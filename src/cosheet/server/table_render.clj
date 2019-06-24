(ns cosheet.server.table-render
  (:require (cosheet [utils :refer [replace-in-seqs multiset separate-by
                                    add-elements-to-entity-list remove-first]]
                     [entity :as entity]
                     [query :refer [matching-elements matching-items
                                    best-matching-term extended-by?]]
                     [debug :refer [simplify-for-print]]
                     [hiccup-utils :refer [dom-attributes
                                           into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq]]
                     [canonical :refer [canonicalize-list canonical-to-list
                                        canonical-set-to-list
                                        common-canonical-multisets]])
            (cosheet.server
             [referent :refer [item-referent exemplar-referent
                               query-referent referent? item-referent?
                               union-referent-if-needed union-referent
                               virtual-referent item-or-exemplar-referent]]
             [hierarchy :refer [hierarchy-node? hierarchy-node-descendants
                                replace-hierarchy-leaves-by-nodes
                                hierarchy-node-leaves
                                hierarchy-node-next-level
                                hierarchy-nodes-extent
                                hierarchy-by-labels-R
                                hierarchy-node-example-elements
                                hierarchy-node-non-immediate-descendant-cover]]
             [order-utils :refer [order-items-R add-order-elements]]
             [model-utils :refer [immutable-visible-to-list
                                  semantic-to-list-R
                                  immutable-semantic-to-list
                                  semantic-elements-R
                                  visible-elements-R visible-non-labels-R
                                  visible-to-list-R
                                  table-header-template
                                  pattern-to-query query-to-template]]
             [render-utils :refer [make-component virtual-element-DOM
                                   transform-inherited-for-children
                                   transform-inherited-for-labels
                                   add-inherited-attribute
                                   remove-inherited-attribute
                                   hierarchy-node-items-referent
                                   hierarchy-node-items-referents
                                   hierarchy-last-item-referent
                                   hierarchy-node-DOM-R]]
             [item-render :refer [elements-DOM-R virtual-element-with-label-DOM
                                  labels-and-elements-DOM-R
                                  label-stack-DOM-R
                                  item-content-and-non-label-elements-DOM-R
                                  item-content-DOM
                                  horizontal-label-hierarchy-node-DOM
                                  element-hierarchy-child-info]])))

;;; The condition elements of a table are its visible elements
;;; that are not column headers.
(defn table-condition-elements-R [row-condition-item]
  (expr-let
      [visible-elements (visible-elements-R row-condition-item)
       column-elements (entity/label->elements row-condition-item :column)]
    (remove (set column-elements) visible-elements)))

;;; This function is here, rather than in actions, because it knows about
;;; the structure of tables.
(defn batch-edit-selectors
  "Given either an item, or a list of batch-edit-items from a DOM, and
  the table row condition, return the list form of the appropriate
  batch selectors.  These will always inclde the row condition, tagged
  with :batch-row-selector.  For a column header, batch-edit-items
  will be present, and will be elements of an item tagged
  :batch-elements. For an element in a cell, It should be an element
  of an item tagged :batch-elements."
  [immutable-item batch-edit-items immutable-row-condition-item]
  ;; We walk up containing items, until we find an item that is either
  ;; column condition, an element of a row, or the entire row condition.
  ;; Then we return the appropriate thing for that situation.
  (let [row-condition
        (concat '(anything)
                (map immutable-visible-to-list
                     (table-condition-elements-R immutable-row-condition-item))
                [:batch-row-selector])
        selectors
        (if (seq batch-edit-items)
          ;; We are told what items to show.
          [row-condition
               (concat '(anything)
                       (map immutable-visible-to-list batch-edit-items)
                       [:batch-elements])]
          (loop [item immutable-item]
            (let [parent-item (entity/subject item)]
              (cond
                (seq (matching-elements :top-level parent-item))
                ;; We have reached a top level row. Add the item as
                ;; an element of the rows.
                [row-condition
                 `(~'anything
                   ~(immutable-visible-to-list item) :batch-elements)]
                (seq (matching-elements :row-condition item))
                ;; The item is part of the row condition. We just show that.
                [row-condition]
                parent-item
                (recur parent-item)))))]
    (map (fn [selector] (-> selector
                            add-order-elements
                            (replace-in-seqs 'anything-immutable 'anything)
                            (concat [:batch-selector :selector])
                            (#(apply list %))))
         selectors)))

(defn is-tag-template?
  "Return true if the template describes a label."
  [template]
  (some #(or (= (if (sequential? %) (first %) %) :tag))
        template))

(defn table-hierarchy-node-exclusions
  "Given a hierarchy node, return a seq of conditions that immediate
  elements of the node must not satisfy, because they are covered
  by sub-nodes."
  [node]
  (map #(pattern-to-query (immutable-semantic-to-list (:item %)))
       (hierarchy-node-non-immediate-descendant-cover node)))

(defn new-header-template
  "Return the template for a new header. new-elements-template gives
  the template for new elements in the header, while inherited gives
  the environment of the header."
  [new-elements-template inherited]
  (apply list (concat (:template inherited)
                      [(cons '??? (rest new-elements-template))])))

(defn target-for-header-add-column-command
  "Return the target for an add column command. elements-template gives
  the template for new elements in the header, while inherited gives
  the environment of the header."
  [node elements-template inherited]
  (assert (:template inherited))
  (let [subject-ref (:subject-referent inherited)
        ;; There is an item for the new column, which has an element
        ;; satisfying the element template. We want to select that
        ;; element.
        adjacent-referents (hierarchy-node-items-referents
                            node inherited)
        new-column-ref (virtual-referent (:template inherited)
                                         subject-ref adjacent-referents
                                         :position :after)
        new-element-ref (virtual-referent (cons '??? (rest elements-template))
                                          new-column-ref
                                          nil
                                          :position :after)
        select-pattern (conj (:key-prefix inherited)
                             [:pattern :subject] [:pattern])]
    {:referent new-element-ref
     :select-pattern select-pattern}))

(defn attributes-for-header-add-column-command
  "Return attributes for an add column command, given the column
  request items that gave rise to the column. elements-template gives
  the template for new elements, while inherited gives the environment
  of the header."
  [node elements-template inherited]
  {:add-column (target-for-header-add-column-command
                node elements-template inherited)})

;;; TODO: Currently, this just returns a generic tag. Fix it to be better.
;;; TODO: We need to also handle the case where there is no new element,
;;;       but a content with a new value.
(defn table-header-element-template
  "Return a template for one new element of a table header (not the entire
   header). The element should include what is common to the specified
   elements, which should be in canonical list form. Always returns a seq."
  [canonical-elements]
  (or
   ;; We always return this, for now.
   '(anything :tag)
   (if (seq canonical-elements)
     (or (when (every? sequential? canonical-elements)
           (let [firsts-sub-elements (second (first canonical-elements))
                 remainder-sub-elements (map second (rest canonical-elements))]
             (if (empty? remainder-sub-elements)
               ;; If there is only one current element, then the next one
               ;; only copies whether or not it has a tag.
               (if (contains? firsts-sub-elements :tag)
                 '(anything :tag)
                 '(anything))
               (let [common (reduce common-canonical-multisets
                                    firsts-sub-elements
                                    remainder-sub-elements)]
                 (if (not (empty? common))
                   (cons 'anything (canonical-set-to-list common))
                   '(anything))))))
         '(anything))
     '(anything :tag))))

(defn set-batch-edit-ids
  "Set the batch edit ids in inherited to be the ones appropriate for
   items that span this node or that don't have any properties of their own."
  [node inherited]
  (let [cleaned (remove-inherited-attribute inherited :batch-edit-id)]
    (if (and (empty? (:properties node))
             (not= (seq (:attributes cleaned)) (seq (:attributes inherited))))
      ;; We have no hierarchy properties of our own, so we refer to whatever
      ;; our sibling column's don't, and we need to keep the
      ;; batch-edit-id that our parent set, if there was one.
      inherited
      (-> cleaned
          (add-inherited-attribute
           [#{:label :element :recursive :optional} #{:content}
            {:batch-edit-ids (map #(:item-id (:item %))
                                  (hierarchy-node-descendants node))}])))))

(defn table-header-properties-inherited
  "Return the inherited to use for the properties of a table header."
  [node inherited]
  (let [descendants (hierarchy-node-descendants node)
        descendants-referent (hierarchy-node-items-referent node inherited)
        item (:item (first descendants))
        elements-template (table-header-element-template
                           ;; TODO: The keys below means that multiplicites of
                           ;;       properties are forgotten, which is wrong.
                           (keys (:cumulative-properties node)))]
    (cond-> (-> (set-batch-edit-ids node inherited)
                (add-inherited-attribute
                 [#{:label :element :recursive :optional} #{:content}
                  (cond-> (attributes-for-header-add-column-command
                           node elements-template inherited)
                    (= (count descendants) 1)
                    (assoc :delete-column {:referent descendants-referent}))])
                ;; Let item-render figure out whether to use narrow
                ;; or wide format.
                (assoc :width (* 0.75 (count descendants))))
      (:leaves node)
      (add-inherited-attribute [#{:content}
                                {:add-twin {:referent nil}
                                 :delete {:clear-only true}}])
      (and (:leaves node)
           (#{'anything 'anything-immutable} (entity/content item)))
      (add-inherited-attribute [#{:content}
                                {:class "placeholder"}]))))

(defn table-header-subtree-DOM
  "Generate the dom for a subtree of a table header hierarchy, given
  the doms for all the children."
  [node child-doms function-info inherited]
  (let [node-dom (horizontal-label-hierarchy-node-DOM
                  node function-info
                  (table-header-properties-inherited node inherited))
        is-leaf (empty? child-doms)
        elements-template (table-header-element-template
                           (keys (:cumulative-properties node)))
        is-tag (is-tag-template? elements-template)
        class (cond-> "column-header"
                is-leaf (str " leaf"))]
    (if (empty? child-doms)
      (add-attributes node-dom {:class class})
      [:div {:class (cond-> class
                      is-tag (str " tag"))}
       (add-attributes node-dom {:class "with-children"})
       (into [:div {:class "column-header-sequence"}]
             child-doms)])))

(defn table-header-child-info
  "In addition to what element-hierarchy-child-info does, set the
   batch-edit-ids to what our children with no properties of their own need."
  [node function-info inherited]
  (let [[function-info inherited] (element-hierarchy-child-info
                                   node function-info inherited)]
    [function-info
     (set-batch-edit-ids node inherited)]))

(defn table-header-top-level-subtree-DOM
  "Generate the dom for a top level subtree of a table header hierarchy.
  Inherited describes the column requests."
  [node inherited]
  (hierarchy-node-DOM-R
   node table-header-subtree-DOM table-header-child-info
   {:top-level true}
   inherited))

(defn table-virtual-header-element-template
  "Return a template for new elements of a virtual table header."
  [hierarchy]
  (table-header-element-template
   (when (seq hierarchy)
     (keys (:cumulative-properties (last hierarchy))))))

(defn table-virtual-header-node-DOM
  [hierarchy adjacent-referent inherited]
  (let [template (table-virtual-header-element-template hierarchy)
        inherited (assoc inherited
                         :key-prefix (conj (:key-prefix inherited)
                                           :virtualColumn)
                         :subject-referent (virtual-referent
                                            (:template inherited)
                                            (:subject-referent inherited)
                                            adjacent-referent)
                         :select-pattern (conj (:key-prefix inherited)
                                               [:pattern :subject] [:pattern])
                         :template template)]
    (add-attributes
     (virtual-element-DOM adjacent-referent :after inherited)
     {:class (cond-> "column-header virtual-column"
               (is-tag-template? template)
               (str " tag"))})))

(defn table-header-DOM
  "Generate DOM for column headers given the hierarchy.
  The column will contain those elements of the rows that match the templates
  in the hierarchy."
  [hierarchy inherited]
  (let [hierarchy (replace-hierarchy-leaves-by-nodes hierarchy)
        adjacent-referent (or (hierarchy-last-item-referent hierarchy)
                              (:subject-referent inherited))
        virtual-header (table-virtual-header-node-DOM
                        hierarchy adjacent-referent inherited)
        columns (map #(table-header-top-level-subtree-DOM % inherited)
                     hierarchy)]
    (into [:div {:class "column-header-sequence"}]
          (concat columns [virtual-header]))))

(defn table-cell-items-DOM-R
  "Return the dom for one cell of a table, given its items.
  Inherited gives the context of each item in the cell."
  [items column-id inherited]
  (let [row-referent (:subject-referent inherited)
        inherited
        (-> inherited
            (assoc :width 0.75)  ;; Tell item-render to use the narrow format.
            (add-inherited-attribute
             [#{:label :element :recursive :optional} #{:content}
              {:column {:referent column-id}}]))]
    (expr-let
        [dom (if (empty? items)
               ;; TODO: Get our left neighbor as an arg, and pass it
               ;; in as adjacent information for new-twin.
               (virtual-element-DOM nil :after inherited)
               (elements-DOM-R items false (:template inherited) :vertical
                               inherited))]
      (add-attributes dom {:class "table-cell has-border"}))))

(defn table-virtual-column-cell-DOM
  [row-item inherited]
  (add-attributes
   (virtual-element-DOM nil :after
                        (assoc inherited :select-pattern
                               (conj (vec (butlast (:key-prefix inherited)))
                                     [:pattern 1] [:pattern])))
   {:class "table-cell virtual-column has-border"}))

(defn table-cell-DOM-R
  "Return the dom for one cell of a table, given its column description."
  [row-item
   {:keys [column-id query template exclusions] :as header-description}
   inherited]
  (let [inherited-down (assoc inherited
                              :key-prefix (conj (:key-prefix inherited)
                                                column-id)
                              :template template)]
    (if (= column-id :virtualColumn)
      (table-virtual-column-cell-DOM row-item inherited-down)
      (expr-let [matches (matching-elements query row-item)
                 do-not-show (when exclusions
                               (expr-seq map #(matching-elements % row-item)
                                         exclusions))]
        (let [elements (seq (clojure.set/difference
                             (set matches)
                             (set (apply concat do-not-show))))]
          (table-cell-items-DOM-R
           elements column-id inherited-down))))))

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

(defn query-for-hierarchy-leaf
  "Given a hierarchy leaf, and the node it is from,
   return the query that items in cells of the column must satisfy.
   (Not worrying about exclusions.)"
  [leaf]
  (pattern-to-query (immutable-visible-to-list (:item leaf))))

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

(defn row-template-and-items-R
  "Given the item giving the row condition, return the template for a row
  and the items for the rows, in order."
  [store row-condition-item]
  (expr-let [condition-elements (table-condition-elements-R row-condition-item)
             elems-list (expr-seq map semantic-to-list-R condition-elements)
             row-template (concat '(anything) elems-list [:top-level])
             row-query (pattern-to-query row-template)
             row-items (expr order-items-R
                         (matching-items row-query store))] 
    [row-template row-items]))

(defn table-top-DOM-R
  "Return a hiccup representation for the top of a table, the part that
  holds its condition."
  [row-condition-item inherited]
  (let [subject-referent (union-referent [(item-referent row-condition-item)])]
    (expr-let [condition-elements (table-condition-elements-R
                                   row-condition-item)
               inherited-down (assoc
                               inherited
                               :subject-referent subject-referent
                               :template '(anything)
                               ;; TODO: Do only when all tags?
                               :attributes [[#{:label} #{:content}
                                             {:add-element
                                              {:referent subject-referent}}]])
               virtual-dom (virtual-element-with-label-DOM
                            'anything :vertical inherited-down)
               dom (labels-and-elements-DOM-R
                    condition-elements virtual-dom
                    true true :horizontal inherited-down)]
      [:div {:class "query-holder tag"}
       [:div {:class "query-indent tag"}]
       (add-attributes dom {:class "query-condition"})])))

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
  (assert (satisfies? entity/StoredEntity table-item))
  (let [store (:store table-item)
        table-referent (item-or-exemplar-referent
                        table-item (:subject-referent inherited))
        table-key (conj (:key-prefix inherited) table-referent)
        inherited (assoc inherited :key-prefix table-key)]
    (expr-let [immutable-item (entity/updating-with-immutable
                               [immutable table-item]
                               immutable)]
      ;; Don't do anything if we don't yet have the table information filled in.
      (when-let [row-condition-item (first (entity/label->elements
                                            immutable-item :row-condition))]
        (let [headers-inherited (update
                                 (assoc
                                  inherited
                                  :subject-referent (item-referent
                                                     row-condition-item)
                                  :template table-header-template)
                                 :priority inc)
              columns (order-items-R
                       (entity/label->elements row-condition-item :column))
              hierarchy (hierarchy-by-labels-R columns)
              headers (table-header-DOM
                       hierarchy headers-inherited)
              condition-dom (table-top-DOM-R
                             row-condition-item inherited)
              column-descriptions (mapcat
                                       table-hierarchy-node-column-descriptions
                                       hierarchy)
              new-column-template (new-header-template
                                   (table-virtual-header-element-template
                                    hierarchy)
                                   headers-inherited)]
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
                [:div {:class "query-result-indent tag"}]
                [:div {:class "table-main"}
                 headers
                 (into [:div {:class "table-rows"}]
                       (concat rows [virtual-row]))]]])))))))

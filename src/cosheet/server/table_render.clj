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
                                hierarchy-node-example-elements]]
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
                                   hierarchy-node-items-referent
                                   hierarchy-node-items-referents
                                   hierarchy-last-item-referent
                                   hierarchy-node-DOM-R]]
             [item-render :refer [elements-DOM-R virtual-element-with-label-DOM
                                  labels-and-elements-DOM-R
                                  label-stack-DOM-R
                                  item-content-and-elements-DOM-R
                                  item-content-and-non-label-elements-DOM-R
                                  item-content-DOM]])))

(def base-table-column-width 150)

;;; The condition elements of a table are its visible elements
;;; that are not column headers.
(defn table-condition-elements-R [row-condition-item]
  (expr-let
      [visible-elements (visible-elements-R row-condition-item)
       column-elements (entity/label->elements row-condition-item :column)]
    (remove (set column-elements) visible-elements)))

;;; This function is here, rather than in actions, because it knows about
;;; the structure of tables.
(defn batch-edit-selector
  "Given an item, return the list form of the batch query that matches
  everything 'like' the item. For a column header, this is everything
  that would go in its column, while for an element, it is all
  elements with its content.  For the table header, it is all matching
  rows."
  [immutable-item immutable-row-condition-item]
  ;; We walk up containing items, until we find an item that is either
  ;; column condition, an element of a row, or the entire row condition.
  ;; Then we return the appropriate thing for that situation.
  (-> (loop [item immutable-item]
        (let [parent-item (entity/subject item)]
          (cond
            ;; If we have reached a top level row or header element,
            ;; add it to the row condition.
            ;; But if it implies part of the row condition, don't include
            ;; that redundant part, while if a row condition implies the
            ;; element, don't include the element.
            (or (seq (matching-elements :column item))  
                (seq (matching-elements :top-level parent-item)))
            (let [condition-elements
                  (map immutable-visible-to-list
                       (table-condition-elements-R
                        immutable-row-condition-item))
                  item-condition (immutable-visible-to-list item)
                  redundant (best-matching-term
                             (map #(immutable-semantic-to-list
                                    (pattern-to-query %))
                                  condition-elements)
                             item-condition)
                  non-redundant (remove-first
                                 #(= (immutable-semantic-to-list
                                      (pattern-to-query %))
                                     redundant)
                                 condition-elements)
                  skip-element (when (not redundant)
                                 (let [query (pattern-to-query item-condition)]
                                   (some #(extended-by? query %)
                                         condition-elements)))]
              (apply list (concat ['anything]
                                  non-redundant
                                  (if skip-element [] [item-condition]))))
            ;; If the item is part of the row condition, just return that.
            (seq (matching-elements :row-condition item))
            (concat '(anything)
                    (map immutable-visible-to-list
                         (table-condition-elements-R item)))
            parent-item
            (recur parent-item))))
      add-order-elements
      (replace-in-seqs 'anything-immutable 'anything)))

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
  (let [descendants (->> (:child-nodes node)
                         (filter #(not (empty? (:properties %))))
                         (mapcat hierarchy-node-descendants))
        cover (if (every? #(#{'anything 'anything-immutable}
                            (:content %))
                          descendants)
                ;; We may not need to match all child nodes, just
                ;; a subset that subsumes all of them.
                (->> (hierarchy-node-next-level node)
                     (filter hierarchy-node?)
                     (hierarchy-nodes-extent))
                ;; The hierarchy ignores content, so if any of the
                ;; leaves have content, we don't know which ones
                ;; subsume the others, and so use them all.
                descendants)]
    (map #(pattern-to-query
           (cons
            (:content %)
            (map canonical-to-list (:property-canonicals %))))
       cover)))

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

(defn table-header-element-template
  "Return a template for new elements of a table header. It should include
  what is common to the specified elements, which should be in canonical
  list form."
  [canonical-elements]
  (if (seq canonical-elements)
    (or (when (every? sequential? canonical-elements)
          (let [firsts-sub-elements (second (first canonical-elements))
                remainder-sub-elements (map second (rest canonical-elements))]
            (if (empty? remainder-sub-elements)
              ;; If there is only one current element, then the next one
              ;; only copies whether or not it has a tag.
              (if (contains? firsts-sub-elements :tag)
                '(anything :tag)
                "")
              (let [common (reduce common-canonical-multisets
                                   firsts-sub-elements
                                   remainder-sub-elements)]
                (if (not (empty? common))
                  (cons 'anything (canonical-set-to-list common))
                  "")))))
        '(anything))
    '(anything :tag)))

(defn table-header-properties-inherited
  "Return the inherited to use for the properties of a table header."
  [{:keys [top-level]}
   node content example-elements column-referent inherited]
  (let [descendants (hierarchy-node-descendants node)
        item (:item (first descendants))
        elements-template (table-header-element-template
                           (keys (:cumulative-properties node)))]
    (cond-> (-> inherited
                (assoc :subject-referent column-referent
                       :template elements-template
                       :width (* 0.75 (count descendants)))
                (update :key-prefix
                        #(conj % (:item-id item)))
                (add-inherited-attribute
                 [#{:label :element :recursive :optional} #{:content}
                  (cond-> (attributes-for-header-add-column-command
                           node elements-template inherited)
                    (= (count descendants) 1)
                    (assoc :delete-column {:referent column-referent})
                    (empty? example-elements)
                    (assoc :expand {:referent column-referent}))]))
      (:leaves node)
      (add-inherited-attribute [#{:content}
                                {:add-twin {:referent nil}
                                 :delete {:clear-only true}}])
      (and (:leaves node) (#{'anything 'anything-immutable} content))
      (add-inherited-attribute [#{:content}
                                {:class "placeholder"}])
      (= (count example-elements) 1)
      (add-inherited-attribute
       [#{:label :element} #{:content}
        {:expand {:referent column-referent}}]))))

(defn table-header-node-DOM-R
  "Generate the DOM for a node in the hierarchy, not including its children."
  [node {:keys [shadowing-nodes top-level] :as function-info}
   inherited]
  (let [example-elements (hierarchy-node-example-elements node) 
        column-referent (hierarchy-node-items-referent node inherited)
        item (:item (first (hierarchy-node-descendants node))) ]
    (expr-let [content (when item (entity/content item))
               non-labels (when item (visible-non-labels-R item))]
      (let [inherited-down (table-header-properties-inherited
                            function-info node content example-elements
                            column-referent inherited)]
        (if (empty? (:properties node))
          (expr-let [inner-dom (item-content-and-non-label-elements-DOM-R
                                content non-labels inherited-down)]
            ;; TODO: Only set width if there is no content and non-elements.
            [:div {:style {:width (str base-table-column-width "px")}
                   :class (cond->
                              "column-header tag wrapped-element virtual-wrapper"
                            (not top-level)
                            (str " merge-with-parent"))}
             (cond-> (virtual-element-DOM
                      column-referent :after
                      (-> inherited-down
                          transform-inherited-for-labels
                          (update :key-prefix #(conj % :label))
                          (assoc :select-pattern (conj (:key-prefix inherited)
                                                       [:pattern]))))
               (is-tag-template? (table-header-element-template
                                  (keys (:cumulative-properties node))))
               (add-attributes {:class "tag"})
               (not top-level)
               (add-attributes {:class "merge-with-parent"}))
             [:div {:class "indent-wrapper tag"}
              (add-attributes
               inner-dom
               {:key (conj (:key-prefix inherited) (:item-id item))
                :class "item"})]])
          (if (empty? (:child-nodes node))
            (item-content-and-elements-DOM-R
             content (concat example-elements non-labels) false inherited-down)
            (label-stack-DOM-R
             example-elements
             (transform-inherited-for-labels inherited-down))))))))

(defn table-header-child-info
  "Generate the function-info and inherited for children of
  a hierarchy node.
  The function-info is a map with
     :shadowing-nodes    The column shouldn't match elements that also match
                         these
     :top-level          If this is a top level node
  Inherited describes the column requests."
  [node function-info inherited]
  (let [children (:child-nodes node)]
    [(assoc function-info
            :shadowing-nodes (filter #(seq (:properties %)) children)
            :top-level false)
     (-> inherited
         (update :key-prefix  #(conj % :nested))
         (update :template
                 #(add-elements-to-entity-list
                   % (canonical-set-to-list (:properties node)))))]))

(defn table-header-subtree-DOM-R
  "Generate the dom for a subtree of a table header hierarchy, given
  the dom particular to the node, and doms for all the children."
  [node child-doms function-info inherited]
  (expr-let [node-dom (table-header-node-DOM-R
                             node function-info inherited)]
    (let [is-leaf (empty? child-doms)
          elements-template (table-header-element-template
                             (keys (:cumulative-properties node)))
          is-tag (is-tag-template? elements-template)
          class (cond-> "column-header"
                  is-leaf (str " leaf"))]
      (if child-doms
        [:div {:class (cond-> class
                        is-tag (str " tag"))}
         (add-attributes node-dom {:class "with-children"})
         (into [:div {:class "column-header-sequence"}]
               child-doms)]
        (add-attributes node-dom {:class class})))))

(defn table-header-top-level-subtree-DOM-R
  "Generate the dom for a top level subtree of a table header hierarchy.
  If the node has no properties then the column shouldn't match
  elements that are also matches by shadowing-nodes.
  Inherited describes the column requests."
  [node inherited]
  (hierarchy-node-DOM-R
   node table-header-subtree-DOM-R table-header-child-info
   {:shadowing-nodes nil
    :top-level true}
   inherited)
  )

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

(defn table-header-DOM-R
  "Generate DOM for column headers given the hierarchy. elements-template
  gives what new elements of a header request need to satisfy.
  The column will contain those elements of the rows that match the templates
  in the hierarchy."
  [hierarchy inherited]
  (let [hierarchy (replace-hierarchy-leaves-by-nodes hierarchy)
        adjacent-referent (or (hierarchy-last-item-referent hierarchy)
                              (:subject-referent inherited))
        virtual-header (table-virtual-header-node-DOM
                        hierarchy adjacent-referent inherited)]
    (expr-let [columns (expr-seq
                        map #(table-header-top-level-subtree-DOM-R
                              % inherited)
                        hierarchy)]
      (into [:div {:class "column-header-sequence"}]
            (concat columns [virtual-header])))))

(defn table-cell-items-DOM-R
  "Return the dom for one cell of a table, given its items.
  Inherited gives the context of each item in the cell."
  [items column-id inherited]
  (let [row-referent (:subject-referent inherited)
        inherited
        (-> inherited
            (assoc :width 0.75)
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
                      (expr-seq map #(table-cell-DOM-R immutable % inherited)
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
   {:keys [column-id template exclusions]} ;; A column header description
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

(defn add-content-and-query-to-hierarchy-leaf-R
  "Given a hierarchy leaf, and the node it is from,
   add additional fields to its info map:
      :content  the content of the item
      :query    the query items in cells of the column must satisfy"
  [leaf node]
  (let [item (:item leaf)]
    (expr-let [content (entity/content item)
               non-labels (visible-non-labels-R item)
               as-lists (expr-seq map visible-to-list-R non-labels)]
      (assoc leaf
             :content content
             :query (pattern-to-query
                     (cons content
                           (concat (canonical-set-to-list
                                    (:cumulative-properties node))
                                   as-lists)))))))

(defn add-content-and-query-to-hierarchy-R
  "Given a hierarchy, add additional fields to each info map:
      :content  the content of the item
      :query    the query items in cells of the column must satisfy"
  [hierarchy]
  (expr-seq map
    (fn [node]
      (expr-let [leaves (expr-seq
                         map #(add-content-and-query-to-hierarchy-leaf-R % node)
                         (:leaves node))
                 children (add-content-and-query-to-hierarchy-R
                           (:child-nodes node))]
        (cond-> (assoc node :leaves leaves)
          children (assoc :child-nodes children))))
    hierarchy))

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
              (let [query (:query node-or-element)]
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
    (expr-let [row-condition-item (expr first (entity/label->elements
                                               table-item :row-condition))]
      ;; Don't do anything if we don't yet have the table information filled in.
      (when row-condition-item
        (let [headers-inherited (update
                                 (assoc
                                  inherited
                                  :subject-referent (item-referent
                                                     row-condition-item)
                                  :template table-header-template)
                                 :priority inc)]
          (expr-let
              [[row-template row-items] (row-template-and-items-R
                                         store row-condition-item)
               columns (expr order-items-R
                         (entity/label->elements row-condition-item :column))
               hierarchy (add-content-and-query-to-hierarchy-R
                          (hierarchy-by-labels-R columns))
               headers (table-header-DOM-R
                        hierarchy headers-inherited)
               condition-dom (table-top-DOM-R
                              row-condition-item inherited)]
            (let [column-descriptions (mapcat
                                       table-hierarchy-node-column-descriptions
                                       hierarchy)
                  new-column-template (new-header-template
                                       (table-virtual-header-element-template
                                        hierarchy)
                                       headers-inherited)
                  virtual-template (virtual-referent
                                    new-column-template
                                    (item-referent row-condition-item)
                                    (item-referent (or (last columns)
                                                       table-item)))
                  virtual-column-description {:column-id :virtualColumn
                                              :template virtual-template
                                              :exclusions nil}
                  rows (map #(table-row-DOM-component
                              % row-template (concat column-descriptions
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

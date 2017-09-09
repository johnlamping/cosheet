(ns cosheet.server.table-render
  (:require (cosheet [utils :refer [replace-in-seqs multiset separate-by
                                    add-elements-to-entity-list]]
                     [entity :as entity]
                     [query :refer [matching-elements matching-items]]
                     [debug :refer [simplify-for-print]]
                     [hiccup-utils :refer [dom-attributes
                                           into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq]]
                     [canonical :refer [canonicalize-list canonical-to-list
                                        canonical-set-to-list
                                        common-canonical-multisets]])
            (cosheet.server
             [referent :refer [item-referent exemplar-referent
                               elements-referent query-referent
                               union-referent-if-needed union-referent
                               difference-referent virtual-referent
                               item-or-exemplar-referent
                               semantic-elements-R semantic-to-list-R
                               pattern-to-condition condition-to-template]]
             [hierarchy :refer [hierarchy-node? hierarchy-node-descendants
                                replace-hierarchy-leaves-by-nodes
                                hierarchy-node-leaves
                                hierarchy-node-next-level hierarchy-node-extent
                                hierarchy-nodes-extent
                                hierarchy-by-all-elements-R
                                hierarchy-node-example-elements
                                hierarchy-node-items-referent
                                hierarchy-node-parallel-items-referent
                                hierarchy-last-item-referent]]
             [order-utils :refer [order-items-R]]
             [render-utils :refer [make-component virtual-item-DOM
                                   transform-inherited-for-children
                                   transform-inherited-for-labels
                                   add-inherited-attribute
                                   hierarchy-node-DOM-R]]
             [item-render :refer [elements-DOM-R condition-elements-DOM-R
                                  item-content-and-elements-DOM-R
                                  item-DOM-R item-content-DOM]])))

(def base-table-column-width 150)

(defn is-tag-template?
  "Return true if the template describes a label."
  [template]
  (some #(or (= (if (sequential? %) (first %) %) :tag))
        template))

(defn table-node-row-elements-referent
  "Generate a referent for the elements in rows covered by the conditions
  of a table header, but by no shadow node, if any are present."
  ([node rows-referent]
   (table-node-row-elements-referent node nil rows-referent))
  ([node shadowing-nodes rows-referent]
   (cond-> (union-referent-if-needed
            (map #(elements-referent (:item %) rows-referent)
                 (hierarchy-node-descendants node)))
     (seq shadowing-nodes)
     (difference-referent
      (union-referent-if-needed
       (map #(elements-referent (:item %) rows-referent)
            (hierarchy-nodes-extent shadowing-nodes)))))))

(defn table-node-delete-referent
  "Generate the referent for the elements to be deleted when the only
  item of a table header node is deleted. If we have no descendants,
  then deletion is not allowed, and we return nil. Otherwise, we
  return a referent to the columns of all descendants that have more than just
  the node does.
  The arguments are the node, a referent to the rows of the table,
  and to the subject of the header requests.
  When the referent is instantiated, the first group must be all the elements
  in table requests, while subsequent groups contain elements in rows brought
  up by the header."
  [node rows-referent header-subject]
  (let [deeper-descendants (filter #(not= (multiset (:property-canonicals %))
                                          (:cumulative-properties node))
                                   (hierarchy-node-descendants node))]
    (when (seq deeper-descendants)
      (let [exemplar-item (first (hierarchy-node-example-elements node))
            referent-for-leaf #(item-or-exemplar-referent
                                (:item %) header-subject)
            header-ref (item-or-exemplar-referent
                        exemplar-item
                        (union-referent-if-needed
                         (map referent-for-leaf deeper-descendants)))
            sub-nodes (filter hierarchy-node? (hierarchy-node-next-level node))
            deeper-columns-elements-referent (union-referent-if-needed
                                              (map #(elements-referent
                                                     (:item %) rows-referent)
                                                   (hierarchy-nodes-extent
                                                    sub-nodes)))
            ;; The element corresponding to this header element in
            ;; the elements in the columns.
            matches-ref (exemplar-referent
                         exemplar-item deeper-columns-elements-referent)]
        ;; We always return a union, to guarantee that instantition will
        ;; return the header elements in the first group.
        (union-referent [header-ref matches-ref])))))

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
        adjacent-referent (hierarchy-node-parallel-items-referent
                           node subject-ref)
        new-column-ref (virtual-referent (:template inherited)
                                         (union-referent [subject-ref])
                                         adjacent-referent
                                         :position :after
                                         :selector :first-group)
        new-element-ref (virtual-referent (cons '??? (rest elements-template))
                                          new-column-ref
                                          nil
                                          :position :after
                                          :selector :first-group)
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
              (when (contains? firsts-sub-elements :tag)
                '(nil :tag))
              (let [common (reduce common-canonical-multisets
                                   firsts-sub-elements
                                   remainder-sub-elements)]
                (when (not (empty? common))
                  (cons nil (canonical-set-to-list common)))))))
        '(nil))
    '(nil :tag)))

(defn table-header-properties-inherited
  "Return the inherited to use for the properties of a table header."
  [node {:keys [top-level rows-referent]}
   example-elements column-referent inherited]
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
                    (assoc :delete-column {:referent column-referent
                                           :alternate true})
                    (empty? example-elements)
                    (assoc :expand {:referent column-referent}))]))
      (:leaves node)
      (add-inherited-attribute
       [#{:content}
        {:delete {:referent nil}
         :class "placeholder"}])
      (= (count example-elements) 1)
      (add-inherited-attribute
       [#{:label :element} #{:content}
        (cond-> {:expand {:referent column-referent}}
          ;; If we are a child, it is OK to delete our last element, as
          ;; our parent will still contribute an element. But if we
          ;; are a top level node, we can't, in general, delete our
          ;; last element, or there would be nothing left in the node.
          top-level
          (assoc :delete
                 {:referent (table-node-delete-referent
                             node rows-referent
                             (:subject-referent inherited))}))]))))

(defn table-header-properties-DOM-R
  "Generate the DOM for the properties of a node in the hierarchy."
  [node {:keys [shadowing-nodes rows-referent]
         :as function-info}
   inherited]
  (let [example-elements (hierarchy-node-example-elements node) 
        column-referent (union-referent
                         [(hierarchy-node-items-referent
                           node (:subject-referent inherited))
                          (table-node-row-elements-referent
                           node (when (empty? example-elements) shadowing-nodes)
                           rows-referent)])
        item (:item (first (hierarchy-node-descendants node)))
        inherited-down (table-header-properties-inherited
                        node function-info example-elements column-referent
                        inherited)]
    (if (empty? (:properties node))
      ;; TODO: This needs to check for not being a tag, and doing something
      ;;       different in that case.
      [:div {:style {:width (str base-table-column-width "px")}
             :class "column-header tag wrapped-element merge-with-parent"}
       (cond-> (virtual-item-DOM
                (conj (:key-prefix inherited-down) :label)
                column-referent :after
                (transform-inherited-for-labels inherited-down))
         (is-tag-template? (table-header-element-template
                            (keys (:cumulative-properties node))))
         (add-attributes {:class "tag content-text"}))
       [:div {:class "indent-wrapper tag"}
        (add-attributes
         (item-content-DOM column-referent 'anything-immutable inherited-down)
         {:key (:key-prefix inherited)
          :class "item"})]]
      (if (empty? (:child-nodes node))
        (expr-let [content (entity/content item)]
          (item-content-and-elements-DOM-R
           content example-elements false inherited-down))
        (condition-elements-DOM-R example-elements true :vertical
                                  inherited-down)))))

(defn table-header-child-info
  "Generate the function-info and inherited for children of
  a hierarchy node.
  The function-info is a map with
     :shadowing-nodes    The column shouldn't match elements that also match
                         these
     :top-level          If this is a top level node
     :rows-referent      all the row items from which this header selects
                         elements
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

(defn table-header-node-DOM-R
  "Generate the dom for a subtree of a table header hierarchy, given
  the dom particular the node, and doms for all the children."
  [node child-doms function-info inherited]
  (expr-let [properties-dom (table-header-properties-DOM-R
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
         (add-attributes properties-dom {:class "with-children"})
         (into [:div {:class "column-header-sequence"}]
               child-doms)]
        (add-attributes properties-dom {:class class})))))

(defn table-header-top-level-subtree-DOM-R
  "Generate the dom for a top level subtree of a table header hierarchy.
  If the node has no properties then the column shouldn't match
  elements that are also matches by shadowing-nodes.
  rows-referent should specify all the row items from which this
  header selects elements. Inherited describes the column requests."
  [node rows-referent inherited]
  (hierarchy-node-DOM-R
   node table-header-node-DOM-R table-header-child-info
   {:shadowing-nodes nil
    :top-level true
    :rows-referent rows-referent}
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
  (let [key (conj (:key-prefix inherited) :virtualColumn)
        template (table-virtual-header-element-template hierarchy)
        inherited (assoc inherited
                         :subject-referent (virtual-referent
                                            (:template inherited)
                                            (:subject-referent inherited)
                                            adjacent-referent
                                            :selector :first-group)
                         :select-pattern (conj (:key-prefix inherited)
                                               :nested [:pattern])
                         :template template)]
    (add-attributes
     (virtual-item-DOM key adjacent-referent :after inherited)
     {:class (cond-> "column-header virtual-column"
               (is-tag-template? template)
               (str " tag"))})))

(defn table-header-DOM-R
  "Generate DOM for column headers given the hierarchy. elements-template
  gives what new elements of a header request need to satisfy.
  The column will contain those elements of the rows that match the templates
  in the hierarchy."
  [hierarchy rows-referent inherited]
  (let [hierarchy (replace-hierarchy-leaves-by-nodes hierarchy)
        inherited-down (assoc inherited
                              :selector-category :table-header
                              :alternate-target true)
        adjacent-referent (or (hierarchy-last-item-referent hierarchy)
                              (:subject-referent inherited))
        virtual-header (table-virtual-header-node-DOM
                        hierarchy adjacent-referent inherited-down)]
    (expr-let [columns (expr-seq
                        map #(table-header-top-level-subtree-DOM-R
                              % rows-referent inherited-down)
                        hierarchy)]
      (into [:div {:class "column-header-sequence"}]
            (concat columns [virtual-header])))))

(defn table-cell-items-DOM-R
  "Return the dom for one cell of a table, given its items.
  Inherited gives the context of each item in the cell."
  [items column-id new-row-template inherited]
  (let [row-referent (:subject-referent inherited)
        inherited
        (-> inherited
            (assoc :width 0.75)
            (add-inherited-attribute
             [#{:label :element :recursive :optional} #{:content}
              {:add-row
               {:referent
                (virtual-referent new-row-template nil row-referent)
                :select-pattern (conj (vec (-> (:key-prefix inherited)
                                               butlast
                                               butlast))
                                      [:pattern] column-id)}
               :delete-row {:referent row-referent}}]))]
    (expr-let
        [dom (if (empty? items)
               ;; TODO: Get our left neighbor as an arg, and pass it
               ;; in as adjacent information for new-twin.
               (virtual-item-DOM
                (:key-prefix inherited) (:subject-referent inherited) :after
                inherited)
               (elements-DOM-R items false (:template inherited) :vertical
                               inherited))]
      (add-attributes dom {:class "table-cell has-border"}))))

(defn table-virtual-column-cell-DOM
  [row-item inherited]
  (add-attributes
   (virtual-item-DOM (:key-prefix inherited)
                     (:subject-referent inherited) :after
                     (assoc inherited :select-pattern
                            (conj (vec (butlast (:key-prefix inherited)))
                                  [:pattern 1] [:pattern])))
   {:class "table-cell virtual-column has-border"}))

(defn table-cell-DOM-R
  "Return the dom for one cell of a table, given its column description."
  [row-item new-row-template
   {:keys [column-id template exclusions] :as header-description}
   inherited]
  (let [inherited-down (assoc inherited
                              :key-prefix (conj (:key-prefix inherited)
                                                column-id)
                              :template template)]
    (if (= column-id :virtualColumn)
      (table-virtual-column-cell-DOM row-item inherited-down)
      (expr-let [matches (matching-elements template row-item)
                 do-not-show (when exclusions
                               (expr-seq map #(matching-elements
                                               (pattern-to-condition %)
                                               row-item)
                                         exclusions))]
        (let [elements (seq (clojure.set/difference
                             (set matches)
                             (set (apply concat do-not-show))))]
          (table-cell-items-DOM-R
           elements column-id new-row-template inherited-down))))))

(defn table-row-DOM-R
  "Generate dom for a table row."
  [row-item row-key new-row-template column-descriptions inherited]
  (let [inherited (-> inherited
                      (assoc :key-prefix row-key)
                      (update :priority inc)
                      (update :subject-referent
                              #(item-or-exemplar-referent row-item %)))]
    (expr-let [cells (expr-seq map #(table-cell-DOM-R
                                     row-item new-row-template % inherited)
                               column-descriptions)]
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
                         :template template
                         :select-pattern select)
        key (conj (:key-prefix inherited) column-id)]
    (add-attributes
     (virtual-item-DOM key adjacent-referent :after inherited)
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

(defn table-hierarchy-node-condition
  "Given a hierarchy node, return the condition that all elements
  under the node satisfy."
  [node]
  (pattern-to-condition
   (cons nil (canonical-set-to-list (:cumulative-properties node)))))

(defn table-hierarchy-node-exclusions
  "Given a hierarchy node, return a seq of conditions that immediate
  elements of the node must not satisfy, because they are covered
  by sub-nodes."
  [node]
  (map #(pattern-to-condition
         (cons nil (map canonical-to-list (:property-canonicals %))))
       (->> (hierarchy-node-next-level node)
            (filter hierarchy-node?)
            (hierarchy-nodes-extent))))

(defn table-hierarchy-node-column-descriptions
  "Given a hierarchy node, for each column under the node,
  return a map:
       :column-id Id that identifies the column.
                  Typically the id of the column item.
        :template Condition that each element of the column must satisfy.
                  May be a constant or a (typically virtual) referent.
                  ('anything is turned to nil before putting a condition here.)
      :exclusions Seq of conditions that elements must not satisfy."
  [node]
  (mapcat (fn [node-or-element]
            (if (hierarchy-node? node-or-element)
              (table-hierarchy-node-column-descriptions node-or-element)
              [{:column-id (:item-id (:item node-or-element))
                :template (table-hierarchy-node-condition node)
                :exclusions (table-hierarchy-node-exclusions node)}]))
          (hierarchy-node-next-level node)))

(defn row-template-and-items-R
  "Given the item giving the row condition, return the template for a row
  and the items for the rows, in order."
  [store row-condition-item]
  (expr-let [row-condition (semantic-to-list-R row-condition-item)
             row-query (add-elements-to-entity-list
                        (pattern-to-condition row-condition)
                        ['(:top-level :non-semantic)])
             ;; Avoid the (nil :order :non-semantic) added by
             ;; pattern-to-condition.
             row-template (condition-to-template row-query)
             row-items (expr order-items-R
                         (matching-items row-query store))]
    [row-template row-items]))

(defn table-top-DOM-R
  "Return a hiccup representation for the top of a table, the part that
  holds its condition. Also return whether the condition is all tags."
  [row-condition-item rows-referent inherited]
  (let [subject-referent (union-referent [(item-referent row-condition-item)
                                          rows-referent])]
    (expr-let [condition-elements (semantic-elements-R row-condition-item)
               conditions-as-lists (expr-seq map semantic-to-list-R
                                             condition-elements)
               condition-tags (filter #(and (sequential? %)
                                            (seq (filter #{:tag} %)))
                                      conditions-as-lists)
               condition-is-all-tags (= (count conditions-as-lists)
                                        (count condition-tags))
               dom (condition-elements-DOM-R
                    condition-elements :wide :vertical
                    (assoc inherited
                           :selector-category :table-condition
                           :subject-referent subject-referent
                           :template (if condition-is-all-tags
                                       '(nil :tag)
                                       '(nil))
                           :alternate-target true
                           ;; TODO: Do only when all tags?
                           :attributes [[#{:label} #{:content}
                                          {:add-element
                                           {:referent subject-referent}}]]))]
      [[:div {:class (cond-> "table-top selectors"
                       condition-is-all-tags (str " tag"))}
        [:div {:class "table-corner"}]
        (add-attributes dom {:class "table-condition"})]
       condition-tags
       condition-is-all-tags])))

(defn table-DOM-R
  "Return a hiccup representation of DOM, with the given internal key,
  describing a table."
  ;; The following elements of item describe the table:
  ;;  :row-query  The content is an item whose list form gives the
  ;;              requirements for an item to appear as a row. When the
  ;;              query is created, an extra [:top-level
  ;;              :non-semantic] element is added, to keep the query,
  ;;              which is also in the database, from matching itself.
  ;;     :column  The semantics gives the requirements for an element
  ;;              of a row to appear in this column. The :column
  ;;              element has, itself, a :non-semantic element, to
  ;;              make it not part of the semantics of the column
  ;;              specifier. Generally, the content of the content
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
        (let [;; We have to use the item in the referent's condition, so
              ;; it doesn't contain strings or other non-serializable stuff.
              rows-referent (query-referent
                             (list (item-referent row-condition-item)
                                   '(:top-level :non-semantic)))
              headers-inherited (update
                                 (assoc
                                  inherited
                                  :subject-referent (item-referent
                                                     row-condition-item)
                                  :template '(anything-immutable
                                              (:column :non-semantic)
                                              (:non-semantic :non-semantic)))
                                 :priority inc)]
          (expr-let
              [[row-template row-items] (row-template-and-items-R
                                         store row-condition-item)
               columns (expr order-items-R
                         (entity/label->elements row-condition-item :column))
               hierarchy (hierarchy-by-all-elements-R columns)
               headers (table-header-DOM-R
                        hierarchy rows-referent headers-inherited)
               
               [condition-dom
                condition-tags
                condition-is-all-tags] (table-top-DOM-R
                                        row-condition-item rows-referent
                                        inherited)]
            (let [column-descriptions (mapcat
                                       table-hierarchy-node-column-descriptions
                                       hierarchy)
                  new-column-template (new-header-template
                                       (table-virtual-header-element-template
                                        hierarchy)
                                       headers-inherited)
                  virtual-template (virtual-referent new-column-template
                                                     (item-referent table-item)
                                                     (item-referent
                                                      (or (last columns)
                                                          table-item))
                                                     :selector :first-group)
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
              [:div {:class "table selector-scope"}
               condition-dom
               [:div {:class "table-body"}
                [:div {:class (cond-> "table-indent"
                                condition-is-all-tags (str " tag"))}]
                [:div {:class "table-main selectees selector-scope"}
                 (add-attributes headers {:class "selectors"})
                 (into [:div {:class "table-rows selectees"}]
                       (concat rows [virtual-row]))]]])))))))

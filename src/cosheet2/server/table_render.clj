(ns cosheet2.server.table-render
  (:require (cosheet2 [utils :refer [replace-in-seqs multiset separate-by
                                     add-elements-to-entity-list remove-first]]
                      [store :refer [id->target target-label->ids]]
                      [reporter :refer [universal-category]]
                      [entity :refer [content elements label->elements
                                      description->entity
                                      description->updating-entity-R
                                      label? label->element]]
                      [query :refer [matching-elements matching-items
                                     extended-by?]]
                      [query-calculator :refer [matching-item-ids-R]]
                      [debug :refer [simplify-for-print]]
                      [hiccup-utils :refer [dom-attributes
                                            into-attributes add-attributes]]
                      [expression :refer [expr expr-let expr-seq]])
            (cosheet2.server
             [hierarchy :refer [hierarchy-node? hierarchy-node-descendants
                                replace-hierarchy-leaves-by-nodes
                                hierarchy-node-leaves
                                hierarchy-node-next-level
                                hierarchy-nodes-extent
                                hierarchy-by-labels
                                hierarchy-node-example-elements
                                hierarchy-node-non-immediate-descendant-cover]]
             [order-utils :refer [ordered-ids-R ordered-entities]]
             [model-utils :refer [table-row-template
                                  table-column-headers-id
                                  table-row-condition-id
                                  semantic-to-list
                                  semantic-elements semantic-non-label-elements
                                  pattern-to-fixed-term fixed-term-to-template
                                  column-header-template
                                  unspecified-column-header-template
                                  exemplar-to-fixed-term]]
             [render-utils :refer [make-component
                                   mutable-store-get-rendering-data
                                   hierarchy-node-DOM
                                   transform-specification-for-elements]]
             [item-render :refer [virtual-DOM-component
                                  render-virtual-DOM
                                  virtual-entity-and-label-DOM
                                  label-stack-DOM
                                  item-content-and-non-label-elements-DOM
                                  item-content-DOM
                                  labels-and-elements-DOM
                                  non-label-entities-DOM
                                  horizontal-label-hierarchy-node-DOM]]
             [action-data :refer [get-id-action-data
                                  get-item-or-exemplar-action-data
                                  get-pass-through-action-data
                                  get-virtual-action-data
                                  composed-get-action-data]])))

(defn get-virtual-column-cell-action-data
  "Create a new column header and an element under that column in the row.
   The containing data's subject-ids are the id of the row."
  [specification containing-action-data action immutable-store]
  (let [column-headers-id (table-column-headers-id
                           (:table-id containing-action-data) immutable-store)
        column-headers (description->entity column-headers-id immutable-store)
        columns (semantic-elements column-headers)
        last-column-id (:item-id (last (ordered-entities columns))) 
        ;; Add the column header for the new column to the store.
        {:keys [store subject-ids]}
        (get-virtual-action-data
         {:sibling true
          :template unspecified-column-header-template}
         {:subject-ids [last-column-id]}
         action immutable-store)
        new-column-id (first subject-ids)
        template (semantic-to-list (description->entity new-column-id store))]
    (get-virtual-action-data
     {:template template} containing-action-data action store)))

(defmethod print-method
  cosheet2.server.table_render$get_virtual_column_cell_action_data
  [v ^java.io.Writer w]
  (.write w "virt-col-cell-AD"))

(defn get-table-condition-do-batch-edit-action-data
  [specification containing-action-data action immutable-store]
  (let [row-condition-id (table-row-condition-id
                          (:table-id containing-action-data) immutable-store)
        row-condition (description->entity row-condition-id immutable-store)
        condition-elements (semantic-elements row-condition)
        query-ids (map :item-id condition-elements)]
    (assoc containing-action-data
           :query-ids query-ids
           :stack-ids query-ids
           :must-show-label true)))

(defmethod print-method
  cosheet2.server.table_render$get_table_condition_do_batch_edit_action_data
  [v ^java.io.Writer w]
  (.write w "table-cond-do-batch-AD"))

(defn get-table-header-do-batch-edit-action-data
  [{:keys [item-id relative-id column-ids competing-ids]}
   containing-action-data action immutable-store]
  (let [id (or item-id relative-id)
        row-condition-id (table-row-condition-id
                          (:table-id containing-action-data) immutable-store)
        row-condition (description->entity row-condition-id immutable-store)
        condition-elements (semantic-elements row-condition)
        query-ids (map :item-id condition-elements)
        stack-ids (concat (when (= (count column-ids) 1)
                            competing-ids)
                          column-ids)]
    (assoc containing-action-data
           :query-ids query-ids
           :stack-ids stack-ids
           :selected-index (let [index (.indexOf stack-ids id)]
                             (when (>= index 0) index)))))

(defmethod print-method
  cosheet2.server.table_render$get_table_header_do_batch_edit_action_data
  [v ^java.io.Writer w]
  (.write w "table-head-do-batch-AD"))

(defn get-table-cell-do-batch-edit-action-data
  "Generate the batch edit information for a cell that is independent
  of its items."
  [{:keys [competing-ids]}
   containing-action-data action immutable-store]
  (let [row-condition-id (table-row-condition-id
                          (:table-id containing-action-data) immutable-store)
        row-condition (description->entity row-condition-id immutable-store)
        condition-elements (semantic-elements row-condition)
        query-ids (map :item-id condition-elements)]
    (assoc containing-action-data
           :query-ids query-ids
           :stack-ids competing-ids)))

(defmethod print-method
  cosheet2.server.table_render$get_table_cell_do_batch_edit_action_data
  [v ^java.io.Writer w]
  (.write w "table-cell-do-batch-AD"))

(defn get-table-cell-item-do-batch-edit-action-data
  [{:keys [item-id relative-id] :as specification}
   {:keys [stack-ids] :as containing-action-data}
    action immutable-store]
  (let [id (or item-id relative-id)]
    (assert id specification)
    (assert (contains? containing-action-data :stack-ids)
            containing-action-data)
    (assoc containing-action-data
           :stack-ids (concat stack-ids [id])
           :selected-index (count stack-ids))))

(defmethod print-method
  cosheet2.server.table_render$get_table_cell_item_do_batch_edit_action_data
  [v ^java.io.Writer w]
  (.write w "table-cell-item-do-batch-AD"))

(defn get-table-condition-rendering-data
  [{:keys [relative-id]} mutable-store]
  [[mutable-store [relative-id]]])

(defn render-table-condition-DOM
  "Return a hiccup representation for the top of a table, the part that
  holds its condition. The relative-id should be for the header"
  [{:keys [relative-id] :as spec} store]
  (let [row-condition (description->entity relative-id store)
        condition-elements (semantic-elements row-condition)
        spec-down {:template 'anything
                   :width 0.75}
        last-item (last (ordered-entities (remove label? condition-elements)))
        virtual-dom
        (add-attributes
         (virtual-entity-and-label-DOM
          (cond-> (assoc spec-down
                         :relative-id :virtual)
            ;; If we have any headers already, put the new one after
            ;; the last of them.
            last-item
            (assoc :item-id (:item-id last-item)
                   :get-action-data get-item-or-exemplar-action-data
                   :sibling true))
          :vertical)
         {:class "virtual-column"})
        dom (labels-and-elements-DOM
             condition-elements virtual-dom
             true true :horizontal spec-down)]
    (add-attributes dom {:class "query-condition"})))

(defn is-label-template?
  "Return true if the template describes a label."
  [template]
  (and (sequential? template)
       (some #(= (content %) :label)
             (rest template))))

(defn table-hierarchy-node-disqualifications
  "Given a hierarchy node, return a seq of conditions that immediate
  elements of the node must not satisfy, because they are covered
  by sub-nodes."
  [node]
  (map #(pattern-to-fixed-term (semantic-to-list (:item %)))
       (hierarchy-node-non-immediate-descendant-cover node)))

(defn table-header-node-specification
  "Return the specification to use for a DOM that is part of a table header."
  [node parent-cover]
  (let [column-ids (map #(:item-id (:item %))
                        (hierarchy-node-descendants node))]
    (cond->
        {:column-ids column-ids
         :get-do-batch-edit-action-data
         get-table-header-do-batch-edit-action-data
         :width (* 0.75 (count column-ids))
         ;; Tell add-twin and delete that they must not add or remove a column.
         :template :singular}
      (and (seq parent-cover) (empty? (:properties node)))
      (assoc :competing-ids parent-cover))))

(defn table-header-subtree-DOM
  "Generate the dom for a subtree of a table header hierarchy, given
  the doms for all the children."
  [node child-doms specification]
  (let [spec (into (dissoc specification :parent-cover-ids :competing-ids)
                   (table-header-node-specification
                    node (:parent-cover-ids specification)))
        node-dom (horizontal-label-hierarchy-node-DOM
                  node spec)
        is-leaf (empty? child-doms)
        class (cond-> "column-header"
                is-leaf (str " leaf"))]
    (if is-leaf
      (add-attributes node-dom {:class class})
      [:div {:class (str class " label")}
       (add-attributes node-dom {:class "with-children"})
       (into [:div {:class "column-header-sequence"}]
             child-doms)])))

(defn table-header-top-level-subtree-DOM
  "Generate the dom for a top level subtree of a table header hierarchy.
  Inherited describes the column requests."
  [node]
  (hierarchy-node-DOM
   node
   table-header-subtree-DOM
   (fn [node spec]
     (let [cover (hierarchy-node-non-immediate-descendant-cover node)]
       (-> spec
           (dissoc :top-level)
           (assoc :parent-cover-ids (map #(:item-id (:item %)) cover)))))
   {:top-level true}))

(defn table-virtual-column-header-DOM
  [hierarchy]
  (let [spec {:relative-id :virtual-column
              :template column-header-template
              :width 0.75}]
    (if (empty? hierarchy)
      (virtual-entity-and-label-DOM spec :vertical-wrapped)
      (let [last-column (last (hierarchy-node-descendants (last hierarchy)))
            last-column-id (:item-id (:item last-column))]
        (add-attributes
         (virtual-entity-and-label-DOM
          (assoc spec
                 :get-action-data get-item-or-exemplar-action-data
                 :item-id last-column-id
                 :sibling true)
          :vertical-wrapped)
         {:class  "column-header virtual-column"})))))

(defn get-table-header-rendering-data
  [{:keys [hierarchy-R] :as spec} mutable-store]
  [[hierarchy-R [universal-category]]])

(defn render-table-header-DOM
  "Generate DOM for column headers given the hierarchy.
  The column will contain those elements of the rows that match the templates
  in the hierarchy."
  [_ hierarchy]
  (let [doms (map table-header-top-level-subtree-DOM hierarchy)
        virtual-header (table-virtual-column-header-DOM hierarchy)]
    (into [:div {:class "column-header-sequence table-header"}]
          (concat doms [virtual-header]))))

(defn table-virtual-column-cell-DOM-component
  [specification]
  (add-attributes
   (make-component
    (assoc specification
           :relative-id :virtual
           :template ""
           :render-dom render-virtual-DOM
           :get-rendering-data mutable-store-get-rendering-data
           :get-action-data get-virtual-column-cell-action-data))
   {:class "table-cell virtual-column has-border"}))

(defn get-table-cell-rendering-data
  [specification mutable-store]
  [[mutable-store [(:row-id specification)]]])

(defmethod print-method
  cosheet2.server.table_render$get_table_cell_rendering_data
  [v ^java.io.Writer w]
  (.write w "cell-RD"))

;;; TODO: This isn't generating the right batch edit action data for
;;; labels of its items.
(defn render-table-cell-DOM
  [{:keys [row-id query disqualifications] :as specification} store]
  (let [row-entity (description->entity row-id store)
        matches (matching-elements query row-entity)
        entities (if (seq disqualifications)
                   (filter (fn [element] (not (some #(extended-by? % element)
                                                    disqualifications)))
                           matches)
                   matches)
        spec (-> specification
                 transform-specification-for-elements
                 (assoc :template (fixed-term-to-template query)))
        non-virtual-spec (assoc spec :get-do-batch-edit-action-data
                                get-table-cell-item-do-batch-edit-action-data)]
    (if (empty? entities)
      ;; TODO: Get our left neighbor as an arg, and pass it
      ;; in the sibling for the virtual dom.
      (virtual-DOM-component (assoc spec :relative-id :virtual))
      (non-label-entities-DOM
       entities (:template spec) false :vertical non-virtual-spec))))

(defmethod print-method
  cosheet2.server.table_render$render_table_cell_DOM
  [v ^java.io.Writer w]
  (.write w "cell-DOM"))

(defn table-cell-DOM-component
  "Return a component for one cell of a table, given its column
  description and the row specification.  We need to put each table
  cell in a component, so its column-id can be included in its client
  id. Otherwise, if several columns show the same item, we could have
  the same client id for both."
  [{:keys [column-id width] :as column-description}
   specification]
  (if (= column-id :virtualColumn)
    (table-virtual-column-cell-DOM-component
     (assoc specification :width width))
    (make-component
     (-> specification
         (assoc :relative-id column-id
                :column-ids [column-id]
                :class "table-cell"
                :render-dom render-table-cell-DOM
                :get-rendering-data get-table-cell-rendering-data
                :get-action-data get-pass-through-action-data
                :get-do-batch-edit-action-data
                get-table-cell-do-batch-edit-action-data)
         (into (select-keys column-description
                            [:query :competing-ids
                             :disqualifications
                             :width]))))))

(defn get-table-row-rendering-data
  [{:keys [relative-id column-descriptions-R]} mutable-store]
  [[mutable-store [relative-id]]
   [column-descriptions-R [universal-category]]])

(defn render-table-row-DOM
  "Generate dom for a table row.
  The specification must have column-descriptions-R"
  [{:keys [relative-id] :as specification} store column-descriptions]
  (let [spec (-> specification
                 (dissoc :column-descriptions-R)
                 (assoc :class "table-cell has-border"))]
    (let [cells (map #(table-cell-DOM-component % spec)
                     column-descriptions)]
      (into [:div {}] cells))))

(defmethod print-method
  cosheet2.server.table_render$render_table_row_DOM
  [v ^java.io.Writer w]
  (.write w "row-DOM"))

(defn table-row-component
  ;; The specification must include column-descriptions-R
  [row-id row-template specification]
  (make-component
   (assoc specification
          :relative-id row-id
          :row-id row-id ; Action data passes this down to everything
                         ; in the row.
          :class "table-row"
          :render-dom render-table-row-DOM
          :get-action-data [get-id-action-data row-id]
          :get-rendering-data get-table-row-rendering-data)))

(defn table-virtual-row-cell-DOM-component
  [{:keys [column-id query width] :as column-description}]
  (make-component
   {:relative-id column-id
    :column-ids [column-id]
    :class "table-cell"
    :render-dom render-virtual-DOM
    :get-rendering-data mutable-store-get-rendering-data
    :template (fixed-term-to-template query)
    :get-action-data get-virtual-action-data
    :width width}))

(defn get-table-virtual-row-rendering-data
  [{:keys [column-descriptions-R]} mutable-store]
  [[column-descriptions-R [universal-category]]])

(defn render-table-virtual-row-DOM
  "Generate dom for a table's virtual row."
  [{:keys [template]} column-descriptions]
  (let [cells (map table-virtual-row-cell-DOM-component
                   ;; Don't make a cell for the virtual column.
                   (butlast column-descriptions))]
    (into [:div {:class "table-row"}] cells)))

(defmethod print-method
  cosheet2.server.table_render$render_table_virtual_row_DOM
  [v ^java.io.Writer w]
  (.write w "virt-row-DOM"))

(defn table-virtual-row-DOM-component
  "Generate the component for a table's virtual row."
  [row-template adjacent-id column-descriptions-R]
    (make-component
     {:relative-id :virtual-row
      :class "table-row"
      :column-descriptions-R column-descriptions-R
      :render-dom render-table-virtual-row-DOM
      :get-rendering-data get-table-virtual-row-rendering-data
      :sibling true
      :template row-template
      :get-action-data [composed-get-action-data
                        [get-id-action-data adjacent-id] ; our sibling
                        get-virtual-action-data]}))

(defn get-table-rows-rendering-data
  [{:keys [row-template-R row-ids-R]} mutable-store]
  [[row-template-R [universal-category]]
   [row-ids-R [universal-category]]])

(defn render-table-rows-DOM
  "The specification must have
   column-descriptions-R row-template-R and row-ids-R."
  [specification row-template row-ids]
  ;; We pass on column-descriptions-R.
  (let [row-spec (dissoc specification
                         :row-template-R :row-ids-R :id-with-no-subject
                         :get-action-data)]
    (into [:div {:class "table-rows"}]
          (concat (map #(table-row-component % row-template row-spec)
                       row-ids)
                  [(table-virtual-row-DOM-component
                    row-template
                    (or (last row-ids) (:id-with-no-subject specification))
                    (:column-descriptions-R specification))]))))

(defn table-hierarchy-R
  "Return a reporter whose value is the hierarchy of the table header."
  [column-headers-R]
  (expr-let [current-headers column-headers-R]
    (let [columns (ordered-entities (semantic-elements current-headers))]
      (replace-hierarchy-leaves-by-nodes (hierarchy-by-labels columns)))))

(defn table-row-ids-R
  "Return a reporter whose value is the row ids for the table, in order."
  [row-template-R mutable-store]
  (expr-let [current-template row-template-R]
    (let [row-query (pattern-to-fixed-term current-template)
          matching-ids-R (matching-item-ids-R row-query mutable-store)]
      (ordered-ids-R matching-ids-R mutable-store))))

(defn table-hierarchy-leaf-column-description
  [parent-node node]
  (let [leaf (first (:leaves node))
        query (exemplar-to-fixed-term (:item leaf))
        competitors (when (and  parent-node (empty? (:properties node)))
                      (hierarchy-node-non-immediate-descendant-cover
                       parent-node))]
    (cond-> {:column-id (:item-id (:item leaf))
             :query query
             :width 0.75}
      (seq competitors) 
      (assoc :competing-ids (map #(:item-id (:item %)) competitors) 
             :disqualifications (map #(exemplar-to-fixed-term (:item %))
                                     competitors)))))

(defn table-hierarchy-node-column-descriptions
  "Given a hierarchy node, for each column under the node,
  return a map:
             :column-id  The id that identifies the column.
                         (the id of the column item)
                 :width  The width of the column
                 :query  Query that each element of the column must satisfy.
                         For a virtual column, this will not be present.
     :disqualifications  Seq of conditions that elements must not satisfy,
                         even if they satisfy the query.
         :competing-ids  Seq of ids whose matches must not appear in the cell.
                         this determines the :disqualifications. It is not
                         used in rendering the dom, only the disqualifications
                         are. But it is currently used in going to batch edit."
  [parent-node node]
  (if-let [children (:child-nodes node)]
    (mapcat #(table-hierarchy-node-column-descriptions node %)
            children)
    [(table-hierarchy-leaf-column-description parent-node node)]))

(defn get-table-rendering-data
  [spec mutable-store]
  ;; We pass the render the current store, with a dependency on the
  ;; table spec.
  ;; We also pass it the mutable store, which it uses
  ;; to build the reporters that each of its subparts depend on.
  ;; The way we give it the mutable store is by making a map consisting of
  ;; the store, so current-value will return the entire map, not the
  ;; current value of the store.
  [[mutable-store [(:table-id spec)]]
   [{:mutable-store mutable-store} nil]])

(defn render-table-DOM
  "Return a hiccup representation of DOM, with the given internal key,
  describing a table."
  ;; The format of the element that describes a table is given in
  ;; model-utils.
  [{:keys [table-id]} immutable-store {:keys [mutable-store]}]
  (println "Generating DOM for table" (simplify-for-print table-id))
  ;; First check to see if we have the table information filled in yet.
  (let [row-condition-id (first (target-label->ids
                                 immutable-store table-id :row-condition))
        column-headers-id (first (target-label->ids
                                  immutable-store table-id :column-headers))]
    ;; Render the table only if the table information has been filled in.
    (if (not (and row-condition-id column-headers-id))
      [:div {}]
      (let [table-R (description->updating-entity-R table-id mutable-store)
            row-template-R (expr table-row-template table-R)
            column-headers-R (expr label->element table-R :column-headers)
            hierarchy-R (table-hierarchy-R column-headers-R)
            row-ids-R (table-row-ids-R row-template-R mutable-store)
            virtual-column-description {:column-id :virtualColumn}
            ;; TODO: Add an "other" column if a table requests it.
            column-descriptions-R
            (expr-let [hierarchy hierarchy-R]
              (concat
               (mapcat #(table-hierarchy-node-column-descriptions nil %)
                       hierarchy)
               [virtual-column-description]))
            id-with-no-subject (loop [id table-id]
                                 (let [target (id->target
                                                immutable-store id)]
                                   (if target
                                     (recur target)
                                     id)))
            condition-dom (make-component
                           {:relative-id row-condition-id
                            :render-dom render-table-condition-DOM
                            :get-rendering-data
                            get-table-condition-rendering-data
                            :get-do-batch-edit-action-data
                            get-table-condition-do-batch-edit-action-data })
            header-dom (make-component
                        {:relative-id column-headers-id
                         :hierarchy-R hierarchy-R
                         :render-dom render-table-header-DOM
                         :get-rendering-data get-table-header-rendering-data})
            body-dom (make-component
                      {:relative-id :body
                       ;; This is used as a sibling of our initial row.
                       :id-with-no-subject id-with-no-subject
                       :column-descriptions-R column-descriptions-R
                       :row-template-R row-template-R
                       :row-ids-R row-ids-R
                       :render-dom render-table-rows-DOM
                       :get-rendering-data get-table-rows-rendering-data
                       :get-action-data get-pass-through-action-data})]
        [:div {:class "table"}
         condition-dom
         [:div {:class "table-main"}
          header-dom
          body-dom]]))))

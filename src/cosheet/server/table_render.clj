(ns cosheet.server.table-render
  (:require (cosheet [utils :refer [replace-in-seqs multiset separate-by
                                     remove-first]]
                      [store :refer [id->target target-label->ids]]
                      [reporter :refer [universal-category]]
                      [entity :refer [content elements label->elements
                                      id->entity
                                      id->updating-entity-R
                                      label-element? label->element]]
                      [query :refer [matching-elements matching-items
                                     extended-by?]]
                      [query-calculator :refer [matching-item-ids-R]]
                      [debug :refer [simplify-for-print]]
                      [hiccup-utils :refer [dom-attributes
                                            into-attributes add-attributes]]
                      [reporter-macros :refer [app-R let-R]])
            (cosheet.server
             [hierarchy :refer [hierarchy-node? hierarchy-node-descendants
                                replace-hierarchy-leaves-by-nodes
                                hierarchy-node-leaves
                                hierarchy-node-next-level
                                hierarchy-nodes-extent
                                hierarchy-by-labels
                                hierarchy-node-non-immediate-descendant-cover]]
             [order-utils :refer [ordered-ids-R ordered-entities]]
             [model-utils :refer [table-column-headers-id
                                  table-row-condition-id
                                  table-column-headers-element
                                  table-row-condition-element
                                  table-row-condition-object
                                  object-semantic-to-tree
                                  semantic-to-tree
                                  semantic-elements
                                  pattern-to-fixed-term fixed-term-to-template
                                  add-non-selector-to-fixed-term
                                  column-header-template
                                  unspecified-column-header-template
                                  exemplar-to-fixed-term]]
             [render-utils :refer [make-component
                                   hierarchy-node-DOM
                                   condition-satisfiers
                                   transform-specification-for-elements]]
             [item-render :refer [virtual-DOM-component
                                  render-virtual-DOM
                                  virtual-element-and-label-DOM
                                  label-stack-DOM
                                  element-content-DOM
                                  labels-and-elements-DOM
                                  non-label-elements-DOM
                                  hierarchy-node-labels-DOM]]
             [action-data :refer [default-get-action-data
                                  get-id-action-data
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
        column-headers (id->entity column-headers-id immutable-store)
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
        template (semantic-to-tree (id->entity new-column-id store))]
    (get-virtual-action-data
     {:template template} containing-action-data action store)))

(defmethod print-method
  cosheet.server.table_render$get_virtual_column_cell_action_data
  [v ^java.io.Writer w]
  (.write w "virt-col-cell-AD"))

(defn get-table-condition-do-batch-edit-action-data
  [specification containing-action-data action immutable-store]
  (let [row-condition-id (table-row-condition-id
                          (:table-id containing-action-data) immutable-store)
        row-condition (id->entity row-condition-id immutable-store)
        condition-elements (semantic-elements (content row-condition))
        query-ids (map :item-id condition-elements)]
    (assoc containing-action-data
           :query-ids query-ids
           :stack-ids query-ids
           :must-show-label true)))

(defmethod print-method
  cosheet.server.table_render$get_table_condition_do_batch_edit_action_data
  [v ^java.io.Writer w]
  (.write w "table-cond-do-batch-AD"))

(defn get-table-header-do-batch-edit-action-data
  [{:keys [auxiliary-item-id relative-id column-ids competing-ids]}
   containing-action-data action immutable-store]
  (let [id (or auxiliary-item-id relative-id)
        row-condition-id (table-row-condition-id
                          (:table-id containing-action-data) immutable-store)
        row-condition (id->entity row-condition-id immutable-store)
        condition-elements (semantic-elements (content row-condition))
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
  cosheet.server.table_render$get_table_header_do_batch_edit_action_data
  [v ^java.io.Writer w]
  (.write w "table-head-do-batch-AD"))

(defn get-table-cell-do-batch-edit-action-data
  "Generate the batch edit information for a cell that is independent
  of its items."
  [{:keys [competing-ids]}
   containing-action-data action immutable-store]
  (let [row-condition-id (table-row-condition-id
                          (:table-id containing-action-data) immutable-store)
        row-condition (id->entity row-condition-id immutable-store)
        condition-elements (semantic-elements (content row-condition))
        query-ids (map :item-id condition-elements)]
    (assoc containing-action-data
           :query-ids query-ids
           :stack-ids competing-ids)))

(defmethod print-method
  cosheet.server.table_render$get_table_cell_do_batch_edit_action_data
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
  cosheet.server.table_render$get_table_cell_item_do_batch_edit_action_data
  [v ^java.io.Writer w]
  (.write w "table-cell-item-do-batch-AD"))

;;; TODO: !!! This should be changed to use a horizontal object
;;; layout, which ought to be able to replace most of this. That would
;;; get rid of the :link-type, which should be :object-type.
(defn render-table-condition-DOM-R
  "Return a hiccup representation for the top of a table, the part that
  holds its condition. The relative-id should be for the row-condition
  object."
  [{:keys [relative-id] :as spec} store]
  (let-R [row-condition-object (id->updating-entity-R relative-id store)]
    (let [condition-elements (semantic-elements row-condition-object)
          spec-down {:template 'anything
                     :width 0.75}
          last-item (last (ordered-entities
                           (remove label-element? condition-elements)))
          virtual-dom
          (add-attributes
           (virtual-element-and-label-DOM
            (cond-> (assoc spec-down
                           :relative-id :virtual)
              ;; If we have any headers already, put the new one after
              ;; the last of them.
              last-item
              (assoc :auxiliary-item-id (:item-id last-item)
                     :get-action-data get-item-or-exemplar-action-data
                     :sibling true))
            :vertical)
           {:class "virtual-column"})
          dom (labels-and-elements-DOM
               condition-elements virtual-dom
               true true :horizontal spec-down :link-type)]
      (add-attributes dom {:class "query-condition"}))))

(defn table-hierarchy-node-disqualifications
  "Given a hierarchy node, return a seq of conditions that immediate
  elements of the node must not satisfy, because they are covered
  by sub-nodes."
  [node]
  (map #(pattern-to-fixed-term (semantic-to-tree (:item %)))
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
        node-dom (hierarchy-node-labels-DOM
                  node spec)
        is-leaf (empty? child-doms)
        class (cond-> "column-header"
                is-leaf (str " leaf"))]
    (if is-leaf
      (add-attributes node-dom {:class class})
      [:div {:class (str class " link-type")}
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
      (virtual-element-and-label-DOM spec :vertical)
      (let [last-column (last (hierarchy-node-descendants (last hierarchy)))
            last-column-id (:item-id (:item last-column))]
        (add-attributes
         (virtual-element-and-label-DOM
          (assoc spec
                 :get-action-data get-item-or-exemplar-action-data
                 :auxiliary-item-id last-column-id
                 :sibling true)
          :vertical)
         {:class  "column-header virtual-column"})))))

(defn render-table-header-DOM-R
  "Generate DOM for column headers given the hierarchy.
  The column will contain those elements of the rows that match the templates
  in the hierarchy."
  [{:keys [hierarchy-R] :as spec} _]
  (let-R [hierarchy hierarchy-R]
    (let [doms (map table-header-top-level-subtree-DOM hierarchy)
          virtual-header (table-virtual-column-header-DOM hierarchy)]
      (into [:div {:class "column-header-sequence table-header"}]
            (concat doms [virtual-header])))))

(defn table-virtual-column-cell-DOM-component
  [specification]
  (add-attributes
   (make-component
    (assoc specification
           :relative-id :virtual
           :virtual true
           :template ""
           :render-dom render-virtual-DOM
           :get-action-data get-virtual-column-cell-action-data))
   {:class "table-cell virtual-column has-border"}))

;;; TODO: This isn't generating the right batch edit action data for
;;; labels of its items.
(defn render-table-cell-DOM-R
  [{:keys [row-id query disqualifications] :as specification} store]
  (let-R [row-entity (id->updating-entity-R row-id store)]
    (let [matches (matching-elements query row-entity)
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
        ;; Add a virtual "filler" after the elements when the cell has more
        ;; than one entity, or when it has a single entity that has a
        ;; semantic element beyond what the query requires. It grows to
        ;; fill any free space in the cell, so clicking there acts like
        ;; typing into a virtual element that adds another element to the
        ;; cell.
        (let [filler (when (or (> (count entities) 1)
                               (when-let [entity (first entities)]
                                 (seq (remove (set (condition-satisfiers
                                                    entity (:template spec)))
                                              (semantic-elements entity)))))
                       (virtual-DOM-component
                        (assoc spec :relative-id :virtual
                               :class "stack-filler"
                               :adjacent-query query)))]
          (non-label-elements-DOM
           entities (:template spec) false filler :vertical
           non-virtual-spec))))))

(defmethod print-method
  cosheet.server.table_render$render_table_cell_DOM_R
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
                :render-dom render-table-cell-DOM-R
                :get-action-data get-pass-through-action-data
                :get-do-batch-edit-action-data
                get-table-cell-do-batch-edit-action-data)
         (into (select-keys column-description
                            [:query :competing-ids
                             :disqualifications
                             :width]))))))

(defn render-table-row-DOM-R
  "Generate dom for a table row.
  The specification must have column-descriptions-R"
  [{:keys [row-id column-descriptions-R] :as specification} store]
  (let-R [column-descriptions column-descriptions-R]
    (let [spec (-> specification
                   (dissoc :column-descriptions-R)
                   (assoc :class "table-cell has-border"))]
      (let [cells (map #(table-cell-DOM-component % spec)
                       column-descriptions)]
        (into [:div {}] cells)))))

(defmethod print-method
  cosheet.server.table_render$render_table_row_DOM_R
  [v ^java.io.Writer w]
  (.write w "row-DOM"))

(defn table-row-component
  ;; The specification must include column-descriptions-R
  [row-id specification]
  (make-component
   ;; The incoming specification must have :column-descriptions-R.
   (assoc specification
          :relative-id row-id
          :row-id row-id ; Action data passes this down to everything
                         ; in the row.
          :class "table-row"
          :render-dom render-table-row-DOM-R
          :get-action-data [get-id-action-data row-id])))

(defn table-virtual-row-cell-DOM-component
  [{:keys [column-id query width] :as column-description}]
  (make-component
   {:relative-id column-id
    :column-ids [column-id]
    :class "table-cell"
    :virtual true
    :render-dom render-virtual-DOM
    :template (fixed-term-to-template query)
    :get-action-data get-virtual-action-data
    :width width}))

(defn render-table-virtual-row-DOM-R
  "Generate dom for a table's virtual row."
  [{:keys [column-descriptions-R]} store]
  (let-R [column-descriptions column-descriptions-R]
    (let [cells (map table-virtual-row-cell-DOM-component
                     ;; Don't make a cell for the virtual column.
                     (butlast column-descriptions))]
      (into [:div {:class "table-row"}] cells))))

(defmethod print-method
  cosheet.server.table_render$render_table_virtual_row_DOM_R
  [v ^java.io.Writer w]
  (.write w "virt-row-DOM"))

(defn table-virtual-row-DOM-component-R
  "Generate the component for a table's virtual row."
  [row-template-R column-descriptions-R adjacent-id]
  ;; We need the value of the row-template, even though our renderer
  ;; doesn't use it, because the action data needs it to be in the
  ;; spec.
  (let-R [row-template row-template-R]
    (make-component
     {:relative-id :virtual-row
      :class "table-row"
      :column-descriptions-R column-descriptions-R
      :render-dom render-table-virtual-row-DOM-R
      :sibling true
      :template row-template
      :get-action-data [composed-get-action-data
                        [get-id-action-data adjacent-id] ; our sibling
                        get-virtual-action-data]})))

(defn render-table-rows-DOM-R
  [{:keys [row-ids-R row-template-R column-descriptions-R] :as specification}
   store]
  ;; We get the current values of the information that is needed for
  ;; all rows.
  (let-R [row-ids row-ids-R]
    (let [row-spec (dissoc specification
                           :row-ids-R :row-template-R
                           :get-action-data :alternate-row-sibling)
          non-virtual-rows (map #(table-row-component % row-spec)
                                row-ids)]
      (let-R [virtual-row (table-virtual-row-DOM-component-R
                          row-template-R
                          column-descriptions-R
                          (or (last row-ids)
                              (:alternate-row-sibling specification)))]
        (into [:div {:class "table-rows"}]
            (concat non-virtual-rows
                    [virtual-row]))))))

(defn table-hierarchy-R
  "Return a reporter whose value is the hierarchy of the table header."
  [column-headers-R]
  (let-R [current-headers column-headers-R]
    (let [columns (ordered-entities (semantic-elements current-headers))]
      (replace-hierarchy-leaves-by-nodes (hierarchy-by-labels columns)))))

(defn table-row-ids-R
  "Return a reporter whose value is the row ids for the table, in order."
  [row-template-R mutable-store]
  (let-R [current-template row-template-R]
    (let [row-query (-> (pattern-to-fixed-term current-template)
                        add-non-selector-to-fixed-term)
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

(defn render-table-DOM-R
  "Return a hiccup representation of DOM, with the given internal key,
  describing a table."
  ;; The format of the element that describes a table is given in
  ;; model-utils.
  [{:keys [table-id]} store]
  (println "Generating DOM for table" (simplify-for-print table-id))
  ;; We first get just the ids of the main parts of the table
  ;; description.
  (let-R [[row-condition-id column-headers-id]
          ;; Even though this computation will be redone whenever
          ;; the table description changes, its result won't
          ;; change, because the identities of the main parts don't
          ;; change once they are created. So that won't trigger
          ;; recomputation of the main body of the function.
          (let-R [table-item (id->updating-entity-R
                             table-id store)]
            [(:item-id (table-row-condition-object table-item))
             (:item-id (table-column-headers-element table-item))])]
    ;; First check to see if we have the table information filled in yet.
    ;; Render the table only if the table information has been filled in.
    (if (not (and row-condition-id column-headers-id))
      [:div {}]
      ;; Suppose the table's row condition changes. We want to reuse
      ;; all the rows that still pass the new condition; we don't
      ;; want to send them to the client all over again. Similarly,
      ;; suppose the table adds a new column. All the rows have to
      ;; change, but we want to reuse all their existing cells, only
      ;; sending the client the cells for the new column.
      ;;
      ;; To support this reuse, we first make reporters for all the
      ;; information that controls the table layout. That way, all
      ;; uses of them can be shared. But then we don't access their
      ;; values until inside the rendering of components that
      ;; directly need them to produce their DOM. In particular,
      ;; higher level components that don't need the values to
      ;; produce their DOM just pass the reporters on in the
      ;; specifications of their subcomponents, without accessing
      ;; their values.
      ;;
      ;; This way, changes to the values don't invalidate the high
      ;; level components. And even for components whose DOM does
      ;; depend on the values, the dom manager will reuse the
      ;; component atoms of any subcomponents that don't change.
      (let [;; Making these two reporters this way takes advantage of
            ;; the row-condition and column headers never changing
            ;; their identity, even though they can change their
            ;; contents. Tbis way, these reporters don't have to be
            ;; reconstructed whenever part of the table description
            ;; changes. That makes computations that depend on them
            ;; not depend on changes elsewhere in the table entity.
            row-template-R (app-R object-semantic-to-tree
                                 (id->updating-entity-R
                                  row-condition-id store))
            column-headers-R (id->updating-entity-R
                              column-headers-id store)
            hierarchy-R (table-hierarchy-R column-headers-R)
            row-ids-R (table-row-ids-R row-template-R store)
            virtual-column-description {:column-id :virtualColumn}
            ;; TODO: Add an "other" column if a table requests it.
            column-descriptions-R
            (let-R [hierarchy hierarchy-R]
              (concat
               (mapcat #(table-hierarchy-node-column-descriptions nil %)
                       hierarchy)
               [virtual-column-description]))
            condition-dom (make-component
                           {:relative-id row-condition-id
                            :render-dom render-table-condition-DOM-R
                            :get-action-data default-get-action-data
                            :get-do-batch-edit-action-data
                            get-table-condition-do-batch-edit-action-data })
            header-dom (make-component
                        {:relative-id column-headers-id
                         :hierarchy-R hierarchy-R
                         :render-dom render-table-header-DOM-R
                         :get-action-data default-get-action-data})
            body-dom (make-component
                      {:relative-id :body
                       ;; If there are no rows, this is used as the
                       ;; sibling of our first row.
                       :alternate-row-sibling column-headers-id
                       :column-descriptions-R column-descriptions-R
                       :row-template-R row-template-R
                       :row-ids-R row-ids-R
                       :render-dom render-table-rows-DOM-R
                       :get-action-data get-pass-through-action-data})]
        [:div {:class "table"}
         condition-dom
         [:div {:class "table-main"}
          header-dom
          body-dom]]))))

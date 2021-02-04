(ns cosheet2.server.table-render
  (:require (cosheet2 [utils :refer [replace-in-seqs multiset separate-by
                                     add-elements-to-entity-list remove-first]]
                      [store :refer [id->subject]]
                      [reporter :refer [universal-category]]
                      [entity :refer [subject content label->elements
                                      description->entity StoredEntity
                                      updating-immutable label?
                                      ;; TODO: Remove this.
                                      to-list]]
                      [query :refer [matching-elements matching-items
                                     extended-by?]]
                      [query-calculator :refer [matching-item-ids-R]]
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
             [order-utils :refer [ordered-ids-R ordered-entities]]
             [model-utils :refer [semantic-to-list
                                  semantic-elements semantic-non-label-elements
                                  pattern-to-query query-to-template
                                  table-header-template exemplar-to-query]]
             [render-utils :refer [make-component
                                   hierarchy-node-DOM
                                   transform-specification-for-elements]]
             [item-render :refer [virtual-DOM-component
                                  render-virtual-DOM
                                  get-virtual-DOM-rendering-data
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

(defn get-column-action-data
  "Add the action data for a command that acts on a header. (One
  header can span multiple columns.)"
  [{:keys [header-id descendant-ids]}
   containing-action-data action immutable-store]
  (assoc containing-action-data :column
         {:header-id header-id
          :column-ids descendant-ids}))

(defmethod print-method
  cosheet2.server.table_render$get_column_action_data
  [v ^java.io.Writer w]
  (.write w "col-AD"))

(defn get-row-action-data
  "Add the action data for a command that acts on a row."
  [{:keys [relative-id row-template]}
   containing-action-data action immutable-store]
  (assoc containing-action-data :row
         {:row-id relative-id
          :row-template row-template}))

(defmethod print-method
  cosheet2.server.table_render$get_row_action_data
  [v ^java.io.Writer w]
  (.write w "row-AD"))

(defn get-virtual-column-cell-action-data
  "Create a new column header and an element under that column in the row.
   The containing data's target-ids are the id of the row."
  [specification containing-action-data action immutable-store
   header-id]
  (let [header (description->entity header-id immutable-store)
        columns (label->elements header :column)
        last-column-id (:item-id (last (ordered-entities columns)))
        {:keys [store target-ids]}
        (get-virtual-action-data
         {:sibling true
          :template (concat table-header-template ['(??? :label)])}
         {:target-ids [last-column-id]} action immutable-store)
        new-column-id (first target-ids)
        template (semantic-to-list (description->entity new-column-id store))]
    (get-virtual-action-data
     {:template template} containing-action-data action store)))

(defmethod print-method
  cosheet2.server.table_render$get_virtual_column_cell_action_data
  [v ^java.io.Writer w]
  (.write w "virt-col-cell-AD"))

;;; The condition elements of a table are its semantic elements
;;; that are not column headers.
(defn table-condition-elements [header-entity]
  (expr-let
      [semantic-elements (semantic-elements header-entity)
       column-elements (label->elements header-entity :column)]
    (remove (set column-elements) semantic-elements)))

(defn get-table-condition-do-batch-edit-action-data
  [{:keys [header-id]}
   containing-action-data action immutable-store]
  (let [header-entity (description->entity header-id immutable-store)
        condition-elements (table-condition-elements header-entity)
        query-ids (map :item-id condition-elements)]
    (assoc containing-action-data
           :batch-edit-ids query-ids
           :stack-selector-index (count query-ids))))

(defmethod print-method
  cosheet2.server.table_render$get_table_condition_do_batch_edit_action_data
  [v ^java.io.Writer w]
  (.write w "table-cond-do-batch-AD"))

;;; TODO: Why is this setting selected-index. The item in the header
;;; should do that.
(defn get-table-header-do-batch-edit-action-data
  [{:keys [item-id relative-id header-id descendant-ids competing-ids]}
   containing-action-data action immutable-store]
  (let [id (or item-id relative-id)
        header-entity (description->entity header-id immutable-store)
        condition-elements (table-condition-elements header-entity)
        query-ids (map :item-id condition-elements)
        stack-selector-ids (concat (when (= (count descendant-ids) 1)
                                     competing-ids)
                                   descendant-ids)
        ids (concat query-ids stack-selector-ids)]
    (assoc containing-action-data
           :batch-edit-ids ids
           :stack-selector-index (count query-ids)
           :selected-index (let [index (.indexOf ids id)]
                             (when (>= index 0) index)))))

(defmethod print-method
  cosheet2.server.table_render$get_table_header_do_batch_edit_action_data
  [v ^java.io.Writer w]
  (.write w "table-head-do-batch-AD"))

(defn get-table-cell-do-batch-edit-action-data
  "Generate the batch edit information for a cell that is independent
  of its items."
  [{:keys [competing-ids header-id]}
   containing-action-data action immutable-store]
  (let [header-entity (description->entity header-id immutable-store)
        condition-elements (table-condition-elements header-entity)
        query-ids (map :item-id condition-elements)]
    (assoc containing-action-data
           :batch-edit-ids (concat query-ids competing-ids)
           :stack-selector-index (count query-ids))))

(defmethod print-method
  cosheet2.server.table_render$get_table_cell_do_batch_edit_action_data
  [v ^java.io.Writer w]
  (.write w "table-cell-do-batch-AD"))

(defn get-table-cell-item-do-batch-edit-action-data
  [{:keys [item-id relative-id] :as specification}
   {:keys [batch-edit-ids stack-selector-index] :as containing-action-data}
    action immutable-store]
  (let [id (or item-id relative-id)
        num-ids (count batch-edit-ids)]
    (assert id specification)
    (assert batch-edit-ids containing-action-data)
    (assoc containing-action-data
           :batch-edit-ids (concat batch-edit-ids [id])
           :selected-index num-ids)))

(defmethod print-method
  cosheet2.server.table_render$get_table_cell_item_do_batch_edit_action_data
  [v ^java.io.Writer w]
  (.write w "table-cell-item-do-batch-AD"))

(defn get-table-condition-rendering-data
  [{:keys [header-id]} mutable-store]
  [[mutable-store [header-id]]])

(defn render-table-condition-DOM
  "Return a hiccup representation for the top of a table, the part that
  holds its condition. The relative-id should be for the header"
  [{:keys [header-id]} store]
  (let [header-entity (description->entity header-id store)
        condition-elements (table-condition-elements header-entity)
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
  (map #(pattern-to-query (semantic-to-list (:item %)))
       (hierarchy-node-non-immediate-descendant-cover node)))

(defn table-header-node-specification
  "Return the specification to use for a DOM that is part of a table header."
  [header-id node parent-cover]
  (let [descendant-ids (map #(:item-id (:item %))
                            (hierarchy-node-descendants node))]
    (cond->
        {:header-id header-id
         :descendant-ids descendant-ids
         :get-column-action-data get-column-action-data
         :get-do-batch-edit-action-data
         get-table-header-do-batch-edit-action-data
         :width (* 0.75 (count descendant-ids))
         ;; Tell add-twin and delete that they must not add or remove a column.
         :template :singular}
      (and (seq parent-cover) (empty? (:properties node)))
      (assoc :competing-ids parent-cover))))

(defn table-header-subtree-DOM
  "Generate the dom for a subtree of a table header hierarchy, given
  the doms for all the children."
  [header-id node child-doms specification]
  (let [spec (into (dissoc specification :parent-cover-ids :competing-ids)
                   (table-header-node-specification
                    header-id node (:parent-cover-ids specification)))
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
  [header-id node]
  (hierarchy-node-DOM
   node
   (partial table-header-subtree-DOM header-id)
   (fn [node spec]
     (let [cover (hierarchy-node-non-immediate-descendant-cover node)]
       (-> spec
           (dissoc :top-level)
           (assoc :parent-cover-ids (map #(:item-id (:item %)) cover)))))
   {:top-level true}))

(defn table-virtual-column-header-DOM
  [hierarchy]
  (let [spec {:relative-id :virtual-column
              :template table-header-template
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
  [{:keys [header-id]} hierarchy]
  (let [doms (map #(table-header-top-level-subtree-DOM header-id %)
                  hierarchy)
        virtual-header (table-virtual-column-header-DOM hierarchy)]
    (into [:div {:class "column-header-sequence table-header"}]
          (concat doms [virtual-header]))))

(defn table-virtual-column-cell-DOM-component
  [header-id specification]
  (add-attributes
   (make-component
    (assoc specification
           :relative-id :virtual
           :template ""
           :render-dom render-virtual-DOM
           :get-rendering-data get-virtual-DOM-rendering-data
           :get-action-data [get-virtual-column-cell-action-data header-id]))
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
                   (let [do-not-show (map #(matching-elements % row-entity)
                                          disqualifications)]
                     (seq (clojure.set/difference
                           (set matches) (set (apply concat do-not-show)))))
                   matches)
        spec (-> specification
                 transform-specification-for-elements
                 (assoc :template (query-to-template query)))
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
  "Return a component for one cell of a table, given its row item and
  column description.  We need to put each table cell in a component,
  so its column-id can be included in its client id. Otherwise, if
  several columns show the same item, we could have the same client id
  for both."
  [row-id
   {:keys [column-id width] :as column-description}
   specification]
  (if (= column-id :virtualColumn)
    (table-virtual-column-cell-DOM-component
     (:header-id column-description) (assoc specification :width width))
    (make-component
     (into (assoc specification
                  :relative-id column-id
                  :row-id row-id
                  :class "table-cell"
                  :render-dom render-table-cell-DOM
                  :get-rendering-data get-table-cell-rendering-data
                  :get-action-data get-pass-through-action-data
                  :get-do-batch-edit-action-data
                  get-table-cell-do-batch-edit-action-data)
           (select-keys column-description
                        [:query :header-id :competing-ids :disqualifications
                         :width])))))

(defn get-table-row-rendering-data
  [{:keys [relative-id column-descriptions-R]} mutable-store]
  [[mutable-store [relative-id]]
   [column-descriptions-R [universal-category]]])

(defn render-table-row-DOM
  "Generate dom for a table row. The specification must already
  have column-descriptions-R and :get-row-action-data"
  [{:keys [relative-id] :as specification} store column-descriptions]
  (let [spec (-> specification
                 (dissoc
                  :column-descriptions-R :get-row-action-data :row-template)
                 (assoc :class "table-cell has-border"))]
    (let [cells (map #(table-cell-DOM-component relative-id % spec)
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
          :row-template row-template
          :class "table-row"
          :render-dom render-table-row-DOM
          :get-action-data [get-id-action-data row-id]
          :get-rendering-data get-table-row-rendering-data
          :get-row-action-data get-row-action-data)))

(defn table-virtual-row-cell-DOM-component
  [{:keys [column-id query width] :as column-description}]
  (make-component
   {:relative-id column-id 
    :class "table-cell"
    :render-dom render-virtual-DOM
    :get-rendering-data get-virtual-DOM-rendering-data
    :template (query-to-template query)
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
      :item-id adjacent-id
      :sibling true
      :template row-template
      :get-action-data [composed-get-action-data
                        get-item-or-exemplar-action-data
                        get-virtual-action-data]}))

(defn get-table-rows-rendering-data
  [{:keys [row-template-R row-ids-R]} mutable-store]
  [[row-template-R [universal-category]]
   [row-ids-R [universal-category]]])

(defn render-table-rows-DOM
  "The specification must have header-id, column-descriptions-R
  row-template-R and row-ids-R."
  [specification row-template row-ids]
  ;; We pass on column-descriptions-R from our spec
  (let [row-spec (-> specification
                     (dissoc :row-template-R :row-ids-R :get-action-data))]
    (into [:div {:class "table-rows"}]
          (concat (map #(table-row-component % row-template row-spec)
                       row-ids)
                  [(table-virtual-row-DOM-component
                    row-template (or (last row-ids) (:header-id specification))
                    (:column-descriptions-R specification))]))))

(defn table-header-entity-R
  [header-id mutable-store]
  (let [header-entity
        (description->entity header-id mutable-store)]
    (updating-immutable header-entity)))

(defn table-hierarchy-R
  "Return a reporter whose value is the hierarchy of the table header."
  [header-entity-R]
  (expr-let [current-header header-entity-R]
    (let [columns (ordered-entities
                   (label->elements current-header :column))]
      (replace-hierarchy-leaves-by-nodes (hierarchy-by-labels columns)))))

(defn table-row-template-R
  "Return a reporter whose value is the row condition"
  [header-entity-R]
  (expr-let [current-header header-entity-R]
    (let [condition-elements (table-condition-elements current-header)
          elements-as-lists (map semantic-to-list condition-elements)]
      (concat '(anything) elements-as-lists [:top-level]))))

(defn table-row-ids-R
  "Return a reporter whose value is the row ids for the table, in order."
  [row-template-R mutable-store]
  (expr-let [current-template row-template-R]
    (let [row-query (pattern-to-query current-template)
          matching-ids-R (matching-item-ids-R row-query mutable-store)]
      (ordered-ids-R matching-ids-R mutable-store))))

(defn table-hierarchy-leaf-column-description
  [header-id parent-node node]
  (let [leaf (first (:leaves node))
        query (exemplar-to-query (:item leaf))
        competitors (when (and  parent-node (empty? (:properties node)))
                      (hierarchy-node-non-immediate-descendant-cover
                       parent-node))]
    (cond-> {:header-id header-id
             :column-id (:item-id (:item leaf))
             :query query
             :width 0.75}
      (seq competitors) 
      (assoc :competing-ids (map #(:item-id (:item %)) competitors) 
             :disqualifications (map #(exemplar-to-query (:item %))
                                     competitors)))))

(defn table-hierarchy-node-column-descriptions
  "Given a hierarchy node, for each column under the node,
  return a map:
             :header-id  The id that defines the entire table header.
             :column-id  The id that identifies the column.
                         Typically the id of the column item.
                 :query  Query that each element of the column must satisfy.
                         For a virtual column, this will not be present.
         :competing-ids  Seq of ids whose matches must not appear in the cell.
     :disqualifications  Seq of conditions that elements must not satisfy.
                         This is determined by :competing ids, but we put in
                         both, so that table cells can have the
                         disqualifications put in their specification,
                         causing them to be recomputed if the contents of
                         the competing ids change, without their having to
                         register a dependency on it."
  [header-id parent-node node]
  (if-let [children (:child-nodes node)]
    (mapcat #(table-hierarchy-node-column-descriptions header-id node %)
            children)
    [(table-hierarchy-leaf-column-description header-id parent-node node)]))

(defn get-ready-table-rendering-data
  [_ mutable-store]
  ;; We pass the renderer the mutable store. That way, it can use that
  ;; to build the reporters that each of its subparts depend on.
  ;; The way we give it the store is by making a map consisting of
  ;; the store, so current-value will return the entire map, not the
  ;; current value of the store.
  [[{:mutable-store mutable-store} nil]])

(defn render-ready-table-DOM
  "Render a table dom, give its header."
  [{:keys [relative-id]} {:keys [mutable-store]}]
  (let [header-id relative-id
        header-entity-R (table-header-entity-R
                         header-id mutable-store)
        hierarchy-R (table-hierarchy-R header-entity-R)
        row-template-R (table-row-template-R header-entity-R)
        row-ids-R (table-row-ids-R row-template-R mutable-store)
        virtual-column-description {:column-id :virtualColumn
                                    :header-id header-id}
        column-descriptions-R (expr-let [hierarchy hierarchy-R]
                                (concat
                                 (mapcat
                                  #(table-hierarchy-node-column-descriptions
                                    header-id nil %)
                                  hierarchy)
                                 [virtual-column-description]))
        condition-dom (make-component
                       {:relative-id :condition
                        :header-id header-id
                        :render-dom render-table-condition-DOM
                        :get-rendering-data get-table-condition-rendering-data
                        :get-action-data get-pass-through-action-data
                        :get-do-batch-edit-action-data
                        get-table-condition-do-batch-edit-action-data })
        header-dom (make-component
                    {:relative-id :header
                     :header-id header-id
                     :hierarchy-R hierarchy-R
                     :render-dom render-table-header-DOM
                     :get-rendering-data get-table-header-rendering-data
                     :get-action-data get-pass-through-action-data})
        body-dom (make-component
                  {:relative-id :body
                   :header-id header-id
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
      body-dom]]))

(defn render-table-DOM
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
  ;; TODO: Add an "other" column if a table requests it.
  [{:keys [relative-id] :as specification} store]
  (let [table-item (description->entity relative-id store)]
    (println "Generating DOM for table" (simplify-for-print table-item))
    (assert (satisfies? StoredEntity table-item))
    ;; Don't do anything if we don't yet have the table information filled in.
    (if-let [row-condition-item (first (label->elements
                                        table-item :row-condition))]
      (make-component {:relative-id (:item-id row-condition-item)
                       :render-dom render-ready-table-DOM
                       :get-rendering-data get-ready-table-rendering-data})
      [:div {}])))

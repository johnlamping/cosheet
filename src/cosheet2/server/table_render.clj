(ns cosheet2.server.table-render
  (:require (cosheet2 [utils :refer [replace-in-seqs multiset separate-by
                                     add-elements-to-entity-list remove-first]]
                      [reporter :refer [universal-category]]
                      [entity :refer [subject content label->elements
                                      description->entity StoredEntity
                                      updating-immutable]]
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
                                  table-header-template]]
             [render-utils :refer [make-component
                                   hierarchy-node-DOM]]
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
             [action-data :refer [get-item-or-exemplar-action-data-for-ids
                                  get-pass-through-action-data
                                  get-column-action-data
                                  get-row-action-data
                                  get-virtual-column-cell-action-data]])))

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
)

;;; The condition elements of a table are its semantic elements
;;; that are not column headers.
(defn table-condition-elements [header-entity]
  (expr-let
      [semantic-elements (semantic-elements header-entity)
       column-elements (label->elements header-entity :column)]
    (remove (set column-elements) semantic-elements)))

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
        virtual-dom
        (virtual-entity-and-label-DOM
         (assoc spec-down :relative-id :virtual)
         :vertical
         {})
        dom (labels-and-elements-DOM
             condition-elements virtual-dom
             true true :horizontal spec-down)]
    [:div {:class "query-holder label"}
     [:div {:class "query-indent label"}]
     (add-attributes dom {:class "query-condition"})]))

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
              :width 0.75
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

(defn get-table-header-rendering-data
  [{:keys [hierarchy-R] :as spec} mutable-store]
  [[hierarchy-R [universal-category]]])

(defn render-table-header-DOM
  "Generate DOM for column headers given the hierarchy.
  The column will contain those elements of the rows that match the templates
  in the hierarchy."
  [{:keys [header-id]} hierarchy]
  (let [hierarchy (replace-hierarchy-leaves-by-nodes hierarchy)
        doms (map #(table-header-top-level-subtree-DOM header-id %)
                     hierarchy)
        virtual-header (table-virtual-column-header-DOM hierarchy)]
    (into [:div {:class "column-header-sequence"}]
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

;;; TODO: Make all the renderers end in -DOM.

(defn get-table-cell-rendering-data
  [specification mutable-store]
  [[mutable-store [(:row-id specification)]]])

(defmethod print-method
  cosheet2.server.table_render$get_table_cell_rendering_data
  [v ^java.io.Writer w]
  (.write w "cell-RD"))

(defn render-table-cell-DOM
  [{:keys [:row-id :query :disqualifications] :as specification} store]
  (let [row-entity (description->entity row-id store)
        matches (matching-elements query row-entity)
        entities (if disqualifications
                   (let [do-not-show (map #(matching-elements % row-entity)
                                          disqualifications)]
                     (seq (clojure.set/difference
                           (set matches)
                           (set (apply concat do-not-show)))))
                   matches)
        spec (-> specification
                 (assoc :template (query-to-template query))
                 (dissoc :relative-id :render-dom :get-rendering-data
                         :row-id :query :disqualifications))
        dom (if (empty? entities)
              ;; TODO: Get our left neighbor as an arg, and pass it
              ;; in the sibling for the virtual dom.
              (virtual-DOM-component (assoc spec :relative-id :virtual)
                                     {})
              (non-label-entities-DOM
               entities
               (:template spec) false :vertical spec))]
    (add-attributes dom {:class "table-cell has-border"})))

(defmethod print-method
  cosheet2.server.table_render$render_table_cell_DOM
  [v ^java.io.Writer w]
  (.write w "cell-DOM"))

(defn table-cell-DOM-component
  "Return a component for one cell of a table, given its row item and
  column description.  We need to put each table cell in a component,
  so its column-id can be included in its client id. Otherwise, if
  several columns show the same item, we could have the same client id
  for both. "
  [row-id
   {:keys [column-id query disqualifications width] :as column-description}
   specification]
    (if (= column-id :virtualColumn)
      (table-virtual-column-cell-DOM-component
       (:header-id column-description) (assoc specification :width width))
      (make-component
         (assoc specification
                :relative-id column-id
                :row-id row-id
                :query query
                :render-dom render-table-cell-DOM
                :get-rendering-data get-table-cell-rendering-data
                :width width))))

(defn get-table-row-rendering-data
  [{:keys [relative-id column-descriptions-R]} mutable-store]
  [[mutable-store [relative-id]]
   [column-descriptions-R [universal-category]]])

(defn render-table-row-DOM
  "Generate dom for a table row. The specification must already
  have column-descriptions-R and :get-row-action-data"
  [{:keys [relative-id] :as specification} store column-descriptions]
  (let [spec (-> specification
                 (dissoc :column-descriptions-R :get-row-action-data)
                 (update :priority inc))]
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
          :render-dom render-table-row-DOM
          :get-rendering-data get-table-row-rendering-data
          :get-row-action-data [get-row-action-data row-id row-template])))

(defn get-table-rows-rendering-data
  [{:keys [row-template-R row-ids-R]} mutable-store]
  [[row-template-R [universal-category]]
   [row-ids-R [universal-category]]])

(defn render-table-rows-DOM
  "The specification must have column-descriptions-R row-template-R
  and row-ids-R."
  [specification row-template row-ids]
  ;; We pass on column-descriptions-R from our spec
  (let [row-spec (-> specification
                     (dissoc :row-template-R :row-ids-R)
                     (update :priority (partial + 2)))]
    (into [:div {:class "table-rows"}]
          (map #(table-row-component % row-template row-spec)
               row-ids)
          ;; TODO: Concat in a virtual row
          )))

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
      (hierarchy-by-labels columns))))

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
  [parent-node leaf]
  (let [query (-> (:item leaf)
                  semantic-to-list
                  pattern-to-query)
        disqualifications (table-hierarchy-node-disqualifications parent-node)]
    [(cond-> {:column-id (:item-id (:item leaf))
              :query query
              :width 0.75}
       (seq disqualifications)
       (assoc :disqualifications disqualifications))]))

(defn table-hierarchy-node-column-descriptions
  "Given a hierarchy node, for each column under the node,
  return a map:
              :column-id Id that identifies the column.
                         Typically the id of the column item.
                  :query Query that each element of the column must satisfy.
                         For a virtual column, this will not be present.
      :disqualifications Seq of conditions that elements must not satisfy."
  [node]
  (mapcat (fn [node-or-element]
            (if (hierarchy-node? node-or-element)
              (table-hierarchy-node-column-descriptions node-or-element)
              (table-hierarchy-leaf-column-description node node-or-element)))
          (hierarchy-node-next-level node)))

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
                                  table-hierarchy-node-column-descriptions
                                  hierarchy)
                                 [virtual-column-description]))
        condition-dom (make-component
                       {:relative-id :condition
                        :header-id header-id
                        :priority 1
                        :render-dom render-table-condition-DOM
                        :get-rendering-data get-table-condition-rendering-data
                        :get-action-data get-pass-through-action-data})
        header-dom (make-component
                    {:relative-id :header
                     :header-id header-id
                     :hierarchy-R hierarchy-R
                     :priority 1
                     :render-dom render-table-header-DOM
                     :get-rendering-data get-table-header-rendering-data
                     :get-action-data get-pass-through-action-data})
        body-dom (make-component
                  {:relative-id :body
                   :header-id header-id
                   :column-descriptions-R column-descriptions-R
                   :row-template-R row-template-R
                   :row-ids-R row-ids-R
                   :priority 1
                   :render-dom render-table-rows-DOM
                   :get-rendering-data get-table-rows-rendering-data
                   :get-action-data get-pass-through-action-data})]
    [:div {:class "table"}
     condition-dom
     [:div {:class "query-result-wrapper"}
      [:div {:class "query-result-indent label"}]
      [:div {:class "table-main"}
       header-dom
       body-dom]]]))

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
  ;; TODO: Add the "other" column if a table requests it.
  [{:keys [relative-id] :as specification} store]
  (let [table-item (description->entity relative-id store)]
    (println "Generating DOM for table" (simplify-for-print table-item))
    (assert (satisfies? StoredEntity table-item))
    ;; Don't do anything if we don't yet have the table information filled in.
    (into
     [:div {}]
     (when-let [row-condition-item (first (label->elements
                                           table-item :row-condition))]
       [(make-component {:relative-id (:item-id row-condition-item)
                         :render-dom render-ready-table-DOM
                         :get-rendering-data get-ready-table-rendering-data})]
       ))))

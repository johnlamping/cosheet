(ns cosheet.server.table-render
  (:require (cosheet [utils :refer [replace-in-seqs multiset]]
                     [entity :as entity]
                     [query :refer [matching-elements matching-items]]
                     [debug :refer [simplify-for-print]]
                     [orderable :as orderable]
                     [dom-utils :refer [dom-attributes
                                        into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq cache]]
                     [canonical :refer [canonicalize-list canonical-to-list
                                        canonical-set-to-list
                                        common-canonical-multisets]])
            (cosheet.server
             [referent :refer [item-referent
                               elements-referent query-referent
                               union-referent-if-needed union-referent
                               parallel-union-referent
                               difference-referent
                               item-or-exemplar-referent
                               semantic-elements-R semantic-to-list-R
                               template-to-condition]]
             [hierarchy :refer [hierarchy-node? hierarchy-node-descendants
                                hierarchy-node-members
                                hierarchy-node-next-level hierarchy-node-extent
                                hierarchy-nodes-extent
                                hierarchy-by-canonical-info
                                hierarchy-node-example-elements]]
             [order-utils :refer [order-items-R]]
             [render-utils :refer [make-component vertical-stack
                                   virtual-item-DOM item-stack-DOM
                                   condition-satisfiers-R]]
             [item-render :refer [elements-DOM-R label-wrapper-DOM-R
                                  item-content-and-elements-DOM-R
                                  item-without-labels-DOM-R
                                  item-DOM-R must-show-label-item-DOM-R]])))

(def base-table-column-width 150)

(defn is-tag-template?
  "Return true if the template describes a label."
  [template]
  (some #(or (= (if (sequential? %) (first %) %) :tag))
        template))

(defn table-node-header-elements-referent
  "Return a referent for the elements in headers spanned by a table node."
  [node header-subject]
  (union-referent-if-needed
   (map #(item-or-exemplar-referent (:item %) header-subject)
        (hierarchy-node-descendants node))))

(defn table-node-row-elements-referent
  "Generate a referent for the elements in rows affected by a table header."
  [node rows-referent]
  (union-referent-if-needed (map #(elements-referent (:item %) rows-referent)
                                 (hierarchy-node-descendants node))))

(defn table-node-exclusive-row-elements-referent
  "Generate a referent for the elements in rows affected
  by a table header, but by no descendant header."
  [node rows-referent]
  (let [positive-ref (table-node-row-elements-referent node rows-referent)
        negative-refs (->> (hierarchy-node-next-level node)
                           (filter hierarchy-node?)
                           (hierarchy-nodes-extent)
                           (map #(elements-referent (:item %) rows-referent)))] 
    (if (empty? negative-refs)
      positive-ref
      (difference-referent
       positive-ref
       (union-referent-if-needed negative-refs)))))

(defn table-node-delete-referent
  "Generate the referent for the elements to be deleted when the only
  item of a table header node is deleted. The arguments are the node,
  a referent to the rows of the table, and to the subject of the
  header requests.
  When the referent is instantiated, the first group must be all the elements
  in table requests, while subsequent groups contain elements in rows brought
  up by the header."
  [node rows-referent header-subject]
  (let [exemplar-item (first (hierarchy-node-example-elements node))
        header-ref (union-referent-if-needed
                    (map #(let [item-ref (item-or-exemplar-referent
                                          (:item %) header-subject)]
                            (if (= (multiset (:property-canonicals %))
                                   (:cumulative-properties node))
                              item-ref
                              (item-or-exemplar-referent
                               exemplar-item item-ref)))
                         (hierarchy-node-descendants node)))
        element-ref (table-node-exclusive-row-elements-referent
                     node rows-referent)]
    ;; We always return a union, to guarantee that instantition will
    ;; return one group for the header elements.
    (union-referent [header-ref element-ref])))

(defn attributes-for-header-add-column-command
  "Return attributes for an add column command, given the column
  request items that gave rise to the column. elements-template gives
  the template for new elements, while inherited gives the environment
  of the header."
  [column-items elements-template inherited]
  (let [subject (:subject inherited)
        new-elements-template (cons '??? (rest elements-template))
        new-header-template (apply list (concat (or (:template inherited)
                                                    '(anything-immutable))
                                           [new-elements-template]))
        ;; There is an item for the new column, which has an element
        ;; satisfying the element template. We want to select that
        ;; element.
        ;; TODO: This doesn't handle header items that are below other
        ;; header items, as there is no query that can pick out the
        ;; new item as opposed to copied template items. The solution is to
        ;; add an "excluded" feature to the pattern, saying patterns not
        ;; to match.
        element-variable `(:variable
                            (:v :name)
                            (~elements-template :condition)
                            (true :reference))
        select-pattern (conj (:parent-key inherited)
                             [:pattern `(nil ~element-variable)])]
    {:column {:adjacent-groups-referent (parallel-union-referent
                                         (map #(item-or-exemplar-referent
                                                % subject)
                                              column-items))
              :subject-referent (union-referent [subject])
              :position :after
              :template new-header-template
              :select-pattern select-pattern}}))

(defn condition-elements-DOM-R
  "Generate the dom for a (subset of) a condition, given its elements."
  [elements inherited]
  (if (empty? elements)
    [:div {:class "elements-wrapper"}]
    (expr-let
        [item (entity/subject (first elements))
         content (entity/content item)
         all-labels (entity/label->elements item :tag)
         all-labels-set (set all-labels)
         labels (seq (filter all-labels-set elements))
         non-labels (seq (remove all-labels-set elements))]
      (cond
        (and labels non-labels)
        (let [inherited-down
              (-> inherited
                  (dissoc :template)
                  (assoc :element-attributes (:selectable-attributes
                                              inherited)))]
          (expr-let [inner-dom (elements-DOM-R non-labels true nil inherited)]
            (label-wrapper-DOM-R
             [:div {:class "item elements-wrapper"} inner-dom]
             item (:subject inherited) labels false inherited)))
        labels
        (expr-let [ordered-elements (order-items-R elements)
                   excludeds (expr-seq map #(matching-elements :tag %)
                                       ordered-elements)]
          (let [subject-ref (:subject inherited)
                inherited-down (update-in
                                inherited [:selectable-attributes]
                                #(assoc % :add-element
                                        {:item-referent subject-ref}))]
            (cond-> (item-stack-DOM item-without-labels-DOM-R
                                    ordered-elements excludeds
                                    {:class "tag"} inherited-down)
              (> (count ordered-elements) 1) (add-attributes {:class "tag"}))))
        true
        (expr-let [elements-dom (elements-DOM-R non-labels true nil inherited)]
          [:div {:class "elements-wrapper"} elements-dom])))))

(defn table-header-element-template
  "Return a template for new elements of a table header. It should include
  what is common to the specified elements, which should be in canonical
  list form."
  [canonical-elements]
  (or (when (and (seq canonical-elements)
                 (every? sequential? canonical-elements))
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
      '(nil)))

(defn table-header-node-DOM-R
  "Generate the dom for a node of a table header hierarchy. The
  rows-referent should specify all the row items from which this
  header selects elements. The elements template describes new elements
  of the column request(s), in contrast to inherited, which describe the
  request(s) overall."
  [node rows-referent elements-template inherited]
  (let [subject (:subject inherited)
        column-referent (union-referent
                         [(table-node-header-elements-referent node subject)
                          (table-node-row-elements-referent node rows-referent)
                          ])
        example-elements (hierarchy-node-example-elements node)
        selectable-attributes
        (when (= (count example-elements) 1)
          {:expand {:item-referent column-referent}
           :delete {:item-referent (table-node-delete-referent
                                    node rows-referent subject)}})
        descendants (hierarchy-node-descendants node)
        column-requests (map :item descendants)
        inherited-down (-> (if selectable-attributes
                               (assoc inherited :selectable-attributes
                                      selectable-attributes)
                               (dissoc inherited :selectable-attributes))
                           (assoc :width (* 0.75 (count descendants))
                                  :template elements-template
                                  :subject column-referent)
                           (update-in
                            [:selectable-attributes]
                            #(into-attributes
                              % (attributes-for-header-add-column-command
                                 column-requests elements-template
                                 inherited))))]
    (condition-elements-DOM-R example-elements inherited-down)))

(defn table-header-member-DOM-R
  "Generate the DOM for an element in a hierarchy that is not the only
  descendant of its parent. It will be displayed under its parent but
  has no elements of its own to show."
  [column-item containing-node rows-referent elements-template inherited]
  (let [column-referent (union-referent
                         [(item-or-exemplar-referent
                           column-item (:subject inherited))
                          (table-node-exclusive-row-elements-referent
                           containing-node rows-referent)])
        inherited-down (-> inherited
                           (assoc :subject column-referent
                                  :template elements-template)
                           (update-in
                            [:selectable-attributes]
                            #(into-attributes
                              (into-attributes
                               %
                               (attributes-for-header-add-column-command
                                [column-item] elements-template inherited))
                              {:delete {:item-referent column-referent}
                               :expand {:item-referent column-referent}
                               :target {:adjacent-referent column-referent
                                        :position :after}})))
        key (conj (:parent-key inherited) (:item-id column-item))]
    (add-attributes
     (virtual-item-DOM key inherited-down)
     (cond-> {:style {:width (str base-table-column-width "px")}}
       (is-tag-template? elements-template) (into-attributes {:class "tag"})))))

(def table-header-subtree-DOM-R)

(defn table-header-node-or-element-DOM-R
  "Given something that is either a hieararchy node or element,
  generate its DOM, but not the DOM for any children."
  [node-or-element containing-node rows-referent elements-template
   inherited]
  (if (hierarchy-node? node-or-element)
    (table-header-subtree-DOM-R
     node-or-element false rows-referent inherited)
    (table-header-member-DOM-R
     (:item node-or-element) containing-node rows-referent elements-template
     inherited)))

(defn table-header-subtree-DOM-R
  "Generate the dom for a subtree of a table header hierarchy. 
  rows-referent should specify all the row items from which this
  header selects elements. Inherited describes the column requests."
  [node top-level rows-referent inherited]
  (let [elements-template (table-header-element-template
                           (keys (:cumulative-properties node)))]
    (expr-let
        [node-dom (table-header-node-DOM-R
                   node rows-referent elements-template inherited)]
      (let [node-dom (cond-> node-dom
                       top-level (add-attributes {:class "top-level"}))
            next-level (hierarchy-node-next-level node)]
        (expr-let
            [dom
             (if (= (count next-level) 1)
               ;; If we have only one descendant, it must be the column request.
               node-dom
               (let [properties-list (canonical-set-to-list (:properties node))
                     inherited (update-in inherited [:template]
                                          #(list* (concat
                                                   (or % '(anything-immutable))
                                                   properties-list)))]
                 (expr-let
                     [dom-seqs (expr-seq
                                map #(table-header-node-or-element-DOM-R
                                      % node
                                      rows-referent elements-template inherited)
                                next-level)]
                   [:div (cond-> {}
                           top-level (into-attributes {:class "top-level"}))
                    (add-attributes node-dom {:class "with-children"})
                    (into [:div {:class "column-header-sequence"}]
                          dom-seqs)])))]
          (let [is-tag (is-tag-template? elements-template)
                num-columns (count (hierarchy-node-descendants node))
                width (+ (* num-columns (- base-table-column-width 2)) 2)]
            (add-attributes dom {:class (cond-> "column-header"
                                          is-tag (str " tag"))
                                 :style {:width (str width "px")}})))))))

(defn table-header-DOM-R
  "Generate DOM for column headers given the hierarchy. elements-template
  gives what new elements of a header request need to satisfy.
  The column will contain those elements of the rows that match the templates
  in the hierarchy."
  [hierarchy rows-referent inherited]
  (let [inherited-down (assoc
                        inherited
                        :selector-category :table-header
                        :alternate-target true)]
    (expr-let [columns (expr-seq
                        map #(table-header-subtree-DOM-R
                              % true rows-referent inherited-down)
                        hierarchy)]
      (into [:div {:class "column-header-sequence"}]
            columns))))

(defn table-cell-items-DOM-R
  "Return the dom for one cell of a table, given its items.
  Inherited gives the context of each item in the cell."
  [items new-row-template inherited]
  (let [inherited (-> inherited
                      (assoc :width 0.75)
                      (update-in [:selectable-attributes]
                                 #(into-attributes
                                   % {:row {:item-referent (:subject inherited)
                                            :template new-row-template}})))]
    (expr-let
        [dom (if (empty? items)
               ;; TODO: Get our left neighbor as an arg, and pass it
               ;; in as adjacent information for new-twin.
               (virtual-item-DOM
                (:parent-key inherited)
                (into-attributes
                 inherited
                 {:selectable-attributes
                  {:target {:adjacent-referent (:subject inherited)
                            :position :after}
                   :delete {:item-referent (-> inherited
                                               :selectable-attributes
                                               :row
                                               :item-referent)}}}))
               (elements-DOM-R items false (:template inherited) inherited))]
      (add-attributes dom {:class "table-cell has-border"}))))

(defn table-cell-DOM-R
  "Return the dom for one cell of a table, given its column description."
  [row-item new-row-template
   {:keys [column-item template exclusions]} ;; A column header description
   inherited]
  (let [inherited-down (assoc inherited
                              :parent-key (conj (:parent-key inherited)
                                                (:item-id column-item))
                              :template template)]
    (expr-let [matches (matching-elements template row-item)
               do-not-show (when exclusions
                             (expr-seq map #(matching-elements
                                             (template-to-condition %)
                                             row-item)
                                       exclusions))]
      (let [elements (seq (clojure.set/difference
                           (set matches)
                           (set (apply concat do-not-show))))]
        (table-cell-items-DOM-R elements new-row-template inherited-down)))))

(defn table-row-DOM-R
  "Generate dom for a table row."
  [row-item row-key new-row-template column-descriptions inherited]
  (let [inherited (-> inherited
                      (assoc :parent-key row-key)
                      (update-in [:subject]
                                 #(item-or-exemplar-referent row-item %)))]
    (expr-let [cells (expr-seq map #(table-cell-DOM-R
                                     row-item new-row-template % inherited)
                               column-descriptions)]
      (into [:div {:key row-key}] cells))))

(defn table-row-DOM-component
  "Generate a component for a table row."
  [row-item new-row-template column-descriptions inherited]
  (let [row-key (conj (:parent-key inherited) (:item-id row-item))]
    (make-component
     {:key row-key :class "table-row"}
     [table-row-DOM-R
      row-item row-key new-row-template column-descriptions inherited])))

(defn add-element-to-entity-list
  [entity element]
  (concat (if (sequential? entity) entity (list entity))
          element))

(defn table-hierarchy-node-condition
  "Given a hierarchy node, return the condition that all elements
  under the node satisfy."
  [node]
  (template-to-condition
   (cons nil (canonical-set-to-list (:cumulative-properties node)))))

(defn table-hierarchy-node-exclusions
  "Given a hierarchy node, return a seq of conditions that immediate
  elements of the node must not satisfy, because they are covered
  by sub-nodes."
  [node]
  (map #(template-to-condition
         (cons nil (map canonical-to-list (:property-canonicals %))))
       (->> (hierarchy-node-next-level node)
            (filter hierarchy-node?)
            (hierarchy-nodes-extent))))

(defn table-hierarchy-node-column-descriptions
  "Given a hierarchy node, for each column under the node,
  return a map:
     :column-item Item that identifies the column.
        :template Condition that each element of the column must satisfy.
                  ('anything is turned to nil before putting a condition here.)
      :exclusions Seq of conditions that elements must not satisfy."
  [node]
  (mapcat (fn [node-or-element]
            (if (hierarchy-node? node-or-element)
              (table-hierarchy-node-column-descriptions node-or-element)
              [{:column-item (:item node-or-element)
                :template (table-hierarchy-node-condition node)
                :exclusions (table-hierarchy-node-exclusions node)}]))
          (hierarchy-node-next-level node)))

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
  ;;              other column.
  ;; TODO: Add the "other" column if a table requests it.
  [table-item inherited]
  (println "Generating DOM for table" (simplify-for-print table-item))
  (assert (satisfies? entity/StoredEntity table-item))
  (let [store (:store table-item)
        table-referent (item-or-exemplar-referent
                        table-item (:subject inherited))
        table-key (conj (:parent-key inherited) table-referent)
        inherited (assoc inherited :parent-key table-key)]
    (expr-let [row-condition-items (entity/label->elements
                                    table-item :row-condition)]
      ;; Don't do anything if we don't yet have the table information filled in.
      (when-let [row-condition-item (first row-condition-items)]
        (expr-let
            [row-condition (semantic-to-list-R row-condition-item)
             row-query (add-element-to-entity-list
                        (template-to-condition row-condition)
                        ['(:top-level :non-semantic)])
             ;; We have to use the item in the referent's condition, so
             ;; it doesn't contain strings or other non-serializable stuff.
             rows-referent (query-referent
                            (list (item-referent row-condition-item)
                                  '(:top-level :non-semantic)))
             ;; Avoid the (nil :order :non-semantic) added by
             ;; template-to-condition.
             row-template (add-element-to-entity-list
                           (replace-in-seqs row-condition 'anything nil)
                           ['(:top-level :non-semantic)])
             row-items  (expr order-items-R
                            (matching-items row-query store))
             columns (expr order-items-R
                       (entity/label->elements table-item :column))
             ;; Unlike row headers for tags, where the header
             ;; information is computed from the items of the elements,
             ;; here the header information is explicitly provided by
             ;; the table definition. So the members of the hierarchy
             ;; are the column definitions.
             columns-elements (expr-seq map semantic-elements-R columns)
             columns-lists (expr-seq map #(expr-seq map semantic-to-list-R %)
                                     columns-elements)
             hierarchy (hierarchy-by-canonical-info
                        (map (fn [column elements lists]
                               {:item column
                                :property-elements elements
                                :property-canonicals (map canonicalize-list
                                                          lists)})
                             columns columns-elements columns-lists))
             condition-elements (semantic-elements-R row-condition-item)
             conditions-as-lists (expr-seq map semantic-to-list-R
                                           condition-elements)
             condition-is-tags (every? #(and (sequential? %)
                                             (seq (filter #{:tag} %)))
                                       conditions-as-lists)
             condition-dom (condition-elements-DOM-R
                            condition-elements
                            (assoc
                             inherited
                             :selector-category :table-condition
                             :subject (union-referent
                                       [(item-referent row-condition-item)
                                        rows-referent])
                             :template (if condition-is-tags
                                         '(nil :tag) '(nil))
                             :alternate-target true))
             headers (table-header-DOM-R
                      hierarchy rows-referent
                      (assoc inherited
                             :subject (item-referent table-item)
                             :template '(anything-immutable
                                         (:column :non-semantic))))]
          (let [column-descriptions (mapcat
                                     table-hierarchy-node-column-descriptions
                                     hierarchy)
                rows (expr-seq map #(table-row-DOM-component
                                     % row-template column-descriptions
                                     inherited)
                               row-items)]
            [:div {:class "table selector-scope"}
             [:div {:class (cond-> "table-top selectors"
                             condition-is-tags (str " tag"))}
              [:div {:class "table-corner"}]
              (add-attributes condition-dom
                              {:class "table-condition"})]
             [:div {:class "table-body"}
              [:div {:class (cond-> "table-indent"
                              condition-is-tags (str " tag"))}]
              [:div {:class "table-main selecteds selector-scope"}
               (add-attributes headers {:class "selectors"})
               (into [:div {:class "table-rows selecteds"}]
                     rows)]]]))))))

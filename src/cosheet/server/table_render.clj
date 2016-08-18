(ns cosheet.server.table-render
  (:require (cosheet [entity :as entity]
                     [query :refer [matching-elements matching-items]]
                     [utils :refer [replace-in-seqs]]
                     [debug :refer [simplify-for-print current-value]]
                     [orderable :as orderable]
                     [dom-utils :refer [into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq cache]])
            (cosheet.server
             [referent :refer [item-referent
                               elements-referent query-referent
                               union-referent always-union-referent
                               parallel-union-referent
                               difference-referent
                               item-or-exemplar-referent
                               semantic-elements-R semantic-to-list-R
                               canonicalize-list]]
             [hierarchy :refer [canonical-set-to-list canonical-to-list
                                hierarchy-node? hierarchy-node-descendants
                                hierarchy-node-members
                                hierarchy-node-next-level hierarchy-node-extent
                                hierarchy-nodes-extent
                                hierarchy-by-canonical-info
                                hierarchy-node-example-elements]]
             [render-utils :refer [make-component
                                   virtual-item-DOM item-stack-DOM
                                   order-items-R condition-satisfiers-R]]
             [item-render :refer [elements-DOM-R
                                  item-without-labels-DOM-R item-DOM-R]])))

(def base-table-column-width 150)

(defn table-column-elements-referent
  "Generate the referent for the elements affected by a table header,
  including elements in table request items at or below this header,
  and also all elements in rows that are brought up by the header. The
  arguments are the info-maps of all the header requests the header
  applies to, info maps that cover the conditions it brings up in
  rows, info maps that cover conditions it must not bring up, a
  referent to the rows of the table, and to the subject of the header
  requests.
  When the referent is instantiated, the first group must be all the elements
  in table requests, while subsequent groups contain elements in rows brought
  up by the header."
  [info-maps extent-info-maps negative-info-maps rows-referent header-subject]
  (let [header-ref (union-referent
                    (map #(item-or-exemplar-referent (:item %) header-subject)
                         info-maps)) 
        make-elements-ref #(elements-referent (:item %) rows-referent)
        positive-element-refs (map make-elements-ref extent-info-maps)
        element-refs (if (empty? negative-info-maps)
                        positive-element-refs
                        [(difference-referent
                             (union-referent positive-element-refs)
                             (union-referent (map make-elements-ref
                                                  negative-info-maps)))])]
    ;; We always return a union, to guarantee that instantition will
    ;; return one group for the header elements.
    (always-union-referent (concat [header-ref] element-refs))))

(defn attributes-for-header-delete-command
  "Return attributes for the delete command, if it needs special attributes."
  [node example-elements rows-referent subject]
  ;; The special cases are where there is just one element.
  (when (= (count example-elements) 1)
    (let [next-level (hierarchy-node-next-level node)
          non-trivial-children (filter hierarchy-node? next-level)
          delete-referent
          ;; If we don't have children, then deletion removes the
          ;; column request, while if we do, it removes the element
          ;; from requests and from elements in the rows.
          (if (empty? non-trivial-children)
            (item-or-exemplar-referent (:item (first next-level)) subject)
            ;; We don't include the item maps at the next level,
            ;; because removing the element from the corresponding
            ;; requests would leave them with no elements.
            (item-or-exemplar-referent
             (first example-elements)
             (table-column-elements-referent
              (mapcat hierarchy-node-descendants non-trivial-children)
              (hierarchy-nodes-extent non-trivial-children) nil
              rows-referent subject)))]
      {:commands {:delete {:delete-referent delete-referent}}})))

(defn attributes-for-header-add-column-command
  "Return attributes for an add a column command, given the column
  request items that gave rise to the column. elements-template gives
  the template for new elements, while inherited gives the environment
  of the header."
  [column-items elements-template inherited]
  (let [subject (:subject inherited)
        new-elements-template (cons '??? (rest elements-template))
        new-header-template (list* (concat (or (:template inherited) '(nil))
                                           [new-elements-template]))
        ;; There is an item for the new column, which has an element
        ;; satisfying the element template. We want to select that
        ;; element.
        ;; TODO: This doesn't handle header items that are below other
        ;; header items, as there is no query that can pick out the
        ;; new item as opposed to the the copied items. The solution is to
        ;; add an :exclusive option on referent variables, which means
        ;; that they can't match the same item as any other exclusive
        ;; referent. With that, we introduce variables to match each
        ;; of the header items that are above, leaving just the new
        ;; one to match the :v variable.
        element-variable `(:variable
                            (:v :name)
                            (~elements-template :condition)
                            (true :reference))
        select-pattern (conj (:parent-key inherited)
                             [:pattern `(nil ~element-variable)])]
    {:commands {:add-column {:select-pattern select-pattern}}
     :column {:adjacent-groups-referent (parallel-union-referent
                                         (map #(item-or-exemplar-referent
                                                % subject)
                                              column-items))
              :subject-referent subject
              :position :after
              :template new-header-template}}))

(defn add-add-column-commands
  "For each element, which must be in list form, modify inherited to
  have its :selectable-attributes include a new column command, whose
  template includes all the elements above the element."
  [element-lists column-requests header-inherited inherited]
  (let [element-lists-above (map #(concat (take % element-lists))
                                 (range (count element-lists)))]
    (map (fn [element-lists]
           (let [modified-header-inherited
                 (update-in header-inherited [:template]
                            #(list* (concat (or % '(nil)) element-lists)))]
             (update-in inherited [:selectable-attributes]
                        #(into-attributes
                          % (attributes-for-header-add-column-command
                             column-requests (:template inherited)
                             modified-header-inherited)))))
         element-lists-above)))

(defn add-table-header-formatting
  "Given a dom of header elements, return a dom with all the appropriate
  formatting."
  [dom num-columns is-tag]
  (let [width (+ (* num-columns (- base-table-column-width 2)) 2)]
    (add-attributes dom {:class (cond-> "column-header"
                                  is-tag (str " tag"))
                         :style {:width (str width "px")}})))

(defn table-header-node-elements-DOM-R
  "Generate the dom for one node of a table header hierarchy given its
  elements. column-requests gives the items that requested all columns
  that this node covers. header-inherited gives the context of
  the header definition(s) of this column, while inherited gives the
  context for elements of both the header and its entire column."
  [example-elements column-requests header-inherited inherited]
  (assert (not (empty? example-elements)))
  (expr-let
      [ordered-elements (order-items-R example-elements)
       element-lists (expr-seq map semantic-to-list-R ordered-elements)
       excludeds (expr-seq map #(condition-satisfiers-R % (:template inherited))
                           ordered-elements)]
    (let [is-tag (some #{:tag} (:template inherited))]
      (item-stack-DOM
       (if is-tag item-without-labels-DOM-R item-DOM-R)
       ordered-elements excludeds
       (if is-tag {:class "tag"} {})
       (add-add-column-commands
        element-lists column-requests header-inherited inherited)))))

(defn table-header-node-DOM-R
  "Generate the dom for a node of a table header hierarchy. The
  row-scope-referent should specify all the row items from which this
  header selects elements."
  [node elements-template rows-referent inherited]
  (let [subject (:subject inherited)
        descendants (hierarchy-node-descendants node) 
        example-elements (hierarchy-node-example-elements node)
        column-referent (table-column-elements-referent
                           descendants (hierarchy-node-extent node) nil
                           rows-referent subject)
        delete-attributes (attributes-for-header-delete-command
                           node example-elements rows-referent subject)
        column-inherited (let [temp (assoc inherited
                                           :template elements-template
                                           :subject column-referent
                                           :width (* 0.75 (count descendants)))]
                           (if delete-attributes
                             (assoc temp :selectable-attributes
                                    delete-attributes)
                             (dissoc temp :selectable-attributes)))]
    (table-header-node-elements-DOM-R
     example-elements (map :item descendants) inherited column-inherited)))

(def table-header-subtree-DOM-R)

(defn table-header-next-level-DOM-R
  "Given something that is either a hieararchy node or element,
  generate its DOM."
  [below non-trivial-siblings elements-template rows-referent
   inherited]
  (if (hierarchy-node? below)
    (table-header-subtree-DOM-R
     below false elements-template rows-referent inherited)
    ;; This is a member that is displayed underneath its node. Since
    ;; the display of the node already presents all the properties, we
    ;; need a header DOM with no elements.
    (let [request-referent (item-or-exemplar-referent
                            (:item below) (:subject inherited))
          exclude-from-members (hierarchy-nodes-extent non-trivial-siblings)
          column-subject (table-column-elements-referent
                          [below] [below] exclude-from-members
                          rows-referent (:subject inherited))
          inherited (-> inherited
                        (assoc :subject column-subject
                               :template elements-template)
                        (update-in
                         [:selectable-attributes]
                         #(into-attributes
                           (into-attributes
                            %
                            (attributes-for-header-add-column-command
                             [(:item below)] elements-template inherited))
                           {:commands
                            {:delete {:delete-referent request-referent}}})))
          key (conj (:parent-key inherited) (:item-id (:item below)))
          is-tag (some #{:tag} elements-template)]
      (cond-> (add-attributes
               (virtual-item-DOM
                key :after (assoc inherited :adjacent-referent column-subject))
               {:style {:width (str base-table-column-width "px")}})
        is-tag (add-attributes {:class "tag"})))))

(defn table-header-subtree-DOM-R
  "Generate the dom for a subtree of a table header hierarchy. 
  elements-template gives what new header items need to satisfy.
  rows-referent should specify all the row items from which this
  header selects elements."
  [node top-level elements-template rows-referent inherited]
  (expr-let
      [node-dom (table-header-node-DOM-R
                 node elements-template rows-referent inherited)]
    (let [node-dom (cond-> node-dom
                     top-level (add-attributes {:class "top-level"}))
          next-level (hierarchy-node-next-level node)
          non-trivial-children (filter hierarchy-node? next-level)]
      (expr-let
          [dom
           (if (and (= (count next-level) 1) (empty? non-trivial-children))
             node-dom
             (let [properties-list (canonical-set-to-list (:properties node))
                   inherited (update-in inherited [:template]
                                        #(list* (concat (or % '(:none))
                                                        properties-list)))
                   is-tag (some #{:tag} elements-template)]
               (expr-let
                   [dom-seqs (expr-seq
                              map #(table-header-next-level-DOM-R
                                     % non-trivial-children
                                     elements-template rows-referent inherited)
                              next-level)]
                 [:div (cond-> {:class "column-header-stack"}
                         top-level (into-attributes {:class "top-level"})
                         is-tag (into-attributes {:class "tag"}))
                  (add-attributes node-dom {:class "with-children"})
                  (into [:div {:class "column-header-sequence"}] dom-seqs)])))]
        (let [is-tag (some #{:tag} elements-template)
              num-columns (count (hierarchy-node-descendants node))]
          (add-table-header-formatting dom num-columns is-tag))))))

(defn table-header-DOM-R
  "Generate DOM for column headers given the hierarchy. elements-template
  gives what new header items need to satisfy.
  The column will contain those elements of the rows that match the templates."
  [hierarchy elements-template rows-referent inherited]
  (expr-let [columns (expr-seq
                      map #(table-header-subtree-DOM-R
                            % true elements-template
                            rows-referent (assoc inherited :selector true))
                      hierarchy)]
    (into [:div {:class "column-header-sequence"}]
          columns)))

(defn table-hierarchy-node-column-descriptions
  "Given a hierarchy node, for each column under the node,
  return a map:
     :column-item Item that identifies the column.
        :template Condition that each element of the column must satisfy.
      :exclusions Seq of conditions that elements must not satisfy."
  [node]
  (let [next-level (hierarchy-node-next-level node)
        non-trivial-children (filter hierarchy-node? next-level)
        condition (cons nil (canonical-set-to-list
                             (:cumulative-properties node)))
        excluded-conditions (map #(replace-in-seqs
                                   (cons nil (map canonical-to-list
                                                  (:property-canonicals %)))
                                   :none nil)
                                 (hierarchy-nodes-extent non-trivial-children))]
    (mapcat (fn [below]
              (if (hierarchy-node? below)
                (table-hierarchy-node-column-descriptions below)
                [{:column-item (:item below)
                  :template condition
                  :exclusions excluded-conditions}]))
            next-level)))

(defn table-cell-items-DOM-R
  "Return the dom for one cell of a table, given its items.
  Inherited gives the context of each item in the cell."
  [items new-row-template inherited]
  (let [inherited (-> inherited
                      (assoc :width 0.75)
                      (update-in [:selectable-attributes]
                                 #(into-attributes
                                   % {:commands {:add-row nil
                                                 :expand {:item-referent
                                                          (:subject inherited)}}
                                      :row {:item-referent (:subject inherited)
                                            :template new-row-template}})))]
    (expr-let
        [dom (if (empty? items)
               ;; TODO: Get our left neighbor as an arg, and pass it
               ;; in as adjacent information for new-twin.
               (virtual-item-DOM
                (:parent-key inherited) :after
                (assoc inherited :adjacent-referent (:subject inherited)))
               (elements-DOM-R items false inherited))]
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
                             (expr-seq map #(matching-elements % row-item)
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
      (into [:div {}] cells))))

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
  ;;              will be the keyword :none, to indicate no constraint
  ;;              on the content of an element in the row, without
  ;;              breaking the rule that the database doesn't contain
  ;;              nil. The exception is the special content :other,
  ;;              which means to show everything not shown in any
  ;;              other column.
  ;; TODO: Make there there be an element on a table descriptor that
  ;;       says what elements of new columns must have, rather than
  ;;       the current '(nil :tag)
  ;; TODO: Add the "other" column if a table requests it.
  [table-item inherited]
  (println "Generating DOM for table" (simplify-for-print table-item))
  (assert (satisfies? entity/StoredEntity table-item))
  (let [store (:store table-item)
        table-key (conj (:parent-key inherited) (:item-id table-item))
        inherited (assoc inherited :parent-key table-key)]
    (expr-let [row-query-item (entity/label->content table-item :row-query)]
      ;; Don't do anything if we don't yet have the table information filled in.
      (when row-query-item
        (expr-let
            [basic-row-query (semantic-to-list-R row-query-item)
             row-query (add-element-to-entity-list
                        (replace-in-seqs basic-row-query :none nil)
                        ['(:top-level :non-semantic)])
             row-items (expr order-items-R
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
             headers (table-header-DOM-R
                      hierarchy '(nil :tag) (query-referent row-query)
                      (assoc inherited
                             :subject (item-referent table-item)
                             :template '(:none (:column :non-semantic))))]
          (let [column-descriptions (mapcat
                                     table-hierarchy-node-column-descriptions
                                     hierarchy)
                rows (expr-seq map #(table-row-DOM-component
                                     % row-query column-descriptions inherited)
                               row-items)]
            (into [:div {:class "table"}
                   headers]
                  rows)))))))

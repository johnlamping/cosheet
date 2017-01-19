(ns cosheet.server.table-render
  (:require (cosheet [utils :refer [replace-in-seqs multiset]]
                     [entity :as entity]
                     [query :refer [matching-elements matching-items]]
                     [debug :refer [simplify-for-print]]
                     [dom-utils :refer [dom-attributes
                                        into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq]]
                     [canonical :refer [canonicalize-list canonical-to-list
                                        canonical-set-to-list
                                        common-canonical-multisets]])
            (cosheet.server
             [referent :refer [item-referent
                               elements-referent query-referent
                               union-referent-if-needed union-referent
                               difference-referent virtual-referent
                               item-or-exemplar-referent
                               semantic-elements-R semantic-to-list-R
                               pattern-to-condition condition-to-template]]
             [hierarchy :refer [hierarchy-node? hierarchy-node-descendants
                                hierarchy-node-members
                                hierarchy-node-next-level hierarchy-node-extent
                                hierarchy-nodes-extent
                                hierarchy-by-canonical-info
                                hierarchy-by-all-elements
                                hierarchy-node-example-elements
                                hierarchy-node-items-referent
                                hierarchy-node-parallel-items-referent]]
             [order-utils :refer [order-items-R]]
             [render-utils :refer [make-component vertical-stack
                                   virtual-item-DOM item-stack-DOM
                                   condition-satisfiers-R]]
             [item-render :refer [elements-DOM-R label-wrapper-DOM-R
                                  item-content-and-elements-DOM-R
                                  item-without-labels-DOM-R
                                  item-DOM-R must-show-label-item-DOM-R]])))

(def base-table-column-width 150)
(def base-table-virtual-column-width 35)

(defn is-tag-template?
  "Return true if the template describes a label."
  [template]
  (some #(or (= (if (sequential? %) (first %) %) :tag))
        template))

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
    ;; return the header elements in the first group.
    (union-referent [header-ref element-ref])))

(defn new-header-template
  "Return the template for a new header. elements-template gives
  the template for new elements in the header, while inherited gives
  the environment of the header."
  [new-elements-template inherited]
  (apply list (concat (:template inherited)
                      [(cons '??? (rest new-elements-template))])))

(defn target-for-header-add-column-command
  "Return the target for an add column command, given the column
  request items that the column spans. elements-template gives
  the template for new elements in the header, while inherited gives
  the environment of the header."
  [node-or-member elements-template inherited]
  (assert (:template inherited))
  (let [subject-ref (:subject-referent inherited)
        ;; There is an item for the new column, which has an element
        ;; satisfying the element template. We want to select that
        ;; element.
        ;; TODO: This doesn't handle header items that are below other
        ;; header items, as there is no query that can pick out the
        ;; new item as opposed to copied template items. The solution is to
        ;; have the variables in the select pattern navigate relative to the
        ;; new item, not relative to whatever it is now.
        element-variable `(:variable
                            (:v :name)
                            (~elements-template :condition)
                            (true :reference))
        select-pattern (conj (:key-prefix inherited)
                             [:pattern `(nil ~element-variable)])
        adjacent-referent (hierarchy-node-parallel-items-referent
                           node-or-member subject-ref)]
    {:referent
     (virtual-referent (new-header-template elements-template inherited)
                       (union-referent [subject-ref])
                       adjacent-referent :position :after
                       :selector :first-group)
     :select-pattern select-pattern}))

(defn attributes-for-header-add-column-command
  "Return attributes for an add column command, given the column
  request items that gave rise to the column. elements-template gives
  the template for new elements, while inherited gives the environment
  of the header."
  [node-or-member elements-template inherited]
  {:add-column (target-for-header-add-column-command
                node-or-member elements-template inherited)})

(defn condition-elements-DOM-R
  "Generate the dom for a (subset of) a condition, given its elements.
  :key-prefix of inherited must give a prefix for the doms of each element."
  [elements inherited]
  (expr-let
      [tags (expr-seq map #(matching-elements :tag %) elements)]
    (let [labels (seq (mapcat (fn [tags element] (when (seq tags) [element]))
                              tags elements))
          non-labels (seq (mapcat (fn [tags element] (when (empty? tags)
                                                       [element]))
                                  tags elements))]
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
             (:subject-referent inherited) labels false inherited)))
        labels
        (expr-let [ordered-elements (order-items-R elements)
                   excludeds (expr-seq map #(matching-elements :tag %)
                                       ordered-elements)]
          (let [subject-ref (:subject-referent inherited)
                inherited-down (update-in
                                inherited [:selectable-attributes]
                                #(assoc % :add-element
                                        {:referent subject-ref}))]
            (cond-> (item-stack-DOM item-without-labels-DOM-R
                                    ordered-elements excludeds
                                    {:class "tag"} inherited-down)
              (> (count ordered-elements) 1) (add-attributes {:class "tag"}))))
        non-labels
        (expr-let [elements-dom (elements-DOM-R non-labels true nil inherited)]
          [:div {:class "elements-wrapper"} elements-dom])
        true
        [:div {:class "elements-wrapper"}]))))

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

(defn table-virtual-header-element-template
  "Return a template for new elements of a virtual table header."
  [hierarchy]
  (table-header-element-template
   (when (seq hierarchy)
     (keys (:cumulative-properties (last hierarchy))))))

(defn table-header-node-DOM-R
  "Generate the dom for a node of a table header hierarchy. The
  rows-referent should specify all the row items from which this
  header selects elements. The elements template describes new elements
  of the column request(s), in contrast to inherited, which describe the
  request(s) overall."
  [node top-level rows-referent elements-template inherited]
  (let [subject-ref (:subject-referent inherited)
        column-referent (union-referent [(hierarchy-node-items-referent
                                          node subject-ref)
                                         (table-node-row-elements-referent
                                          node rows-referent)])
        example-elements (hierarchy-node-example-elements node)
        selectable-attributes
        (when (= (count example-elements) 1)
          (cond-> {:expand {:referent column-referent}}
            top-level (assoc :delete
                             {:referent (table-node-delete-referent
                                         node rows-referent subject-ref)})))
        descendants (hierarchy-node-descendants node)]
    (let [inherited-down (-> (if selectable-attributes
                               (assoc inherited :selectable-attributes
                                      selectable-attributes)
                               (dissoc inherited :selectable-attributes))
                             (assoc :width (* 0.75 (count descendants))
                                    :template elements-template
                                    :subject-referent column-referent)
                             (update-in
                              [:selectable-attributes]
                              #(into-attributes
                                % (attributes-for-header-add-column-command
                                   node elements-template
                                   inherited)))
                             (update :key-prefix
                                     #(conj % :nested)))]
      (condition-elements-DOM-R example-elements inherited-down))))

(defn table-virtual-header-node-DOM
  [hierarchy adjacent-referent inherited]
  (let [key (conj (:key-prefix inherited) :virtualColumn)
        inherited (assoc inherited
                         :subject-referent (virtual-referent
                                            (:template inherited)
                                            (:subject-referent inherited)
                                            adjacent-referent
                                            :selector :first-group)
                         :template (table-virtual-header-element-template
                                    hierarchy))]
    (add-attributes
     (virtual-item-DOM key adjacent-referent :after inherited)
     {:class "column-header"
      :style {:width (str base-table-virtual-column-width "px")}})))

(defn table-header-member-DOM
  "Generate the DOM for an element in a hierarchy that is not the only
  descendant of its parent. It will be displayed under its parent but
  has no elements of its own to show."
  [column-item containing-node rows-referent elements-template inherited]
  (let [column-referent (union-referent
                         [(item-or-exemplar-referent
                           column-item (:subject-referent inherited))
                          (table-node-exclusive-row-elements-referent
                           containing-node rows-referent)])
        inherited-down (-> inherited
                           (assoc :subject-referent column-referent
                                  :template elements-template)
                           (update-in
                            [:selectable-attributes]
                            #(into-attributes
                              (into-attributes
                               %
                               (attributes-for-header-add-column-command
                                {:item column-item}
                                elements-template inherited))
                              {:delete {:referent column-referent}
                               :expand {:referent column-referent}})))
        key (conj (:key-prefix inherited) (:item-id column-item))]
    (add-attributes
     (virtual-item-DOM key column-referent :after inherited-down)
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
     node-or-element false rows-referent
     ;; The child nodes might use the same item in their keys as their parent,
     ;; so add to the prefix to make their keys distinct.
     (update inherited :key-prefix #(conj % :nested)))
    (table-header-member-DOM
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
                   node top-level rows-referent elements-template inherited)]
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
                        :alternate-target true)
        adjacent-referent (if (seq hierarchy)
                            (let [last-column (last (hierarchy-node-descendants
                                                     (last hierarchy)))]
                              (item-referent (:item last-column)))
                            (:subject-referent inherited))
        virtual-header (table-virtual-header-node-DOM
                        hierarchy adjacent-referent inherited-down)]
    (expr-let [columns (expr-seq
                        map #(table-header-subtree-DOM-R
                              % true rows-referent inherited-down)
                        hierarchy)]
      (into [:div {:class "column-header-sequence"}]
            (concat columns [virtual-header])))))

(defn table-cell-items-DOM-R
  "Return the dom for one cell of a table, given its items.
  Inherited gives the context of each item in the cell."
  [items new-row-template inherited]
  (let [inherited (-> inherited
                      (assoc :width 0.75)
                      (update-in
                       [:selectable-attributes]
                       #(into-attributes
                         % {:add-row {:referent
                                      (virtual-referent
                                       new-row-template nil
                                       (:subject-referent inherited))}})))]
    (expr-let
        [dom (if (empty? items)
               ;; TODO: Get our left neighbor as an arg, and pass it
               ;; in as adjacent information for new-twin.
               (virtual-item-DOM
                (:key-prefix inherited) (:subject-referent inherited) :after
                (into-attributes
                 inherited
                 {:selectable-attributes
                  {:delete {:referent (:subject-referent inherited)}}}))
               (elements-DOM-R items false (:template inherited) inherited))]
      (add-attributes dom {:class "table-cell has-border"}))))

(defn table-virtual-column-cell-DOM
  [row-item new-row-template
   {:keys [column-id template exclusions]}
   inherited]
  (let [inherited (assoc inherited :template template)
        key (conj (:key-prefix inherited) column-id)]
    (add-attributes
     (virtual-item-DOM key (:subject-referent inherited) :after inherited)
     {:class "table-cell virtual-column has-border"})))

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
      (table-virtual-column-cell-DOM
       row-item new-row-template header-description inherited)
      (expr-let [matches (matching-elements template row-item)
                 do-not-show (when exclusions
                               (expr-seq map #(matching-elements
                                               (pattern-to-condition %)
                                               row-item)
                                         exclusions))]
        (let [elements (seq (clojure.set/difference
                             (set matches)
                             (set (apply concat do-not-show))))]
          (table-cell-items-DOM-R elements new-row-template inherited-down))))))

(defn table-row-DOM-R
  "Generate dom for a table row."
  [row-item row-key new-row-template column-descriptions inherited]
  (let [inherited (-> inherited
                      (assoc :key-prefix row-key)
                      (update-in [:subject-referent]
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
  (let [inherited (assoc inherited :template template)
        key (conj (:key-prefix inherited) column-id)]
    (add-attributes
     (virtual-item-DOM key adjacent-referent :after inherited)
     {:class "table-cell has-border"})))

(defn table-virtual-row-DOM
  "Generate dom for a table's virtual row."
  [row-key new-row-template adjacent-referent column-descriptions inherited]
  (let [inherited (-> inherited
                      (assoc :key-prefix row-key)
                      (update-in [:subject-referent]
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

(defn add-element-to-entity-list
  [entity element]
  (concat (if (sequential? entity) entity (list entity))
          element))

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
             row-query (add-element-to-entity-list
                        (pattern-to-condition row-condition)
                        ['(:top-level :non-semantic)])
             ;; Avoid the (nil :order :non-semantic) added by
             ;; pattern-to-condition.
             row-template (condition-to-template row-query)
             row-items (expr order-items-R
                         (matching-items row-query store))]
    [row-template row-items]))

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
              headers-inherited (assoc inherited
                                       :subject-referent (item-referent
                                                          table-item)
                                       :template '(anything-immutable
                                                   (:column :non-semantic)))]
          (expr-let
              [[row-template row-items] (row-template-and-items-R
                                         store row-condition-item)
               columns (expr order-items-R
                         (entity/label->elements table-item :column))
               hierarchy (hierarchy-by-all-elements columns)
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
                               :subject-referent (union-referent
                                                  [(item-referent
                                                    row-condition-item)
                                                   rows-referent])
                               :template (if condition-is-tags
                                           '(nil :tag) '(nil))
                               :alternate-target true))
               headers (table-header-DOM-R
                        hierarchy rows-referent headers-inherited)]
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
                              inherited)
                            row-items)
                  virtual-row (table-virtual-row-DOM-component
                               row-template
                               (item-referent (or (last row-items) table-item))
                               column-descriptions inherited)]
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
                       (concat rows [virtual-row]))]]])))))))

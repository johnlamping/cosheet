(ns cosheet.server.render
  (:require (cosheet [entity :as entity]
                     [query :refer [matching-elements matching-items]]
                     [utils :refer [multiset multiset-diff multiset-union
                                    multiset-to-generating-values update-last]]
                     [mutable-set :refer [mutable-set-intersection]]
                     [debug :refer [simplify-for-print current-value]]
                     [orderable :as orderable]
                     [dom-utils
                      :refer [into-attributes dom-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq cache]])
            (cosheet.server
             [key :refer [item-referent content-referent
                          comment-referent key-referent
                          content-location-referent
                          elements-referent query-referent
                          parallel-referent
                          prepend-to-key elements-referent?
                          item-referent? first-primitive-referent
                          remove-first-primitive-referent
                          semantic-elements
                          canonicalize-list semantic-to-list
                          replace-nones]]
             [hierarchy :refer [canonical-info canonical-set-to-list
                                hierarchy-node? hierarchy-node-descendants
                                hierarchy-node-members flatten-hierarchy-node
                                hierarchy-node-next-level hierarchy-node-extent
                                hierarchy-nodes-extent
                                hierarchy-node-items-referent
                                hierarchy-by-canonical-info
                                item-maps-by-elements
                                items-hierarchy-by-elements
                                hierarchy-node-example-elements]])))

;;; TODO: hierarchy-nodes-extent should be aware of refinements of conditions,
;;;       not just added conditions.

;;; Code to create hiccup style dom for a database entity.

;;; For a basic entity, we show its contents and its user semantic
;;; elements, but not its non-semantic elements. The latter are
;;; identified by, themselves, having elements with keyword contents.
;;; in addition, an entity may be marked as a tag, by having
;;; an element whose content is :tag. The :tag mark is considered
;;; semantic.
;;; So, for example, the entity:
;;;    ("Joe"
;;;        ("married" (1 :order)
;;;        (39 (2: order)
;;;            ("age" :tag)
;;;            "doubtful"))
;;; would be rendered to dom that lays out like:
;;;   Joe
;;;     married
;;;     age: 39
;;;            doubtful

;;; We use attributes, as supported by hiccup, to store both html
;;; attributes, and additional attributes that are used by the server.
;;; There are removed by the dom manager before dom is sent to the client.
(def server-specific-attributes
  [               :key  ; A unique client side key further described below.
             :commands  ; a map from command symbol to expression
                        ; to carry out that command. The function
                        ; of the expression will be called with:
                        ; the mutable store, the key of the item,
                        ; the additional arguments passed from
                        ; the UI, and the rest of the command expression.
   ])

;;; The value of the style attribute is represented with its own map,
;;; rather than as a string, so they are easier to adjust. Conviently,
;;; reagent accepts that format too.

;;; We don't create the entire dom in one call, because we want to be
;;; able to reuse subsidiary parts of the dom that the client has,
;;; even if the outer level of the dom changes. For example, if the
;;; outermost dom node adds a new child, we don't want to re-compute,
;;; or re-transmit its other children.

;;; Instead, we generate dom that has subsidiary components. These are
;;; specified as
;;;   [:component {:key <key>  See key.clj for more on keys.
;;;                <other attributes to add to the definition's
;;;                 result>}
;;;               definition}
;;; The dom_tracker code understands these components. It will give
;;; the client a dom with these subsidiary components, all with the
;;; provided attributes already present, and it will create additional
;;; computations to compute the dom for the components, and pass them
;;; as updates to the client once they are computed.

;;; The emitted dom uses CSS classes to indicate formatting. One group
;;; of classes gives information about how a div fits into a column of
;;; values:
;;;            column  The div spans the entire column.
;;;              tags  The column spaning div contains tag elements at
;;;                    top level. It may get "ll-corner".
;;;          full-row  This is the most deeply nested div under a tags
;;;                    that contains all the items for its row, and
;;;                    spans the full height of its row. If indented,
;;;                    it may not span its full column. Its borders
;;;                    will be set explicitly with "top-border" and
;;;                    "bottom-border"
;;;             stack  The div contains several items for its row.
;;;                    It might not fill the entire column.
;;;     with-children  Rows below this full-row are logical children.
;;;      for-multiple  Everything in the full-row pertains to multiple items.
;;;              item  The div contains an item.
;;;     with-elements  The item div contains elements.
;;; Several of these classes can pertain to a single div. For
;;; example, a row with a single non-indented item would have all of
;;; "column", "full-row", and "item".

(defn orderable-comparator
  "Compare two sequences each of whose first element is an orderable."
  [a b]
  (orderable/earlier? (first a) (first b)))

(defn order-items
  "Return the items in the proper sort order."
  [items]
  (expr-let [order-info
             (expr-seq map #(entity/label->content % :order) items)]
    (map second (sort orderable-comparator
                      (map (fn [order item]
                             ;; It is possible for an item not to have
                             ;; order information, especially
                             ;; temporarily while information is being
                             ;; propagated. Tolerate that.
                             (vector (or order orderable/initial) item))
                           order-info items)))))

(defn condition-satisfiers
  "Return a sequence of elements of an entity sufficient to make it
  satisfy the condition and nothing extra. The condition must be in list form.
  and have a nil content.
  If part of a condition is not satisfied by any element, ignore that part."
  [entity condition]
  (assert (and (sequential? condition)
               (not (empty? condition))
               (nil? (first condition))))
  (expr-let [elements (entity/elements entity)
             canonical-elements (expr-seq map canonical-info elements)]
    (multiset-to-generating-values
     (multiset (map canonical-info (rest condition)))
     canonical-elements elements)))

(def item-DOM)

(defn vertical-stack
  "If there is only one item in the doms, return it. Otherwise, return
  a vertical stack of the items. Add a separator between items if specified."
  [doms]
  (case (count doms)
    (if (= (count doms) 1)
      (first doms)
      (into [:div (if (empty? doms) {} {:class "stack"})] doms))))

(defn empty-DOM
  "Generate dom for an empty editable cell."
  [parent-key condition inherited]
  [:div {:class "editable"
         :key (prepend-to-key (elements-referent condition)
                              parent-key)
         :commands {:set-content [:do-create-content]}}])

(defn empty-DOM-with-position
  "Create DOM for an empty cell whose content should be
  in a particular logical position."
  [parent-key condition adjacent-key position]
  [:div {:class "editable"
         :key (prepend-to-key (elements-referent condition) parent-key)
         :commands {:set-content
                    [:do-create-content
                     :position position
                     :adjacent-key adjacent-key]
                    :add-row
                    [:do-add
                     :subject-key parent-key
                     :position position
                     :adjacent-key adjacent-key]}}])

(defn make-component
  "Make a component dom descriptor, with the given attributes and definition.
   The attributes must include a key."
  [attributes definition]
  (assert (map? attributes))
  (assert (:key attributes))
  [:component attributes definition])

(defn components-DOM
  "Given a non-empty list of [item, excluded-elements] pairs,
  generate DOM for a vertical list of a component for each item.
  Add the added attributes to the content of each item dom."
  [items-with-excluded parent-key condition content-attributes inherited]
  (assert (not (empty? items-with-excluded)))
  (let [item-doms (map (fn [[item excluded-elements]]
                           (let [key (prepend-to-key (item-referent item)
                                                     parent-key)]
                             (make-component
                              {:key key}
                              [item-DOM item parent-key
                               (set excluded-elements)
                               (into-attributes
                                {:commands {:add-sibling
                                            [:do-add
                                             :template condition]}}
                                content-attributes)
                               inherited])))
                         items-with-excluded)]
    (vertical-stack item-doms)))

(defn displaced-element-component
  "Return a component for an element that satisfies a condition and
  may be displayed under the condition, rather than under its parent."
  [element parent-key condition inherited & {:as content-attributes}]
  (assert (not (elements-referent? (first parent-key))))
  (expr-let [condition-specs (condition-satisfiers element condition)]
    (let [;; We must make this key different from what it would have
          ;; been if this element were displayed under its parent's
          ;; component, because it might have been in that situation.
          condition-key (prepend-to-key (comment-referent condition) parent-key)
          key (prepend-to-key (item-referent element) condition-key)]      
      (make-component {:key key}
                      [item-DOM element condition-key
                       (set condition-specs)
                       (into-attributes 
                        content-attributes
                        {:commands {:add-sibling [:do-add
                                                  :template condition]}})
                       inherited]))))

(defn add-adjacent-command
  "Return a command for adding an item adjacent to a key."
  [key]
  [:do-add
   :subject-key (remove-first-primitive-referent key)
   :adjacent-group-key key])

(defn empty-displaced-in-row-DOM
  "Return DOM for an empty set of displaced elements satisfying a condition,
  suitable for putting in a row about its parent."
  [parent-key condition]
  [:div {:key (prepend-to-key
               (elements-referent condition)
               ;; If a value gets put in here, it's key will have
               ;; the following comment. Adding it now lets
               ;; the action that will create the value know
               ;; how to select it.
               (prepend-to-key (comment-referent condition)
                               parent-key))
         :commands {:set-content [:do-create-content]
                    :add-row (add-adjacent-command parent-key)}
         :class "editable"}])

(defn displaced-elements-in-row-DOM
  "Given a possibly empty sequence of elements (possibly exemplary),
  return a DOM containing components for each of them, suitable for
  putting in a row about their parent(s)."
  [elements parent-key condition inherited]
  (if (empty? elements)
    (empty-displaced-in-row-DOM parent-key condition)
    (expr-let [components
               (expr-seq
                map
                #(displaced-element-component
                  % parent-key condition inherited
                  :commands {:add-row (add-adjacent-command parent-key)})
                elements)]
      (vertical-stack components))))

(defn add-row-header-border-info
  "Given the flattened hierarchy expansion of a top level node of a row
  header, add the information about what borders each node in the
  expansion should be responsible for."
  ;; Hierarchies make this a bit tricky. We use a separate table row
  ;; for each node of the hierarchy, letting us align the header and
  ;; items for each node. This means that all but the deepest nodes of
  ;; a hierarchy will be displayed as several rows of the table. A row
  ;; thus needs to be responsible not only for borders of its node,
  ;; but also for parts of the borders of nodes it is contained in. In
  ;; practice, this means that a row needs to handle the left border
  ;; of the outermost node it is in. By making nodes always resposible
  ;; for their own top border, we get the border lengths between nodes
  ;; right. Thus, when laying out a row, we need to know:
  ;;            :depth  in the hierarchy, for the indendation
  ;;       :top-border  :full or :indented
  ;;    :bottom-border  :corner, for only the lower left corner
  ;;     :for-multiple  true if this row applies to several items.
  ;; In addition, other processing may add
  ;;          :is-tags  true if this node holds tags
  [nodes]
  (let [nodes (vec nodes)
        n (count nodes)]
    (for [i (range n)]
      (as-> (nodes i) node
        (if (= i 0)
          (assoc node :top-border :full)
          (assoc node :top-border (if (>= (:depth node) 0) :indented :full)))
        (if (= i (dec n))
          ;; Don't add a bottom border to the last node, as it will be
          ;; handled by the top border of the first node of the next
          ;; flattened hierarchy. But we do need to add curvature.
          (assoc node :bottom-border :corner) 
          (if (< (:depth node) (:depth (nodes (+ i 1))))
            (assoc node :with-children true)
            node))
        (if (> (count (hierarchy-node-members node)) 1)
          (assoc node :for-multiple true)
          node)))))

(defn flatten-hierarchy-add-row-header-border-info
  [hierarchy]
  (update-last 
   (vec (mapcat
         #(add-row-header-border-info (flatten-hierarchy-node % 0))
         hierarchy))
   ;; We need to put on a final closing border.
   #(assoc % :bottom-border :full)))

(defn hierarchy-members-DOM
  "Given a hierarchy node with tags as the properties,
  generate DOM for the elements. common-condition is an additional
  condition that all members of the hierarchy satisfy.  The members 
  of the node may contain an additional :exclude-elements field that gives
  more of their elements not to show, typically the ones that satisfy
  common-condition."
  [hierarchy-node parent-key common-condition inherited]
  (let [items-with-excluded (map (fn [item]
                                   [(:item item)
                                    (concat (:property-elements item)
                                            (:exclude-elements item))])
                                 (hierarchy-node-members hierarchy-node))
        condition (concat common-condition
                          (canonical-set-to-list
                           (:cumulative-properties hierarchy-node)))]
    (if (empty? items-with-excluded)
      (let [adjacent-item (:item
                           (first (hierarchy-node-descendants hierarchy-node)))
            adjacent-key (prepend-to-key (item-referent adjacent-item)
                                         parent-key)]
        (empty-DOM-with-position parent-key condition adjacent-key :before))
      (components-DOM items-with-excluded
                      parent-key condition
                      {:commands {:add-row [:do-add]}}
                      inherited))))

(defn row-header-elements-DOM
  "Given information about the appearance of a flattened hierarchy
  node for a row header, and a sequence of elements in it, return a DOM
  containing components for each of them, wrapped as necessary to give
  the right appearance."
  [appearance-info elements condition row-key inherited]
  (assert (not (elements-referent? (first row-key))))
  ;; This code works by wrapping in successively more divs, if
  ;; necessary, and adding the right attributes at each level.
 (expr-let [dom (displaced-elements-in-row-DOM
                 elements row-key condition inherited)]
   (let [{:keys [is-tags top-border bottom-border
                 for-multiple with-children depth]} appearance-info]
     (as-> dom dom
       (if (empty? elements)
         dom
         [:div {:class "vertical-center-wrapper"} dom])          
       (add-attributes
        dom
        {:class (str "full-row"
                     (when (= top-border :indented) " top-border")
                     (when (= bottom-border :indented) " bottom-border")
                     (when with-children " with-children")
                     (when for-multiple " for-multiple"))})
       (let [depth (:depth appearance-info)]
         (if (> depth 0)
           [:div {:class "indent-wrapper"}
            (add-attributes dom {:class (str "depth-" depth)})]
           dom))
       (add-attributes
        dom
        {:class
         (str "row-header"
              (when is-tags " tags")
              (when (= top-border :full) " top-border")
              (when (= bottom-border :full) " bottom-border")
              (when (= bottom-border :corner) " ll-corner"))})))))

(defn tag-label-DOM
  "Given a flattened hierarchy node with tags as the info,
  generate DOM for a tag label."
  [hierarchy-node parent-key inherited]
  (let [appearance-info (select-keys hierarchy-node
                                     [:depth :for-multiple :with-children
                                      :top-border :bottom-border])
        example-elements (hierarchy-node-example-elements hierarchy-node)
        items-referent (hierarchy-node-items-referent hierarchy-node)]
    (expr row-header-elements-DOM
      (assoc appearance-info :is-tags true)
      (order-items example-elements) ; Why we need the expr
      '(nil :tag)
      (prepend-to-key items-referent parent-key)
      inherited)))

(defn tag-items-pair-DOM
  "Given a flattened hierarchy node,
  generate DOM for an element table row for the items."
  ;; TODO: for deep items, use a layout with tag above content to conserve
  ;; horizontal space.
  [hierarchy-node parent-key inherited]
  (expr-let [tags-label-dom (tag-label-DOM
                             hierarchy-node parent-key inherited)
             tags-items-dom (add-attributes
                             (hierarchy-members-DOM
                              hierarchy-node parent-key '(nil) inherited)
                             {:class "element"})]
    [:div {:class "element-row"} tags-label-dom tags-items-dom]))

(defn tagged-items-table-DOM
  "Return DOM for the given items, in order, as a grid of tags and values."
  [items parent-key inherited]
  (expr-let [labels (expr-seq map (partial matching-elements '(nil :tag))
                              items)
             hierarchy (items-hierarchy-by-elements
                        items labels (:do-not-merge inherited))
             flattened-hierarchy (flatten-hierarchy-add-row-header-border-info
                                  hierarchy)
             row-doms (expr-seq
                       map #(cache tag-items-pair-DOM
                                   % parent-key inherited)
                       flattened-hierarchy)]
    (into [:div {:class "element-table"}]
          (as-> row-doms row-doms
            (if (every? #(empty? (:property-elements
                                  (first (hierarchy-node-members %))))
                        flattened-hierarchy)
              (map #(add-attributes % {:class "no-tags"}) row-doms)
              row-doms)
            (update-in (vec row-doms) [(- (count row-doms) 1)]
                       #(add-attributes % {:class "last-row"}))))))

;;; An alternate format for tagged items, using a single column. Wrapper
;;; divs hold the labels.

(defn tagged-items-column-tags-DOM
  "Return DOM for the tags of one hierarchy node in a tagged-items columm."
  [hierarchy-node parent-key inherited]
  (let [example-elements (hierarchy-node-example-elements hierarchy-node)
        items-referent (hierarchy-node-items-referent hierarchy-node)]
    (displaced-elements-in-row-DOM
     example-elements (prepend-to-key items-referent parent-key)
     '(nil :tag) inherited)))

(defn tagged-items-column-subtree-DOM
  "Return DOM for the given hierarchy node and its descendants,
  as a single column."
  [hierarchy-node parent-key extra-condition
   top-level must-show-empty-labels inherited]
  (let [num-members (count (hierarchy-node-members hierarchy-node))]
    (expr-let [items-dom (when (not= num-members 0)
                           (hierarchy-members-DOM hierarchy-node parent-key
                                                  extra-condition inherited))
               ;; TODO: Need unit test for the case where there are children.
               child-doms (when (:children hierarchy-node)
                            (expr-seq map #(tagged-items-column-subtree-DOM
                                            % parent-key extra-condition
                                            false false inherited)
                                      (:children hierarchy-node)))]
      (let [content (if (> num-members 1)
                       (conj items-dom child-doms)
                       (vertical-stack
                        (if items-dom (cons items-dom child-doms) child-doms)))]
        (if (empty? (:properties hierarchy-node))
          (if must-show-empty-labels
            (conj (add-attributes
                   (empty-displaced-in-row-DOM
                    (prepend-to-key
                     (hierarchy-node-items-referent hierarchy-node)
                     parent-key)
                    '(nil :tag))
                   {:class "wrapped-element tags indent-wrapper"})
                  (add-attributes content {:class "depth-1"}))
            content)
          (expr-let [tags-dom (tagged-items-column-tags-DOM
                               hierarchy-node parent-key inherited)]
            [:div {:class "wrapped-element tags"}
             tags-dom
             [:div {:class "indent-wrapper"}
              (add-attributes content {:class "depth-1 has-border"})]]))))))

(defn possibly-tagged-items-column-DOM
  "Return DOM for the given items in order as a single column.
  Don't include tags needed to imply condition. If there are no tags,
  just give an ordinary column."
  [items parent-key condition must-show-empty-labels
   content-attributes inherited]
  (expr-let
      [all-labels (expr-seq
                   map #(matching-elements '(nil :tag) %) items)
       excluded (expr-seq
                 map #(condition-satisfiers % condition) items)]
    (let [labels (map (fn [all minus]
                        (seq (clojure.set/difference (set all) (set minus))))
                      all-labels excluded)]
      (if (and (every? empty? labels) (not must-show-empty-labels))
        (components-DOM (map vector items excluded)
                        parent-key condition content-attributes inherited)
        (expr-let [item-maps (item-maps-by-elements items labels)
                   augmented (map (fn [item-map excluded]
                                    (assoc item-map :exclude-elements excluded))
                                  item-maps excluded)
                   hierarchy (hierarchy-by-canonical-info
                              augmented (:do-not-merge inherited))
                   ;; TODO: Pipe content-attributes down through here.
                   doms (expr-seq map #(tagged-items-column-subtree-DOM
                                        % parent-key condition true
                                        must-show-empty-labels inherited)
                                  hierarchy)]
          (vertical-stack doms))))))

(defn item-DOM
  "Return a hiccup representation of DOM, with the given internal key,
   describing an item and all its elements, except the ones in
   excluded. Where appropriate, inherit properties from the map of
   inherited properties."
  [item parent-key excluded content-attributes inherited]
  (println "Generating DOM for"
           (simplify-for-print item) (simplify-for-print parent-key))
  (expr-let [content (entity/content item)
             elements (semantic-elements item)]
    (let [item-key (prepend-to-key (item-referent item) parent-key)
          elements (remove excluded elements)
          inherited-down (update-in inherited [:depth] inc)
          content-dom
          (if (entity/atom? content)
            (let [is-placeholder (and (symbol? content)
                                      (= (subs (str content) 0 3) "???"))]
              [:div (into-attributes
                     {:class (cond-> "content-text editable"
                               (empty? elements) (str " item")
                               is-placeholder (str " placeholder"))
                      :key (if (empty? elements)
                             item-key
                             (prepend-to-key (content-location-referent)
                                             item-key))
                      :commands {:set-content [:do-set-content]
                                 :delete [:do-delete]
                                 :add-element [:do-add :subject-key item-key]}}
                     content-attributes)
               (cond (= content :none) ""
                     is-placeholder "???"                     
                     true (str content))])
            ;; TODO: This doesn't support add-sibling.
            (make-component
             {:key (prepend-to-key (item-referent content) item-key)}
             [item-DOM content item-key #{} {}  inherited-down]))]
      (if (empty? elements)
        content-dom
        (expr-let [ordered-elements (order-items elements)
                   elements-dom
                   (if (:narrow inherited)
                     (possibly-tagged-items-column-DOM
                      ordered-elements item-key '(nil) true {} inherited-down)
                     (tagged-items-table-DOM
                      ordered-elements item-key inherited-down))]
          [:div {:class "item with-elements" :key item-key}
           content-dom elements-dom])))))

;;; Tables

(def base-table-column-width 150)

(defn table-header-key
  "Generate the key for the cell that holds a table header, given the
  info-maps of all the header requests it applies to, info maps that
  are sufficient to cover the elements it applies to, and the ones it
  must not apply to, as well as the table item, the parent key of the
  table, and the scope referent of the table."
  [info-maps extent-info-maps negative-info-maps
   table-item table-parent-key row-scope-referent]
  (prepend-to-key
    (parallel-referent
     []
     (vec (concat
           ;; All header requests the header applies to.
           (map (fn [info-map]
                  (key-referent [(item-referent (:item info-map))
                                 table-item]))
                   info-maps)
           ;; All row elements the header applies to.
           (let [excluded-elements (vec (map #(elements-referent (:item %))
                                             negative-info-maps))]
             (map (fn [info-map]
                    (let [elements (elements-referent (:item info-map))]
                      (parallel-referent
                       [(if (empty? excluded-elements)
                          elements
                          (parallel-referent [] [elements] excluded-elements))]
                       [row-scope-referent])))
                  extent-info-maps)))))
    table-parent-key))

(defn add-column-command
  "Return instructions to add a new column."
  [column-header-key column-condition]
  {:add-column [:do-add
                :subject-key (remove-first-primitive-referent column-header-key)
                :adjacent-group-key column-header-key
                :template column-condition]})

(defn table-header-node-elements-DOM
  "Generate the dom for one node of a table header hierarchy given its elements.
  parent-key gives the parent scope the header applies to, and
  elements-condition the condition that all the elements
  satisfy. column-key gives the key for just the header definition(s)
  of this column, and not the elements in rows, and new-column-condition
  gives the condition that new parallel columns must satisfy."
  [example-elements width parent-key elements-condition
   column-key new-column-condition delete-key rightmost inherited]
  (assert (not (elements-referent? (first parent-key))))
  (expr-let
      [ordered-elements (order-items example-elements)
       components (expr-seq map
                            #(displaced-element-component
                              % parent-key elements-condition inherited)
                            ordered-elements)
       element-lists (expr-seq map semantic-to-list ordered-elements)]
    (let [components (map-indexed
                      (fn [position component]
                        ;; Make the new column condition also require the
                        ;; elements above this one in the stack for this node.
                        (let [column-condition
                              (list* (concat new-column-condition
                                             (take position element-lists)))]
                          (add-attributes
                           component
                           {:commands (add-column-command
                                       column-key column-condition)})))
                          components)]
      (as-> (vertical-stack components) dom
        (cond delete-key (add-attributes
                          dom
                          {:commands {:delete
                                      [:do-delete :delete-key delete-key]}})
              true
              dom)
        (if (empty? ordered-elements)
          (add-attributes dom {:key (prepend-to-key
                                     (elements-referent elements-condition)
                                     parent-key)
                               :class "editable"
                               :commands (into (add-column-command
                                                column-key new-column-condition)
                                               {:set-content
                                                [:do-create-content]})})
          dom)
        (add-attributes dom {:class "column-header"})
        [:div {:class "column-header-container" 
               :style {:width (str width "px")}} dom]
        (if rightmost
          (add-attributes dom {:class "rightmost"})
          dom)
        (if (empty? ordered-elements)
          (add-attributes dom {:class "empty"})
          dom)
        (add-attributes dom {:class "tags"})))))

(defn make-column-header-key
  "Return a key that refers to all descendants of a column header,
  but not any of the rows it affects."
  [descendants table-item table-parent-key]
  (let [descendant-items (map :item descendants)
        column-referent (if (= (count descendants) 1)
                          (item-referent (first descendant-items))
                          (parallel-referent [] descendant-items))]
    (prepend-to-key column-referent
                    (prepend-to-key (item-referent table-item)
                                    table-parent-key))))

(defn table-header-node-DOM
  "Generate the dom for a node of a table header hierarchy.  The
  new-column-condition gives the condition that all headers under
  the parent must satisfy. The row-scope-referent should specify all
  the row items from which this header selects elements."
  [table-item node elements-condition table-parent-key
   new-column-condition row-scope-referent rightmost inherited]
  (let [next-level (hierarchy-node-next-level node)
        non-trivial-children (filter hierarchy-node? next-level)
        descendants (hierarchy-node-descendants node)
        num-descendants (count descendants)
        example-elements (hierarchy-node-example-elements node)
        key (table-header-key
             descendants (hierarchy-node-extent node) nil
             table-item table-parent-key row-scope-referent)
        column-header-key (make-column-header-key
                           descendants table-item table-parent-key)
        delete-key (when (= (count example-elements) 1)
                     ;; If we have children, then deletion affects the rows,
                     ;; while if we don't, then it just removes the column.
                     (if (empty? non-trivial-children)
                       column-header-key
                       (prepend-to-key
                        (item-referent (first example-elements))
                        (table-header-key
                         (mapcat hierarchy-node-descendants
                                 non-trivial-children)
                         (hierarchy-nodes-extent non-trivial-children)
                         nil
                         table-item table-parent-key
                         row-scope-referent))))]
    (table-header-node-elements-DOM
     example-elements
     (* num-descendants base-table-column-width)
     key elements-condition
     column-header-key new-column-condition delete-key rightmost
     (assoc inherited :narrow (<= num-descendants 1)))))

(defn last-special
  "Return a sequence with the given length as the incoming sequene,
  consisting of all nils, except for a final special value."
  [length final]
  (concat (repeat (dec length) nil) [final]))

(defn table-header-subtree-DOM
  "Generate the dom for a subtree of a table header hierarchy.  The
  new-column-condition gives the condition that all headers under
  the parent must satisfy. The row-scope-referent should specify all
  the row items from which this header selects elements."
  [table-item node elements-condition table-parent-key
   new-column-condition row-scope-referent top-level rightmost inherited]
  (expr-let
      [node-dom (table-header-node-DOM
                 table-item node elements-condition table-parent-key
                 new-column-condition row-scope-referent
                 rightmost inherited)]
    (let [node-dom (cond-> node-dom
                     top-level (add-attributes {:class "top-level"}))
          next-level (hierarchy-node-next-level node)
          non-trivial-children (filter hierarchy-node? next-level)]
      (if (and (= (count next-level) 1) (empty? non-trivial-children))
        node-dom
        (let [inherited (update-in inherited [:level] inc)
              exclude-from-members (hierarchy-nodes-extent non-trivial-children)
              new-subcolumn-condition (list* (concat new-column-condition
                                                     (canonical-set-to-list
                                                      (:properties node))))]
          (expr-let
              [dom-seqs (expr-seq
                         map
                         (fn [node rightmost]
                           (if (hierarchy-node? node)
                             (table-header-subtree-DOM
                              table-item node elements-condition
                              table-parent-key new-subcolumn-condition
                              row-scope-referent false rightmost inherited)
                             ;; We need a header DOM with no elements.
                             (let [key (table-header-key
                                        [node] [node]
                                        exclude-from-members table-item
                                        table-parent-key row-scope-referent)
                                   column-header-key (make-column-header-key
                                                      [node] table-item
                                                      table-parent-key)]
                               (table-header-node-elements-DOM
                                nil base-table-column-width
                                key elements-condition
                                column-header-key new-subcolumn-condition
                                column-header-key rightmost inherited))))
                         next-level
                         (last-special (count next-level) rightmost))]
            [:div {:class "column-header-stack"}
             (add-attributes node-dom {:class "with-children"})
             (into [:div {:class "column-header-sequence"}] dom-seqs)]))))))

(defn table-header-DOM
  "Generate DOM for column headers for the specified templates.
  The column will contain those elements of the rows that match the templates."
  [table-item hierarchy header-condition
   table-parent-key rows-referent inherited]
  (let [inherited (update-in inherited [:level] (fnil inc -1))]
    ;; Unlike row headers for tags, where the header information is
    ;; computed from the items of the rows, here the header information
    ;; is explicitly provided by the table definition, so the members of
    ;; the hierarchy are the column definitions.
    (expr-let [columns (expr-seq
                        map (fn [node rightmost]
                              (table-header-subtree-DOM
                               table-item node header-condition
                               table-parent-key
                               `(:none ~(cons '??? (rest header-condition))
                                       (:column :non-semantic))
                               rows-referent true rightmost inherited))
                        hierarchy (last-special (count hierarchy) true))]
      (into [:div {:class "column-header-sequence"}]
            columns))))

(defn table-cell-DOM
  "Return the dom for one cell of a table. The condition must be in list form."
  [items condition row-key new-row-condition cell-key inherited]
  (let [commands {:add-row
                  [:do-add
                   :subject-key (remove-first-primitive-referent row-key)
                   :adjacent-key row-key
                   :template new-row-condition]}]
    (if (empty? items)
      ;; TODO: Get our left neighbor as an arg, and pass it in
      ;; as adjacent information for new-sibling.
      (add-attributes (empty-DOM cell-key condition inherited)
                      {:class "table-cell has-border"
                       :commands commands})
      (expr-let [items (order-items items)
                 dom (possibly-tagged-items-column-DOM
                      items cell-key condition false
                      {:commands commands}
                      (assoc inherited :narrow true))]
        (add-attributes dom {:class "table-cell has-border"})))))

(defn table-row-column-group-member-DOM
  "Generate the dom for one member of a row under one hierarchy group."
  [row-item row-key new-row-condition member do-not-show inherited]
  (let [{:keys [item condition]} member
        exclusions (apply clojure.set/union (map set do-not-show))
        cell-key (prepend-to-key (comment-referent (item-referent item))
                                 row-key)]
    (expr-let [matches (matching-elements condition row-item)]
      (let [elements (seq (clojure.set/difference (set matches) exclusions))]
        (table-cell-DOM elements condition row-key
                        new-row-condition cell-key inherited)))))

(defn table-row-column-group-DOM
  "Generate the dom for the cells of a row under one hierarchy group."
  [row-item row-key new-row-condition node inherited]
  (let [next-level (hierarchy-node-next-level node)
        non-trivial-children (filter hierarchy-node? next-level)
        excluded-conditions (map :condition
                                 (hierarchy-nodes-extent non-trivial-children))]
    (expr-let
        [do-not-show
         (when excluded-conditions
           (expr-seq map #(matching-elements % row-item)
                     excluded-conditions))
         cell-lists
         (expr-seq map
                   #(if (hierarchy-node? %)
                      (table-row-column-group-DOM
                       row-item row-key new-row-condition
                       % inherited)
                      (expr-let [dom (table-row-column-group-member-DOM
                                      row-item row-key new-row-condition %
                                      do-not-show inherited)]
                        [dom]))
                   next-level)]
      (apply concat cell-lists))))

(defn table-row-DOM-contents
  "Generate the contents for a component corresponding to a table row."
  [row-item new-row-condition hierarchy row-key inherited]
  (expr-let [cell-groups (expr-seq
                          map #(table-row-column-group-DOM
                                row-item row-key new-row-condition %
                                inherited)
                          hierarchy)]
    (into [:div {:key row-key}]
          (apply concat cell-groups))))

(defn table-row-DOM-component
  "Generate dom for one row of a possibly hierarchical table."
  [row-item table-item new-row-condition hierarchy table-parent-key inherited]
  (let [row-key (prepend-to-key
                 (item-referent row-item)
                 (prepend-to-key
                  (comment-referent (item-referent table-item))
                  table-parent-key))]
    (make-component
     {:key row-key :class "table-row"}
     [table-row-DOM-contents
      row-item new-row-condition hierarchy row-key inherited])))

(defn add-element-to-entity-list
  [entity element]
  (concat (if (sequential? entity) entity (list entity))
          element))

(defn table-DOM
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
  ;;       says what new columns must have, rather than the current '(nil :tag)
  ;; TODO: Add the "other" column it a table requests it.
  ;; TODO: Check that the elements-referent for cells uses the header item
  ;;       that contains the condition, so the key won't change
  ;;       if the header value changes.
  [table-item parent-key inherited]
  (println "Generating DOM for table" (simplify-for-print table-item))
  (assert (satisfies? entity/StoredEntity table-item))
  (let [store (:store table-item)]
    (expr-let [row-query-item (entity/label->content table-item :row-query)]
      ;; Don't do anything if we don't yet have the table information filled in.
      (when row-query-item
        (expr-let [basic-row-query (semantic-to-list row-query-item)
                   row-query (add-element-to-entity-list
                              (replace-nones basic-row-query)
                              ['(:top-level :non-semantic)])
                   columns (expr order-items
                             (entity/label->elements table-item :column))
                   columns-elements (expr-seq map semantic-elements columns)
                   columns-lists (expr-seq map
                                           #(expr-seq map semantic-to-list %)
                                           columns-elements)
                   row-items (expr order-items
                               (matching-items row-query store))
                   hierarchy (hierarchy-by-canonical-info
                              (map (fn [column elements lists]
                                     {:item column
                                      :property-elements elements
                                      :property-canonicals (map
                                                            canonicalize-list
                                                            lists)
                                      :condition (replace-nones
                                                  (list* (into '[nil] lists)))})
                                   columns
                                   columns-elements
                                   columns-lists)
                              #{})
                   headers (table-header-DOM table-item hierarchy
                                             '(nil :tag) parent-key
                                             (query-referent row-query)
                                             inherited)
                   rows (expr-seq map
                                  #(table-row-DOM-component
                                    % table-item row-query hierarchy
                                    parent-key inherited)
                                  row-items)]
          (into [:div {:class "table"
                       :key (prepend-to-key (item-referent table-item)
                                            parent-key)} headers]
                rows))))))

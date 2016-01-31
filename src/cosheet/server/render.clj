(ns cosheet.server.render
  (:require (cosheet [entity :as entity]
                     [query :refer [matching-elements matching-items]]
                     [utils :refer [multiset multiset-diff multiset-union
                                    update-last]]
                     [mutable-set :refer [mutable-set-intersection]]
                     [debug :refer [simplify-for-print current-value]]
                     [orderable :as orderable]
                     [dom-utils
                      :refer [into-attributes dom-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq cache]])
            (cosheet.server [key :refer [item-referent content-referent
                                         comment-referent key-referent
                                         content-location-referent
                                         elements-referent query-referent
                                         parallel-referent semantic-entity?
                                         prepend-to-key elements-referent?
                                         item-referent? first-primitive-referent
                                         semantic-elements filtered-items
                                         canonicalize-list semantic-to-list
                                         replace-nones]])))

;;; TODO: Are members of hierarchy getting ordered?
;;; TODO: hierarchy-nodes-extent should be aware of refiments of conditions,
;;;       not just added conditions.

;;; Code to create hiccup style dom for a database entity.

;;; For a basic entity, we show its contents and its user semantic
;;; elements, but not its non-semantic elements. The latter are
;;; identified by, themselves, having elements with keyword contents.
;;; in addition, an entity may be marked as a tag, by having
;;; an element whose content is 'tag. The 'tag mark is considered
;;; semantic.
;;; So, for example, the entity:
;;;    ("Joe"
;;;        ("married" (1 :order)
;;;        (39 (2: order)
;;;            ("age" 'tag)
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
    :sibling-condition  ; Condition that a sibling of this item must satisfy.
         :add-adjacent  ; A key that a new item for this empty dom
                        ; should be adjacent to in the ordering.
        :add-direction  ; Whether a new item for this empty dom
                        ; should come before or after :add-adjacent
          :row-sibling  ; A key that a new adjacent row should be a sibling
                        ; of. If the key indicates multiple items, the row
                        ; is a sibling of the last, or, if there are
                        ; multiple groups of items, the last of each
                        ; group.
        :row-condition  ; Condition that a new adjacent row must satisfy.
       :column-sibling  ; Analog of :row-sibling
     :column-condition  ; Analog of :column-sibling
           :delete-key  ; The key to identity items for a delete,
                        ; if different from the usual key.
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

(def canonical-to-list)

(defn canonical-set-to-list
  "Given a set of canonicalized lists or sets,
   with the set also in canonical form, return a list of the items."
  [set]
  (when (not (empty? set))
    (reduce (fn [result [key count]]
              (concat result
                      (repeat count (if (map? key)
                                      (canonical-set-to-list key)
                                      (canonical-to-list key)))))
            [] (seq set))))

(defn canonical-to-list
  "Given a canonicalized list form of an item or set of items,
  return a list form for it."
  [item]
  (if (sequential? item)
    (do (assert (= (count item) 2))
        (cons (canonical-to-list (first item))
              (canonical-set-to-list (second item))))
    item))

(defn canonical-info
  [entity]
  (expr-let [semantic (semantic-to-list entity)]
    (canonicalize-list semantic)))

(defn canonical-info-set
  "Given a seq of items, return a canonical representation of the items,
   treated as a multi-set."
  [entities]
  (expr-let [canonicals (expr-seq map canonical-info entities)]
    (multiset canonicals)))

(defn condition-specifiers
  "Return a sequence of elements of an entity sufficient to make it
  satisfy the condition and nothing extra. The condition must be in list form.
  and have a nil content.
  If part of a condition is not satisfied by any element, ignore that part."
  [entity condition]
  (assert (and (sequential? condition)
               (not (empty? condition))
               (nil? (first condition))))
  ;; Both the elements and the condition might have repeated items,
  ;; so we have to keep track of what has been used.
  (expr-let [elements (entity/elements entity)
             canonical-elements (expr-seq map canonical-info elements)]
    (let [elements-map (reduce (fn [elements-map [element canonical]]
                                 (update-in elements-map [canonical]
                                            #(conj % element)))
                               {} (map vector elements canonical-elements))]
      (first (reduce (fn [[result elements-map] sub-condition]
                       (let [canonical (canonical-info sub-condition)]
                         (if (empty? (get elements-map canonical))
                           [result elements-map]
                           [(conj result (peek (elements-map canonical)))
                            (update-in elements-map [canonical] pop)])))
                     [[] elements-map] (rest condition))))))

(defn canonical-info-to-generating-items
  "Given a canonical-info-set, a list of items, and a list of the
  canonical-infos of those items, return a list of items whose
  canonical-infos add up to the canonical-info-set."
  [canonical-info items item-canonicals]
  (let [;; A map from canonical-info to a vector of elements
        ;; with that info.
        canonicals-map (reduce (fn [map [item canonical]]
                                 (update-in map [canonical] #(conj % item)))
                               {} (map vector items item-canonicals))]
    (reduce (fn [result [info count]]
              (concat result (take count (canonicals-map info))))
            [] canonical-info)))

;;; A hierarchy, for our purposes here, organizes a bunch of "members"
;;; into a hierarchy based on common "groups". The information for
;;; each member is recorded as a canonical-info-set of groups. The hierarchy
;;; consists of vector of nodes. The groups for each member at or below
;;; a hierarchy node include the groups for that node.
;;; The data for each node consists of
;;;   :hierarchy-node  true
;;;           :groups  A canonical-info-set of the groups added by this node.
;;; :cumulatve-groups  A canonical-info-set the groups for members of this node.
;;;          :members  A vector of members exactly matching the
;;;                    cumulative-groups for this node.
;;;                    All members must come before all children in the order
;;;                    from which the hierarchy was built. This means that
;;;                    some children may contain members that would
;;;                    have qualified to be members of the node,
;;;                    except for coming after other non-members.
;;;         :children  An optional vector of child nodes.
;;; The :groups of a child node only includes the information not
;;; reflected in any of its parents. When a hierarchy is flattened,
;;; the cumulative information down through all parents is stored in
;;; :cumulative-groups.
;;; For example, if a node has :groups {:b 1}, and has a parent with
;;; :groups {:a 1}, and has no other ancestors, the :cumulative-groups is
;;; {:a 1 :b 1}
;;; There are no requirements on members, but some of the hierarchy
;;; building functions assume each member is itself a map, containing
;;;               :item  The item that is the member
;;; Other information may be present, including
;;;     :group-elements  The elements of the item that contribute
;;;                      to the :cumulative-groups of this node
;;;                      in the hierarchy
;;;   :group-canonicals  A list of canonical-info-sets for each element in
;;;                      :group-elements.
;;; For a flattened node, the :cumulative-groups for each member will be
;;; the aggregation of :group-canonicals. But we need them split out for
;;; each :group-elements

;;; TODO: Get rid of the distinction between :members and :children,
;;; having just children that can be either hierarchy nodes or leaf
;;; items. This is the information currently returned by
;;; hierarchy-next-level. To get there, make all the clients use the
;;; output of hierarchy-next-level, then switch the underlying data.

(defn hierarchy-node?
  [node]
  (contains? node :hierarchy-node))

(defn append-to-hierarchy
  "Given groups and the corresponding item, add them to the hierarchy.
  If top-level is true, we are at the top level of a hierarchy, in which
  case we don't merge items with empty groups."
  [hierarchy groups item top-level]
  (if (empty? hierarchy)
    [{:hierarchy-node true :groups groups :members [item]}]
    (let [last-entry (last hierarchy)]
      (if (and ;; Don't merge an empty group.
           (or (empty? (:groups last-entry)) (empty? groups))
           (or top-level
               ;; Unless we are below top level and both are empty
               (not (empty? (:groups last-entry)))
               (not (empty? groups))))
        (conj hierarchy {:hierarchy-node true :groups groups :members [item]})
        (let [[old-only new-only both] (multiset-diff (:groups last-entry)
                                                      groups)]
          (if (empty? old-only)
            (update-last
             hierarchy
             (if (and (empty? new-only) (not (contains? last-entry :children)))
               (fn [last] (update-in last [:members] #((fnil conj []) % item)))
               (fn [last] (update-in
                           last [:children]
                           #(append-to-hierarchy % new-only item false)))))
            (if (empty? both)
              (conj hierarchy {:hierarchy-node true
                               :groups groups
                               :members [item]})
              (append-to-hierarchy
               (update-last hierarchy
                            (fn [last]
                              {:hierarchy-node true
                               :groups both
                               :members []
                               :children [(assoc last :groups old-only)]}))
               groups item top-level))))))))

(defn split-by-do-not-merge-subset
  "Given a list of item maps, and a subset of items not to merge,
  return a list of lists, broken so that any item-info-map whose item
  is in the set gets its own list."
  [item-info-maps do-not-merge-subset]
  (first
   (reduce
    (fn [[result do-not-merge-with-prev] item-info-map]
      (cond (do-not-merge-subset (:item item-info-map))
            [(conj result [item-info-map]) true]
            do-not-merge-with-prev
            [(conj result [item-info-map]) false]
            true
            [(update-last result #((fnil conj []) % item-info-map)) false]))
    [[] false]
    item-info-maps)))

(defn hierarchy-by-canonical-info
  "Given a list of item info maps, and a subset of items in the maps not to
  merge, return a hierarchy."
  [item-info-maps do-not-merge]
  (let [items (map :item item-info-maps)
        item-to-item-info-maps (zipmap items item-info-maps)]
    (expr-let
        [do-not-merge-subset (mutable-set-intersection do-not-merge items)
         ordered-items (order-items items)]
      (let [ordered-maps (map item-to-item-info-maps ordered-items)]
        (mapcat
         #(reduce (fn [hierarchy item-info-map]
                    (append-to-hierarchy
                     hierarchy (multiset (:group-canonicals item-info-map))
                     item-info-map true))
                  [] %)
         (split-by-do-not-merge-subset ordered-maps do-not-merge-subset))))))

(defn items-hierarchy-by-condition
  "Given items, organize them into a hierarchy by their value on
  elements matching the condition. Don't merge items that are in
  do-not-merge. If extra-data-map is provided, it must be a map from
  label to map from item to data for that item. The data will be
  stored with the item-map for that item, under the key used in the
  extra data map."
  [items do-not-merge condition]
  (expr-let
      [item-maps (expr-seq
                  map
                  (fn [item]
                    (expr-let [elements (matching-elements condition item)
                               filtered (filtered-items semantic-entity?
                                                        elements)
                               canonicals (expr-seq
                                           map canonical-info filtered)]
                      {:item item
                       :group-elements filtered
                       :group-canonicals canonicals}))
                  items)]
    (hierarchy-by-canonical-info item-maps do-not-merge)))

(def flatten-hierarchy)

(defn flatten-hierarchy-node
  "Given a hierarchy node, a depth, and the combined info for all
  ancestors, return a flattened version of its hierarchy in
  pre-order. Add :cumulative-groups and :depth to the node and all its
  descendants."
  [node depth ancestor-info]
  (let [cumulative-info (multiset-union (:groups node) ancestor-info)]
    (cons (-> node
              (assoc :depth depth)
              (assoc :cumulative-groups cumulative-info))
          (flatten-hierarchy (:children node) (inc depth) cumulative-info))))

(defn flatten-hierarchy
  "Given a hierarchy and a depth, return a flattened version in pre-order,
  for each node of the hierarchy, also giving its depth."
  [hierarchy depth ancestor-info]
  (mapcat #(flatten-hierarchy-node % depth ancestor-info) hierarchy))

(defn hierarchy-node-descendants
  "Return all members at or below the node."
  [node]
  (concat (:members node) (mapcat hierarchy-node-descendants (:children node))))

(defn hierarchy-node-next-level
  "Return the concatenation of the members and children of the node.
  If any children have empty :groups, splice in their members."
  [node]
  (concat (:members node)
          (mapcat #(if (empty? (:groups %))
                     (do (assert (empty? (:children %)))
                         (:members %))
                     [%])
                  (:children node))))

(def hierarchy-nodes-extent)

(defn hierarchy-node-extent
  "Return a seq of members at or below the node
  that subsumes all its descendants and nothing more."
  [node]
  (if (seq (:members node))
    [(first (:members node))]
    ;; Check for a child with no groups. Its members work as extents.
    (if-let [child-members (seq (filter #(and (not (empty? (:members %)))
                                              (empty? (:groups %)))
                                        (:children node)))]
      [(first (:members (first child-members)))]
      (hierarchy-nodes-extent (:children node)))))

(defn hierarchy-nodes-extent
  "Return a seq of members at or below the nodes
  that subsume all their descendants and nothing more."
  [nodes]
  (seq (apply clojure.set/union (map #(set (hierarchy-node-extent %)) nodes))))

(defn stack-vertical
  "Make a dom stack vertically with its siblings."
  [dom]
  (let [style (:style (dom-attributes dom))
        display (:display style)
        width (:width style)]
    (add-attributes (if width dom (add-attributes dom
                                                  ;; TODO: Make this a
                                                  ;; CSS class.
                                                  {:style {:width "100%"}}))
                    {:style {:display
                             (case display
                               (nil "block" "inline-block") "block"
                               ("table" "inline-table") "table")}})))

(defn vertical-separated
  "Make a dom have separators between its siblings."
  [dom]
  (add-attributes dom {:class "vertical-separated"}))

(def item-DOM)

(defn vertical-stack
  "If there is only one item in the doms, return it. Otherwise, return
  a vertical stack of the items. Add a separator between items if specified."
  [doms & {:keys [separators]}]
  (case (count doms)
    (if (= (count doms) 1)
      (first doms)
      (into [:div]
            (map (if separators
                   (comp vertical-separated stack-vertical)
                   stack-vertical)
                 doms)))))

(defn make-component
  "Make a component dom descriptor, with the given attributes and definition.
   The attributes must include a key."
  [attributes definition]
  (assert (map? attributes))
  (assert (:key attributes))
  [:component attributes  definition])

(defn condition-component
  "Return the component for an element that satisfies a condition and
  may be displayed under the condition, rather than under its parent."
  [element condition parent-key inherited & {:as extra-attributes}]
  (assert (not (elements-referent? (first parent-key))))
  (expr-let [condition-specs (condition-specifiers element condition)]
    (let [;; We must make this key different from what it would have
          ;; been if this element were displayed under its parent's
          ;; component.  Because it might have been in that situation.
          condition-key (prepend-to-key (comment-referent condition) parent-key)
          key (prepend-to-key (item-referent element) condition-key)]      
      (make-component (into {:key key
                             ;; TODO: Use original condition?
                             :sibling-condition (cons nil (rest condition))}
                            extra-attributes)
                      [item-DOM element condition-key
                       (set condition-specs) inherited]))))

(defn empty-DOM
  "Generate dom for an empty editable cell. A map may be provided so
  that when content is added it will be put adjacent to the specified
  item in the specified direction."
  [parent-key sibling-elements inherited]
  (add-attributes (vertical-stack nil)
                  {:class "editable"
                   :key (prepend-to-key
                         (elements-referent (cons nil sibling-elements))
                         parent-key)}))

(defn components-DOM
  "Given a non-empty list of [item, excluded-elements] pairs,
  generate DOM for a vertical list of a component for each item."
  [items-with-excluded parent-key sibling-elements added-attributes inherited]
  (assert (not (empty? items-with-excluded)))
  (let [item-doms (map (fn [[item excluded-elements]]
                           (let [key (prepend-to-key (item-referent item)
                                                     parent-key)]
                             (make-component
                              (into
                               {:key key
                                :sibling-condition (cons nil sibling-elements)}
                               added-attributes)
                              [item-DOM item parent-key
                               (set excluded-elements) inherited])))
                         items-with-excluded)]
      (vertical-stack item-doms :separators true)))

(defn add-row-header-border-info
  "Given the flattened hierarchy expansion of a top level node of a row
  header, add the information about what borders each node in the
  expansion should be responsible for."
  ;; Hierarchies make this a bit tricky. We use a separate table row
  ;; for each node of the hierarchy, so we can align the header and
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
        (if (> (count (:members node)) 1)
          (assoc node :for-multiple true)
          node)))))

(defn flatten-hierarchy-add-row-header-border-info
  [hierarchy]
  (update-last 
   (vec (mapcat
         #(add-row-header-border-info (flatten-hierarchy-node % 0 {}))
         hierarchy))
   ;; We need to put on a final closing border.
   #(assoc % :bottom-border :full)))

(defn row-header-elements-DOM
  "Given information about the appearance of a flattened hierarchy
  node for a row header, and a sequence of elements in it, return a DOM
  containing components for each of them, wrapped as necessary to give
  the right appearance."
  [appearance-info elements condition row-item row-key inherited]
  (assert (not (elements-referent? (first row-key))))
  ;; This code works by wrapping in successively more divs, if
  ;; necessary, and adding the right attributes at each level.
  (expr-let [components
             (expr-seq map #(condition-component
                             % condition row-key inherited
                             :row-sibling row-key)
                       elements)]
    (let [num-elements (count elements)
          elements-key (prepend-to-key
                        (elements-referent condition) row-key)
          {:keys [is-tags top-border bottom-border
                  for-multiple with-children depth]} appearance-info]
      (as-> (vertical-stack components :separators true) dom
        (if (> num-elements 1)
          (add-attributes dom {:class "stack"})
          dom)
        (if (empty? elements)
          (add-attributes dom {:key elements-key})
          dom)
        (if (or for-multiple (> num-elements 1))
          [:div dom [:div {:class "spacer"}]]
          dom)
        (add-attributes
         dom
         {:class (str "full-row"
                      (when (= top-border :indented) " top-border")
                      (when (= bottom-border :indented) " bottom-border")
                      (when (empty? elements) " editable")
                      (when with-children " with-children")
                      (when for-multiple " for-multiple"))})
        (let [depth (:depth appearance-info)]
          (if (> depth 0)
            [:div (add-attributes dom {:class (str "indent-" depth)})]
            dom))
        (add-attributes
         dom (cond-> {:class
                      (str "column"
                           (when is-tags " tags")
                           (when (= top-border :full) " top-border")
                           (when (= bottom-border :full) " bottom-border")
                           (when (= bottom-border :corner) " ll-corner"))}
               (> num-elements 1)
               (into {:key elements-key})
               (not= num-elements 1)
               (into {:row-sibling row-key})))))))

(defn tag-label-DOM
  "Given a flattened hierarchy node with tags as the info,
  generate DOM for a tag label."
  [hierarchy-node parent-key inherited]
  (let [descendants (hierarchy-node-descendants hierarchy-node)
        example (first descendants)
        appearance-info (select-keys hierarchy-node
                                     [:depth :for-multiple :with-children
                                      :top-border :bottom-border])
        example-elements (canonical-info-to-generating-items
                          (:groups hierarchy-node)
                          (:group-elements example) (:group-canonicals example))
        affected-items (map :item descendants)
        items-referent (if (= (count affected-items) 1)
                         (item-referent (first affected-items))
                         (parallel-referent [] affected-items))]
    (expr row-header-elements-DOM
      (assoc appearance-info :is-tags true)
      (order-items example-elements) ; Why we need the expr
      '(nil tag)
      (:item example)
      (prepend-to-key items-referent parent-key)
      inherited)))

(defn tag-items-DOM
  "Given a flattened hierarchy node with tags as the info,
  generate DOM for the elements."
  [hierarchy-node parent-key inherited]
  (let [items-with-excluded (map #((juxt :item :group-elements) %)
                                 (:members hierarchy-node))
        sibling-elements (canonical-set-to-list
                          (:cumulative-groups hierarchy-node))]
    (if (empty? items-with-excluded)
      (let [adjacent-item (:item
                           (first (hierarchy-node-descendants hierarchy-node)))]
        (add-attributes
         (empty-DOM parent-key sibling-elements inherited)
         {:add-adjacent (prepend-to-key (item-referent adjacent-item)
                                        parent-key)
          :add-direction :before
          :class "column"}))
      (add-attributes
       (components-DOM items-with-excluded
                       parent-key sibling-elements {} inherited)
       {:class "column"}))))

(defn tag-items-pair-DOM
  "Given a flattened hierarchy node,
  generate DOM for an element table row for the items."
  ;; TODO: for deep items, use a layout with tag above content to conserve
  ;; horizontal space.
  [hierarchy-node parent-key inherited]
  (expr-let [tags-label-dom (tag-label-DOM
                             hierarchy-node parent-key inherited)
             tags-items-dom (tag-items-DOM
                             hierarchy-node parent-key inherited)]
    (into [:div {:style {:display "table-row"}}]
          (map #(add-attributes % {:style {:display "table-cell"}})
               [tags-label-dom tags-items-dom]))))

;;; TODO: make this use flex boxes, rather than a table,
;;;       to get the full heights.
(defn tagged-items-table-DOM
  "Return DOM for the given items, as a grid of tags and values."
  ;; We use a table as a way of making all the cells of a row
  ;; be the same height.
  [items parent-key inherited]
  (expr-let [hierarchy (items-hierarchy-by-condition
                        items (:do-not-merge inherited) '(nil tag))
             flattened-hierarchy (flatten-hierarchy-add-row-header-border-info
                                  hierarchy)
             row-doms (expr-seq
                       map #(cache tag-items-pair-DOM
                                   % parent-key inherited)
                       flattened-hierarchy)]
    (into [:div {:class "element-table"
                 :style {:height "1px" ;; So height:100% in rows will work.
                         :display "table" :table-layout "fixed"}}]
          (as-> row-doms row-doms
            (if (every? #(empty? (get-in % [:members 0 :group-elements]))
                        flattened-hierarchy)
              (map #(add-attributes % {:class "no-tags"}) row-doms)
              row-doms)
            (update-in (vec row-doms) [(- (count row-doms) 1)]
                       #(add-attributes % {:class "last-row"}))))))

;;; TODO: Make this handle an item that needs to be wrapped in some labels.
(defn item-DOM
  "Return a hiccup representation of DOM, with the given internal key,
   describing an item and all its elements, except the ones in
   excluded. Where appropriate, inherit properties from the map of
   inherited properties."
  [item parent-key excluded inherited]
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
              [:div {:class (cond-> "content-text editable"
                              (empty? elements) (str " item")
                              is-placeholder (str " placeholder"))
                     :key (if (empty? elements)
                            item-key
                            (prepend-to-key (content-location-referent)
                                            item-key))}
               (cond (= content :none) ""
                     is-placeholder "???"                     
                     true (str content))])
            (make-component
             {:key (prepend-to-key (item-referent content) item-key)}
             [item-DOM content item-key #{} inherited-down]))]
      (if (empty? elements)
        content-dom
        (expr-let [elements-dom (tagged-items-table-DOM
                                 elements item-key inherited-down)]
          (add-attributes (vertical-stack [content-dom elements-dom])
                          {:class "item with-elements" :key item-key}))))))

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

(defn table-header-node-DOM
  "Generate the dom for one node of a table header hierarchy.
  parent-key gives the parent scope the header applies to, and
  elements-condition the condition that all the elements
  satisfy. column-key gives the key for just the header definition(s)
  of this column, and not the elements in rows, and new-column-condition
  gives the condition that new parallel columns must satisfy."
  [elements width parent-key elements-condition
   column-key new-column-condition delete-key rightmost inherited]
  (assert (not (elements-referent? (first parent-key))))
  (expr-let [components
             (expr-seq map
                       #(condition-component
                         % elements-condition parent-key inherited)
                       elements)]
    (let [num-elements (count elements)
          components (map #(add-attributes %
                            {:column-sibling column-key
                             :column-condition new-column-condition
                             :row-condition elements-condition})
                          components)]
      (as-> (vertical-stack components :separators true) dom
        (cond (> num-elements 1)
              (add-attributes dom {:class "stack"})
              delete-key (add-attributes dom {:delete-key delete-key})
              true
              dom)
        (if (empty? elements)
          (add-attributes dom {:key (prepend-to-key
                                     (elements-referent elements-condition)
                                     parent-key)
                               :class "editable"
                               :column-sibling column-key
                               :column-condition new-column-condition})
          dom)
        (add-attributes dom {:class "column-header"})
        [:div {:class "column-header-container" 
               :style {:width (str width "px")}} dom]
        (if rightmost
          (add-attributes dom {:class "rightmost"})
          dom)
        (if (zero? num-elements)
          (add-attributes dom {:class "empty"})
          dom)
        (add-attributes dom {:class "tags"})))))

(defn last-special
  "Return a sequence with the same length as the incoming sequence,
  consisting of all nils, except for a final special value."
  [seq final]
  (concat (repeat (dec (count seq)) nil) [final]))

(defn table-header-subtree-DOM
  "Generate the dom for a subtree of a table header hierarchy.  The
  new-column-condition gives the condition that all headers under
  the parent must satisfy. The row-scope-referent should specify all
  the row items from which this header selects elements."
  [table-item node elements-condition table-parent-key
   new-column-condition row-scope-referent top-level rightmost inherited]
  (let [descendants (hierarchy-node-descendants node)
        width (count descendants)
        new-subcolumn-condition (list* (concat new-column-condition
                                               (canonical-set-to-list
                                                (:groups node))))   
        column-referent (if (= (count descendants) 1)
                          (item-referent (:item (first descendants)))
                          (parallel-referent [] (map :item descendants)))
        column-key (prepend-to-key column-referent
                                   (prepend-to-key (item-referent table-item)
                                                   table-parent-key))
        example (first descendants)
        example-elements (canonical-info-to-generating-items
                          (:groups node) 
                          (:group-elements example) (:group-canonicals example))
        next-level (hierarchy-node-next-level node)
        non-trivial-children (filter hierarchy-node? next-level)]
    (expr-let
        [node-dom (let [key (table-header-key
                             descendants (hierarchy-node-extent node) nil
                             table-item table-parent-key row-scope-referent)]
                    (expr table-header-node-DOM
                      (order-items example-elements) ; Why we need the expr.
                      (* (count descendants) base-table-column-width)
                      key elements-condition
                      column-key new-column-condition
                      (when (= (count example-elements) 1)
                        (if (empty? non-trivial-children)
                          column-key
                          (prepend-to-key
                           (item-referent (first example-elements))
                           (table-header-key
                            (mapcat hierarchy-node-descendants
                                    non-trivial-children)
                            (hierarchy-nodes-extent non-trivial-children)
                            nil
                            table-item table-parent-key row-scope-referent))))
                      rightmost inherited))]
      (if (and (= (count next-level) 1) (empty? non-trivial-children))
        (cond-> node-dom
          top-level (add-attributes {:class "top-level"}))
        (let [inherited (update-in inherited [:level] inc)
              exclude-from-members (hierarchy-nodes-extent
                                    non-trivial-children)]
          (expr-let
              [dom-seqs (expr-seq
                         map
                         (fn [node rightmost]
                           (if (hierarchy-node? node)
                             (table-header-subtree-DOM
                              table-item node elements-condition
                              table-parent-key
                              new-subcolumn-condition
                              row-scope-referent false rightmost inherited)
                             ;; We need a header DOM with no elements.
                             (let [key (table-header-key
                                        [node] [node]
                                        exclude-from-members table-item
                                        table-parent-key row-scope-referent)
                                   column-key (prepend-to-key
                                               (item-referent (:item node))
                                               (prepend-to-key
                                                (item-referent table-item)
                                                 table-parent-key))]
                               (table-header-node-DOM
                                nil base-table-column-width
                                key elements-condition
                                column-key new-subcolumn-condition
                                column-key rightmost inherited))))
                         next-level (last-special next-level rightmost))]
            [:div {:class "column-header-stack"}
             (cond-> (add-attributes node-dom {:class "with-children"})
               top-level (add-attributes {:class "top-level"}))
             (into [:div {:class "column-header-sequence"}] dom-seqs)
             ]))))))

(defn table-header-DOM
  "Generate DOM for column headers for the specified templates.
  The column will contain those elements of the rows that match the templates."
  [table-item hierarchy header-condition
   table-parent-key rows-referent inherited]
  (let [inherited (update-in inherited [:level] (fnil inc -1))]
    ;; Unlike row headers for tags, where the header information is
    ;; computed from the items of the rows, here the header information
    ;; explicitly provided by the table definition, so the "items" for
    ;; the hierarchy are the column definitions.
    (expr-let [columns (expr-seq
                        map (fn [node rightmost]
                              (table-header-subtree-DOM
                               table-item node header-condition
                               table-parent-key
                               `(:none ~(cons '??? (rest header-condition))
                                       (:column :non-semantic))
                               rows-referent true rightmost inherited))
                        hierarchy (last-special hierarchy true))]
      (into [:div {:class "column-header-sequence"}]
            columns))))

(defn table-cell-DOM
  "Return the dom for one cell of a table. The condition must be in list form."
  [items condition row-item row-key new-row-condition cell-key inherited]
  (if (empty? items)
    ;; TODO: Get our left neighbor as an arg, and pass it in
    ;; as adjacent information.
    (add-attributes (empty-DOM cell-key (rest condition) inherited)
                    {:class "table-cell"
                     :row-sibling row-key
                     :row-condition new-row-condition})
    (expr-let [items (order-items items)
               excluded (expr-seq map #(condition-specifiers % condition)
                                  items)]
      (add-attributes
       (components-DOM (map vector items excluded)
                       cell-key (rest condition)
                       {:row-sibling row-key
                        :row-condition new-row-condition}
                       inherited)
       {:class "table-cell"}))))

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
         make-member-cell-DOM
         (let [exclusions (apply clojure.set/union (map set do-not-show))]
           (fn [member]
             (let [{:keys [item condition]} member]
               (expr-let [matches (matching-elements condition row-item)]
                 (let [items (seq (clojure.set/difference (set matches)
                                                          exclusions))
                       cell-key (prepend-to-key
                                 (comment-referent (item-referent item))
                  row-key)]
                   (table-cell-DOM items condition row-item row-key
                                   new-row-condition cell-key inherited))))))
         cell-lists
         (expr-seq map
                   #(if (hierarchy-node? %)
                      (table-row-column-group-DOM
                       row-item row-key new-row-condition
                       % inherited)
                      (expr-let [dom (make-member-cell-DOM %)]
                        [dom]))
                   next-level)]
      (apply concat cell-lists))))

(defn table-row-DOM
  "Generate the dom for one row of a possibly hierarchical table."
  [row-item new-row-condition hierarchy row-key inherited]
  (expr-let [cell-groups (expr-seq
                          map #(table-row-column-group-DOM
                                row-item row-key new-row-condition %
                                inherited)
                          hierarchy)]
    ;; TODO: remove the class?
    (into [:div {:class "table-row"
                 :key row-key}]
          (apply concat cell-groups))))

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
  ;;       says what new columns must have, rather than the current '(nil tag)
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
                                      :group-elements elements
                                      :group-canonicals (map canonicalize-list
                                                             lists)
                                      :condition (replace-nones
                                                  (list* (into '[nil] lists)))})
                                   columns
                                   columns-elements
                                   columns-lists)
                              #{})
                   headers (table-header-DOM table-item hierarchy
                                             '(nil tag) parent-key
                                             (query-referent row-query)
                                             inherited)
                   rows (expr-seq
                         map
                         (fn [row-item]
                           ;; Add the table as a comment to the row key, for uniqueness.
                           (let [row-key (prepend-to-key
                                          (item-referent row-item) parent-key)]
                             (make-component
                              {:key row-key
                               :class "table-row"}
                              [table-row-DOM row-item row-query hierarchy
                               row-key inherited])))
                         row-items)]
          (into [:div {:class "table"
                       :key (prepend-to-key (item-referent table-item)
                                            parent-key)} headers]
                rows))))))

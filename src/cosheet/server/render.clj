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
                                         elements-referent query-referent
                                         parallel-referent visible-entity?
                                         prepend-to-key elements-referent?
                                         visible-elements filtered-items
                                         canonicalize-list visible-to-list]])))

;;; Code to create hiccup style dom for a database entity.

;;; For a basic entity, we show its contents and its user visible
;;; elements, but not its non-user visible elements. The latter are
;;; identified by, themselves, having elements with keyword contents.
;;; in addition, an entity may be marked as a tag, by having
;;; an element whose content is 'tag. The 'tag mark is considered
;;; visible, because it changes the visible appearance of its element,
;;; and, more importantly, matters for whether two items are
;;; considered equal, even though it is not shown in the usual way.
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
     :sibling-elements  ; Elements that a sibling of this item must have.
         :add-adjacent  ; A key that a new item for this empty dom
                        ; should be adjacent to in the ordering.
        :add-direction  ; Whether a new item for this empty dom
                        ; should come before or after :add-adjacent
          :row-sibling  ; A key that a new adjacent row should be a sibling
                        ; of. If the key indicates multiple items, the row
                        ; is a sibling of the last, or, if there are
                        ; multiple groups of items, the last of each
                        ; group.
         :row-elements  ; Elements that a new adjacent row must have.
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
  "Given a set of canonicalized lists, with the set also in canonical form,
  return a list of the items"
  [set]
  (when (not (empty? set))
    (reduce (fn [result [key count]]
              (concat result (repeat count (canonical-to-list key))))
            [] (seq set))))

(defn canonical-to-list
  "Given a canonicalized list form of an item, return a list form for it."
  [item]
  (if (sequential? item)
    (do (assert (= (count item) 2))
        (cons (canonical-to-list (first item))
              (canonical-set-to-list (second item))))
    item))

(defn canonical-info
  [entity]
  (expr-let [visible (visible-to-list entity)]
    (canonicalize-list visible)))

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
  (println "condition specifiers for" condition)
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

;;; A hierarchy, for our purposes here, organizes a bunch of "members"
;;; into a hierarchy based on common "information". The data about
;;; each member is recorded as a canonical-info-set. The hierarchy
;;; consists of vector of nodes. All members at or below a hierarchy
;;; node share all the common information for that node.
;;; The data for each node consists of
;;;           :info  A canonical-info-set of the new common info at this node.
;;; :cumulatve-info  A canonical-info-set all common info for this node.
;;;        :members  A vector of members exactly matching the cumulative-info
;;;                  for this node.
;;;       :children  An optional vector of child nodes.
;;; The :info of a child node only includes the information not
;;; reflected in any of its parents. When a hierarchy is flattened,
;;; the cumulative information down through all parents is stored in
;;; :cumulative-info.
;;; For example, if a node has :info {:b 1}, and has a parent with
;;; :info {:a 1}, and has no other ancestors, the :cumulative-info is
;;; {:a 1 :b 1}
;;; There are no requirements on members but as typically used,
;;; each member is itself a map, containing
;;;              :item  The item that is the member
;;;     :info-elements  The elements of the item that contribute
;;;                     to the hierarchy info
;;;   :info-canonicals  The canonical-info for all the item in :info-elements
;;; For a flattened node, the :cumulative-info for each member will be
;;; the same as :info-canonicals. But a node might not have any
;;; members, so we still need :info-canonicals.

;;; TODO: Change hierarchy creation to assume that the member
;;; has :info-canonicals present.

(defn append-to-hierarchy
  "Given an info and corresponding item, add them to the hierarchy."
  [hierarchy info item]
  (if (empty? hierarchy)
    [{:info info :members [item]}]
    (let [last-entry (last hierarchy)]
      (if (or (empty? (:info last-entry)) (empty? info))
        (conj hierarchy {:info info :members [item]})
        (let [[old-only new-only both] (multiset-diff (:info last-entry) info)]
          (if (empty? old-only)
            (update-last
             hierarchy
             (if (and (empty? new-only) (not (contains? last-entry :children)))
               (fn [last] (update-in last [:members] #((fnil conj []) % item)))
               (fn [last] (update-in last [:children]
                                     #(append-to-hierarchy % new-only item)))))
            (if (empty? both)
              (conj hierarchy {:info info :members [item]})
              (append-to-hierarchy
               (update-last hierarchy
                            (fn [last]
                              {:info both
                               :members []
                               :children [(assoc last :info old-only)]
                               }))
               info item))))))))

(defn split-by-subset
  "Given a list of infos and item maps, and a subset of items not to merge,
  return a list of lists, broken so that any info-and-item-maps whose item
  is in the set gets its own list."
  [infos-and-item-maps do-not-merge-subset]
  (first
   (reduce
    (fn [[result do-not-merge-with-prev] info-and-item-map]
      (cond (do-not-merge-subset (:item (second info-and-item-map)))
            [(conj result [info-and-item-map]) true]
            do-not-merge-with-prev
            [(conj result [info-and-item-map]) false]
            true
            [(update-last result #((fnil conj []) % info-and-item-map)) false]))
    [[] false]
    infos-and-item-maps)))

(defn hierarchy-by-canonical-info
  "Given a list of canonical-set-info and item maps for each,
  and a subset of items in the maps not to merge, return a hierarchy."
  [infos-and-item-maps do-not-merge-subset]
  (mapcat
   #(reduce (fn [hierarchy [info item-map]]
             (append-to-hierarchy hierarchy info item-map))
           []
           %)
   (split-by-subset infos-and-item-maps do-not-merge-subset)))

(defn items-hierarchy-by-condition
  "Given items, organize them into a hierarchy by their value
  on elements matching the condition. Don't merge items that are in
  do-not-merge."
  [items do-not-merge condition]
  (expr-let [do-not-merge-subset (mutable-set-intersection do-not-merge items)
             item-maps
             (expr-seq
              map
              (fn [item]
                (expr-let [info-elements (matching-elements condition item)
                           visible-info-elements (filtered-items
                                                  visible-entity? info-elements)
                           info-canonicals (expr-seq map canonical-info
                                                     visible-info-elements)]
                  {:item item
                   :info-elements visible-info-elements
                   :info-canonicals info-canonicals}))
              (order-items items))]
    (hierarchy-by-canonical-info
     (map (fn [map] [(multiset (:info-canonicals map)) map])
          item-maps)
     do-not-merge-subset)))

(def flatten-hierarchy)

(defn flatten-hierarchy-node
  "Given a hierarchy node, a depth, and the combined info for all
  ancestors, return a flattened version in of its hierarchy in
  pre-order. Add :cumulative-info and :depth to the node and all its
  descendants."
  [node depth ancestor-info]
  (let [cumulative-info (multiset-union (:info node) ancestor-info)]
    (cons (-> node
              (assoc :depth depth)
              (assoc :cumulative-info cumulative-info))
          (flatten-hierarchy (:children node) (inc depth) cumulative-info))))

(defn flatten-hierarchy
  "Given a hierarchy and a depth, return a flattened version in pre-order,
  for each node of the hierarchy, also giving its depth."
  [hierarchy depth ancestor-info]
  (mapcat #(flatten-hierarchy-node % depth ancestor-info) hierarchy))

(defn hierarchy-node-descendants
  [node]
  (concat (:members node) (mapcat hierarchy-node-descendants (:children node))))

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
  is displayed under the condition, rather than under its parent."
  [element condition parent-item parent-key inherited]
  (println "condition-component for" condition)
  (assert (not (elements-referent? (first parent-key))))
  (expr-let [condition-specs (condition-specifiers element condition)]
    (let [key (->> parent-key
                   ;; We must make this key different from what it
                   ;; would have been if this element were displayed
                   ;; under its parent's component.
                   ;; Because it might have been in that situation.
                   (prepend-to-key (elements-referent condition))
                   (prepend-to-key (item-referent element)))]      
      (make-component {:key key
                       :sibling-elements (rest condition) 
                       :row-sibling parent-key}
                      [item-DOM element key (set condition-specs) inherited]))))

(defn empty-DOM
  "Generate dom for an empty editable cell. A map may be provided so
  that when content is added it will be put adjacent to the specified
  item in the specified direction."
  [parent-item parent-key sibling-elements inherited
   & {:keys [adjacent-item adjacent-direction]
      :or {adjacent-item nil adjacent-direction nil}}]
  (when adjacent-item
    (assert (#{:before :after} adjacent-direction)))
  (add-attributes (vertical-stack nil)
                  (cond-> {:class "column editable"
                           :key (prepend-to-key
                                 (elements-referent (cons nil sibling-elements))
                                 parent-key)}
                    adjacent-item
                    (assoc :add-adjacent (prepend-to-key
                                          (item-referent adjacent-item)
                                          parent-key)
                           :add-direction adjacent-direction))))

(defn components-DOM
  "Given a non-empty list of [item, excluded-elements] pairs,
  generate DOM for a vertical list of a component for each item."
  [items-with-excluded parent-key sibling-elements inherited]
  (assert (not (empty? items-with-excluded)))
  (let [item-doms (map (fn [[item excluded-elements]]
                           (let [key (prepend-to-key (item-referent item)
                                                     parent-key)]
                             (make-component
                              {:key key :sibling-elements sibling-elements}
                              [item-DOM
                               item key (set excluded-elements) inherited])))
                         items-with-excluded)]
      (add-attributes (vertical-stack item-doms :separators true)
                      {:class "column"})))

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
  (println "generating row-header dom.")
  (assert (not (elements-referent? (first row-key))))
  ;; This code works by wrapping in successively more divs, if
  ;; necessary, and adding the right attributes at each level.
  (expr-let [components
             (expr-seq map #(condition-component
                             % condition row-item row-key inherited)
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
  [hierarchy-node parent-item parent-key inherited]
  (let [descendants (hierarchy-node-descendants hierarchy-node)
        example (first descendants)
        appearance-info (select-keys hierarchy-node
                                     [:depth :for-multiple :with-children
                                      :top-border :bottom-border])
        example-elements (canonical-info-to-generating-items
                          (:info hierarchy-node)
                          (:info-elements example) (:info-canonicals example))
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
  [hierarchy-node parent-item parent-key inherited]
  (let [items-with-excluded (map #((juxt :item :info-elements) %)
                                 (:members hierarchy-node))
        sibling-elements (canonical-set-to-list
                          (:cumulative-info hierarchy-node))]
    (if (empty? items-with-excluded)
      (empty-DOM parent-item parent-key sibling-elements inherited
                 :adjacent-item (:item (first (hierarchy-node-descendants
                                               hierarchy-node)))
                 :adjacent-direction :before)
      (components-DOM items-with-excluded
                      parent-key sibling-elements inherited))))

(defn tag-items-pair-DOM
  "Given a flattened hierarchy node,
  generate DOM for an element table row for the items."
  ;; TODO: for deep items, use a layout with tag above content to conserve
  ;; horizontal space.
  [hierarchy-node parent-item parent-key inherited]
  (expr-let [tags-label-dom (tag-label-DOM
                             hierarchy-node parent-item parent-key inherited)
             tags-items-dom (tag-items-DOM
                             hierarchy-node parent-item parent-key inherited)]
    (into [:div {:style {:display "table-row"}}]
          (map #(add-attributes % {:style {:display "table-cell"}})
               [tags-label-dom tags-items-dom]))))

;;; TODO: make this use flex boxes, rather than a table,
;;;       to get the full heights.
(defn tagged-items-table-DOM
  "Return DOM for the given items, as a grid of tags and values."
  ;; We use a table as a way of making all the cells of a row
  ;; be the same height.
  [items parent-item parent-key inherited]
  (expr-let [hierarchy (items-hierarchy-by-condition
                        items (:do-not-merge inherited) '(nil tag))
             flattened-hierarchy (flatten-hierarchy-add-row-header-border-info
                                  hierarchy)
             row-doms (expr-seq
                       map #(cache tag-items-pair-DOM
                                   % parent-item parent-key inherited)
                       flattened-hierarchy)]
    (into [:div {:class "element-table"
                 :style {:height "1px" ;; So height:100% in rows will work.
                         :display "table" :table-layout "fixed"}}]
          (as-> row-doms row-doms
            (if (every? #(empty? (get-in % [:members 0 :info-elements]))
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
  [item key excluded inherited]
  (println "Generating DOM for" (simplify-for-print key))
  (expr-let [content (entity/content item)
             elements (visible-elements item)]
    (let [elements (remove excluded elements)
          inherited-down (update-in inherited [:depth] inc)
          content-dom
          (if (entity/atom? content)
            [:div (if (empty? elements)
                    {:class "item content-text editable"
                     :key key}
                    {:class "content-text editable"
                     :key  (prepend-to-key (content-referent) key)})
             (if (= content :none) "" (str content))]
            (let [child-key (prepend-to-key (item-referent content) key)]
              (make-component
               {:key child-key}
               [item-DOM child-key content #{} inherited-down])))]
      (if (empty? elements)
        content-dom
        (expr-let [elements-dom (tagged-items-table-DOM
                                 elements item key inherited-down)]
          (add-attributes (vertical-stack [content-dom elements-dom])
                          {:class "item with-elements" :key key}))))))

(defn table-header-elements-DOM
  "Generate the dom for one node of a table header hierarchy."
  [elements column-condition parent-item parent-key inherited]
  (println "generating header elements dom for " (count elements) " elements")
  (assert (not (elements-referent? (first parent-key))))
  (expr-let [components
             (expr-seq map
                       #(condition-component
                         % column-condition parent-item parent-key inherited)
                       elements)]
    (let [num-elements (count elements)
          elements-key (prepend-to-key
                        (elements-referent column-condition)
                        parent-key)]
      (as-> (vertical-stack components :separators true) dom
        (if (> num-elements 1)
          (add-attributes dom {:class "stack"})
          dom)
        (if (empty? elements)
          (add-attributes dom {:key elements-key})
          dom)
        [:div {:class "column_header_container"} dom]
        ;; TODO: add more appearance info.
        (add-attributes dom {:class "column tags column_header"})))))

(defn table-header-subtree-DOM
  "Generate the dom for a subtree of a table header hierarchy.
  The scope-referent should specify all the items from which
  this header selects elements."
  [node column-condition parent-key scope-referent inherited]
  (println "generating subtree dom" scope-referent)
  (let [{:keys [info members children]} node
        width (count (hierarchy-node-descendants node))
        descendants (hierarchy-node-descendants node)
        example (first descendants)
        example-elements (canonical-info-to-generating-items
                          info 
                          (:info-elements example) (:info-canonicals example))
        affected-header-items (map :item descendants)
        items-referent (parallel-referent
                        []
                        ;; TODO: When keys support it, make the header
                        ;; items trace the entire path, so it will
                        ;; work inside another parallel.
                        (vec (cons scope-referent affected-header-items)))
        node-dom (expr table-header-elements-DOM
                   (order-items example-elements) ; Why we need the expr
                   column-condition
                   (:item example)
                   (prepend-to-key items-referent parent-key)
                   inherited)]
    (if (empty? children)
      node-dom
      (let [inherited (update-in inherited [:level] inc)
            members-referent (parallel-referent
                              []
                              ;; TODO: When keys support it, make the header
                              ;; items trace the entire path, so it will
                              ;; work inside another parallel.
                              (vec (cons scope-referent (map :item members))))]
        [:div node-dom [:div {:class "column_header_sequence"}
                        (concat
                         (map #(table-header-elements-DOM
                                nil
                                column-condition
                                (:item example)
                                (prepend-to-key members-referent parent-key)
                                inherited)))
                         (map #(table-header-subtree-DOM
                                % parent-key scope-referent inherited)
                              children)]]))))

(defn table-header-DOM
  "Generate DOM for column headers for the specified templates.
  The column will contain those elements of the rows that match the templates."
  [column-templates column-condition key row-referent inherited]
  (println "generating table header dom")
  (let [inherited (assoc inherited :level 0)]
    ;; Unlike row headers for tags, where the header information is
    ;; computed from the items of the rows, here the header information
    ;; explicitly provided by the table definition, so the "items" for
    ;; the hierarchy are the column definitions.
    ;; TODO: Make this handle column definitions like x: 1, x: 2, x: 3
    ;; by adding a new step after hierarchy generation that combines
    ;; elements that have common tags but different values.
    (expr-let [hierarchy (items-hierarchy-by-condition
                          column-templates #{} '(nil))
               columns (expr-seq
                        map #(table-header-subtree-DOM
                              % column-condition key row-referent inherited)
                        hierarchy)]
      (println "hierrchy" hierarchy)
      (into [:div {:class "column_header_sequence"}]
            columns))))

(defn table-cell-DOM
  "Return the dom for one cell of a table. The condition must be in list form."
  [items condition row-item row-key inherited]
  (if (empty? items)
    ;; TODO: Get our left neighbor as an arg, and pass it in
    ;; as adjacent information.
    (empty-DOM row-item row-key (rest condition) inherited)
    (expr-let [excluded (expr-seq map #(condition-specifiers % condition)
                                  items)]
      (add-attributes
       (components-DOM (map vector items excluded)
                       row-key (rest condition) inherited)
       {:class "table_cell"}))))

(defn table-row-DOM
  "Generate the dom for one row of a table."
  [item column-conditions key inherited]
  ;; TODO: Need the column spec items too, so they can be added to the
  ;; cell keys to make cells for otherwise identical columns be different.
  (expr-let [cell-items (expr-seq map #(matching-elements % item)
                                     column-conditions)
             cells (expr-seq
                    map (fn [items condition]
                          (table-cell-DOM items condition item key inherited))
                    cell-items column-conditions)]
    (into [:div {:class "table_row"}] cells)))

(defn replace-nones
  "Replace any :none in the seq with nil"
  [x]
  (cond (sequential? x) (map replace-nones x)
        (= x :none) nil
        true x))

(defn table-DOM
  "Return a hiccup representation of DOM, with the given internal key,
  describing a table."
  ;; The following elements of item describe the table:
  ;;  :row-query  The content is an item whose list form gives the
  ;;              requirements for an item to appear as a row
  ;;     :column  The content is an item whose list form gives the
  ;;              requirements for an element of a row to appear in
  ;;              this column. The special contens :other means to show
  ;;              everything not shown in any other column.
  ;; TODO: Make there there be an element on a column descriptor that
  ;;       says how the column is described, rather than the current '(nil tag)
  ;; TODO: Add the "other" column to all tables.
  [item key inherited]
  (println "Generating DOM for table" (simplify-for-print key))
  (assert (satisfies? entity/StoredEntity item))
  (let [store (:store item)]
    (expr-let [row-query-item (entity/label->content item :row-query)]
      ;; Don't do anything if we don't yet have the table information filled in.
      (when row-query-item
        (expr-let [row-query (visible-elements row-query-item)
                   columns (expr order-items
                             (entity/label->elements item :column))
                   column-templates (expr-seq map entity/content columns) 
                   column-conditions (expr-seq map
                                               #(expr replace-nones
                                                  (expr visible-to-list %))
                                               column-templates)
                   row-items (matching-items row-query store)
                   headers (table-header-DOM column-templates '(nil tag)
                                             key (query-referent row-query-item)
                                             inherited)
                   rows (expr-seq map #(table-row-DOM
                                        % column-conditions key inherited)
                                  row-items)]
          (println "column conditions" (current-value (expr-seq map entity/to-list column-conditions)))
          (into [:div {:class "table"} headers] rows))))))

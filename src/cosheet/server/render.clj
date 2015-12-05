(ns cosheet.server.render
  (:require (cosheet [entity :as entity]
                     [utils :refer [multiset multiset-diff multiset-union
                                    update-last]]
                     [mutable-set :refer [mutable-set-intersection]]
                     [debug :refer [simplify-for-print]]
                     [orderable :as orderable]
                     [dom-utils
                      :refer [into-attributes dom-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq cache]])
            (cosheet.server [key :refer [item-referent content-referent
                                         elements-referent parallel-referent
                                         prepend-to-key elements-referent?
                                         visible-elements filtered-elements
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
      :content-sibling  ; A key that a content item for this empty dom
                        ; should be a sibling of. Used for getting
                        ; ordering information.
    :content-direction  ; Whether a content item for this empty dom
                        ; should come before or after :content-sibling
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
;;;                    spans the full height of its row. Its borders
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

(defn tag-specifier?
  "Return true if an element specifies that the item it pertains to is
   a tag. (Note that a tag specifier is considered visible, because its
   presence affects the user visible structure, even though it is not
   shown as its own cell."
  [element]
  (expr-let [content (entity/content element)]
    (= content 'tag)))

(defn tag-specifiers
  "Return the tag specifiers of an entity."
  [entity]
  (filtered-elements entity tag-specifier?))

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

;;; A hierarchy, for our purposes here, consists of vector of nodes. A
;;; node is a map with:
;;;           :info  A canonical-info-set of the new info at this level.
;;; :cumulatve-info  A canonical-info-set all info for this node.
;;;        :members  A vector of members matching the node.
;;;       :children  An optional vector of child nodes.
;;; The :info of a child node only includes the information not
;;; reflected in any of its parents. When a hierarchy is flattened,
;;; the cumulative information down through all parents is stored in
;;; :cumulative-info.
;;; For example, if a node has :info {:b 1}, and has a parent with
;;; :info {:a 1}, and has no other ancestors, the :cumulative-info is
;;; {:a 1 :b 1}
;;; As used here, the members are themselves a map, containing
;;;           :item The item that is the member
;;;      :tag-items The elements of the item that are tags
;;; :tag-canonicals The canonical-info for all the item in :tag-items
;;; The :tag-canonicals of each member of a flattened node will be the
;;; same as its :cumulative-info, but a node might not have any
;;; members, so we still need :cumulative-info.

(defn append-to-hierarchy
  "Given an info and corresponding item, add them to the hierarchy"
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
                               :children [{:info old-only
                                           :members (:members last)}]}))
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

(defn hierarchy-node-descendants
  [node]
  (concat (:members node) (mapcat hierarchy-node-descendants (:children node))))

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

(defn make-component
  "Make a component dom descriptor, with the given attributes and definition.
   The attributes must include a key."
  [attributes definition]
  (assert (map? attributes))
  (assert (:key attributes))
  [:component attributes  definition])

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

(defn tag-component
  "Return the component for a tag element."
  [element parent-item parent-key inherited]
  (assert (not (elements-referent? (first parent-key))))
  (expr-let [tag-specs (tag-specifiers element)]
    (let [key (->> parent-key
                   ;; We must make this key different from what it
                   ;; would have been if this element weren't a tag,
                   ;; and were located under its parent's component.
                   ;; Because it might have been in that situation.
                   (prepend-to-key (elements-referent parent-item [nil 'tag]))
                   (prepend-to-key (item-referent element)))]      
      (make-component {:key key
                       :sibling-elements ['tag] 
                       :row-sibling parent-key}
                      [item-DOM element key (set tag-specs) inherited]))))

(defn tags-DOM
  "Given information about the appearance of a tag hierarchy node,
  and a sequence of tag items in it, return components for each of them,
  wrapped as necessary to give the right appearance."
  [appearance-info tags parent-item parent-key inherited]
  (println "generating tags dom.")
  (assert (not (elements-referent? (first parent-key))))
  ;; This code works by wrapping in successively more divs, if
  ;; necessary, and adding the right attributes at each level.
  (expr-let [tag-components
             (expr-seq map #(tag-component % parent-item parent-key inherited)
                       tags)]
    (let [{:keys [top-border bottom-border for-multiple with-children depth]}
          appearance-info
          elements-key (prepend-to-key
                        (elements-referent parent-item [nil 'tag]) parent-key)]
      (as-> (vertical-stack tag-components :separators true) dom
        (if (> (count tags) 1)
          (add-attributes dom {:class "stack"})
          dom)
        (if (empty? tags)
          (add-attributes dom {:key elements-key})
          dom)
        (if (or for-multiple (> (count tags) 1))
          [:div dom [:div {:class "spacer"}]]
          dom)
        (add-attributes
         dom
         {:class (str "full-row"
                      (when (= top-border :indented) " top-border")
                      (when (= bottom-border :indented) " bottom-border")
                      (when (empty? tags) " editable")
                      (when with-children " with-children")
                      (when for-multiple " for-multiple"))})
        (let [depth (:depth appearance-info)]
          (if (> depth 0)
            [:div (add-attributes dom {:class (str "indent-" depth)})]
            dom))
        (add-attributes
         dom (cond-> {:class
                      (str "tags column"
                           (when (= top-border :full) " top-border")
                           (when (= bottom-border :full) " bottom-border")
                           (when (= bottom-border :corner) " ll-corner"))}
               (> (count tags) 1)
               (into {:key elements-key})
               (not= (count tags) 1)
               (into {:row-sibling parent-key})))))))

(defn components-DOM
  "Given a list of [item, excluded-elements] pairs, and the first item
  of this or subsequent rows,
  generate DOM for a vertical list of a component for each item."
  [items-with-excluded first-affected-item parent-item parent-key
   sibling-elements inherited]
  (if (empty? items-with-excluded)
    (add-attributes (vertical-stack nil :separators true)
                    {:class "column editable"
                     :key (prepend-to-key
                           (elements-referent
                            parent-item (cons nil sibling-elements))
                           parent-key)
                     :add-sibling (prepend-to-key
                                   (item-referent first-affected-item)
                                   parent-key)
                     :add-direction :before})
    (let [item-doms (map (fn [[item excluded-elements]]
                           (let [key (prepend-to-key (item-referent item)
                                                     parent-key)]
                             (make-component
                              {:key key :sibling-elements sibling-elements}
                              [item-DOM
                               item key (set excluded-elements) inherited])))
                         items-with-excluded)]
      (add-attributes (vertical-stack item-doms :separators true)
                      {:class "column"}))))

(defn tag-items-pair-DOM
  "Given information about the appearance of this node,
  a list of tag items, a list of pairs of (item, excluded elements),
  a list of elements (each in list form) that all items must satisfy,
  and a list of all items that changes to the tags should apply to, in
  the order they appear,
  generate DOM for an element table row for the items."
  ;; TODO: for deep items, use a layout with tag above content to conserve
  ;; horizontal space.
  [[appearance-info
    tag-items items-with-excluded sibling-elements affected-items]
   parent-item parent-key inherited]
  (expr-let [tags-dom (let [items-referent
                            (if (= (count affected-items) 1)
                              (item-referent (first affected-items))
                              (parallel-referent [] affected-items))]
                        (expr tags-DOM appearance-info (order-items tag-items)
                              (first affected-items)
                              (prepend-to-key items-referent parent-key)
                              inherited))
             items-dom (components-DOM items-with-excluded
                                       (first affected-items)
                                       parent-item parent-key
                                       sibling-elements inherited)]
    [:div {:style {:display "table-row"}}
     (add-attributes tags-dom
                     {:style {:display "table-cell"}})
     (add-attributes items-dom {:style {:display "table-cell"}})]))

(defn items-for-info
  "Given a canonical-info-set, a list of items, 
  and a list of the canonical-infos for those items,
  return a list of items matching the elements of the set."
  [info items canonicals]
  (let [;; A map from canonical-info to a vector of tag elements
        ;; with that info.
        canonicals-map (reduce (fn [map [tag canonical]]
                                 (update-in map [canonical] #(conj % tag)))
                               {} (map vector items canonicals))]
    (reduce (fn [result [info count]]
              (concat result (take count (canonicals-map info))))
            [] info)))

(defn hierarchy-node-to-row-info
  "Given a flattened hierarcy element, compute the information needed
  by tag-items-pair-DOM."
  [node]
  (let [descendants (hierarchy-node-descendants node)
        example (first descendants)
        tag-items (items-for-info
                   (:info node) (:tag-items example) (:tag-canonicals example))
        items-with-excluded (map #((juxt :item :tag-items) %) (:members node))
        descendant-items (map :item descendants)
        appearance-info (select-keys node [:depth :for-multiple :with-children
                                           :top-border :bottom-border])]
    (let [sibling-elements (canonical-set-to-list (:cumulative-info node))]
      [appearance-info tag-items items-with-excluded
       sibling-elements descendant-items])))

(defn add-border-info
  "Given the flattened hierarchy expansion of one top level node,
  add the information about what borders each node in the expansion
  should be responsible for."
  ;; Hierarchies make this a bit tricky. We use a separate table row
  ;; for each node of the hierarchy, so we can align the tags and
  ;; items for each node. This means that all but the deepest nodes of
  ;; a hierarchy will be displayed as several rows. A row thus needs
  ;; to be responsible not only for borders of its node, but also for
  ;; parts of the borders of nodes it is contained in. In practice,
  ;; this means that a row needs to handle the left border of the
  ;; outermost node it is in. By making nodes always resposible for
  ;; their own top border, we get the border lengths between nodes
  ;; right. Thus, when laying out a row, we need to know:
  ;;            :depth in the hierarchy, for the indendation
  ;;       :top-border :full or :indented
  ;;    :bottom-border :corner, for only the lower left corner
  ;;     :for-multiple true if this row applies to several items.
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

(defn tagged-items-hierarchy
  "Given items, organize them into a flattened hierarchy by tag"
  [items do-not-merge]
  (expr-let [do-not-merge-subset (mutable-set-intersection do-not-merge items)
             item-maps
             (expr-seq map
                       (fn [item]
                         (expr-let [tag-items (entity/label->elements item 'tag)
                                    tag-canonicals (expr-seq map canonical-info
                                                             tag-items)]
                           {:item item
                            :tag-items tag-items
                            :tag-canonicals tag-canonicals}))
                       (order-items items))]
    (let [hierarchy (hierarchy-by-canonical-info
                     (map (fn [map] [(multiset (:tag-canonicals map)) map])
                          item-maps)
                     do-not-merge-subset)]
      (update-last 
       (vec (mapcat #(add-border-info (flatten-hierarchy-node % 0 {}))
                    hierarchy))
       ;; We need to put on a final closing border.
       #(assoc % :bottom-border :full)))))

(defn tagged-items-DOM
  "Return DOM for the given items, as a grid of tags and values."
  ;; We use a table as a way of making all the cells of a row
  ;; be the same height.
  [items parent-item parent-key inherited]
  (expr-let [hierarchy (tagged-items-hierarchy items (:do-not-merge inherited))
             hierarchy-info (expr-seq map hierarchy-node-to-row-info hierarchy)
             row-doms (expr-seq
                       map #(cache tag-items-pair-DOM
                                   % parent-item parent-key inherited)
                       hierarchy-info)]
    (into [:div {:class "element-table"
                 :style {:height "1px" ;; So height:100% in rows will work.
                         :display "table" :table-layout "fixed"}}]
          (as-> row-doms row-doms
            (if (every? #(empty? (get-in % [:members 0 :tag-items])) hierarchy)
              (map #(add-attributes % {:class "no-tags"}) row-doms)
              row-doms)
            (update-in (vec row-doms) [(- (count row-doms) 1)]
                       #(add-attributes % {:class "last-row"}))))))

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
        (expr-let [elements-dom (tagged-items-DOM
                                 elements item key inherited-down)]
          (add-attributes (vertical-stack [content-dom elements-dom])
                          {:class "item with-elements" :key key}))))))

(defn table-DOM
  "Return a hiccup representation of DOM, with the given internal key,
  describing a table."
  ;; The following elements of item describe the table:
  ;;  :row-query  The content is an item whose list form gives the
  ;;              requirements for an item to appear as a row
  ;;     :column  The content is an item whose list form gives the
  ;;              requirements for an element of a row to appear in
  ;;              this column.
  [item key inherited]
  (println "Generating DOM for table" (simplify-for-print key))
  (expr-let [row-query-item (entity/label->content item :row-query)
             columns (entity/label->elements item :column)
             row-query (visible-elements row-query-item)
             column-elements (expr-seq
                              (map #(visible-elements (entity/content %))
                                   columns))]))

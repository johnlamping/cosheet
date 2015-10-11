(ns cosheet.server.render
  (:require (cosheet [entity :as entity]
                     [utils :refer [multiset]]
                     [debug :refer [simplify-for-print]]
                     [orderable :as orderable]
                     [dom-utils
                      :refer [into-attributes dom-attributes add-attributes]]
                     [reporters
                      :refer [value expr expr-let expr-seq cache]])))

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
;;; Those server specific attributes, which include
;;;                :key  A unique client side key further described below.
;;;           :ordering  Optional ordering information, described below.
;;;   :sibling-elements  Elements that a sibling of this item must have.
;;;        :row-sibling  A key that a new row should be a sibling of.
;;;                      If the key indicates multiple items, the row
;;;                      is a sibling of the last, or, if there are
;;;                      multiples groups of items, the last of each
;;;                      group.
;;;       :row-elements  Elements that a new row must have.
;;; There are removed by the dom manager before dom is sent to the client.

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
;;;   [:component {:key <key>
;;;                <other attributes to add to the definition's
;;;                 result>}
;;;               definition}
;;; The dom_tracker code understands these components. It will give
;;; the client a dom with these subsidiary components, all with the
;;; provided attributes already present, and it will create additional
;;; computations to compute the dom for the components, and pass them
;;; as updates to the client once they are computed.

;;; A key is used both for components and for any other dom node that
;;; the user might interact with. Every key must be unique, even if
;;; two dom nodes describe the same item. This is handled by having
;;; keys reflect the path of containment in the dom. The key of a
;;; component must not change unless it's parent dom changes, or the
;;; connection between parent and child component will be lost, and
;;; messages between client and server not understood.

;;; All keys are sequences. Their first element is a referent that
;;; gives the information needed by action interpretation, while their
;;; subsequent elements are referents for parent pieces of dom, not
;;; necessarily for every parent, but for enough to make the key
;;; unique among all doms, and sometimes to give any additional
;;; information necessary to interpret the referent key.

;;; There are several kinds of referents
;;;        item: <an item id>
;;;     content: [:content]
;;;       group: [:group <An item id of the group, typically the first>]
;;;   condition: [:condition @<list of elements, each in list form,
;;;                           that an item must have>]
;;;    parallel: [:parallel [<list of referents>] [<list of item ids>]]

;;; An item referent indicates a dom node that describes a particular
;;; item. Typically, a dom that refers to an item will additionally
;;; have a :sibling-elements attribute giving a list of elements that
;;; a sibling item must have, each in list form. This condition
;;; attribute is not the same as a condition referent. Incorporating
;;; the condition into the item referent would be a mistake, as the
;;; referent could then change even though the identity of the item
;;; hadn't.

;;; A content referent indicates a subnode of an item node that holds
;;; its atomic content. Since atomic content nodes don't have
;;; subnodes, a content referent will always be the first referent of
;;; its key. The next referent of the key will be the item node.

;;; A group referent indicates a dom node that holds several items,
;;; the first of which is the given item. It's prototypical use is
;;; when several items from a list are grouped into one dom for
;;; display purposes.

;;; A condition referent also indicates a dom node that holds several
;;; items, but one where the items are defined to be all the ones
;;; whose subject is the previous item in the key, and that satisfy a
;;; particular condition. Currently, the condition is just a sequence
;;; of elements, in list form. Condition referents are used for tag
;;; nodes, since they show all tags of the subject, and for cells of a
;;; table, if the table columns are conditions. In contrast to a group
;;; node, a condition node might be empty, or two sibling condition
;;; nodes might both show the same item.

;;; Typically a dom for a group referent or a condition referent will
;;; additionally have a :ordering attribute that gives information
;;; about the first and last item in the group, or about the items
;;; just before or after it. If there are any elements in the node,
;;; then first-item and last-item will be listed, so that ordering
;;; information can be inferred for items added at the beginning and
;;; end of the node. If the node is empty, then before-item and
;;; after-item may be listed, giving items that an item added to the
;;; node should come after or before.

;;; A parallel referent stands for a set of keys. Its prototypical use
;;; is when the tags of several items would be displayed identically,
;;; and the display of the tags is collapsed into a single dom node. A
;;; change to that node should change tags for each of the items. The
;;; parallel referent's job is to indicate the set of items
;;; corresponding to those parallel tags. A key can have at most one
;;; parallel referent, while will be the first referent of the key.
;;; But as described below, a parallel referent can itself have
;;; another key, which can contain another parallel referent.

;;; A parallel referent consists of an exemplar key, and a list of
;;; items. In the simplest case, the exemplar key is empty, in which
;;; case the parallel reference refers to each of its items. A
;;; non-empty exemplar describes a navigation path to be traced
;;; through each of the items. Starting with each item, the navigation
;;; finds an element whose visible information matches the visible
;;; information of the last item in the exemplar. That becomes the new
;;; item in the navigation, which continues moving foward in the
;;; exemplar. If the first referent of the exemplar is an item, then
;;; the parallel referent refers to what that item ended up matching
;;; at the end of each of the navigations. If the first referent is
;;; another exemplar, each item of the nested exemplar is matched, and
;;; the navigation recurses.

(defn item-referent
  "Create an item referent"
  [item]
  (assert entity/mutable-entity? item)
  (:item-id item))

(defn condition-referent
  "Create a condition referent"
  [elements]
  (into [:condition] elements))

(defn parallel-referent
  "Create a parallel referent"
  [exemplar items]
  (doseq [item items] (assert entity/mutable-entity? item))
  [:parallel exemplar (map :item-id items)])

(defn prepend-to-key
  "Prepend a new referent to the front of a key, maintaining the invariant
  that a parallel referent can only occur in first position."
  [referent key]
  (let [initial (first key)]
    (vec
     (if (and (sequential? initial)
              (= :parallel (first initial)))
       (let [[_ exemplar item-ids] initial]
         (cons [:parallel (prepend-to-key referent exemplar) item-ids]
               (rest key)))
       (cons referent key)))))

(defn visible-entity?
  "Return true if an entity is visible to the user
  (Doesn't have a keyword element.)"
  [entity]
  (expr-let [elements (entity/elements entity)
             element-contents (expr-seq map entity/content elements)]
    (not-any? keyword? element-contents)))

(defn tag-specifier?
  "Return true if an element specifies that the item it pertains to is
   a tag. (Note that a tag specifier is considered visible, because its
   presence affects the user visible structure, even though it is not
   shown as its own cell."
  [element]
  (expr-let [content (entity/content element)]
    (= content 'tag)))

(defn filtered-elements
  "Return a seq of all elements of the entity that satisfy the condition."
  [entity condition]
  (expr-let [elements (entity/elements entity)
             passed (expr-seq map #(expr-let [passes (condition %)]
                                        (when passes %))
                              elements)]
    (filter identity passed)))

(defn visible-elements
  "Return the elements of an entity that are visible to the user."
  [entity]
  (filtered-elements entity visible-entity?))

(defn tag-specifiers
  "Return the tag specifiers of an entity."
  [entity]
  (filtered-elements entity tag-specifier?))

(defn visible-to-list
  "Given an entity, make a list representation of the visible information
  of the item."
  [entity]
  (if (entity/atom? entity)
    (entity/content entity)
    (expr-let [content (entity/content entity)
               elements (visible-elements entity)
               content-visible (visible-to-list content)
               element-visibles (expr-seq map visible-to-list elements)]
      (if (empty? element-visibles)
        content-visible
        (list* (into [content-visible] element-visibles))))))

(defn canonicalize-list
  "Given the list form of an entity, return a canonical representation of it."
  ;; We record the elements as a map from element to multiplicities,
  ;; so that we are not sensitive to the order of the elements.
  ;; That is easier than sorting, because Clojure doesn't define
  ;; a sort order between heterogenous types, like strings and ints.
  [entity]
  (if (sequential? entity)
    [(canonicalize-list (first entity))
     (multiset (map canonicalize-list (rest entity)))]
    entity))

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

(defn group-by-tag
  "Given a sequence of items, group consecutive identically tagged,
   items together. Return a list of the groups, where each group is
   described as a list of [item, tags] pairs."
  [items]
  (expr-let [tag-lists (expr-seq
                        map #(entity/label->elements % 'tag) items)
             canonical-tags (expr-seq map canonical-info-set tag-lists)]
    (first (reduce (fn [[groups prev-item-canonical-tags]
                        [item item-tags item-canonical-tags]]
                     [(if (and (not (empty? item-canonical-tags))
                               (= item-canonical-tags prev-item-canonical-tags))
                        (update-in groups [(dec (count groups))]
                                   #(conj % [item item-tags]))
                        (conj groups [[item item-tags]]))
                      item-canonical-tags])
                   [[] ::no-match]
                   (map vector items tag-lists canonical-tags)))))

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
  [element parent-key inherited]
  (assert (not= (first parent-key) (condition-referent ['tag])))
  (expr-let [tag-specs (tag-specifiers element)]
    (let [key (->> parent-key
                   ;; We must make this key different from what it
                   ;; would have been if this element weren't a tag,
                   ;; and were located under its parent's component.
                   ;; Because it might have been in that situation.
                   (prepend-to-key (condition-referent ['tag]))
                   (prepend-to-key (item-referent element)))]      
      (make-component {:key key
                       :sibling-elements ['tag] 
                       :row-sibling parent-key}
                      [item-DOM element key (set tag-specs) inherited]))))

(defn tags-DOM
  "Given a sequence of items that are tags, return components for them,
  wrapped in a div if there is more than one."
  [tags parent-key inherited]
  (println "generating tags dom.")
  (assert (not= (first parent-key) (condition-referent ['tag])))
  (expr-let [tag-components
             (expr-seq
              map #(tag-component % parent-key inherited) tags)]
    (add-attributes
     (vertical-stack tag-components :separators true)
     (cond-> {:class (str "tag-column" (when (empty? tags) " editable"))}
       (not= (count tags) 1)
       (into {:key (prepend-to-key (condition-referent ['tag])
                                   parent-key)
              :row-sibling parent-key})))))

(defn tag-items-pair-DOM
  "Given a list, each element of the form [item, [tag ... tag], where
   the tags for each item are equivalent, generate DOM for the items,
   sharing a label with the tags."
  ;; TODO: for deep items, use a layout with tag above content to conserve
  ;; horizontal space.
  [items-and-tags parent parent-key inherited]
  (println "generating tag-item")
  (let [sample-tags (get-in items-and-tags [0 1])]
    (expr-let [sibling-elements (expr-seq map visible-to-list sample-tags)
               items (map first items-and-tags)
               item-doms (expr-seq
                          map (fn [[item tag-list]]
                                (let [key (prepend-to-key (item-referent item)
                                                          parent-key)]
                                  (make-component
                                   {:key key :sibling-elements sibling-elements}
                                   [item-DOM
                                    item key (set tag-list) inherited])))
                          items-and-tags)
               tags-dom (let [parent-key
                              (if (= (count items) 1)
                                (prepend-to-key
                                 (item-referent (first items)) parent-key)
                                (prepend-to-key
                                 (parallel-referent [] items) parent-key))]
                          (expr tags-DOM
                            (order-items sample-tags) parent-key inherited))]
      [:div {:style {:display "table-row"}}
       (add-attributes tags-dom
                       {:style {:display "table-cell"}
                        :class (when (> (count item-doms) 1)
                                 "for-multiple-items")})
       (add-attributes (vertical-stack item-doms :separators true)
                       {:style {:display "table-cell"}
                        :class "item-column"})])))

(defn tagged-items-DOM
  "Return DOM for the given items, as a grid of tags and values."
  ;; We use a table as a way of making all the cells of a row the same height.
  [items parent parent-key inherited]
  (expr-let [rows (expr group-by-tag (order-items items))
             row-doms (expr-seq
                       map #(cache tag-items-pair-DOM % parent parent-key inherited)
                       rows)]
    (into [:div {:class "element-table"
                 :style {:display "table" :table-layout "fixed"}}]
          (as-> row-doms row-doms
            (if (every? #(empty? (get-in % [0 1])) rows)
              (map #(add-attributes % {:class "no-tags"}) row-doms)
              row-doms)
            (update-in (vec row-doms) [(- (count rows) 1)]
                       #(add-attributes % {:class "last-row"}))))))

(defn item-DOM
  "Return a hiccup representation of DOM, with the given internal key,
   describing an item and all its elements, except the ones in
   excluded. Where appropriate, inherit properties from the map of
   inherited properties."
  [item key excluded inherited]
  ;;; TODO: for deep items, use a layout with tag above content to conserve
  ;;; horizontal space.
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
                     :key  (prepend-to-key [:content] key)})
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

(ns cosheet.server.render
  (:require (cosheet [entity :as entity]
                     [dom-utils
                      :refer [into-attributes dom-attributes add-attributes]]
                     [reporters
                      :refer [value expr expr-let expr-seq]])))

;;; TODO: Don't have item-dom add the key to the item; have
;;; dom-tracker do it instead.

;;; Code to create hiccup style dom for a database entity.
;;; Sub-components of the dom are specified as
;;;   [:component {:key sibling-key
;;;                :definition definition
;;;                :attributes <attributes to add the definition's result>}]
;;; This is what is expected by dom_tracker.

;;; For a basic entity, we show its contents and its user visible
;;; elements, but not its non-user visible elements. The latter are
;;; identified by, themselves, having elements with keyword contents.
;;; in addition, an entity may be marked as a tag, by having
;;; an element whose content is 'tag. The 'tag mark is considered
;;; visible, because it changes the visible appearance of its element,
;;; even though it is not shown in the usual way.
;;; So, for example, the entity:
;;;    ("Joe"
;;;        "married"
;;;        (39
;;;           ("age" 'tag)
;;;           "doubtful"))
;;; would look like:
;;;   Joe
;;;     married
;;;     age: 39
;;;            doubtful

;;; The style attributes are represented with their own map, rather
;;; than as a string, so they are easier to adjust. Conviently,
;;; reagent accepts that format too.

;;; The key is both by dom-tracker to keep track of components as they
;;; change, so every component must have a unique key, even if two
;;; components reeflect the same item. The key is also used by actions
;;; to identify what part of the data the user is operating on, so
;;; there can also be keys for other dom elements that the user can
;;; interact with, such as targets for the user to add new data.
;;; Tere keys for different kinds of dom look like this:
;;;             an item: (cons <item> <parent id>)
;;;    a potential item: (cons [:virtual <template>] <parent id>)
;;;      a generic item: (cons (cons :generic <query> <generic id>)
;;;                            <parent id>)
;;; Here, a potential item is one that is not yet created, but that
;;; when created will have the given parent and satisfy the template.

;;; A generic item stands for several items, typically because it is a
;;; tag dom that covers several items with the same tag. The parent id
;;; gives a representative item that contains the common structure,
;;; while the query extracts that structure from the parent item. The
;;; generic pertains to that common structure of all items adjacent to
;;; the representative item in the ordering with the same visible
;;; results for the query. Within each instance of common structure,
;;; the specific item matched is the one that has the same visible
;;; structure and visible nesting as the generic id has in the
;;; representative item.

(defn multiset-conj
  "Add an item to a multiset,
   represented as a map from items to multiplicities."
  [ms item]
  (update-in ms [item] #((fnil + 0) % 1)))

(defn multiset
  "Turn a seq into a multiset."
  [items]
  (reduce multiset-conj {} items))

(defn visible-entity?
  "Return true if an entity is visible to the user
  (Doesn't have a keyword element.)"
  [entity]
  (expr-let [elements (entity/elements entity)
             element-contents (expr-seq map entity/content elements)]
    (not-any? keyword? element-contents)))

(defn tag-specifier?
  "Return true if an element is a specifier that the item pertains to is a tag.
   (Note that a tag specifier is considered visible, but its presence affects
   the user visible structure, even though it is not shown as its own cell."
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

(defn canonical-info
  "Given an item, return a canonical representation of its
   information, ignoring all information not directly visible
  to the user."
  ;; We record the elements as a map from element to multiplicities,
  ;; so that we are not sensitive to the order of the elements.
  ;; That is easier than sorting, because Clojure doesn't define
  ;; a sort order between heterogenous types, like strings and ints.
  [entity]
  (if (entity/atom? entity)
    (entity/content entity)
    (expr-let [content (entity/content entity)
               elements (visible-elements entity)]
      (expr-let [content-info (canonical-info content)
                 element-infos (expr-seq map canonical-info elements)]
        (if (empty? element-infos)
          content-info
          [content-info (multiset element-infos)])))))

(defn canonical-info-set
  "Given a seq of items, return a canonical representation of the items,
   treated as a multi-set."
  [entities]
  (expr-let [canonicals (expr-seq map canonical-info entities)]
    (multiset canonicals)))

(defn order-items
  "Return the items in the proper sort order."
  [items]
  (expr-let [order-info
             (expr-seq map #(entity/label->content % :order) items)]
    (map second (sort (map vector order-info items)))))

(defn stack-vertical
  "Make a dom stack vertically with its siblings."
  [dom]
  (let [style (:style (dom-attributes dom))
        display (:display style)
        width (:width style)]
    (add-attributes (if width dom (add-attributes dom {:style {:width "100%"}}))
                    {:style {:display
                             (case display
                               (nil "block" "inline-block") "block"
                               ("table" "inline-table") "table")}})))

(defn vertical-separated
  "Make a dom have separators between its siblings."
  [dom]
  (add-attributes dom {:class "vertical-separated"}))

(def item-DOM)

(defn child-item-key
  "Return the key for a child item, given a key of its parent."
  [child-item parent-key]
  (assert (not (nil? child-item)))
  (if (or (nil? parent-key) (empty? parent-key))
    [child-item]
    (cons child-item parent-key)))

(defn make-component
  "Make a component dom descriptor, with the given key and definition,
   and, optionally, additional attributes."
  ([key definition]
   [:component {:key key
                :definition definition}])
  ([key definition attributes]
   (assert (map? attributes))
   [:component {:key key
                :definition definition
                :attributes attributes}]))

(defn group-by-tag
  "Given a sequence of items, group consequtively, identically tagged,
   items together. Return a list of the groups, where each group is
   described as a list of [item, tags] pairs."
  [items]
  (expr-let [tag-lists (expr-seq
                        map #(entity/label->elements % 'tag) items)
             canonical-tags (expr-seq map canonical-info-set tag-lists)]
    (first (reduce (fn [[groups prev-item-canonical-tags]
                        [item item-tags item-canonical-tags]]
                     [(if (= item-canonical-tags prev-item-canonical-tags)
                        (update-in groups [(dec (count groups))]
                                   #(conj % [item item-tags]))
                        (conj groups [[item item-tags]]))
                      item-canonical-tags])
                   [[] ::none]
                   (map vector items tag-lists canonical-tags)))))

(defn vertical-stack
  "If there is only one item in the doms, return it. Otherwise, return
  a vertical stack of the items. Add a separator between items if specified."
  ([doms] (vertical-stack doms false))
  ([doms add-separator]
   (case (count doms)
     0 [:div]
     1 (first doms)
     (into [:div]
           (map (if add-separator
                  (comp vertical-separated stack-vertical)
                  stack-vertical)
                doms)))))

(defn tag-component
  "Return the component for a tag element."
  ;; TODO: Make this use a generic key, if it is generic.
  [element parent-key inherited]
  (expr-let [tag-specs (tag-specifiers element)]
    (let [key (child-item-key element parent-key)]
      (make-component
       key [item-DOM element key (set tag-specs) inherited]))))

(defn tags-DOM
  "Given a sequence of tags, return components for the given items, wrapped in
   a div if there is more than one."
  [tags parent-key inherited]
  (expr-let [tag-components
             (expr-seq map #(tag-component % parent-key inherited) tags)]
    (add-attributes (vertical-stack tag-components true) {:class "tag"})))

(defn tag-items-pair-DOM
  "Given a list of items and the tags for each, where the tags for each item
  are equivalent, generate DOM for the items, labeled with the tags."
  ;;; TODO: Add some data-* attributes to the tag items that indicate
  ;;; what items they pertain to.
  ;;; TODO: pass down, via inherited, how deeply nested the item is,
  ;;; and for deep items, use a layout with tag above content to conserve
  ;;; horizontal space.
  [items-and-tags parent-key inherited]
  (expr-let [item-doms (expr-seq
                        map (fn [[item tag-list]]
                              (let [key (child-item-key
                                         item parent-key)]
                                (make-component key
                                                [item-DOM item key
                                                 (set tag-list) inherited])))
                        items-and-tags)
             tags-dom (expr tags-DOM
                        (order-items (get-in items-and-tags [0 1]))
                        parent-key inherited)]
    [:div {:style {:display "table-row"}}
     (add-attributes tags-dom
                     {:style {:display "table-cell"}
                      :class (if (> (count item-doms) 1)
                               "tag-column for-multiple-items"
                               "tag-column")})
     (add-attributes (vertical-stack item-doms true)
                     {:style {:display "table-cell"}
                      :class "item-column"})]))

(defn tagged-items-DOM
  "Return DOM for the given items, as a grid of tags and values."
  ;; We use a table as a way of making all the cells of a row the same height.
  [items parent-key inherited]
  (expr-let [rows (expr group-by-tag (order-items items))
             row-doms (expr-seq
                       map #(tag-items-pair-DOM % parent-key inherited) rows)]
    (into [:div {:class "element-table"
                 :style {:display "table" :table-layout "fixed"}}]
          (if (and (= (count rows) 1)
                   (= (count (get-in rows [0 0 1])) 0))
            [(add-attributes (first row-doms) {:class "no-tags last-row"})]
            (concat (butlast row-doms)
                    [(add-attributes (last row-doms) {:class "last-row"})])))))

(defn item-DOM
  "Return a hiccup representation of DOM, with the given internal key,
   describing an item and all its elements, except the ones in
   excluded. Where appropriate, inherit properties from the map of
   inherited properties."
  [item key excluded inherited]
  ;;; TODO: pass down, via inherited, how deeply nested the item is,
  ;;; and for deep items, use a layout with tag above content to conserve
  ;;; horizontal space.
  (expr-let [content (entity/content item)
             elements (visible-elements item)]
    (let [elements (remove excluded elements)
          inherited-down (update-in inherited [:depth] inc)
          content-dom
          (if (entity/atom? content)
            [:div {:class "content-text"}
             (if (= content :none) "" (str content))]
            (let [child-key (child-item-key content key)]
              (make-component
               child-key [item-DOM child-key content #{} inherited-down])))]
      (if (empty? elements)
        (add-attributes content-dom {:class "item" :key key})
        (expr-let [elements-dom (tagged-items-DOM elements key inherited-down)]
          (add-attributes (vertical-stack [content-dom elements-dom])
                          {:class "item" :key key}))))))

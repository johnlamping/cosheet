(ns cosheet.server.render
  (:require (cosheet [entity :as entity]
                     [utils :refer [multiset]]
                     [orderable :as orderable]
                     [dom-utils
                      :refer [into-attributes dom-attributes add-attributes]]
                     [reporters
                      :refer [value expr expr-let expr-seq]])))

;;; Code to create hiccup style dom for a database entity.
;;; Sub-components of the dom are specified as
;;;   [:component {:key <key>
;;;                :definition <definition>
;;;                :attributes <attributes to add the definition's result>}]
;;; This is what is expected by dom_tracker.

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

;;; The key is used by dom-tracker to track components, even as they
;;; change. There are several reasons why a key can't just be an item.
;;; First, every component must have a unique key, even if two
;;; components reflect the same item. This is handled by having keys
;;; reflect the path of containment in the dom.

;;; Further, the key is also used to provide actions with the
;;; information to interpret them, in particular, to identify what
;;; part of the data the user is referring to. This isn't always just
;;; an item. Further, an addition action requires a condition, to know
;;; what elements the added item must have.

;;; All keys are sequences. Their first element is a referent that
;;; gives the information needed by action interpretation, while their
;;; subsequent elements are referents for parent pieces of dom, not
;;; necessarily for every parent, but for enough to make the key
;;; unique among all doms, and to give any additional information
;;; necessary to interpret the referent key.

;;; There are several kinds of referents, each a map whose keys depend
;;; on what the dom cell refers to. The first of the keys of each of
;;; the following maps are required, while the others are optional.
;;;       item: {:item <item>
;;;              :condition <condition>}
;;;   elements: {:condition <condition>
;;;              :after-item <item>
;;;              :first-item <item>
;;;              :last-item <item>
;;;              :before-item <item>}
;;;        set: {:items <list of items>
;;;              :exemplar <exemplar key>}

;;; In these maps, a condition is a query that an item must satisfy.
;;; It is a map, which can contain
;;;   {:subject <subject of item> (or a vector of subjects)
;;;    :elements <list of elements that an item must have>}

;;; An item referent indicates a dom node that holds a particular
;;; item. It typically also gives a condition required for a new item
;;; to appear adjacent in the dom to the item in question. That way,
;;; insert actions can be interpreted.

;;; An elements referent indicates a dom node that displays all the
;;; elements of some item that satisfy a condition. There could be
;;; none, one, or several such elements. If there are any elements in
;;; the node, then first-item and last-item will be listed, so that
;;; ordering information can be inferred for items added at the
;;; beginning and end of the node. If the node is empty, then
;;; before-item and after-item may be listed, giving items that an
;;; item added to the node should come after or before.

;;; A set referent stands for a set of items. Its canonical use is
;;; when the tags of several items look the same, and the display
;;; collapses the display of the tags into a single dom node. A change
;;; to that node should change tags for each of the items; The set
;;; referent's job is to indicate the set of items corresponding to
;;; those parallel tags.

;;; A set referent consists of a list of items and possibly an
;;; exemplar. In the simplest case, the exemplar key is empty, in
;;; which case, the set reference refers to each of its items. If the
;;; set's exemplar is non-empty, then the exemplar describes a
;;; navigation path through each of the items. Starting with each
;;; item, the navigation finds an element whose visible information
;;; matches the visible information of the last item in the exemplar.
;;; That becomes the new item in the navigation, which continues
;;; moving foward in the exemplar. If the first referent of the
;;; exemplar is an item, then the set refers to what that item ended
;;; up matching at the end of each of the navigations. If the first
;;; referent is another exemplar, each item of the nested exemplar is
;;; matched, and the navigation recurses.

(defn item-referent
  "Validate keyword arguments for an item referent"
  [& {:as args}]
  (assert (:item args))
  (assert (every? #{:item :condition} (keys args)))
  args)

(defn elements-referent
  "Validate keyword arguments for a elements referent"
  [& {:as args}]
  (assert (:condition args))
  (assert (every? #{:condition :first-item :last-item :before-item :after-item}
                  (keys args)))
  args)

(defn set-referent
  "Validate keyword arguments for a set referent"
  [& {:as args}]
  (assert (:items args))
  (assert (every? #{:items :exemplar} (keys args)))
  args)

(defn condition-map
  "Validate keyword arguments for a condition."
  [& {:as args}]
  (assert (every? #{:subject :elements}(keys args)))
  args)

(defn prepend-to-key
  "Prepend a new referent to the front of a key, maintaining the invariant
  that a set referent can only occur in first position."
  [referent key]
  (let [initial (first key)]
    (if (:items initial)
      (cons (set-referent
             :items (:items initial)
             :exemplar (prepend-to-key referent (:exemplar initial)))
            (rest key))
      (cons referent key))))

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
               elements (visible-elements entity)]
      (expr-let [content-visible (visible-to-list content)
                 element-visibles (expr-seq map visible-to-list elements)]
        (if (empty? element-visibles)
          content-visible
          (into [content-visible] element-visibles))))))

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

(defn elements-condition
  "Given a subject and a set of elements, return the condition
   of having that subject and having elements with matching visible
   information."
  [subject elements]
  (expr-let [visible-elements (expr-seq map visible-to-list elements)]
    (condition-map :subject subject :elements visible-elements)))

(defn orderable-comparator
  "Compare two sequences each of whose first element is an orderable."
  [a b]
  (orderable/earlier? (first a) (first b)))

(defn order-items
  "Return the items in the proper sort order."
  [items]
  (expr-let [order-info
             (expr-seq map #(entity/label->content % :order) items)]
    (map second (sort orderable-comparator (map vector order-info items)))))

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
                     [(if (and (not (empty? item-canonical-tags))
                               (= item-canonical-tags prev-item-canonical-tags))
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
  [element condition parent-key inherited]
  ;; TODO: if there are multiple parents, then we we need to use an
  ;; set reference. (Or maybe conditions need to be able to
  ;; handle exemplars)
  (expr-let [tag-specs (tag-specifiers element)]
    (let [referent (item-referent :item element :condition condition)
          key (prepend-to-key referent parent-key)]      
      (make-component
       key [item-DOM element key (set tag-specs) inherited]))))

(defn tags-DOM
  "Given a sequence of items that are tags, return components for them,
  wrapped in a div if there is more than one."
  ;; TODO: do different stuff, depending on number of tags. In
  ;; particular, for no tags, we need to make a cell with a key.
  ;; TODO: Actually do the wrapping in a div.
  [tags parent parent-key inherited]
  (let [condition (condition-map :subject parent :elements ['tag])]
    (expr-let [tag-components
               (expr-seq
                map #(tag-component % condition parent-key inherited) tags)]
      (add-attributes (vertical-stack tag-components true) {:class "tag"}))))

(defn tag-items-pair-DOM
  "Given a list, each element of the form [item, [tag ... tag], where
   the tags for each item are equivalent, generate DOM for the items,
   sharing a label with the tags."
  ;; TODO: Add some data-* attributes to the tag items that indicate
  ;; what items they pertain to.
  ;; TODO: for deep items, use a layout with tag above content to conserve
  ;; horizontal space.
  [items-and-tags parent parent-key inherited]
  (let [sample-tags (get-in items-and-tags [0 1])]
    (expr-let [condition (elements-condition parent sample-tags)
               items (map first items-and-tags)
               item-referents (map #(item-referent :item %
                                                   :condition condition)
                                   items)
               item-doms (expr-seq
                          map (fn [[item tag-list] referent]
                                (let [key (prepend-to-key referent parent-key)]
                                  (make-component key
                                                  [item-DOM item key
                                                   (set tag-list) inherited])))
                          items-and-tags
                          item-referents)
               tags-dom (let [parent-ref (if (= (count items) 1)
                                           (first item-referents)
                                           (set-referent :items item-referents))
                              parent-key (prepend-to-key parent-ref parent-key)]
                          (expr tags-DOM
                            (order-items sample-tags)
                            ;; TODO: Is the next line really right for
                            ;; nested parallel tags?
                            (if (= (count items) 1) (first items) (vec items))
                            parent-key inherited))]
      [:div {:style {:display "table-row"}}
       ;; TODO: Give these keys, so they can support insertion?
       (add-attributes tags-dom
                       {:style {:display "table-cell"}
                        :class (if (> (count item-doms) 1)
                                 "tag-column for-multiple-items"
                                 "tag-column")})
       (add-attributes (vertical-stack item-doms true)
                       {:style {:display "table-cell"}
                        :class "item-column"})])))

(defn tagged-items-DOM
  "Return DOM for the given items, as a grid of tags and values."
  ;; We use a table as a way of making all the cells of a row the same height.
  [items parent parent-key inherited]
  (expr-let [rows (expr group-by-tag (order-items items))
             row-doms (expr-seq
                       map #(tag-items-pair-DOM % parent parent-key inherited)
                       rows)]
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
            [:div {:class "content-text editable"}
             (if (= content :none) "" (str content))]
            (let [referent (item-referent :item content)
                  child-key (cond referent key)]
              (make-component
               child-key [item-DOM child-key content #{} inherited-down])))]
      (if (empty? elements)
        (add-attributes content-dom {:class "item" :key key})
        (expr-let [elements-dom (tagged-items-DOM
                                 elements item key inherited-down)]
          (add-attributes (vertical-stack [content-dom elements-dom])
                          {:class "item" :key key}))))))

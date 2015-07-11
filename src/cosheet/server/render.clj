(ns cosheet.server.render
  (:require (cosheet [entity :as entity]
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

;;; Further, the key is also used by actions to provide the
;;; information to interpret the action, in particular, identify what
;;; part of the data the user is referring to, which isn't always just
;;; an item, and the context of data, so that it knows what added data
;;; must look like.

;;; All keys are sequences. Their first element is a context description
;;; that gives the information needed by action interpretation, while
;;; their subsequent elements are descriptions for parent pieces of dom, not
;;; necessarily for every parent, but for enough to make the key
;;; unique.

;;; There are several kinds of context descriptions, each a map whose
;;; keys depend on what the dom cell refers to.
;;;       item: {:item <item> :condition <condition>}
;;;   template: {:template <condition>
;;;              :before-item <item>
;;;              :first-item <item>
;;;              :last-item <item>
;;;              :after-item <item>}
;;;   exemplar: {:exemplar <exemplar key>}

;;; In these descriptions, a condition is a query that an item must satisfy.
;;; It is a map, which can contain
;;;   {:subject <id of subject of item>
;;;    :elements <list of elements that item must have>}

;;; An item key refers to a particular item, but may also give a
;;; condition required for a new item to appear adjacent to the item
;;; in question. That way, insert actions can be interpreted.

;;; A template key refers to all elements of the parent key that
;;; satisfy a template condition. These keys are used for dom cells
;;; where the user can add items. If items are already in the cell,
;;; then first-item and last-item will be listed, so that ordering
;;; information can be inferred for items added at the beginning and
;;; end of the cell. If the cell is empty, the before-item and
;;; after-item may be listed, giving items that an item added to the
;;; cell should come after or before.

;;; An exemplar key stands for each of several different items or
;;; templates. It's canonical use is for tag doms that pertain to
;;; several items, each having equal tags. A change in that dom should
;;; change the tags of each of the items. An exemplar key has two
;;; parts, the exemplar and the ancestor. The ancestor is a template,
;;; and the top item of the exemplar satisfies it. Concatenated
;;; together, the exemplar and ancestor would be a single ordinary
;;; key. As an exemplar key, the visible information of the exemplar
;;; is matched against each element satisfying the template, to yield
;;; a different key for each match. The exemplar stands for that set
;;; of keys.

;;; TODO: Move this multiset stuff to utils?

(defn multiset-conj
  "Add an item to a multiset,
   represented as a map from items to multiplicities."
  [ms item]
  (update-in ms [item] #((fnil + 0) % 1)))

(defn multiset
  "Turn a seq into a multiset."
  [items]
  (reduce multiset-conj {} items))

(defn item-description
  "Turn keyword arguments into an item context description."
  [& {:as args}]
  (assert (every? #{:item :condition}(keys args)))
  args)

(defn template-description
  "Turn keyword arguments into a template context description."
  [& {:as args}]
  (assert (every? #{:condition :first-item :last-item :before-item :after-item}
                  (keys args)))
  args)

(defn condition-description
  "Turn keyword arguments into a template context description."
  [& {:as args}]
  (assert (every? #{:subject :elements}(keys args)))
  args)

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
    (condition-description :subject subject :elements visible-elements)))

(defn order-comparator
  "Compare two ordering values, where the input is sequences,
   with the ordering information first."
  [a b]
  ;; TODO: For now the order information is just a number. But it
  ;; should soon change to be an interval, so that a new element can
  ;; be added before or after an old one by splitting the interval of
  ;; the old one, handing half back to the old one and half to the new
  ;; one. To allow for arbitrary precision, we use a sequence of
  ;; numbers, first number most significant. That gives the starting
  ;; point of the interval. Then the interval is given by a pair of
  ;; the starting point, and a single number for the end point,
  ;; implicitly preceeded by all but the last number of the starting
  ;; point.
  (< (first a) (first b)))

(defn order-items
  "Return the items in the proper sort order."
  [items]
  (expr-let [order-info
             (expr-seq map #(entity/label->content % :order) items)]
    (map second (sort order-comparator (map vector order-info items)))))

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

(defn template-key
  "Return a template key, given the template and parent key."
  [template parent-key]
  (cons [:template template] parent-key))

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
  [element condition parent-key inherited]
  ;; TODO: if there are multiple parents, then we we need to use an
  ;; exemplar description. (Or maybe conditions need to be able to
  ;; handle exemplars)
  (expr-let [tag-specs (tag-specifiers element)]
    (let [description (item-description :item element :condition condition)
          key (cons description parent-key)]      
      (make-component
       key [item-DOM element key (set tag-specs) inherited]))))

(defn tags-DOM
  "Given a sequence of tags, return components for the given items, wrapped in
  a div if there is more than one."
  ;; TODO: do different stuff, depending on number of tags. In
  ;; particular, for no tags, we need to make a cell with a key.
  [tags parent parent-key inherited]
  (let [condition (condition-description :subject parent :elements ['tag])]
    (expr-let [tag-visibles (expr-seq map visible-to-list tags)]
      (expr-let [tag-components
                 (expr-seq
                  map #(tag-component % condition parent-key inherited) tags)]
        (add-attributes (vertical-stack tag-components true) {:class "tag"})))))

(defn tag-items-pair-DOM
  "Given a list, each element of the form [item, [tag ... tag], where
   the tags for each item are equivalent, generate DOM for the items,
   labeled with the tags."
  ;; TODO: Add some data-* attributes to the tag items that indicate
  ;; what items they pertain to.
  ;; TODO: for deep items, use a layout with tag above content to conserve
  ;; horizontal space.
  [items-and-tags parent parent-key inherited]
  (let [sample-tags (get-in items-and-tags [0 1])]
    (expr-let [condition (elements-condition parent sample-tags)
               item-doms (expr-seq
                          map (fn [[item tag-list]]
                                (let [description (item-description
                                                   :item item
                                                   :condition condition)
                                      key (cons description parent-key)]
                                  (make-component key
                                                  [item-DOM item key
                                                   (set tag-list) inherited])))
                          items-and-tags)
               tags-dom (expr tags-DOM
                          (order-items sample-tags)
                          parent parent-key inherited)]
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
            (let [description (item-description :item content)
                  child-key (cond description key)]
              (make-component
               child-key [item-DOM child-key content #{} inherited-down])))]
      (if (empty? elements)
        (add-attributes content-dom {:class "item" :key key})
        (expr-let [elements-dom (tagged-items-DOM
                                 elements item key inherited-down)]
          (add-attributes (vertical-stack [content-dom elements-dom])
                          {:class "item" :key key}))))))

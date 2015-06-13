(ns cosheet.server.render
  (:require (cosheet [entity :as entity]
                     [dom-utils
                      :refer [into-attributes dom-attributes add-attributes]]
                     [reporters
                      :refer [value expr expr-let expr-seq]])))

;;; Code to create hiccup style dom for a database entity.
;;; Sub-components of the dom are specified as
;;;   [:component {:sibling-key sibling-key
;;;                :definition definition
;;;                :attributes <attributes to add the definition's result>}]
;;; This is what is expected by dom_tracker.

;;; For convenience here, style attributes are represented with their
;;; own map, rather than as a string, so they are easier to adjust.
;;; They must be turned into a string when the dom is sent to reagent.

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
                               ("table" "inline-table") "table") }})))

(def item-DOM)

(defn make-component
  "Make a component dom descriptor, with the given sibling key and definition,
   and, optionally, additional attributes."
  ([sibling-key definition]
   [:component {:sibling-key sibling-key
                :definition definition}])
  ([sibling-key definition attributes]
   (assert (map? attributes))
   [:component {:sibling-key sibling-key
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
  a vertical stack of the items."
  [doms]
  (case (count doms)
    0 [:div]
    1 (first doms)
    (into [:div]
          (map stack-vertical doms))))

;;; TODO: Make the tag styling fancier, with things like rounded
;;; corners and graduated colors.
(def tag-styling {:style {:background-color "#66FFFF"}})

(def item-styling {:style {:border-style "solid"
                           :border-width "2px"
                           :box-sizing "border-box"
                           :margin "-2px"}})

(defn tag-component
  "Return the component for a tag element."
  [element inherited]
  (expr-let [tag-specs (tag-specifiers element)]
    (make-component
     element
     [item-DOM element (set tag-specs) inherited]
     {})))

(defn tags-DOM
  "Given a sequence of tags, return components for the given items, wrapped in
   a div if there is more than one."
  [tags inherited]
  (expr-let [tag-components (expr-seq map #(tag-component % inherited) tags)]
    (add-attributes (vertical-stack tag-components) {:class "tag"})))

(defn tag-items-pair-DOM
  "Given a list of items and the tags for each, where the tags for each item
  are equivalent, generate DOM for the items, labeled with the tags."
  ;;; TODO: Add some data-* attributes to the tag items that indicate
  ;;; what items they pertain to.
  ;;; TODO: pass down, via inherited, how deeply nested the item is,
  ;;; and for deep items, use a layout with tag above content to conserve
  ;;; horizontal space.
  [items-and-tags inherited]
  (expr-let [item-doms (expr-seq
                        map (fn [[item tag-list]]
                              (make-component
                               item [item-DOM item (set tag-list) inherited]))
                        items-and-tags)
             tags-dom (expr tags-DOM
                        (order-items (get-in items-and-tags [0 1]))
                        inherited)]
    [:div {:style {:display "table-row"}}
     (add-attributes tags-dom
                     {:style {:display "table-cell"}})
     (add-attributes (vertical-stack item-doms)
                     {:style {:display "table-cell"}})]))

(defn tagged-items-DOM
  "Return DOM for the given items, as a grid of tags and values."
  [items inherited]
  (expr-let [rows (expr group-by-tag (order-items items))
             row-doms (expr-seq map #(tag-items-pair-DOM % inherited) rows)]
    (into [:div {:style {:display "table"}}
           [:colgroup
            [:col {:style {:width "30%"}}]
            [:col {:style {:width "70%"}}]]]
          row-doms)))

(defn item-DOM
  "Return a hiccup representation of DOM describing
  an item and all its elements. Where appropriate, inherit
  properties from the map of inherited properties."
  [item excluded inherited]
  ;;; TODO: pass down, via inherited, how deeply nested the item is,
  ;;; and for deep items, use a layout with tag above content to conserve
  ;;; horizontal space.
  (expr-let [content (entity/content item)
             elements (visible-elements item)]
    (let [elements (remove excluded elements)
          content-dom
          (if (entity/atom? content)
            [:div (if (= content :none) "" (str content))]
            (make-component "content" [item-DOM content #{} inherited]))]
      (if (empty? elements)
        (add-attributes content-dom {:class "item"})
        (expr-let [elements-dom (tagged-items-DOM elements inherited)]
          (add-attributes (vertical-stack [content-dom elements-dom])
                          item-styling))))))

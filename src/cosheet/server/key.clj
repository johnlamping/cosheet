(ns cosheet.server.key
  (:require (cosheet [utils :refer [multiset]]
                     [expression :refer [expr expr-let expr-seq]]
                     [entity :as entity :refer [elements description->entity
                                                StoredEntity]]
                     [store  :as store :refer [id-valid? StoredItemDescription]]
                     [query :refer [matching-elements matching-items]])))

;;; Each component is identified with a key, as is any other dom node
;;; that the user might interact with. The key both uniquely
;;; identifies its dom and indicates what item(s) that dom reflects.
;;; Since the same item can be reflected by several dom nodes,
;;; uniqueness is achived by handled by having keys indicate not only
;;; the item(s), but also the path of containment in the dom. The key
;;; of a component must not change throughout the life of its parent
;;; dom, because we keep a mapping between client DOM ids and server keys,
;;; which will be broken if the key changes.

;;; Keys are sequences. The first element is a referent that specifies
;;; the item(s) the key refers to, while the rest of the sequence
;;; gives a context for those items. Each referent along the key that
;;; refers to items must be the next step up the subject/content
;;; hierarchy. Elements referents rely on this to find their subject,
;;; and parallel referents rely on it to be able to instantiate their
;;; exemplar keys into multiple examples. Additional information, in
;;; the form of comment referents, may make the key unique, even among
;;; keys that refer to the same item(s).

;;; There are several kinds of referents
;;;        item: <an item-id>
;;;     comment: [:comment <information>
;;;     content: [:content]
;;;       group: [:group <An item-id of the group, typically the first>]
;;;       query: [:query <condition>]
;;;    elements: [:elements <condition>]
;;;    parallel: [:parallel <list of exemplar referents>
;;;                         <list of item, query, or elements referents>]

;;; Query, elements, and parallel reverents inherently indicate
;;; multiple items. A key can have at most one of them, which must be
;;; the first referent of the key.  As described below, however, a
;;; parallel referent can itself have another key, which can contain
;;; another multiple item referent.

;;; An item referent indicates a dom node that describes a particular
;;; item. Typically, a dom that refers to an item will additionally
;;; have a :sibling-elements attribute giving a list of elements that
;;; a sibling item must have, each in list form. Incorporating that
;;; into the item referent would be a mistake, as the referent could
;;; then change even though the identity of the item hadn't, causing
;;; the key of a component to change unexpectedly.

;;; A comment referent does not refer to an item, and has no effect on
;;; what the key refers to. It is used to make a key unique among
;;; other keys that refer to the same item.

;;; A content referent indicates the content of an item.

;;; A group referent indicates a dom node that holds several items,
;;; the first of which is the given item. It's prototypical use is
;;; when several items from a list are grouped into one dom for
;;; display purposes. (Not currently used.)

;;; A query referent indicates a dom node that holds those all items
;;; that satisfy the given condition, which is either an item id or
;;; the list form of an element. Query referents are used for tables,
;;; to find all the items for their rows.

;;; An elements referent indicates a dom node that holds those
;;; elements that satisfy the given condition and whose subject is
;;; what the later part of the key refers to. The condition is either
;;; an item id or the list form of an element. Element referents are
;;; used for tag nodes, since they show all tags of the subject, and
;;; for cells of a table, if the table columns are conditions. In
;;; contrast to a group node, a condition node might be empty, or two
;;; sibling condition nodes might both show the same item.

;;; A parallel referent stands for a set of items. Its prototypical use
;;; is when the tags of several items would be displayed identically,
;;; and the display of the tags is collapsed into a single dom node. A
;;; change to that node should change tags for each of the items. The
;;; parallel referent's job is to indicate the set of items
;;; corresponding to those parallel tags.

;;; TODO: allow parallel referents to take a vector of items as one of
;;; its parallel referents, not just simple referents. That way,
;;; different path lengths are supported.

;;; A parallel referent consists of an exemplar key, and a list of
;;; item, query, or elements referents. In the simplest case, the
;;; exemplar key is empty, in which case the parallel reference refers
;;; to each of items referred to any of the referents. A non-empty
;;; exemplar describes a navigation path to be traced through each of
;;; those items. Starting with each item, the navigation finds an
;;; element whose visible information matches the visible information
;;; of the last item in the exemplar. That becomes the new item in the
;;; navigation, which continues moving foward in the exemplar. If the
;;; first referent of the exemplar is an item, then the parallel
;;; referent refers to what that item ended up matching at the end of
;;; each of the navigations. If the first referent is another
;;; parallel, each item of the nested parallel is matched, and the
;;; navigation recurses.

;;; Note: The exemplar of a parallel could use list forms of elements,
;;; rather than items. The advantage of items is that if the content
;;; changes, the exemplar still makes sense, while the advantage of
;;; list forms is that the exemplar still makes sense if its exemplar
;;; item is deleted. For now, we use items, because it provides a
;;; natural way to handle nested parallels and to provide uniqueness.

(defn item-referent? [referent]
  (and referent (not (sequential? referent))))

(defn comment-referent? [referent]
  (and (sequential? referent) (= ( first referent) :comment)))

;;; A content location referent refers to the location of a content,
;;; not its current value, so it leaves the item to still be the item
;;; that holds the content. Thus, it is just a comment.
(defn content-location-referent? [referent]
  (and (comment-referent? referent)
       (= (second referent) 'content-location)))

(defn content-referent? [referent]
  (and (sequential? referent) (= ( first referent) :content)))

(defn query-referent? [referent]
  (and (sequential? referent) (= ( first referent) :query)))

(defn elements-referent? [referent]
  (and (sequential? referent) (= ( first referent) :elements)))

(defn parallel-referent? [referent]
  (and (sequential? referent) (= ( first referent) :parallel)))

(defn referent? [referent]
  (or (item-referent? referent)
      (and (sequential? referent)
           (#{:comment :content :query :elements :parallel} (first referent)))))

(defn referent-type [referent]
  (if (sequential? referent) (first referent) :item))

(defn item-referent
  "Create an item referent from an item."
  [item]
  (assert (satisfies? StoredEntity item))
  (:item-id item))

(defn comment-referent
  "Create a content referent."
  [info]
  [:comment info])

(defn content-location-referent
  "Create a content location referent."
  []
  (comment-referent 'content-location))

(defn content-referent
  "Create a content referent."
  []
  [:content])

(defn query-referent
  "Create an query referent."
  [condition]
  [:query (if (satisfies? StoredEntity condition)
            (:item-id condition)
            condition)])

(defn elements-referent
  "Create an elements referent."
  [condition]
  [:elements (if (satisfies? StoredEntity condition)
               (:item-id condition)
               condition)])

(defn parallel-referent
  "Create a parallel referent."
  [exemplar parallel-referents]
  (doseq [referent exemplar] (assert (referent? referent)))
  (doseq [x parallel-referents]
    (assert (or (satisfies? StoredEntity x) (referent? x))))
  [:parallel exemplar (map #(if (satisfies? StoredEntity %) (:item-id %) %)
                           parallel-referents)])

(defn prepend-to-key
  "Prepend a new referent to the front of a key, maintaining the invariant
  that a parallel referent may only occur in first position."
  [referent key]
  (let [initial (first key)]
    (vec
     (if (and (sequential? initial)
              (= :parallel (first initial)))
       (let [[_ exemplar item-ids] initial]
         (cons [:parallel (prepend-to-key referent exemplar) item-ids]
               (rest key)))
       (cons referent key)))))

(defn first-primitive-referent
  "Return the first non-parallel referent of a key, recursing through
  the exemplars of parallels."
  [key]
  (let [referent (first key)]
    (if (parallel-referent? referent)
      (let [[type exemplar items] referent]
        (first-primitive-referent exemplar))
      referent)))

(defn remove-first-primitive-referent
  "Remove the first referent from the key, going into exemplars."
  [[first & rest]]
  (if (parallel-referent? first)
    (let [[type exemplar items] first]
      (if (empty? exemplar)
        rest
        (vec (cons [type
                    (vec (remove-first-primitive-referent exemplar))
                    items]
                   rest))))
    rest))

(defn remove-content-location-referent
  "If a key starts with a content location referent, remove it."
  [key]
  (if (content-location-referent? (first-primitive-referent key))
    (remove-first-primitive-referent key)
    key))

(defn item-ids-referred-to
  "Return all the item ids referred to by item referents in the key,
  in order from most specific to most generic
  (Only includes exemplar items from parallel referents.)"
  [key]
  (when (not (empty? key))
    (let [[first & rest] key
          rest-items (item-ids-referred-to rest)]
      (cond (parallel-referent? first)
            (let [[type exemplar items] first]
              (concat (item-ids-referred-to exemplar) rest-items))
            (item-referent? first)
            (cons first rest-items)
            true
            rest-items))))

(defn filtered-items
  "Run the filter on each of the items,
  returning the items for which it is true."
  [condition items]
  (expr-let [passed (expr-seq map #(expr-let [passes (condition %)]
                                     (when passes %))
                              items)]
    (filter identity passed)))

(defn filtered-elements
  "Return a seq of all elements of the entity that satisfy the condition."
  [entity condition]
  (expr-let [elements (entity/elements entity)]
    (filtered-items condition elements)))

;;; TODO: Replace "visible" with "semantic", because it is what counts
;;; for matching purposes, not what is displayed. In particular, table
;;; layouts are certainly visible, but only the contents of the table
;;; is semantic.

;;; Note that the item that makes an element non-semantic for its
;;; subject is a semantic part of the element. This makes it possible
;;; to find a non-semantic element of one item that matches a
;;; non-semantic element of an exemplar item.

(defn visible-entity?
  "Return true if an entity is visible to the user
  (Doesn't have a keyword element.)"
  [entity]
  (expr-let [elements (entity/elements entity)
             element-contents (expr-seq map entity/content elements)]
    (not-any? keyword? element-contents)))

(defn visible-elements
  "Return the elements of an entity that are visible to the user."
  [entity]
  (filtered-elements entity visible-entity?))

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

(defn item->canonical-visible
  "Return the canonical form of the visible information for the item.
  Only works on immutable items."
  [item]
  (canonicalize-list (visible-to-list item)))

(defn visible-matching-element
  "Given the list form of visible information and an item,
  find an element of the item that matches the visible information.
  Return nil if there is no matching element.
  Only works on immutable items."
  [visible-info item]
  (let [canonical-visible (canonicalize-list visible-info)]
    (->> (matching-elements visible-info item)
         ;; Get rid of the ones with extra visible info.
         (filter #(= (item->canonical-visible %) canonical-visible))
         first)))

(defn instantiate-exemplar-item-id
  "Given the id of an exemplar item and a regular item, find an element
   of the item that matches the visible information of the exemplar.
   Return nil if there is no matching element.
   The store and item must be immutable."
  [immutable-store exemplar-id item]
  (when (id-valid? immutable-store exemplar-id)
    (visible-matching-element
     (visible-to-list (description->entity exemplar-id immutable-store))
     item)))

(def instantiate-exemplar)
(def instantiate-referent)

(defn instantiate-parallel-referent
  [immutable-store group referent parent item-id-instantiator]
  (let [[type exemplar parallel-referents] referent]
    (assert (= type :parallel))
    (let [instantiated-items
          (mapcat #(instantiate-referent
                    immutable-store false % parent item-id-instantiator)
                  parallel-referents)]
      (if (empty? exemplar)
        (if group
          (if (empty? instantiated-items) nil [instantiated-items])
          instantiated-items)
        (mapcat (partial instantiate-exemplar
                         immutable-store group exemplar)
                instantiated-items
                (map (fn [item]
                       #(instantiate-exemplar-item-id immutable-store % item))
                     instantiated-items))))))

(defn condition-as-list
  "If the condition is an item id, return its visible list form, otherwise,
  assume it is already in visible list form."
  [condition item-id-instantiator]
  (if (satisfies? StoredItemDescription condition)
    (visible-to-list (item-id-instantiator condition))
    condition))

(defn instantiate-referent
  [immutable-store group referent parent item-id-instantiator]
  (if (parallel-referent? referent)
    (instantiate-parallel-referent
     immutable-store group referent parent item-id-instantiator)
    (let [items
          (case (referent-type referent)
            :item (when-let [item (item-id-instantiator referent)] [item])
            :comment [parent]
            :content [(entity/content parent)]
            :query (let [[_ condition] referent]
                     (matching-items
                      (condition-as-list condition item-id-instantiator)
                      immutable-store))
            :elements (let [[_ condition] referent]
                        (matching-elements
                           (condition-as-list condition item-id-instantiator)
                           parent)))]
      (if group (when items [items]) items))))

(defn instantiate-exemplar
  "Given a store, an exemplar, a parent item, and a function from
  item-id to immutable entity, instantiate the exemplar with respect
  to the function, returning the sequence of items matched, or a
  sequence of groups of items if group is true. The exemplar must have
  been pruned to only referents that refer to items. (The groups are
  all the items returned by the ultimate first referent.)"
  [immutable-store group exemplar parent item-id-instantiator]
  (assert (vector? exemplar))  ; So peek and pop take from end.
  (let [last-referent (peek exemplar)
        remainder (pop exemplar) ]
    (if (empty? remainder)
      (instantiate-referent
       immutable-store group last-referent parent item-id-instantiator)
      (do (assert ((some-fn item-referent?
                            content-referent?
                            comment-referent?)
                   last-referent))
          (when-let [item (first (instantiate-referent
                                  immutable-store false last-referent
                                  parent item-id-instantiator))]
            (instantiate-exemplar
             immutable-store group remainder
             item #(instantiate-exemplar-item-id immutable-store % item)))))))

(defn key->items-or-item-groups
  "Return the list of items or groups of items that a key describes.
  The store must be immutable."
  [immutable-store key group]
  (println "items for key" key)
  (let [first-item-referent-index
        (first (keep-indexed
                (fn [index referent] (when (item-referent? referent) index))
                key))
        ;; We include at least one item referent, to make sure we have
        ;; a parent item for the first referent to make use of.
        sufficient-key (if first-item-referent-index
                         (vec (take (inc first-item-referent-index) key))
                         key)]
    (instantiate-exemplar
     immutable-store group sufficient-key nil
     #(when (id-valid? immutable-store %)
        (description->entity % immutable-store)))))

(defn key->items
  "Return the list of items that a key describes.
  The store must be immutable."
  [immutable-store key]
  (key->items-or-item-groups immutable-store key false))

(defn key->item-groups
  "Given a key, return a list of the groups of items it describes.
  If the first element of the key is a parallel with an empty exemplar,
  there is one group for each instantiation of the items of the parallel.
  Otherwise, each item is its own group.
  The store must be immutable."
  [immutable-store key]
  (key->items-or-item-groups immutable-store key true))

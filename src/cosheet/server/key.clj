(ns cosheet.server.key
  (:require (cosheet [utils :refer [multiset]]
                     [expression :refer [expr expr-let expr-seq]]
                     [entity :as entity :refer [elements description->entity]]
                     [store :refer [id-valid?]])))

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

;;; NOTE: :ordering is not currently implemented, and may not be
;;; needed. Typically a dom for a group referent or a condition
;;; referent will additionally have a :ordering attribute that gives
;;; information about the first and last item in the group, or about
;;; the items just before or after it. If there are any elements in
;;; the node, then first-item and last-item will be listed, so that
;;; ordering information can be inferred for items added at the
;;; beginning and end of the node. If the node is empty, then
;;; before-item and after-item may be listed, giving items that an
;;; item added to the node should come after or before.

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

(defn content-referent
  "Create a condent referent"
  []
  [:content])

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

(defn item-referent? [referent]
  (and referent (not (sequential? referent))))

(defn content-referent? [referent]
  (and (sequential? referent) (= ( first referent) :content)))

(defn condition-referent? [referent]
  (and (sequential? referent) (= ( first referent) :condition)))

(defn parallel-referent? [referent]
  (and (sequential? referent) (= ( first referent) :parallel)))

(defn item-determining-referents
  "Return the elements of a key that may be needed to determine the items
   it means."
  [key]
  (vec (filter (some-fn item-referent? parallel-referent?) key)))

(defn first-primitive-referent
  "Return the first non-parallel referent of a key."
  [key]
  (let [referent (first key)]
    (if (parallel-referent? referent)
      (let [[type exemplar items] referent]
        (first-primitive-referent exemplar))
      referent)))

(defn remove-first-referent
  "Remove the first referent from the key."
  [[first & rest]]
  (if (parallel-referent? first)
    (let [[type exemplar items] first]
      (if (empty? exemplar)
        rest
        (vec (cons [type (remove-first-referent exemplar) items] rest))))
    rest))

(defn remove-content-referent
  "If a key starts with a content referent, remove it."
  [[first & rest]]
  (cond (content-referent? first)
        rest
        (parallel-referent? first)
        (let [[type exemplar items] first]
          (vec (cons [type (remove-content-referent exemplar) items] rest)))
        (nil? first) []
        true (vec (cons first rest))))

(defn item-ids-referred-to
  "Return all the item ids referred to by item referents in the key,
  in order from most specific to most generic
  (Only includes exemplars items from parallel referents.)"
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

(defn filtered-elements
  "Return a seq of all elements of the entity that satisfy the condition."
  [entity condition]
  (expr-let [elements (entity/elements entity)
             passed (expr-seq map #(expr-let [passes (condition %)]
                                        (when passes %))
                              elements)]
    (filter identity passed)))

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
  "Return the canonical form of the visible information for the item."
  [item]
  (canonicalize-list (visible-to-list item)))

(defn visible-matching-element
  "Given the list form of visible information and an item,
  find an element of the item that matches the visible information.
  Return nil if there is no matching element."
  [store visible-info item]
  (first (filter #(= (item->canonical-visible %) visible-info)
                 (elements item))))

(defn instantiate-item-id
  "Given the id of an exemplar item and a regular item, find an element
   of the item that matches the visible information of the exemplar.
   Return nil if there is no matching element."
  [store exemplar-id item]
  (when (id-valid? store exemplar-id)
    (visible-matching-element
     store
     (item->canonical-visible (description->entity exemplar-id store))
     item)))

(defn instantiate-exemplar
  "Given a store, an exemplar, and a function from item-id to entity,
  instantiate the exemplar with respect to the function,
  returning the sequence of items matched, or a sequence of
  groups of items if group is true.
  The exemplar must have been pruned to only referents that refer to items.
  (A non-trivial group is formed by a parallel referent with empty exemplar.)"
  [store group exemplar item-id-instantiator]
  (assert (vector? exemplar))  ; So peek and pop take from end.
  (let [last-referent (peek exemplar)
          remainder (pop exemplar)]
      (if (sequential? last-referent)
        (let [[type exemplar item-ids] last-referent
              exemplar-referents (item-determining-referents exemplar)]
          (assert (= type :parallel))  ; Other complex referents filtered.
          (assert (empty? remainder))  ; Parallel referents must be first.
          (let [instantiated-items
                (remove nil? (map item-id-instantiator item-ids))]
            (if (empty? exemplar)
              (if group
                (if (empty? instantiated-items) nil [instantiated-items])
                instantiated-items)
              (mapcat (partial instantiate-exemplar
                               store group exemplar-referents)
                      (map (fn [item] #(instantiate-item-id store % item))
                           instantiated-items)))))
        (let [exemplar-item (item-id-instantiator last-referent)]
          (cond (nil? exemplar-item) []
                (empty? remainder) (if group [[exemplar-item]] [exemplar-item])
                true (instantiate-exemplar
                      store group remainder
                      #(instantiate-item-id store % exemplar-item)))))))

(defn key->items
  "Return the list of items that a key describes."
  [store key]
  (instantiate-exemplar
   store false [(first (item-determining-referents key))]
   #(when (id-valid? store %) (description->entity % store))))

(defn key->item-groups
  "Given a key, return a list of the groups of items it describes.
   If the first element of the key is a parallel with an empty exemplar,
   there is one group for each instantiation of the items of the parallel.
  Otherwise, each item is its own group."
  [store key]
  (instantiate-exemplar
   store true [(first (item-determining-referents key))]
   #(when (id-valid? store %) (description->entity % store))))

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
;;;     comment: [:comment <information>] or string or keyword
;;;     content: [:content]
;;;       group: [:group <An item-id of the group, typically the first>]
;;;       query: [:query <condition>]
;;;    elements: [:elements <condition>]
;;;         key: [:key <key>]
;;;    parallel: [:parallel <exemplar key>
;;;                         <list of branch referents>
;;;                         (optional)<list of negative referents]

;;; Only used as temporaries during key instantiation:
;;;    template: [:template <list form of semantic information>]
;;;  bound-item: [:bound-item <item>]

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

;;; A key referent has an entire key, a list of other referents. It
;;; refers to the items referred to by the key, in the current
;;; context. It is used when a key needs to be used where referent is
;;; required. This is in parallel referents, described next.

;;; A parallel referent stands for a set of items. Its prototypical use
;;; is when the tags of several items would be displayed identically,
;;; and the display of the tags is collapsed into a single dom node. A
;;; change to that node should change tags for each of the items. The
;;; parallel referent's job is to indicate the set of items
;;; corresponding to those parallel tags.

;;; A parallel referent consists of an exemplar key, a list of branch
;;; referents, and an optional list of negative referents. In the
;;; simplest case, the exemplar key is empty, in which case the
;;; parallel reference refers to all items that are referred to any of
;;; the branch referents and not referred to by any of the negative
;;; referents. A non-empty exemplar describes a navigation path to be
;;; traced through each of those items. Starting with each item, the
;;; navigation finds an element whose semantic information matches the
;;; semantic information of the last item in the exemplar. That
;;; becomes the new item in the navigation, which continues moving
;;; foward in the exemplar. If the first referent of the exemplar is
;;; an item, then the parallel referent refers to what that item ended
;;; up matching at the end of each of the navigations. If the first
;;; referent is another parallel, each item of the nested parallel is
;;; matched, and the navigation recurses.

;;; Note: The exemplar of a parallel could use list forms of elements,
;;; rather than items. The advantage of items is that if the content
;;; changes, the exemplar still makes sense, while the advantage of
;;; list forms is that the exemplar still makes sense if its exemplar
;;; item is deleted. For now, we use items, because it provides a
;;; natural way to handle nested parallels and to provide uniqueness.

;;; During instantiation of a key, item referents are turned into
;;; template referents or bound-item referents. The former is used for
;;; items that occur in exemplars, and gives the pattern that the
;;; exemplar must match. The latter is for items that occur outside of
;;; exemplars, and is just the item id connected to the store.

(defn item-referent? [referent]
  (satisfies? StoredItemDescription referent))

(defn comment-referent? [referent]
  (or (string? referent)
      (keyword? referent)
      (and (sequential? referent) (= ( first referent) :comment))))

;;; A content location referent refers to the location of a content,
;;; not its current value, so it leaves the item to still be the item
;;; that holds the content. Thus, it is just a comment.
(defn content-location-referent? [referent]
  (and (sequential? referent)
       (comment-referent? referent)
       (= (second referent) 'content-location)))

(defn content-referent? [referent]
  (and (sequential? referent) (= ( first referent) :content)))

(defn query-referent? [referent]
  (and (sequential? referent) (= ( first referent) :query)))

(defn elements-referent? [referent]
  (and (sequential? referent) (= ( first referent) :elements)))

(defn key-referent? [referent]
  (and (sequential? referent) (= ( first referent) :key)))

(defn parallel-referent? [referent]
  (and (sequential? referent) (= ( first referent) :parallel)))

(defn referent? [referent]
  (or (item-referent? referent)
      (comment-referent? referent)
      (and (sequential? referent)
           (#{:comment :content :query :elements :key :parallel
              :template :bound-item}
            (first referent)))))

(defn referent-type [referent]
  (assert (referent? referent) referent)
  (cond (sequential? referent) (first referent)
        (comment-referent? referent) :comment
        true :item))

(defn item-referent
  "Create an item referent from an item."
  [item]
  (assert (satisfies? StoredEntity item))
  (:item-id item))

(defn convert-item-to-referent
  "If an item is provided, convert it to a referent."
  [x]
  (if (satisfies? StoredEntity x)
    (item-referent x)
    (do (assert (referent? x) x)
        x)))

(defn convert-items-to-referents
  "Convert items in the list to referents."
  [referents]
  (vec (map convert-item-to-referent referents)))

(defn comment-referent
  "Create a comment referent."
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

(defn key-referent
  "Create a key referent."
  [key]
  [:key (convert-items-to-referents key)])

(defn parallel-referent
  "Create a parallel referent."
  ([exemplar branch-referents]
   (parallel-referent exemplar branch-referents nil))
  ([exemplar branch-referents negative-referents]
   (assert (vector? exemplar))
   (cond-> [:parallel
            (convert-items-to-referents exemplar)
            (convert-items-to-referents branch-referents)]
         (not (empty? negative-referents))
         (conj (convert-items-to-referents negative-referents)))))

(defn prepend-to-key
  "Prepend a new referent to the front of a key, maintaining the invariant
  that a parallel referent may only occur in first position."
  [referent key]
  (let [initial (first key)]
    (vec
     (if (and (sequential? initial)
              (= :parallel (first initial)))
       (let [[_ exemplar branch-referents negative-referents] initial]
         (cons (parallel-referent (prepend-to-key referent exemplar)
                                  branch-referents negative-referents)
               (rest key)))
       (cons referent key)))))

(defn first-primitive-referent
  "Return the first non-parallel referent of a key, recursing through
  the exemplars of parallels."
  [key]
  (let [referent (first key)]
    (if (parallel-referent? referent)
      (let [[type exemplar _] referent]
        (first-primitive-referent exemplar))
      referent)))

(defn remove-first-primitive-referent
  "Remove the first referent from the key, going into exemplars."
  [[first & rest]]
  (if (parallel-referent? first)
    (let [[type exemplar branch-referents negative-referents] first]
      (if (empty? exemplar)
        rest
        (vec (cons (parallel-referent
                    (vec (remove-first-primitive-referent exemplar))
                    branch-referents negative-referents)
                   rest))))
    rest))

(defn remove-content-location-referent
  "If a key starts with a content location referent, remove it."
  [key]
  (if (content-location-referent? (first-primitive-referent key))
    (remove-first-primitive-referent key)
    key))

(def remove-comments)

(defn remove-comments-from-referent
  "Remove the comments from the referent,
   returning nil if the referent is itself a comment."
  [referent]
  (when (not (comment-referent? referent))
    (cond (parallel-referent? referent)
          (let [[tag exemplar branch-referents negative-referents] referent]
            (parallel-referent (remove-comments exemplar)
                               (remove-comments branch-referents)
                               (remove-comments negative-referents)))
          (key-referent? referent)
          (let [[tag key] referent]
            (key-referent (remove-comments key)))
          true
          referent)))

(defn remove-comments
  "Remove the comment referents (including content-location-referent)
  from the key"
  [key]
  (vec (filter identity (map remove-comments-from-referent key))))

(defn item-ids-referred-to
  "Return all the item ids referred to by item referents in the key,
  in order from most specific to most generic
  (Only includes exemplar items from parallel referents.)"
  [key]
  (when (not (empty? key))
    (let [[first & rest] key
          rest-items (item-ids-referred-to rest)]
      (cond (parallel-referent? first)
            (let [[type exemplar _ _] first]
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

;;; For purposes of comparing two entities, not all of their elements
;;; matter. In particular, order information, or other information
;;; about how to display the eements is considered irrelevant for
;;; coalescing entities. We call the elements that matter the semantic
;;; elements. Non-semantic elements will always themselves have an
;;; element with a keyword as content. For example, order information
;;; has :order as an element's value, and so is not semantic.

;;; Note, however that being non-semantic is a relationship between an
;;; element and its subject, not an intrinsic property of the
;;; element. Typically, even though an element is non-semantic to its
;;; parent, all its elements are semantic to it.  This makes it
;;; possible to find a non-semantic element of one item that matches a
;;; non-semantic element of an exemplar item.

(defn semantic-entity?
  "Return true if an entity counts as semantic information
  (Doesn't have a keyword element.)"
  [entity]
  (expr-let [elements (entity/elements entity)
             element-contents (expr-seq map entity/content elements)]
    (not-any? keyword? element-contents)))

(defn semantic-elements
  "Return the elements of an entity that are semantic to it."
  [entity]
  (filtered-elements entity semantic-entity?))

(defn semantic-to-list
  "Given an item, make a list representation of the
  semantic information of the item."
  [item]
  (if (entity/atom? item)
    (entity/content item)
    (expr-let [content (entity/content item)
               elements (semantic-elements item)
               content-semantic (semantic-to-list content)
               element-semantics (expr-seq map semantic-to-list elements)]
      (if (empty? element-semantics)
        content-semantic
        (list* (into [content-semantic] element-semantics))))))

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

(defn item->canonical-semantic
  "Return the canonical form of the semantic information for the item.
  Only works on immutable items."
  [item]
  (canonicalize-list (semantic-to-list item)))

(defn semantic-matching-element
  "Given the list form of semantic information and an item,
  find an element of the item that matches the semantic information.
  Return nil if there is no matching element.
  Only works on immutable items."
  [semantic-info item]
  (let [canonical-semantic (canonicalize-list semantic-info)]
    (let [matches (matching-elements semantic-info item)]
      (when (not (empty? matches))
        (first (if (> (count matches) 1)
                 ;; Prefer one with no extra semantic info.
                 (let [perfect-matches
                       (filter #(= (item->canonical-semantic %)
                                   canonical-semantic)
                               matches)]
                   (if (empty? perfect-matches)
                     matches
                     perfect-matches))
                 matches))))))

(defn replace-nones
  "Replace any :none in the seq with nil"
  [x]
  (cond (sequential? x) (map replace-nones x)
        (= x :none) nil
        true x))

(defn semantics-as-list
  "If the item is an item id, return the list form of its semantics,
   otherwise, return the item, assuming it is already in semantic list form."
  [item immutable-store]
  (replace-nones
   (if (satisfies? StoredItemDescription item)
     (semantic-to-list (when (id-valid? immutable-store item)
                         (description->entity item immutable-store)))
     item)))

;;; The binding step does all the context independent access to the
;;; store, so it needs to be done only once, not for every
;;; instantiation of an exemplar.

(def bind-referents)
(def bind-referent)

(defn bind-and-combine-referent
  "Bind the referent, and if more than one bound referent comes back,
  turn them into a key referent, so we always return a list of
  one referent or none."
  [referent immutable-store in-exemplar]
  (let [bound (bind-referent referent immutable-store in-exemplar)]
    (when (not (empty? bound))
      (if (= (count bound) 1)
        bound
        [[:key (vec bound)]]))))

(defn bind-referent
  "Return a possibly empty list of bound referents for the referent."
  [referent immutable-store in-exemplar]
  (case (referent-type referent)
    :item (if in-exemplar
            [[:template (semantics-as-list referent immutable-store)]]
            (when (id-valid? immutable-store referent)
              [[:bound-item (description->entity referent immutable-store)]]))
    :comment nil
    :content [referent]
    :query [[:query (semantics-as-list (second referent) immutable-store)]] 
    :elements [[:elements (semantics-as-list (second referent)
                                             immutable-store)]]
    :key [[:key (vec (bind-referents
                      (second referent) immutable-store in-exemplar))]]
    :parallel (let [[_ exemplar branch-referents negative-referents] referent]
                [(into
                  [:parallel
                   (vec (bind-referents exemplar immutable-store true))
                   (mapcat #(bind-and-combine-referent
                             % immutable-store in-exemplar)
                           branch-referents)]
                  (when (not (empty? negative-referents))
                    [(mapcat #(bind-and-combine-referent
                                % immutable-store in-exemplar)
                              negative-referents)]))])))

(defn bind-referents
  "Bind the key to the store, in preparation for instantiation. Most
  significantly, replace item referents with either bound-item
  referents that have the item, if we are in an exemplar, replace them
  with template references specifying their contents. Template
  references are used only during instantiation.
  Return a lazy seq of the bound referents in the key."
  [key immutable-store in-exemplar]
  (mapcat #(bind-referent % immutable-store in-exemplar) key))

(def instantiate-bound-key)
(def instantiate-bound-referent)

(defn instantiate-bound-parallel-referent
  [immutable-store group referent parent]
  (let [[type exemplar branch-referents negative-referents] referent]
    (assert (= type :parallel))
    (let [positive-items (set (mapcat
                               #(instantiate-bound-referent
                                 immutable-store false % parent)
                               branch-referents))
          negative-items (set (mapcat
                               #(instantiate-bound-referent
                                 immutable-store false % parent)
                               negative-referents))
          instantiated-items (seq (clojure.set/difference positive-items
                                                               negative-items))]
      (if (empty? exemplar)
        (if group
          (if (empty? instantiated-items) nil [instantiated-items])
          instantiated-items)
        (mapcat (partial instantiate-bound-key
                         immutable-store group exemplar)
                instantiated-items)))))

(defn instantiate-bound-referent
  [immutable-store group referent parent]
  (if (parallel-referent? referent)
    (instantiate-bound-parallel-referent
     immutable-store group referent parent)
    (let [items
          (case (referent-type referent)
            :bound-item [(second referent)]
            :template (when parent
                        (when-let [match (semantic-matching-element
                                          (second referent) parent)]
                          [match]))
            :content (when parent [(entity/content parent)])
            :query (let [[_ condition] referent]
                     (matching-items condition  immutable-store))
            :elements (when parent
                        (let [[_ condition] referent]
                          (matching-elements condition parent)))
            :key (instantiate-bound-key
                  immutable-store group (second referent) parent))]
      (if group (when items [items]) items))))

(defn instantiate-bound-key
  "Given a store, whether to group, a bound key, and a parent item,
  instantiate the key relative to the parent, returning the sequence
  of items matched, or a sequence of groups of items if group is
  true. (The groups are all the items returned by the ultimate first
  referent.)"
  [immutable-store group exemplar parent]
  (assert (vector? exemplar))  ; So peek and pop take from end.
  (let [last-referent (peek exemplar)
        remainder (pop exemplar)]
    (if (empty? remainder)
      (instantiate-bound-referent
       immutable-store group last-referent parent)
      (when-let [items (instantiate-bound-referent
                        immutable-store false last-referent parent)]
        (assert (= (count items) 1))
        (instantiate-bound-key
         immutable-store group remainder (first items))))))

(defn key->items-or-item-groups
  "Return the list of items or groups of items that a key describes.
  The store must be immutable."
  [immutable-store key group]
  (let [bound-referents (bind-referents key immutable-store false)
        ;; The first two referents will be enough to include an item referent.
        sufficient-key (vec (take 2 bound-referents))]
    (instantiate-bound-key
     immutable-store group sufficient-key nil)))

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

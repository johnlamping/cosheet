(ns cosheet.server.hierarchy
  (:require (cosheet [utils :refer [multiset multiset-diff multiset-union
                                    multiset-to-generating-values update-last]]
                     [mutable-set :refer [mutable-set-intersection]]
                     [debug :refer [simplify-for-print current-value]]
                     [expression :refer [expr-let expr-seq]])
            (cosheet.server [key :refer [filtered-items semantic-element?
                                         item-referent parallel-referent
                                         canonicalize-list semantic-to-list]])))
;;; A hierarchy organizes a sequence of "members"
;;; into a hierarchy, based on a multiset of "properties" associated with
;;; each member.
;;; The hierarchy consists of a vector of nodes, each of which is a map that
;;; has:
;;;       :hierarchy-node  true (used to identify hierarchy nodes)
;;;           :properties  A multiset of the properties added by this node.
;;; :cumulatve-properties  The multiset union of the properties of this node
;;;                        all all its ancestors.
;;;              :members  A vector of members whose properties exactly
;;;                        match the cumulative-properties of this node.
;;;                        All members must come before all children in the
;;;                        order from which the hierarchy was built. This means 
;;;                        that some children may contain members that would
;;;                        have qualified to be members of the node,
;;;                        except for coming after other non-members.
;;;             :children  An optional vector of child nodes.

(defn hierarchy-node?
  [node]
  (contains? node :hierarchy-node))

(defn append-to-hierarchy
  "Given a member and its properties, add them to the hierarchy.
  If the node has an parent, its ancestor properties must be provided.
  Don't merge items with empty properties at the top level."
  ([hierarchy member properties]
   (append-to-hierarchy hierarchy member properties {}))
  ([hierarchy member properties ancestor-properties]
   (let [make-node (fn [members properties]
                     {:hierarchy-node true
                      :members members
                      :properties properties
                      :cumulative-properties (multiset-union
                                              properties ancestor-properties)})]
     (if (empty? hierarchy)
       [(make-node [member] properties)]
       (let [last-entry (last hierarchy)]
         (if (and ;; Don't merge a member with empty properties.
              (or (empty? (:properties last-entry)) (empty? properties))
              ;; Unless both are empty and we are not at top level.
              (not (and (not (empty? ancestor-properties))
                        (empty? (:properties last-entry))
                        (empty? properties)))) 
           (conj hierarchy (make-node [member] properties))
           (let [[old-only new-only both] (multiset-diff
                                           (:properties last-entry) properties)]
             (if (empty? old-only)
               (update-last
                hierarchy
                (if (and (empty? new-only)
                         (not (contains? last-entry :children)))
                  (fn [last] (update-in last [:members]
                                        #((fnil conj []) % member)))
                  (fn [last] (update-in
                              last [:children]
                              #(append-to-hierarchy
                                % member new-only
                                (multiset-union both ancestor-properties))))))
               (if (empty? both)
                 (conj hierarchy (make-node [member] properties))
                 (append-to-hierarchy
                  (update-last hierarchy
                               (fn [last]
                                 (assoc (make-node [] both)
                                        :children
                                        [(assoc last :properties old-only)])))
                  member properties ancestor-properties))))))))))

(def flatten-hierarchy)

(defn flatten-hierarchy-node
  "Given a hierarchy node, a depth, return the sequence of
  all descendant nodes in pre-order. Add :depth to the returned nodes."
  [node depth]
  (cons (assoc node :depth depth)
        (flatten-hierarchy (:children node) (inc depth))))

(defn flatten-hierarchy
  "Given a hierarchy and a depth, return the sequence of all descendant nodes
  in pre-order. Add :depth to the returned nodes."
  [hierarchy depth]
  (mapcat #(flatten-hierarchy-node % depth) hierarchy))

(defn hierarchy-node-descendants
  "Return all members at or below the node."
  [node]
  (concat (:members node) (mapcat hierarchy-node-descendants (:children node))))

(defn hierarchy-node-next-level
  "Return the concatenation of the members and children of the node.
  If any children have empty :properties, splice in their members."
  [node]
  (concat (:members node)
          (mapcat #(if (empty? (:properties %))
                     (do (assert (empty? (:children %)))
                         (:members %))
                     [%])
                  (:children node))))

(defn hierarchy-node-members
  "Return the members at the level of the hierarchy node
  (not the descendants below)."
  [node]
  (:members node))

(def hierarchy-nodes-extent)

(defn hierarchy-node-extent
  "Return a seq of descendants the node that is just big enough that
  the properties of each descendant of the node are a superset
  of the properties of some member of the extent."
  [node]
  (if (seq (:members node))
    [(first (:members node))]
    ;; Check for a child with no properties. Its members work as extents.
    (if-let [child-members (seq (filter #(and (not (empty? (:members %)))
                                              (empty? (:properties %)))
                                        (:children node)))]
      [(first (:members (first child-members)))]
      (hierarchy-nodes-extent (:children node)))))

(defn hierarchy-nodes-extent
  "Return a seq of descendants the nodes that is just big enough that
  the properties of each descendant of the nodes are a superset
  of the properties of some member of the extent."
  [nodes]
  (seq (apply clojure.set/union (map #(set (hierarchy-node-extent %)) nodes))))

;;; The following code assumes that the members of a hierarchy are info-maps,
;;; containing
;;;               :item  The item that is the member
;;;   :property-elements  The elements of the item that contribute
;;;                       to the cumulative properties of this node
;;;                       in the hierarchy
;;; :property-canonicals  A list of canonical-info-sets for each element in
;;;                       :property-elements.

(def canonical-to-list)

(defn canonical-set-to-list
  "Given a set of canonicalized lists or sets,
   with the set also in canonical form, return a list of the items."
  [set]
  (when (not (empty? set))
    (reduce (fn [result [key count]]
              (concat result
                      (repeat count (if (map? key)
                                      (canonical-set-to-list key)
                                      (canonical-to-list key)))))
            [] (seq set))))

(defn canonical-to-list
  "Given a canonicalized list form of an item or set of items,
  return a list form for it."
  [item]
  (if (sequential? item)
    (do (assert (= (count item) 2))
        (cons (canonical-to-list (first item))
              (canonical-set-to-list (second item))))
    item))

(defn canonical-info
  [entity]
  (expr-let [semantic (semantic-to-list entity)]
    (canonicalize-list semantic)))

(defn canonical-info-set
  "Given a seq of items, return a canonical representation of the items,
   treated as a multi-set."
  [entities]
  (expr-let [canonicals (expr-seq map canonical-info entities)]
    (multiset canonicals)))

(defn split-by-do-not-merge-subset
  "Given a list of item maps, and a subset of items not to merge,
  return a list of lists, broken so that any item-info-map whose item
  is in the set gets its own list, if it has non-trivial properties."
  [item-info-maps do-not-merge-subset]
  (first
   (reduce
    (fn [[result do-not-merge-with-prev] item-info-map]
      (cond (do-not-merge-subset (:item item-info-map))
            [(conj result [item-info-map]) true]
            do-not-merge-with-prev
            [(conj result [item-info-map]) false]
            true
            [(update-last result #((fnil conj []) % item-info-map)) false]))
    [[] false]
    item-info-maps)))

(defn hierarchy-by-canonical-info
  "Given a sequence of item info maps, and a subset of items in the maps not to
  merge, return a hierarchy. If restrict-empty-merges is true, don't merge
  items at the top level with empty properties."
  [item-info-maps do-not-merge]
  (let [non-empty-items (map :item
                             (filter #(not (empty? (:property-canonicals %)))
                                     item-info-maps))]
    (expr-let
        [do-not-merge-subset (mutable-set-intersection
                              do-not-merge non-empty-items)]
      (mapcat
       #(reduce (fn [hierarchy item-info-map]
                  (append-to-hierarchy
                   hierarchy item-info-map
                   (multiset (:property-canonicals item-info-map)) {}))
                [] %)
       (split-by-do-not-merge-subset item-info-maps do-not-merge-subset)))))

(defn item-maps-by-elements
  "Given items in order, and a list of elements for characterize the hierarchy,
  return item maps for each item."
  [items elements]
  (expr-seq
   map
   (fn [item elements]
     (expr-let [filtered (filtered-items semantic-element?
                                         elements)
                canonicals (expr-seq
                            map canonical-info filtered)]
       {:item item
        :property-elements filtered
        :property-canonicals canonicals}))
   items elements))

(defn items-hierarchy-by-elements
  "Given items in order, and a list of elements for each, organize the items
  into a hierarchy by the semantic info of the corresponding
  elements. Don't merge items that are in do-not-merge."
  [items elements do-not-merge]
  (expr-let
      [item-maps (item-maps-by-elements items elements)]
    (hierarchy-by-canonical-info item-maps do-not-merge)))

(defn hierarchy-node-example-elements
  "Given a hierarchy node, return a list of example elements
  for its properties."
  [hierarchy-node]
  (let [example (first (hierarchy-node-descendants hierarchy-node))]
    (multiset-to-generating-values
     (:properties hierarchy-node)
     (:property-canonicals example)
     (:property-elements example))))

(defn hierarchy-node-items-referent
  "Given a hierarchy node, return an item referent to all its descendants."
  [hierarchy-node]
  (let [descendants (hierarchy-node-descendants hierarchy-node)
        affected-items (map :item descendants)]
    (if (= (count affected-items) 1)
      (item-referent (first affected-items))
      (parallel-referent [] affected-items))))

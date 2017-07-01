(ns cosheet.server.hierarchy
  (:require (cosheet
             [utils :refer [multiset multiset-diff multiset-union
                            multiset-to-generating-values update-last]]
             [debug :refer [simplify-for-print]]
             [expression :refer [expr-let expr-seq]])
            (cosheet.server
             [referent :refer [semantic-elements-R semantic-to-list-R
                               item->canonical-semantic-R
                               union-referent-if-needed
                               item-referent parallel-union-referent
                               item-or-exemplar-referent]])))

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
;;;          :child-nodes  An optional vector of child nodes.
;;;
;;; All the functions below that take a node-or-member argument also work
;;; on non-hierarchy nodes, which are assumed to be members of the hierarchy.
;;; These are interpreted as nodes with just themselves as a member,
;;; and no properties or children.

(defn hierarchy-node?
  [node]
  (and (map? node)
       (contains? node :hierarchy-node)))

(defn append-to-hierarchy
  "Given a member and its properties, add them to the hierarchy.
  If the node has a parent, its ancestor properties must be provided.
  Don't merge items with empty properties at the top level."
  [hierarchy member properties ancestor-properties]
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
                        (not (contains? last-entry :child-nodes)))
                 (fn [last] (update-in last [:members]
                                       #((fnil conj []) % member)))
                 (fn [last] (update-in
                             last [:child-nodes]
                             #(append-to-hierarchy
                               % member new-only
                               (multiset-union both ancestor-properties))))))
              (if (empty? both)
                (conj hierarchy (make-node [member] properties))
                (append-to-hierarchy
                 (update-last hierarchy
                              (fn [last]
                                (assoc (make-node [] both)
                                       :child-nodes
                                       [(assoc last :properties old-only)])))
                 member properties ancestor-properties)))))))))

(defn hierarchy-node-descendants
  "Return all members at or below the node."
  [node-or-member]
  (if (hierarchy-node? node-or-member)
    (concat (:members node-or-member)
            (mapcat hierarchy-node-descendants (:child-nodes node-or-member)))
    [node-or-member]))

(defn hierarchy-node-next-level
  "Return the concatenation of the members and children of the node.
  If any children have empty :properties, splice in their members."
  [node-or-member]
  (if (hierarchy-node? node-or-member)
    (concat (:members node-or-member)
            (mapcat #(if (empty? (:properties %))
                       (do (assert (empty? (:child-nodes %)))
                           (:members %))
                       [%])
                    (:child-nodes node-or-member)))
    [node-or-member]))

(defn hierarchy-node-members
  "Return the members at the level of the hierarchy node."
  [node-or-member]
  (if (hierarchy-node? node-or-member)
    (:members node-or-member)
    [node-or-member]))

(defn hierarchy-node-properties
  "Return the local properties of the hierarchy node."
  [node-or-member]
  (when (hierarchy-node? node-or-member)
    (:properties node-or-member)))

(defn hierarchy-node-cumulative-properties
  "Return the cumulative properties of the hierarchy node."
  [node-or-member]
  (when (hierarchy-node? node-or-member)
    (:cumulative-properties node-or-member)))

(def hierarchy-nodes-extent)

;;; TODO: This code should be aware of refinements of properties, not just
;;; added properties.
(defn hierarchy-node-extent
  "Return a seq of descendants of the node that is just big enough that
  the properties of each descendant of the node are a superset
  of the properties of some member of the extent."
  [node]
  (if (seq (:members node))
    [(first (:members node))]
    ;; Check for a child with no properties. Its members work as extents.
    (if-let [child-members (seq (filter #(and (not (empty? (:members %)))
                                              (empty? (:properties %)))
                                        (:child-nodes node)))]
      [(first (:members (first child-members)))]
      (hierarchy-nodes-extent (:child-nodes node)))))

(defn hierarchy-nodes-extent
  "Return a seq of descendants the nodes that is just big enough that
  the properties of each descendant of the nodes are a superset
  of the properties of some member of the extent."
  [nodes]
  (seq (apply clojure.set/union (map #(set (hierarchy-node-extent %)) nodes))))

;;; The following code assumes that the members of a hierarchy are info maps,
;;; containing at least the following:
;;;                :item  The item that is the member.
;;;   :property-elements  The elements of the item that contribute
;;;                       to the cumulative properties of this node
;;;                       in the hierarchy.
;;; :property-canonicals  A list of canonical-semantic for each element in
;;;                       :property-elements.

(defn hierarchy-by-canonical-info
  "Given a sequence of item info maps, return a hierarchy."
  [item-info-maps]
  (reduce (fn [hierarchy item-info-map]
            (append-to-hierarchy
             hierarchy item-info-map
             (multiset (:property-canonicals item-info-map)) {}))
          [] item-info-maps))

(defn item-map-by-elements-R
  "Given an item and a seq of elements of the item that characterize how
   it should fit in a hierarchy, return an item info map."
  [item elements]
  (expr-let [canonicals (expr-seq map item->canonical-semantic-R elements)]
       {:item item
        :property-elements elements
        :property-canonicals canonicals}))

(defn item-maps-by-elements-R
  "Given parallel sequences of items in order, and lists of elements
   that characterize the hierarchy,
  return item info maps for each item."
  [items elements]
  (expr-seq map item-map-by-elements-R items elements))

(defn hierarchy-by-all-elements-R
  "Given a sequence of items, generate a hierarchy based on all their elements."
  [items]
  (expr-let
      [items-elements (expr-seq map semantic-elements-R items)
       item-maps (item-maps-by-elements-R items items-elements)]
    (hierarchy-by-canonical-info item-maps)))

(defn hierarchy-node-example-elements
  "Given a hierarchy node, return a list of example elements
  for its properties."
  [node-or-member]
  (when (hierarchy-node? node-or-member)
    (let [example (first (hierarchy-node-descendants node-or-member))]
      (multiset-to-generating-values
       (:properties node-or-member)
       (:property-canonicals example)
       (:property-elements example)))))

(defn hierarchy-node-items-referent
  "Given a hierarchy node or member, return a referent to all its descendants."
  [hierarchy-node-or-member subject-referent]
  (union-referent-if-needed
   (map #(item-or-exemplar-referent (:item %) subject-referent)
        (hierarchy-node-descendants hierarchy-node-or-member))))

(defn hierarchy-node-parallel-items-referent
  "Given a hierarchy node or member, return a referent to all its descendants,
  returning one group per group the subject returns."
  [hierarchy-node-or-member subject-referent]
  (parallel-union-referent
   (map #(item-or-exemplar-referent (:item %) subject-referent)
        (hierarchy-node-descendants hierarchy-node-or-member))))

(defn hierarchy-last-item-referent
  "Return a referent to the last item of the hierarchy, if any."
  [hierarchy]
  (when (seq hierarchy)
    (let [last-item (last (hierarchy-node-descendants (last hierarchy)))]
      (item-referent (:item last-item)))))

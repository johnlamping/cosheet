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

;;; A hierarchy organizes a sequence of "leaves" into a hierarchy,
;;; based on a multiset of "properties" associated with each leaf.
;;; The hierarchy consists of a vector of nodes, each of which is a map that
;;; has:
;;;       :hierarchy-node  true (used to identify hierarchy nodes)
;;;           :properties  A multiset of the properties added by this node.
;;; :cumulatve-properties  The multiset union of the properties of this node
;;;                        all all its ancestors.
;;;               :leaves  A vector of leaves whose properties exactly
;;;                        match the cumulative-properties of this node.
;;;                        All leaves must come before all children in the
;;;                        order from which the hierarchy was built. This means 
;;;                        that some children may contain leaves that would
;;;                        have qualified to be leaves of the node,
;;;                        except for coming after other non-leaves.
;;;          :child-nodes  An optional vector of child nodes.
;;;
;;; All the functions below that take a node-or-leaf argument also work
;;; on non-hierarchy nodes, which are assumed to be leaves of the hierarchy.
;;; These are interpreted as nodes with just themselves as a leaf,
;;; and no properties or children.

(defn hierarchy-node?
  [node]
  (and (map? node)
       (contains? node :hierarchy-node)))

(defn append-to-hierarchy
  "Given a leaf and its properties, add them to the hierarchy.
  If the node has a parent, its ancestor properties must be provided.
  Don't merge items with empty properties at the top level."
  [hierarchy leaf properties ancestor-properties]
  (let [make-node (fn [leaves properties]
                    {:hierarchy-node true
                     :leaves leaves
                     :properties properties
                     :cumulative-properties (multiset-union
                                             properties ancestor-properties)})]
    (if (empty? hierarchy)
      [(make-node [leaf] properties)]
      (let [last-entry (last hierarchy)]
        (if (and ;; Don't merge a leaf with empty properties.
             (or (empty? (:properties last-entry)) (empty? properties))
             ;; Unless both are empty and we are not at top level.
             (not (and (not (empty? ancestor-properties))
                       (empty? (:properties last-entry))
                       (empty? properties)))) 
          (conj hierarchy (make-node [leaf] properties))
          (let [[old-only new-only both] (multiset-diff
                                          (:properties last-entry) properties)]
            (if (empty? old-only)
              (update-last
               hierarchy
               (if (and (empty? new-only)
                        (not (contains? last-entry :child-nodes)))
                 (fn [last] (update-in last [:leaves]
                                       #((fnil conj []) % leaf)))
                 (fn [last] (update-in
                             last [:child-nodes]
                             #(append-to-hierarchy
                               % leaf new-only
                               (multiset-union both ancestor-properties))))))
              (if (empty? both)
                (conj hierarchy (make-node [leaf] properties))
                (append-to-hierarchy
                 (update-last hierarchy
                              (fn [last]
                                (assoc (make-node [] both)
                                       :child-nodes
                                       [(assoc last :properties old-only)])))
                 leaf properties ancestor-properties)))))))))

(defn hierarchy-node-descendants
  "Return all leaves at or below the node."
  [node-or-leaf]
  (if (hierarchy-node? node-or-leaf)
    (concat (:leaves node-or-leaf)
            (mapcat hierarchy-node-descendants (:child-nodes node-or-leaf)))
    [node-or-leaf]))

(defn hierarchy-node-next-level
  "Return the concatenation of the leaves and children of the node.
  If any children have empty :properties, splice in their leaves."
  [node-or-leaf]
  (if (hierarchy-node? node-or-leaf)
    (concat (:leaves node-or-leaf)
            (mapcat #(if (empty? (:properties %))
                       (do (assert (empty? (:child-nodes %)))
                           (:leaves %))
                       [%])
                    (:child-nodes node-or-leaf)))
    [node-or-leaf]))

(defn hierarchy-node-leaves
  "Return the leaves at the level of the hierarchy node."
  [node-or-leaf]
  (if (hierarchy-node? node-or-leaf)
    (:leaves node-or-leaf)
    [node-or-leaf]))

(defn hierarchy-node-properties
  "Return the local properties of the hierarchy node."
  [node-or-leaf]
  (when (hierarchy-node? node-or-leaf)
    (:properties node-or-leaf)))

(defn hierarchy-node-cumulative-properties
  "Return the cumulative properties of the hierarchy node."
  [node-or-leaf]
  (when (hierarchy-node? node-or-leaf)
    (:cumulative-properties node-or-leaf)))

(def hierarchy-nodes-extent)

;;; TODO: This code should be aware of refinements of properties, not just
;;; added properties.
(defn hierarchy-node-extent
  "Return a seq of descendants of the node that is just big enough that
  the properties of each descendant of the node are a superset
  of the properties of some leaf of the extent."
  [node]
  (if (seq (:leaves node))
    [(first (:leaves node))]
    ;; Check for a child with no properties. Its leaves work as extents.
    (if-let [child-leaves (seq (filter #(and (not (empty? (:leaves %)))
                                              (empty? (:properties %)))
                                        (:child-nodes node)))]
      [(first (:leaves (first child-leaves)))]
      (hierarchy-nodes-extent (:child-nodes node)))))

(defn hierarchy-nodes-extent
  "Return a seq of descendants the nodes that is just big enough that
  the properties of each descendant of the nodes are a superset
  of the properties of some leaf of the extent."
  [nodes]
  (seq (apply clojure.set/union (map #(set (hierarchy-node-extent %)) nodes))))

;;; The following code assumes that the leaves of a hierarchy are info maps,
;;; containing at least the following:
;;;                :item  The item that is the leaf.
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
  [node-or-leaf]
  (when (hierarchy-node? node-or-leaf)
    (let [example (first (hierarchy-node-descendants node-or-leaf))]
      (multiset-to-generating-values
       (:properties node-or-leaf)
       (:property-canonicals example)
       (:property-elements example)))))

(defn hierarchy-node-items-referent
  "Given a hierarchy node or leaf, return a referent to all its descendants."
  [hierarchy-node-or-leaf subject-referent]
  (union-referent-if-needed
   (map #(item-or-exemplar-referent (:item %) subject-referent)
        (hierarchy-node-descendants hierarchy-node-or-leaf))))

(defn hierarchy-node-parallel-items-referent
  "Given a hierarchy node or leaf, return a referent to all its descendants,
  returning one group per group the subject returns."
  [hierarchy-node-or-leaf subject-referent]
  (parallel-union-referent
   (map #(item-or-exemplar-referent (:item %) subject-referent)
        (hierarchy-node-descendants hierarchy-node-or-leaf))))

(defn hierarchy-last-item-referent
  "Return a referent to the last item of the hierarchy, if any."
  [hierarchy]
  (when (seq hierarchy)
    (let [last-item (last (hierarchy-node-descendants (last hierarchy)))]
      (item-referent (:item last-item)))))

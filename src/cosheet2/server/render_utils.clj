(ns cosheet2.server.render-utils
  (:require (cosheet2 [entity :as entity :refer [label?]]
                      [utils :refer [multiset multiset-to-generating-values
                                     replace-in-seqs assoc-if-non-empty
                                     add-elements-to-entity-list
                                     separate-by]]
                      [debug :refer [simplify-for-print]]
                      [store :refer [StoredItemDescription]]
                      [query :refer [matching-elements]]
                      [orderable :as orderable]
                      [canonical :refer [canonicalize-list
                                         canonical-extended-by
                                         canonical-have-common-elaboration]]
                      [hiccup-utils
                       :refer [into-attributes add-attributes]]
                      [expression :refer [expr expr-let expr-seq expr-filter]])
            (cosheet2.server
             [order-utils :refer [semantic-entity?]]
             [model-utils :refer [semantic-elements semantic-elements
                                  entity->fixed-term
                                  entity->fixed-term-with-negations
                                  entity->canonical-semantic]]
             [hierarchy :refer [hierarchy-node-descendants]])))

(defn condition-satisfiers
  "Return a sequence of elements of an entity sufficient to make it
  satisfy the elements of condition and nothing extra, except that
  the empty string is considered nothing extra for a nil. The condition
  must be in list form.  If part of a condition is not satisfied by
  any element, ignore that part."
  [entity condition]
  (when (and (sequential? condition)
             (not (empty? (rest condition))))
    (let [elements (entity/elements entity)
          canonical-elements (map entity->canonical-semantic elements)]
      (multiset-to-generating-values
       (multiset (map #(entity->canonical-semantic
                        (replace-in-seqs % nil ""))
                      (rest condition)))
       canonical-elements elements))))

(defn transform-specification-for-elements
  [specification]
  (assoc (select-keys specification [:width :immutable])
         :template 'anything))

(defn transform-specification-for-labels
  [specification]
  (assoc (select-keys specification [:width :immutable])
         :template '(anything :label)))

(defn transform-specification-for-non-contained-labels
  [specification]
  ;; The label is not contained in a component for the items it
  ;; applies to. We have to keep the action data functions for the
  ;; items the labels pertain to, and the information they use,
  ;; because the label needs to use them as part of its action data
  ;; function.
  (assoc (select-keys specification [:width :immutable
                                     :query-id :stack-id
                                     :excluding-ids :get-action-data
                                     :get-do-batch-edit-action-data])
         :template '(anything :label)))

(defn entity->canonical-term
  "Return the canonical list version of the semantic parts of an entity,
  with 'anything changed to nil."
  [entity]
  (canonicalize-list (entity->fixed-term entity)))

(defn competing-siblings
  "Given an entity that is functioning as a query, return a seq of its
  siblings that compete with matching for it. This is all siblings
  that have a common elaboration and for which the element is not a
  pure elaboration.  In other words, the sibling has to either be
  identical, or not contradict the item and have something that the
  item doesn't have.  Don't include redundant siblings more than
  once."
  [entity]
  (let [entity-canonical (entity->canonical-term entity)
        siblings (semantic-elements (entity/subject entity))
        [labels non-labels] (separate-by label? siblings)
        candidates (if ((set labels) entity) labels non-labels)
        matching (filter #(= entity-canonical (entity->canonical-term %))
                         candidates)]
    (cond-> (vals
             ;; We make a map from canonical to candidate so we can not
             ;; add redunant candidates
             (reduce (fn [so-far candidate]
                       (let [candidate-canonical (entity->canonical-term
                                                  candidate)]
                         (cond-> so-far
                           (and (canonical-have-common-elaboration
                                 entity-canonical candidate-canonical)
                                (not (canonical-extended-by
                                      candidate-canonical entity-canonical))
                                (not (so-far candidate-canonical)))
                           (assoc candidate-canonical candidate))))
                     {} candidates))
      ;; The matching list includes the entity, so there is an identical
      ;; candidate if there is more than one element.
      (not (empty? (rest matching)))
      ;; We only need one matching candidate. If the entitiess are
      ;; distinguishable, choose one different from the entity we
      ;; started with.
      (conj (or (first (remove #(= % entity) matching)) (first matching))))))

;;; DOM creators that are used by several files.

(defn make-component
  "Make a component dom with the given specification"
  [{:as specification}]
  (assert (:relative-id specification) specification)
  [:component specification])

(defn item-component
  "Make a component dom to display the given item. The item's id becomes
   the relative-id."
  [item specification]
  (make-component (assoc specification
                         :relative-id (:item-id item))))

(defn item-minus-excluded-component
  "Make a component dom to display the given item, minus the excluded
  elements."
  [item excluded-elements specification]
  (assert (empty? (:excluded-element-ids specification))
          [excluded-elements specification])
  (if (empty? excluded-elements)
    (item-component item specification)
    (item-component
     item
     (assoc specification
            :excluded-element-ids (vec (map :item-id excluded-elements))))))

(defn nest-if-multiple-DOM
  "If there is only one dom in the doms, return it. Otherwise, return
  a dom with all of the doms as children and with css class for the
  given orientation."
  [doms orientation]
  (cond
    (empty? doms) [:div {}]
    (= (count doms) 1) (first doms)
    true (let [orientation-class (case orientation
                                   :vertical "vertical-stack"
                                   :horizontal "horizontal-stack")]
           (assert (not= (first doms) :div))
           (into [:div {:class orientation-class}]
                 doms))))

(defn item-stack-DOM
  "Given a list of items and a matching list of elements to exclude,
  generate components for each item, and put them in a DOM.
  If there is more than one item, make the stack in the given orientation."
  [items excludeds orientation specification]
  (let [components (map #(item-minus-excluded-component %1 %2 specification)
                        items excludeds)]
    (nest-if-multiple-DOM components orientation)))

(defn hierarchy-node-DOM
  "Create a DOM for a hierarchy node, calling functions to make the pieces.
  For each node, calls
     (node-f node child-doms specification)
  where child-doms are the results of calling node-f for all the child
  nodes.  Before doing calls for a child, it calls
     (child-specification-f node specification)
  This must return the specification to be used for the children.
  child-specification-f may be left out, in which case the original
  specification is used for all children."
  ([node node-f specification]
   (hierarchy-node-DOM node node-f
                       (fn [node specification] specification)
                       specification))
  ([node node-f child-specification-f specification]
   (let [child-doms (when-let [children (:child-nodes node)]
                     (let [child-spec (child-specification-f
                                       node specification)]
                       (map #(hierarchy-node-DOM
                              % node-f child-specification-f child-spec)
                        children)))]
     (node-f node child-doms specification))))





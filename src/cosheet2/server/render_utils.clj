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

(comment
;;; These next functions handle inherited attributes.
;;; In inherited, :attributes is a vector of descriptors.
;;; A descriptor is either:
;;;   <attributes>
;;;       A map of attribute->value pairs to add
;;;   (<step>* <attributes>)
;;;      Add the attributes to descendant doms following the sequence of steps, 
;;;      where <step> is either the id of an item to which the rest of the
  ;;       descriptor should apply, or a set that can contain any of
;;;         :label
;;;            Apply to label elements
;;;         :element
;;;            Apply to non-label elements
;;;         :content
;;;            Apply to the content of the item (which may be the overall item)
;;;         :recursive
;;;            Apply to any repetition of the path
;;;         :optional
;;;            May be skipped
;;;     NOTE: Currently, if step is an id, the rest of the descriptor must be
;;;           only a map.

  (defn add-inherited-attribute
    "Add an attribute, specified with a descriptor, to inherited."
    [inherited descriptor]
    (update inherited :attributes #(conj (or % []) descriptor)))

  (defn remove-inherited-attribute
    "Given inherited and an attribute key, remove that attribute from all paths."
    [inherited attribute-key]
    (update inherited :attributes
            #(vec (keep (fn [description]
                          (let [attributes (if (map? description)
                                             description
                                             (last description))]
                            (if (attribute-key attributes)
                              (let [smaller (dissoc attributes attribute-key)]
                                (when (not-empty smaller)
                                  (if (map? description)
                                    smaller
                                    (conj (vec (butlast description)) smaller))))
                              description)))
                        %))))

  (defn remove-inherited-for-item
    "Remove any inherited attributes specified as being for the specific item."
    [inherited item]
    (if-let [descriptors (:attributes inherited)]
      (let [id (:item-id item)]
        (assoc-if-non-empty inherited :attributes
                            (remove #(and (sequential? %) (= (first %) id))
                                    descriptors)))
      inherited))

  (defn advance-along-descriptor
    "Advance one step along an inherited descriptor. Return a seq of possible
  successor descriptors."
    [descriptor motion]
    (when (sequential? descriptor)
      (let [step (first descriptor)
            remainder (rest descriptor)
            following (if (empty? (rest remainder))
                        (first remainder)
                        remainder)]
        (if (set? step)
          (concat (when (step motion)
                    [(if (:recursive step)
                       (vec (concat [(conj step :optional)] remainder))
                       following)])
                  (when (:optional step)
                    (advance-along-descriptor following motion)))
          [descriptor]))))

  (defn transform-descriptors
    "Transform the descriptors as appropriate
  for moving down to a :content :label or :element."
    [descriptors motion]
    (mapcat #(advance-along-descriptor % motion) descriptors))

  (defn transform-inherited-attributes
    "Transform any attributes specified by inherited as appropriate
  for moving down to a :content :label or :element."
    [inherited motion]
    (if-let [descriptors (:attributes inherited)]
      (assoc-if-non-empty inherited :attributes
                          (transform-descriptors descriptors motion))
      inherited))

  (defn split-descriptors-by-currency
    "Return a map of all attributes specified by the desciptors as applying
  only for the current dom and a descriptor of all the remaining attributes,
  some of which may also apply to the current dom."
    [descriptors]
    (reduce (fn [[current-only others] descriptor]
              (if (map? descriptor)
                [(into-attributes current-only descriptor) others]
                [current-only (conj others descriptor)]))
            [{} []] descriptors))

  (defn inherited-attributes
    "Return a map of all attributes specified by inherited for the current dom.
  The attributes are specified as in the comment at
  transform-inherited-attribute."
    [inherited item]
    (let [id (:item-id item)]
      (reduce (fn [attributes descriptor]
                (cond
                  (map? descriptor)
                  (into-attributes attributes descriptor)
                  (satisfies? StoredItemDescription (first descriptor))
                  (cond-> attributes
                    (and (= (first descriptor) id)
                         (= (count descriptor) 2))
                    (into-attributes (last descriptor)))
                  (every? :optional (butlast descriptor))
                  (into-attributes attributes (last descriptor))
                  true
                  attributes))
              {} (:attributes inherited))))

  (defn content-attributes
    "Return the attributes for the content of the current item."
    [inherited]
    (inherited-attributes
     (transform-inherited-attributes inherited :content) nil))

  (defn item-or-content-attributes
    "Return the attributes for the current item or content."
    [inherited]
    (inherited-attributes
     {:attributes
      (concat (:attributes inherited)
              (:attributes (transform-inherited-attributes
                            inherited :content)))}
     nil))

  (defn transform-inherited-for-children
    "Given an inherited, modify it to apply to children of the item.
  Do not alter the attributes, since the child could be either an element
  or label."
    [inherited item-key item-referent]
    (-> inherited
        (assoc :subject-referent item-referent
               :key-prefix item-key)))

  (defn transform-inherited-for-labels
    "Given an inherited that has been transformed for children,
  modify it to apply to labels."
    [inherited]
    (-> inherited
        (transform-inherited-attributes :label)
        (assoc :template (list (:elements-template inherited) :tag)))))

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

(comment
  (defn item-referent-given-inherited
    "Return the proper referent for the item, given inherited."
    [item inherited]
    (let [subject-referent (:subject-referent inherited)
          multiple (:match-multiple inherited)]
      (if multiple
        (let [item-ref (item-referent item)]
          (assert (satisfies? entity/StoredEntity item))
          (assert (not (entity/mutable-entity? item)))
          (if (nil? subject-referent)
            item-ref
            ;; if :match-multiple is :exclusive, we don't match items
            ;; that are matched by siblings that are more general than us.
            (let [competing (when (= multiple :exclusive)
                              (competing-siblings item))]
              (if (empty? competing)
                (elements-referent item-ref subject-referent)
                (exclusive-elements-referent
                 item-ref subject-referent competing)))))
        (item-or-exemplar-referent item subject-referent)))))

(comment
  (defn virtual-referent-DOM
    "Make a dom for a place that could hold an item, but doesn't, given
  the virtual referent for the new item."
    [referent inherited]
    [:div (-> (:selectable-attributes inherited)
              (into-attributes (item-or-content-attributes inherited))
              (into-attributes
               {:class "editable"
                :key (conj (:key-prefix inherited) :virtual)
                :target {:referent referent
                         :select-pattern (or (:select-pattern inherited)
                                             (conj (:key-prefix inherited)
                                                   [:pattern]))}}))]))

(comment
  (defn virtual-element-DOM
    "Make a dom for a place that could hold an item, but doesn't.
  inherited must include a :template and a :subject-referent."
    [adjacent-referent position inherited]
    (assert (not (nil? (:subject-referent inherited))))
    (let [referent (virtual-referent
                    (:template inherited)
                    (subject-referent-given-inherited inherited)
                    adjacent-referent
                    :position position)]
      (virtual-referent-DOM referent inherited))))

;;; DOM creators that are used by several files.

(defn make-component
  "Make a component dom with the given attributes"
  [& {:as attributes}]
  (assert (:relative-id attributes))
  [:component attributes])

(defn item-component
  "Make a component dom to display the given item. The item's id becomes
   the relative-id."
  [dom-fn item & additional-properties]
  (apply make-component
         :relative-id (:item-id item)
         additional-properties))

(defn item-minus-excluded-component
  "Make a component dom to display the given item, minus the excluded
  elements.  The item's id becomes the relative-id, and the combination of
  that and the excluded elements' ids becomes the relative-identity."
  [dom-fn item excluded-elements & additional-properties]
  (if (empty? excluded-elements)
    (apply item-component item additional-properties)
    (apply item-component item
           :excluded-elements (vec excluded-elements)
           :relative-identity (concat [(:item-id item)]
                                      (map :item-id excluded-elements))
           additional-properties)))

(defn nest-if-multiple-DOM
  "If there is only one dom in the doms, return it. Otherwise, return
  a dom with all of the doms as children and with class for
  the stack direction."
  [doms direction]
  (cond
    (empty? doms) [:div {}]
    (= (count doms) 1)  (first doms)
    true (let [class (case direction
                       :vertical "vertical-stack"
                       :horizontal "horizontal-stack")]
           (into [:div {:class class}] doms))))

(defn item-stack-DOM
  "Given a list of items and a matching list of elements to exclude,
  and additional properties that the doms for each of the items should have,
  generate components for each item, and put them in a DOM.
  If there is more than one item, make the stack in the given direction."
  [dom-fn items excludeds direction & additional-properties]
  (let [components (map #(apply item-minus-excluded-component
                                %1 %2 additional-properties)
                        items excludeds)]
    (nest-if-multiple-DOM components direction)))

(defn hierarchy-node-DOM-R
  "Create a DOM for a hierarchy node, calling functions to make the pieces.
  For each node, calls
     (node-f node child-doms function-info inherited)
  where child-doms are the results of calling node-f for all the child
  nodes.  Before doing calls for a child, calls
     (child-info-f node function-info inherited)
  This must return the function-info and inherited to be used for the children.
  Either of the functions may return reporters.
  child-information-f and function-info may be left out, in which case
  node-f is called without function-info, and always with the same value
  of inherited."
  ([node node-f inherited]
   (hierarchy-node-DOM-R node
                         (fn [node child-doms function-info inherited]
                           (node-f node child-doms inherited)) 
                         (fn [node function-info inherited]
                           [function-info inherited])
                         nil inherited))
  ([node node-f child-info-f function-info inherited]
   (assert (every? #(not (nil? %)) (add-elements-to-entity-list
                                    (:template inherited) nil))
           (add-elements-to-entity-list (:template inherited) nil))
   (expr-let
       [child-doms (when-let [children (:child-nodes node)]
                     (expr-let [child-info (child-info-f
                                            node function-info inherited)]
                       (let [[child-function-info child-inherited] child-info]
                         (expr-seq map #(hierarchy-node-DOM-R
                                         % node-f child-info-f
                                         child-function-info child-inherited)
                                   children))))]
     (node-f node child-doms function-info inherited))))

(comment
  (defn hierarchy-node-items-referents
    "Given a hierarchy node or leaf, return a referent to each of its
   descendants."
    [hierarchy-node-or-leaf inherited]
    (map #(item-referent-given-inherited (:item %) inherited)
         (hierarchy-node-descendants hierarchy-node-or-leaf))))

(comment
  (defn hierarchy-node-items-referent
    "Given a hierarchy node or leaf, return a referent to all its descendants."
    [hierarchy-node-or-leaf inherited]
    (union-referent-if-needed
     (hierarchy-node-items-referents hierarchy-node-or-leaf inherited))))

(comment
  (defn hierarchy-last-item-referent
    "Return a referent to the last item of the hierarchy, if any."
    [hierarchy]
    (when (seq hierarchy)
      (let [last-item (last (hierarchy-node-descendants (last hierarchy)))]
        (item-referent (:item last-item))))))



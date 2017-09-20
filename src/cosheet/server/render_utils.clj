(ns cosheet.server.render-utils
  (:require (cosheet [entity :as entity]
                     [utils :refer [multiset multiset-to-generating-values
                                    replace-in-seqs assoc-if-non-empty]]
                     [debug :refer [simplify-for-print]]
                     [store :refer [StoredItemDescription]]
                     [query :refer [matching-elements]]
                     [orderable :as orderable]
                     [hiccup-utils
                      :refer [into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq expr-filter]])
            (cosheet.server
             [referent :refer [referent?
                               virtual-referent item->canonical-semantic
                               parallel-union-referent elements-referent
                               item-referent item-or-exemplar-referent
                               union-referent-if-needed
                               item->canonical-semantic-R
                               semantic-element?-R]]
             [hierarchy :refer [hierarchy-node-descendants]])))

(defn condition-satisfiers-R
  "Return a sequence of elements of an entity sufficient to make it
  satisfy the elements of condition and nothing extra, except that
  the empty string is considered nothing extra for a nil. The condition
  must be in list form.  If part of a condition is not satisfied by
  any element, ignore that part."
  [entity condition]
  (when (and (sequential? condition)
             (not (empty? (rest condition))))
    (expr-let [satisfiers
               (entity/call-with-immutable
                entity
                (fn [entity]
                  (let [elements (entity/elements entity)
                        canonical-elements (expr-seq
                                            map item->canonical-semantic-R
                                            elements)]
                    (multiset-to-generating-values
                     (multiset (map #(item->canonical-semantic
                                      (replace-in-seqs % nil ""))
                                    (rest condition)))
                     canonical-elements elements))))]
      (map #(entity/in-different-store % entity) satisfiers))))

(defn non-implied-matching-elements-R
  "Given an item, a template, and an implied template, return all semantic
  elements matching the template, but throwing out enough elements to exactly
  match the implied template."
  [item template implied-template]
  (expr-let [elements (expr-filter semantic-element?-R
                                   (matching-elements template item))
             excludeds (when implied-template
                         (condition-satisfiers-R item implied-template))]
    (if excludeds
      (seq (clojure.set/difference (set elements) (set excludeds)))
      elements)))

(defn copy-alternate-request-to-target
  "Given the map for a target, and inherited information, change the target
  to have a :alternate if the inherited information says to."
  [target inherited]
  (cond-> target
    (:alternate-target inherited) (assoc :alternate true)))

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

(defn remove-inherited-for-item
  "Remove any inherited attributes specified as being for the specific item."
  [inherited item]
  (if-let [descriptors (:attributes inherited)]
    (let [id (:item-id item)]
      (assoc-if-non-empty inherited :attributes
                          (remove #(and (sequential? %) (= (first %) id))
                                   descriptors)))
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
      (assoc :template '(nil :tag))))

;;; DOM creators that are used by several files.

(defn make-component
  "Make a component dom with the given attributes and definition."
  [attributes definition]
  (assert (map? attributes))
  (assert (:key attributes))
  [:component attributes definition])

(defn item-component
  "Make a component dom for the given item.
  dom-fn should be item-DOM-R, or item-without-labels-DOM-R, or similar."
  [dom-fn item exclude-elements inherited]
  (let [key (conj (:key-prefix inherited) (:item-id item))
        excluded (if (empty? exclude-elements) nil (vec exclude-elements))]
    (make-component {:key key} [dom-fn item excluded inherited])))

(defn subject-referent-given-inherited
  "Return the subject from inherited."
  [inherited]
  (let [subject-referent (:subject-referent inherited)]
    (if (clojure.test/function? subject-referent)
      (subject-referent)
      subject-referent)))

(defn item-referent-given-inherited
  "Return the proper referent for the item, given inherited."
  [item inherited]
  (let [subject-referent (:subject-referent inherited)]
    (if (clojure.test/function? subject-referent)
      (subject-referent item)
      (item-or-exemplar-referent item subject-referent))))

(defn virtual-item-DOM
  "Make a dom for a place that could hold an item, but doesn't.
  inherited must include a :template and a :subject-referent."
  [key adjacent-referent position inherited]
  (assert (not (nil? (:subject-referent inherited))))
  [:div (-> (:selectable-attributes inherited)
            (into-attributes (item-or-content-attributes inherited))
            (into-attributes (select-keys inherited [:selector-category]))
            (into-attributes
             {:class "editable"
              :key key
              :target (copy-alternate-request-to-target
                       {:referent (virtual-referent
                                   (:template inherited)
                                   (subject-referent-given-inherited inherited)
                                   adjacent-referent
                                   :position position
                                   :selector (when (:selector-category
                                                    inherited)
                                               :first-group))
                        :select-pattern (or (:select-pattern inherited)
                                            (conj (:key-prefix inherited)
                                                  [:pattern]))}
                       inherited)}))])

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
  and attributes that the doms for each of the items should have,
  generate DOM for a list of a component for each item.
  If there is more than one item, add stack-class to the stack.
  Any attributes that should apply to the immediate dom will get
  applied to each component, plus to the stack if there is more
  than one component.
  The excludeds can be either a single value to be used for all items,
  or a sequence of values, one per item.
  dom-fn should be item-DOM-R, or item-without-labels-DOM-R, or similar."
  [dom-fn items excludeds direction inherited]
  (let [descriptors (:attributes inherited)
        [local remaining] (split-descriptors-by-currency descriptors)
        inherited (assoc-if-non-empty inherited :attributes remaining)]
    (cond-> (nest-if-multiple-DOM
             (map (fn [item excluded]
                    (add-attributes
                     (item-component dom-fn item excluded inherited) local))
                  items
                  (if (sequential? excludeds) excludeds (repeat excludeds)))
             direction)
      (> (count items) 1)
      (add-attributes local))))

(defn hierarchy-node-DOM-R
  "Create a DOM for a hierarchy node, calling functions to make the pieces.
  For each node, calls
     (node-f node child-doms function-info inherited)
  where child-doms are the results of calling node-f for all the child
  nodes.  Before doing calls for a child, calls
     (child-information-f node function-info inherited)
  It must return the function-info and inherited to be used for the children.
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

(defn hierarchy-node-items-referent
  "Given a hierarchy node or leaf, return a referent to all its descendants."
  [hierarchy-node-or-leaf inherited]
  (union-referent-if-needed
   (map #(item-referent-given-inherited (:item %) inherited)
        (hierarchy-node-descendants hierarchy-node-or-leaf))))

(defn hierarchy-node-parallel-items-referent
  "Given a hierarchy node or leaf, return a referent to all its descendants,
  returning one group per group the subject returns."
  [hierarchy-node-or-leaf inherited]
  (parallel-union-referent
   (map #(item-referent-given-inherited (:item %) inherited)
        (hierarchy-node-descendants hierarchy-node-or-leaf))))

(defn hierarchy-last-item-referent
  "Return a referent to the last item of the hierarchy, if any."
  [hierarchy]
  (when (seq hierarchy)
    (let [last-item (last (hierarchy-node-descendants (last hierarchy)))]
      (item-referent (:item last-item)))))




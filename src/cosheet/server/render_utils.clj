(ns cosheet.server.render-utils
  (:require (cosheet [entity :as entity]
                     [utils :refer [multiset multiset-to-generating-values
                                    replace-in-seqs assoc-if-non-empty]]
                     [debug :refer [simplify-for-print]]
                     [orderable :as orderable]
                     [hiccup-utils
                      :refer [into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq cache]])
            (cosheet.server
             [referent :refer [virtual-referent item->canonical-semantic
                               item->canonical-semantic-R]])))

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
;;;      where <step> is a set that can contain any of
;;;         :label
;;;            Apply to label elements
;;;         :element
;;;            Apply to non-label elements
;;;         :content
;;;            Apply to the content of the item (which may be the overall item)
;;;         :recursive
;;;            Apply to any repetition of the path
;;;         : optional
;;;            May be skipped

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
  successor descriptors"
  [descriptor motion]
  (when (sequential? descriptor)
    (let [step (first descriptor)
          remainder (rest descriptor)
          following (if (empty? (rest remainder))
                      (first remainder)
                      remainder)]
      (concat (when (step motion)
                [(if (:recursive step)
                   (vec (concat [(conj step :optional)] remainder))
                   following)])
              (when (:optional step)
                (advance-along-descriptor following motion))))))

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
  [descriptor]
  (reduce (fn [[current-only others] descriptor]
            (if (map? descriptor)
              [(into-attributes current-only descriptor) others]
              [current-only (conj others descriptor)]))
          [{} []] descriptor))

(defn inherited-attributes
  "Return a map of all attributes specified by inherited for the current dom.
  The attributes are specified as in the comment at
  transform-inherited-attribute."
  [inherited]
  (reduce (fn [attributes descriptor]
            (cond
              (map? descriptor)
              (into-attributes attributes descriptor)
              (every? :optional (butlast descriptor))
              (into-attributes attributes (last descriptor))
              true
              attributes))
          {} (:attributes inherited)))

(defn content-attributes
  "Return the attributes for the current item or content."
  [inherited]
  (inherited-attributes
   (transform-inherited-attributes inherited :content)))

(defn item-or-content-attributes
  "Return the attributes for the current item or content."
  [inherited]
  (inherited-attributes
   {:attributes
    (concat (:attributes inherited)
            (:attributes (transform-inherited-attributes
                          inherited :content)))}))

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
                                   (:subject-referent inherited)
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
  a dom with the given class, and with each of the doms as children."
  [doms & {:keys [class] :or {class "stack"}}]
  (if (= (count doms) 1)
       (first doms)
       (into [:div (if (empty? doms) {} {:class class})] doms)))

(defn item-stack-DOM
  "Given a list of items and a matching list of elements to exclude,
  and attributes that the doms for each of the items should have,
  generate DOM for a vertical list of a component for each item.
  Any attributes that should apply to the immediate dom will get
  applied to each component, plus to the stack if there is more
  than one component.
  The excludeds can be either a single value to be used for all items,
  or a sequence of values, one per item.
  dom-fn should be item-DOM-R, or item-without-labels-DOM-R, or similar."
  [dom-fn items excludeds inherited]
  (let [descriptors (:attributes inherited)
        [local remaining] (split-descriptors-by-currency descriptors)
        inherited (assoc-if-non-empty inherited :attributes remaining)]
    (cond-> (nest-if-multiple-DOM
             (map (fn [item excluded]
                    (add-attributes
                     (item-component dom-fn item excluded inherited) local))
                  items
                  (if (sequential? excludeds) excludeds (repeat excludeds)))
             :class "item-stack")
      (> (count items) 1)
      (add-attributes local))))

;;; TODO: Make a version where function-info and child-info-f are optional.
(defn hierarchy-node-DOM-R
  "Create a DOM for a hierarchy node, calling functions to make the pieces.
  For each node, calls
     (node-f node child-doms function-info inherited)
  where child-doms are the results of calling node-f for all the child
  nodes.  Before doing calls for a child, calls
     (child-information-f node function-info inherited)
  It must return the function-info and inherited to be used for the children.
  Either of the functions may return reporters."
  [node node-f child-info-f function-info inherited]
  (expr-let
      [child-doms (when-let [children (:child-nodes node)]
                    (expr-let [child-info (child-info-f
                                           node function-info inherited)]
                      (let [[child-function-info child-inherited] child-info]
                        (expr-seq map #(hierarchy-node-DOM-R
                                        % node-f child-info-f
                                        child-function-info child-inherited)
                                  children))))]
    (node-f node child-doms function-info inherited)))




(ns cosheet.server.render-utils
  (:require (cosheet [entity :as entity]
                     [utils :refer [multiset multiset-to-generating-values
                                    replace-in-seqs]]
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

(defn add-alternate-to-target
  "Given the map for a target, and inherited information, change the target
  to have a :alternate if the inherited information says to."
  [target inherited]
  (if-let [alternate (:alternate-target inherited)]
    (assoc target :alternate alternate)
    target))

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

(defn transform-inherited-attributes
  "Transform any attributes specified by inherited as appropriate
  for moving down to a :content :label or :element.
  In inherited, :attributes is a vector of descriptors.
  A descriptor is either:
     <attributes>
        A map of attribute->value pairs to add
     (<step>* <attributes>)
        Add the attributes to descendant doms following the sequence of steps, 
        where <step> is a set that can contain any of
           :label
              Apply to label elements
           :element
              Apply to non-label elements
           :content
              Apply to the content of the item (which may be the overall item)
           :recursive
              Apply to any repetition of the path
           : optional
              May be skipped"
  [inherited motion]
  (if-let [attributes (:attributes inherited)]
    (let [revised (mapcat #(advance-along-descriptor % motion) attributes)]
      (if (seq revised)
        (assoc inherited :attributes revised)
        (dissoc inherited :attributes)))
    inherited))

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

(defn virtual-item-DOM
  "Make a dom for a place that could hold an item, but doesn't.
  inherited must include a :template and a :subject-referent."
  [key adjacent-referent position inherited]
  (assert (not (nil? (:subject-referent inherited))))
  [:div (into-attributes
         (into-attributes (into-attributes (:selectable-attributes inherited)
                                           (item-or-content-attributes inherited))
                          (select-keys inherited [:selector-category]))
         {:class "editable"
          :key key
          :target (add-alternate-to-target
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
                   inherited)})])

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

(defn vertical-stack
  "If there is only one item in the doms, return it. Otherwise, return
  a vertical stack of the items, with the given class."
  [doms & {:keys [class] :or {class "stack"}}]
  (if (= (count doms) 1)
       (first doms)
       (into [:div (if (empty? doms) {} {:class class})] doms)))

(defn item-stack-DOM
  "Given a list of items and a matching list of elements to exclude,
  and attributes that the doms for each of the items should have,
  generate DOM for a vertical list of a component for each item.  The
  excludeds, attributes and inherited can be either a single value to
  be used for all items, or a sequence of values, one per item.
  dom-fn should be item-DOM-R, or item-without-labels-DOM-R, or similar."
  [dom-fn items excludeds attributes inherited]
  (vertical-stack
   (map (fn [item excluded attributes inherited]
          (add-attributes (item-component dom-fn item excluded inherited)
                          attributes))
        items
        (if (sequential? excludeds) excludeds (repeat excludeds))
        (if (sequential? attributes) attributes (repeat attributes))
        (if (sequential? inherited) inherited (repeat inherited)))
   :class "item-stack"))




(ns cosheet.server.render-utils
  (:require (cosheet [entity :as entity]
                     [utils :refer [multiset multiset-to-generating-values
                                    replace-in-seqs]]
                     [debug :refer [simplify-for-print]]
                     [orderable :as orderable]
                     [dom-utils
                      :refer [into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq cache]])
            (cosheet.server
             [hierarchy :refer [canonical-info]]
             [referent :refer [virtual-referent]])))

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
                        canonical-elements (expr-seq map canonical-info
                                                     elements)]
                    (multiset-to-generating-values
                     (multiset (map #(canonical-info
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

(defn virtual-item-DOM
  "Make a dom for a place that could hold an item, but doesn't.
  inherited must specify a :selectable-attributes :target,
  into which we will put the subject and template."
  [key adjacent-referent position inherited]
  (assert (not (nil? (:subject-referent inherited))))
  [:div (into-attributes
         (into-attributes (:selectable-attributes inherited)
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
                                           :first-group))}
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


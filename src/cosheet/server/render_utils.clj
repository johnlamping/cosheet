(ns cosheet.server.render-utils
  (:require (cosheet [entity :as entity]
                     [utils :refer [multiset multiset-to-generating-values]]
                     [debug :refer [simplify-for-print current-value]]
                     [orderable :as orderable]
                     [dom-utils
                      :refer [into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq cache]])
            (cosheet.server
             [hierarchy :refer [canonical-info]])))

(defn orderable-comparator
  "Compare two sequences each of whose first element is an orderable."
  [a b]
  (orderable/earlier? (first a) (first b)))

(defn order-items-R
  "Return the items in the proper sort order."
  [items]
  (if (empty? (rest items))
    items
    (expr-let [order-info
               (expr-seq map #(entity/label->content % :order) items)]
      (map second (sort orderable-comparator
                        (map (fn [order item]
                               ;; It is possible for an item not to have
                               ;; order information, especially
                               ;; temporarily while information is being
                               ;; propagated. Tolerate that.
                               (vector (or order orderable/initial) item))
                             order-info items))))))

(defn condition-satisfiers-R
  "Return a sequence of elements of an entity sufficient to make it
  satisfy the condition and nothing extra. The condition must be in
  list form.  and have a nil content.  If part of a condition is not
  satisfied by any element, ignore that part."
  [entity condition]
  (when (not (empty? condition))
    (assert (and (sequential? condition)
                 (nil? (first condition))))
    (expr-let [satisfiers
               (entity/call-with-immutable
                entity
                #(let [elements (entity/elements %)
                       canonical-elements (expr-seq map canonical-info
                                                    elements)]
                   (multiset-to-generating-values
                    (multiset (map canonical-info (rest condition)))
                    canonical-elements elements)))]
      (map #(entity/in-different-store % entity) satisfiers))))

(defn virtual-item-DOM
  "Make a dom for a place that could hold an item, but doesn't.
  inherited must specify one of adjacent-referent or adjacent-groups-referent."
  [key position inherited]
  [:div (into-attributes
         (:selectable-attributes inherited)
         {:class "editable"
          :key key
          :commands {:set-content nil}
          :target (assoc (select-keys inherited
                                      [:template :adjacent-referent
                                       :adjacent-groups-referent])
                         :position position
                         :subject-referent (:subject inherited))})])

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
  (let [key (conj (:parent-key inherited) (:item-id item))
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
(ns cosheet.server.item-render
  (:require (cosheet [canonical :refer [canonical-set-to-list]]
                     [entity :as entity]
                     [query :refer [matching-elements]]
                     [utils :refer [multiset-diff assoc-if-non-empty]]
                     [debug :refer [simplify-for-print]]
                     [hiccup-utils
                      :refer [into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq expr-filter]])
            (cosheet.server
             [referent :refer [item-or-exemplar-referent virtual-referent
                               semantic-elements-R semantic-element?-R ]]
             [hierarchy :refer [hierarchy-node-descendants
                                hierarchy-node-leaves
                                hierarchy-node-parallel-items-referent
                                hierarchy-by-canonical-info
                                item-maps-by-elements-R
                                hierarchy-node-example-elements]]
             [order-utils :refer [order-items-R]]
             [render-utils :refer [virtual-item-DOM item-stack-DOM
                                   copy-alternate-request-to-target
                                   nest-if-multiple-DOM condition-satisfiers-R
                                   transform-inherited-for-children
                                   transform-inherited-for-labels
                                   transform-inherited-attributes
                                   add-inherited-attribute
                                   remove-inherited-attribute
                                   inherited-attributes
                                   content-attributes
                                   hierarchy-node-DOM-R]])))

(declare item-without-labels-DOM-R)

(defn hierarchy-adjacent-virtual-target
  "Given a hierarchy node, generate attributes for the target of
  a command to add an item that would be adjacent to the hierarchy node."
  [hierarchy-node inherited]
  (let [subject-ref (:subject-referent inherited)
        ancestor-props (first (multiset-diff
                               (:cumulative-properties hierarchy-node)
                               (:properties hierarchy-node)))
        conditions (concat (canonical-set-to-list ancestor-props)
                           (rest (:template inherited)))]
    {:referent (virtual-referent
                (when (seq conditions) (list* nil conditions))
                subject-ref
                (hierarchy-node-parallel-items-referent
                 hierarchy-node subject-ref))
     :select-pattern (conj (:key-prefix inherited) [:pattern])}))

(defn add-adjacent-sibling-command
  "Given inherited, and a node from a hierarchy over elements, update
  inherited to have a command to adjacent sibling element."
  [inherited hierarchy-node]
  (-> inherited
      (remove-inherited-attribute :add-sibling)
      (add-inherited-attribute
       [#{:label :optional} #{:content}
        {:add-sibling (copy-alternate-request-to-target
                       (hierarchy-adjacent-virtual-target
                        hierarchy-node inherited)
                       inherited)}])))

(defn hierarchy-leaf-items-DOM
  "Given a hierarchy node with tags as the properties, generate DOM
  for leaves that are items. The leaves of the node may contain an additional
  :exclude-elements field that gives more of the item's elements not to
  show, typically the ones that satisfy the :template of inherited."
  [hierarchy-node inherited]
  (let [leaves (hierarchy-node-leaves hierarchy-node)
        property-list (canonical-set-to-list
                       (:cumulative-properties hierarchy-node))
        inherited-down (if (not (empty? property-list))
                         (assoc inherited :template
                                (concat (or (:template inherited) '(nil))
                                        property-list))
                         inherited)]
    (if (empty? leaves)
      (let [adjacent-item (:item (first (hierarchy-node-descendants
                                         hierarchy-node)))
            adjacent-referent (item-or-exemplar-referent
                               adjacent-item (:subject-referent inherited))
            example-elements (hierarchy-node-example-elements hierarchy-node)
            key (conj (:key-prefix inherited)
                      :example-element
                      (:item-id (first example-elements)))]
        (virtual-item-DOM key adjacent-referent :before inherited-down))
      (let [items (map :item leaves)
            excludeds (map #(concat (:property-elements %)
                                    (:exclude-elements %))
                           leaves)]
        (item-stack-DOM
         item-without-labels-DOM-R items excludeds inherited-down)))))

(declare elements-DOM-R)

(defn label-stack-DOM-R
  "Given a non-empty list of label elements, return a stack of their doms."
  [label-elements inherited]
  (expr-let [ordered-labels (order-items-R label-elements)
             tags (expr-seq map #(condition-satisfiers-R % '(nil :tag))
                            ordered-labels)]
    (item-stack-DOM item-without-labels-DOM-R
                    ordered-labels tags
                    (add-inherited-attribute inherited {:class "tag"}))))

(defn wrapped-element-DOM
  "Given a label stack and an inner dom, make a wrapper for the two."
  [labels-dom inner-dom inherited]
  (-> [:div {:class "wrapped-element tag"}
       labels-dom
       [:div {:class "indent-wrapper tag"} inner-dom]]
      (add-attributes (inherited-attributes inherited))))

(defn non-empty-labels-wrapper-DOM-R
  "Given a dom for an item, not including its labels, and a non-empty 
  list of labels, make a dom that includes the labels wrapping the item.
  Inherited should be half way to the children."
  [inner-dom label-elements inherited]
  (expr-let [stack (label-stack-DOM-R
                    label-elements (transform-inherited-for-labels inherited))]
    (wrapped-element-DOM stack inner-dom inherited)))

(defn labels-wrapper-DOM-R
  "Given a dom for an item, not including its labels, and a list of labels,
  make a dom that includes any necessary labels wrapping the item.
  inherited should be half way to the children."
  [dom label-elements must-show-empty-labels inherited]
  (if (not (empty? label-elements))
    (non-empty-labels-wrapper-DOM-R dom label-elements inherited)
    (let [dom (if (not must-show-empty-labels)
                dom
                [:div {:class "horizontal-tags-element narrow"}
                 (add-attributes
                  (virtual-item-DOM (conj (:key-prefix inherited) :tags)
                                    nil :after
                                    (transform-inherited-for-labels inherited))
                  {:class "tag"})
                 dom])]
      (add-attributes dom (inherited-attributes inherited)))))

(defn condition-elements-DOM-R
  "Generate the dom for a (subset of) a condition, given its elements.
  inherited must be half way to the children.
  must-show-empty-labels gets passed on to elements-DOM-R."
  [elements must-show-empty-labels inherited]
  (expr-let
      [tags (expr-seq map #(matching-elements :tag %) elements)]
    (let [pairs (map vector tags elements)
          labels (seq (keep (fn [[tags elem]] (when (seq tags) elem))
                            pairs))
          non-labels (seq (keep (fn [[tags elem]] (when (empty? tags) elem))
                                pairs))]
      (expr-let [elements-dom
                 (when non-labels
                   (expr-let [inner-dom
                              (elements-DOM-R
                               non-labels must-show-empty-labels nil
                               (transform-inherited-attributes
                                inherited :element))]
                     [:div {:class "item elements-wrapper"} inner-dom]))]
        (cond
          (and labels non-labels)
          (non-empty-labels-wrapper-DOM-R elements-dom labels inherited)
          labels
          (label-stack-DOM-R elements
                             (-> inherited
                                 transform-inherited-for-labels
                                 (add-inherited-attribute {:class "tag"})))
          non-labels
          elements-dom
          true
          (add-attributes
           (virtual-item-DOM (conj (:key-prefix inherited) :virtual)
                             nil :after (assoc inherited :template '(nil :tag)))
           {:class "elements-wrapper"}))))))

(defn tagged-items-properties-DOM-R
  "Given a hierarchy node for tags, Return DOM for example elements
  that give rise to the properties of the node.
  Inherited describes the overall context of the node."
  [hierarchy-node inherited]
  ;; We have to use a parallel union referent here, so that the groupings
  ;; of the subject are preserved if we are part of a header.
  (let [items-referent (hierarchy-node-parallel-items-referent
                        hierarchy-node (:subject-referent inherited))
        example-descendant (first (hierarchy-node-descendants
                                   hierarchy-node))
        tags-key-prefix (conj (:key-prefix inherited) :label)
        inherited-for-tags (-> inherited
                               (transform-inherited-attributes :label)
                               (add-adjacent-sibling-command hierarchy-node)
                               (assoc :key-prefix tags-key-prefix
                                      :template '(nil :tag)
                                      :subject-referent items-referent)) ]
    (expr-let
        [dom (if (empty? (:properties hierarchy-node))
               (virtual-item-DOM (conj tags-key-prefix
                                       ;; Need to make it different from
                                       ;; sibling virtuals.
                                       (:item-id (:item example-descendant))
                                       :virtual)
                                 nil :after inherited-for-tags)
               (label-stack-DOM-R
                (hierarchy-node-example-elements hierarchy-node)
                inherited-for-tags))]
        ;; Even if stacked, we need to mark the stack as "tag" too.
        (add-attributes dom {:class "tag"}))))

(defn tagged-items-one-column-node-DOM-R
  [node child-doms must-show-empty-labels inherited]
  (expr-let [properties-dom (when (or (seq (:properties node))
                                      must-show-empty-labels)
                              (tagged-items-properties-DOM-R
                               node inherited))]
    (let [descendants-doms (nest-if-multiple-DOM
                            (if (empty? (hierarchy-node-leaves node))
                              child-doms
                              (let [inherited (add-adjacent-sibling-command
                                               inherited node)]
                                (cons (hierarchy-leaf-items-DOM node inherited)
                                      child-doms))))]
      (if (empty? (:properties node))
        (if must-show-empty-labels
          [:div {:class "horizontal-tags-element narrow"}
           properties-dom descendants-doms]
          descendants-doms)
        [:div {:class "wrapped-element tag"}
         ;; If the properties-dom is an item-stack, we need to mark it as tag.
         (add-attributes properties-dom {:class "tag"})
         [:div {:class "indent-wrapper"} descendants-doms]]))))

(defn tagged-items-one-column-DOM-R
  [hierarchy inherited]
  (expr-let [doms (expr-seq map #(hierarchy-node-DOM-R
                                  % tagged-items-one-column-node-DOM-R
                                  (fn [node must-show-empty-labels inherited]
                                    [false inherited])
                                  true inherited)
                            hierarchy)]
    (nest-if-multiple-DOM doms)))

;;; Wrappers are used when one logical entity is broken into several
;;; adjacent divs. This typically means that the first and last of the
;;; divs need to be styled differently from the others. A wrapper will
;;; be called with the div to wrap and with whether it is the first
;;; and/or last div of the logical entity.

(defn nest-wrapper
  "Return a wrapper that nests one level more deeply. This is used
  when a logical entity has been broken into several pieces and one of
  those pieces has been further subdivided.
  We are given an outer wrapper and the position of our sub-entity in
  the larger entity. We may get further subdivided. So we return a new
  wrapper that itself expects to be called several times, each time
  given the position of the call with respect to all calls of the
  sub-entity. For each call, call inner-wrapper with the dom and the
  position of the call with respect to all calls of our logical cell,
  then call the outer wrapper with the revised dom and the position
  with respect to the all the doms of the larger entity."
  [outer-wrapper inner-wrapper generated-first generated-last]
  (fn [body called-first called-last]
    (outer-wrapper (inner-wrapper body called-first called-last)
                   (and generated-first called-first)
                   (and generated-last called-last))))

(defn nest-wrappers-for-children
  "Generate a sequence of nested wrappers, one for each child,
  with the last one knowing that it is the last."
  [outer-wrapper inner-wrapper children]
  (let [middle-wrapper (nest-wrapper outer-wrapper inner-wrapper false false)
        last-wrapper (nest-wrapper outer-wrapper inner-wrapper false true)]
    (concat (repeat (- (count children) 1) middle-wrapper)
            [last-wrapper])))

(defn identity-wrapper
  "Return a wrapper that does no wrapping -- just returns the body."
  [body called-first called-last]
  body)

(defn horizontal-tag-wrapper
  "Return a modifier for a horizontal tag dom that is logically part of
  a larger entity."
  [body is-first is-last]
  [:div {:class (cond-> "tag horizontal-header"
                  is-first (str " top-border")
                  (not is-first) (str " indent")
                  is-last (str " bottom-border"))}
   body])

(defn horizontal-value-wrapper
  "Return a modifier for a value in a horizontal tag layout that is 
  logically part of a larger entity."
  [body is-first is-last]
  (if (and is-last (not is-first))
    [:div {:class "horizontal-value-last"} body]
    body))

(declare tagged-items-two-column-subtree-DOMs-R)

(defn tagged-items-two-column-children-DOMs-R
  "Return a seq of doms for the children of the hierarchy node,
  as two columns."
  [hierarchy-node header-wrapper member-wrapper inherited]
  (let [children (:child-nodes hierarchy-node)]
    (when children
      (expr-let [child-lists
                 (expr-seq
                  map (fn [child-node header-wrapper member-wrapper]
                        (tagged-items-two-column-subtree-DOMs-R
                         child-node header-wrapper member-wrapper
                         inherited))
                  children
                  (nest-wrappers-for-children
                   header-wrapper horizontal-tag-wrapper children)
                  (nest-wrappers-for-children
                   member-wrapper horizontal-value-wrapper children))]
        (apply concat child-lists)))))

(defn tagged-items-two-column-subtree-DOMs-R
  "Return a sequence of DOMs for the given hierarchy node and its descendants,
  as two columns."
  [hierarchy-node header-wrapper member-wrapper inherited]
  (let [tags-inherited (update inherited :width #(* % 0.25))
        items-inherited (update
                         (add-adjacent-sibling-command inherited hierarchy-node)
                         :width #(* % 0.6875))
        leaves-dom (hierarchy-leaf-items-DOM hierarchy-node items-inherited)
        no-children (empty? (:child-nodes hierarchy-node))]
    (expr-let [properties-dom (tagged-items-properties-DOM-R
                               hierarchy-node tags-inherited) 
               child-doms (tagged-items-two-column-children-DOMs-R
                           hierarchy-node header-wrapper member-wrapper
                           inherited)]
      (cons [:div {:class "horizontal-tags-element wide"}
             (header-wrapper properties-dom true no-children)
             (member-wrapper leaves-dom true no-children)]
            child-doms))))

(defn tagged-items-two-column-DOM-R
  [hierarchy inherited]
  (expr-let [dom-lists (expr-seq
                        map #(tagged-items-two-column-subtree-DOMs-R
                              %
                              horizontal-tag-wrapper horizontal-value-wrapper
                              inherited)
                        hierarchy)]
    (nest-if-multiple-DOM (apply concat dom-lists))))

(defn elements-DOM-R
  "Make a dom for a stack of elements.
   If implied-template is non-nil, don't show sub-elements implied by it.
   If must-show-empty-labels is true, show a space for labels, even if
   there are none. If, additionally, it is :wide, show them with substantial
   space, if there is any space available."
  [elements must-show-empty-labels implied-template inherited]
  (expr-let
      [ordered-elements (order-items-R elements)
       all-labels (expr-filter semantic-element?-R
                               (expr-seq map #(matching-elements '(nil :tag) %)
                                         ordered-elements))
       excludeds (expr-seq map #(when implied-template
                                  (condition-satisfiers-R % implied-template))
                           ordered-elements)]
    (let [labels (map (fn [all minus]
                        (seq (clojure.set/difference (set all) (set minus))))
                      all-labels excludeds)
          no-labels (every? empty? labels)]
      (if (and no-labels (not must-show-empty-labels))
        (item-stack-DOM
         item-without-labels-DOM-R ordered-elements excludeds inherited)
        (expr-let [item-maps (item-maps-by-elements-R ordered-elements labels)
                   augmented (map (fn [item-map excluded]
                                    (assoc item-map :exclude-elements excluded))
                                  item-maps excludeds)
                   hierarchy (hierarchy-by-canonical-info augmented)]
          (if (or (< (:width inherited) 1.0)
                  (and no-labels (not (= must-show-empty-labels :wide))))
            (tagged-items-one-column-DOM-R hierarchy inherited)
            (tagged-items-two-column-DOM-R hierarchy inherited)))))))

(defn item-content-DOM
  "Make dom for the content part of an item."
  [item-referent content inherited]
  ;; We don't currently handle items as content. That would need
  ;; more interaction and UI design work to deal with the distinction
  ;; between elements of an item and elements on its content.
  (assert (entity/atom? content))
  (let [anything (#{'anything 'anything-immutable} content)]
    ;; Any attributes we inherit take precedence over basic commands,
    ;; but nothing else.
    [:div (into-attributes
           (into-attributes (select-keys inherited [:selector-category])
                            (content-attributes inherited))
           {:class (cond-> "content-text editable"
                     (= content 'anything-immutable) (str " immutable"))
            :target (copy-alternate-request-to-target
                     (assoc (select-keys inherited [:template])
                            :referent item-referent)
                     inherited)})
     (if anything "\u00A0..." (str content))]))

(defn item-content-and-non-label-elements-DOM-R
  "Make a dom for a content and a group of non-label elements,
  all of the same item. Inherited should be half way to the children."
  [content elements inherited]
  (let [content-dom (add-attributes
                     (item-content-DOM
                      (:subject-referent inherited) content inherited)
                     ;; Give it a unique key.
                     ;; This will be overridden to the item's key
                     ;; if the item has nothing but this content.
                     {:key (conj (:key-prefix inherited) :content)})]
    (if (empty? elements)
      (add-attributes (or content-dom [:div]) {:class "item"})
      (let [inherited-down
            (-> (transform-inherited-attributes inherited :element)
                (update :priority inc)
                (assoc :template '(nil)))]
        (expr-let [elements-dom (elements-DOM-R
                                 elements true nil inherited-down)]
          (if content-dom
            [:div {:class "item with-elements"}
             content-dom elements-dom]
            [:div {:class "item elements-wrapper"}
             elements-dom]))))))

(defn item-content-and-elements-DOM-R
  "Make a dom for a content and a group of elements, all of the same item.
  Inherited should be half way to the children."
  [content elements inherited]
  (expr-let [tags (expr-seq map #(matching-elements :tag %) elements)]
    (let [labels (seq (mapcat (fn [tags element] (when (seq tags) [element]))
                              tags elements))
          non-labels (seq (mapcat (fn [tags element] (when (empty? tags)
                                                       [element]))
                                  tags elements))]
      (expr-let [content-and-elements-dom
                 (item-content-and-non-label-elements-DOM-R
                  content non-labels inherited)]
        (if labels
          (labels-wrapper-DOM-R
           content-and-elements-dom labels false inherited)
          content-and-elements-dom)))))

(defn item-without-labels-DOM-R
  "Make a dom for an item or exemplar for a group of items,
   given that any of its labels are in excluded-elements.
   Either the referent for the item/group must be provided,
   or inherited must contain :subject-referent.
   We only record the key on the content, not the whole item
   (unless it is just the content)."
  ([item excluded-elements inherited]
   (let [referent (item-or-exemplar-referent
                   item (:subject-referent inherited))]
     (item-without-labels-DOM-R item referent excluded-elements inherited)))
  ([item referent excluded-elements inherited]
   (println
    "Generating DOM for"
    (simplify-for-print (conj (:key-prefix inherited) (:item-id item))))
   (expr-let [content (entity/content item)
              elements (semantic-elements-R item)]
     (item-content-and-non-label-elements-DOM-R
      content (remove (set excluded-elements) elements)
      (transform-inherited-for-children
       inherited (conj (:key-prefix inherited) (:item-id item))
       referent)))))

(defn item-DOM-impl-R
   "Make a dom for an item or exemplar of a group of items.
   If the item is a tag, the caller is responsible for tag formatting."
  [item referent excluded-elements must-show-empty-label inherited]
  (expr-let [labels (entity/label->elements item :tag)]
    (let [labels (remove (set excluded-elements) labels)
          excluded (concat labels excluded-elements)
          inherited-for-children (transform-inherited-for-children
                                  inherited
                                  (conj (:key-prefix inherited) (:item-id item))
                                  referent)]
      (expr-let [dom (item-without-labels-DOM-R
                      item referent excluded inherited)]
        (labels-wrapper-DOM-R
         dom labels must-show-empty-label
         (add-inherited-attribute
          inherited-for-children
          [#{:label} #{:content} {:expand {:referent referent}}]))))))

(defn item-DOM-R
   "Make a dom for an item or exemplar for a group of items.
   Either the referent for the item/group must be provided,
   or inherited must contain :subject-referent.
   If the item is a tag, the caller is responsible for tag formatting."
  ([item excluded-elements inherited]
   (let [referent (item-or-exemplar-referent
                   item (:subject-referent inherited))]
     (item-DOM-impl-R item referent excluded-elements false inherited)))
  ([item referent excluded-elements inherited]
   (item-DOM-impl-R item referent excluded-elements false inherited)))

(defn must-show-label-item-DOM-R
   "Make a dom for an item or exemplar for a group of items, always showing
   at least one label, empty, if necessary. 
   Either the referent for the item/group must be provided,
   or inherited must contain :subject-referent.
   If the item is a tag, the caller is responsible for tag formatting."
  ([item excluded-elements inherited]
   (let [referent (item-or-exemplar-referent
                   item (:subject-referent inherited))]
     (item-DOM-impl-R item referent excluded-elements true inherited)))
  ([item referent excluded-elements inherited]
   (item-DOM-impl-R item referent excluded-elements true inherited)))

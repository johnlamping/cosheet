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
                                hierarchy-node-members
                                hierarchy-node-items-referent
                                hierarchy-node-parallel-items-referent
                                hierarchy-by-canonical-info
                                item-maps-by-elements
                                hierarchy-node-example-elements]]
             [order-utils :refer [order-items-R]]
             [render-utils :refer [virtual-item-DOM add-alternate-to-target
                                   vertical-stack item-stack-DOM
                                   condition-satisfiers-R
                                   transform-inherited-attributes
                                   inherited-attributes
                                   content-attributes]])))

(def item-without-labels-DOM-R)

(defn hierarchy-add-adjacent-target
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
  "Given a node from a hierarchy over elements and inherited, update
  inherited to add an element adjacent to the sibling."
  [inherited hierarchy-node]
  (update
     inherited :attributes
     #(let [filtered (vec (keep (fn [description]
                                  (let [attributes (if (map? description)
                                                     description
                                                     (last description))]
                                    (if (:add-sibling attributes)
                                      (let [smaller (dissoc attributes
                                                            :add-sibling)]
                                        (when (not-empty smaller)
                                          (if (map? description)
                                            smaller
                                            (conj (vec (butlast description))
                                                  smaller))))
                                      description)))
                                %))]
           (conj filtered
                 [#{:label :optional} #{:content}
                  {:add-sibling (add-alternate-to-target
                                 (hierarchy-add-adjacent-target
                                  hierarchy-node inherited)
                                 inherited)}]))))

(defn hierarchy-members-DOM
  "Given a hierarchy node with tags as the properties, generate DOM
  for the members. The members of the node may contain an additional
  :exclude-elements field that gives more of their elements not to
  show, typically the ones that satisfy the :template of inherited."
  [hierarchy-node inherited]
  (let [members (hierarchy-node-members hierarchy-node)
        property-list (canonical-set-to-list
                       (:cumulative-properties hierarchy-node))
        inherited-down (if (not (empty? property-list))
                         (assoc inherited :template
                                (concat (or (:template inherited) '(nil))
                                        property-list))
                         inherited)]
    (if (empty? members)
      (let [adjacent-item (:item (first (hierarchy-node-descendants
                                         hierarchy-node)))
            adjacent-referent (item-or-exemplar-referent
                               adjacent-item (:subject-referent inherited))
            example-elements (hierarchy-node-example-elements hierarchy-node)
            key (conj (:key-prefix inherited)
                      :example-element
                      (:item-id (first example-elements)))]
        (virtual-item-DOM key adjacent-referent :before inherited-down))
      (let [items (map :item members)
            excludeds (map #(concat (:property-elements %)
                                    (:exclude-elements %))
                           members)]
        (item-stack-DOM
         item-without-labels-DOM-R items excludeds {} inherited-down)))))

(defn hierarchy-properties-DOM-R
  "Return DOM for example elements that give rise to the properties
  of one hierarchy node. Inherited describes the context of the properties.
  dom-fn should be item-DOM-R, or item-without-labels-DOM-R, or similar."
  [dom-fn hierarchy-node attributes inherited]
  (let [example-elements (hierarchy-node-example-elements hierarchy-node)]
    (expr-let [ordered-elements (order-items-R example-elements)
               satisfiers (expr-seq
                           map #(condition-satisfiers-R % (:template inherited))
                           ordered-elements)]
      (item-stack-DOM
       dom-fn ordered-elements satisfiers attributes inherited))))

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
               (hierarchy-properties-DOM-R
                item-without-labels-DOM-R
                hierarchy-node {:class "tag"} inherited-for-tags))]
        ;; Even if stacked, we need to mark the stack as "tag" too.
        (add-attributes dom {:class "tag"}))))

(def tagged-items-one-column-subtree-DOM-R)

(defn tagged-items-one-column-descendants-DOM-R
  "Return DOM for all descendants of the hierarchy node, as a single column."
  [hierarchy-node inherited]
  (let [members (hierarchy-node-members hierarchy-node)
        nested-inherited (add-adjacent-sibling-command inherited hierarchy-node)
        items-dom (when (not (empty? members))
                    (hierarchy-members-DOM hierarchy-node nested-inherited))]
    (expr-let
        [child-doms (when (:child-nodes hierarchy-node)
                      (expr-seq map #(tagged-items-one-column-subtree-DOM-R
                                      % false nested-inherited)
                                (:child-nodes hierarchy-node)))]
      (vertical-stack (if items-dom (cons items-dom child-doms) child-doms)))))

(defn tagged-items-one-column-subtree-DOM-R
  "Return DOM for the given hierarchy node and its descendants,
  as a single column."
  [hierarchy-node must-show-empty-labels inherited]
  (expr-let
      [descendants-dom (tagged-items-one-column-descendants-DOM-R
                        hierarchy-node inherited)]
    (if (empty? (:properties hierarchy-node))
        (if must-show-empty-labels
          (expr-let [properties-dom (tagged-items-properties-DOM-R
                                     hierarchy-node inherited)]
            [:div {:class "horizontal-tags-element narrow"}
             properties-dom descendants-dom])
          descendants-dom)
        (expr-let [properties-dom (tagged-items-properties-DOM-R
                                   hierarchy-node inherited)]
          [:div {:class "wrapped-element tag"}
           ;; If the properties-dom is an item-stack, we need to mark it as tag.
           (add-attributes properties-dom {:class "tag"})
           [:div {:class "indent-wrapper"} descendants-dom]]))))

(defn tagged-items-one-column-DOM-R
  [hierarchy inherited]
  (expr-let [doms (expr-seq map #(tagged-items-one-column-subtree-DOM-R
                                  % true inherited)
                            hierarchy)]
    (vertical-stack doms)))

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

(def tagged-items-two-column-subtree-DOMs-R)

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
        members-dom (hierarchy-members-DOM hierarchy-node items-inherited)
        no-children (empty? (:child-nodes hierarchy-node))]
    (expr-let [properties-dom (tagged-items-properties-DOM-R
                               hierarchy-node tags-inherited) 
               child-doms (tagged-items-two-column-children-DOMs-R
                           hierarchy-node header-wrapper member-wrapper
                           inherited)]
      (cons [:div {:class "horizontal-tags-element wide"}
             (header-wrapper properties-dom true no-children)
             (member-wrapper members-dom true no-children)]
            child-doms))))

(defn tagged-items-two-column-DOM-R
  [hierarchy inherited]
  (expr-let [dom-lists (expr-seq
                        map #(tagged-items-two-column-subtree-DOMs-R
                              %
                              horizontal-tag-wrapper horizontal-value-wrapper
                              inherited)
                        hierarchy)]
    (vertical-stack (apply concat dom-lists))))

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
         item-without-labels-DOM-R ordered-elements excludeds {} inherited)
        (expr-let [item-maps (item-maps-by-elements ordered-elements labels)
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
            :target (add-alternate-to-target
                     (assoc (select-keys inherited [:template])
                            :referent item-referent)
                     inherited)})
     (if anything "\u00A0..." (str content))]))

(defn item-content-and-non-label-elements-DOM-R
  "Make a dom for a content and a group of non-label elements,
  all of the same item."
  [item-key item-referent content elements inherited]
  (let [content-dom (add-attributes
                     (item-content-DOM item-referent content inherited)
                     ;; Give it a unique key.
                     ;; This will be overridden to the item's key
                     ;; if the item has nothing but this content.
                     {:key (conj item-key :content)})]
    (if (empty? elements)
      (add-attributes (or content-dom [:div]) {:class "item"})
      (let [inherited-down
            (-> (transform-inherited-attributes inherited :element)
                (update :priority inc)
                (assoc :key-prefix item-key
                       :subject-referent item-referent
                       :template '(nil)))]
        (expr-let [elements-dom (elements-DOM-R
                                 elements true nil inherited-down)]
          (if content-dom
            [:div {:class "item with-elements"}
             content-dom elements-dom]
            [:div {:class "item elements-wrapper"}
             elements-dom]))))))

(defn label-wrapper-DOM-R
  "Given a dom for an item, not including its labels, and a list of labels,
  make a dom that includes any necessary labels wrapping the item. The
  :key-prefix of inherited must give a unique prefix for the labels."
  [dom item-referent label-elements must-show-empty-labels inherited]
  (expr-let
      [dom
       (if (and (empty? label-elements) (not must-show-empty-labels))
         dom
         (let [inherited-for-tags (-> inherited
                                      (assoc :template '(nil :tag)
                                             :subject-referent item-referent))]
           (if (empty? label-elements)
             [:div {:class "horizontal-tags-element narrow"}
              (add-attributes
               (virtual-item-DOM (conj (:key-prefix inherited) :tags)
                                 nil :after inherited-for-tags)
               {:class "tag"})
              dom]
             (expr-let [ordered-labels (order-items-R label-elements)
                        tags (expr-seq
                              map #(condition-satisfiers-R % '(nil :tag))
                              ordered-labels)]
               ;; We need "tag" in the class to make any margin
               ;; after the tags also have tag coloring.
               [:div {:class "wrapped-element tag"}
                (add-attributes
                 (item-stack-DOM item-without-labels-DOM-R
                                 ordered-labels tags {:class "tag"}
                                 inherited-for-tags)
                 {:class "tag"})
                [:div {:class "indent-wrapper tag"} dom]]))))]
    (add-attributes dom (inherited-attributes inherited))))

(defn item-content-and-elements-DOM-R
  "Make a dom for a content and a group of elements, all of the same item."
  [item-key item-referent content elements inherited]
  (expr-let [tags (expr-seq map #(matching-elements :tag %) elements)]
    (let [labels (seq (mapcat (fn [tags element] (when (seq tags) [element]))
                              tags elements))
          non-labels (seq (mapcat (fn [tags element] (when (empty? tags)
                                                       [element]))
                                  tags elements))]
      (expr-let [content-and-elements-dom
                 (item-content-and-non-label-elements-DOM-R
                  item-key item-referent
                  content non-labels inherited)]
        (if labels
          (let [inherited-for-labels (-> inherited
                                         (transform-inherited-attributes :label)
                                         (assoc :key-prefix item-key))]
            (label-wrapper-DOM-R
             content-and-elements-dom
             item-referent labels false inherited-for-labels))
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
      (conj (:key-prefix inherited) (:item-id item))
      referent content (remove (set excluded-elements) elements)
      inherited))))

(defn condition-elements-DOM-R
  "Generate the dom for a (subset of) a condition, given its elements.
  :key-prefix of inherited must give a prefix for the doms corresponding to
  the elements. must-show-empty-labels gets passed on to elements-DOM-R.
  If content-item is present, also show its content at the content position
  with the given added attributes."
  [elements must-show-empty-labels inherited]
  (expr-let
      [tags (expr-seq map #(matching-elements :tag %) elements)]
    (let [labels (seq (mapcat (fn [tags element] (when (seq tags) [element]))
                              tags elements))
          non-labels (seq (mapcat (fn [tags element] (when (empty? tags)
                                                       [element]))
                                  tags elements))
          subject-referent (:subject-referent inherited)
          inherited-for-labels (transform-inherited-attributes
                                inherited :label)]
      (expr-let [elements-dom
                 (when non-labels
                   (expr-let [elements-dom
                              (elements-DOM-R
                               non-labels must-show-empty-labels
                               nil inherited)]
                     [:div {:class "item elements-wrapper"} elements-dom]))]
        (cond
          (and labels elements-dom)
          (label-wrapper-DOM-R
           elements-dom subject-referent labels false inherited-for-labels)
          labels
          (expr-let [ordered-elements (order-items-R elements)
                     excludeds (expr-seq map #(matching-elements :tag %)
                                         ordered-elements)]
            (cond-> (item-stack-DOM item-without-labels-DOM-R
                                    ordered-elements excludeds
                                    {:class "tag"} inherited-for-labels)
              (> (count ordered-elements) 1) (add-attributes {:class "tag"})))
          non-labels
          elements-dom
          true
          (add-attributes
           (virtual-item-DOM (conj (:key-prefix inherited) :virtual)
                             nil :after (assoc inherited :template '(nil :tag)))
           {:class "elements-wrapper"}))))))

(defn item-DOM-impl-R
   "Make a dom for an item or exemplar of a group of items.
   If the item is a tag, the caller is responsible for tag formatting."
  [item referent excluded-elements must-show-empty-label inherited]
  (expr-let [labels (entity/label->elements item :tag)]
    (let [labels (remove (set excluded-elements) labels)
          excluded (concat labels excluded-elements)]
      (expr-let [dom (item-without-labels-DOM-R
                      item referent excluded inherited)]
        (label-wrapper-DOM-R
         dom referent labels must-show-empty-label
         (-> (transform-inherited-attributes inherited :label)
             (update :attributes
                     #(conj (or % [])
                            [#{:content} {:expand {:referent referent}}]))
             (update :key-prefix #(conj % (:item-id item) ))))))))

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

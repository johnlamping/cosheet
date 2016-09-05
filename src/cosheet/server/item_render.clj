(ns cosheet.server.item-render
  (:require (cosheet [entity :as entity]
                     [query :refer [matching-elements]]
                     [utils :refer [multiset-diff]]
                     [debug :refer [simplify-for-print current-value]]
                     [dom-utils
                      :refer [into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq cache]])
            (cosheet.server
             [referent :refer [item-or-exemplar-referent semantic-elements-R]]
             [hierarchy :refer [canonical-set-to-list hierarchy-node-descendants
                                hierarchy-node-members
                                hierarchy-node-items-referent
                                hierarchy-by-canonical-info
                                item-maps-by-elements
                                hierarchy-node-example-elements]]
             [render-utils :refer [virtual-item-DOM
                                   vertical-stack item-stack-DOM
                                   order-items-R condition-satisfiers-R]])))

(def item-without-labels-DOM-R)

(defn item-target
  "Return a target for the given item."
  [item-referent inherited]
  (let [template (:template inherited)]
    (cond-> {:item-referent item-referent}
      template (assoc :template template))))

(defn hierarchy-add-adjacent-target
  "Given a hierarchy node, generate attributes for the target of
  a command to add an item that would be adjacent to the hierarchy node."
  [hierarchy-node inherited]
  (let [subject (:subject inherited)
        ancestor-props (first (multiset-diff
                               (:cumulative-properties hierarchy-node)
                               (:properties hierarchy-node)))
        conditions (concat (canonical-set-to-list ancestor-props)
                           (rest (:template inherited)))]
   (cond->
       {:subject-referent subject
        :adjacent-groups-referent (hierarchy-node-items-referent
                                   hierarchy-node subject)}
     (not (empty? conditions))
     (assoc :template (list* nil conditions)))))

(defn add-adjacent-sibling-command
  "Given a node from a hierarchy over elements and inherited, update
  inherited to add an element adjacent to the sibling, if that would be
  different than just adding a twin."
  [hierarchy-node inherited]
  (if (empty? (:properties hierarchy-node))
    inherited
    (update-in
     inherited [:selectable-attributes]
     #(into-attributes
       % {:commands {:add-sibling {:select-pattern (conj (:parent-key inherited)
                                                         [:pattern])}}
          :sibling (hierarchy-add-adjacent-target hierarchy-node inherited)}))))

(defn hierarchy-members-DOM
  "Given a hierarchy node with tags as the properties, generate DOM
  for the members. The members of the node may contain an additional
  :exclude-elements field that gives more of their elements not to
  show, typically the ones that satisfy the :template of inherited."
  [hierarchy-node inherited]
  (let [members (hierarchy-node-members hierarchy-node)
        property-list (canonical-set-to-list
                       (:cumulative-properties hierarchy-node))
        template (:template inherited)
        inherited-down (if (or template (not (empty? property-list)))
                         (assoc inherited :template
                                (concat (or template '(nil)) property-list))
                         inherited)]
    (if (empty? members)
      (let [subject (:subject inherited)
            adjacent-item (:item (first (hierarchy-node-descendants
                                         hierarchy-node)))
            adjacent-referent (item-or-exemplar-referent adjacent-item subject)
            example-elements (hierarchy-node-example-elements hierarchy-node)
            key (conj (:parent-key inherited)
                      :example-element
                      (:item-id (first example-elements)))]
        (virtual-item-DOM key :before (assoc inherited-down :adjacent-referent
                                             adjacent-referent)))
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
  (let [items-referent (hierarchy-node-items-referent
                        hierarchy-node (:subject inherited))
        example-descendant (first (hierarchy-node-descendants
                                   hierarchy-node))
        tags-parent-key (conj (:parent-key inherited)
                              (:item-id (:item example-descendant))
                              :outside)
        inherited-for-tags (assoc (add-adjacent-sibling-command
                                   hierarchy-node inherited)
                                  :parent-key tags-parent-key
                                  :template '(nil :tag)
                                  :subject items-referent)]
    (expr-let
        [dom (if (empty? (:properties hierarchy-node))
               (virtual-item-DOM (conj tags-parent-key :tags) :after
                                 (assoc inherited-for-tags
                                        :adjacent-referent items-referent))
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
        nested-inherited (add-adjacent-sibling-command hierarchy-node inherited)
        items-dom (when (not (empty? members))
                    (hierarchy-members-DOM hierarchy-node nested-inherited))]
    (expr-let
        [child-doms (when (:children hierarchy-node)
                      (expr-seq map #(tagged-items-one-column-subtree-DOM-R
                                      % false nested-inherited)
                                (:children hierarchy-node)))]
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

;;; Wrappers are used when one logical cell is broken into several
;;; adjacent divs. This typically means that the first and last of the
;;; divs need to be styled differently from the others. A wrapper will
;;; be called with the div to wrap and with whether it is the first
;;; and/or last div of the logical cell.

(defn nest-wrapper
  "Return a wrapper that nests one level more deeply. This is used
  when a logical cell has been broken into several pieces and one of those
  pieces has been further subdivided.
  We are given an outer wrapper and the position of our sub-part in
  the logical cell. We may get further subdivided. So we return a new wrapper
  that itself expects to be called several times, each time given the position
  of the call with respect to all calls of the subcell. For each call,
  call position->class with the position of the call with respect to all calls
  of our logical cell, and call the outer wrapper with the same information."
  [outer-wrapper position->class generated-first generated-last]
  (fn [body called-first called-last]
    (let [first (and generated-first called-first)
          last (and generated-last called-last)]
      (outer-wrapper
       [:div {:class (position->class first last)} body]
       first last))))

(defn identity-wrapper
  "Return a wrapper that does no wrapping -- just returns the body."
  [body called-first called-last]
  body)

(defn nest-horizontal-tag-wrapper
  [outer-wrapper generated-first generated-last]
  (nest-wrapper outer-wrapper
                (fn [first last] (cond-> "tag horizontal-header"
                                   first (str " top-border")
                                   (not first) (str " indent")
                                   last (str " bottom-border")))
                generated-first generated-last))

(def tagged-items-two-column-subtree-DOMs-R)

(defn tagged-items-two-column-children-DOMs-R
  "Return a seq of doms for the children of the hierarchy node,
  as two columns."
  [hierarchy-node header-wrapper inherited]
  (let [children (:children hierarchy-node)]
    (when children
      (let [wrapper (nest-horizontal-tag-wrapper header-wrapper false false)
            last-wrapper (nest-horizontal-tag-wrapper header-wrapper false true)
            wrappers (concat (map (constantly wrapper) (butlast children))
                             [last-wrapper])]
        (expr-let [child-lists
                   (expr-seq map (fn [child-node wrapper]
                                   (tagged-items-two-column-subtree-DOMs-R
                                    child-node wrapper inherited))
                             children wrappers)]
          (apply concat child-lists))))))

(defn tagged-items-two-column-subtree-DOMs-R
  "Return a sequence of DOMs for the given hierarchy node and its descendants,
  as two columns."
  [hierarchy-node header-wrapper inherited]
  (let [tags-inherited (update-in inherited [:width] #(* % 0.25))
        items-inherited (update-in
                         (add-adjacent-sibling-command hierarchy-node inherited)
                         [:width] #(* % 0.6875))
        members-dom (hierarchy-members-DOM hierarchy-node items-inherited)
        no-children (empty? (:children hierarchy-node))
        wrapper (nest-horizontal-tag-wrapper header-wrapper
                                             true no-children)]
    (expr-let [properties-dom (tagged-items-properties-DOM-R
                               hierarchy-node tags-inherited) 
               child-doms (tagged-items-two-column-children-DOMs-R
                           hierarchy-node header-wrapper inherited)]
      (cons [:div {:class "horizontal-tags-element wide"}
             (wrapper properties-dom true no-children) members-dom]
            child-doms))))

(defn tagged-items-two-column-DOM-R
  [hierarchy inherited]
  (expr-let [dom-lists (expr-seq map #(tagged-items-two-column-subtree-DOMs-R
                                       % identity-wrapper inherited)
                            hierarchy)]
    (vertical-stack (apply concat dom-lists))))

(defn elements-DOM-R
  "Make a dom for a stack of elements.
   Don't show elements of the elements that are implied by the template,
   if any."
  [elements must-show-empty-labels inherited]
  (expr-let
      [ordered-elements (order-items-R elements)
       all-labels (expr-seq map #(matching-elements '(nil :tag) %)
                            ordered-elements)
       excludeds (expr-seq map #(condition-satisfiers-R % (:template inherited))
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
          (if (or (< (:width inherited) 1.0) no-labels)
            (tagged-items-one-column-DOM-R hierarchy inherited)
            (tagged-items-two-column-DOM-R hierarchy inherited)))))))

(defn item-content-DOM
  "Make dom for the content part of an item."
  [item item-referent content inherited]
  ;; We don't currently handle items as content. That would need
  ;; more interaction and UI design work to deal with the distinction
  ;; between elements of an item and elements on its content.
  (assert (entity/atom? content))
  (let [is-placeholder (and (symbol? content)
                            (= (subs (str content) 0 3) "???"))
        selector-map (when (:selector inherited) {:selector true})]
    ;; Any attributes we inherit take precedence over basic commands,
    ;; but nothing else.
    [:div (into-attributes
           (into-attributes {:commands {:set-content nil
                                        :delete nil
                                        :add-element selector-map
                                        :add-twin selector-map
                                        :expand nil}}
                            (:selectable-attributes inherited))
           {:class (cond-> "content-text editable"
                     is-placeholder (str " placeholder")
                     (= content 'anything) (str " anything"))
            :target (item-target item-referent inherited)})
     (cond (#{'anything 'anything-immutable} content) "..."
           is-placeholder "???"                     
           true (str content))]))

(defn item-content-and-elements-DOM-R
  "Make a dom for a content and a group of non-label elements,
  all of the same item."
  [item item-referent content elements inherited]
  (let [key (conj (:parent-key inherited) (:item-id item))
        content-dom (add-attributes
                     (item-content-DOM item item-referent content inherited)
                     ;; Give it a unique key.
                     ;; This will be overridden to the item's key
                     ;; if the item has nothing but this content.
                     {:key (conj key :content)})]
    (if (empty? elements)
      (add-attributes content-dom {:class "item"})
      (let [inherited-down
            (-> inherited
                (update-in [:priority] inc)
                (assoc :parent-key key
                       :subject item-referent)
                (dissoc :template :selectable-attributes))]
        (expr-let [elements-dom (elements-DOM-R
                                 elements true inherited-down)]
          [:div {:class "item with-elements"}
           content-dom elements-dom])))))

(defn label-wrapper-DOM-R
  "Given a dom for an item, not including its labels, and a list of labels,
  make a dom that includes any necessary labels wrapping the item."
  [dom item item-referent label-elements must-show-empty-labels inherited]
  (if (and (empty? label-elements) (not must-show-empty-labels))
    dom
    (let [key (conj (:parent-key inherited) (:item-id item))
          inherited-for-tags (-> inherited
                                 (assoc :template '(nil :tag)
                                        :subject item-referent
                                        :parent-key key)
                                 ;; TODO: Keep :expand commands.
                                 (dissoc :selectable-attributes))]
      (if (empty? label-elements)
        [:div {:class "horizontal-tags-element narrow"}
         (add-attributes
          (virtual-item-DOM (conj key :tags) :after
                            (assoc inherited-for-tags
                                   :adjacent-referent item-referent))
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
           [:div {:class "indent-wrapper tag"} dom]])))))

(defn item-without-labels-DOM-R
  "Make a dom for an item or exemplar for a group of items,
   given that any of its labels are in excluded-elements.
   Either the referent for the item/group must be provided,
   or inherited must contain :subject.
   We only record the key on the content, not the whole item
   (unless it is just the content)."
  ([item excluded-elements inherited]
   (let [referent (item-or-exemplar-referent item (:subject inherited))]
     (item-without-labels-DOM-R item referent excluded-elements inherited)))
  ([item referent excluded-elements inherited]
   (println
    "Generating DOM for"
    (simplify-for-print (conj (:parent-key inherited) (:item-id item))))
   (expr-let [content (entity/content item)
              elements (semantic-elements-R item)]
     (item-content-and-elements-DOM-R
      item referent content (remove (set excluded-elements) elements)
      inherited))))

(defn item-DOM-impl-R
   "Make a dom for an item or exemplar for a group of items.
   Either the referent for the item/group must be provided,
   or inherited must contain :subject.
   If the item is a tag, the caller is responsible for tag formatting."
  [item referent excluded-elements must-show-empty-label inherited]
  (expr-let [labels (entity/label->elements item :tag)]
    (let [labels (remove (set excluded-elements) labels)
          excluded (concat labels excluded-elements)]
      (expr-let [dom (item-without-labels-DOM-R
                      item referent excluded inherited)]
        (label-wrapper-DOM-R
         dom item referent labels must-show-empty-label inherited)))))

(defn item-DOM-R
   "Make a dom for an item or exemplar for a group of items.
   Either the referent for the item/group must be provided,
   or inherited must contain :subject.
   If the item is a tag, the caller is responsible for tag formatting."
  ([item excluded-elements inherited]
   (let [referent (item-or-exemplar-referent item (:subject inherited))]
     (item-DOM-impl-R item referent excluded-elements false inherited)))
  ([item referent excluded-elements inherited]
   (item-DOM-impl-R item referent excluded-elements false inherited)))

(defn must-show-label-item-DOM-R
   "Make a dom for an item or exemplar for a group of items, always showing
   at least one label, empty, if necessary. 
   Either the referent for the item/group must be provided,
   or inherited must contain :subject.
   If the item is a tag, the caller is responsible for tag formatting."
  ([item excluded-elements inherited]
   (let [referent (item-or-exemplar-referent item (:subject inherited))]
     (item-DOM-impl-R item referent excluded-elements true inherited)))
  ([item referent excluded-elements inherited]
   (item-DOM-impl-R item referent excluded-elements true inherited)))

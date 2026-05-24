(ns cosheet.server.render-utils
  (:require (cosheet [entity :refer [target-entity elements element?
                                      label-element? object? label-object?
                                      link-type-object? object-type-object?
                                      name-element?
                                      link-type name-label make-object-list
                                      label->elements
                                      make-element-list content orientation]]
                      [store :refer [item-id?]]
                      [utils :refer [multiset multiset-to-generating-values
                                     replace-in-seqs assoc-if-non-empty
                                     separate-by]]
                      [category-change-calculator :refer [category-change-R]]
                      [debug :refer [simplify-for-print]]
                      [query :refer [matching-elements]]
                      [orderable :as orderable]
                      [canonical :refer [canonicalize
                                         canonical-extended-by?
                                         canonical-have-common-elaboration?]]
                      [hiccup-utils
                       :refer [into-attributes add-attributes]])
            (cosheet.server
             [model-utils :refer [semantic-elements semantic-elements
                                  entity->fixed-term
                                  entity->canonical-semantic]]
             [hierarchy :refer [hierarchy-node-descendants]])))

;;; The following records hold definitions for templates for virtual items.

(defrecord
    ^{:doc
      "This is a template for a location that holds a virtual object that
       requires creating a sequence of items. An item matching the
       first template of the template-sequence must be created, with
       that item used as the target for creating the item matching the
       next template the sequence, etc. Except if a template is an
       object, the new object becomes the content of the target. The
       reference of the whole sequence is the item created for the
       final template."}
    SequentialTemplate
    [template-sequence])

(defmethod print-method SequentialTemplate [s ^java.io.Writer w]
  (.write w (str "SequentialTemplate " (vec (:template-sequence s)))))

(defn make-sequential-template
  [template]
  (->SequentialTemplate template))

(defn sequential-template?
  [template]
  (instance? SequentialTemplate template))

(defn universal-template?
  "Return true if the template can match both elements and objects."
  [template]
  (or (= template 'anything) (= template nil)))

(defn display-type
  "Return :link-type, :object-type, or :name depending on whether the
  entity satisfies link-type-object?, object-type-object?, or
  name-element? respectively."
  [entity]
  (cond (link-type-object? entity) :link-type
        (object-type-object? entity) :object-type
        (name-element? entity) :name))

;;; TODO: !!! This counts class objects as labels, but only creates
;;; label objects. make it consistent.
(defn ensure-label-object
  "Give a template that can indicate an object, make it be a label object if
  it isn't already."
  [template]
  (assert (not (sequential-template? template)))
  (let [is-label (label-object? template)
        has-name (seq (label->elements template name-label))]
    (if (and is-label has-name)
      template
      (make-object-list
       (cond-> (if (universal-template? template)
                 []
                 (do (assert (object? template))
                     (elements template)))
         (not is-label)
         (conj `(~link-type))
         (not has-name)
         (conj `("" (~name-label))))))))

(defn ensure-label-object-content
  "Given a template that can match an element, make its content be a
  label object, if it isn't already."
  [template]
  (assert (or (element? template) (universal-template? template)))
  (make-element-list (orientation template)
                     (ensure-label-object (content template))
                     (elements template)))

(defn make-virtual-label-template
  "Given a template for an element, make a template for a virtual label
  consisting of that element. In other words, pull out the last
  template if there's a sequence. That template should be a template
  for an element.  Replace it with two templates, one for an identical
  element, except with content of the empty string. and one for a
  label object that matches the element's content."
  [template]
  (let [[prefix-templates last-template]
        (if (sequential-template? template)
          (let [template-sequence (:template-sequence template)]
            [(butlast template-sequence) (last template-sequence)])
          [[] template])]
    (make-sequential-template
     (concat
      prefix-templates
       ;; The label.
      [(cons "" (elements last-template))
       ;; It's content.
       (ensure-label-object (content last-template))]))))

(defn specification-item-id
  [specification]
  (or (:auxiliary-item-id specification) (:relative-id specification)))

(defn restrict-store-to-specification-id
  "Given a dom specification and a mutable store, return a category
  change reporter over the mutable store, that restricts its interest
  to the category of the specification-item-id"
  [specification mutable-store]
   (let [id (specification-item-id specification)]
     (assert (item-id? id) id)
     (category-change-R [id] mutable-store)))

(defn condition-satisfiers
  "Return a sequence of elements of an entity that match the elements of
  condition, except that the empty string in an element is considered
  to match a nil in a condition. The condition must be in list form.
  If part of a condition is not satisfied by any element, ignore that
  part."
  [entity condition]
  (when-let [condition-elements (seq (elements condition))]
    (let [elements (elements entity)
          canonical-elements (map entity->canonical-semantic elements)]
      (multiset-to-generating-values
       (multiset (map #(entity->canonical-semantic
                        (replace-in-seqs % nil ""))
                      condition-elements))
       canonical-elements elements))))

(defn transform-specification-for-elements
  [specification]
  (assoc (select-keys specification [:width :immutable])
         :template 'anything))

(defn transform-specification-for-labels
  [specification]
  (assoc (select-keys specification [:width :immutable])
         :template `(~(ensure-label-object 'anything))
         :omit-universal-elements true))

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
         :template (ensure-label-object-content 'anything)
         :omit-universal-elements true))

(defn entity->canonical-term
  "Return the canonical list version of the semantic parts of an entity,
  with 'anything changed to nil."
  [entity]
  (canonicalize (entity->fixed-term entity)))

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
        siblings (semantic-elements (target-entity entity))
        [labels non-labels] (separate-by label-element? siblings)
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
                           (and (canonical-have-common-elaboration?
                                 entity-canonical candidate-canonical)
                                (not (canonical-extended-by?
                                      candidate-canonical entity-canonical))
                                (not (so-far candidate-canonical)))
                           (assoc candidate-canonical candidate))))
                     {} candidates))
      ;; The matching list includes the entity, so there is an identical
      ;; candidate if there is more than one element.
      (not (empty? (rest matching)))
      ;; We only need one matching candidate. If the entities are
      ;; distinguishable, choose one different from the entity we
      ;; started with.
      (conj (or (first (remove #(= % entity) matching)) (first matching))))))

;;; DOM creators that are used by several files.

(defn make-component
  "Make a component dom with the given specification"
  [{:as specification}]
  (assert (:relative-id specification) ["NO relative- id" specification])
  (assert (:render-dom specification) ["NO render-dom" specification])
  (assert (:get-action-data specification) ["NO get-action-data" specification])
  [:component specification])

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

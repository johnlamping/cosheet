(ns cosheet.server.model-utils
  (:require
   (cosheet
    [utils :refer [threaded-map
                   extract-first]]
    [orderable :refer [initial split]]
    [reporter-macros :refer [let-R]]
    [canonical :refer [canonicalize equivalent-primitives?]]
    [store :refer [new-element-store
                   update-source update-target id->source id->target
                   add-link remove-link declare-ephemeral-id
                   target->ids source->ids id-valid-link?
                   item-id? link-id? object-id? interned-object-id?
                   generic-name?
                   target-label->ids get-new-object-id]]
    [entity :refer [object?
                    object-type-object? non-type-object? universal-object?
                    uniquely-identified-object? interned-object?
                    id-identified-object?
                    element? label-element? name-element? id->entity
                    content all-elements forward-elements entity-direction
                    reverse? store-item-id DirectedItemId
                    containing-elements
                    link-type object-type name-label
                    content->elements label->elements label->element
                    label->content
                    target-entity entity-key
                    make-tree-element make-tree-object primitive?
                    make-tree-object-copying-id make-tree-id
                    make-conflux-tree-object conflux-tree-object-id
                    conflux-tree-object? non-conflux-tree-object?
                    convert-unneeded-conflux-tree-objects
                    identity-post-fn
                    threaded-traverse
                    repetition-avoiding-threaded-traverse
                    add-elements-to-entity
                    entity-complexity stored-entity?
                    tree-entity? presumed-interned-object?
                    in-different-store]]
    [store-utils :refer [add-element add-object add-isolated-element
                         update-directed-id-content-value
                         find-object-by-name add-universal-objects
                         link-type-object object-type-object]]
    [query :refer [matching-items
                   make-not-query special-form? variable-query?
                   variable-name variable-qualifier variable-reference
                   make-variable-query
                   extended-by?]]
    [query-calculator :refer [matching-item-ids-R]])
   (cosheet.server
    [order-utils :refer [orderable-entity? ordered-ids-R ordered-entities
                         order-element-for-item furthest-item]]
    [semantic :refer [semantic-element? semantic-elements
                      internal-semantic-to-tree semantic-to-tree
                      selector-semantic-elements selector-semantic-to-tree
                      selector? mark-template-as-selector
                      replace-anythings-with-empty selector-marked-entities
                      fixed-term-to-template
                      make-non-semantic-tree-object]])))

;;; Utilities that know about how information is encoded in terms of the store.

(def current-format 7)

;;; Creating new labels

(defn next-new-string
  "Given a string, return the next string."
  [string]
  (if (empty? string)
    "A"
    (let [prefix (subs string 0 (dec (count string)))
          end (last string)]
      (if (>= (int end) (int \Z))
        (str (next-new-string prefix) "A")
        (str prefix (char (inc (int end))))))))

(defn get-new-string
  "Given an immutable store, return a new short string of all cap letters
   that does not occur in the store, and an updated store that knows what
   the last new string was."
  [store]
  (let [last-string-item (first (matching-items
                                 '(nil :last-new-string) store))]
    (loop [last-new (when last-string-item (content last-string-item))]
      (let [next-new (next-new-string last-new)]
        (if (seq (matching-items next-new store))
          (recur next-new)
          [next-new
           (if last-string-item
             (update-source
              store (:item-id last-string-item) next-new)
             (first (add-object store (make-non-semantic-tree-object
                                       [`(~next-new :last-new-string)]))))])))))

(defn get-n-new-strings
  "Get n new strings"
  [store n]
  (reduce (fn [[strings store] _]
            (let [[new-string store] (get-new-string store)]
              [(conj strings new-string) store]))
          [[] store] (range n)))

(defn ordered-semantic-elements
  "Return the semantic elements of an entity, in the order that the
  :order information calls for."
  [immutable-entity]
  (ordered-entities (semantic-elements immutable-entity)))

(defn- only-name-and-type-semantic-elements?
  "Return true if every semantic element of the object with the given id
  is either a name, or has a content that is an object-type object or
  a universal object. That means that the object carries no
  information beyond what was present when it was created."
  [store id]
  (every? (fn [element]
            (or (name-element? element)
                (let [element-content (content element)]
                  (or (universal-object? element-content)
                      (object-type-object? element-content)))))
          (semantic-elements (id->entity id store))))

(defn- has-object-type-label?
  "Return true if the object with the given id has an element whose
  content is a non-universal object-type object."
  [store id]
  (some (fn [element]
          (let [element-content (content element)]
            (and (object-type-object? element-content)
                 (not (universal-object? element-content)))))
        (forward-elements (id->entity id store))))

(defn- links-to-remove
  "Return a list of ids of links to remove in order to remove the entity
  with the given id, and all its elements and non-interned objects or
  minimal interned objects in their contents. Don't recurse into
  previous-id; our caller will handle that. (This prevents infinite
  loops when an element contains an object as its source.)  Return the
  list in an order suitable for removing."
  [store previous-id id]
  (assert (item-id? id))
  (concat
   ;; First, we have to remove all the elements.
   (mapcat (partial links-to-remove store id)
           (cond-> (target->ids store id)
             ;; Object ids can also be sources.
             (object-id? id)
             (concat (remove #(= previous-id %)
                             (source->ids store id)))))
   ;; Once all the elements are gone, if the id is a link, we can
   ;; remove its content, if that is an unnamed object or a trivial
   ;; named one, it has no object type label, and we are the only
   ;; reference to it. Finally, remove the link, itself.
   (when (link-id? id)
     (concat (let [content-id (id->source store id)]
               (when (and (object-id? content-id)
                          (not= content-id previous-id)
                          (empty? (remove #(= id %)
                                          (source->ids store content-id)))
                          (not (has-object-type-label? store content-id))
                          (or (not (interned-object-id? store content-id))
                              ;; An interned object that is just a bare
                              ;; label or class, and that this link is
                              ;; the only reference to, can have its
                              ;; elements removed too.
                              (only-name-and-type-semantic-elements?
                               store content-id)))
                 (links-to-remove store id content-id)))
             [id]))))

(defn remove-entity-by-id
  "Remove the entity with the given id, and all its elements."
  [store id]
  (assert (item-id? id))
  (reduce (fn [store id] (remove-link store id))
          store (links-to-remove store nil id)))

(defn remove-semantic-elements
  "Return the store with all semantic elements of the given id removed."
  [immutable-store id]
  (let [item (id->entity id immutable-store)]
    (reduce remove-entity-by-id
            immutable-store
            (map (fn [element] (store-item-id (:item-id element)))
                 (semantic-elements item)))))

(defn ordered-semantic-to-tree
  "Given an immutable entity, make a list representation of the
  semantic information of the entity, putting elements in the order that the
  :order information calls for."
  [immutable-entity]
  (internal-semantic-to-tree immutable-entity ordered-semantic-elements))

;;; In the store, an entity is marked as a selector if it is intended
;;; to be used as a query. This means that it should typically not be
;;; returned as the result of a query. For example, a table's row
;;; condition shouldn't be returned as one of the rows of a table. And
;;; even part of a row condition shouldn't be returned for a query
;;; that is looking for that part.

;;; To indicate that an entity is a selector, it is marked with an
;;; element whose content is :selector. Further, all of its recursive
;;; sub-parts (not counting interned objects) are also considered to
;;; be selectors. In addition, reverse links encountered in the
;;; recursion have their :selector element marked with :reversed.

(defn ordered-selector-semantic-elements
  "Return the selector-marked semantic elements of an entity, in the
  order that the :order information calls for."
  [entity]
  (ordered-entities (selector-semantic-elements entity)))

(defn object-semantic-to-tree
  "Given an immutable object, make a list representation of its semantic
  information, even if the object is identified. Put elements in the
  order that the :order information calls for."
  [immutable-entity]
  ;; internal-semantic-to-tree wouldn't expand an interned object, so
  ;; do the first expansion here and call internal-semantic-to-tree on
  ;; each of the resulting sub-elements.
  (->> (ordered-semantic-elements immutable-entity)
       (map ordered-semantic-to-tree)
       make-tree-object))

(defn selector-object-semantic-to-tree
  "Like object-semantic-to-tree, but only follows elements that are
  marked with a :selector whose orientation matches theirs."
  [immutable-entity]
  (->> (ordered-selector-semantic-elements immutable-entity)
       (map (fn [element]
              (internal-semantic-to-tree
               element ordered-selector-semantic-elements)))
       make-tree-object))

(defn- replace-anything-content-with-empty
  "Return the store with the content of the element replaced by an
  empty object if the element is reversed, otherwise by the empty
  string."
  [store element]
  (let [[store value] (if (reverse? element)
                        (add-object store (make-tree-object []))
                        [store ""])]
    (update-directed-id-content-value store (:item-id element) value)))

(defn modify-to-non-selector
  "Update the store so that the entity with the given id is no longer a
  selector: remove its :selector markings and replace its 'anything
  contents with the empty string, or with an empty object for reversed
  elements."
  [store id]
  (reduce (fn [store entity]
            (cond-> (reduce remove-entity-by-id store
                            (map entity-key
                                 (content->elements entity :selector)))
              (and (element? entity) (= (content entity) 'anything))
              (replace-anything-content-with-empty entity)))
          store
          (selector-marked-entities store id)))

;;; Adding new elements and objects, noting the orders in their
;;; templates.

(defn changes-to-merge-elements
  "Given two sequences of elements, find a subset of their union with as
  few elements as possible such that an entity with those elements
  would satisfy:
     * All positive queries that an entity with the donor elements would
       satisfiy.
     * All positive queries that an entity with the recipient elements
       would satisfy.
     * No positive queries that aren't satisfied by an entity
       with the union of all the elements.
  
  The last condition implies that you can't merge two elements from
  the seqs, as that could satisfy a query the union of the seqs
  doesn't satisfy. For example, suppose you merged a '(5 1) element
  with a '(5 2) element, to get a '(5 1 2) element, the '(5 1 2)
  satisfies any positive query containing either '(5 1) or '(5 2). But
  it also satisfies the query '(5 1 2), which an object containing
  both '(5 1) and '(5 2) as separate elements doesn't satisfy.
  Instead, you have to start with the union of the elements of the two
  arguments, but you can remove an element from the union if it is
  extended by an element from the other argument.
  
  Return the diff between that union and the recipient elements - the
  changes to the recipients to turn it into the union - a seq of:
     * The subset of donor elements that must be added
     * The subset of the recipient elements that must be removed

  Elements are paired greedily, from most complex to least, so that
  mergers of complex elements are preferred. Each element, when it is
  chosen, is paired with the most complex still unpaired element of
  the other seq that it extends, if there is one. Ties in complexity
  go to the recipient, so that a donor element that an equally
  complex recipient element extends can simply be ignored."
  [recipient-elements donor-elements]
  (let [;; For each element, record its semantic tree, because only semantic
        ;; information matters for whether one element can replace
        ;; another, plus the complexity of that tree.
        element-info (into {}
                           (map (fn [element]
                                  (let [semantic (if (stored-entity? element)
                                                   (semantic-to-tree element)
                                                   element)]
                                    [element
                                     [semantic (entity-complexity semantic)]]))
                                (concat recipient-elements donor-elements)))
        complexity (fn [element] (second (element-info element)))
        by-complexity (fn [elements]
                        (reverse (sort-by complexity elements)))
        ;; Return the most complex of candidates that extender extends,
        ;; and the remaining candidates.
        extract-extended (fn [extender candidates]
                           (let [semantic (first (element-info extender))]
                             (extract-first
                              (fn [candidate]
                                (extended-by? (first (element-info candidate))
                                              semantic))
                              candidates)))]
    (loop [unpaired-recipient (by-complexity recipient-elements)
           unpaired-donor (by-complexity donor-elements)
           donor-elements-to-add []
           recipient-elements-to-remove []]
      (cond
        (and (empty? unpaired-recipient) (empty? unpaired-donor))
        [donor-elements-to-add recipient-elements-to-remove]
        (and (seq unpaired-recipient)
             (or (empty? unpaired-donor)
                 (>= (complexity (first unpaired-recipient))
                     (complexity (first unpaired-donor)))))
        ;; This recipient element is at least as complex as any donor.
        ;; We are going to want to keep it, but we can forget a donor
        ;; that it extends.
        (let [[chosen & remaining-recipient] unpaired-recipient
              [_ remaining-donor] (extract-extended chosen unpaired-donor)]
          (recur remaining-recipient remaining-donor
                 donor-elements-to-add recipient-elements-to-remove))
        :else
        ;; This donor element is more complex than any recipient.
        ;; We are going to want to copy it, but we can remove a
        ;; recipient that it extends.
        (let [[chosen & remaining-donor] unpaired-donor
              [extended remaining-recipient] (extract-extended
                                              chosen unpaired-recipient)]
          (recur remaining-recipient remaining-donor
                 (conj donor-elements-to-add chosen)
                 (cond-> recipient-elements-to-remove
                   extended (conj extended))))))))

(defn merge-objects
  "Given a store and the ids of two interned objects, modify the
  recipient object so that it satisfies every positive semantic query
  that either object satisfies, then remove all remaining elements
  from the donor object. Return the updated store.
  Rather than copying elements, a semantic element of the donor object
  that the recipient object needs is moved to the recipient object, by
  re-pointing its target to the recipient. Elements of the recipient
  object that this makes redundant are removed. Likewise, elements
  elsewhere whose source is the donor are re-pointed to have the
  recipient as source.
  Both objects must be interned, so that a link joining them is not a
  cycle that a traversal would have to descend into."
  [store recipient-id donor-id]
  (assert (item-id? recipient-id))
  (assert (item-id? donor-id))
  (let [recipient (id->entity recipient-id store)
        donor (id->entity donor-id store)
        _ (assert (interned-object? recipient) recipient)
        _ (assert (interned-object? donor) donor)
        [elements-to-move elements-to-remove]
        (changes-to-merge-elements
         (filter semantic-element? (forward-elements recipient))
         (filter semantic-element? (forward-elements donor)))
        store (reduce (fn [store element]
                        (update-target store (:item-id element) recipient-id))
                      store elements-to-move)
        store (reduce remove-entity-by-id store
                      (map :item-id elements-to-remove))
        ;; Now re-point the links that have the donor as source.
        store (reduce (fn [store id] (update-source store id recipient-id))
                      store (source->ids store donor-id))]
    ;; Finally, remove the left-over elements in the donor.
    (reduce remove-entity-by-id store
            (map (fn [element] (store-item-id (:item-id element)))
                 (all-elements (in-different-store donor store))))))

(def update-add-object-with-order-without-revisiting)

(def update-add-elements-with-order-without-revisiting)

(defn update-add-position-and-elements-with-order-without-revisiting
  "Given the id of an already-created entity, add an :order element to
   it for its own position, and add the given template-elements to it,
   each with its own order. The provided order is split into a bigger
   and smaller piece, with the entity getting the bigger piece if
   use-bigger is true, otherwise the smaller one. The seen map (stored
   object → entity id) is threaded through for cycle detection.
   Return [store remainder seen]."
  [store entity-id template-elements order position use-bigger seen]
  (assert (or (nil? entity-id) (item-id? entity-id)))
  (let [entity-order-index (case position :before 0 :after 1)
        other-position ([:after :before] entity-order-index)
        split-order (split order (if use-bigger position other-position))
        bigger-index (if use-bigger
                       entity-order-index
                       (- 1 entity-order-index))
        bigger-order (split-order bigger-index)
        smaller-order (split-order (- 1 bigger-index))
        [store bigger-order seen]
        (update-add-elements-with-order-without-revisiting
         store entity-id template-elements bigger-order position seen)
        [store _] (add-element
                   store entity-id
                   `(~(if use-bigger bigger-order smaller-order) :order))]
    [store (if use-bigger smaller-order bigger-order) seen]))

(defn update-add-element-with-order-and-ephemeral-without-revisiting
  "Like update-add-element-with-order-and-ephemeral, but threads the
   seen map for cycle detection. If the element's content is a stored
   object already in seen, the element is treated as a back-reference
   and skipped entirely (no link is added), since the corresponding
   link in the other direction has already been created by the call
   that added the object to seen. The attachment-id is the endpoint
   of the new link that does NOT hold the element's content: for an
   element with :source orientation it becomes the link's target, and
   for :target orientation it becomes the link's source. Return
   [store id remainder seen]."
  [store attachment-id template order position use-bigger seen]
  (assert (or (nil? attachment-id) (item-id? attachment-id)))
  (assert (empty? (label->elements template :order)))
  (let [template-content (content template)
        template-elements (all-elements template)
        is-ephemeral (some (fn [element] (= (content element) :ephemeral))
                           template-elements)]
    (cond
      (and (stored-entity? template-content)
           (object? template-content)
           (not (interned-object? template-content))
           (contains? seen (entity-key template-content)))
      [store nil order seen]
      (not (orderable-entity? template))
      (if (and (object? template-content)
               (not (stored-entity? template-content)))
        ;; The element is non-semantic, but the semantic parts of the
        ;; new object it holds still need orders.
        (let [[store object-id order seen]
              (update-add-object-with-order-without-revisiting
               store template-content order position false seen)
              [store id] (if (reverse? template)
                           (add-link store object-id attachment-id)
                           (add-link store attachment-id object-id))
              [store order seen]
              (update-add-elements-with-order-without-revisiting
               store id template-elements order position seen)]
          [store id order seen])
        (let [[s id] (add-element store attachment-id template)]
          [s id order seen]))
      :else
      (let [[store value-to-store order seen]
            (if (object? template-content)
              (update-add-object-with-order-without-revisiting
               store template-content order position false seen)
              [store template-content order seen])
            [store id] (if (reverse? template)
                         (add-link store value-to-store attachment-id)
                         (add-link store attachment-id value-to-store))
            [store remainder seen]
            (update-add-position-and-elements-with-order-without-revisiting
             store id template-elements order position use-bigger seen)]
        [(if is-ephemeral (declare-ephemeral-id store id) store)
         id
         remainder
         seen]))))

(defn update-add-elements-with-order-without-revisiting
  "Return [store order seen]."
  [store attachment-id elements order position seen]
  (assert (or (nil? attachment-id) (item-id? attachment-id)))
  (let [[s _ order seen]
        (reduce (fn [[store _ order seen] element]
                  (update-add-element-with-order-and-ephemeral-without-revisiting
                   store attachment-id element order position false seen))
                [store nil order seen]
                (case position
                  :before elements
                  ;; If we are adding them after the current order
                  ;; chunk, then each one is before the previous
                  ;; one, as the chunk shrinks.
                  :after (reverse elements)))]
    [s order seen]))

(defn update-add-object-with-given-elements-and-order-without-revisiting
  "Like update-add-object-with-given-elements-and-order. If seen-key
   is non-nil, register seen-key → new object id in seen before
   recursing into the elements (to break cycles). Return
   [store object-id remainder seen]."
  [store element-templates order position use-bigger seen seen-key]
  (let [[store object-id] (get-new-object-id store)
        seen (cond-> seen seen-key (assoc seen-key object-id))
        [store remainder seen]
        (update-add-position-and-elements-with-order-without-revisiting
         store object-id element-templates order position use-bigger seen)]
    [store object-id remainder seen]))

(defn get-or-make-ordered-object-by-name-without-revisiting
  "Like get-or-make-ordered-object-by-name. If seen-key is non-nil,
   register seen-key → object id in seen before recursing into the
   additional elements. Return [store object-id order seen]."
  [store name fixed-term order position use-bigger seen seen-key]
  (assert (object? fixed-term) fixed-term)
  (if (presumed-interned-object? fixed-term)
    
    ;; The fixed term is an interned object. Make sure it is the
    ;; object we are looking for, and return it
    (do (assert (equivalent-primitives?
                 name (content (label->element fixed-term name-label))))
        (let [object-id (entity-key fixed-term)]
          [store object-id order
           (cond-> seen seen-key (assoc seen-key object-id))]))
    
    ;; It's not interned.
    (let [;; First, get or make an object with the given name.
          [store object-id order seen]
          (if-let [object (find-object-by-name store name fixed-term)]
            [store (entity-key object) order
             (cond-> seen seen-key (assoc seen-key (entity-key object)))]
            (update-add-object-with-given-elements-and-order-without-revisiting
             store `((~name (~name-label))) order position use-bigger seen
             seen-key))
          ;; Now make it satisfy the fixed term.
          [templates-to-add elements-to-remove]
          (if (presumed-interned-object? fixed-term)
            ;; Don't change anything if the template is already interned.
            [nil nil]
            (let [[terms-to-add elements-to-remove]
                  (changes-to-merge-elements
                   (all-elements (id->entity object-id store))
                   (remove special-form? (all-elements fixed-term)))]
              [(map fixed-term-to-template terms-to-add) elements-to-remove]))
          [store order seen]
          (update-add-elements-with-order-without-revisiting
           store object-id templates-to-add order position seen)
          store (reduce remove-entity-by-id store
                        (map (fn [element] (store-item-id (:item-id element)))
                             elements-to-remove))]
      [store object-id order seen])))

(defn update-add-object-with-order-without-revisiting
  "Like update-add-object-with-order, but checks seen for cycle
   detection. If the template is a stored entity already in seen,
   return its cached id without recursing into its elements. Otherwise
   process the template, registering it in seen before recursing into
   any sub-objects. Return [store id remainder seen]."
  [store template order position use-bigger seen]
  (assert (object? template) template)
  (let [seen-key (when (and (stored-entity? template)
                            (not (interned-object? template)))
                   (entity-key template))]
    (if-let [cached-id (when seen-key (get seen seen-key))]
      [store cached-id order seen]
      (cond (id-identified-object? template)
            (let [id (entity-key template)]
              [store id order
               (cond-> seen seen-key (assoc seen-key id))])
            (and (stored-entity? template) (nil? (:store template)))
            (do (assert (uniquely-identified-object?
                         (in-different-store template store))
                        template)
                (let [id (entity-key template)]
                  [store id order
                   (cond-> seen seen-key (assoc seen-key id))]))
            (uniquely-identified-object? template)
            (let [name-elements (label->elements template name-label)]
              (assert (seq name-elements) template)
              (get-or-make-ordered-object-by-name-without-revisiting
               store (content (first name-elements)) template order position
               use-bigger seen seen-key))
            true
            (do (assert (empty? (label->elements template :order)))
                (update-add-object-with-given-elements-and-order-without-revisiting
                 store (all-elements template) order position use-bigger seen
                 seen-key))))))

(defn update-add-element-adjacent-to-without-revisiting
  "Like update-add-element-adjacent-to, threading seen. Return
   [store id seen]."
  [store attachment-id element adjacent-to position use-bigger seen]
  (assert (or (nil? attachment-id) (item-id? attachment-id)))
  (let [order-element (order-element-for-item adjacent-to store)
        order (content order-element)
        [store id remainder seen]
        (update-add-element-with-order-and-ephemeral-without-revisiting
         store attachment-id element order position use-bigger seen)]
    [(update-source store (:item-id order-element) remainder) id seen]))

(defn update-add-object-adjacent-to-without-revisiting
  "Like update-add-object-adjacent-to, threading seen. Return
   [store object-id seen]."
  [store object-template adjacent-to position use-bigger seen]
  (assert (object? object-template))
  (assert (empty? (label->elements object-template :order)))
  (let [order-element (order-element-for-item adjacent-to store)
        order (content order-element)
        [store object-id remainder seen]
        (update-add-object-with-order-without-revisiting
         store object-template order position use-bigger seen)]
    [(update-source store (:item-id order-element) remainder)
     object-id
     seen]))

;;; Client-facing wrappers: call the -without-revisiting versions with
;;; an empty seen map.

(defn update-add-element-with-order-and-ephemeral
  "Add an element, described in list form, to the store, attached to
  attachment-id (the endpoint of the new link that does NOT hold the
  element's content). Add ordering information to the element and each
  part of it, except for non-semantic elements, splitting the provided
  order for the orders, and returning an unused piece of it.  Put the
  new entity in the specified position (:before or :after) of the
  returned order, and make the entity use the bigger piece if
  use-bigger is true, otherwise use the smaller piece.  If the
  template has a :ephemeral element, mark it ephemeral in the store.
  Return the new store, the id of the item, and the remaining order."
  [store attachment-id template order position use-bigger]
  (assert (or (nil? attachment-id) (item-id? attachment-id)))
  (let [[store id remainder _]
        (update-add-element-with-order-and-ephemeral-without-revisiting
         store attachment-id template order position use-bigger {})]
    [store id remainder]))

(defn get-or-make-ordered-object-by-name
  "Find or make an object with the given name, and satisfying the
  fixed-term.  If an object is found, add elements to it if necessary
  to make it satisfy the fixed term, and remove elements that were
  rendered redundant because we added a more specific element. A newly
  created object gets the bigger piece of the order split if
  use-bigger is true, otherwise the smaller piece. The name must not
  be generic, since a generic name can't identify an object. Return
  the new store, the id of the matching object, and the unused part of
  the order."
  [store name fixed-term order position use-bigger]
  (assert (not (generic-name? name)) name)
  (let [[store id order _]
        (get-or-make-ordered-object-by-name-without-revisiting
         store name fixed-term order position use-bigger {} nil)]
    [store id order]))

(defn update-add-object-with-order
  "Add an object matching the template to the store, or update a unique
  one to match the template. The new object gets the bigger piece of
  the order split if use-bigger is true, otherwise the smaller piece.
  Return the new store, the id of the object, and the unused part of
  the order."
  [store template order position use-bigger]
  (let [[store id remainder _]
        (update-add-object-with-order-without-revisiting
         store template order position use-bigger {})]
    [store id remainder]))

(defn update-add-element-adjacent-to
  "Add an entity attached to attachment-id (the endpoint of the new
   link that does NOT hold the element's content), taking its order
   from the given item, in the given position, and giving the entity
   the bigger piece if use-bigger is true. Return the updated store
   and the id of the entity."
  [store attachment-id element adjacent-to position use-bigger]
  (assert (or (nil? attachment-id) (item-id? attachment-id)))
  (let [[store id _]
        (update-add-element-adjacent-to-without-revisiting
         store attachment-id element adjacent-to position use-bigger {})]
    [store id]))

(defn update-add-object-adjacent-to
  "Add a top-level object matching the template,
   taking its order from the given item, in the given position,
   and giving the new object the bigger piece of the order
   if use-bigger is true.
   Return the updated store and the id of the new object."
  [store object-template adjacent-to position use-bigger]
  (let [[store id _]
        (update-add-object-adjacent-to-without-revisiting
         store object-template adjacent-to position use-bigger {})]
    [store id]))

;;; Handling of generics and templates

(defn specialize-generic
  "Adjust a generic to make it ready for adding as an
  element. Specifically, replace each '??? with a new unique string.
  Allocating new strings will require updating the store. Return the
  specialized template and the new store."
  [generic store]
  (letfn [(pre-fn [_ e _ store]
            (if (= e '???)
              (let [[s new-store] (get-new-string store)]
                [s new-store])
              [e store]))]
    (repetition-avoiding-threaded-traverse
       generic pre-fn identity-post-fn store)))

(defn create-possible-selector-entity
  "Create an entity matching the template. If the subject-id is a
  selector, mark the template as a selector before adding it.
  If the template is an element, make the subject-id, whatever its
  direction, its subject, with the new link direction matching the
  template's.
  If the template is an object and the subject-id is non-nil, it may
  be a DirectedItemId; its content endpoint will be set to the new
  object.
  Return the updated store and the id of the new entity."
  [template subject-id adjacent-id position use-bigger store]
  (assert (or (nil? subject-id) (satisfies? DirectedItemId subject-id)))
  (let [template (if (and subject-id
                          (selector? (id->entity subject-id store)))
                   (mark-template-as-selector template)
                   (replace-anythings-with-empty template))]
    (if (object? template)
      (let [[store id] (update-add-object-adjacent-to
                        store template
                        (id->entity adjacent-id store)
                        position use-bigger)]
        [(if subject-id
           (update-directed-id-content-value store subject-id id)
           store)
         id])
      (update-add-element-adjacent-to store (some-> subject-id store-item-id)
                                      template (id->entity adjacent-id store)
                                      position use-bigger))))

(defn create-possible-selector-entities
  "Create entities, specializing the template as appropriate, depending on
   whether each subject is a selector. Return the updated store and the
   new ids."
  [template subjects adjacents position use-bigger store]
  (let [[specialized-template store] (specialize-generic template store)
        [ids store] (threaded-map
                     (fn [[subject adjacent] store]
                       (let [[store id] (create-possible-selector-entity
                                         specialized-template
                                         subject adjacent position use-bigger
                                         store)]
                         [id store]))
                     (map vector subjects adjacents)
                     store)]
    [store ids]))

;;; Tabs

;;; A tab is a non-semantic object marked with a :tab element.
;;; Its names are its semantic elements, and its :order element gives
;;; its position among the tabs.
;;; The topic of a tab, a table or an item, is the target of a link,
;;; marked :tab-topic, whose source is the tab. The link is
;;; non-semantic, seen from the topic, because its content is the tab.
;;; This way, the topic can be an element, as well as an object. But
;;; if it's an element, the link does not make an element of the tab
;;; object, so it has to be found by looking through the elements
;;; containing the tab object.

;;; This the matching items of this query will be all tabs.
(def tab-query (make-tree-object [:tab]))

(defn ordered-tabs-ids-R
  "Return the ids of the tabs, in order."
  [store]
  (let-R [tab-ids (matching-item-ids-R tab-query store)]
    (ordered-ids-R tab-ids store)))

(defn tab-topic
  "Return the topic of the tab, found through the element of the topic,
  marked :tab-topic, whose content is the tab. Return nil if the tab
  has no topic."
  [tab]
  (when-let [link (first (filter #(seq (content->elements % :tab-topic))
                                 (containing-elements tab)))]
    (target-entity link)))

(def label-object-template
  (make-tree-object [`(~'anything (~name-label)) `(~link-type)]))

;;; A table is a non-semantic object with a :table element, and with
;;; the following elements that describe the table:
;;;   :row-condition  The content is an object that gives the
;;;                   requirements for an item to appear as a row.
;;;                   It is marked as :selector.
;;;  :column-headers  An element, each of whose sub-elements is
;;;                   the condition for a column header. Generally,
;;;                   the sub-elements' content will be the keyword
;;;                   'anything, to indicate no constraint on the
;;;                   content of an element in the row, without
;;;                   breaking the rule that the database doesn't
;;;                   contain nil. The exception is the special
;;;                   content :other, which means to show everything
;;;                   not shown in any other column. (:other not yet
;;;                   implemented.) It is marked as :selector.

;;; For doing matches for batch edits, it is sort of like each header
;;; is part of the row condition, but only aone header at a time. That
;;; means that there are two ways to match the query condition:
;;;    A The row condition matches, by itself.
;;;      In this case, each header gets a chance to match the stack,
;;;      as well as each part of the row condition.

;;;    B Case A fails, but the combination of the row condition and a
;;;      header.
;;;      In this case, only a header that makes the match succeed can
;;;      match the stack.

(defn table-column-headers-id
  [table-id immutable-store]
  (assert (item-id? table-id))
  (first (target-label->ids immutable-store table-id :column-headers)))

(defn table-column-headers-element
  [table-item]
  (label->element table-item :column-headers))

(defn table-row-condition-id
  [table-id immutable-store]
  (assert (item-id? table-id))
  (first (target-label->ids immutable-store table-id :row-condition)))

(defn table-row-condition-element
  [table-item]
  (label->element table-item :row-condition))

(defn table-row-condition-object
  "Return the object inside the row-condition element of the table."
  [table-item]
  (content (table-row-condition-element table-item)))

(defn table-row-condition->row-template
  "Return the row template from the row condition. The template is the
   object each row must extend."
  [row-condition]
  (selector-object-semantic-to-tree (content row-condition)))

(defn table-row-template
  "Return the row condition as a template."
  [table-item]
  (selector-object-semantic-to-tree
   (table-row-condition-object table-item)))

(defn table-item?
  "Return whether the item is a table."
  [item]
  (seq (content->elements item :table)))

(defn table-object-template
  "Return the template for a table with the given row condition
  elements and header elements. One of the row condition elements
  should be an object type, which will give the kind of object the
  rows should contain"
  [row-condition-elements header-elements]
  (make-non-semantic-tree-object
   [:table
    `(~(make-tree-object
        (conj (map mark-template-as-selector row-condition-elements)
              :selector))
      :row-condition)
    (concat '(anything :column-headers :selector)
            (map mark-template-as-selector header-elements))]))

(def new-table-object-template
  (table-object-template [`(~(object-type-object '???))]
                         [`(~'anything (~(link-type-object '???)))]))

(defn tab-object-template
  "Return the template for a tab with the given names, and with the
  given topic object, if there is one."
  [names topic]
  (make-non-semantic-tree-object
   (cond-> (into [:tab] names)
     topic (conj (make-tree-element topic [:tab-topic] :reverse)))))

(def column-header-template
  ;; The minimum content for a column header.
  ;; In addition, a column header must be stored as an element
  ;; of the :column-headers entity.
  `(~'anything ; a keyword here would make this non-semantic and hence
               ; not orderable
    :selector
    ) 
  )

(def unspecified-column-header-template
  ;; A header for a newly created column that we don't know anything about.
  ;; We give it a new label, so that it won't start out match everything.
  (add-elements-to-entity
   column-header-template [(mark-template-as-selector
                            `(~(link-type-object '???)))]))

(defn add-at-end-of-tabs
  "Add a tab matching the template after the last tab, or after the
  unused order if there are no tabs. Return the updated store and the
  id of the new tab."
  [store tab-template]
  (let [tabs (ordered-entities (matching-items tab-query store))
        adjacent (if (seq tabs)
                   (last tabs)
                   (first (matching-items '(nil :unused-orderable) store)))]
    (update-add-object-adjacent-to store tab-template adjacent :after true)))

(defn starting-store
  "Return an initial immutable store. If a tab name is provided, the store
  will have a single tab with that name and a table with that name."
  [tab-name]
  (let [store (add-universal-objects (new-element-store))
        [store _] (add-object store (make-non-semantic-tree-object
                                     [(list current-format :format)
                                      (list initial :unused-orderable)]))]
    (if tab-name
      (let [[tab store] (specialize-generic
                         (tab-object-template
                          [tab-name]
                          (table-object-template
                           [`(~(object-type-object tab-name))]
                           [`(~'anything (~(link-type-object '???)))]))
                         store)]
        (first (add-at-end-of-tabs store tab)))
      store)))

;;; Consistency checks

(defn column-header-problem
  "Return something truthy if the given immutable entity is a column header
   with a problem."
  [entity]
  ;; A header is a problem if it has the vacuous condition.
  (and
   (item-id? (:item-id entity))
   ;; It has universal content
   (= 'anything (content entity))
   ;; It is a column header.
   (some #(= (content %) :column-headers) (all-elements (target-entity entity)))
   ;; It has no elements, except 'amything
   (let [semantic (semantic-elements entity)]
     (or (empty? semantic)
         (and (empty? (rest semantic))
              (= (content (first semantic)) 'anything)
              (empty? (semantic-elements (first semantic))))))))

(defn abandon-problem-changes
  "Given an old store, a new store, both immutable, and an id where
   changes were made, return the new store if the changes don't have any
   problems, otherwise the old store."
  [old-store new-store id]
  (if (and (satisfies? DirectedItemId id)
           ;; No matter which orientation the id has, we examine the
           ;; forward orientation of the underlying link.
           (let [revised-entity (id->entity (store-item-id id) new-store)]
             (or (column-header-problem revised-entity)
                 (column-header-problem (target-entity revised-entity)))))
    old-store
    new-store))

;;; CSV file importing

(defn add-rows
  "Given a sequence of rows, each of which is a sequence of values,
  add data corresponding to them to the store, each as a new top-level
  object satisfying the row-template (which must be an object).
  Return the store and the column header names."
  [store rows row-template]
  (assert (object? row-template) row-template)
  (let [num-columns (apply max (map count rows))
        first-row (first rows)
        num-first (count first-row)
        first-row-is-header (and (= num-first num-columns)
                                 (every? string? first-row)
                                 (= (count (set (map clojure.string/lower-case
                                                     first-row)))
                                    num-first))
        [headers data-rows store] (if first-row-is-header
                                    [first-row (rest rows) store]
                                    (let [[headers store]
                                          (get-n-new-strings store num-columns)]
                                      [headers rows store]))
        order-element (order-element-for-item nil store)]
    [(reduce
      (fn [store row]
        (let [[store row-id] (update-add-object-adjacent-to
                              store row-template
                              order-element :before false)]
          (reduce
           (fn [store [header-value cell-value]]
             (first (update-add-element-adjacent-to
                     store row-id
                     ;; TODO: This is inefficient, since it looks up
                     ;; the header link-type-object every
                     ;; time. Instead, make a list of all the header
                     ;; objects once, then use them here.
                     `(~cell-value (~(link-type-object header-value)))
                     order-element :before false)))
           store
           (map vector headers row))))
      store
      data-rows)
     headers]))

(defn add-table-tab
  "Given a sequence of header names, add a tab with a table consisting of those
  headers."
  [store table-name headers]
  (first (add-at-end-of-tabs
          store
          (tab-object-template
           [table-name]
           (table-object-template
            [`(~(object-type-object table-name))]
            (map (fn [header]
                   `(~'anything (~(link-type-object header))))
                 headers))))))

(defn item-tab-name
  "Return a name to use for the tab title for an item to be examined.
  A string name longer than 12 characters is truncated to 10."
  [item]
  (let [item-content (if (object? item) item (content item))
        tab-name (cond (or (string? item-content) (number? item-content))
                       item-content
                       (object? item-content)
                       (content (first (label->elements item-content
                                                        name-label))))]
    (cond-> tab-name
      (and (string? tab-name) (> (count tab-name) 12)) (subs 0 10))))

(defn add-item-tab
  "Add a tab, after the last tab, whose topic is the item with the given
  id. The item gets a non-semantic element, marked :tab-topic, whose
  content is the tab. Return the updated store and the id of the new
  tab."
  [store id]
  (let [tab-name (item-tab-name (id->entity id store))
        [store tab-id] (add-at-end-of-tabs
                        store
                        (tab-object-template (when tab-name [tab-name]) nil))
        [store _] (add-element store id `(~(id->entity tab-id store)
                                          :tab-topic))]
    [store tab-id]))

(defn- tab-topic-known?
  "Return whether the topic of the tab still exists, and something is
  known about it besides the link that makes it the topic."
  [tab]
  (when-let [topic (tab-topic tab)]
    (or (element? topic)
        (seq (containing-elements topic))
        (seq (remove #(seq (content->elements % :tab-topic))
                     (forward-elements topic))))))

(defn- table-has-tab?
  "Return whether the table is the topic of some tab."
  [table]
  (some #(seq (content->elements % :tab-topic)) (forward-elements table)))

(defn tabs-cleanup
  "Return the store with every tab removed whose topic is an item that
  no longer exists or is an object about which nothing else is known,
  and with every table removed that is no longer the topic of a tab.
  Removing the tab also removes the link that made the item its topic."
  [store]
  (as-> store store
    (reduce (fn [store tab]
              (cond-> store
                (not (tab-topic-known? (id->entity (:item-id tab) store)))
                (remove-entity-by-id (:item-id tab))))
            store
            (matching-items tab-query store))
    (reduce (fn [store table]
              (cond-> store
                (not (table-has-tab? (id->entity (:item-id table) store)))
                (remove-entity-by-id (:item-id table))))
            store
            (matching-items (make-tree-object [:table]) store))))

(defn add-table
  "Given a sequence of rows, each a sequence of values,
  add a table corresponding to them to the store, with its own tab."
  [store table-name rows]
  (let [rows-template (make-tree-object
                       [`(~(object-type-object table-name))])
        [store headers] (add-rows store rows rows-template)]
    (add-table-tab store table-name headers)))

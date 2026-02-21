(ns cosheet2.server.order-utils
  (:require
   (cosheet2
    [orderable :refer [split earlier? initial]]
    [reporter :refer [reporter-data set-value! set-attendee!
                      inform-attendees data-attended? remove-attendee!
                      new-reporter reporter-value reporter?
                      valid? validity-category]]
    [calculator :refer [modify-and-act! propagate-calculator-data!
                        update-to-invalid]]
    [store :refer [update-source add-link declare-temporary-id
                   target-label->ids id->source ImmutableStore
                   get-new-object-id]]
    [entity :refer [content elements label->elements label->content
                    id->entity object? entity-complexity
                    make-object-list content->elements name-label]]
    [query :refer [matching-items special-form? extended-by?]]
    [store-utils :refer [add-element find-object-by-name remove-entity-by-id]]
    [expression :refer [expr-let expr-seq]]
    [utils :refer [thread-map with-latest-value update-new-further-action
                   prewalk-seqs extract-first]]
    [task-queue :refer [add-task-with-priority]])))

;;; Utilities for creating and using orders.

;;; For purposes of comparing two entities, not all of their elements
;;; matter. In particular, order information, or other information
;;; about how to display the elements, is considered irrelevant for
;;; matching a condition. We call the elements that do matter the
;;; semantic elements.
;;; Logically, semantic-element? would make more sense in model-utils,
;;; but our update-add-element-adjacent-to needs semantic-element?, to
;;; know what parts need order information. And model-utils imports
;;; update-add-element-adjacent-to. So semantic-element? has to go here,
;;; or in what would be its own file, practically.
(defn semantic-element?
  "Return true if an item counts as semantic information."
  [immutable-entity]
   (let [cont (content immutable-entity)]
     (or (string? cont)
         (number? cont)
         (object? cont)
         (#{:label :category :name 'anything} cont))))

(defn orderable-comparator
  "Compare two sequences each of whose first element is an orderable."
  [a b]
  (earlier? (first a) (first b)))

(defn sort-by-order
  "Given a seq of things and a parallel seq of their order information,
  sort the things by the corresponding order information."
  [things order-info]
  (->> (map (fn [order thing]
              ;; It is possible to not have order information,
              ;; especially temporarily while information is being
              ;; propagated. Tolerate that.
              (vector (or order initial) thing))
            order-info things)
       (sort orderable-comparator)
       (map second)))

(defn ordered-ids
  "Return the ids in the correct order, based on their order data."
  [ids immutable-store]
  (let [order-info (map #(-> %
                             (id->entity immutable-store)
                             (label->content :order))
                        ids)]
    (sort-by-order ids order-info)))

(defn ordered-entities
  "Return the immutable entities in the proper sort order."
  [entities]
  (if (empty? (rest entities))
    entities
    (let [order-info (map #(label->content % :order) entities)]
      (sort-by-order entities order-info))))

(defn order-recursively
  "Return the list form of the immutable entity with the elements at
  each level ordered."
  [entity]
  (if (elements entity)
    (cons (content entity)
          (ordered-entities (map order-recursively (elements entity))))
    entity))

;;; The next few functions implement a reporter that orders a set of
;;; ids, updating as either the set membership or their order
;;; information changes.

(def ordered-ids-callback)

(defn ordered-ids-register-and-calculate
  "Register for the store changes that we care about, and compute our value."
  ;; We do all this in the same function because both rely on some of
  ;; the same information: what the order elements are for each id.
  [reporter]
  (let [data (reporter-data reporter)]
    (when (data-attended? data)
      (with-latest-value [immutable-ids (reporter-value (:ids data))]
        (when (valid? immutable-ids)
          (with-latest-value [immutable-store (reporter-value (:store data))]
            (let [immutable-ids (seq immutable-ids)
                  order-ids (map #(first (target-label->ids
                                          immutable-store % :order))
                                 immutable-ids)
                  ;; If an item has an order element, we can watch
                  ;; that. Otherwise, we have to watch the entire item,
                  ;; to see if an order element appears.
                  ids-to-watch (map #(or %1 %2) order-ids immutable-ids)]
              ;; Now we only need to be attending to the ids that affect
              ;; the order.
              ;; By doing the set-attendee! inside with-latest-value
              ;; we make sure that we won't miss a change between our
              ;; computation and the registration kicking in.
              ;; We don't have to unattend to any old ids, as the
              ;; store only keeps one set of catagories for each id
              ;; (our reporter).
              (set-attendee!
               (:store data)
               reporter
               (+ (:priority data) 1)
               (when (seq ids-to-watch)
                 (conj ids-to-watch validity-category))
               ordered-ids-callback)
              (let [order-info (map
                                ;; It is possible for an item not to
                                ;; have order information, especially
                                ;; temporarily while an entity is being
                                ;; added. Tolerate that.
                                #(if % (id->source immutable-store %) initial)
                                order-ids)
                    ordered (sort-by-order immutable-ids order-info)]
                (set-value! reporter ordered)))))))))

(defn ordered-ids-callback
  [& {reporter :key}]
  (let [data (reporter-data reporter)
        cd (:calculator-data data)]
    (modify-and-act!
     reporter
     (fn [data]
       (-> data
           (update-to-invalid)
           (update-new-further-action inform-attendees reporter #{} #{})
           (update-new-further-action
            add-task-with-priority (:queue cd) (:priority data)
            ordered-ids-register-and-calculate reporter))))))

(defn do-ordered-ids-calculate
  [reporter cd]
  (modify-and-act!
   reporter
   (fn [data]
     (let [{:keys [store ids id->order id->order-element calculator-data]} data]
       (if (data-attended? data)
         (-> data
             (assoc :dependent-depth 1)
             (update-new-further-action
              propagate-calculator-data! ids calculator-data)
             (update-new-further-action
              set-attendee!
              ids reporter (+ (:priority data) 1) ordered-ids-callback)
             (update-new-further-action
            add-task-with-priority (:queue cd) (:priority data)
            ordered-ids-register-and-calculate reporter))
         (-> data
             (update-to-invalid)
             (update-new-further-action remove-attendee! store reporter)
             (update-new-further-action remove-attendee! ids reporter)))))))

(defn ordered-ids-calculator
  [reporter cd]
  (add-task-with-priority
   (:queue cd) (:priority (reporter-data reporter))
   do-ordered-ids-calculate reporter cd))

(defn ordered-ids-R
  "Make a reporter that takes a mutable list of ids, and a mutable
  store, and returns the ids in correct order."
  [ids store]
  (if (and (satisfies? ImmutableStore store)
           (not (reporter? ids)))
    (ordered-ids ids store)
    ;; We don't cache any information from the store, since it is pretty
    ;; efficient to get what we need whenever we need to recompute. The
    ;; main point of this reporter is to avoid recomputing when it isn't
    ;; necessary.
    (new-reporter :calculator ordered-ids-calculator
                  :ids ids
                  :store store)))

(defn orderable-entity?
  "Return whether this entity should get an order position."
  [entity]
  (and (semantic-element? entity) (not (keyword? (content entity)))))

;;; TODO: !!! Delete once this code is moved to model_utils, which already
;;;           define this.
(defn fixed-term-to-template
  "Given a fixed-term, turn it into a template by removing any (nil :order),
   removing any negations, and replacing any nil by the specified replacement,
   which defaults to the empty string."
  ([query]
   (fixed-term-to-template query ""))
  ([query nil-replacement]
   (prewalk-seqs (fn [query] (cond (nil? query)
                                   nil-replacement
                                   (seq? query)
                                   (remove #(or (= % '(nil :order))
                                                (special-form? %))
                                           query)
                                   true
                                   query))
                 query)))

;;; TODO: !!! Move this stuff, up to and including
;;;           update-add-element-with-order-and-temporary to model_utils
(defn match-terms-and-targets
  "Given a sequence of fixed terms and a sequence of targets, make the
  best possible pairing of fixed terms with targets that extend
  them. Return a seq of the matched pairs, a seq of the unpaired fixed
  terms and a seq of the unpaired targets."
  [fixed-terms targets]
  ;; We order the terms starting from highest complexity
  ;; (hardest to find an extension for), and the object elements
  ;; starting from lowest complexity (hardest to be an
  ;; extension). This way, when we start choosing matches, and
  ;; there is a choice, we take ones that are least likely to
  ;; preclude subsequent matches.
  (let [sorted-fixed-terms (->> fixed-terms (sort-by entity-complexity) reverse)
        sorted-targets (->> targets (sort-by entity-complexity))
        [pairs unmatched-terms unmatched-targets]
        (reduce
         ;; We go through each fixed term.
         (fn [[pairs unmatched-terms unmatched-targets] fixed-term]
           (let [[matching-target remaining-targets]
                 (extract-first #(extended-by? fixed-term %) unmatched-targets)]
             (if matching-target
               [(conj pairs [fixed-term matching-target])
                unmatched-terms
                remaining-targets]
               [pairs
                (conj unmatched-terms fixed-term)
                unmatched-targets])))
         [[] sorted-targets]
         sorted-fixed-terms)]))

(defn elements-to-add-to-satisfy-fixed-term-elements
  "Given a fixed-term and a stored object, return templates for elements
  that must be added to the stored object, and ids of any elements
  that may be removed from it, in order to make the object have as few
  elements as possible and still satisfy:
     * The fixed-term.
     * All positive queries that the original object satisfies.
     * No positive queries that aren't satisfied by an object
       consisting of the union of the elements of the original object
       and of the template.
  The last condition implies that you can't merge two elements from
  the two arguments, as that could satisfy a query the union doesn't
  satisfy. In particular, you can't match a template nil to a content
  from the original object, because that acts like a merge. Instead,
  you have to start with the union of the elements of the two
  arguments, but you can remove an element from the union if it is
  extended by an element from the other argument.
  Return a pair of a seq of templates to add, and a seq of ids of
  elements of the object to remove."
  [fixed-term object]
  (assert object? fixed-term)
  (assert object? object)
  (let [;; First find elements that extend targets. We will need to add the
        ;; un-matched targets.
        [_ unmatched-term-elements unmatched-object-elements]
        (match-terms-and-targets (remove (special-form? (elements fixed-term)))
                                 (elements object))
        templates-to-add (map fixed-term-to-template unmatched-term-elements)
        ;; Now find unmatched targets that extend unmatched
        ;; elements. We won't need the elements that are extended.
        [object-term-pairs _ _]
        (match-terms-and-targets unmatched-object-elements templates-to-add)]
    [templates-to-add (map #(:item-id (first %)) object-term-pairs)]))

(def update-add-element-with-order-and-temporary)

(defn add-elements-with-order
  [store target-id elements order position]
  (let [[s id order]
        (reduce (fn [[store _ order] element]
                  (update-add-element-with-order-and-temporary
                   store target-id element order position false))
                [store nil order]
                (case position
                  :before elements
                  ;; If we are adding them after the current order
                  ;; chunk, then each one is before the previous
                  ;; one, as the chunk shrinks.
                  :after (reverse elements)))]
    [s order]))

(defn get-or-make-ordered-object-by-name
  "Find or make an object with the given name, and satisfying the
  fixed-term. If an object is found, add elements to it if necessary
  to make it satisfy the template, and remove elements that are
  rendered redundant. Return the new store the id of the matching
  object, and the unused part of the order."
  [store name fixed-term order position]
  (assert object? fixed-term)
  (if-let [object (find-object-by-name store name fixed-term)]
    (let [object-id (:item-id object)
          [terms-to-add ids-to-remove]
          (elements-to-add-to-satisfy-fixed-term-elements fixed-term object)
          [store order] (add-elements-with-order
                         store object-id terms-to-add order false)
          store (reduce remove-entity-by-id store ids-to-remove)]
      [store object-id order])
    ;; Remove any existing name in the template, replacing it with
    ;; the name we are looking for.
    (let [object-elements (-> (remove #(seq (content->elements % name-label))
                              (elements fixed-term))
                              (conj `(~name (~name-label))))
          [store object-id] (get-new-object-id store)
          [store order] (add-elements-with-order
                         store object-id object-elements order false)]
      [store object-id order])))

;;; TODO: !!! Make the following use get-or-make-ordered-object-by-name

(defn update-add-element-with-order-and-temporary
  "Add an element, described in list form, to the store, with the given
  target.  Add ordering information to the element and each part of it,
  except for :label or :category specifiers and non-semantic elements,
  splitting the provided order for the orders, and returning an unused
  piece of it.  Put the new entity in the specified position (:before
  or :after) of the returned order, and make the entity use the bigger
  piece if use-bigger is true, otherwise use the smaller piece.  If
  the template has a :temporary element, mark it temporary in the store.
  Return the new store, the id of the item, and the remaining order."
  [store target-id template order position use-bigger]
  (let [template-content (content template)
        template-elements (elements template)
        temporary (some (fn [element] (= (content element) :temporary))
                        template-elements)]
    (if (not (orderable-entity? template))
      (let [[s1 id] (add-element store target-id template)]
        [s1 id order])
      (let [value-to-store template-content
            ;; TODO: !!! Put call to get-or-make-ordered-object-by-name.
            [s1 id] (add-link store target-id value-to-store)
            ;; The next bunch of complication is to split the order up
            ;; the right way in all cases. First, we split it into a
            ;; bigger and a smaller part, putting the bigger part in
            ;; correct position. Then, when we recursively add the
            ;; elements, we take their order from the bigger position,
            ;; leaving most of the space on the bigger position. Finally,
            ;; we use the appropriate position for the entity and the
            ;; return value.
            entity-order-index (case position :before 0 :after 1)
            other-position ([:after :before] entity-order-index)
            split-order (split order (if use-bigger position other-position))
            bigger-index (if use-bigger
                           entity-order-index
                           (- 1 entity-order-index))
            bigger-order (split-order bigger-index)
            smaller-order (split-order (- 1 bigger-index))
            [s2 bigger-order]
            (add-elements-with-order
             s1 id template-elements bigger-order position)
            [s3 _] (add-element
                    s2 id `(~(if use-bigger bigger-order smaller-order)
                            :order))]
        [(if temporary (declare-temporary-id s3 id) s3)
         id
         (if use-bigger smaller-order bigger-order)]))))

(defn furthest-item
  "Given a list of items and a position,
  return the furthest item in that position."
  [items position]
  (cond
    (empty? items) nil
    (= (count items) 1) (first items)
    true (second
          (reduce (case position
                    :before (fn [a b] (if (earlier? (first a) (first b)) a b))
                    :after (fn [a b] (if (earlier? (first a) (first b)) b a)))
                  (map (fn [item] [(label->content item :order) item])
                       items)))))

(defn furthest-element [item position]
  "Return the furthest element of the item, in the direction of the position.
   If the item has no ordered elements, return the item."
  (let [candidates (filter (fn [element] (label->content element :order))
                           (elements item))]
    (if candidates
      (furthest-item candidates position)
      item)))

(defn order-element-for-item
  "Return an element with the order information for item,
   or, if that is not available, for the overall store."
  [item store]
  (or (first (label->elements item :order))
      (first (matching-items '(nil :unused-orderable) store))))

;;; TODO: !!! Make this private.
(defn update-add-element-adjacent-to
  "Add an entity with the given target id and contents,
   taking its order from the given item, in the given position,
   and giving the entity the bigger piece if use-bigger is true.
   Return the updated store and the id of the entity."
  [store target-id element adjacent-to position use-bigger]
  (let [order-element (order-element-for-item adjacent-to store)
        order (content order-element)
        [store id remainder] (update-add-element-with-order-and-temporary
                              store target-id element
                              order position use-bigger)]
    [(update-source store (:item-id order-element) remainder) id]))

(defn update-add-object-adjacent-to
  "Add an object with the given contents,
   taking its order from the given item, in the given position,
   and giving the entity the bigger piece if use-bigger is true.
   Return the updated store and the id of the entity."
  [store object adjacent-to position use-bigger]
  (assert (object? object))
  (update-add-element-adjacent-to nil object adjacent-to position use-bigger))

(defn add-order-elements-internal
  "This form uses the specified order to order the elements,
   and returns the new list and the unused part of order."
  [entity order]
  (cond
    (sequential? entity)
    (let [[elements order] (thread-map add-order-elements-internal
                                       (rest entity) order)
          [before after] (split order :after)]
      [(apply list (concat [(first entity)]
                           elements
                           [`(~before :order)]))
       after])
    (orderable-entity? entity)
    (let [[before after] (split order :after)]
      [`(~entity (~before :order))
       after])
    true
    [entity order]))

(defn add-order-elements
  "Given the list form of the semantic part of an item, add order
  information to each user selectable part so they are in the same
  order as in the list form. (If order information isn't added to a
  new item, queries may fail to find it, as the presence of order
  information is how queries restrict to semantic elements."
  [entity]
  (first (add-order-elements-internal entity initial)))

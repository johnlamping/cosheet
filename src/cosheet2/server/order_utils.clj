(ns cosheet2.server.order-utils
  (:require
   (cosheet2
    [orderable :refer [split earlier? initial]]
    [reporter :refer [reporter-data set-value! set-attendee! invalid
                      inform-attendees data-attended? remove-attendee!
                      new-reporter reporter-value reporter?]]
    [calculator :refer [modify-and-act!]]
    [store :refer [update-content add-simple-item declare-temporary-id
                   id-label->element-ids id->content ImmutableStore]]
    [entity :refer [content elements label->elements label->content
                    description->entity]]
    [query :refer [matching-items]]
    [store-utils :refer [add-entity]]
    [expression :refer [expr-let expr-seq]]
    [utils :refer [thread-map with-latest-value update-new-further-action]]
    [task-queue :refer [add-task-with-priority]])))

;;; For purposes of comparing two entities, not all of their elements
;;; matter. In particular, order information, or other information
;;; about how to display the elements is considered irrelevant for
;;; matching a condition. We call the elements that matter the semantic
;;; elements.

(defn semantic-entity?
  "Return true if an item counts as semantic information."
  [immutable-entity]
   (let [cont (content immutable-entity)]
     (or (string? cont)
         (number? cont)
         (#{:label :category 'anything} cont))))

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

(defn order-ids
  "Return the ids in the correct order, based on their order data."
  [ids immutable-store]
  (let [order-info (map #())])
  (let [entities (map #(description->entity % immutable-store) ids)
        order-info (map #(label->content % :order) entities)]
    (sort-by-order ids order-info)))

(defn order-entities
  "Return the immutable entities in the proper sort order."
  [entities]
  (if (empty? (rest entities))
    entities
    (let [order-info (map #(label->content % :order) entities)]
      (sort-by-order entities order-info))))

;;; The next few functions implement a reporter that orders a set of
;;; ids, bupdating as either the set membership or their order
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
        (with-latest-value [immutable-store (reporter-value (:store data))]
          (let [immutable-ids (seq immutable-ids)
                order-ids (map #(first (id-label->element-ids
                                        immutable-store % :order))
                               immutable-ids)
                ;; If an item has an order element, we can watch
                ;; that. Otherwise, we have to watch the entire item,
                ;; to see if an order element appears.
                ids-to-watch (map #(or %1 %2) order-ids immutable-ids)]
            ;; By doing the set-attendee! inside with-latest-value we
            ;; make sure that we won't miss a change between our
            ;; computation and the registration kicking in.
            (set-attendee!
             (:store data) reporter (+ (:priority data) 1) ids-to-watch
             ordered-ids-callback)
            (let [order-info (map
                              ;; It is possible for an item not to
                              ;; have order information, especially
                              ;; temporarily while an entity is being
                              ;; added. Tolerate that.
                              #(if % (id->content immutable-store %) initial)
                              order-ids)
                  ordered (sort-by-order immutable-ids order-info)]
              (set-value! reporter ordered))))))))

(defn ordered-ids-callback
  [& {reporter :key}]
  (let [data (reporter-data reporter)
        cd (:calculator-data data)]
    (modify-and-act!
     reporter
     (fn [data]
       (-> data
           (assoc :value invalid)
           (update-new-further-action inform-attendees reporter #{} #{})
           (update-new-further-action
            add-task-with-priority (:queue cd) (:priority data)
            ordered-ids-register-and-calculate reporter))))))

(defn do-ordered-ids-calculate
  [reporter cd]
  (modify-and-act!
   reporter
   (fn [data]
     (let [{:keys [store ids id->order id->order-element]} data]
       (if (data-attended? data)
         (-> data
             (assoc :dependent-depth 1) 
             (update-new-further-action
              set-attendee!
              ids reporter (+ (:priority data) 1) ordered-ids-callback)
             (update-new-further-action
            add-task-with-priority (:queue cd) (:priority data)
            ordered-ids-register-and-calculate reporter))
         (-> data
             (assoc :value :invalid)
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
    (order-ids ids store)
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
  (and (semantic-entity? entity) (not (keyword? (content entity)))))

(defn update-add-entity-with-order-and-temporary
  "Add an entity, described in list form, to the store, with the given
  subject.  Add ordering information to each part of the entity,
  except for :label or :category specifiers and non-semantic elements,
  splitting the provided order for the orders, and returning an unused
  piece of it.  Put the new entity in the specified position (:before
  or :after) of the returned order, and make the entity use the bigger
  piece if use-bigger is true, otherwise return the bigger piece.  If
  the entity has a :temporary element, mark it temporary in the store.
  Return the new store, the id of the item, and the remaining order."
  [store subject-id entity order position use-bigger]
  (let [entity-content (content entity)
        entity-elements (elements entity)
        trans (some (fn [element] (= (content element) :temporary))
                    entity-elements)]
    (if (not (orderable-entity? entity))
      (let [[s1 id] (add-entity store subject-id entity)]
        [s1 id order])
      (let [value-to-store entity-content
            [s1 id] (add-simple-item store subject-id value-to-store)
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
            [s2 _ bigger-order]
            (reduce (fn [[store _ order] element]
                      (update-add-entity-with-order-and-temporary
                       store id element order position false))
                    [s1 nil bigger-order]
                    (case position ;; Make the order match
                      :before entity-elements
                      :after (reverse entity-elements)))
            [s3 _] (add-entity
                    s2 id `(~(if use-bigger bigger-order smaller-order)
                            :order))]
        [(if trans (declare-temporary-id s3 id) s3)
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

(defn update-add-entity-adjacent-to
  "Add an entity with the given subject id and contents,
   taking its order from the given item, in the given position,
   and giving the entity the bigger piece if use-bigger is true.
   Return the updated store and the id of the entity."
  [store subject-id entity adjacent-to position use-bigger]
  (let [order-element (order-element-for-item adjacent-to store)
        order (content order-element)
        [store id remainder] (update-add-entity-with-order-and-temporary
                              store subject-id entity
                              order position use-bigger)]
    [(update-content store (:item-id order-element) remainder) id]))

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

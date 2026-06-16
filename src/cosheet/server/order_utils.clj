(ns cosheet.server.order-utils
  (:require
   (cosheet
    [orderable :refer [split earlier? initial]]
    [reporter :refer [reporter-data set-value! set-attendee!
                      inform-attendees data-attended? remove-attendee!
                      make-reporter reporter-value-or-invalid reporter?
                      reporter-valid? validity-category]]
    [calculator :refer [modify-and-act! propagate-calculator-data!
                        update-to-invalid]]
    [store :refer [update-source add-link
                   target-label->ids id->source ImmutableStore]]
    [entity :refer [content elements orientation
                    label->elements label->content
                    id->entity make-tree-element make-tree-object
                    element? object? interned-object?]]
    [query :refer [matching-items special-form?]]
    [utils :refer [thread-map with-latest-value update-new-further-action]]
    [task-queue :refer [add-task-with-priority]])))

;;; Utilities for creating and using orders.

;;; For purposes of comparing two entities, not all of their elements
;;; matter. In particular, order information, or other information
;;; about how to display the elements, is considered irrelevant for
;;; matching a condition. We call the elements that do matter the
;;; semantic elements.
;;; Note: Logically, semantic-element? would make more sense in
;;; model-utils, but our orderable-entity? needs semantic-element?, to
;;; know what parts need order information. And model-utils imports
;;; orderable-entity?. So semantic-element? has to go here, or in what
;;; would be its own file, practically.
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
      (with-latest-value [immutable-ids (reporter-value-or-invalid (:ids data))]
        (when (reporter-valid? immutable-ids)
          (with-latest-value [immutable-store (reporter-value-or-invalid (:store data))]
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
    (make-reporter :calculator ordered-ids-calculator
                  :ids ids
                  :store store)))

(defn orderable-entity?
  "Return whether this entity should get an order position."
  [entity]
  (and (semantic-element? entity) (not (keyword? (content entity)))))

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

(def add-order-elements-to-element)

(defn add-order-elements-inside-object
  "Use the specified order to add order information to all the object's
  subparts. Return a list form for the new object and the unused part
  of order."
  [object order]
  (let [[elements remainder] (thread-map add-order-elements-to-element
                                         (elements object) order)]
    [(make-tree-object elements) remainder]))

(defn add-order-elements-to-element
  "Use the specified order to add order information to the entity, as if
  it were an element, and to all its subparts. Return a list form for
  the new entity and the unused part of order."
    [entity order]
    (cond
      (element? entity)
      (let [[elements remainder] (thread-map add-order-elements-to-element
                                             (rest entity) order)
            contents (content entity)
            [contents remainder] (if (and (object? contents)
                                          (not (interned-object? contents)))
                                   (add-order-elements-inside-object
                                    contents remainder)
                                   [contents remainder])
            [before after] (split remainder :after)]
        [(make-tree-element (orientation entity)
                            contents
                            (concat elements [`(~before :order)]))
         after])
      (orderable-entity? entity)
      ;; We have an orderable primitive acting like an element. Turn
      ;; it into an element, with an order.
      (let [[before after] (split order :after)]
        [`(~entity (~before :order))
         after])
      true
      [entity order]))

(defn add-order-elements
  "Given the list form of semantic part of an element, add order
  information to each user selectable sub-part so they are in the same
  order as in the list form. (If order information isn't added to a
  new item, queries may fail to find it, as the presence of order
  information is how queries restrict to semantic elements."
  [entity]
  (first (add-order-elements-to-element entity initial)))

(ns cosheet.server.order-utils
  (:require
   (cosheet
    [orderable :refer [split earlier? initial]]
    [store :refer [update-content add-simple-element declare-temporary-id]]
    [entity :refer [content elements label->elements label->content]]
    [query :refer [matching-items]]
    [store-utils :refer [add-entity]]
    [expression :refer [expr-let expr-seq]]
    [utils :refer [thread-map]])))

;;; This is copied from model_utils to avoid a circular dependency
(defn semantic-element?
  "Return true if an element counts as semantic information."
  [immutable-entity]
   (let [cont (content immutable-entity)]
     (or (string? cont)
         (number? cont)
         (#{:tag 'anything 'anything-immutable} cont))))

(defn orderable-comparator
  "Compare two sequences each of whose first element is an orderable."
  [a b]
  (earlier? (first a) (first b)))

(defn order-items-R
  "Return the items in the proper sort order."
  [items]
  (if (empty? (rest items))
    items
    (expr-let [order-info
               (expr-seq map #(label->content % :order) items)]
      (map second (sort orderable-comparator
                        (map (fn [order item]
                               ;; It is possible for an item not to have
                               ;; order information, especially
                               ;; temporarily while information is being
                               ;; propagated. Tolerate that.
                               (vector (or order initial) item))
                             order-info items))))))

(defn update-add-entity-with-order-and-temporary
  "Add an entity, described in list form, to the store, with the given
   subject.  Add ordering information to each part of the entity,
   except for tag specifiers and non-semantic elements, splitting the
   provided order for the orders, and returning an unused piece of it.
   Put the new entity in the specified position (:before or :after) of
   the returned order, and make the entity use the bigger piece if
   use-bigger is true, otherwise return the bigger piece.
   If the entity has a :temporary element, mark it temporary in the store. 
   Return the new store, the id of the item, and the remaining order."
  [store subject-id entity order position use-bigger]
  (let [entity-content (content entity)
        entity-elements (elements entity)
        trans (some (fn [element] (= (content element) :temporary))
                    entity-elements)]
    ;; Keyword markers and non-semantic elements don't get an ordering.
    (if (or (not (semantic-element? entity))
            (= entity :tag))
      (let [[s1 id] (add-entity store subject-id entity)]
        [s1 id order])
      (let [value-to-store entity-content
            [s1 id] (add-simple-element store subject-id value-to-store)
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
                            :order :non-semantic))]
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
                           [`(~before :order :non-semantic)]))
       after])
    (or (nil? entity) (= entity :tag))
    [entity order]
    true
    (let [[before after] (split order :after)]
      [`(~entity (~before :order :non-semantic))
       after])))

(defn add-order-elements
  "Given the list form of the semantic part of an item, add order
  information to each user selectable part. Otherwise when a referent
  is instantiated with respect to the item, elements won't match, being
  assumed to be non-semantic."
  [entity]
  (first (add-order-elements-internal entity initial)))

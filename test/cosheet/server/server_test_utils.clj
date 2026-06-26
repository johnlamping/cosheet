(ns cosheet.server.server-test-utils
  (:require (cosheet
             [orderable :refer [split initial]]
             [reporter :refer [reporter-value-or-invalid]]
             [calculator :refer [make-calculator-data computation-value
                                 request compute]]
             [task-queue :refer [make-priority-task-queue]]
             [store :refer [new-mutable-store]]
             [entity :refer [content elements orientation
                             element? object? tree-object? interned-object?
                             make-tree-element make-tree-object-copying-id
                             stored-entity? presumed-interned-object?]]
             [utils :refer [threaded-map]]
             mutable-store-impl)
            (cosheet.server
             [order-utils :refer [orderable-entity?]])))

(defn run-renderer
  "Run the renderer, then run the resulting dom-R, to get the final
  dom."
  [renderer spec mutable-store]
  (let [queue (make-priority-task-queue 0)
        cd (make-calculator-data queue)
        dom-R (renderer spec mutable-store)]
    (computation-value dom-R cd)))

(def add-order-elements-to-element)

(defn add-order-elements-inside-object
  "Use the specified order to add order information to all the object's
  subparts. Return a tree form for the new object and the unused part
  of order."
  [object order]
  (assert (tree-object? object))
  (let [[elements remainder] (threaded-map add-order-elements-to-element
                                         (elements object) order)]
    [(make-tree-object-copying-id object elements) remainder]))

(defn add-order-elements-to-element
  "Use the specified order to add order information to the entity, as if
  it were an element, and to all its subparts. Return a list form for
  the new entity and the unused part of order."
    [entity order]
    (when (stored-entity? entity)
      (assert (presumed-interned-object? entity)))
    (cond
      (element? entity)
      (let [[elements remainder] (threaded-map add-order-elements-to-element
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
  information is how queries restrict to semantic elements.

  Since this function doesn't return the unused order, it doesn't make
  sense for adding elements to a normal store. It is only called to do
  set-up in unit testss"
  [entity]
  (first (add-order-elements-to-element entity initial)))

(ns cosheet2.store-utils
  (:require (cosheet2 [store :refer [add-simple-item remove-simple-item
                                     id->content id->element-ids
                                     id->containing-ids
                                     StoredItemDescription]])))

(defn- items-to-add
  "Return a seq of items, described as [subject content], to add
   to accomplish the addition of an entity with the given subject
   and with elements and content equal to the target.
   Also return the item that corresponds to the entity.
   Subjects or contents may reference earlier elements in the seq,
   and should be replaced by the id of the added element."
  [subject-id target]
  (let [compound? (fn [entity]
                    ;; We use ISeq, because, for example, while '(1 2)
                    ;; is a PersistentList, `(1 2) is a Cons.
                    (instance? clojure.lang.ISeq entity))]
    (if (compound? target)
      (let [[content-elements content-entity]
            (let [content (first target)]
              (if (compound? content)
                (items-to-add nil content)
                [[] content]))
            entity-item [subject-id content-entity]]
        [(apply concat
                content-elements
                [entity-item]
                (map (fn [element] (first (items-to-add entity-item element)))
                     (rest target)))
         entity-item])
      (let [entity-item [subject-id target]]
        [[entity-item] entity-item]))))

(defn add-entity
  "Add an entity, given in list form, to the store, with content
   and elements equal to the target, and with the given subject.
   Return the new store and the id of the item for the entity."
  [store subject-id target]
  (let [[items entity-item] (items-to-add subject-id target)
        [store item->id]
        (reduce (fn [[store item->id] item]
                  (let [mapped-item (map #(or (item->id %) %) item)
                        [added-store id] (apply add-simple-item
                                                store mapped-item)]
                    [added-store (assoc item->id item id)]))
                [store {}]
                items)]
    [store (item->id entity-item)]))

(defn- items-to-remove
  "Return a list of ids of items to remove in order to remove the
  entity with the given id, and all its elements.  Return the list in
  an order suitable for removing."
  [store id]
  (let [content (id->content store id)
        element-removals (mapcat (partial items-to-remove store)
                                 (id->element-ids store id))]
    (concat element-removals
            [id]
            (when (and (satisfies? StoredItemDescription content)
                       (every? (conj (set element-removals) id)
                               (id->containing-ids store content)))
              ;; The content is an item that nobody else holds,
              ;; so remove it too.
              (items-to-remove store content)))))

(defn remove-entity-by-id
  "Remove the entity with the given id, and all its elements and content."
  [store id]
  (reduce (fn [store id] (remove-simple-item store id))
          store (items-to-remove store id)))


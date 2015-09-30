(ns cosheet.store-utils
  (:require [cosheet.store :refer :all]
            [cosheet.entity :as entity]))

(defn- items-to-add
  "Return a seq of items, described as [subject content], to add
   to accomplish the addition of an entity with the given subject
   and with elements and content equal to the target.
   Also return the item that corresponds to the entity.
   Subjects or contents may reference earlier elements in the seq,
   and should be replaced by the id of the added element."
  [subject-id target]
  (let [referable? (fn [entity] (or (satisfies? StoredItemDescription entity)
                                    (entity/atom? entity)))]
    (if (referable? target)
      (let [entity-item [subject-id target]]
        [[entity-item] entity-item])
      (let [[content-elements
             content-entity] (let [content (entity/content target)]
                               (if (referable? content)
                                 [[] content]
                                 (items-to-add nil content)))
            entity-item [subject-id content-entity]]
        [(apply concat
                content-elements
                [entity-item]
                (map (fn [element] (first (items-to-add entity-item element)))
                     (entity/elements target)))
         entity-item]))))

(defn add-entity
  "Add an entity to the store, with content and elements
   equal to the target, and with the given subject.
   Return the new store and the id of the item for the entity."
  [store subject-id target]
  (let [[items entity-item] (items-to-add subject-id target)
        [store item->id]
        (reduce (fn [[store item->id] item]
                  (let [mapped-item (map #(or (item->id %) %) item)
                        [added-store id] (apply add-simple-element
                                                store mapped-item)]
                    [added-store (assoc item->id item id)]))
                [store {}]
                items)]
    [store (item->id entity-item)]))

(defn add-entity!
  "Add an entity to the mutable store, with content and elements
   equal to the target, and with the given subject.
   Return the id of the item for the entity."
  [store subject-id target]
  (let [[items entity-item] (items-to-add subject-id target)
        item->id
        (reduce (fn [item->id item]
                  (let [mapped-item (map #(or (item->id %) %) item)
                        id (apply add-simple-element! store mapped-item)]
                    (assoc item->id item id)))
                {}
                items)]
     (item->id entity-item)))

(defn- items-to-remove
  "Return a list of ids of items to remove to remove the entity with
   the given id, and all its elements.
   Return the list in an order suitable for removing."
  [store id]
  (let [content (id->content store id)
        element-removals (mapcat (partial items-to-remove store)
                                 (id->element-ids store id))]
    (concat element-removals
            [id]
            (when (and (satisfies? StoredItemDescription content)
                       (not (atom-description? content))
                       (not (id-is-content? store content
                                            (into #{id} element-removals))))
              (items-to-remove store content)))))

(defn remove-entity-by-id
  "Remove the entity with the given id, and all its elements and content."
  [store id]
  (reduce (fn [store id] (remove-simple-id store id))
          store (items-to-remove store id)))

(defn remove-entity-by-id!
  "Remove the entity with the given id, and all its elements and content."
  [store id]
  (do-update! store #(remove-entity-by-id % id)))

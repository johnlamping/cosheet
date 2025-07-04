(ns cosheet2.store-utils
  (:require (cosheet2 [store :refer [add-link remove-link
                                     id->source id->element-ids
                                     id->containing-ids
                                     is-item-id?]])))

(defn- items-to-add
  "Return a seq of items, described as [target source], to add
   to accomplish the addition of an link with the given target
   and with elements and source equal to the template.
   Also return the item that corresponds to the entity.
   Targets or sources may reference earlier elements in the seq,
   and should be replaced by the id of the added element."
  [target-id template]
  (let [compound? (fn [entity]
                    ;; We use ISeq, because, for example, while '(1 2)
                    ;; is a PersistentList, `(1 2) is a Cons.
                    (instance? clojure.lang.ISeq entity))]
    (if (compound? template)
      (let [[content-elements content-entity]
            (let [content (first template)]
              (if (compound? content)
                (items-to-add nil content)
                [[] content]))
            entity-item [target-id content-entity]]
        [(apply concat
                content-elements
                [entity-item]
                (map (fn [element] (first (items-to-add entity-item element)))
                     (rest template)))
         entity-item])
      (let [entity-item [target-id template]]
        [[entity-item] entity-item]))))

(defn add-entity
  "Add an entity, given in list form, to the store, with source
   and elements equal to the template, and with the given target.
   Return the new store and the id of the item for the entity."
  [store target-id template]
  (let [[items entity-item] (items-to-add target-id template)
        [store item->id]
        (reduce (fn [[store item->id] item]
                  (let [mapped-item (map #(or (item->id %) %) item)
                        [added-store id] (apply add-link
                                                store mapped-item)]
                    [added-store (assoc item->id item id)]))
                [store {}]
                items)]
    [store (item->id entity-item)]))

(defn- links-to-remove
  "Return a list of ids of items to remove in order to remove the
  entity with the given id, and all its elements.  Return the list in
  an order suitable for removing."
  [store id]
  (let [source (id->source store id)
        element-removals (mapcat (partial links-to-remove store)
                                 (id->element-ids store id))]
    (concat element-removals
            [id]
            (when (and (is-item-id? source)
                       (every? (conj (set element-removals) id)
                               (id->containing-ids store source)))
              ;; The source is an item that nobody else holds,
              ;; so remove it too.
              (links-to-remove store source)))))

(defn remove-entity-by-id
  "Remove the entity with the given id, and all its elements and source."
  [store id]
  (reduce (fn [store id] (remove-link store id))
          store (links-to-remove store id)))


(ns cosheet2.store-utils
  (:require (cosheet2 [store :refer [add-link remove-link get-new-object-id
                                     id->source target->ids
                                     link-id?
                                     name-label-id link-type-id object-type-id]]
                      [entity :refer [StoredEntity
                                      element? object? named-object?
                                      anonymous-object?
                                      content orientation elements
                                      name-label link-type object-type]])))

;;; These are utilities for adding and removing element and object
;;; entities from the store.

(def add-element)

(defn- add-elements [store container-id elements]
  "Add the given elements, all to the given container."
  (reduce (fn [store element]
            (first (add-element store container-id element)))
          store elements))

(defn add-object
  "Add an object, given in vector form, to the store.
  Return the new store and the id of the new object."
  [store template]
  (assert object? template)
  (let [[store object-id] (get-new-object-id store)
        store (add-elements store object-id (elements template))]
    [store object-id]))

(defn add-element
  "In the store, add an element matching the template to the containing
  entity with the given id.
  Return the new store and the id of the new element."
  [store container-id template]
  (assert (not (instance? clojure.lang.PersistentVector template)))
  (assert (not (element? (content template))) template)
  (assert (not (anonymous-object? template)) template)
  (if (and (named-object? template)
           (satisfies? StoredEntity template))
    (add-link store container-id (:item-id template))
    (let [[store content-endpoint]
          (let [element-content (content template)]
            ;; If we have an expanded object, we need to make an instance of it.
            (if (instance? clojure.lang.PersistentVector element-content)
              (add-object store element-content)
              [store (if (satisfies? StoredEntity element-content)
                       (:item-id element-content)
                       element-content)]))
          [store entity-link] (apply add-link store
                                     (if (= (orientation template) :target)
                                       [content-endpoint container-id]
                                       [container-id content-endpoint]))
          store (add-elements store entity-link (elements template))]
      [store entity-link])))

(defn add-universal-objects
  "Add the 'name', 'link-type', and 'object-type' objects to the store,
  includding their names."
  [store]
  (let [[s1 _] (add-element store name-label-id `("name" (~name-label)))
        [s2 _] (add-element s1 name-label-id `(~link-type))
        [s3 _] (add-element s2 link-type-id `("label" (~name-label)))
        [s4 _] (add-element s3 link-type-id `(~object-type))
        [s5 _] (add-element s4 object-type-id `("class" (~name-label)))
        [s6 _] (add-element s5 object-type-id `(~object-type))]
    s6))

(defn- links-to-remove
  "Return a list of ids of items to remove in order to remove the
  entity with the given id, and all its elements.  Return the list in
  an order suitable for removing."
  [store id]
  (let [element-removals (mapcat (partial links-to-remove store)
                                 (target->ids store id))]
    (concat element-removals
            ;; Once everythig pointing to the element is gone, we can
            ;; remove the element.
            (if (link-id? id) [id] []))))

(defn remove-entity-by-id
  "Remove the entity with the given id, and all its elements."
  [store id]
  (reduce (fn [store id] (remove-link store id))
          store (links-to-remove store id)))


(ns cosheet.store-utils
  (:require
   (cosheet
    [store :refer [add-link remove-link get-new-object-id
                   id->source target->ids source->ids
                   link-id? object-id? interned-object-id?
                   generic-name?
                   name-label-id link-type-id object-type-id]]
    [entity :refer [StoredEntity
                    element? object? stored-entity?
                    uniquely-identified-object?
                    id-identified-object?
                    link-type-object? object-type-object? non-type-object?
                    content orientation elements to-list
                    label->elements content->elements
                    make-object-list name-label link-type object-type]]
    [query :refer [matching-items extended-by?]]
    query-impl)))

;;; These are utilities for adding and removing element and object
;;; entities from the store.

(def add-element)

(defn- add-elements [store container-id elements]
  "Add the given elements, all to the given container."
  (reduce (fn [store element]
            (first (add-element store container-id element)))
          store elements))

(defn add-object-with-given-elements
  "Add an object with the given elements. Return the revised store and
  the id of the new object."
  [store elements]
  (let [[store object-id] (get-new-object-id store)
        store (add-elements store object-id elements)]
    [store object-id]))

(defn template-type-test
  "Return a function that tests whether an object's type (link-type or
  object-type) matches that of the template."
  [template-object]
  (cond (link-type-object? template-object) link-type-object?
        (object-type-object? template-object) object-type-object?
        true non-type-object?))

(defn find-object-by-name
  "Find an object with the given name, and with the type that matches
   the template's type."
  [store name template]
  (let [query (make-object-list [`(~name (~name-label))])
        matches (matching-items query store) 
        filtered (filter (template-type-test template) matches)]
    (when (seq filtered)
      (assert (= (count filtered) 1))
      (first filtered))))

(defn get-or-make-object-by-name
  "Find or make an object with the given name, and satisfying the
  template. Throw an error an object is found that matches the name
  and template type, but doesn't satisfy the template. Return the new
  store and the id of the matching object."
  [store name template]
  (assert object? template)
  (if-let [object (find-object-by-name store name template)]
    (do (assert (extended-by? template object)
                [(map to-list (elements template))
                 (map to-list (elements object))])
        [store (:item-id object)])
    (let [;; Remove any existing name in the template, replacing it
          ;; with the name we are looking for.
          pattern (-> (remove #(seq (content->elements % name-label))
                              (elements template))
                      (conj `(~name (~name-label)))
                      make-object-list)]
      (add-object-with-given-elements store (elements pattern)))))

(defn add-object
  "Add an object to the store, unless it is uniquely identified and is
  already in the store, in which case, check that it satisfies the
  template.  Return the new store and the id of the object."
  [store template]
  (assert object? template)
  (cond (id-identified-object? template)
        [store (:item-id template)]
        (uniquely-identified-object? template)
        ;; The template doesn't have a string id, so it must have a name.
        (let [name (->> (label->elements template name-label)
                        (map content)
                        (remove generic-name?)
                        first)
              _ (assert name name)]
          (get-or-make-object-by-name store name template))
        true
        (add-object-with-given-elements store (elements template))))

(defn add-element
  "In the store, add an element matching the template to the containing
  entity with the given id.
  Return the new store and the id of the new element."
  [store container-id template]
  (assert (not (object? template)) template) ; Use add-object.
  (assert (not (element? (content template))) template)
  (let [[store content-endpoint]
        (let [element-content (content template)]
          ;; If we have an expanded object, we need to make an instance of it.
          (if (and (object? element-content)
                   (not (stored-entity? element-content)))
            (add-object store element-content)
            [store (if (stored-entity? element-content)
                     (:item-id element-content)
                     element-content)]))
        [store entity-link] (apply add-link store
                                   (if (= (orientation template) :target)
                                     [content-endpoint container-id]
                                     [container-id content-endpoint]))
        store (add-elements store entity-link (elements template))]
    [store entity-link]))

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

(defn add-link-type-object
  "Add a label object with the given name to the store.
   Return the updated store and the id of the label object."
  [store name]
  (add-object store (make-object-list [`(~name (~name-label)) `(~link-type)])))

(defn add-object-type-object
  "Add an object-type object with the given name to the store.
   Return the updated store and the id of the object-type object."
  [store name]
  (add-object store (make-object-list [`(~name (~name-label)) `(~object-type)])))

(defn- links-to-remove
  "Return a list of ids of links to remove in order to remove the entity
  with the given id, and all its elements and non-interned objects
  in their contents. Don't recurse into containing-id; our caller will
  handle that. (This prevents infinite loops when an element contains
  an object as its source.)
  Return the list in an order suitable for removing."
  [store containing-id id]
  (concat
   ;; First, we have to remove all the elements.
   (mapcat (partial links-to-remove store id)
           (cond-> (target->ids store id)
             ;; Object ids can also be sources.
             (object-id? id)
             (concat (remove #(= containing-id %)
                             (source->ids store id)))))
   ;; Once all the elements are gone, if the id is a link, we can
   ;; remove its content, if that is an object, then the link, itself.
   (when (link-id? id)
     (concat (let [content-id (id->source store id)]
               (when (and (object-id? content-id)
                          (not (interned-object-id? store content-id)))
                 (links-to-remove store id content-id)))
             [id]))))

(defn remove-entity-by-id
  "Remove the entity with the given id, and all its elements."
  [store id]
  (reduce (fn [store id] (remove-link store id))
          store (links-to-remove store nil id)))


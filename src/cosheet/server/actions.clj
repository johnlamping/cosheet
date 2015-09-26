(ns cosheet.server.actions
  (:require
   [hiccup.page :refer [html5 include-js include-css]]
   [ring.util.response :refer [response]]
   (cosheet
    [debug :refer [simplify-for-print]]
    [orderable :refer [split]]
    [store :refer [update-content add-simple-element do-update! id->subject]]
    store-impl
    mutable-store-impl
    [entity :refer [StoredEntity description->entity
                    content elements label->elements]]
    [dom-utils :refer [dom-attributes]]
    [query :refer [query-matches]]
    query-impl)
   (cosheet.server
    [dom-tracker :refer [id->key key->attributes]]
    [render :refer [visible-to-list canonicalize-list]])))

;;; TODO: validate the data coming in, so nothing can cause us to
;;; crash.

(defn item-referent? [referent]
  (not (sequential? referent)))

(defn content-referent? [referent]
  (and (sequential? referent) (= ( first referent) :content)))

(defn condition-referent? [referent]
  (and (sequential? referent) (= ( first referent) :condition)))

(defn parallel-referent? [referent]
  (and (sequential? referent) (= ( first referent) :parallel)))

(defn item-determining-referents
  "Return the elements of a key that may be needed to determine the items
   it means."
  [key]
  (vec (filter (some-fn item-referent? parallel-referent?) key)))

(defn first-primitive-referent
  "Return the first non-parallel referent of a key."
  [key]
  (let [referent (first key)]
    (if (parallel-referent? referent)
      (let [[type exemplar items] referent]
        (first-primitive-referent exemplar))
      referent)))

(defn remove-content-referent
  "If a key starts with a content referent, remove it."
  [[first & rest]]
  (cond (content-referent? first)
        rest
        (parallel-referent? first)
        (let [[type exemplar items] first]
          (vec (cons [type (remove-content-referent exemplar) items] rest)))
        true (vec (cons first rest))))

(defn item->canonical-visible
  "Return the canonical form of the visible information for the item."
  [item]
  (canonicalize-list (visible-to-list item)))

(defn visible-matching-element
  "Given the list form of visible information and an item,
  find an element of the item that matches the visible information.
  Return nil if there is no matching element."
  [store visible-info item]
  (first (filter #(= (item->canonical-visible %) visible-info)
                 (elements item))))

(defn instantiate-item-id
  "Given the id of an exemplar item and a regular item, find an element
   of the item that matches the visible information of the exemplar.
  Return nil if there is no matching element."
  [store exemplar-id item]
  (visible-matching-element
   store
   (item->canonical-visible (description->entity exemplar-id store))
   item))

(defn instantiate-exemplar
  "Given a store, an exemplar, and an item, instantiate the exemplar
  to the item, returning the sequence of items matched.
  The exemplar must have been pruned to only referents that refer to items."
  [store exemplar item]
  (assert (vector? exemplar))  ; So peek and pop take from end.
  (if (empty? exemplar)
    [item]
    (let [last-referent (peek exemplar)
          remainder (pop exemplar)]
      (if (sequential? last-referent)
        (let [[type exemplar item-ids] last-referent
              exemplar-referents (item-determining-referents exemplar)]
          (assert (= type :parallel))  ; Other complex referents filtered.
          (assert (empty? remainder))  ; Parallel referents must be first.
            (mapcat (partial instantiate-exemplar store exemplar-referents)
                    (remove nil? (map #(instantiate-item-id store % item)
                                      item-ids))))
        (let [exemplar-item (instantiate-item-id store last-referent item)]
          (if (nil? exemplar-item)
            []
            (instantiate-exemplar store remainder exemplar-item)))))))

(defn key->items
  "Return the list of items that a key describes."
  [store key]
  (let [referent (first (item-determining-referents key))]
    (cond
      (nil? referent)
      []
      (sequential? referent)
      (let [[type exemplar item-ids] referent
            exemplar-referents (item-determining-referents exemplar)]
        (assert (= type :parallel))
        (mapcat (partial instantiate-exemplar store exemplar-referents)
                (map #(description->entity % store) item-ids)))
      true
      [(description->entity referent store)])))

(defn parse-string
  "Parse user entered characters into a number if possible.
  Otherwise return the characters as a string."
  ;; NOTE: This is not compabible with ClojureScript.
  [str]
  (try (let [x (Float/parseFloat (clojure.string/trim str))
             int-x (int x)]
         (if (== x int-x) int-x x))
       (catch Exception e str)))

(defn update-set-content
  "Set the content of the item in the store provided the current content
   matches 'from'."
  [from to store item]
  (if (= (content item) (parse-string from))
    (update-content store (:item-id item) to)
    (do (println "content doesn't match" (content item) from)
        store)))

(defn update-add-entity-with-order
  "Add an entity, described in list form, to the store, with the given subject.
   Add ordering information to each part of the entity,
   except for tag specifiers, splitting the provided order for the orders,
   and returning an unused piece of it.
   Put the new entity on the specified side (:before or :after) of
   the returned order, and make the entity use the bigger piece if
   use-bigger is true, otherwise return the bigger piece.
   Return the new store and the remaining order."
  [store subject-id entity order side use-bigger]
  (let [entity-content (content entity)
        entity-elements (elements entity)]
    (println "adding entity" entity "to" subject-id)
    (if (and (= entity-content 'tag) (empty? entity-elements))
      ;; Tags markers don't get an ordering.
      (let [[s id] (add-simple-element store subject-id 'tag)]
        [s order])
      (let [[s1 id] (add-simple-element
                     store subject-id (if (nil? entity-content)
                                     ""
                                     entity-content))
            ;; The next bunch of complication is to split the order up
            ;; the right way in all cases. First, we split it into a
            ;; bigger and a smaller part, putting the bigger part on
            ;; correct side. Then, when we recursively add the
            ;; elements, we take their order from the bigger side,
            ;; leaving most of the space on the bigger side. Finally,
            ;; we use the appropriate side for the entity and the
            ;; return value.
            entity-order-index (case side :before 0 :after 1)
            other-side ([:after :before] entity-order-index)
            split-order (split order (if use-bigger side other-side))
            bigger-index (if use-bigger
                           entity-order-index
                           (- 1 entity-order-index))
            bigger-order (split-order bigger-index)
            smaller-order (split-order (- 1 bigger-index))
            [s2 bigger-order] (reduce (fn [[store order] element]
                                        (update-add-entity-with-order
                                         store id element order side false))
                                      [s1 bigger-order]
                                      (case side ;; Make the order match
                                        :before entity-elements
                                        :after (reverse entity-elements)))
            [s3 order-id] (add-simple-element
                           s2 id (if use-bigger bigger-order smaller-order))
            [s4 _] (add-simple-element s3 order-id :order)]
        [s4 (if use-bigger smaller-order bigger-order)]))))

(defn order-element-for-item
  "Return an element with the order information for item,
   or, if that is not available, for the overall store."
  [item store]
  (or (first (label->elements item :order))
      (:v (first (query-matches
                  '(:variable
                    (:v :name)
                    ((nil :unused-orderable) :condition)
                    (true :reference))
                  store)))))

(defn update-add-entity-with-order-item
  "Add an entity with the given subject id and contents,
   taking its order from the given item, in the given direction,
   and giving the entity the bigger piece if use-bigger is true."
  [store subject-id entity order-item side use-bigger]
  (let [order-element (order-element-for-item order-item store)
        order (content order-element)
        [store remainder] (update-add-entity-with-order
                           store subject-id entity
                           order side use-bigger)]
    (update-content store (:item-id order-element) remainder)))

(defn update-add-element
  "Add an entity to thhe store as new element of the given subject."
  [entity store subject]
  (update-add-entity-with-order-item
   store (:item-id subject) entity
   subject :after false))

(defn add-element-handler
  "Add a element to the item with the given client id"
  [store dom-tracker id]
  (let [key (id->key dom-tracker id)
        items (key->items store key)
        first-primitive (first-primitive-referent key)]
    (println "new element for id:" id " with key:" (simplify-for-print key))
    (println "total items:" (count items))
    (println "with content" (map content items))
    (when ((some-fn item-referent? content-referent?) first-primitive)
      (reduce (partial update-add-element "")
              store items))))

(defn set-content-handler
  [store dom-tracker id from to]
  ;; TODO: Handle deleting.
  (let [key (id->key dom-tracker id)
        first-primitive (first-primitive-referent key)
        items (key->items store key)
        to (parse-string to)]
    (println "set id:" id " with key:" (simplify-for-print key))
    (println "total items:" (count items))
    (println "with content" (map content items))
    (println "from:" from " to:" to)
    (reduce (cond ((some-fn nil? item-referent? content-referent?)
                   first-primitive)
                  (partial update-set-content from to)
                  (and (condition-referent? first-primitive) (not= to ""))
                  (partial update-add-element (cons to (rest first-primitive)))
                  true (fn [a b] a))
            store items)))

(defn update-add-sibling
  "Given an item and elements that its siblings must have, add a sibling
  in the given direction (:before or :after)"
  [sibling-elements direction store item]
  (update-add-entity-with-order-item
   store (id->subject store (:item-id item)) (cons "" sibling-elements)
   item direction true))

(defn add-sibling-handler
  "Add a sibling to the item with the given client id"
  [store dom-tracker id direction]
  (let [key (id->key dom-tracker id)
        items (key->items store key)
        first-primitive (first-primitive-referent key)
        sibling-elements (:sibling-elements
                          (key->attributes dom-tracker
                                           (remove-content-referent key)))]
    (println "sibling for id:" id " with key:" (simplify-for-print key))
    (println "total items:" (count items))
    (println "with content" (map content items))
    (println "in direction" direction)
    (println "sibling elements" sibling-elements)
    (when ((some-fn item-referent? content-referent?) first-primitive)
      (reduce (partial update-add-sibling sibling-elements direction)
              store items))))

;;; TODO: Undo functionality should be added here. It shouldn't be
;;; hard, because the updated store already has a list of changed ids.
;;; Support could even be incorporated into immutable stores.
(defn do-action
  [mutable-store dom-tracker [action-type & action-args]]
  (let [handler (case action-type
                  :set-content set-content-handler
                  :add-element add-element-handler
                  :add-sibling add-sibling-handler
                  nil)]
    (if handler
      (do-update! mutable-store
                  (fn [store] (or (apply handler store dom-tracker action-args)
                                  store)))
      (println "unknown action type:" action-type))))

(defn do-actions
  [mutable-store dom-tracker actions]
  (let [keys (sort (keys actions))]
    (doseq [key keys]
      (let [action (actions key)]
        (do-action mutable-store dom-tracker action)))))

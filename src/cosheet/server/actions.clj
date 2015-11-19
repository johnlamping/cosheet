(ns cosheet.server.actions
  (:require
   [hiccup.page :refer [html5 include-js include-css]]
   [ring.util.response :refer [response]]
   (cosheet
    [debug :refer [simplify-for-print]]
    [orderable :refer [split earlier?]]
    [mutable-set :refer [mutable-set-swap!]]
    [store :refer [update-content add-simple-element do-update-control-return!
                   id->subject id-valid?]]
    store-impl
    [store-utils :refer [remove-entity-by-id]]
    mutable-store-impl
    [entity :refer [StoredEntity description->entity
                    content elements label->elements label->content]]
    [dom-utils :refer [dom-attributes]]
    [query :refer [query-matches]]
    query-impl)
   (cosheet.server
    [dom-tracker :refer [id->key key->attributes]]
    [render :refer [visible-to-list canonicalize-list
                    item-referent prepend-to-key]])))

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

(defn remove-first-referent
  "Remove the first referent from the key."
  [[first & rest]]
  (if (parallel-referent? first)
    (let [[type exemplar items] first]
      (if (empty? exemplar)
        rest
        (vec (cons [type (remove-first-referent exemplar) items] rest))))
    rest))

(defn remove-content-referent
  "If a key starts with a content referent, remove it."
  [[first & rest]]
  (cond (content-referent? first)
        rest
        (parallel-referent? first)
        (let [[type exemplar items] first]
          (vec (cons [type (remove-content-referent exemplar) items] rest)))
        (nil? first) []
        true (vec (cons first rest))))

(defn item-ids-referred-to
  "Return all the item ids referred to by item referents in the key,
  in order from most specific to most generic
  (Only includes exemplars items from parallel referents.)"
  [key]
  (when (not (empty? key))
    (let [[first & rest] key
          rest-items (item-ids-referred-to rest)]
      (cond (parallel-referent? first)
            (let [[type exemplar items] first]
              (concat (item-ids-referred-to exemplar) rest-items))
            (item-referent? first)
            (cons first rest-items)
            true
            rest-items))))

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
  (when (id-valid? store exemplar-id)
    (visible-matching-element
     store
     (item->canonical-visible (description->entity exemplar-id store))
     item)))

(defn instantiate-exemplar
  "Given a store, an exemplar, and a function from item-id to entity,
  instantiate the exemplar with respect to the function,
  returning the sequence of items matched, or a sequence of
  groups of items if group is true.
  The exemplar must have been pruned to only referents that refer to items.
  (A non-trivial group is formed by a parallel referent with empty exemplar.)"
  [store group exemplar item-id-instantiator]
  (assert (vector? exemplar))  ; So peek and pop take from end.
  (let [last-referent (peek exemplar)
          remainder (pop exemplar)]
      (if (sequential? last-referent)
        (let [[type exemplar item-ids] last-referent
              exemplar-referents (item-determining-referents exemplar)]
          (assert (= type :parallel))  ; Other complex referents filtered.
          (assert (empty? remainder))  ; Parallel referents must be first.
          (let [instantiated-items
                (remove nil? (map item-id-instantiator item-ids))]
            (if (empty? exemplar)
              (if group
                (if (empty? instantiated-items) nil [instantiated-items])
                instantiated-items)
              (mapcat (partial instantiate-exemplar
                               store group exemplar-referents)
                      (map (fn [item] #(instantiate-item-id store % item))
                           instantiated-items)))))
        (let [exemplar-item (item-id-instantiator last-referent)]
          (cond (nil? exemplar-item) []
                (empty? remainder) (if group [[exemplar-item]] [exemplar-item])
                true (instantiate-exemplar
                      store group remainder
                      #(instantiate-item-id store % exemplar-item)))))))

(defn key->items
  "Return the list of items that a key describes."
  [store key]
  (instantiate-exemplar
   store false [(first (item-determining-referents key))]
   #(when (id-valid? store %) (description->entity % store))))

(defn key->item-groups
  "Given a key, return a list of the groups of items it describes.
   If the first element of the key is a parallel with an empty exemplar,
   there is one group for each instantiation of the items of the parallel.
  Otherwise, each item is its own group."
  [store key]
  (instantiate-exemplar
   store true [(first (item-determining-referents key))]
   #(when (id-valid? store %) (description->entity % store))))

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
   Return the new store, the id of the item, and the remaining order."
  [store subject-id entity order side use-bigger]
  (let [entity-content (content entity)
        entity-elements (elements entity)]
    (if (and (= entity-content 'tag) (empty? entity-elements))
      ;; Tags markers don't get an ordering.
      (let [[s id] (add-simple-element store subject-id 'tag)]
        [s id order])
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
            [s2 _ bigger-order] (reduce (fn [[store _ order] element]
                                          (update-add-entity-with-order
                                           store id element order side false))
                                        [s1 nil bigger-order]
                                        (case side ;; Make the order match
                                          :before entity-elements
                                          :after (reverse entity-elements)))
            [s3 order-id] (add-simple-element
                           s2 id (if use-bigger bigger-order smaller-order))
            [s4 _] (add-simple-element s3 order-id :order)]
        [s4 id (if use-bigger smaller-order bigger-order)]))))

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
   and giving the entity the bigger piece if use-bigger is true.
   Return the updated store and the id of the entity."
  [store subject-id entity order-item side use-bigger]
  (let [order-element (order-element-for-item order-item store)
        order (content order-element)
        [store id remainder] (update-add-entity-with-order
                              store subject-id entity
                              order side use-bigger)]
    [(update-content store (:item-id order-element) remainder) id]))

(defn update-add-element
  "Add an entity to the store as a new element of the given subject.
  Return the new store and the id of the new element."
  [entity store subject]
  (update-add-entity-with-order-item
   store (:item-id subject) entity
   subject :after false))

(defn reduce-update
  "Given a function from store and item to store and id, reduce it
   on the store and each of the items, returning the final store
   and the first item that came back." 
  [f store items]
  (reduce (fn [[store id] item]
            (let [[store new-id] (f store item)]
              [store (or id new-id)]))
          [store nil] items))

(defn add-element-handler
  "Add a element to the item with the given client id.
  Indicate that the new element should be selected."
  [store dom-tracker client-id]
  (let [key (id->key dom-tracker client-id)
        items (key->items store key)
        first-primitive (first-primitive-referent key)]
    (println "new element for id:" client-id
             " with key:" (simplify-for-print key))
    (println "total items:" (count items))
    (println "with content" (map content items))
    (when ((some-fn item-referent? content-referent?) first-primitive)
      (let [[store element-id] (reduce-update (partial update-add-element "")
                                               store items)]
        (if element-id
          {:store store
           :select [(prepend-to-key
                     (item-referent (description->entity element-id store))
                     (remove-content-referent key))
                    [key]]}
          store)))))

(defn update-add-sibling
  "Given an item and elements that its siblings must have, add a sibling
  in the given direction (:before or :after). Also return the id of the
  new element"
  [sibling-elements direction store item]
  (update-add-entity-with-order-item
   store (id->subject store (:item-id item)) (cons "" sibling-elements)
   item direction true))

(defn add-sibling-handler
  "Add a sibling to the item with the given client id."
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
      (let [[store element-id]
            (reduce-update (partial update-add-sibling
                                    sibling-elements direction)
                           store items)]
        (if element-id
          {:store store
           :select [(prepend-to-key
                     (item-referent (description->entity element-id store))
                     (remove-first-referent (remove-content-referent key)))
                    [key]]}
          store)))))

(defn furthest-item
  "Given a list of items and a direction,
  return the furthest item in that direction."
  [items direction]
  (if (= (count items) 1)
    (first items)
    (second
     (reduce (case direction
               :before (fn [a b] (if (earlier? (first a) (first b)) a b))
               :after (fn [a b] (if (earlier? (first a) (first b)) b a)))
             (map (fn [item] [(label->content item :order) item]) items)))))

(defn add-row-handler
  "Add a row to the item with the given client id."
    [store dom-tracker id direction]
    (let [key (id->key dom-tracker id)
          attributes (key->attributes dom-tracker (remove-content-referent key))
          row-sibling (:row-sibling attributes)
          row-elements (:row-elements attributes)
          item-groups (if row-sibling
                        (key->item-groups store row-sibling)
                        (map vector (key->items store key)))
          item-key (if row-sibling row-sibling key)
          first-primitive (first-primitive-referent (or row-sibling key))]
      (println "new row for id:" id " with key:" (simplify-for-print key))
      (println "row sibling" (simplify-for-print row-sibling))
      (println "total groups:" (count item-groups))
      (println "with content" (map #(map content %) item-groups))
      (println "in direction" direction)
      (when ((some-fn item-referent? content-referent?) first-primitive)
        (let [[store element-id]
              (reduce-update
               (partial update-add-sibling row-elements direction)
               store (map #(furthest-item % direction) item-groups))]
          (if element-id
            {:store store
             :select [(prepend-to-key
                       (item-referent (description->entity element-id store))
                       (remove-first-referent
                        (remove-content-referent item-key)))
                      [key]]}
            store)))))

(defn update-delete
  "Given an item, remove it and all its elements from the store"
  [store item]
  (remove-entity-by-id store (:item-id item)))

(defn delete-handler
  [store dom-tracker id]
  (let [key (id->key dom-tracker id)
        first-primitive (first-primitive-referent key)
        items (key->items store key)]
    (println "delete id:" id " with key:" (simplify-for-print key))
    (println "total items:" (count items))
    (println "with content" (map content items))
    (when ((some-fn item-referent? content-referent?) first-primitive)
      (reduce update-delete store items))))

(defn set-content-handler
  [store dom-tracker id from to]
  ;; TODO: Handle deleting.
  (let [key (id->key dom-tracker id)
        first-primitive (first-primitive-referent key)
        items (key->items store key)
        to (parse-string to)]
    (println "set id:" id " with key:" (simplify-for-print key))
    (println "from:" from " to:" to)
    (println "affecting" (count items) "items")
    (cond ((some-fn nil? item-referent? content-referent?) first-primitive)
          (reduce (partial update-set-content from to)
                  store items)
          (and (condition-referent? first-primitive) (not= to ""))
          (let [attributes (key->attributes dom-tracker key)
                sibling (:add-sibling attributes)
                model-entity (cons to (rest first-primitive)) ]
            (if sibling
              (let [siblings (key->items store sibling)
                    direction (:add-direction attributes)]
                (assert (= (count items) (count siblings)))
                (reduce (fn [store [subject sibling]]
                          (first (update-add-entity-with-order-item
                                  store (:item-id subject) model-entity
                                  sibling direction false)))
                        store (map vector items siblings)))
              (reduce (fn [store item]
                        (first (update-add-element model-entity store item)))
                      store items)))))) 

(defn selected-handler
  [store session-state id]
  (let [key (id->key (:tracker session-state) id)
        ids (item-ids-referred-to key)
        items (map #(description->entity % store) ids)]
    (println "Selected key" key)
    (mutable-set-swap!
     (:do-not-merge session-state)
     (fn [old]
       (if (empty? items)
         #{}
         (set (cons (first items)
                    (clojure.set/intersection (set (rest items)) old))))))))

;;; TODO: Undo functionality should be added here. It shouldn't be
;;; hard, because the updated store already has a list of changed ids.
;;; Support could even be incorporated into immutable stores.
(defn do-storage-update-action
  [handler mutable-store tracker action-args]
  (println "action-args" action-args)
  (do-update-control-return!
   mutable-store
   (fn [store]
     (let [result (apply handler store tracker action-args)]
       (if result
         (if (satisfies? cosheet.store/Store result)
           [result nil]
           (do
             (assert (map? result))
             (assert (:store result))
             [(:store result) (dissoc result store)]))
         [store nil])))))

(defn do-action
  [mutable-store session-state [action-type & action-args]]
  (if-let [handler (case action-type
                     :set-content set-content-handler
                     :add-element add-element-handler
                     :add-sibling add-sibling-handler
                     :add-row add-row-handler
                     :delete delete-handler                  
                     nil)]
    (do-storage-update-action
     handler mutable-store (:tracker session-state) action-args)
    (if-let [handler (case action-type
                       :selected selected-handler
                       nil)]
      (apply handler mutable-store session-state action-args)
      (println "unknown action type:" action-type))))

(defn do-actions
  "Run the actions, and return any additional information to be returned
  to the client"
  [mutable-store session-state actions]
  (let [keys (sort (keys actions))]
    (reduce (fn [client-info key]
              (let [action (actions key)
                    remaining (do-action mutable-store session-state action)]
                (into client-info (select-keys remaining [:select]))))
            {} keys)))

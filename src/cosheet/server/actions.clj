(ns cosheet.server.actions
  (:require
   [hiccup.page :refer [html5 include-js include-css]]
   [ring.util.response :refer [response]]
   (cosheet
    [debug :refer [simplify-for-print]]
    [utils :refer [parse-string-as-number]]
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
    [key :refer [item-referent elements-referent prepend-to-key
                 item-referent? content-referent?
                 elements-referent?
                 first-primitive-referent remove-first-primitive-referent
                 remove-content-referent item-ids-referred-to
                 key->items key->item-groups]])))

;;; TODO: validate the data coming in, so nothing can cause us to
;;; crash.

(defn update-set-content
  "Set the content of the item in the store provided the current content
   matches 'from'."
  [from to store item]
  (if (= (content item) (parse-string-as-number from))
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

(defn reduce-update-add
  "Given a function from store and data to store and id, reduce it
   on the store and each of the datum, returning the final store
   and the first item that came back." 
  [f store datums]
  (reduce (fn [[store id] data]
            (let [[store new-id] (f store data)]
              [store (or id new-id)]))
          [store nil] datums))

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
      (let [[store element-id] (reduce-update-add
                                (partial update-add-element "")
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
            (reduce-update-add (partial update-add-sibling
                                        sibling-elements direction)
                               store items)]
        (if element-id
          {:store store
           :select [(prepend-to-key
                     (item-referent (description->entity element-id store))
                     (remove-first-primitive-referent
                      (remove-content-referent key)))
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
          sibling-key (or row-sibling key)
          first-primitive (first-primitive-referent sibling-key)]
      (println "new row for id:" id " with key:" (simplify-for-print key))
      (println "row sibling" (simplify-for-print row-sibling))
      (println "total groups:" (count item-groups))
      (println "with content" (map #(map content %) item-groups))
      (println "in direction" direction)
      (when ((some-fn nil? item-referent? content-referent?) first-primitive)
        (let [[store element-id]
              (reduce-update-add
               (partial update-add-sibling row-elements direction)
               store (map #(furthest-item % direction) item-groups))]
          (if element-id
            {:store store
             :select [(prepend-to-key
                       (item-referent (description->entity element-id store))
                       (remove-first-primitive-referent
                        (remove-content-referent sibling-key)))
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
        to (parse-string-as-number to)]
    (println "set id:" id " with key:" (simplify-for-print key))
    (println "from:" from " to:" to)
    (cond ((some-fn nil? item-referent? content-referent?) first-primitive)
          (reduce (partial update-set-content from to)
                  store (key->items store key))
          (and (elements-referent? first-primitive) (not= to ""))
          (let [items (key->items store (remove-first-primitive-referent key))
                attributes (key->attributes dom-tracker key)
                sibling-key (:add-sibling attributes)
                [_ subject condition] first-primitive
                model-entity (cons to (rest condition))
                [new-store new-id]
                (if sibling-key
                  (let [siblings (key->items store sibling-key)
                        direction (:add-direction attributes)]
                    (assert (= (count items) (count siblings)))
                    (reduce-update-add
                     (fn [store [subject sibling]]
                       (update-add-entity-with-order-item
                        store (:item-id subject) model-entity
                        sibling direction false))
                     store (map vector items siblings)))
                  (reduce-update-add
                   (fn [store item]
                     (update-add-element model-entity store item))
                   store items))]
            {:store new-store
             :select [(prepend-to-key
                       (item-referent (description->entity new-id store))
                       ;; Remove the condition, unless it is a tag condition.
                       (if (= condition [nil 'tag])
                         key
                         (remove-first-primitive-referent key)))
                      [key]]})))) 

(defn selected-handler
  [store session-state id]
  (let [key (id->key (:tracker session-state) id)
        ids (item-ids-referred-to key)
        items (map #(description->entity % store) ids)]
    (println "Selected key" key)
    (mutable-set-swap!
         (:do-not-merge session-state)
         (fn [old]
           (if (item-referent? (first-primitive-referent
                                (remove-content-referent key)))
               (set (cons (first items)
                          (clojure.set/intersection (set (rest items)) old)))
               (clojure.set/intersection (set items) old))))))

;;; TODO: Undo functionality should be added here. It shouldn't be
;;; hard, because the updated store already has a list of changed ids.
;;; Support could even be incorporated into immutable stores.
(defn do-storage-update-action
  "Do an action that can update the store. The action is given the
  store, the dom_tracker, and any additional arguments, and can either
  return nil, meaning no change, a new store, or a map with a :store
  value and any additional commands it wants to request of the
  client (currently, only :select, whose value is the key of the dom
  it wants to be selected)."
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

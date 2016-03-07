(ns cosheet.server.actions
  (:require
   [hiccup.page :refer [html5 include-js include-css]]
   [ring.util.response :refer [response]]
   (cosheet
    [debug :refer [simplify-for-print]]
    [utils :refer [parse-string-as-number thread-map thread-recursive-map]]
    [orderable :refer [split earlier?]]
    [mutable-set :refer [mutable-set-swap!]]
    [store :refer [update-content add-simple-element do-update-control-return!
                   id->subject id-valid?]]
    [store-impl :refer [get-unique-id-number]]
    [store-utils :refer [remove-entity-by-id]]
    mutable-store-impl
    [entity :refer [StoredEntity description->entity
                    content elements label->elements label->content to-list]]
    [dom-utils :refer [dom-attributes]]
    [query :refer [matching-items]]
    query-impl)
   (cosheet.server
    [dom-tracker :refer [id->key key->attributes]]
    [key :refer [item-referent elements-referent prepend-to-key
                 item-referent? content-location-referent?
                 elements-referent?
                 first-primitive-referent remove-first-primitive-referent
                 remove-content-location-referent remove-comments
                 item-ids-referred-to
                 key->items key->item-groups]])))

;;; TODO: validate the data coming in, so mistakes won't cause us to
;;; crash.

(defn update-set-content
  "Set the content of the item in the store provided the current content
   matches 'from'."
  [from to store item]
  ;; There are two special cases to match: If we have a number,
  ;; the user will have a string. If we have an anonymous symbol, the user
  ;; will have "???".
  (if (= (parse-string-as-number from)
         (let [content (content item)]
           (if (and (symbol? content) (= (subs (str content) 0 3) "???"))
             "???"
             content)))
    (update-content store (:item-id item) to)
    (do (println "content doesn't match" (content item) from)
        store)))

(defn update-add-entity-with-order
  "Add an entity, described in list form, to the store, with the given subject.
   Add ordering information to each part of the entity,
   except for tag specifiers, splitting the provided order for the orders,
   and returning an unused piece of it.
   Put the new entity in the specified position (:before or :after) of
   the returned order, and make the entity use the bigger piece if
   use-bigger is true, otherwise return the bigger piece.
   Return the new store, the id of the item, and the remaining order."
  [store subject-id entity order position use-bigger]
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
            [s2 _ bigger-order] (reduce (fn [[store _ order] element]
                                          (update-add-entity-with-order
                                           store id element
                                           order position false))
                                        [s1 nil bigger-order]
                                        (case position ;; Make the order match
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
      (first (matching-items '(nil :unused-orderable) store))))

(defn add-and-select
  "Run the specified function on the store and each of the arguments.
  The function must return an updated store and the id of a new item.
  Return a map of the new store and a selection request for the first
  of the new items."
  [f store arguments key-suffix, old-key]
  (let [[store element-ids] (thread-map f store arguments)]
    (if (empty? element-ids)
      store
      {:store store
       :select [(prepend-to-key
                 (item-referent (description->entity (first element-ids) store))
                 key-suffix)
                [old-key]]})))

(defn update-add-entity-adjacent-to
  "Add an entity with the given subject id and contents,
   taking its order from the given item, in the given position,
   and giving the entity the bigger piece if use-bigger is true.
   Return the updated store and the id of the entity."
  [store subject-id entity adjacent-to position use-bigger]
  (let [order-element (order-element-for-item adjacent-to store)
        order (content order-element)
        [store id remainder] (update-add-entity-with-order
                              store subject-id entity
                              order position use-bigger)]
    [(update-content store (:item-id order-element) remainder) id]))

(defn update-add-element
  "Add an entity to the store as a new element of the given subject.
  Return the new store and the id of the new element."
  [entity store subject]
  (update-add-entity-adjacent-to
   store (:item-id subject) entity
   subject :after false))

(defn add-element-handler
  "Add a element to the item with the given client id.
  Indicate that the new element should be selected."
  [store dom-tracker key]
  (let [items (key->items store key)
        first-primitive (first-primitive-referent (remove-comments key))]
    (println "total items:" (count items))
    (println "with content" (map content items))
    (when (item-referent? first-primitive)
      (add-and-select (partial update-add-element "") store items
                         (remove-content-location-referent key) key))))

(defn adjust-condition
  "Adjust a condition to make it ready for adding as an
  element. Specifically, replace nil with \"\", and replace :??? with
  a new unique keyword.  This may require updating the store.
  Return the new condition and new store."
  [condition store]
  (reverse
   (thread-recursive-map
    (fn [store item]
      (cond  (= item nil) [store ""]
             (= item '???)  (let [[id store] (get-unique-id-number store)]
                              [store (symbol (str "???-" id))])
             true [store item]))
    store condition)))

(defn update-add-sibling
  "Given an item and a condition, add a sibling in the given position
  (:before or :after) satisfying the condition. Also return the id of
  the new element."
  [sibling-condition position store item]
  (let [[adjusted store] (adjust-condition sibling-condition store)]
    (update-add-entity-adjacent-to
     store (id->subject store (:item-id item)) adjusted
     item position true)))

(defn add-sibling-handler
  "Add a sibling to the item with the given client id."
  [store dom-tracker key position]
  (let [items (key->items store key)
        first-primitive (first-primitive-referent (remove-comments key))
        sibling-condition (:sibling-condition
                           (key->attributes
                            dom-tracker
                            (remove-content-location-referent key)))]
    (println "total items:" (count items))
    (println "with content" (map content items))
    (when (item-referent? first-primitive)
      (add-and-select (partial update-add-sibling sibling-condition position)
                      store items
                      (remove-first-primitive-referent
                       (remove-content-location-referent key))
                      key))))

(defn furthest-item
  "Given a list of items and a position,
  return the furthest item in that position."
  [items position]
  (if (= (count items) 1)
    (first items)
    (second
     (reduce (case position
               :before (fn [a b] (if (earlier? (first a) (first b)) a b))
               :after (fn [a b] (if (earlier? (first a) (first b)) b a)))
             (map (fn [item] [(label->content item :order) item]) items)))))

(defn add-row-or-column
  "Add a row/column parallel to the item with the given client id.
  The last two arguments are either :row-sibling :row-condition or
  :column-sibling :column-condition."
    [store dom-tracker key position order-key condition-key]
    (let [attributes (key->attributes
                      dom-tracker (remove-content-location-referent key))
          order-sibling (order-key attributes)
          order-groups (if order-sibling
                        (key->item-groups store order-sibling)
                        (map vector (key->items store key)))
          sibling-key (or order-sibling key)
          new-condition (condition-key attributes)]
      (println "keys" order-key condition-key)
      (println "attributes" (simplify-for-print attributes))
      (println "order sibling" (simplify-for-print order-sibling))
      (println "new-condition" (simplify-for-print new-condition))
      (println "total groups:" (count order-groups))
      (println "with content" (map #(map content %) order-groups))
      (when ((some-fn nil? item-referent? content-location-referent?)
             (first-primitive-referent sibling-key))
        (add-and-select (partial update-add-sibling new-condition position)
                        store (map #(furthest-item % position) order-groups)
                        (remove-first-primitive-referent
                         (remove-content-location-referent sibling-key))
                        key))))

(defn do-add
  "Add new item(s), in accord with the optional arguments.
  The default is to add a new element to the target, adjacent to the target." 
  [store target-key
   & {:keys [template subject-key adjacent-key adjacent-group-key
             all-elements position use-bigger]
      :or  {template nil           ; template that added item(s) should satisfy
            subject-key nil        ; subject(s) of the new item(s)
            adjacent-key nil       ; item(s) adjacent to new item(s)
            adjacent-group-key nil ; item group(s) adjacent to new item(s)
            all-elements false     ; adjacency applies to elements of adjacent
            position :after       ; :before or :after adjacent
            use-bigger false       ; use the bigger part of adjacent's order
            }}]
  (let [subject-key (or subject-key target-key)
        subjects (key->items store subject-key)
        adjacents (cond
                    adjacent-key (key->items store adjacent-key)
                    adjacent-group-key (map #(furthest-item % position)
                                            (key->item-groups
                                             store adjacent-group-key))
                   true subjects)
        [adjusted-template store] (adjust-condition template store)]
    ;; TODO: handle all-elements argument
    (println "total items added: " (count subjects))
    (assert (= (count subjects) (count adjacents)))
    (add-and-select (fn [store [subject adjacent]]
                      (update-add-entity-adjacent-to
                       store (:item-id subject) adjusted-template
                       adjacent position use-bigger))
                    store
                    (map vector subjects adjacents)
                    subject-key target-key)))

(defn add-row-handler
  "Add a row to the item with the given client id."
  [store dom-tracker key position]
  (add-row-or-column store dom-tracker key position
                     :row-sibling :row-condition))

(defn add-column-handler
  "Add a row to the item with the given client id."
  [store dom-tracker key position]
  (add-row-or-column store dom-tracker key position
                     :column-sibling :column-condition))

(defn update-delete
  "Given an item, remove it and all its elements from the store"
  [store item]
  (remove-entity-by-id store (:item-id item)))

(defn delete-handler
  [store dom-tracker key]
  (let [delete-key (or (:delete-key (key->attributes dom-tracker key)) key)
        first-primitive (first-primitive-referent (remove-comments delete-key))
        items (key->items store delete-key)]
    (when (not= delete-key key)
      (println "delete-key" (simplify-for-print delete-key)))
    (println "total items:" (count items))
    (println "items" (map to-list items))
    (if (item-referent? first-primitive)
      (reduce update-delete store items)
      (do (println "NOT DELETING: first primitive not an item.")
          nil))))

(defn do-delete
  "Add new item(s), in accord with the optional arguments.
  The default is to add a new element to the target, adjacent to the target." 
  [store target-key
   & {:keys [delete-key] :or {delete-key nil}}] 
  (let [delete-key (or delete-key target-key)
        items (key->items store delete-key)]
    (println "total items:" (count items))
    (reduce update-delete store items)))

(defn set-content-handler
  [store dom-tracker key from to]
  ;; TODO: Handle deleting.
  (let [basic-key (remove-comments key)
        first-primitive (first-primitive-referent basic-key) 
        to (parse-string-as-number to)]
    (cond ((some-fn nil? item-referent?) first-primitive)
          (let [items (key->items store basic-key)]
            (println "updating " (count items) " items")
            (reduce (partial update-set-content from to)
                    store items))
          (and (elements-referent? first-primitive) (not= to ""))
          (let [items (key->items store (remove-first-primitive-referent
                                         basic-key))
                attributes (key->attributes dom-tracker key)
                adjacent-key (:add-adjacent attributes)
                [_ condition] first-primitive
                model-entity (cons to (rest condition))
                selection-suffix (if (= condition [nil 'tag])
                                   key
                                   (remove-first-primitive-referent key))]
            (if adjacent-key
              (let [adjacents (key->items store adjacent-key)
                    position (:add-direction attributes)]
                (assert (= (count items) (count adjacents)))
                (add-and-select
                 (fn [store [subject adjacent]]
                   (update-add-entity-adjacent-to
                    store (:item-id subject) model-entity
                    adjacent position false))
                 store (map vector items adjacents) selection-suffix key))
              (add-and-select
               (fn [store item]
                 (update-add-element model-entity store item))
               store items selection-suffix key))))))

(defn do-set-content
  [store target-key   
   & {:keys [from to] :or {from nil to nil}}]
  ;; TODO: Handle deleting.
  (assert from)
  (assert to)
  (let [to (parse-string-as-number to)]
    (let [items (key->items store target-key)]
      (println "updating " (count items) " items")
      (reduce (partial update-set-content from to)
              store items))))

(defn do-create-content
  [store target-key
   & {:keys [content adjacent-key adjacent-group-key all-elements position]
      :or  {content nil            ; value of content
            adjacent-key nil       ; item(s) adjacent to new item(s)
            adjacent-group-key nil ; item group(s) adjacent to new item(s)
            all-elements false     ; adjacency applies to elements of adjacent
            position :after        ; :before or :after adjacent
            }}]
  (assert (elements-referent? (first-primitive-referent target-key)))
  (let [content (parse-string-as-number content)]
    (when (not= content "")
      (let [subjects (key->items store (remove-first-primitive-referent
                                        target-key))
            adjacents (cond
                        adjacent-key (key->items store adjacent-key)
                        adjacent-group-key (map #(furthest-item % position)
                                                (key->item-groups
                                                 store adjacent-group-key))
                        true subjects)
            first-primitive (first-primitive-referent target-key)
            [_ condition] first-primitive
            model-entity (cons content (rest condition))
            selection-suffix (if (= condition [nil 'tag])
                               ;; TODO: Should this change the condition
                               ;; to a comment?
                               target-key
                               (remove-first-primitive-referent target-key))]
        ;; TODO: handle all-elements argument
        (assert (= (count subjects) (count adjacents)))
        (add-and-select
         (fn [store [subject adjacent]]
           (update-add-entity-adjacent-to
            store (:item-id subject) model-entity
            adjacent position false))
         store (map vector subjects adjacents) selection-suffix target-key)))))

(defn selected-handler
  [store session-state target-key]
  (let [ids (item-ids-referred-to target-key)
        items (map #(description->entity % store) ids)]
    (println "Selected key" target-key)
    (mutable-set-swap!
         (:do-not-merge session-state)
         (fn [old]
           (if (item-referent?
                (first-primitive-referent (remove-comments target-key)))
               (set (cons (first items)
                          (clojure.set/intersection (set (rest items)) old)))
               (clojure.set/intersection (set items) old))))))

;;; TODO: Undo functionality should be added here. It shouldn't be
;;; hard, because the updated store already has a list of changed ids.
;;; Support could even be incorporated into immutable stores.
(defn do-storage-update-action
  "Do an action that can update the store. The action is given the
  store, and the other arguments passed to this function. It can either
  return nil, meaning no change, a new store, or a map with a :store
  value and any additional commands it wants to request of the
  client (currently, only :select, whose value is the key of the dom
  it wants to be selected and a list of keys, one of which should already
  be selected)."
  [handler mutable-store & action-args]
  (do-update-control-return!
   mutable-store
   (fn [store]
     (let [result (apply handler store action-args)]
       (if result
         (if (satisfies? cosheet.store/Store result)
           [result nil]
           (do
             (assert (map? result))
             (assert (:store result))
             [(:store result) (dissoc result store)]))
         [store nil])))))

(defn do-action
  [mutable-store session-state [action-type client-id & additional-args]]
  (println "doing action " action-type)
  (let [tracker (:tracker session-state)
        target-key (id->key tracker client-id)]
    (println "id: " client-id " with key: " (simplify-for-print target-key))
    (when (not (empty? additional-args))
      (println "additional arguments: " additional-args))
    (if-let [handler (case action-type
                           :selected selected-handler
                           nil)]
      (apply handler mutable-store session-state target-key additional-args)
      (if-let [command (get-in (key->attributes tracker target-key)
                               [:commands action-type])]
        (let [[action-name & args] command
              handler (case action-name
                        :do-add do-add
                        :do-delete do-delete
                        :do-set-content do-set-content
                        :do-create-content do-create-content
                        nil)]
          (when handler
            (apply do-storage-update-action
                   handler mutable-store target-key additional-args)))
        (if-let [handler (case action-type
                           :set-content set-content-handler
                           :add-element add-element-handler
                           :add-sibling add-sibling-handler
                           :add-row add-row-handler
                           :add-column add-column-handler
                           :delete delete-handler                  
                           nil)]
          (apply do-storage-update-action
                 handler mutable-store tracker target-key additional-args)
          (println "unknown action type:" action-type))))))

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

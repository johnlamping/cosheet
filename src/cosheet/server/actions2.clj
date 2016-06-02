(ns cosheet.server.actions2
  (:require
   (cosheet
    [debug :refer [simplify-for-print]]
    [utils :refer [parse-string-as-number thread-map thread-recursive-map
                   swap-control-return! replace-in-seqs]]
    [orderable :refer [split earlier?]]
    [store :refer [update-content add-simple-element do-update-control-return!
                   id->subject id-valid? undo! redo!]]
    [store-impl :refer [get-unique-id-number]]
    [store-utils :refer [add-entity remove-entity-by-id]]
    mutable-store-impl
    [entity :refer [StoredEntity description->entity
                    content elements label->elements label->content to-list]]
    [dom-utils :refer [dom-attributes]]
    [query :refer [matching-items]]
    query-impl)
   (cosheet.server
    [dom-tracker :refer [id->key key->attributes]]
    [referent :refer [instantiate-referent]])))

;;; TODO: validate the data coming in, so mistakes won't cause us to
;;; crash.

(defn update-set-content-if-matching
  "Set the content of the item in the store provided the current content
   matches 'from'."
  [from to store item]
  ;; There are two special cases to match: If we have a number,
  ;; the client will have a string. If we have an anonymous symbol, the client
  ;; will have "???".
  (if (= (parse-string-as-number from)
         (let [content (content item)]
           (if (and (symbol? content) (= (subs (str content) 0 3) "???"))
             "???"
             content)))
    (update-content store (:item-id item) (parse-string-as-number to))
    (do (println "content doesn't match" (content item) from)
        store)))

(defn update-add-entity-with-order
  "Add an entity, described in list form, to the store, with the given
   subject.  Add ordering information to each part of the entity,
   except for tag specifiers and non-semantic elements, splitting the
   provided order for the orders, and returning an unused piece of it.
   Put the new entity in the specified position (:before or :after) of
   the returned order, and make the entity use the bigger piece if
   use-bigger is true, otherwise return the bigger piece.
   Return the new store, the id of the item, and the remaining order."
  [store subject-id entity order position use-bigger]
  (let [entity-content (content entity)
        entity-elements (elements entity)]
    (if (or (keyword? entity-content)
            (some #{:non-semantic} entity-elements))
      ;; Keyword markers and non-semantic elements don't get an ordering.
      (let [[s1 id] (add-entity store subject-id
                                (replace-in-seqs entity nil ""))]
        [s1 id order])
      (let [value-to-store (if (nil? entity-content) "" entity-content)
            [s1 id] (add-simple-element store subject-id value-to-store)
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
            [s3 _] (add-entity
                    s2 id `(~(if use-bigger bigger-order smaller-order)
                            :order :non-semantic))]
        [s3 id (if use-bigger smaller-order bigger-order)]))))

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

(defn order-element-for-item
  "Return an element with the order information for item,
   or, if that is not available, for the overall store."
  [item store]
  (or (first (label->elements item :order))
      (first (matching-items '(nil :unused-orderable) store))))

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

(defn adjust-condition
  "Adjust a condition to make it ready for adding as an
   element. Specifically, replace '??? with a new unique symbol.  This
   may require updating the store.  Return the new condition and new
   store."
  [condition store]
  (reverse
   (thread-recursive-map
    (fn [store item]
      (if (= item '???)
        (let [[id store] (get-unique-id-number store)]
          [store (symbol (str "???-" id))])
        [store item]))
    store condition)))

(defn add-and-select
  "Run the specified function on the store and each of the arguments.
  The function must return an updated store and the id of a new item.
  Return a map of the new store and a selection request for the first
  of the new items."
  [f store arguments parent-key old-key]
  (let [[store element-ids] (thread-map f store arguments)]
    (if (empty? element-ids)
      store
      {:store store
       :select [(cons (first element-ids) parent-key)
                old-key]})))

(defn generic-add
  "Add new item(s), relative to the target information. Either
   sibling-referent or (adjacent-referent and subject-referent) must
   be provided."
  [store target-info parent-key old-key use-bigger]
  (let [{:keys [item-referent       ; item(s) referred to
                subject-referent    ; subject(s) of the new item(s)
                adjacents-referent  ; groups of item(s) adjacent to new item(s)
                position            ; :before or :after sibling/adjacent
                template]           ; added item(s) should satisfy this
         :or {position :after}}         
        target-info]
    (assert (or item-referent adjacents-referent))
    (assert (not (and item-referent adjacents-referent)))
    (let [adjacents (if adjacents-referent
                      (map #(furthest-item % position)
                           (instantiate-referent adjacents-referent store))
                      (apply concat
                             (instantiate-referent item-referent store)))
          subject-ids (if subject-referent
                        (map :item-id
                             (apply concat
                                    (instantiate-referent subject-referent
                                                          store)))
                        (map #(id->subject store (:item-id %)) adjacents))
          [adjusted-template store] (adjust-condition template store)]
      (println "total items added: " (count subject-ids))
      (assert (= (count subject-ids) (count adjacents)))
      (add-and-select (fn [store [subject-id adjacent]]
                        (update-add-entity-adjacent-to
                         store subject-id adjusted-template
                         adjacent position use-bigger))
                      store
                      (map vector subject-ids adjacents)
                      parent-key old-key))))

(defn do-add-sibling
  [store key attributes]
  (when-let [item (:target attributes)]
    (generic-add store item (rest key) key true)))

(defn do-add-row
  [store key attributes]
  (when-let [row (:row attributes)]
    (generic-add store row nil nil true)))

(defn do-add-column
  [store key attributes]
  (when-let [column (:column attributes)]
    (generic-add store column (rest key) key true)))

(defn do-add-element
  [store key attributes]
  (when-let [subject-referent (:item-referent (:target attributes))]
    (generic-add store
                 {:subject-referent subject-referent
                  :adjacents-referent subject-referent}
                 key key true)))

(defn update-delete
  "Given an item, remove it and all its elements from the store"
  [store item]
  (remove-entity-by-id store (:item-id item)))

(defn do-delete
  "Remove item(s)." 
  [store key attributes]
  (let [{:keys [target delete-referent] :or {target nil delete-referent nil}}
        attributes]
    (when-let [to-delete (or delete-referent (:item-referent target))]
      (let [items (apply concat (instantiate-referent to-delete store))]
        (println "total items:" (count items))
        (reduce update-delete store items)))))

(defn do-set-content
  [store key attributes]
  ;; TODO: Handle deleting.
  (let [{:keys [target from to]} attributes
        referent (:item-referent target)]
    (when (and referent from to)
      (let [to (parse-string-as-number to)]
        (let [items (apply concat (instantiate-referent referent store))]
          (println "updating " (count items) " items")
          (reduce (partial update-set-content-if-matching from to)
                  store items))))))

(defn do-create-content
  [store target-key attributes]
  (let [{:keys [target content]} attributes
        content (parse-string-as-number content)
        new-template (cons content (rest (:template target)))]
    (when (not= content "")
      (generic-add store (into target {:template new-template})
                   (rest target-key) target-key false))))

(defn do-storage-update-action
  "Do an action that can update the store. The action is given the
  store, the target key, and a map of attributes associated with the
  DOM element, the particular command, and anything sent by the
  client. The action can either return nil, meaning no change, a new
  store, or a map with a :store value and any additional commands it
  wants to request of the client (currently, only :select, whose value
  is the key of the dom it wants to be selected and a list of keys,
  one of which should already be selected)."
  [handler mutable-store target-key attributes]
  (do-update-control-return!
   mutable-store
   (fn [store]
     (let [result (handler store target-key attributes)]
       (if result
         (if (satisfies? cosheet.store/Store result)
           [result nil]
           (do
             (assert (map? result))
             (assert (:store result))
             [(:store result) (dissoc result :store)]))
         [store nil])))))

(defn get-contextual-handler
  [action]
  ({:add-sibling do-add-sibling
    :add-row do-add-row
    :add-column do-add-column
    :add-element do-add-element
    :delete do-delete
    :set-content do-set-content
    :create-content do-create-content}
   action))

(defn do-contextual-action
  "Do an action that applies to a DOM cell, and whose interpretation depends
  on that cell."
  [mutable-store tracker [action-type client-id & {:as client-args}]]
  (let [target-key (id->key tracker client-id)
        attributes (key->attributes tracker target-key)
        commands (:commands attributes)
        handler (get-contextual-handler action-type)]
    (if (and commands (contains? commands action-type) handler)
      (let [action-args (or (and commands (commands action-type)) {})
            extra-info (into action-args client-args)]
        (do
          (println "command: " (map simplify-for-print
                                    (list* action-type target-key
                                           (map concat (seq extra-info)))))
          (do-storage-update-action handler mutable-store target-key
                                    (into attributes extra-info))))
      (println "unhandled action type:" action-type))))

(defn do-undo
  [mutable-store session-state]
  (undo! mutable-store)
  nil)

(defn do-redo
  [mutable-store session-state]
  (redo! mutable-store)
  nil)

(defn do-action
  [mutable-store session-state action]
  (let [[action-type & additional-args] action]
    (println "doing action " action-type)
    (if-let [handler (case action-type
                       :undo do-undo
                       :redo do-redo
                       nil)]
      (do (println "command: " (map simplify-for-print action))
          (apply handler mutable-store session-state additional-args))
      (if (= (mod (count action) 2) 0)
        (do-contextual-action mutable-store (:tracker session-state) action)
        (println "Error: odd number of keyword/argument pairs:" action)))))

(defn do-actions
  "Run the actions, and return any additional information to be returned
  to the client"
  [mutable-store session-state action-sequence]
  (reduce (fn [client-info action]
            (let [for-client (do-action mutable-store session-state action)]
              (println "for client " (simplify-for-print for-client))
              (into client-info (select-keys for-client [:select]))))
          {} action-sequence))

(defn confirm-actions
  "Check that the actions have not already been done, update the
  last-action atom to reflect that these actions have
  been done, and return the sequence of actions to be done."
  [actions last-action-atom]
  (swap-control-return!
   last-action-atom
   (fn [last-action]
     (let [keys (cond->> (sort (keys actions))
                  last-action (filter #(pos? (compare % last-action))))]
       [(if (empty? keys) last-action (last keys))
        (map actions keys)]))))

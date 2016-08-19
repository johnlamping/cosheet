(ns cosheet.server.actions
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
    [query :refer [matching-items template-matches]]
    query-impl)
   (cosheet.server
    [dom-tracker :refer [id->key key->attributes]]
    [referent :refer [instantiate-referent referent->string
                      item-referent? exemplar-referent?]])))

;;; TODO: Validate the data coming in, so mistakes won't cause us to
;;; crash.

;;; TODO: Replace the asserts with log messages, so things are robust.

(defn substitute-in-key
  "Substitute into the key, instantiating patterns with the item.  A
  pattern is of the form [:pattern <template>], where template is
  either empty, or somewhere contains a variable named :v. An empty
  pattery will be replaced by the item, while a non-empty pattern gets
  replaced by the id of the match for its variable."
  [key item]
  (vec (map (fn [part]
              (if (and (sequential? part)
                       (= (first part) :pattern))
                (let [pattern (second part)]
                  (if pattern
                    (let [matches (template-matches pattern item)
                          value (:v (first matches))]
                      (assert value)
                      (:item-id value))
                    (:item-id item)))
                part))
            key)))

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
    (if (or (and (keyword? entity-content) (empty? entity-elements))
            (some #{:non-semantic} entity-elements))
      ;; Keyword markers and non-semantic elements don't get an ordering.
      (let [[s1 id] (add-entity store subject-id entity)]
        [s1 id order])
      (let [value-to-store entity-content
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
  (cond
    (empty? items) nil
    (= (count items) 1) (first items)
    true (second
          (reduce (case position
                    :before (fn [a b] (if (earlier? (first a) (first b)) a b))
                    :after (fn [a b] (if (earlier? (first a) (first b)) b a)))
                  (map (fn [item] [(label->content item :order) item])
                       items)))))

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
  "Add an entity matching the template to the store for each 
  subject-id, adjacents pair.
  Return a map of the new store and a selection request for the first
  of the new items."
  [store entity subject-ids adjacents position use-bigger
   select-pattern old-key]
  (let [f (fn [store [subject-id adjacent]]
            (update-add-entity-adjacent-to
             store subject-id entity adjacent position use-bigger))        
        [store element-ids] (thread-map f store
                                        (map vector subject-ids adjacents))]
    {:store store
     :select (when (not (empty? element-ids))
               [(substitute-in-key
                 select-pattern (description->entity (first element-ids) store))
                [old-key]])}))

(defn generic-add
  "Add new item(s), relative to the target information. Either
   sibling-referent or (adjacent-referent and subject-referent) must
   be provided."
  [store target-info parent-key old-key use-bigger]
  (let [{:keys [item-referent             ; item(s) referred to
                subject-referent          ; subject(s) of the new item(s)
                adjacent-referent         ; item(s) adjacent to new item(s)
                adjacent-groups-referent  ; groups of item(s) adjacent to
                                          ; new item(s)
                position                  ; :before or :after item/adjacent
                template                  ; added item(s) should satisfy this
                nil-to-anything           ; if true, then for the first group
                                          ; of items, nils in the template
                                          ; should go to 'anything
                select-pattern]           ; a key pattern to use for selecting 
         :or {position :after}}         
        target-info]
    (println "adding" (simplify-for-print target-info))
    (assert (= (count (remove nil? [item-referent
                                    adjacent-referent
                                    adjacent-groups-referent]))
               1))
    (let [adjacents (cond
                      adjacent-referent
                      (instantiate-referent adjacent-referent store)
                      adjacent-groups-referent
                      [(map #(furthest-item % position)
                             (instantiate-referent adjacent-groups-referent
                                                   store))]
                      true (instantiate-referent item-referent store))
          subject-ids (if subject-referent
                        (map #(map :item-id %)
                             (instantiate-referent subject-referent store))
                        (map (fn [group]
                               (map #(id->subject store (:item-id %)) group))
                             adjacents))
          [adjusted-template store] (adjust-condition template store)
          select-pattern (or select-pattern (conj parent-key [:pattern]))
          entity (replace-in-seqs adjusted-template nil "")
          alt-entity (if nil-to-anything
                       (replace-in-seqs adjusted-template nil 'anything)
                       entity)]
      (println "total items added: " (apply + (map count subject-ids)))
      (assert (= (map count subject-ids) (map count adjacents)))
      (let [{:keys [store select]}
            (add-and-select
             store alt-entity
             (first subject-ids) (first adjacents)
             position use-bigger select-pattern old-key)
            select1 select
            {:keys [store select]}
            (add-and-select
             store entity
             (apply concat (rest subject-ids)) (apply concat (rest adjacents))
             position use-bigger select-pattern old-key)]
        {:store store :select (or select1 select)}))))

(defn do-add-twin
  [store attributes]
  (let [{:keys [target target-key]} attributes]
    (when target
      (let [item-key (if (= (last target-key) :content)
                       (pop target-key)
                       target-key)]
        (generic-add store target (pop item-key) target-key true)))))

(defn do-add-element
  [store attributes]
  (when-let [subject-referent (:item-referent (:target attributes))]
    (generic-add store
                 {:subject-referent subject-referent
                  :adjacent-groups-referent subject-referent}
                 (:target-key attributes) (:target-key attributes) true)))

(defn do-add-sibling
  [store attributes]
  (when-let [sibling (:sibling attributes)]
    (generic-add
     store (assoc sibling :select-pattern (:select-pattern attributes))
     (:parent-key sibling) (:target-key attributes) true)))

(defn do-add-row
  [store attributes]
  (when-let [row (:row attributes)]
    (generic-add
     store (assoc row :select-pattern (:select-pattern attributes))
     (:parent-key row) (:target-key attributes) true)))

(defn do-add-column
  [store attributes]
  (when-let [column (:column attributes)]
    (generic-add
     store (assoc column :select-pattern (:select-pattern attributes))
     (:parent-key column) (:target-key attributes) true)))

(defn update-delete
  "Given an item, remove it and all its elements from the store"
  [store item]
  (remove-entity-by-id store (:item-id item)))

(defn do-delete
  "Remove item(s)." 
  [store attributes]
  (let [{:keys [target delete-referent] :or {target nil delete-referent nil}}
        attributes]
    (when-let [to-delete (or delete-referent (:item-referent target))]
      (let [items (apply concat (instantiate-referent to-delete store))]
        (println "total items:" (count items))
        (reduce update-delete store items)))))

(defn do-set-content
  [store attributes]
  ;; TODO: Handle deleting.
  (let [{:keys [target-key target from to]} attributes
        referent (:item-referent target)]
    (if referent
      ;; Changing an existing value.
      (when (and from to)
        (let [to (parse-string-as-number to)]
          (let [items (apply concat (instantiate-referent referent store))]
            (println "updating " (count items) " items")
            (reduce (partial update-set-content-if-matching from to)
                    store items))))
      ;; Creating a new value.
      (when (not= to "")
        (let [content (parse-string-as-number to)
              new-template (cons content (rest (:template target)))]
          (generic-add store (into target {:template new-template})
                       (pop target-key) target-key false))))))

(defn do-expand
  [store attributes]
  (let [{:keys [target item-referent session-state]} attributes
        target-referent (or item-referent (:item-referent target))]
    (when (or (item-referent? target-referent)
              (exemplar-referent? target-referent))
      (println "a rootable referent" target-referent)
      {:store store
       :open (str (:name session-state)
                  "?referent=" (referent->string target-referent))})))

(defn do-storage-update-action
  "Do an action that can update the store. The action is given the
  store, the target key, and a map of attributes associated with the
  DOM element, the particular command, and anything sent by the
  client. The action can either return nil, meaning no change, a new
  store, or a map with a :store value and any additional commands it
  wants to request of the client (currently, :select, whose value
  is the key of the dom it wants to be selected and a list of keys,
  one of which should already be selected, and :expand, whose value
  is the key of the item it wants to appear in a new tab.)."
  [handler mutable-store attributes]
  (do-update-control-return!
   mutable-store
   (fn [store]
     (let [result (handler store attributes)]
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
  ({:add-twin do-add-twin
    :add-element do-add-element
    :add-sibling do-add-sibling
    :add-row do-add-row
    :add-column do-add-column
    :delete do-delete
    :set-content do-set-content
    :expand do-expand}
   action))

(defn do-contextual-action
  "Do an action that applies to a DOM cell, and whose interpretation depends
  on that cell. We will call a contextual action handler with attributes
  including all the atttributes from the dom, any that the dom says to add to
  the command, and any added by the client. Also include
  :session-state and :target-key. (The dom will provide :target.)"
  [mutable-store session-state [action-type client-id & {:as client-args}]]
  (let [handler (get-contextual-handler action-type)
        tracker (:tracker session-state)
        target-key (id->key tracker client-id)
        dom-attributes (key->attributes tracker target-key)
        commands (:commands dom-attributes)
        action-args (or (and commands (commands action-type)) {})
        attributes (-> dom-attributes
                       (into action-args)
                       (into client-args)
                       (assoc :target-key target-key
                              :session-state session-state))]
    (if (and commands (contains? commands action-type) handler)
      (do
          (println "command: " (map simplify-for-print
                                    (list* action-type target-key
                                           (map concat
                                                (concat (seq action-args)
                                                         (seq client-args))))))
          (println "dom attributes: " (simplify-for-print dom-attributes))
          (do-storage-update-action handler mutable-store attributes))
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
        (do-contextual-action mutable-store session-state action)
        (println "Error: odd number of keyword/argument pairs:" action)))))

(defn do-actions
  "Run the actions, and return any additional information to be returned
  to the client"
  [mutable-store session-state action-sequence]
  (reduce (fn [client-info action]
            (let [for-client (do-action mutable-store session-state action)]
              (println "for client " (simplify-for-print for-client))
              (into client-info (select-keys for-client [:select :open]))))
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

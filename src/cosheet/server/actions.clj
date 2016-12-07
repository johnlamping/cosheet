(ns cosheet.server.actions
  (:require
   (cosheet
    [debug :refer [simplify-for-print]]
    [utils :refer [parse-string-as-number thread-map thread-recursive-map
                   swap-control-return! replace-in-seqs equivalent-atoms?]]
    [orderable :refer [split earlier?]]
    [store :refer [update-content add-simple-element
                   fetch-and-clear-modified-ids
                   do-update-control-return! revise-update-control-return!
                   id->subject id-valid? undo! redo! current-store]]
    [store-impl :refer [get-unique-id-number]]
    [store-utils :refer [add-entity remove-entity-by-id]]
    mutable-store-impl
    [entity :refer [StoredEntity description->entity to-list
                    content elements label->elements label->content subject]]
    [dom-utils :refer [dom-attributes map-combiner]]
    [query :refer [matching-items matching-elements template-matches]]
    query-impl)
   (cosheet.server
    [dom-tracker :refer [id->key key->attributes]]
    [referent :refer [instantiate-referent instantiate-to-items
                      referent->string referent?
                      referent->exemplar-and-subject
                      item-referent first-group-referent
                      semantic-elements-R]])))

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
  ;; There are several special cases to match: If we have a number,
  ;; the client will have a string. If we have an anonymous symbol,
  ;; the client will have "???". If the client had ..., it was probably
  ;; a wild card, and we could have anything.
  (if (let [from (parse-string-as-number from)
            content (content item)]
        (println "from" from "content" content)
        (or (equivalent-atoms? from content)
            ;; Probably a wildcard -- matches anything.
            (and (= from "..."))
            (and (= from "???")
                 (symbol? content)
                 (= (subs (str content) 0 3) "???"))))
    (update-content store (:item-id item) (parse-string-as-number to))
    (do (println "content doesn't match" from (content item))
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
                template                  ; added item(s) should satisfy this.
                nil-to-anything           ; if true, then for the first group
                                          ; of items, nils in the template
                                          ; should go to 'anything
                select-pattern            ; a key pattern to use for selecting
                                          ; :content will to be added if
                                          ; it makes sense.
                ] 
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
      (assert (= (map count subject-ids) (map count adjacents))
              [(simplify-for-print subject-ids) (simplify-for-print adjacents)])
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

(defn do-add-element
  [store context attributes]
  (let [{:keys [target-key selector-category]} attributes]
    (when-let [subject-referent (:item-referent context)]
      (generic-add store
                   (cond-> {:subject-referent subject-referent
                            :adjacent-referent subject-referent}
                     selector-category (assoc :nil-to-anything true))
                   target-key target-key true))))

(defn do-add-twin
  [store context attributes]
  (let [{:keys [target-key]} attributes]
    (when (and (:item-referent context) (:template context))
      (let [item-key (if (= (last target-key) :content)
                       (pop target-key)
                       target-key)]
        (generic-add store
                     (cond-> context
                       (:selector-category attributes)
                       (assoc :nil-to-anything true))
                     (pop item-key) target-key true)))))

(defn do-add-sibling
  [store context attributes]
  (generic-add
   store context (:parent-key context) (:target-key attributes) true))

(defn do-add-row
  [store context attributes]
  (generic-add
     store context (:parent-key context) (:target-key attributes) true))

(defn do-add-column
  [store context attributes]
  (generic-add
     store context (:parent-key context) (:target-key attributes) true))

(defn update-delete
  "Given an item, remove it and all its elements from the store"
  [store item]
  (remove-entity-by-id store (:item-id item)))

(defn do-delete
  "Remove item(s)." 
  [store context attributes]
  (when-let [to-delete (:item-referent context)]
    (let [item-groups (instantiate-referent to-delete store)
          header-group (first item-groups)
          first-content (content (first header-group))
          items (distinct ;; distinct should not be necessary, but is a
                          ;; safety measure to make sure we don't delete twice
                 (apply concat item-groups))]
      (println "total items:" (count items))
      (let [removed (reduce update-delete store items)]
        ;; If we removed a placeholder from a header, there will be no way to
        ;; reference the placeholder again, so change any instances of it
        ;; to a simple "???".
        (if (and (symbol? first-content)
                 (= (subs (str first-content) 0 3) "???")
                 (seq (matching-elements
                       :column (subject (first header-group)))))
          (reduce (fn [store item]
                    (update-content store (:item-id item) "???"))
                  removed (matching-items first-content removed))
          removed)))))

(defn do-set-content
  [store context attributes]
  ;; TODO: Handle deleting.
  (let [{:keys [target-key from to immutable]} attributes
        referent (:item-referent context)]
    (if referent
      ;; Changing an existing value.
      (when (and from to (not immutable))
        (let [to (parse-string-as-number to)]
          (let [items (apply concat (instantiate-referent referent store))]
            (println "updating " (count items) " items")
            (reduce (partial update-set-content-if-matching from to)
                    store items))))
      ;; Creating a new value.
      (when (not= to "")
        (let [content (parse-string-as-number to)
              new-template (cons content (rest (:template context)))]
          (generic-add store (into context {:template new-template})
                       (pop target-key) target-key false))))))

(defn do-expand
  [store context attributes]
  (let [{:keys [session-state selector-category]} attributes
        target-referent (:item-referent context)]
    (when (referent? target-referent)
      ;; If the target is a single item with no elements, switch the target
      ;; to its subject.
      (let [items (apply concat (instantiate-referent target-referent store))
            item (first items)
            [_ subject-ref] (referent->exemplar-and-subject target-referent)
            subject-ref (or subject-ref
                            (when-let [subject (subject item)]
                              (item-referent subject)))
            referent (if (and (every? #(= (content %) :tag)
                                      (semantic-elements-R item))
                              subject-ref)
                       subject-ref
                       target-referent)]
        {:store store
         :open (cond-> (str (:name session-state)
                            "?referent=" (referent->string referent))
                 selector-category
                 (str "&selector=" (referent->string selector-category)))}))))

(defn do-storage-update-action
  "Do an action that can update the store and also return any client
  information requested by the action. store-modifier should be a function
  like (partial do-update-control-return! mutable-store)
  that expects a function from current store to new store and return value.
  We return a map containing the new store and any additional requested
  information.
  The action is given the store and any additional arguments.
  The action can either return nil, meaning no change, a new
  store, or a map with a :store value and any additional information
  it wants to convey.
  The extra information can be:
        :select  The key of the dom to be selected, and a list of keys,
                 one of which should already be selected.
        :expand  The key of an item to appear in a new tab."
  [store-modifier handler & args]
  (store-modifier
   (fn [store]
     (let [result (apply handler store args)]
       (if result
         (if (satisfies? cosheet.store/Store result)
           [result {:store result}]
           (do
             (assert (map? result))
             (assert (:store result))
             [(:store result) result]))
         (do (println "handler didn't update store.")
             [store {:store store}]))))))

(defn get-contextual-handler
  "Return the handler for the command, and the key from the attributes
   which holds the contextual information for the command. If several keys
   might hold the information, return them in priority order."
  [action]
  ({:add-element [do-add-element :target]
    :add-twin [do-add-twin :target]
    :add-sibling [do-add-sibling :sibling]
    :add-row [do-add-row :row]
    :add-column [do-add-column :column]
    :delete [do-delete :delete :target]
    :set-content [do-set-content :target]
    :expand [do-expand :expand :target]}
   action))

(defn context-referent
  [context]
  ((some-fn :item-referent :adjacent-referent :adjacent-groups-referent)
   context))

(defn narrow-referents
  "Given a context, adjust all its referents to be their narrow versions."
  [context]
  (reduce (fn [context key]
            (update context key #(first-group-referent %)))
          context
          (filter #{:item-referent
                    :adjacent-referent
                    :adjacent-groups-referent
                    :subject-referent}
                  (keys context))))

(defn broad-alternate-text
  [selector-category]
  (selector-category
   {:table-header ["Column's description changed."
                   "Change selection instead."]
    :table-condition ["Table's description changed."
                      "Change selection instead."]
    :only-element-delete ["Column data deleted."
                          "Only hide the column instead."]}))

(defn narrow-alternate-text
  [selector-category]
  (selector-category
   {:table-header ["Column selection changed."
                   "Change description instead."]
    :table-condition ["Table selection changed."
                      "Change description instead."]
    :only-element-delete ["Column hidden."
                          "Delete the column data also."]}))

(defn alternate-contexts
  "Given the session state, action type and a context,
  return a vector of the default context
  to use, and the alternate context, if any. Also return the text to show the
  user to give them the chance to ask for the alternate context"
  [session-state action-type context attributes]
  (or
   (when-let [alternate (:alternate context)]
     (when (not= action-type :expand)
       (println "alternate requested.")
       (when-let [selector-category (if (= alternate true)
                                      (:selector-category attributes)
                                      alternate)]
         (println "selector found" selector-category)
         (let [context (dissoc context :alternate)
               narrow-context (narrow-referents context)
               store (current-store (:store session-state))]
           (when (not= (set (instantiate-to-items
                             (context-referent context) store))
                       (set (instantiate-to-items
                             (context-referent narrow-context) store)))
             (let [result
                   (if (= (:selector-interpretation session-state) :broad)
                     [context narrow-context
                      (broad-alternate-text selector-category)]
                     [narrow-context context
                      (narrow-alternate-text selector-category)])]
               (println "computed alternate" (simplify-for-print result))
               result))))))
   [context nil nil]))

(defn do-contextual-action
  "Do an action that applies to a DOM cell, and whose interpretation depends
  on that cell. We will call a contextual action handler with attributes
  including all the atttributes from the dom, any that the dom says to add to
  the command, and any added by the client. Also include
  :session-state and :target-key."
  [mutable-store session-state [action-type client-id & {:as client-args}]]
  (let [[handler & context-keys] (get-contextual-handler action-type)
        tracker (:tracker session-state)
        target-key (id->key tracker client-id)
        dom-attributes (key->attributes tracker target-key)
        attributes (-> dom-attributes
                       (into client-args)
                       (assoc :target-key target-key
                              :session-state session-state))]
    (if handler
      (if-let [context (->> (map attributes context-keys)
                            (remove nil?)
                            reverse
                            (apply merge-with (partial map-combiner nil)))]
        (do
          (println "command: " (map simplify-for-print
                                    (list* action-type target-key
                                           (map concat (seq client-args)))))
          (println "dom attributes: " (simplify-for-print dom-attributes))
          (let [[context alternate-context text] (alternate-contexts
                                                  session-state action-type
                                                  context attributes)
                result (do-storage-update-action
                           (partial do-update-control-return! mutable-store)
                           handler context attributes)]
            (reset! (:alternate session-state)
                    (when alternate-context
                      (println "setting non-trivial alternate context." text)
                      {:new-store (first (fetch-and-clear-modified-ids
                                          (:store result)))
                       :action [handler
                                alternate-context
                                (dissoc attributes :session-state)]
                       :text text}))
            (dissoc result :store)))
        (println "No context for action:" action-type attributes))
      (do 
        (println "unhandled action type:" action-type)))))

(defn do-alternate-contextual-action
  "Do the alternate interpretation of the recorded contextual action."
  [mutable-store session-state]
  (when-let [alternate @(:alternate session-state)]
    (println "doing alternate.")
    (let [{:keys [new-store action]} alternate
          [handler context attributes] action
          result (apply do-storage-update-action
                  (partial revise-update-control-return!
                           mutable-store new-store)
                  [handler context (assoc attributes
                                          :session-state session-state)])]
      (dissoc result :store))))

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
                       :alternate do-alternate-contextual-action
                       nil)]
      (do (println "command: " (map simplify-for-print action))
          (let [for-client
                (apply handler mutable-store session-state additional-args)]
            (reset! (:alternate session-state) nil)
            for-client))
      (if (= (mod (count action) 2) 0)
        (do-contextual-action mutable-store session-state action)
        (println "Error: odd number of keyword/argument pairs:" action)))))

(defn do-actions
  "Run the actions, and return any additional information to be returned
  to the client"
  [mutable-store session-state action-sequence]
  (let [for-client
        (reduce (fn [client-info action]
                  (let [for-client
                        (do-action mutable-store session-state action)]
                    (println "for client " (simplify-for-print for-client))
                    (into client-info
                          (select-keys for-client [:select :open]))))
                {} action-sequence)]
    (if-let [alternate-text (:text @(:alternate session-state))]
      (assoc for-client :alternate-text alternate-text)
      for-client)))

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

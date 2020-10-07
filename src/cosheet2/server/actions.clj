(ns cosheet2.server.actions
  (:require
   (cosheet2
    [debug :refer [simplify-for-print]]
    [utils :refer [parse-string-as-number thread-map
                   swap-control-return! equivalent-atoms?]]
    [map-state :refer [map-state-get-current map-state-reset!
                       map-state-change-value-control-return!]]
    [store :refer [update-content update-equivalent-undo-point
                   fetch-and-clear-modified-ids
                   store-update-control-return!
                   id->subject id-label->element-ids id-valid? undo! redo!
                   current-store
                   id-label->element-ids id->content
                   StoredItemDescription]]
    [store-impl :refer [id->string string->id]]
    [store-utils :refer [add-entity remove-entity-by-id]]
    mutable-store-impl
    [entity :refer [StoredEntity description->entity to-list
                    content elements label->elements label->content subject]]
    [hiccup-utils :refer [dom-attributes map-combiner]]
    [query :refer [matching-elements matching-extensions]]
    query-impl)
   (cosheet2.server
    [session-state :refer [queue-to-log]]
    [dom-manager :refer [client-id->action-data component->client-id]]
    [model-utils :refer [selector? semantic-elements abandon-problem-changes
                         semantic-to-list entity->canonical-semantic]]
    [order-utils :refer [furthest-item]])))

;;; TODO: Validate the data coming in, so mistakes won't cause us to
;;; crash.

;;; TODO: Replace the asserts with log messages, so things are robust.

(defn update-selected
  "Make the client id stored under the temporary id be the given id."
  [store temporary-id client-id]
  ;; We store the client id as a keyword, rather than a string, so it
  ;; is not semantic.
  (let [client-id-keyword (keyword client-id)
        element-id (first (id-label->element-ids
                           store subject :current-selection))]
        (if element-id
          (update-content store element-id client-id-keyword)
          (first (add-entity store temporary-id
                             (list client-id-keyword :current-selection))))))

(defn get-selected
  "Return the path stored in the above format in the given item."
  [store temporary-id]
  (when-let [element-id (first (id-label->element-ids
                                store temporary-id :current-selection))]
    (name (id->content store element-id))))

(defn update-set-content-if-matching
  "Set the content of the id in the store provided the current content
   matches 'from'."
  [store id from to]
  ;; There are several special cases to match: If we have a number,
  ;; the client will have a string. If the client had ..., it was a
  ;; a wild card, and we could have anything.
  (let [from (parse-string-as-number from)
        content (id->content store id)]
    (println "Old content" content)
    (if (and
         (or (equivalent-atoms? from content)
             ;; Wildcard text matches anything,
             ;; because it has to match instances too
             (= from "\u00A0...")
             ;; Setting a new selector.
             (and (= from "") (= content 'anything)))
         ;; When the user edits a heading whose value was filled in
         ;; automatically, the UI clears the text to blank. Don't match
         ;; in that case, as we don't want to remove the original heading
         ;; if the user didn't type anything.
         (not (and (string? from)
                   (= (first from) \u00A0)
                   (not= from "\u00A0...")
                   (= to ""))))
      (update-content store id (parse-string-as-number to))
      (do (println "content doesn't match" from content)
          store))))

(defn update-set-content
  [store id from to]
  (println "!!!" (selector? (description->entity id store)))
  (let [to (if (and (= to "")
                    (selector? (description->entity id store)))
             'anything
             to)
        modified (update-set-content-if-matching store id from to)]
    (abandon-problem-changes store modified id)))

(defn do-set-content
  [store {:keys [target-ids from to]}]
  (when (and from to)
    (let [to (parse-string-as-number (clojure.string/trim to))]
      (println "Setting " (count target-ids) "items from" from "to" to)
      (reduce
       (fn [store id]
         (update-set-content store id from to))
       store target-ids))))

(comment
  (defn pop-content-from-key
    "If the last item of the key is :content, remove it."
    [key]
    (if (= (last key) :content) (pop key) key))

  (defn substitute-for-pattern
    "Given a pattern and a list of items, replace the pattern by the id of the
  appropriate item.
  A pattern is of the form [:pattern <number>? :subject? <template>?]
    number indicates which of the items in the list to use, defaulting to 0.
    :subject, if present, indicates that the result of the pattern should be
      the subject of the match, rather than the match, itself.
    template, if present, must contains a variable named :v, and will return
      the part of the item matching that variable. Otherwise, the
      entire item is used."
    [pattern items]
    (let [[_ & args] pattern
          [item args] (if (number? (first args))
                        [(nth items (first args)) (rest args)]
                        [(first items) args])
          [subject? args] (if (= (first args) :subject)
                            [true (rest args)]
                            [false args])
          template (first args)
          match (if template
                  (let [matches (matching-extensions template item)
                        value (:v (first matches))]
                    (assert value)
                    value)
                  item)]
      (:item-id (if subject? (subject match) match))))

  (defn substitute-in-key
    "Substitute into the sequence of possible patterns, instantiating patterns
  from the items."
    [key items]
    (vec (map (fn [part]
                (if (and (sequential? part) (= (first part) :pattern))
                  (substitute-for-pattern part items)
                  part))
              key)))

  (defn add-select-request
    "Add a selection request to a response, given the sequence of items
  to use for substituting in the pattern."
    [response items select-pattern old-key]
    (if select-pattern
      (let [response (if (satisfies? cosheet.store/Store response)
                       {:store response}
                       response)
            store (:store response)]
        (assoc response :select
               (when (not-empty items)
                 [(substitute-in-key select-pattern items)
                  [old-key]])))
      response))

  (defn do-add-element
    [store arguments]
    (let [{:keys [select-pattern target-key]} arguments]
      (when-let [referent (:referent arguments)]
        (let [items (instantiate-referent referent store)
              [added store] (create-possible-selector-elements
                             'anything nil items items :after true store)]
          (add-select-request
           store [(first added)]
           (or select-pattern
               (conj (pop-content-from-key target-key) [:pattern]))
           target-key)))))

  (defn do-add-label
    [store arguments]
    (let [{:keys [select-pattern target-key]} arguments]
      (when-let [referent (:referent arguments)]
        (let [items (instantiate-referent referent store)
              sample-item (first items)
              is-tag (when sample-item
                       (seq (matching-elements :tag sample-item)))]
          (when (not is-tag)
            (let [[added store] (create-possible-selector-elements
                                 '(anything :tag) nil items items
                                 :after true store)]
              (add-select-request
               store [(first added)]
               (or select-pattern
                   ;; Labels don't include their item in their key,
                   ;; so back up to just before the target to make the
                   ;; pattern.
                   (let [item-back-in-key
                         (if (= (last target-key) :content) 2 1)]
                     (conj (subvec
                            target-key 0 (- (count target-key) item-back-in-key))
                           [:pattern])))
               target-key)))))))

  (defn do-add-twin
    [store arguments]
    (let [{:keys [target-key]} arguments]
      (when-let [referent (:referent arguments)]
        (when-let [condition (:template arguments)]
          (let [[items restrictions] (instantiate-referent-inheriting-restrictions
                                      referent store)
                subjects (map subject items)
                [added store] (create-possible-selector-elements
                               condition restrictions subjects items
                               :after true store)
                item-key (pop-content-from-key target-key)]
            (add-select-request
             store [(first added)]
             (conj (pop item-key) [:pattern]) target-key))))))

  (defn add-virtual [store target-key referent select-pattern]
    (println "adding virtual"
             (simplify-for-print [target-key referent select-pattern]))
    (let [[items new-ids store] (instantiate-or-create-referent referent store)]
      (add-select-request store (map #(description->entity % store) new-ids)
                          select-pattern target-key)))

  (defn do-add-virtual
    [store arguments]
    (let [{:keys [referent select-pattern target-key]} arguments]
      (add-virtual store target-key referent select-pattern)))

  (defn do-add-row
    [store arguments]
    (println "adding row")
    (let [{:keys [row column]} arguments]
      (add-virtual
       store (:target-key arguments)
       (virtual-referent (:template row) nil (:referent row))
       (conj (vec (butlast (:key row))) [:pattern] (:referent column)))))

  (defn update-delete
    "Given an item, remove it and all its elements from the store"
    [store item]
    (let [modified (remove-entity-by-id store (:item-id item))]
      (abandon-problem-changes store modified (subject item))))

  (defn do-delete
    "Remove item(s)." 
    [store arguments]
    (when-let [to-delete (:referent arguments)]
      (let [;; distinct should not be necessary, but is a
            ;; safety measure to make sure we don't delete twice
            items (distinct (instantiate-referent to-delete store))]
        (println "total items:" (count items))
        (if (:clear-only arguments)
          (reduce (fn [store item]
                    (update-set-content store item (content item) ""))
                  store items)
          (reduce update-delete store items)))))

  (defn do-expand
    [store arguments]
    (let [{:keys [referent session-state]} arguments]
      (when (referent? referent)
        ;; If the target is a single item with no elements, switch the target
        ;; to its subject.
        (let [items (instantiate-referent referent store)
              item (first items)
              [_ subject-ref] (referent->exemplar-and-subject referent)
              subject-ref (or subject-ref
                              (when-let [subject (subject item)]
                                (item-referent subject)))
              referent (if (and (every? #(= (content %) :tag)
                                        (semantic-elements-R item))
                                subject-ref)
                         subject-ref
                         referent)]
          {:store store
           :open (cond-> (str (:url-path session-state)
                              "?referent=" (referent->string referent)))}))))

  (defn do-selected
    [store {:keys [referent session-state target-key special] :as arguments}]
    (let [client-state (:client-state session-state)
          temporary-id (:session-temporary-id session-state)
          temporary-item (description->entity temporary-id store)
          key-changed (not= target-key (path-in-item temporary-item))
          store (if key-changed
                  (-> store
                      (update-store-with-path temporary-id target-key)
                      (update-equivalent-undo-point true))
                  store)]
      (cond
        (= special :tab)
        (do (map-state-reset! client-state :referent referent)
            {:store store
             :set-url (str (:url-path session-state)
                           "?referent=" (referent->string referent))})
        (= special :new-tab)
        (do-set-content
         store (merge-with (partial map-combiner nil)
                           arguments (:target arguments) {:from "" :to ""}))
        true
        (when key-changed store))))

  (defn batch-edit-select-key
    "Return the key for the item in the new batch edit that corresponds
   to the path, given the containment sequence to the currently selected item,
   and the new selectors that might match it."
    [containment-items batch-edit-selectors]
    (some
     (fn [selector]
       (when (= (item->canonical-semantic (first containment-items))
                (item->canonical-semantic selector))
         (when-let
             [key (first
                   (reduce
                    ;; Selector should be something on the batch edit side
                    ;; that has an element matching element-item. Find that
                    ;; matching element, add it to the key, and repeat one
                    ;; level lower.
                    (fn [[key selector] element-item]
                      (when-let [match (first (best-matching-element
                                               (immutable-semantic-to-list
                                                element-item)
                                               selector))]
                        [(concat (if (seq (matching-elements :tag match))
                                   ;; A label's parent does not go in the key.
                                   (butlast key)
                                   key)
                                 [(:item-id match)])
                         match]))
                    [[(:item-id selector)] selector]
                    (rest containment-items)))]
           (concat [:batch] key))))
     batch-edit-selectors))

  (defn do-batch-edit
    [store {:keys [referent target-key batch-edit-ids session-state]}]
    (when referent
      (let [target (first (instantiate-referent referent store))
            batch-edit-items (map #(description->entity % store) batch-edit-ids)
            client-state (:client-state session-state)
            top-item (first
                      (instantiate-referent
                       (map-state-get-current client-state :referent)
                       store))
            topic (first (label->elements top-item :tab-topic))
            row-condition (when topic
                            (first (label->elements topic :row-condition)))
            [containing-items element-item?] (batch-edit-containment-path target)
            batch-elements (or (seq batch-edit-items)
                               (when element-item? [(first containing-items)]))
            new-batch-selectors (when (and row-condition
                                           (or target batch-edit-items))
                                  (batch-edit-selectors
                                   row-condition batch-elements))]
        (when (not (empty? new-batch-selectors))
          (let [temporary-id (:session-temporary-id session-state)
                temporary-item (description->entity temporary-id store)
                store
                (as-> store store
                  (reduce (fn [s selector]
                            (remove-entity-by-id s (:item-id selector)))
                          store
                          (label->elements temporary-item :batch-selector))
                  (reduce (fn [s selector]
                            (first (add-entity s temporary-id selector)))
                          store new-batch-selectors)
                  (update-equivalent-undo-point store true))]
            (if-let [select (batch-edit-select-key
                             containing-items
                             (elements
                              (first (label->elements
                                      (description->entity temporary-id store)
                                      (if element-item?
                                        :batch-elements
                                        :batch-row-selector)))))]
              {:store store
               :select [select [target-key]]
               :batch-editing true}
              store))))))

  (defn request-selection-from-store
    "Return a client request asking it to select the target saved in the
   temporary item of the store."
    [mutable-store old-store session-state]
    (let [store (current-store mutable-store)
          temporary-id (:session-temporary-id session-state)]
      {:select [(path-in-item (description->entity temporary-id store))
                [(path-in-item (description->entity temporary-id old-store))]]}))

  (defn do-undo
    [mutable-store session-state]
    (let [old-store (current-store mutable-store)]
      (undo! mutable-store)
      (request-selection-from-store mutable-store old-store session-state)))

  (defn do-redo
    [mutable-store session-state]
    (let [old-store (current-store mutable-store)]
      (redo! mutable-store)
      (request-selection-from-store mutable-store old-store session-state)))

  (defn do-quit-batch-edit
    [mutable-store session-state]
    (let [client-state (:client-state session-state)]
      (map-state-reset! client-state :batch-editing false)))
  )

(defn adjust-handler-response
  "Adjust the handler's response to be a pair of an updated store and
  a map of client data."
  [response store]
  (if response
           (if (satisfies? cosheet.store/Store response)
             [response {}]
             (do
               (assert (map? response))
               (assert (:store response))
               [(:store response) (dissoc response :store)]))
           (do (println "handler didn't update store.")
               [store {}])))

(defn do-storage-update-action
  "Do an action that can update a store and also return any client
  information requested by the action.
  that expects a function from current store to new store and return value.
  We return a map containing the new store and any additional requested
  information.
  The action is given the store and any additional arguments.
  The action can either return nil, meaning no change, a new
  store, or a map with a :store value and any additional information
  it wants to convey."
  [mutable-store handler args]
  )

(defn get-contextual-handler
    "Return the handler for the command."
    [action]
  ({
    ; :add-element do-add-element
    ; :add-label do-add-label
    ; :add-sibling do-add-virtual
    ; :add-row do-add-row
    ; :add-column do-add-column
    ; :delete do-delete
    ; :delete-row do-delete-row
    ; :delete-column do-delete-column
    :set-content do-set-content
    ; :expand do-expand
    ; :selected do-selected
    ; :batch-edit do-batch-edit
      }
   action))

(defn do-contextual-action
  "Do an action that applies to a DOM component, and whose
  interpretation depends on that component. We will call a contextual
  action handler with a map of the action data for the component,
  plus :client-id, :session-state, and any other arguments the client
  provided. In addition to information for the client, the handler can
  also specify a :batch-editing target."
    [mutable-store session-state [action-type client-id & {:as client-args}]]
    (let [handler (get-contextual-handler action-type) 
          manager (:dom-manager session-state)]
      (cond (not handler)
            (println "Unhandled action type:" action-type)
            (not client-id)
            (println "No context specified:" action-type)
            true
            (let [_ (println "command: "
                             (map simplify-for-print
                                  (list* action-type client-id
                                         (map concat (seq client-args)))))
                  result
                  (store-update-control-return!
                   mutable-store
                   (fn [store]
                     (let [action-data (client-id->action-data
                                        @manager client-id action-type store)
                           arguments (-> action-data
                                         (into client-args)
                                         (assoc :session-state session-state
                                                :client-id client-id))
                           store (or (:store action-data) store)
                           store (update-equivalent-undo-point store false)
                           response (handler store arguments)]
                       (println "handler arguments: "
                                (simplify-for-print
                                 (dissoc arguments :session-state)))
                       (adjust-handler-response response store))))]
              (when (contains? result :batch-editing)
                (map-state-reset! (:client-state session-state) :batch-editing
                                  (:batch-editing result)))
              (dissoc result :batch-editing)))))

(defn do-action
  "Update the store, in accordance with the action, and return a map
  of any information to give the client. The map can have any of:
                  :open  A url to open in a new window
               :set-url  A url to set as the current url
      :select-store-ids  A seq of store ids such that the client should
                         select a component that represents one of them.
           :if-selected  A seq of client ids, one of which must currently
                         be selected by the client for select-store-ids
                         to have an effect."
  [mutable-store session-state action]
  (let [[action-type & additional-args] action]
    (println "doing action " action-type)
    (if-let [handler (case action-type
                       ; :undo do-undo
                       ; :redo do-redo
                       ; :quit-batch-edit do-quit-batch-edit
                       nil)]
      (do (println "command: " (map simplify-for-print action))
          (apply handler mutable-store session-state additional-args))
      (if (= (mod (count action) 2) 0)
        (do-contextual-action mutable-store session-state action)
        (println "Error: odd number of keyword/argument pairs:" action)))))

(defn do-actions
  "Run the actions, and return any additional information to be returned
  to the client."
  [mutable-store session-state action-sequence]
  (try
    (reduce (fn [client-info action]
              (let [for-client
                    (do-action mutable-store session-state action)]
                (println "for client " (simplify-for-print for-client))
                (into (cond-> client-info
                        (:select-store-ids for-client)
                        ;; Get rid of obsolete if-selected, since a new
                        ;; :select-store-ids might not come with one.
                        (dissoc :if-selected))
                      (select-keys for-client
                                   [:select-store-ids :if-selected
                                    :open :set-url]))))
            {} action-sequence)
    (catch Exception e
      (queue-to-log [:error (str e)] (:url-path session-state))
      (println "Error" (str e))
      (clojure.stacktrace/print-stack-trace e)
      nil)))

(defn confirm-actions
  "Check that the actions have not already been done, update the
  last-action to reflect that these actions have
  been done, and return the sequence of actions to be done."
  [actions client-state]
  (map-state-change-value-control-return!
   client-state :last-action
   (fn [last-action]
     (let [later-times (cond->> (sort (keys actions))
                         last-action
                         (filter #(pos? (compare % last-action))))]
       [(if (empty? later-times) last-action (last later-times))
        (map actions later-times)]))))

(ns cosheet2.server.actions
  (:require
   (cosheet2
    [debug :refer [simplify-for-print]]
    [utils :refer [parse-string-as-number thread-map add-elements-to-entity-list
                   swap-control-return! equivalent-atoms?]]
    [map-state :refer [map-state-get-current map-state-reset!
                       map-state-change-value!
                       map-state-change-value-control-return!]]
    [store :refer [update-content
                   equivalent-undo-point? update-equivalent-undo-point
                   fetch-and-clear-modified-ids
                   store-update! store-update-control-return!
                   id->subject id-label->element-ids id-valid? undo! redo!
                   current-store
                   id-label->element-ids id->content
                   StoredItemDescription
                   Store]]
    [store-impl :refer [id->string string->id]]
    [store-utils :refer [add-entity remove-entity-by-id]]
    mutable-store-impl
    [entity :refer [StoredEntity description->entity to-list label->element
                    content elements label->elements label->content subject]]
    [hiccup-utils :refer [dom-attributes map-combiner]]
    [query :refer [matching-elements matching-extensions]]
    query-impl
    [orderable :refer [initial split]])
   (cosheet2.server
    [session-state :refer [queue-to-log]]
    [dom-manager :refer [client-id->action-data component->client-id]]
    [model-utils :refer [selector? semantic-elements abandon-problem-changes
                         ordered-semantic-to-list entity->canonical-semantic
                         create-possible-selector-elements
                         exemplar-to-query remove-semantic-elements]]
    [order-utils :refer [furthest-item
                         update-add-entity-with-order-and-temporary]])))

;;; TODO: Validate the data coming in, so mistakes won't cause us to
;;; crash.

;;; TODO: Replace the asserts with log messages, so things are robust.

(defn update-selected
  "Make the client id stored under the temporary id be the given id."
  [store temporary-id client-id]
  (if-let [element-id (first (id-label->element-ids
                                store temporary-id :current-selection))]
    ;; We store the client id as a keyword, rather than a string, so it
    ;; is not semantic.
    (update-content store element-id (keyword client-id))
    store))

(defn get-selected
  "Return the client stored by update-selected."
  [store temporary-id]
  (when-let [element-id (first (id-label->element-ids
                                store temporary-id :current-selection))]
    (let [content (id->content store element-id)]
      (when (keyword? content)
        (name content)))))

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
  (let [to (if (and (= to "")
                    (selector? (description->entity id store)))
             'anything
             to)
        modified (update-set-content-if-matching store id from to)]
    (abandon-problem-changes store modified id)))

(defn add-select-store-ids-request
  [response ids session-state]
  (let [temporary-id (:session-temporary-id session-state)
        response (if (satisfies? Store response) {:store response} response)
        current-selection (get-selected (:store response) temporary-id)]
    (cond-> (assoc response :select-store-ids ids)
      current-selection (assoc :if-selected [current-selection]))))

(defn do-set-content
  [store {:keys [target-ids from to session-state]}]
  (when (and from to (seq target-ids))
    (let [to (parse-string-as-number (clojure.string/trim to))]
      (println "Setting " (count target-ids) "items from" from "to" to)
      (->
       (reduce
        (fn [store id]
          (update-set-content store id from to))
        store target-ids)
       ;; We might have set the content on a virtual item.
       ;; This will make sure any newly created item is selected.
       (add-select-store-ids-request target-ids session-state)))))

(defn do-add-twin
  [store {:keys [target-ids template session-state]}]
  (when (not= template :singular)
   (let [[ids store] (create-possible-selector-elements
                      (or template 'anything)
                      (map #(id->subject store %) target-ids)
                      target-ids
                      :after true store)]
     (add-select-store-ids-request store ids session-state))))

(defn do-add-element
  [store {:keys [target-ids session-state]}]
  (let [[ids store] (create-possible-selector-elements
                     'anything target-ids target-ids
                     :before false store)]
    (add-select-store-ids-request store ids session-state)))

(defn do-add-label
  [store {:keys [target-ids session-state]}]
  (let [[ids store] (create-possible-selector-elements
                     '(anything :label) target-ids target-ids
                     :before false store)]
    (add-select-store-ids-request store ids session-state)))

(defn do-delete 
  [store {:keys [target-ids template]}]
  (assert (= (count target-ids) (count (distinct target-ids)))
          target-ids)
  (when (not= template :singular)
    (reduce (fn [store id]
              (let [subject-id (id->subject store id) 
                    modified (remove-entity-by-id store id)]
                (abandon-problem-changes store modified subject-id)))
            store target-ids)))

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
      (let [response (if (satisfies? Store response)
                       {:store response}
                       response)
            store (:store response)]
        (assoc response :select
               (when (not-empty items)
                 [(substitute-in-key select-pattern items)
                  [old-key]])))
      response))

  (defn do-add-row
    [store arguments]
    (println "adding row")
    (let [{:keys [row column]} arguments]
      (add-virtual
       store (:target-key arguments)
       (virtual-referent (:template row) nil (:referent row))
       (conj (vec (butlast (:key row))) [:pattern] (:referent column)))))

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

  
  )

(defn matching-element-ids
  "Given an id and an id that is a template for one of its elements,
  return the ids of the elements that matches it."
  [id template-id store]
  (when id
    (let [query (exemplar-to-query (description->entity template-id store))]
      (map :item-id
       (matching-elements query (description->entity id store))))))

(defn do-batch-edit
  [store {:keys [query-ids stack-ids
                 selected-index selection-sequence must-show-label
                 session-state]}]
  (let [temporary-id (:session-temporary-id session-state)
        temporary-item (description->entity temporary-id store)]
    (if query-ids
      (let [[new-ids [store _]]
            ;; For each of query-id and stack-id, replace the
            ;; temporary item's elements with the new elements.
            (reduce
             ;; This function returns a list of new ids, plus a new
             ;; [store order] pair.
             (fn [[_ [store order]] [item-label ids]]
               (let [item (label->element temporary-item item-label)
                     subject-id (:item-id item)
                     new-lists (map #(ordered-semantic-to-list
                                      (description->entity % store))
                                    ids)
                     store (remove-semantic-elements store (:item-id item))]
                 (thread-map
                  (fn [new-list [store order]]
                    (let [[store id remainder]
                          (update-add-entity-with-order-and-temporary
                           store subject-id new-list
                           order :before :false)]
                      [id [store remainder]]))
                  new-lists
                  [store order])))
             [nil [store initial]]
             [[:batch-query query-ids]
              ;; By passing the stack ids last, the new ids
              ;; corresponding to them will be what the reduce
              ;; returns.
              [:batch-stack stack-ids]])
            selected-ids (when selected-index
                           (reduce
                            (fn [ids template-id]
                              (mapcat (fn [id] (matching-element-ids
                                                 id template-id store))
                                      ids))
                            [(nth new-ids selected-index)]
                            selection-sequence))]
        {:store (update-equivalent-undo-point store true)
         :select-store-ids selected-ids
         :batch-editing true})
      ;; TODO: This can be confusing when the user has something selected that
      ;; doesn't have batch editing information. Check for no selection before
      ;; doing this.
      (when (seq (semantic-elements
                  (label->element temporary-item :batch-query)))
        ;; Reuse the last batch edit specification.
        {:store store
         :batch-editing true}))))

(defn do-quit-batch-edit
  [mutable-store session-state]
  (map-state-reset! (:client-state session-state)
                    {:batch-editing false})
  {})

(defn adjust-handler-response
  "Adjust the handler's response to be a pair of an updated store and
  a map of client data."
  [response store]
  (if response
    (if (satisfies? Store response)
      [response {}]
      (do
        (assert (map? response))
        (assert (:store response))
        [(:store response) (dissoc response :store)]))
    (do (println "handler didn't update store.")
        [store {}])))

(defn get-contextual-handler
  "Return the handler for the command.
  A handler is a function of the current store and a map of arguments
  and action data. It can either return nil, meaning no change, a new
  store, or a map with a :store value and any additional information
  it wants to convey."
  [action]
  ({:add-element do-add-element
    :add-label do-add-label
    :add-twin do-add-twin
    ; :add-row do-add-row
    ; :add-column do-add-column
    :delete do-delete
    ; :delete-row do-delete-row
    ; :delete-column do-delete-column
    :set-content do-set-content
    ; :expand do-expand
    :batch-edit do-batch-edit}
   action))

(defn do-contextual-action
  "Do an action that applies to a DOM component, and whose
  interpretation depends on that component. We will call a contextual
  action handler with a map of the action data for the component,
  plus :template from the spec, plus :client-id, :session-state, and
  any other arguments the client provided. In addition to information
  for the client, the handler can also specify whether we
  are :batch-editing."
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
                         spec (:dom-specification @(:component action-data))
                         spec-info (select-keys
                                    spec [:template])
                         arguments (-> action-data
                                       (into spec-info)
                                       (into client-args)
                                       (assoc :session-state session-state
                                              :client-id client-id))
                         _ (println "handler arguments: "
                                    (simplify-for-print
                                     (dissoc arguments :session-state)))
                         store (or (:store action-data) store) 
                         response (handler
                                   (update-equivalent-undo-point store false)
                                   arguments)]
                     (adjust-handler-response response store))))]
            (when (contains? result :batch-editing)
              (map-state-reset! (:client-state session-state)
                                {:batch-editing (:batch-editing result)}))
            (dissoc result :batch-editing)))))

(defn request-selection-from-store
   "Return a client request asking it to select the target saved in the
   temporary item of the store."
    [mutable-store old-store session-state]
    (let [store (current-store mutable-store)
          temporary-id (:session-temporary-id session-state)]
      {:select (get-selected store temporary-id)
       :if-selected (when-let [former (get-selected old-store temporary-id)]
                      [former])}))

;;; While do-selected takes a client id, like a contextual action
;;; does, it doesn't rely on what that client id references. In
;;; particular, it does want create more items if a virtual id is
;;; selected. So it can not be handled like other contextual handlers.
(defn do-selected
  [mutable-store session-state client-id & _]
  (when client-id
    (let [{:keys [client-state session-temporary-id dom-manager]} session-state
          action-data (client-id->action-data
                       @dom-manager client-id :select
                       (current-store mutable-store))
          {:keys [select]} action-data]
      (map-state-reset! client-state {:select-store-ids nil
                                      :if-selected nil})
      (store-update!
       mutable-store
       (fn [store]
         (if (not= client-id (get-selected store session-temporary-id))
           (-> store
               (update-selected session-temporary-id client-id)
               (update-equivalent-undo-point true))
           store)))
      (when select
        (when-let [tab-id (:tab-id select)]
          (do
            (map-state-reset! client-state {:root-id tab-id})
            {:set-url (str (:url-path session-state)
                           "?root=" (id->string tab-id))}))))))

(defn do-undo
  [mutable-store session-state & _]
  (let [old-store (current-store mutable-store)]
    (undo! mutable-store)
    (when (not= old-store (current-store mutable-store))
      (request-selection-from-store mutable-store old-store session-state))))

(defn do-redo
  [mutable-store session-state & _]
  (let [old-store (current-store mutable-store)]
    (redo! mutable-store)
    (when (not= old-store (current-store mutable-store))
      (request-selection-from-store mutable-store old-store session-state))))

;;; TODO: Check for :handle-action, and do what it says. In
;;; particular, there should be a version that takes keyword arguments
;;; of special handlers for specific actions.
(defn do-action
  "Update the store, in accordance with the action, and return a map
  of any information to give the client. The map can have any of:
                  :open  A url to open in a new window
               :set-url  A url to set as the current url
                :select  A client id to select
      :select-store-ids  A seq of store ids such that the client should
                         select a component that represents one of them.
           :if-selected  A seq of client ids, one of which must currently
                         be selected by the client for select or 
                         select-store-ids to have an effect."
  [mutable-store session-state action]
  (let [[action-type & extra-args] action]
    (println)
    (println)
    (println "DOING ACTION " action-type)
    (if-let [handler (case action-type
                       :undo do-undo
                       :redo do-redo
                       :selected do-selected
                       :quit-batch-edit do-quit-batch-edit
                       nil)]
      (do (println "command: " (map simplify-for-print action))
          (apply handler mutable-store session-state extra-args))
      (if (= (mod (count action) 2) 0)
        (do-contextual-action mutable-store session-state action)
        (println "Error: odd number of keyword/argument pairs:" action)))))

(defn do-actions
  "Run the actions, and return any additional information to be returned
  to the client."
  [mutable-store session-state action-sequence]
  (try
    (let [client-info
          (reduce (fn [client-info action]
                    (let [for-client
                          (do-action mutable-store session-state action)]
                      (println "Equivalent undo:"
                               (equivalent-undo-point?
                                (current-store mutable-store)))
                      (println "for client " (simplify-for-print for-client))
                      (into (cond-> client-info
                              (or (:select for-client)
                                  (:select-store-ids for-client))
                              ;; Get rid of obsolete selection information.
                              (dissoc :select :select-store-ids :if-selected))
                            (select-keys for-client
                                         [:select :select-store-ids :if-selected
                                          :open :set-url]))))
                  {} action-sequence)]
      (let [{:keys [client-state]} session-state
            root-id (map-state-get-current client-state :root-id)]
        ;; If the root id has become invalid, set our copy to nil, and
        ;; tell the client to set its url back to no root
        ;; id. Otherwise, if creating a new tab is undone, the client
        ;; url will still have its old id, and when some new item gets
        ;; that id, we will try to focus on it.
        (if (and root-id
                 (not (:set-url client-info))
                 (not (id-valid? (current-store mutable-store) root-id)))
          (do
            (map-state-change-value! client-state :root-id (constantly nil))
            (assoc client-info :set-url (str (:url-path session-state) "?")))
          client-info)))
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

(ns cosheet2.server.actions
  (:require
   (cosheet2
    [debug :refer [simplify-for-print]]
    [utils :refer [parse-string-as-number thread-map truncate-at-value
                   swap-control-return!]]
    [canonical :refer [equivalent-primitives?]]
    [map-state :refer [map-state-get-current map-state-reset!
                       map-state-change-value!
                       map-state-change-value-control-return!]]
    [store :refer [update-source
                   equivalent-undo-point? update-equivalent-undo-point
                   fetch-and-clear-modified-ids
                   store-update! store-update-control-return!
                   id->target target-label->ids id-valid-link?
                   object-id? link-id? item-id? interned-object-id?
                   undo! redo!
                   name-label-id
                   current-store
                   id->string string->id id->source
                   Store]]
    [store-utils :refer [remove-entity-by-id add-object]]
    [entity :refer [id->entity id->element make-object-list
                    elements content content->elements
                    label->element label->elements label->content
                    name-label object? element? interned-object? label-element?
                    link-type-object? object-type-object? non-type-object?]]
    [query :refer [matching-items]]
    mutable-store-impl
    [hiccup-utils :refer [dom-attributes map-combiner]]
    [query :refer [matching-elements matching-extensions]]
    query-impl
    [orderable :refer [initial split]]
    [map-state :refer [map-state-get-current]])
   (cosheet2.server
    [session-state :refer [queue-to-log]]
    [dom-manager :refer [client-id->action-data
                         client-id->relative-ids
                         relative-ids->client-id]]
    [model-utils :refer [selector? semantic-elements abandon-problem-changes
                         ordered-semantic-to-list entity->canonical-semantic
                         create-possible-selector-elements
                         exemplar-to-fixed-term remove-semantic-elements
                         label-object-template
                         table-row-template table-column-headers-id
                         unspecified-column-header-template
                         update-add-element-with-order-and-temporary
                         get-or-make-ordered-object-by-name
                         object-semantic-to-list]]
    [render-utils :refer [sequential-template?]]
    [order-utils :refer [furthest-item order-element-for-item]])))

;;; TODO: Validate the data coming in, so mistakes won't cause us to
;;; crash.

;;; TODO: Replace the asserts with log messages, so things are robust.

(defn update-selected
  "Store the client id of the currently selected dom as a temporary in
  the store. (We put it in the store, because that way, when
  there is an undo, we can undo to the last selection.)"
  [store temporary-id client-id]
  (if-let [element-id (first (target-label->ids
                              store temporary-id :current-selection))]
    ;; We store the client id as a keyword, rather than a string, so it
    ;; is not semantic.
    (update-source store element-id (keyword client-id))
    store))

(defn get-selected
  "Retrieve the client id of the currently selected dom, as stored by
  update-selected."
  [store temporary-id]
  (when-let [element-id (first (target-label->ids
                                store temporary-id :current-selection))]
    (let [source (id->source store element-id)]
      (when (keyword? source)
        (name source)))))

(defn ensure-response-map
  "If the response is a store, turn it into a map {:store response}"
  [response]
  (if (satisfies? Store response)
    {:store response}
    response))

(defn current-source-matches-from?
  "Return true if the store's source for the id matches the from value
  that the client reported, in the context of changing from to to.
  We use this to make sure the store we are about to change looks like
  what the user saw when they asked for a change."
  [store id from to]
  ;; There are several special cases for what counts as match:
  ;;   * If the store has a number, the client's from will be a
  ;;     string.
  ;;   * If the client's from is an uninterned object, any
  ;;     semantically equivalent one should count as a match. (Actions
  ;;     like add-twin can make multiple non-interned objects, which
  ;;     are semantically identical. So we allow that.
  ;;   * If the client had ..., it was a wild card, and we could
  ;;     have anything.
  ;;   * if the source has 'anything, the client should have "".
  (let [from (parse-string-as-number from)
        source (id->source store id)]
    (or
     ;; Equivalent primitives
     (and ; The nots here avoid a crash in equivalent-primitives?
      (not (item-id? source))
      (not (item-id? from))
      (equivalent-primitives? from source)
      ;; When the user edits a heading whose value was filled in
      ;; automatically, the UI clears the text to blank. Don't
      ;; match in that case, if to is "", as we don't want to
      ;; remove the original heading if the user didn't type
      ;; anything.
      (not (and (string? from)
                (= (first from) \u00A0)
                (not= from "\u00A0...")
                (= to ""))))
     (and (item-id? source)
          (item-id? from)
          (if (and (object-id? from)
                   (not (interned-object-id? store from)))
            ;; Equivalent non-interned objects
            (= (entity->canonical-semantic (id->entity from store))
               (entity->canonical-semantic (id->entity source store)))
            ;; Identical identified objects
            (= source from)))
     ;; Wildcard text matches anything, because it has to match
     ;; instances too in batch edit.
     (= from "\u00A0...")
     ;; Setting a formerly universal selector
     (and (= from "") (= source 'anything)))))

(defn update-set-source
  "Set the source to to, provided it previously matched from."
  [store id from to]
  (let [to (if (and (= to "")
                    (selector? (id->entity id store)))
             'anything
             to)]
    (if (current-source-matches-from? store id from to)
      (let [modified (update-source store id (parse-string-as-number to))]
        (abandon-problem-changes store modified id))
      (do (println "Old source doesn't match" from (id->source store id))
          store))))

(defn add-select-store-ids-request
  "Add a :select-store-ids instruction to a response, to select an item
  showing one of the specified ids. The ajax reply handler will
  translate that to a :select instruction."
  ;; TODO: If an item shows up at several places in a table, and the user
  ;;       adds an element to it, there is no guarantee that the selected
  ;;       element will be the one under the item view the user was editing.
  [response ids session-state]
  (let [temporary-id (:session-temporary-id session-state)
        response (ensure-response-map response)
        current-selection (get-selected (:store response) temporary-id)]
    (cond-> (assoc response :select-store-ids ids)
      current-selection (assoc :if-selected [current-selection]))))

(defn do-set-content
  [store {:keys [subject-ids past-subject-ids template is-object-name
                 from to session-state]}]
  (when (and from to (seq subject-ids)
             (every? link-id? subject-ids)
             (not (equivalent-primitives? from to)))
    (let [last-template (if (sequential-template? template)
                          ;; We need to get to the last template, which
                          ;; should be an object reference.
                          (last (:template-sequence template))
                          ;; The incoming template is for the whole
                          ;; element or its content.
                          template)
          ;; Usually, the template would be for the content we are
          ;; setting. But if an element has nothing but its content,
          ;; then there is not necessarily a separate component for
          ;; the content, and we will get the template from the
          ;; element. So we have to check if the template is an
          ;; element, and get its content in that case.
          template (if (element? last-template)
                     (content last-template)
                     last-template)]
      (if is-object-name
        ;; We are setting a new name in a place that holds a named object.
        ;; First, get an object corresponding to the name. Then check
        ;; that the position still holds an object, and swap in the
        ;; new one.
        ;; TODO: !!!  We need to handle reversed links, which we can
        ;; do by checking which end matches the old object.
        (let [name (clojure.string/trim to)
              order-element (order-element-for-item
                             (id->element (first subject-ids) store) store)
              order (content order-element)
              ;; The template might have a generic name. Remove it, or
              ;; we'll make an object with both that and the name the
              ;; user set.
              template (make-object-list
                        (remove #(and (seq (content->elements % name-label))
                                      (= (content %) ""))
                                (elements template)))
              [store object-id remainder] (get-or-make-ordered-object-by-name
                                           store name template order :after)
              store (update-source store (:item-id order-element) remainder)
              ;; TODO: !!! This needs to handle orientation.
              logical-from (id->source store (first subject-ids))]
          ;; We are going to claim that the user saw logical-from when
          ;; they asked for the change. Make sure that what the user
          ;; actually saw is consistent with that.
          (when (or
                 ;; We were already empty.
                 (and (or (= logical-from "")
                          (= logical-from 'anything))
                      (= from ""))
                 ;; There was an object with the name the user saw.
                 (and (object-id? logical-from)
                      (let [name (-> (id->entity logical-from store)
                                     (label->elements name-label)
                                     first
                                     content)]
                        (equivalent-primitives? name from))))
            (let [store (reduce
                         (fn [store element-id]
                           ;; TODO: !!! This needs to handle orientation.
                           (update-set-source
                            store element-id logical-from object-id))
                         store subject-ids)]
              ;; TODO: !!! This needs to handle orientation.
              (if-let [name-element-id
                       (first (target-label->ids
                               store object-id name-label-id))]
                (add-select-store-ids-request
                 {:store store} [name-element-id] session-state)
                store))))
        (let [to (parse-string-as-number (clojure.string/trim to))]
          (println "Setting" (count subject-ids) "items from" from "to"
                   (if (object-id? to)
                     (object-semantic-to-list (id->entity to store))
                     to))
          (->
           (reduce
            (fn [store id]
              (update-set-source store id from to))
            store subject-ids)
           ;; We might have set the source on a virtual item.
           ;; This will make sure any newly created item is selected.
           (add-select-store-ids-request subject-ids session-state)))))))

(defn do-add-twin
  [store {:keys [subject-ids template is-object-name session-state]}]
  (when (not= template :singular)
    (let [template (cond (not template) 'anything
                         (object? template) (do (assert is-object-name)
                                                `(~template))
                         true template)
          [ids store] (create-possible-selector-elements
                      template
                      (map #(id->target store %) subject-ids)
                      subject-ids
                      :after true store)]
     (add-select-store-ids-request store ids session-state))))

(defn do-add-element
  [store {:keys [subject-ids session-state]}]
  (let [[ids store] (create-possible-selector-elements
                     'anything subject-ids subject-ids
                     :before false store)]
    (add-select-store-ids-request store ids session-state)))

(defn do-add-label
  [store {:keys [subject-ids session-state]}]
  ;; We disallow adding a label to a label.
  (when (not-any? #(label-element? (id->entity % store)) subject-ids)
    (let [[ids store] (create-possible-selector-elements
                       `(~label-object-template) subject-ids subject-ids
                       :before false store)]
      (add-select-store-ids-request store ids session-state))))

(defn do-add-row
  [store {:keys [row-id table-id column-ids client-id]}]
  (println "adding row")
  (when (and row-id table-id)
    (let [table-entity (id->entity table-id store)
          row-template (table-row-template table-entity)
          row-parent-id (id->target store row-id)
          [ids store] (create-possible-selector-elements
                       row-template [row-parent-id] [row-id]
                       :after false store)]
      (if (and column-ids client-id)
        ;; Select the cell in the new row that is in the same column
        ;; as the cell that was selected.
        (let [relative-ids (client-id->relative-ids client-id)
              prefix-ids (truncate-at-value relative-ids row-id)
              new-cell-client-id (relative-ids->client-id
                                  (concat prefix-ids
                                          [(first ids) (first column-ids)]))]
          {:store store
           :select new-cell-client-id})
        store))))

(defn do-add-column
  [store {:keys [column-ids table-id row-id client-id]}]
  (println "adding column")
  (when (and column-ids table-id)
    (let [column-headers-id (table-column-headers-id table-id store)
          [ids store] (create-possible-selector-elements
                       unspecified-column-header-template
                       [column-headers-id] [(last column-ids)]
                       :after false store)]
      (if (and row-id client-id)
        ;; Select the cell in the new column that is in the same row
        ;; as the cell that was selected.
        (let [relative-ids (client-id->relative-ids client-id)
              prefix-ids (truncate-at-value relative-ids row-id)
              new-cell-client-id (relative-ids->client-id
                                  (concat prefix-ids
                                          [row-id (first ids)]))]
          {:store store
           :select new-cell-client-id})
        store))))

(defn do-delete 
  [store {:keys [subject-ids template]}]
  (assert (= (count subject-ids) (count (distinct subject-ids)))
          subject-ids)
  (when (not= template :singular)
    (reduce (fn [store id]
              (let [target-id (id->target store id) 
                    modified (remove-entity-by-id store id)]
                (abandon-problem-changes store modified target-id)))
            store subject-ids)))

(defn do-delete-row
  [store {:keys [row-id]}]
  (println "deleting row")
  (when row-id
    (remove-entity-by-id store row-id)))

(defn do-delete-column
  [store {:keys [column-ids table-id]}]
  (println "deleting row")
  (when (and column-ids table-id)
    (let [column-headers-id (table-column-headers-id table-id store)
          column-headers-entity (id->entity column-headers-id store)
          columns (semantic-elements column-headers-entity)]
      (when (and (> (count columns) 1) ; Don't remove the last column.
                 (= (count column-ids) 1)) ; Don't remove multiple columns.
        (remove-entity-by-id store (first column-ids))))))

(defn do-expand
    [store {:keys [subject-ids session-state]}]
  (when-let [subject-id (first subject-ids)]
    (let [root-id (map-state-get-current (:client-state session-state) :root-id)
          target (id->target store subject-id)
          ;; In two cases we want to show the target of the
          ;; subject, rather than the subject, itself:
          ;;    * The subject is an element with nothing but a label.
          ;;    * The subject is the current root.
          show-target (when target
                        (or (let [entity (id->entity subject-id store)
                                  displayed-elements (semantic-elements entity)]
                              (and (<= (count displayed-elements) 1)
                                   (every? label-element?
                                           displayed-elements)))
                            (= subject-id root-id)))
          id-to-open (if show-target target subject-id)]
      {:store store
       :open (str "?root=" (id->string id-to-open))})))

(defn matching-element-ids
  "Given an id and an id that is a template for one of its elements,
  return the ids of the elements that matches it."
  [id template-id store]
  (when id
    (let [query (exemplar-to-fixed-term
                 (id->entity template-id store))]
      (map :item-id
       (matching-elements query (id->entity id store))))))

(defn do-batch-edit
  [store {:keys [query-ids stack-ids
                 selected-index selection-sequence must-show-label
                 session-state]}]
  (let [temporary-id (:session-temporary-id session-state)
        temporary-item (id->entity temporary-id store)]
    (if query-ids
      (let [[new-ids [store _]]
            ;; For each of query-id and stack-id, replace the
            ;; temporary item's elements with the new elements.
            (reduce
             ;; This function returns a list of new ids, plus a new
             ;; [store order] pair.
             (fn [[_ [store order]] [item-label ids]]
               (let [item (label->element temporary-item item-label)
                     target-id (:item-id item)
                     new-lists (map #(ordered-semantic-to-list
                                      (id->entity % store))
                                    ids)
                     store (remove-semantic-elements store (:item-id item))]
                 (thread-map
                  (fn [new-list [store order]]
                    (let [[store id remainder]
                          (update-add-element-with-order-and-temporary
                           store target-id new-list
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

(defn normalize-handler-response
  "Whatever format the handler's response, normalize it to be a pair of
  an updated store and a map of data for the client.
  The store argument must be the original store, without any virtuals
  instantiated. It's the store we return if the handler returns nil."
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
  and action data. If it doesn't want to make any changes, it must
  return nil (which tells the calling code to throw out any virtual
  instantiations it made to prepare for the handler). Otherwise it
  either returns a revised store, or a map with a :store value and any
  additional information it wants to convey."
  [action]
  ({:add-element do-add-element
    :add-label do-add-label
    :add-twin do-add-twin
    :add-row do-add-row
    :add-column do-add-column
    :delete do-delete
    :delete-row do-delete-row
    :delete-column do-delete-column
    :set-content do-set-content
    :expand do-expand
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
                         spec-info (select-keys spec [:template])
                         arguments (-> action-data
                                       (into spec-info)
                                       (into client-args)
                                       (assoc :session-state session-state
                                              :client-id client-id))
                         _ (println "HANDLER ARGUMENTS: "
                                    (simplify-for-print
                                     (dissoc arguments :session-state)))
                         store-with-virtuals (or (:store action-data) store) 
                         response (handler
                                   (update-equivalent-undo-point
                                    store-with-virtuals false)
                                   arguments)]
                     (normalize-handler-response response store))))]
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
;;; particular, it does not want to create more items if a virtual id
;;; is selected. So it can't be handled like other contextual
;;; handlers.
(defn do-selected
  [mutable-store session-state client-id & _]
  (when client-id
    (let [{:keys [client-state session-temporary-id dom-manager]} session-state
          action-data (client-id->action-data
                       @dom-manager client-id :select
                       (current-store mutable-store))
          {:keys [tab-id]} action-data]
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
      (when tab-id
        (do
          (map-state-reset! client-state {:root-id tab-id})
          {:set-url (str (:url-path session-state)
                         "?root=" (id->string tab-id))})))))

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
  of any additional instructions to give the client.
  The map can have any of:
                  :open  A url to open in a new window
               :set-url  A url to set as the current url
                :select  A client id to select
      :select-store-ids  A seq of store ids such that the client should
                         select a component that represents one of them.
                         These will get translated to :select before
                         the response goes to the client.
           :if-selected  A seq of client ids, one of which must currently
                         be selected by the client for :select or 
                         :select-store-ids to have an effect."
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
                 (not (id-valid-link? (current-store mutable-store) root-id)))
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

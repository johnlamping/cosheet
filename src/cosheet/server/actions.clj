(ns cosheet.server.actions
  (:require
   (cosheet
    [debug :refer [simplify-for-print]]
    [utils :refer [parse-string-as-number thread-map
                   swap-control-return! equivalent-atoms?]]
    [state-map :refer [state-map-get-current-value state-map-reset!
                       state-map-swap-control-return!]]
    [store :refer [update-content update-valid-undo-point
                   fetch-and-clear-modified-ids
                   do-update-control-return!
                   id->subject id-valid? undo! redo! current-store]]
    [store-utils :refer [add-entity remove-entity-by-id]]
    mutable-store-impl
    [entity :refer [StoredEntity description->entity to-list
                    content elements label->elements label->content subject]]
    [hiccup-utils :refer [dom-attributes map-combiner]]
    [query :refer [matching-elements matching-extensions]]
    query-impl)
   (cosheet.server
    [session-state :refer [queue-to-log]]
    [dom-tracker :refer [id->key key->attributes]]
    [model-utils :refer [selector? semantic-elements-R abandon-problem-changes]]
    [table-render :refer [batch-edit-selectors]]
    [referent :refer [referent->string referent?
                      virtual-referent? virtual-union-referent?
                      referent->exemplar-and-subject
                      item-referent virtual-referent]]
    [instantiate :refer [instantiate-or-create-referent
                         instantiate-referent
                         instantiate-referent-inheriting-restrictions
                         create-possible-selector-elements]]
    [order-utils :refer [furthest-item]])))

;;; TODO: Validate the data coming in, so mistakes won't cause us to
;;; crash.

;;; TODO: Replace the asserts with log messages, so things are robust.

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

(defn update-set-content-if-matching
  "Set the content of the item in the store provided the current content
   matches 'from'."
  [from to store item]
  ;; There are several special cases to match: If we have a number,
  ;; the client will have a string. If the client had ..., it was a
  ;; a wild card, and we could have anything.
  (if (let [from (parse-string-as-number from)
            content (content item)]
        (println "from" from "content" content)
        (and
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
                   (= to "")))
         (not= content 'anything-immutable)))
    (update-content store (:item-id item) (parse-string-as-number to))
    (do (println "content doesn't match" from (content item))
        store)))

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

(defn update-set-content
  [store item from to]
  (let [to (if (and (= to "")
                    (selector? item))
             'anything
             to)
        modified (update-set-content-if-matching
                  from to store item)]
    (abandon-problem-changes store modified item)))

(defn do-set-content
  [store arguments]
  (let [{:keys [from to referent select-pattern target-key immutable]}
        arguments]
    (when (and from to (not immutable))
      (let [to (parse-string-as-number (clojure.string/trim to))]
        (let [[items new-ids store] (instantiate-or-create-referent
                                      referent store)]
          (println "updating " (count items) " items")
          (let [new-store (reduce
                           (fn [store item]
                             (update-set-content store item from to))
                           store items)]
            ;; If we have set a virtual item, tell the client to select it.
            (if (and (or (virtual-referent? referent)
                         (virtual-union-referent? referent))
                     select-pattern
                     (seq items))
              (add-select-request
               new-store (map #(description->entity % new-store) new-ids)
               select-pattern target-key)
              new-store)))))))

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

(defn do-selected [store arguments]
  (let [referent (:referent arguments)
        client-state (:client-state (:session-state arguments))]
    (cond
      (= (:special arguments) :tab)
      (do (state-map-reset! client-state :referent referent)
          {:store store
           :set-url (str (:url-path (:session-state arguments))
                         "?referent=" (referent->string referent))})
      (= (:special arguments) :new-tab)
      (do-set-content
       store (merge-with (partial map-combiner nil)
                         arguments (:target arguments) {:from "" :to ""})))))

(defn do-batch-edit
  [store {:keys [referent batch-edit-ids session-state]}]
  (when referent
    (let [target (first (instantiate-referent referent store))
          batch-edit-items (map #(description->entity % store) batch-edit-ids)
          client-state (:client-state session-state)
          top-item (first
                    (instantiate-referent
                     (state-map-get-current-value client-state :referent)
                     store))
          topic (first (label->elements top-item :tab-topic))
          row-condition (when topic
                          (first (label->elements topic :row-condition)))
          temporary-id (:temporary-id session-state)
          temporary-item (description->entity temporary-id store)
          current-batch-selectors (label->elements
                                   temporary-item :batch-selector)
          new-batch-selectors (when (and target row-condition)
                                (batch-edit-selectors
                                 target batch-edit-items row-condition))]
      (when (not (empty? new-batch-selectors))
        (state-map-reset! client-state :batch-editing true)
        (as-> store store
          (reduce (fn [s selector]
                    (remove-entity-by-id s (:item-id selector)))
                  store current-batch-selectors)
          (update-valid-undo-point
           (reduce (fn [s selector]
                     (first (add-entity s temporary-id selector)))
                   store new-batch-selectors)
           false))))))

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
          :open  The key of an item to appear in a new tab.
       :set-url  The new url for the client."
  [store-modifier handler & args]
  (store-modifier
   (fn [store]
     (let [result (apply handler (update-valid-undo-point store true) args)]
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
  "Return the handler for the command, and the key that holds
   the additional arguments for the command."
  [action]
  ({:add-element [do-add-element :target]
    :add-label [do-add-label :target]
    :add-twin [do-add-twin :target]
    :add-sibling [do-add-virtual :add-sibling]
    :add-row [do-add-row :row]
    :add-column [do-add-virtual :add-column]
    :delete [do-delete :target]
    :delete-row [do-delete :row]
    :delete-column [do-delete :delete-column]
    :set-content [do-set-content :target]
    :expand [do-expand :target]
    :selected [do-selected :target]
    :batch-edit [do-batch-edit :target]}
   action))

(defn do-contextual-action
  "Do an action that applies to a DOM cell, and whose interpretation depends
  on that cell. We will call a contextual action handler with arguments
  and with attributes. The arguments is the subset of the attributes focused
  on the particular command; its target. The attributes comprise
  all the attributes from the dom, any added by the client, plus
  :session-state and :target-key."
  [mutable-store session-state [action-type client-id & {:as client-args}]]
  (let [[handler arguments-key] (get-contextual-handler action-type) 
        tracker (:tracker session-state)
        target-key (when client-id (id->key tracker client-id))
        dom-attributes (key->attributes tracker target-key)
        ;; We take the arguments for the context key and override any
        ;; specific to the action type,
        action-specific-arguments
        (apply merge-with (partial map-combiner nil)
               (map dom-attributes [arguments-key action-type]))
        ;; We augment attributes of the dom with the action specific arguments
        ;; pulled from that dom, with anything provided
        ;; by the client, and with the target key and session
        ;; state.
        arguments (merge-with (partial map-combiner nil)
                              dom-attributes
                              action-specific-arguments
                              client-args
                              {:target-key target-key
                               :session-state session-state})]
    (if handler
      (if (or action-specific-arguments (= action-type :batch-edit))
        (do
          (println "command: " (map simplify-for-print
                                    (list* action-type target-key
                                           (map concat (seq client-args)))))
          (println "arguments: " (simplify-for-print
                                  (dissoc arguments :session-state)))
          (let [result (do-storage-update-action
                          (partial do-update-control-return! mutable-store)
                          handler arguments)]
            (dissoc result :store)))
        (println "No context for action:" action-type
                 dom-attributes))
      (do 
        (println "unhandled action type:" action-type)))))

(defn do-undo
  [mutable-store session-state]
  (undo! mutable-store)
  nil)

(defn do-redo
  [mutable-store session-state]
  (redo! mutable-store)
  nil)

(defn do-quit-batch-edit
  [mutable-store session-state]
  (let [client-state (:client-state session-state)]
    (state-map-reset! client-state :batch-editing false)))

(defn do-action
  [mutable-store session-state action]
  (let [[action-type & additional-args] action]
    (println "doing action " action-type)
    (if-let [handler (case action-type
                       :undo do-undo
                       :redo do-redo
                       :quit-batch-edit do-quit-batch-edit
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
  (try
    (reduce (fn [client-info action]
              (let [for-client
                    (do-action mutable-store session-state action)]
                (println "for client " (simplify-for-print for-client))
                (into client-info
                      (select-keys for-client [:select :open :set-url]))))
            {} action-sequence)
    (catch Exception e
      (queue-to-log [:error (str e)] (:url-path session-state))
      (println "Error" (str e))
      nil)))

(defn confirm-actions
  "Check that the actions have not already been done, update the
  last-action to reflect that these actions have
  been done, and return the sequence of actions to be done."
  [actions client-state]
  (state-map-swap-control-return!
   client-state :last-action
   (fn [last-action]
     (let [later-times (cond->> (sort (keys actions))
                  last-action (filter #(pos? (compare % last-action))))]
       [(if (empty? later-times) last-action (last later-times))
        (map actions later-times)]))))

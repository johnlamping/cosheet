(ns cosheet.server.actions
  (:require
   (cosheet
    [debug :refer [simplify-for-print]]
    [utils :refer [parse-string-as-number thread-map
                   swap-control-return! equivalent-atoms?]]
    [state-map :refer [state-map-get-current-value state-map-reset!
                       state-map-swap-control-return!]]
    [store :refer [update-content
                   fetch-and-clear-modified-ids
                   do-update-control-return! revise-update-control-return!
                   id->subject id-valid? undo! redo! current-store]]
    [store-utils :refer [remove-entity-by-id]]
    mutable-store-impl
    [entity :refer [StoredEntity description->entity to-list
                    content elements label->elements label->content subject]]
    [dom-utils :refer [dom-attributes map-combiner]]
    [query :refer [matching-elements template-matches]]
    query-impl)
   (cosheet.server
    [session-state :refer [queue-to-log]]
    [dom-tracker :refer [id->key key->attributes]]
    [referent :refer [instantiate-referent instantiate-or-create-referent
                      instantiate-to-items
                      referent->string referent? virtual-referent?
                      referent->exemplar-and-subject
                      item-referent first-group-referent
                      semantic-elements-R
                      condition-to-template adjust-adjacents
                      create-possible-selector-elements
                      create-elements-satisfying]]
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
    :subject, if present, indicates that the result of the pattern is the
      subject of the match, rather than the match, itself.
    template, if present, must contains a variable named :v, and will return
      the part of the item matching that variable. An empty; otherwise, the
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
                (let [matches (template-matches template item)
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
  ;; the client will have a string. If we have an anonymous symbol,
  ;; the client will have "???". If the client had ..., it was probably
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
         (not (and (string? from) (= (first from) \u00A0) (= to "")))))
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
  [store arguments attributes]
  (let [{:keys [target-key selector-category]} attributes
        {:keys [select-pattern]} arguments]
    (when-let [referent (:referent arguments)]
      (let [items (instantiate-referent referent store)
            selector (when selector-category :first-group)
            [added store] (create-possible-selector-elements
                           nil items items :after true selector
                           store)]
        (add-select-request
         store [(first (apply concat added))]
         (or select-pattern
             (conj (pop-content-from-key target-key) [:pattern]))
         target-key)))))

(defn do-add-label
  [store arguments attributes]
  (let [{:keys [target-key selector-category]} attributes
        {:keys [select-pattern]} arguments]
    (when-let [referent (:referent arguments)]
      (let [items (instantiate-referent referent store)
            sample-item (first (apply concat items))
            is-tag (when sample-item
                     (seq (matching-elements :tag sample-item)))]
        (when (not is-tag)
          (let [selector (when selector-category :first-group)
                [added store] (create-possible-selector-elements
                               '(anything :tag) items items
                               :after true selector store)]
            (add-select-request
             store [(first (apply concat added))]
             (or select-pattern
                 (conj (subvec target-key 0 (- (count target-key) 1))
                       :label [:pattern])) target-key)))))))

(defn do-add-twin
  [store arguments attributes]
  (let [{:keys [target-key selector-category]} attributes]
    (when-let [referent (:referent arguments)]
      (when-let [condition (:template arguments)]
        (let [items (instantiate-referent referent store)
              subjects (map #(map subject %) items)
              selector (when selector-category :first-group)
              [added store] (create-possible-selector-elements
                             condition subjects items :after true selector
                             store)
              item-key (pop-content-from-key target-key)]
          (add-select-request
           store [(first (apply concat added))]
           (conj (pop item-key) [:pattern]) target-key))))))

(defn do-add-virtual
  [store arguments attributes]
  (let [{:keys [referent select-pattern]} arguments
        [items new-ids store] (instantiate-or-create-referent referent store)]
    (add-select-request store (map #(description->entity % store) new-ids)
                        select-pattern (:target-key attributes))))

(defn update-delete
  "Given an item, remove it and all its elements from the store"
  [store item]
  (remove-entity-by-id store (:item-id item)))

(defn do-delete
  "Remove item(s)." 
  [store arguments attributes]
  (when-let [to-delete (:referent arguments)]
    (let [item-groups (instantiate-referent to-delete store)
          header (first (first item-groups))
          items (distinct ;; distinct should not be necessary, but is a
                          ;; safety measure to make sure we don't delete twice
                 (apply concat item-groups))]
      (println "total items:" (count items))
      (reduce update-delete store items))))

(defn do-set-content
  [store arguments attributes]
  (let [{:keys [from to immutable target-key]} attributes
        {:keys [referent select-pattern]} arguments]
    (when (and from to (not immutable))
      (let [to (parse-string-as-number (clojure.string/trim to))]
        (let [[groups new-ids store] (instantiate-or-create-referent
                                      referent store)
              items (apply concat groups)]
          (println "updating " (count items) " items")
          (let [new-store (reduce (partial update-set-content-if-matching
                                           from to)
                                  store items)]
            ;; If we have set a virtual item, tell the client to select it.
            (if (and (virtual-referent? referent) select-pattern (seq items))
              (add-select-request
               new-store (map #(description->entity % new-store) new-ids)
               select-pattern target-key)
              new-store)))))))

(defn do-expand
  [store arguments attributes]
  (let [{:keys [session-state selector-category]} attributes
        target-referent (:referent arguments)]
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
         :open (cond-> (str (:url-path session-state)
                            "?referent=" (referent->string referent))
                 selector-category
                 (str "&selector=" (referent->string selector-category)))}))))

(defn do-selected [session-state store arguments attributes]
  (cond
    (= (:special arguments) :tab)
    (let [referent (:referent arguments)]
      (state-map-reset! (:client-state session-state)
                        :referent referent)
      {:store store
       :set-url (str (:url-path session-state)
                     "?referent=" (referent->string referent))})
    (= (:special arguments) :new-tab)
    (do-set-content
     store (:target attributes) (into attributes {:from "" :to ""}))))

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
   which holds the arguments for the command."
  [action]
  ({:add-element [do-add-element :target]
    :add-label [do-add-label :target]
    :add-twin [do-add-twin :target]
    :add-sibling [do-add-virtual :add-sibling]
    :add-row [do-add-virtual :add-row]
    :add-column [do-add-virtual :add-column]
    :delete [do-delete :target]
    :delete-row [do-delete :delete-row]
    :delete-column [do-delete :delete-column]
    :set-content [do-set-content :target]
    :expand [do-expand :target]
    :selected [do-selected :selected]}
   action))

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

(defn alternate-arguments
  "Given the session state, action type and arguments,
  return a vector of the default arguments
  to use, and the alternate arguments, if any. Also return the text to show the
  user to give them the chance to ask for the alternate arguments"
  [session-state action-type arguments attributes]
  (or
   (when-let [alternate (:alternate arguments)]
     (when (not= action-type :expand)
       (println "alternate requested.")
       (when-let [selector-category (if (= alternate true)
                                      (:selector-category attributes)
                                      alternate)]
         (println "selector found" selector-category)
         (let [arguments (dissoc arguments :alternate)
               narrow-arguments (update-in arguments [:referent]
                                         #(first-group-referent %))
               wide-referent (:referent arguments)
               narrow-referent (:referent narrow-arguments)
               store (current-store (:store session-state))]
           (when (if (virtual-referent? wide-referent)
                   (not= wide-referent narrow-referent)
                   (not= (set (instantiate-to-items wide-referent store))
                         (set (instantiate-to-items narrow-referent store))))
             (let [result
                   (if (= (:selector-interpretation session-state) :broad)
                     [arguments narrow-arguments
                      (broad-alternate-text selector-category)]
                     [narrow-arguments arguments
                      (narrow-alternate-text selector-category)])]
               (println "computed alternate" (simplify-for-print result))
               result))))))
   [arguments nil nil]))

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
        attributes (-> dom-attributes
                       ;; TODO: Move client-args to arguments.
                       (into client-args)
                       (assoc :target-key target-key
                              :session-state session-state))]
    (if handler
      (if-let [;; We take the arguments for the context key and override any
               ;; specific to the action type.
               arguments (->> (map attributes [arguments-key action-type])
                            (remove nil?)
                            (apply merge-with (partial map-combiner nil)))]
        (do
          (println "command: " (map simplify-for-print
                                    (list* action-type target-key
                                           (map concat (seq client-args)))))
          (println "dom attributes: " (simplify-for-print dom-attributes))
          (let [[arguments alternate-arguments text] (alternate-arguments
                                                      session-state action-type
                                                      arguments attributes)
                ;; do-selected gets the session-state in addition.
                handler (if (= action-type :selected)
                          (partial handler session-state)
                          handler)
                result (do-storage-update-action
                          (partial do-update-control-return! mutable-store)
                          handler arguments attributes)]
            (when (or (not (:special arguments)) (= action-type :set-content))
                  (state-map-reset!
                   (:client-state session-state) :alternate
                   (when alternate-arguments
                     (println "setting non-trivial alternate arguments." text)
                     {:new-store (first (fetch-and-clear-modified-ids
                                         (:store result)))
                      :action [handler
                               alternate-arguments
                               (dissoc attributes :session-state)]
                      :text text})))
            (dissoc result :store)))
        (println "No context for action:" action-type
                 (dissoc attributes :session-state)))
      (do 
        (println "unhandled action type:" action-type)))))

(defn do-alternate-contextual-action
  "Do the alternate interpretation of the recorded contextual action."
  [mutable-store session-state]
  (when-let [alternate (state-map-get-current-value
                        (:client-state session-state) :alternate)]
    (println "doing alternate.")
    (let [{:keys [new-store action]} alternate
          [handler arguments attributes] action
          result (apply do-storage-update-action
                  (partial revise-update-control-return!
                           mutable-store new-store)
                  [handler arguments (assoc attributes
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
            (state-map-reset! (:client-state session-state) :alternate nil)
            for-client))
      (if (= (mod (count action) 2) 0)
        (do-contextual-action mutable-store session-state action)
        (println "Error: odd number of keyword/argument pairs:" action)))))

(defn do-actions
  "Run the actions, and return any additional information to be returned
  to the client"
  [mutable-store session-state action-sequence]
  (let [for-client
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
            nil))]
    (if-let [alternate-text (:text (state-map-get-current-value
                                    (:client-state session-state) :alternate))]
      (assoc for-client :alternate-text alternate-text)
      for-client)
    ;; Remove this to re-enable alternate text.
    for-client))

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

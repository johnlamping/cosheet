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
    [query :refer [matching-items matching-elements template-matches]]
    query-impl)
   (cosheet.server
    [session-state :refer [queue-to-log]]
    [dom-tracker :refer [id->key key->attributes]]
    [referent :refer [instantiate-referent instantiate-or-create-referent
                      instantiate-to-items
                      referent->string referent? virtual-referent?
                      referent->exemplar-and-subject
                      item-referent first-group-referent
                      semantic-elements-R specialize-template
                      condition-to-template adjust-adjacents
                      create-possible-selector-elements
                      create-elements-satisfying]]
    [order-utils :refer [update-add-entity-adjacent-to furthest-item
                         order-element-for-item]]
    [tabs-render :refer [new-tab-elements]])))

;;; TODO: Validate the data coming in, so mistakes won't cause us to
;;; crash.

;;; TODO: Replace the asserts with log messages, so things are robust.

(defn substitute-in-key
  "Substitute into the key, instantiating patterns with the item.  A
  pattern is of the form [:pattern <template>], where template is
  either empty, or somewhere contains a variable named :v. An empty
  pattern will be replaced by the item, while a non-empty pattern gets
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
        (and
         (or (equivalent-atoms? from content)
             ;; Wildcard text matches anything,
             ;; because it has to match instances too
             (= from "\u00A0...")
             ;; Setting a new selector.
             (and (= from "") (= content 'anything)))
         ;; Don't believe blank for anything where the UI sets the initial
         ;; text to blank.
         (not (and (= (first from) \u00A0) (= to "")))))
    (update-content store (:item-id item) (parse-string-as-number to))
    (do (println "content doesn't match" from (content item))
        store)))

(defn add-select-request
  "Add a selection request to a response, given a group of items, the first
  of which should be selected."
  [response items select-pattern old-key]
  (let [response (if (satisfies? cosheet.store/Store response)
                   {:store response}
                   response)
        store (:store response)
        first-item (first (apply concat items))]
    (assoc response :select
           (when (not (nil? first-item))
             [(substitute-in-key
               select-pattern (description->entity (:item-id first-item) store))
              [old-key]]))))

(defn add-and-select-virtual-elements
  "Create actual elements for the virtual element, and select the first."
  [store virtual-referent select-pattern old-key]
  (let [[items [store _]] (instantiate-or-create-referent
                           virtual-referent [store {}])]
    (add-select-request store items select-pattern old-key)))

(defn do-add-element
  [store context attributes]
  (let [{:keys [target-key select-pattern selector-category]} attributes]
    (when-let [referent (:referent context)]
      (let [items (instantiate-referent referent store)
            selector (when selector-category :first-group)
            [added [store _]] (create-possible-selector-elements
                               nil items items :after true selector
                               [store {}])]
        (add-select-request
         store added (conj target-key [:pattern]) target-key)))))

(defn do-add-label
  [store context attributes]
  (let [{:keys [target-key select-pattern selector-category]} attributes]
    (when-let [referent (:referent context)]
      (let [items (instantiate-referent referent store)
            sample-item (first (apply concat items))
            is-tag (when sample-item
                     (seq (matching-elements :tag sample-item)))]
        (when (not is-tag)
          (let [selector (when selector-category :first-group)
                [added [store _]] (create-possible-selector-elements
                                   '(anything :tag) items items
                                   :after true selector [store {}])]
            (add-select-request
             store added (conj (subvec target-key 0 (- (count target-key) 1))
                               :label [:pattern]) target-key)))))))

(defn do-add-twin
  [store context attributes]
  (let [{:keys [target-key selector-category]} attributes]
    (when-let [referent (:referent context)]
      (when-let [condition (:template context)]
        (let [items (instantiate-referent referent store)
              subjects (map #(map subject %) items)
              selector (when selector-category :first-group)
              [added [store _]] (create-possible-selector-elements
                                 condition subjects items :after true selector
                                 [store {}])
              item-key (if (= (last target-key) :content)
                         (pop target-key)
                         target-key)]
          (add-select-request
           store added (conj (pop item-key) [:pattern]) target-key))))))

(defn do-add-virtual
  [store context attributes]
  (let [{:keys [referent key-prefix select-pattern]} context
        select-pattern (or select-pattern (conj key-prefix [:pattern]))]
    (add-and-select-virtual-elements
     store referent select-pattern (:target-key attributes))))

(defn update-delete
  "Given an item, remove it and all its elements from the store"
  [store item]
  (remove-entity-by-id store (:item-id item)))

(defn do-delete
  "Remove item(s)." 
  [store context attributes]
  (when-let [to-delete (:referent context)]
    (let [item-groups (instantiate-referent to-delete store)
          header (first (first item-groups))
          items (distinct ;; distinct should not be necessary, but is a
                          ;; safety measure to make sure we don't delete twice
                 (apply concat item-groups))]
      (println "total items:" (count items))
      (reduce update-delete store items))))

(defn do-set-content
  [store context attributes]
  (let [{:keys [from to immutable]} attributes
        referent (:referent context)]
    (when (and from to (not immutable))
      (let [to (parse-string-as-number (clojure.string/trim to))]
        (let [[groups [store _]] (instantiate-or-create-referent
                                  referent [store {}])
              items (apply concat groups)]
          (println "updating " (count items) " items")
          (reduce (partial update-set-content-if-matching from to)
                  store items))))))

(defn do-expand
  [store context attributes]
  (let [{:keys [session-state selector-category]} attributes
        target-referent (:referent context)]
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

(defn do-selected
 [session-state context]
 (when (= (:special context) :tab)
   (state-map-reset! (:client-state session-state)
                     :referent (:referent context))))

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
    :add-label [do-add-label :target]
    :add-twin [do-add-twin :target]
    :add-sibling [do-add-virtual :add-sibling]
    :add-row [do-add-virtual :add-row]
    :add-column [do-add-virtual :add-column]
    :delete [do-delete :target]
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
               narrow-context (update-in context [:referent]
                                         #(first-group-referent %))
               wide-referent (:referent context)
               narrow-referent (:referent narrow-context)
               store (current-store (:store session-state))]
           (when (if (virtual-referent? wide-referent)
                   (not= wide-referent narrow-referent)
                   (not= (set (instantiate-to-items wide-referent store))
                         (set (instantiate-to-items narrow-referent store))))
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
  on that cell. We will call a contextual action handler with a context
  and with attributes. The context is the subset of the attributes focused
  on the particular command; its target. The attributes comprise
  all the attributes from the dom, any added by the client, plus
  :session-state and :target-key."
  [mutable-store session-state [action-type client-id & {:as client-args}]]
  (let [[handler context-key] (get-contextual-handler action-type) 
        tracker (:tracker session-state)
        target-key (when client-id (id->key tracker client-id))
        dom-attributes (key->attributes tracker target-key)
        attributes (-> dom-attributes
                       (into client-args)
                       (assoc :target-key target-key
                              :session-state session-state))]
    (if handler
      (if-let [;; We take all attributes for the context and override any
               ;; specific to the action type.
               context (->> (map attributes [context-key action-type])
                            (remove nil?)
                            (apply merge-with (partial map-combiner nil)))]
        (do
          (println "command: " (map simplify-for-print
                                    (list* action-type target-key
                                           (map concat (seq client-args)))))
          (println "dom attributes: " (simplify-for-print dom-attributes))
          (if (= action-type :selected)
            (handler session-state context)
            (let [[context alternate-context text] (alternate-contexts
                                                    session-state action-type
                                                    context attributes)
                  result (do-storage-update-action
                          (partial do-update-control-return! mutable-store)
                          handler context attributes)]
              (when (or (not (:special context)) (= action-type :set-content))
                (state-map-reset!
                 (:client-state session-state) :alternate
                 (when alternate-context
                   (println "setting non-trivial alternate context." text)
                   {:new-store (first (fetch-and-clear-modified-ids
                                       (:store result)))
                    :action [handler
                             alternate-context
                             (dissoc attributes :session-state)]
                    :text text}))
                (dissoc result :store)))))
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
                            (select-keys for-client [:select :open]))))
                  {} action-sequence)
          (catch Exception e
            (queue-to-log [:error (str e)] (:name session-state))
            (println "Error" (str e))
            nil))]
    (if-let [alternate-text (:text (state-map-get-current-value
                                    (:client-state session-state) :alternate))]
      (assoc for-client :alternate-text alternate-text)
      for-client)))

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

;;; Used by views.clj.
;;; TODO: Put someplace better?
(defn update-add-blank-table-view
  "Add a blank table view with the given name to the store, returning the
  new store and the id of the new view."
  [store view-name]
  (let [generic (cons "" (cons view-name new-tab-elements))
        [specialized [store _]] (specialize-template generic [store {}])
        tabs-holder (first (matching-items '(nil :tabs) store))]
    (update-add-entity-adjacent-to
     store (:item-id tabs-holder) specialized (order-element-for-item nil store)
     :after false)))

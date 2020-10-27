(ns cosheet2.server.action-data
  (:require (cosheet2 [utils :refer [multiset replace-in-seqs
                                    map-map thread-map thread-recursive-map
                                    add-elements-to-entity-list]]
                      [debug :refer [simplify-for-print]]
                      [store :refer [StoredItemDescription
                                     id->subject id->content]]
                      [entity :refer [subject elements content label->elements
                              description->entity in-different-store]]
                      [canonical :refer [canonicalize-list
                                         update-canonical-content]]
                      [store-utils :refer [add-entity]]
                      [query :refer [matching-elements matching-items
                                     extended-by?]
                      :as query])
            (cosheet2.server
             [model-utils :refer [semantic-elements semantic-to-list
                                  entity->canonical-semantic
                                  pattern-to-query
                                  specialize-generic
                                  flatten-nested-content
                                  create-possible-selector-elements]])))

;;; The action data is a map that may contain any of these fields:
;;;      :target-ids  A seq of the ids that should be acted upon
;;;      :column-ids  A seq of the ids that should be acted up for a column
;;;                   action
;;;         :row-ids  A seq of the ids that should be acted up for a row
;;;                   action
;;;           :store  A store that should replace the current immutable store
;;;                   This is filled in by virtual DOM components, after
;;;                   adding the elements they imply.

(defn run-action-data-getter
  "Handle pulling the store out of the inherited action data, and handle
  a getter that has extra arguments."
  [getter specification containing-action-data action immutable-store]
  (println "Getting action data" getter)
  (println "  from spec" (dissoc specification :reporter :id-R))
  (println "  containing AD" (dissoc containing-action-data :component))
  (let [store (or (:store containing-action-data) immutable-store)
        [fun extra-args] (if (vector? getter)
                           [(first getter) (rest getter)]
                           [getter nil])]
    (apply fun specification containing-action-data action store extra-args)))

(def get-item-or-exemplar-action-data)

(defn update-action-data-for-component
  "Update the action data for one component"
  [component containing-action-data action immutable-store]
  (let [spec (:dom-specification @component)
        {:keys [get-action-data
                get-column-action-data get-row-action-data]} spec 
        data (reduce (fn [data getter]
                       (if getter
                         (run-action-data-getter
                          getter spec data action immutable-store)
                         data))
                     containing-action-data
                     [(or get-action-data get-item-or-exemplar-action-data)
                      get-column-action-data
                      get-row-action-data])]
    (assoc data :component component)))

(defn best-match
  "Given an immutable template, and a seq of items
   that match it, return the best matching item."
  [template matches]
  (if (<= (count matches) 1)
    ;; No choice.
    (first matches)
    ;; Prefer a match with no extra semantic info.
    (let [semantic (semantic-to-list template)
          ;; A nil in the template probably came from a wildcard.
          ;; It should be considered a perfect match with 'anything,
          ;; to handle selectors, and with the empty string, to handle
          ;; blank elements added to match a selector.
          canonical1 (canonicalize-list (replace-in-seqs semantic nil ""))
          canonical2 (canonicalize-list (replace-in-seqs
                                         semantic nil 'anything))
          perfect-matches (filter #(let [canonical-match
                                         (entity->canonical-semantic %)]
                                     (or (= canonical-match canonical1)
                                         (= canonical-match canonical2)))
                                  matches)
          ;; A good match is something identical to the template, except
          ;; that the template has nil content while the match has non-nil
          ;; content.
          good-matches (when (and (empty? perfect-matches)
                                  (nil? (content semantic)))
                         (let [canonical (canonicalize-list semantic)]
                           (filter #(let [canonical-match
                                         (entity->canonical-semantic %)]
                                      (= (update-canonical-content
                                          canonical-match nil)
                                         canonical))
                                   matches)))]
      ;; TODO: Add a function that computes the "complexity" of an item,
      ;;       which is the total number of elements, sub-elements, etc,
      ;;       with sub-elements counting less. Then choose the lowest
      ;;       complexity match.
      (first (or (seq perfect-matches) (seq good-matches) matches)))))

(defn best-matching-id
  "Return the id, if any, of the element of the subject whose item
  best matches the exemplar id's item."
  [exemplar-id subject-id immutable-store]
  (if (= (id->subject immutable-store exemplar-id) subject-id)
    ;; The exemplar id is an element of the given subject. Return it.
    exemplar-id
    (let [template (-> (description->entity exemplar-id immutable-store)
                       semantic-to-list
                       pattern-to-query)
          subject (description->entity subject-id immutable-store)]
      (:item-id (best-match template (matching-elements template subject))))))

(defn get-item-or-exemplars-for-id
  "Given the subject(s), find items or exemplars for the id."
  [subject-ids immutable-store id]
  (assert (satisfies? StoredItemDescription id) id)
  (assert (or (empty? subject-ids)
              (let [subject-id (id->subject immutable-store id)]
                (some #{subject-id} subject-ids)))
          [id
           (id->subject immutable-store id)
           (id->content immutable-store id)
           subject-ids
           (map #(id->content immutable-store %) subject-ids)])
  (if (<= (count subject-ids) 1)
    [id]
    (->> subject-ids
         (map #(best-matching-id id % immutable-store))
         (filter identity))))

(defn get-item-or-exemplar-action-data
  "This is the default for :get-action-data."
  [specification containing-action-data action immutable-store]
  (let [id (or (:item-id specification) (:relative-id specification))
        subject-ids (:target-ids containing-action-data)]
    (assoc containing-action-data :target-ids
           (get-item-or-exemplars-for-id subject-ids immutable-store id))))

(defmethod print-method
  cosheet2.server.action_data$get_item_or_exemplar_action_data
  [v ^java.io.Writer w]
  (.write w "item-AD"))

(defn get-item-or-exemplar-action-data-for-ids
  "Target all the given ids."
  [specification containing-action-data action immutable-store ids]
  (let [subject-ids (:target-ids containing-action-data)]
    (let [targets (mapcat #(get-item-or-exemplars-for-id
                            subject-ids immutable-store %)
                         ids)]
      (assoc containing-action-data :target-ids targets))))

(defmethod print-method
  cosheet2.server.action_data$get_item_or_exemplar_action_data_for_ids
  [v ^java.io.Writer w]
  (.write w "ids-AD"))

(defn get-content-only-action-data
  "This is a content-only node under a node for the data.
  Our targets are our container's targets."
  [specification containing-action-data action immutable-store]
  containing-action-data)

(defmethod print-method
  cosheet2.server.action_data$get_content_only_action_data
  [v ^java.io.Writer w]
  (.write w "content-AD"))

(defn get-column-action-data
  "Add the action data for a command that acts on a column.
  The header-id is the id that holds all the columns. The column-ids
  are the ids of all columns under this component. (One header can
  span multiple columns.)"
  [specification containing-action-data action immutable-store
   header-id column-ids]
  (assoc containing-action-data :column
         {:header-id header-id
          :column-ids column-ids}))

(defmethod print-method
  cosheet2.server.action_data$get_column_action_data
  [v ^java.io.Writer w]
  (.write w "col-AD"))

(defn get-virtual-action-data
  "Create the specified virtual items.
   The containing data's target-ids are the subject of the new items,
   unless sibling is true in which case they are the siblings.
   The new items are ordered after the containing target, unless position
   is :before, in which case they are ordered before.
   the new items use the smaller part of the order split, unless use-bigger
   is true, in which case they use the larger."
  [specification containing-action-data action immutable-store
   & {:keys [template sibling position use-bigger]}]
  (assert template template)
  (let [incoming-ids (:target-ids containing-action-data)
        subjects (if sibling
                  (map #(id->subject immutable-store %) incoming-ids)
                  incoming-ids)
        adjacents (if sibling incoming-ids subjects)
        [ids store] (create-possible-selector-elements
                     template subjects adjacents
                     (or position :after)
                     use-bigger
                     immutable-store)]
    (assoc containing-action-data :target-ids ids :store store)))

(defmethod print-method
  cosheet2.server.action_data$get_virtual_action_data
  [v ^java.io.Writer w]
  (.write w "virt-AD"))

(defn composed-get-action-data
  [specification containing-action-data action immutable-store & getters]
  (reduce (fn [action-data getter]
            (run-action-data-getter
             getter specification action-data action immutable-store))
          containing-action-data getters))

(defmethod print-method
  cosheet2.server.action_data$composed_get_action_data
  [v ^java.io.Writer w]
  (.write w "comp-AD"))

(defn compose-action-data-getter
  "Add an action data getter to any current one."
  [current to-add]
  (cond
    (nil? current)
    to-add
    (and (sequential? current)
         (= (first current) composed-get-action-data))
    (conj current to-add)
    true
    [composed-get-action-data current to-add]))


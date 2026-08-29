(ns cosheet.server.action-data
  (:require (cosheet [utils :refer [replace-in-seqs
                                     assoc-if-non-empty
                                     call-pseudo-closure]]
                      [debug :refer [simplify-for-print]]
                      [store :refer [item-id? id->target]]
                      [entity :refer [elements content label->elements
                                      id->entity entity-complexity object?]]
                      [canonical :refer [canonicalize
                                         update-canonical-content]]
                      [orderable :refer [initial orderable-compare]]
                      [query :refer [matching-elements]])
            (cosheet.server
             [render-utils :refer [sequential-template?]]
             [model-utils :refer [semantic-to-tree
                                  entity->canonical-semantic
                                  pattern-to-fixed-term
                                  create-possible-selector-entities]])))

;;; This file contains the basic action data getters, plus utilities for them.
;;; Files with unique needs for action data getters define their own.

;;; The action data is a map that is constructed when the user does an
;;; action, and gives the information relevant to the display element
;;; that is being acted on that is necessary to carry out the action.

;;; It may contain any of these fields:
;;;           :store  Always present, this gives the immutable store
;;;                   that the action should act on to get the revised
;;;                   store.  This may not be the starting store
;;;                   state, as the action getter may make its own
;;;                   changes to that. In particular, when the user
;;;                   acts on a virtual UI element (one for which
;;;                   there is not yet any corresponding item in the
;;;                   store) the action getter creates the implied
;;;                   items.
;;;     :subject-ids  A seq of the ids that should be acted upon
;;; :past-subject-ids A stack of previous subject-ids, as long as they
;;;                   were used in the calculation of the current
;;;                   subject ids, and their lengths match the length
;;;                   of the current :subject-ids.

;;; These three give information about the cell's position in a table,
;;; if any.  They are copied over from the specification, even before
;;; the action data getter is run. So action data getters don't have
;;; to worry about handling them.
;;;        :table-id  The id of the table.
;;;          :row-id  The id of the item shown in the cell's row.
;;;      :column-ids  The ids of the columns the cell is in. Usually,
;;;                   this is a list of just one item. But when there is
;;;                   a hierarchy of column headers, cells can span
;;;                   multiple columns, in which case this lists them
;;;                   from left to right.

;;; This gives the tab id if the component is part of the display of a
;;; single tab (not whatever the tab brings up, just the tab
;;; description, itself)
;;;          :tab-id  The tab that this component belongs to.
;;;                   For a virtual tab, the value is :virtual.

;;; If the action is do-batch-edit, the previous items are not present.
;;; The first two following ones will be present, and the later ones may be.
;;;          :query-ids  ids whose list forms make up the query
;;;          :stack-ids  ids whose list forms makes up the stack section of
;;;                      changeable things.
;;;    :must-show-label  If true, the stack ids must show a place for a label,
;;;                      if they don't have one.
;;;     :selected-index  If present, the index into stack-ids that includes
;;;                      what should be selected.
;;; :selection-sequence  A sequence of successively contained elements of
;;;                      the selected id that terminates in the item
;;;                      that should be selected.

(defn run-action-data-getter
  "Handle pulling the store out of the inherited action data, and handle
  a getter that has extra arguments."
  [getter specification inherited-action-data action immutable-store]
  (println)
  (println "getting ACTION DATA" getter)
  (println "  from spec" (dissoc specification
                                 :reporter :id-R :client-state
                                 :hierarchy
                                 :column-descriptions :row-template
                                 :row-ids))
  (let [store (or (:store inherited-action-data) immutable-store)
        result (call-pseudo-closure
                getter specification inherited-action-data action store)]
    (println "  returning AD" (dissoc result :component))
    result))

(defn get-empty-action-data
  "Return a blank action data."
  [specification inherited-action-data action immutable-store]
  {})

(defmethod print-method
  cosheet.server.action_data$get_empty_action_data
  [v ^java.io.Writer w]
  (.write w "empty-AD"))

(defn get-pass-through-action-data
  "Our DOM is a wrapper node for a position in a table, or a content-only DOM,
  under a DOM for the data. We don't affect anything."
  [specification inherited-action-data action immutable-store]
  inherited-action-data)

(defmethod print-method
  cosheet.server.action_data$get_pass_through_action_data
  [v ^java.io.Writer w]
  (.write w "pass-AD"))

(defn get-id-action-data
  "Return a single item id as the subject ids. This is only suitable
  for doms in a simple context where they can't possibly refer to
  several ids."
  [specification inherited-action-data action immutable-store id]
  (assoc inherited-action-data :subject-ids [id]))

(defmethod print-method
  cosheet.server.action_data$get_id_action_data
  [v ^java.io.Writer w]
  (.write w "id-AD"))

(defn best-match
  "Given an immutable template, and a seq of items
   that match it, return the best matching item."
  [template matches]
  (if (<= (count matches) 1)
    ;; No choice.
    (first matches)
    ;; Prefer a match with no extra semantic info.
    (let [semantic (semantic-to-tree template)
          ;; A nil in the template probably came from a wildcard.
          ;; It should be considered a perfect match with 'anything,
          ;; to handle selectors, and with the empty string, to handle
          ;; blank elements added to match a selector.
          canonical1 (canonicalize (replace-in-seqs semantic nil ""))
          canonical2 (canonicalize (replace-in-seqs
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
                         (let [canonical (canonicalize semantic)]
                           (filter #(let [canonical-match
                                         (entity->canonical-semantic %)]
                                      (= (update-canonical-content
                                          canonical-match nil)
                                         canonical))
                                   matches)))]
      ;; In case of ties, go with the lowest complexity match.
      (first (or (seq perfect-matches)
                 (seq (sort-by entity-complexity good-matches))
                 (seq (sort-by entity-complexity matches)))))))

(defn best-matching-element-id
  "Find the element of the subject that best matches the exemplar id's item.
   Return that element's id, if there is any match."
  [exemplar-id subject-id immutable-store]
  (if (= (id->target immutable-store exemplar-id) subject-id)
    ;; The exemplar id is an element of the given subject. Return it.
    exemplar-id
    (let [template (-> (id->entity exemplar-id immutable-store)
                       semantic-to-tree
                       pattern-to-fixed-term)
          subject (id->entity subject-id immutable-store)]
      (:item-id (best-match template (matching-elements template subject))))))

(defn get-item-or-exemplars-for-id
  "Given the subject(s), find items or exemplars for the id."
  [subject-ids immutable-store id]
  (assert (item-id? id) id)
  ;; If there are any subjects, and exemplar id has a target, it must
  ;; be an element of one of the subjects. Otherwise, we return a nil
  ;; to say we failed.
  (when (or (empty? subject-ids)
              (let [target-id (id->target immutable-store id)]
                (or (nil? target-id)
                    (some #{target-id} subject-ids))))
    (if (<= (count subject-ids) 1)
      [id]
      (->> subject-ids
           (map #(best-matching-element-id id % immutable-store))
           (remove nil?)))))

(defn get-item-or-exemplar-action-data
  "This is the vanilla action getter, for doms that might be in a
  context that makes them refer to several items."
  [{:keys [target-item-id relative-id]} ; specification
   inherited-action-data action immutable-store]
  (let [id (or target-item-id relative-id)
        {:keys [:subject-ids :past-subject-ids]} inherited-action-data
        new-subject-ids (get-item-or-exemplars-for-id
                         subject-ids immutable-store id)]
    (when new-subject-ids
      (-> inherited-action-data
          (assoc :subject-ids new-subject-ids)
          (assoc-if-non-empty 
           :past-subject-ids (when (= (count subject-ids)
                                      (count new-subject-ids))
                               (cons subject-ids past-subject-ids)))))))

(defmethod print-method
  cosheet.server.action_data$get_item_or_exemplar_action_data
  [v ^java.io.Writer w]
  (.write w "item-AD"))

(defn parallel-items-get-action-data
  "For each of parallel-ids, run the data getter on the specification modified
  to have that item id. Update the subject-ids of inherited-action-data
  to be the union of the subject-ids of each of the results."
  [{:keys [parallel-ids] :as specification}
   inherited-action-data action immutable-store getter]
  (letfn [(get-action-data-for-id [id]
            (run-action-data-getter
             getter
             (-> specification
                 (assoc :target-item-id id
                        :relative-id :overridden)
                 (dissoc :parallel-ids))
             inherited-action-data action immutable-store))]
    (let [results (map get-action-data-for-id parallel-ids)]
      (when (every? some? results)
        (-> inherited-action-data
            (assoc :subject-ids (distinct (mapcat :subject-ids results)))
            (dissoc :past-subject-ids))))))

(defmethod print-method
  cosheet.server.action_data$parallel_items_get_action_data
  [v ^java.io.Writer w]
  (.write w "parallel-AD"))

(defn default-get-action-data
  "There must be a target-item-id or a relative-id.  If there are no
  parallel-ids, just do get-item-or-exemplar-action-data. If there are
  parallel-ids, first do parallel-items-get-action-data on them,
  followed by get-item-or-exemplar-action-data."
  [{:keys [parallel-ids] :as specification}
   inherited-action-data action immutable-store]
  (if (seq parallel-ids)
    (when-let [action-data (parallel-items-get-action-data
                            specification inherited-action-data action
                            immutable-store get-item-or-exemplar-action-data)]
      (get-item-or-exemplar-action-data
       specification action-data action immutable-store))
    (get-item-or-exemplar-action-data
     specification inherited-action-data action immutable-store)))

(defn action-data-getter
  [dom-specification]
  (assert (:get-action-data dom-specification))
  (:get-action-data dom-specification))

(defmethod print-method
  cosheet.server.action_data$default_get_action_data
  [v ^java.io.Writer w]
  (.write w "default-AD"))

;;; do-batch-edit action data generators are called only when the
;;; requested action is to start a batch-edit. (These are completely
;;; different from the action data generators called while a batch
;;; edit is open.) These generators' job is to record the information
;;; needed to create the batch edit's query and stack selector.

;;; The primary information they produce is a list of ids whose list
;;; forms contain everything the query and stack selectors will
;;; contain, and in the right order. The batch-edit action will take
;;; this information and create the query and stack items.

;;; By breaking down the work this way, the action-data code can work
;;; in terms of ids, and can also describe sub-regions to select,
;;; while the action code does the translation into lists that are
;;; used to create the items.

(defn batch-selected-id
  "Return the id, if any, that the do-batch-edit action data says
  should be selected. (Technically, the id that will give rise to a
  list whose content should be selected.)"
  [{:keys [stack-ids selected-index selection-sequence]}]
  (cond (seq selection-sequence)
        (last selection-sequence)
        selected-index
        (nth stack-ids selected-index)))

(defn get-item-do-batch-edit-action-data
  "Find the dom's id or extend the selection sequence with it."
  [{:keys [target-item-id relative-id]} ; specification
   inherited-action-data action immutable-store]
  (let [id (or target-item-id relative-id)]
    (if (and id (:stack-ids inherited-action-data))
      (let [target (id->target immutable-store id)
            selected-id (batch-selected-id inherited-action-data)]
        (if selected-id
          (cond (= id selected-id)
                ;; We are the content of an item, so no change needed.
                inherited-action-data
                (= target selected-id)
                (update inherited-action-data :selection-sequence
                        #(concat % [id]))
                true
                (assert false [id target selected-id]))
          (let [ids (:stack-ids inherited-action-data)
                index (.indexOf ids id)]
            (if (>= index 0)
              (assoc inherited-action-data :selected-index index)
              (let [index (.indexOf ids target)]
                (assert (>= index 0) [id target selected-id])
                (assoc inherited-action-data
                        :selected-index index
                        :selection-sequence [id]))))))
      inherited-action-data)))

(defmethod print-method
  cosheet.server.action_data$get_item_do_batch_edit_action_data
  [v ^java.io.Writer w]
  (.write w "item-do-batch-AD"))

(defn parallel-items-get-do-batch-edit-action-data
  "For the first of the parallel-ids, run the data getter on the
  specification modified to have that item id."
  ;; Note: This assumes that when an exemplar is chosen, it will be an
  ;; element of the first of the parallel-ids.
  [{:keys [parallel-ids] :as specification}
   inherited-action-data action immutable-store getter]
  (getter (-> specification
              (assoc :target-item-id (first parallel-ids)
                     :relative-id :overridden)
              (dissoc :parallel-ids))
          inherited-action-data action immutable-store))

(defmethod print-method
  cosheet.server.action_data$parallel_items_get_do_batch_edit_action_data
  [v ^java.io.Writer w]
  (.write w "parallel-do-batch-AD"))

(defn default-get-do-batch-edit-action-data
  "There must be a relative-id or an item-id. If there are no
  parallel-ids, just do get-item-do-batch-edit-action-data. If there
  are parallel-ids, first do
  parallel-items-get-do-batch-edit-action-data on them, followed by
  get-item-do-batch-edit-action-data."
  [{:keys [parallel-ids] :as specification}
   inherited-action-data action immutable-store]
  (get-item-do-batch-edit-action-data
   specification
   (if (seq parallel-ids)
     (parallel-items-get-do-batch-edit-action-data
      specification inherited-action-data action immutable-store
      get-item-do-batch-edit-action-data)
     inherited-action-data)
   action immutable-store))

(defmethod print-method
  cosheet.server.action_data$default_get_do_batch_edit_action_data
  [v ^java.io.Writer w]
  (.write w "default-do-batch-AD"))

(defn find-virtual-adjacents
  "Return the ids of items the virtual item should be created next
  to. We assume that that adjacent items were not explicitly
  specified."
  [targets {:keys [adjacent-query position]} immutable-store]
  (if (not adjacent-query)
    targets
    (map
     (fn [target]
       (let [target-entity (id->entity target immutable-store)
             adjacent-elements (matching-elements
                                adjacent-query target-entity)
             sorted (sort-by
                     (fn [element]
                       (let [order (first (label->elements element :order))]
                         (if order (content order) initial)))
                     orderable-compare
                     adjacent-elements)
             picked ((if (= position :before) first last)
                     sorted)]
         (if picked (:item-id picked) target)))
     targets)))

(defn get-virtual-action-data
  "Create the specified virtual item(s) and make them the target(s).
   The new items are instances of the template.
   The containing data's subject-ids are the target of the new items,
   unless sibling is true, in which case they are the siblings.
   If sibling is true, the new items will be adjacent to the
   siblings. If not, they will be adjacent to elements of the target
   that match adjacent-query, if there are any, otherwise, they will
   be adjacent to the target.
   The new items are ordered after what they are adjacent to, unless
   position is :before, in which case they are ordered before.
   the new items use the smaller part of the order split, unless use-bigger
   is true, in which case they use the larger."
  [{:keys [template sibling position use-bigger past-subject-ids]
           ; adjacent-query also used.
    :as specification}
   inherited-action-data action immutable-store]
  (assert template template)
  (let [subject-ids (:subject-ids inherited-action-data)
        targets (if sibling
                  (map #(id->target immutable-store %) subject-ids)
                  subject-ids)
        adjacents (if sibling
                    subject-ids
                    (find-virtual-adjacents
                     targets specification immutable-store))
        templates (if (sequential-template? template)
                    (:template-sequence template)
                    [template])
        [targets _ past-ids new-store]
        (reduce
         (fn [[targets adjacents past-ids store] template]
           (assert (or (not (object? template))
                       (every? nil? targets))
                   [targets template])
           (let [[ids store] (create-possible-selector-entities
                              template targets adjacents
                              (or position :after) use-bigger store)]
             [ids ids (cons targets past-ids) store]))
         [targets adjacents past-subject-ids immutable-store]
         templates)]
    (println "Made items"
             template
             (simplify-for-print targets)
             (simplify-for-print adjacents))
    (assoc inherited-action-data
           :subject-ids targets
           :past-subject-ids past-ids
           :store new-store)))

(defmethod print-method
  cosheet.server.action_data$get_virtual_action_data
  [v ^java.io.Writer w]
  (.write w "virt-AD"))

(defn composed-get-action-data
  "Run each argument getter in turn, feeding the output of each into
  the next."
  [specification inherited-action-data action immutable-store & getters]
  (reduce (fn [action-data getter]
            (run-action-data-getter
             getter specification action-data action immutable-store))
          inherited-action-data getters))

(defmethod print-method
  cosheet.server.action_data$composed_get_action_data
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

(defn update-action-data-for-component
  "Update the action data for one component atom, running all the
  different possible getters the component might have, and adding the
  component to the data."
  [component containing-action-data action immutable-store]
  (let [spec (:dom-specification @component)
        {:keys [get-do-batch-edit-action-data]} spec 
        ;; These keys are always copied from the spec to the action
        ;; data, no matter what getter is run. They indicate overall context,
        ;; like what table, row, and column a DOM is in.
        copied-keys [:table-id :row-id :column-ids :tab-id
                     :is-object-name]
        partial-action-data (into containing-action-data
                                  (select-keys spec copied-keys))
        action-data (let [getter (if (= action :batch-edit)
                                   (or get-do-batch-edit-action-data
                                       default-get-do-batch-edit-action-data)
                                   (action-data-getter spec))]
                      (run-action-data-getter
                       getter spec partial-action-data action immutable-store))]
    (assoc action-data :component component)))

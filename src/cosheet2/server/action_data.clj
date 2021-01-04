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
                                  create-possible-selector-elements
                                  table-header-template]]
             [order-utils :refer [ordered-entities]])))

;;; The action data is a map that may contain any of these fields:
;;;      :target-ids  A seq of the ids that should be acted upon
;;;          :column  {:target-ids
;;;                    :header-id}
;;;             :row  {:row-id
;;;                    :row-template}
;;;          :select  {:tab-id  ; The tab this component belongs to.
;;;                   }
;;;                   For a virtual tab, the value is :virtual.
;;;           :store  A store that should replace the current immutable store
;;;                   This is filled in by virtual DOM components, after
;;;                   adding the elements they imply.

;;; This file contains the basic action data getters, plus utilities for them.
;;; Files with unique needs for action data getters define their own.

(defn run-action-data-getter
  "Handle pulling the store out of the inherited action data, and handle
  a getter that has extra arguments."
  [getter specification containing-action-data action immutable-store]
  (println)
  (println "getting ACTION DATA" getter)
  (println "  from spec" (dissoc specification
                                 :reporter :id-R :client-state
                                 :get-rendering-data :hierarchy-R
                                 :column-descriptions-R :row-template-R
                                 :row-ids-R))
  (let [store (or (:store containing-action-data) immutable-store)
        [fun extra-args] (if (vector? getter)
                           [(first getter) (rest getter)]
                           [getter nil])
        result (apply fun specification containing-action-data
                      action store extra-args)]
    (println "  returning AD" (dissoc result :component))
    result))

(def default-get-action-data)

(defn update-action-data-for-component
  "Update the action data for one component"
  [component containing-action-data action immutable-store]
  (let [spec (:dom-specification @component)
        {:keys [get-action-data get-tab-action-data
                get-column-action-data get-row-action-data]} spec 
        data (reduce (fn [data getter]
                       (if getter
                         (run-action-data-getter
                          getter spec data action immutable-store)
                         data))
                     containing-action-data
                     [(or get-action-data default-get-action-data)
                      get-column-action-data
                      get-row-action-data
                      get-tab-action-data])]
    (assoc data :component component)))

(defn item-complexity
  "Return the complexity of the item, which is the total number of
   elements, sub-elements, etc, with sub-elements counting less."
  [item]
  (+ (get {nil 0.1   'anything 0.1   "" 0.2}
          (content item) 1.0)
     (* 0.5 (apply + (map item-complexity (elements item))))))

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
      ;; In case of ties, go with the lowest complexity match.
      (first (or (seq perfect-matches)
                 (seq (sort-by item-complexity good-matches))
                 (seq (sort-by item-complexity matches)))))))

(defn best-matching-element-id
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
         (map #(best-matching-element-id id % immutable-store))
         (remove nil?))))

(defn get-item-or-exemplar-action-data
  "This is the default for :get-action-data."
  [specification containing-action-data action immutable-store]
  (let [id (or (:item-id specification) (:relative-id specification))
        subject-ids (:target-ids containing-action-data)]
    (assoc containing-action-data :target-ids
           (get-item-or-exemplars-for-id subject-ids immutable-store id))))

(def default-get-action-data get-item-or-exemplar-action-data)

(defmethod print-method
  cosheet2.server.action_data$get_item_or_exemplar_action_data
  [v ^java.io.Writer w]
  (.write w "item-AD"))

(defn get-pass-through-action-data
  "This is a content-only node under a node for the data.
  Our targets are our container's targets."
  [specification containing-action-data action immutable-store]
  containing-action-data)

(defmethod print-method
  cosheet2.server.action_data$get_pass_through_action_data
  [v ^java.io.Writer w]
  (.write w "pass-AD"))

(defn get-virtual-action-data
  "Create the specified virtual items.
   The containing data's target-ids are the subject of the new items,
   unless sibling is true in which case they are the siblings.
   The new items are ordered after the containing target, unless position
   is :before, in which case they are ordered before.
   the new items use the smaller part of the order split, unless use-bigger
   is true, in which case they use the larger."
  [specification containing-action-data action immutable-store
   {:keys [template sibling position use-bigger]}]
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

(defn get-tab-action-data
  "Add the action data for a tab."
  [specification containing-action-data action immutable-store tab-id]
  (assoc containing-action-data :select {:tab-id tab-id}))

(defmethod print-method
  cosheet2.server.action_data$get_tab_action_data
  [v ^java.io.Writer w]
  (.write w "tab-AD"))

(defn composed-get-action-data
  "Run each argument getter in turn, feeding the output of each into
  the next."
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

(defn multiple-items-get-action-data
  "For each item id, run the data getter on the specification modified
  to have that item id. Update the target-id of containing-action-data
  to be the union of the target-id of each of the results."
  [specification containing-action-data action immutable-store ids getter]
  (assoc
   containing-action-data :target-ids
   (distinct
    (mapcat (fn [id] (:target-ids
                      (run-action-data-getter
                       getter
                       (assoc specification :item-id id)
                       containing-action-data action immutable-store)))
            ids))))

(defmethod print-method
  cosheet2.server.action_data$multiple_items_get_action_data
  [v ^java.io.Writer w]
  (.write w "mult-items-AD"))


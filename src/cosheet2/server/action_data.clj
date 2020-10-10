(ns cosheet2.server.action-data
  (:require (cosheet2 [utils :refer [multiset replace-in-seqs
                                    map-map thread-map thread-recursive-map
                                    add-elements-to-entity-list]]
                      [debug :refer [simplify-for-print]]
                      [store :refer [StoredItemDescription id->subject]]
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
                                  pattern-to-query]])))

;;; The action data is a map that may contain any of these fields:
;;;      :target-ids  A seq of the ids that should be acted upon
;;;           :store  A store that should replace the current immutable store
;;;                   This is filled in by virtual DOM components, after
;;;                   adding the elements they imply.

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

(defn get-item-or-exemplar-action-data
  "This is the default for :get-action-data."
  [specification containing-action-data action immutable-store]
  (let [id (or (:item-id specification) (:relative-id specification))]
    (assert (satisfies? StoredItemDescription id) id)
    (let [subject-ids (:target-ids containing-action-data)]
      (assert (or (empty? subject-ids)
                  (let [subject-id (id->subject immutable-store id)]
                    (some #{subject-id} subject-ids))))
      (assoc containing-action-data :target-ids
             (if (<= (count subject-ids) 1)
               [id]
               (->> subject-ids
                    (map #(best-matching-id id % immutable-store))
                    (filter identity)))))))


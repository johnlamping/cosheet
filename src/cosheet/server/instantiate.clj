(ns cosheet.server.instantiate
  (:require (cosheet [utils :refer [multiset replace-in-seqs
                                    map-map thread-map thread-recursive-map
                                    add-elements-to-entity-list]]
                     [debug :refer [simplify-for-print]]
                     [entity
                      :as Entity
                      :refer [subject elements content label->elements
                              description->entity in-different-store]]
                     [canonical :refer [canonicalize-list
                                        update-canonical-content]]
                     [store :as store :refer [id-valid? update-content]]
                     [store-utils :refer [add-entity]]
                     [query :refer [matching-elements matching-items
                                    extended-by?]])
            (cosheet.server
             [referent :refer [referent? item-referent? virtual-referent?
                               union-referent? virtual-union-referent?
                               element-restriction-referent? referent-type]]
             [order-utils :refer [update-add-entity-adjacent-to
                                  order-items-R furthest-item furthest-element]]
             [model-utils :refer [create-selector-or-non-selector-element
                                  immutable-semantic-to-list semantic-to-list-R
                                  item->canonical-semantic
                                  pattern-to-query query-to-template
                                  flatten-nested-content specialize-template
                                  semantic-elements-R]])))

(defn best-match
  "Given an immutable template, and a seq of items
   that match it, return the best matching item."
  [template matches]
  (if (> (count matches) 1)
    ;; Prefer one with no extra semantic info.
    (let [semantic (immutable-semantic-to-list template)
          ;; A nil in the template probably came from a wildcard.
          ;; It should be considered a perfect match with 'anything,
          ;; to handle selectors, and with the empty string, to handle
          ;; blank elements added to match a selector.
          canonical1 (canonicalize-list (replace-in-seqs semantic nil ""))
          canonical2 (canonicalize-list (replace-in-seqs semantic
                                                         nil 'anything))
          perfect-matches (filter #(let [canonical-match
                                         (item->canonical-semantic %)]
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
                                         (item->canonical-semantic %)]
                                      (= (update-canonical-content
                                          canonical-match nil)
                                         canonical))
                                  matches)))]
      (first (or (seq perfect-matches) (seq good-matches) matches)))
    (first matches)))

(defn best-matching-element
  "Given an immutable template, find an element of the item that
  matches it. Return a seq of the best matching element if there is one,
  otherwise return nil. Only works on immutable templates."
  [template subject]
  (let [matches (matching-elements template subject)]
    (when (not (empty? matches))
      [(best-match template matches)])))

(defn expand-pattern-items
  "If the pattern has any item ids, look them up in the store and replace
  them with the semantic list form of what is in the store."
  [pattern immutable-store]
  (flatten-nested-content
   (clojure.walk/postwalk
    (fn [referent]
      (if (item-referent? referent)
        (when (id-valid? immutable-store referent)
          (immutable-semantic-to-list
           (description->entity referent immutable-store)))
        referent))
    pattern)))

(defn merge-conditions
  "Given two conditions, return a merged condition that requires both of them
   to be met."
  [c1 c2]
  (cond
    (nil? c1) c2
    (nil? c2) c1
    true (let [merged-content (or (content c1) (content c2))]
           ;; Our current implementation requires c1 and c2 to have compatible
           ;; contents and disjoint elements.
           ;; Check that that is the case.
           (assert (and (extended-by? (content c1) merged-content)
                        (extended-by? (content c2) merged-content)
                        (every? #(empty? (matching-elements % c2))
                                (elements c1))
                        (every? #(empty? (matching-elements % c1))
                                (elements c2)))
                   [c1 c2 merged-content])
           (add-elements-to-entity-list merged-content
                                        (concat (elements c1) (elements c2))))))

(defn best-exemplar
  "Return the element of the subject best matching the condition and 
   the restriction."
  [condition restriction subject]
  (let [immutable-store (:store subject)]
    (or
     ;; If the condition is an item and satisfies the restriction, return it
     ;; for its subject, even if another element of its subject is as good
     ;; a match.
     (when (and (item-referent? condition)
                (id-valid? immutable-store condition))
       (let [exemplar-item (description->entity condition immutable-store)
             subj (Entity/subject exemplar-item)]
         (when (and (= subject subj)
                    (extended-by? restriction exemplar-item))
           [exemplar-item])))
     (best-matching-element
      (pattern-to-query (merge-conditions
                         (expand-pattern-items condition immutable-store)
                         restriction))
      subject))))

(declare instantiate-referent)

(defn instantiate-referent-with-restrictions
  "Return two lists: one with each item the referent refers tom and ont
   with any restriction on elements of the corresponding item."
  [referent immutable-store]
  (case (referent-type referent)
    :element-restriction (let [[_ condition ref] referent
                               items (instantiate-referent ref immutable-store)]
                           [items (repeat (count items) condition)])
    :union (let [[_ & referents] referent
                 [items conditions _]
                 (reduce (fn [[accum-items accum-conditions item-set] referent]
                           (let [[items conditions]
                                 (instantiate-referent-with-restrictions
                                  referent immutable-store)]
                             [(concat accum-items (remove item-set items))
                              (concat accum-conditions
                                      (mapcat (fn [item condition]
                                                (when (not (item-set item))
                                                  [condition]))
                                              items conditions))
                              (into item-set items)]))
                         [[] [] #{}]
                         referents)]
             [items conditions])
    (let [items (instantiate-referent referent immutable-store)]
      [items (repeat (count items) nil)])))

(defn instantiate-referent-inheriting-restrictions
  "For each item the referent refers to, return a pair of the item and any
   restriction for which that item was an element."
  [referent immutable-store]
  ;; TODO: Code this.
  nil)

(defn instantiate-referent
  "Return the items that the referent refers to. Does not handle
  virtual referents."
  [referent immutable-store]
  (case (referent-type referent)
    :item (when (id-valid? immutable-store referent)
            [(description->entity referent immutable-store)])
    :exemplar (let [[_ condition subject-ref] referent]
                (apply mapcat (fn [subject restriction]
                                (best-exemplar condition restriction subject))
                       (instantiate-referent-with-restrictions
                        subject-ref immutable-store)))
    :elements (let [[_ condition subject-ref] referent
                    condition (expand-pattern-items condition immutable-store)]
                (apply mapcat (fn [subject restriction]
                                (matching-elements
                                 (pattern-to-query
                                  (merge-conditions condition restriction))
                                 subject))
                       (instantiate-referent-with-restrictions
                        subject-ref immutable-store)))
    :non-competing-elements
    (let [[_ condition subject-ref & competing-conditions] referent
          competing-queries (map #(pattern-to-query
                                   (expand-pattern-items % immutable-store))
                                 competing-conditions)]
      (apply mapcat
             (fn [subject restriction]
               (let [items (matching-elements
                            (pattern-to-query
                             (merge-conditions
                              (expand-pattern-items condition immutable-store)
                              restriction))
                            subject)
                     filtered (reduce
                               (fn [items competing-query]
                                 (remove #(extended-by? competing-query %)
                                         items))
                               items competing-queries)]
                 (if (empty? filtered)
                   ;; If all matches have competitors, take the best exemplar.
                   (best-exemplar condition restriction subject)
                   filtered)))
             (instantiate-referent-with-restrictions
              subject-ref immutable-store)))
    :element-restriction
    (let [[_ condition ref] referent]
      (instantiate-referent ref immutable-store))
    :query (let [[_ condition] referent
                 query (pattern-to-query
                        (expand-pattern-items condition immutable-store))]
             (matching-items query immutable-store))
    :union (let [[_ & referents] referent]
             (distinct (mapcat #(instantiate-referent % immutable-store)
                               referents)))
    :difference (let [[_ plus minus] referent]
                  (remove
                   (set (instantiate-referent minus immutable-store))
                   (instantiate-referent plus immutable-store)))
    :virtual []))

(defn create-possible-selector-elements
  "Create elements, specializing the template as appropriate, depending on
   whether each subject is a selector. Return the new items and the updated
   store."
  [query restrictions subjects adjacents position use-bigger store]
  (let [template (query-to-template query)
        [specialized-template store] (specialize-template template store)
        flattened-template (flatten-nested-content specialized-template)
        restrictions (or restrictions (repeat (count subjects) nil))
        [new-ids store]
        (thread-map
         (fn [[restriction subject adjacent] store]
           (let [[store id] (create-selector-or-non-selector-element
                             (merge-conditions flattened-template restriction)
                             subject adjacent position use-bigger store)]
             [id store]))
         (map vector restrictions subjects adjacents)
         store)]
    [(map #(description->entity % store) new-ids)
     store]))

(declare instantiate-or-create-referent)
(declare instantiate-or-create-referent-with-restrictions)

(defn instantiate-or-create-template
  "Run instantiate-or-create-referent on all referents in the template."
  [template original-store store]
  (cond (referent? template)
        (let [[items new-ids store]
              (instantiate-or-create-referent template original-store store)]
          [(semantic-to-list-R (first items)) new-ids store])
        (seq? template)
        (let [[template [new-ids store]]
              (thread-recursive-map
               (fn [template [new-ids store]]
                 (if (referent? template)
                   (let [[items newest-ids store]
                         (instantiate-or-create-referent
                          template original-store store)]
                     [(semantic-to-list-R (first items))
                      [(concat new-ids newest-ids) store]])
                   [template [nil store]]))
               template [nil store])]
          [template new-ids store])
        true
        [template nil store]))

(defn find-adjacents
  "Given a list of adjacent referents from a virtual referent,
   and the subject items,
   Return the adjacent to use for each subject item. original-store gives the
   store to instantiate in, while store gives the store that the returned
   items should reference."
  [adjacent-referents subjects position original-store store]
  (if
    (empty? adjacent-referents)
    (map (fn [item] (furthest-element item position)) subjects)
    (let [adjacentses (map (fn [referent]
                             (map #(in-different-store % store)
                                  (instantiate-referent
                                   referent original-store)))
                        adjacent-referents)]
      (apply map (fn [& items] (furthest-item items position))
             adjacentses))))

(defn create-virtual-referent
  "Create items for virtual referents. Return the
  new items, a seq of ids of the first item created for this referent
  and each nested virtual referent, and the updated store."
  [referent original-store store]
  (assert (virtual-referent? referent))
  (let [[_ template subject-referent adjacent-referents position use-bigger]
        referent
        [template new-template-ids store]
        (instantiate-or-create-template template original-store store)
        [subjects restrictions new-subject-ids store]
        (if (nil? subject-referent)
          [nil nil nil store]
          (instantiate-or-create-referent-with-restrictions
           subject-referent original-store store))
        adjacents (find-adjacents adjacent-referents subjects
                                  position original-store store)
        subjects (if (nil? subject-referent)
                   (repeat (count adjacents) nil)
                   subjects)
        [items store] (create-possible-selector-elements
                        template restrictions subjects adjacents
                        position use-bigger store)]
    [items
     (concat [(:item-id (first items))]
             new-subject-ids new-template-ids)
     store]))

(defn create-virtual-union-referent
  "Does instantiate-or-create-referent on each referent of a union,
   and combines the results."
  [referent original-store store]
  (let [[items ids store]
        (reduce (fn [[accum-items first-ids store] referent]
                  (let [[items ids store] (instantiate-or-create-referent
                                            referent original-store store)]
                    [(concat accum-items items)
                     (or first-ids ids)
                     store]))
                [[] nil store]
                (rest referent))]
    ;; Some items may have the original store or a partially
    ;; updated store. Make them all have the latest store.
    [(map #(in-different-store % store) items) ids store]))

(defn instantiate-or-create-referent
  "Find the items that the referent refers to, creating items
  for virtual referents. Return the items, a seq of the ids of first item
  created for this referent and each nested virtual referent, 
  and the updated store.
  Evaluate non-virtual sub-parts in the original store, so that
  they will have their original interpretation, even if some embedded
  virtual modifies the store.
  To support recursion, the original store can be specified explicitly."
  ([referent store]
   (instantiate-or-create-referent referent store store))
  ([referent original-store store]
   (cond
     (virtual-referent? referent)
     (create-virtual-referent referent original-store store)
     (virtual-union-referent? referent)
     (create-virtual-union-referent referent original-store store)
     true
     [(instantiate-referent referent original-store) nil store])))

(defn instantiate-or-create-union-referent-with-restrictions
  "Does instantiate-or-create-referent-with-restrictions
   on each referent of a union, and combines the results."
  [referent original-store store]
  (let [[items restrictions ids store]
        (reduce (fn [[accum-items accum-restrictions first-ids store] referent]
                  (let [[items restrictions ids store]
                        (instantiate-or-create-referent-with-restrictions
                         referent original-store store)]
                    [(concat accum-items items)
                     (concat accum-restrictions restrictions)
                     (or first-ids ids)
                     store]))
                [nil nil nil store]
                (rest referent))]
    ;; Some items may have the original store or a partially
    ;; updated store. Make them all have the final store.
    [(map #(in-different-store % store) items)
     restrictions
     ids
     store]))

(defn instantiate-or-create-referent-with-restrictions
  "Find the items that the referent refers to, creating items
  for virtual referents. Return the items, the restrictions on each item
  a seq of the ids of first item created for this referent and each
  nested virtual referent, and the updated store."
  [referent original-store store]
  (case (referent-type referent)
    :element-restriction (let [[_ condition ref] referent
                               [items restrictions ids store]
                               (instantiate-or-create-referent-with-restrictions
                                ref original-store store)]
                           [items
                            (map #(merge-conditions condition %) restrictions)
                            ids store])
    :union (instantiate-or-create-union-referent-with-restrictions
            referent original-store store)
    (let [[items ids store] (instantiate-or-create-referent
                             referent original-store store)]
      [items (repeat (count items) nil) ids store])))

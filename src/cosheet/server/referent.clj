(ns cosheet.server.referent
  (:require (cosheet [utils :refer [multiset replace-in-seqs
                                    map-map thread-map thread-recursive-map]]
                     [debug :refer [simplify-for-print]]             
                     [expression :refer [expr expr-let expr-seq]]
                     [entity
                      :as Entity
                      :refer [elements content label->elements
                              description->entity
                              in-different-store StoredEntity]]
                     [canonical :refer [canonicalize-list]]
                     [store :as store :refer [id-valid? StoredItemDescription
                                              update-content]]
                     [store-utils :refer [add-entity]]
                     [store-impl :refer [->ItemId]]
                     [query :refer [matching-elements matching-items
                                    template-matches]])
            (cosheet.server
             [order-utils :refer [update-add-entity-adjacent-to
                                  order-items-R furthest-item furthest-element]]
             [model-utils :refer [create-selector-or-non-selector-element
                                  immutable-semantic-to-list semantic-to-list-R
                                  item->canonical-semantic
                                  pattern-to-query query-to-template
                                  pattern-to-possible-non-selector-template
                                  flatten-nested-content
                                  specialize-template semantic-elements-R]])))

;;; Commands are typically run with respect to a referent, which
;;; describes what the command should act on.

;;; The semantics of a referent is a sequence of items. The
;;; sequence supports DOM elements, like labels, that can refer to
;;; several parallel parts of the database. The most common situation
;;; is a singleton item.

;;; When a referent contains a <condition>, it can be either an item
;;; id, the list form of an element, or a list of an item id and
;;; the list form of additional elements. Item ids are turned into the
;;; condition corresponding to their list expansion.

;;; The kinds of referent are:
;;;            item  <an item-id>
;;;                  Refers to the single item.
;;;        exemplar  [:exemplar <item-id> <sequence-referent>]
;;;                  For each item refered to by sequence-referent,
;;;                  refers to an element, if there is one, whose semantic
;;;                  information includes that of the item. If there are
;;;                  several matching items, chooses the one with the least
;;;                  additional stuff.
;;;        elements  [:elements <condition> <sequence-referent>]
;;;                  For each item refered to by sequence-referent,
;;;                  refers to all elements whose semantic information
;;;                  matches the condition.
;;;           query  [:query <condition>]
;;;                  Refers to all items that satisfy the condition.
;;;           union  [:union  <referent> ...]
;;;                  Refers to all items that any of the referents refers to.
;;;                  Replications are removed
;;;      difference  [:difference <referent> <referent>]
;;;                  Refers to all items refered to by the first referent
;;;                  and not by the second.
;;;         virtual  [:virtual <exemplar-referent> <subject-referent>
;;;                            <adjacent-referents> <position> <use-bigger>]
;;;                  For each item refered to by subject-referent,
;;;                  refers to a new item matching the exemplar
;;;                  referent. The exemplar may be a condition, with nils,
;;;                  and may be a template, with 'anything or refer to an
;;;                  item that is a template. It can also be
;;;                  virtual or contain virtual referents. The subject
;;;                  referent can be virtual, or may be nil, in which case
;;;                  one element is created for each item in adjacent.
;;;                  Adjacent-referent is a list of referents, each of which
;;;                  gives items the new items could be adjacent to in the
;;;                  order. Each of the adjacent
;;;                  referents must yield the same number of items,
;;;                  which must be the same number as subject, if it is
;;;                  present. If there are multiple adjacent referents, the
;;;                  new item will be adjacent to the furthest in the direction
;;;                  of position. If there are no adjacent-referents,
;;;                  subject-referent
;;;                  must not be nil, and the element of the subject furthest
;;;                  in the position direction is used for adjacent, or
;;;                  subject, itself, if it has no ordered elements.
;;;                  Position and use-bigger say where the new
;;;                  item should go relative to the adjacent item(s).
;;;                  Virtual referents may not be nested inside non-virtual
;;;                  referents, except union referents.

(defn item-referent? [referent]
  (satisfies? StoredItemDescription referent))

(defn exemplar-referent? [referent]
  (and (sequential? referent) (= (first referent) :exemplar)))

(defn union-referent? [referent]
  (and (sequential? referent) (= (first referent) :union)))

(defn virtual-referent? [referent]
  (and (sequential? referent) (= (first referent) :virtual)))

(defn referent? [referent]
  (or (item-referent? referent)
      (and (sequential? referent)
           (#{:exemplar :elements :query :union :difference :virtual}
            (first referent)))))

(defn referent-type [referent]
  (assert (referent? referent) referent)
  (if (item-referent? referent) :item (first referent)))

;;; In the following constructors, items may be used wherever
;;; item ids are required.

(defn- coerce-item-to-id
  "Turn items into their ids."
  [item]
  (if (satisfies? StoredEntity item)
    (:item-id item)
    item))

(defn- require-item-or-id
  "Make sure the argument is an item or id, and coerce it to an id."
  [item]
  (let [id (coerce-item-to-id item)]
    (assert (satisfies? StoredItemDescription id)
            (simplify-for-print id))
    id))

(defn item-referent
  "Create an item referent from an item."
  [item]
  (require-item-or-id item))

(defn exemplar-referent
  "Create an exemplar referent."
  [exemplar subject]
  (assert (referent? subject))
  [:exemplar (require-item-or-id exemplar) subject])

(defn elements-referent
  "Create an elements referent."
  [condition subject]
  (assert (referent? subject))
  [:elements (coerce-item-to-id condition) subject])

(defn query-referent
  "Create an query referent."
  [condition]
  [:query (coerce-item-to-id condition)])

(defn union-referent
  "Create a union referent."
  [referents]
  (doseq [referent referents]
    (assert (referent? referent)))
  (vec (cons :union referents)))

(defn union-referent-if-needed
  "Create a referent for the union of the referents, unless there is only one."
  [referents]
  (if (= (count referents) 1)
    (first referents)
    (union-referent referents)))

(defn difference-referent
  "Create a difference referent."
  [plus minus]
  (assert (referent? plus))
  (assert (referent? minus))
  [:difference plus minus])

(defn virtual-referent
  "Create a virtual referent."
  ([exemplar subject]
   (virtual-referent exemplar subject nil))
  ([exemplar subject adjacents & {:keys [position use-bigger]
                                  :or {position :after
                                       use-bigger false}
                                  :as options}]
   (when subject (assert (referent? subject)))
   (when adjacents (assert (or (referent? adjacents)
                              (and (sequential? adjacents)
                                   (every? referent? adjacents)))))
   (assert (or subject adjacents))
   (assert (#{:before :after} position))
   (assert (contains? #{true false} use-bigger))
   (assert (every? #{:position :use-bigger} (keys options)))
   [:virtual (coerce-item-to-id exemplar) (coerce-item-to-id subject)
    (if (referent? adjacents)
      [(coerce-item-to-id adjacents)]
      (map coerce-item-to-id adjacents))
    position use-bigger]))

(defn item-or-exemplar-referent
  "Make an item or an exemplar referent, as necessary,
   depending on the subject."
  [item subject-referent]
  (if (or (nil? subject-referent)
          (and (item-referent? subject-referent)
               (satisfies? cosheet.entity/StoredEntity item)
               (if-let [subj (cosheet.entity/subject item)]
                 (= (item-referent subj) subject-referent))) )
    (item-referent item)
    (exemplar-referent item subject-referent)))

(defn referent->exemplar-and-subject
  "Given a referent, return an exemplar and subject such that
  item-or-exemplar-referent could reconstruct the referent.
  Returns nil for anything but item and exemplar referents."
  [referent]
  (cond (item-referent? referent) [referent nil]
        (exemplar-referent? referent) (rest referent)))

;;; The string format of a referent is
;;;     for item referents: I<the digits of their id>
;;;    for other referents: <type letter><referent>r
;;;           for keywords: K<delimiter><keyword letters><delimiter>
;;;            for symbols: S<delimiter><keyword letters><delimiter>
;;;                for nil: N
;;;               for true: T
;;;              for false: F
;;;              for lists: L<items in the list>l
;;; The last three cases are needed to describe conditions in, for example,
;;; query referents.
;;; The type letter for the various types of referent is:
(def letters->type
  {\X :exemplar
   \E :elements
   \Q :query
   \U :union
   \D :difference
   \V :virtual})
(def type->letters (clojure.set/map-invert letters->type))

(defn referent->string
  "Return a string representation of the referent that can appear in a url."
  [referent]
  (cond (referent? referent)
        (if (item-referent? referent)
          (str "I" (:id referent))
          (let [argument-strings (map referent->string (rest referent))]
            (when (not-any? nil? argument-strings)
              (str (type->letters (referent-type referent))
                   (apply str argument-strings)
                   \r))))
        (or (keyword? referent) (symbol? referent))
        ;; For a keywords and symbols, we find a letter not in the keyword,
        ;; and use that as the delimiter.
        (let [name (name referent)
              letters (set (seq name))
              candidates (concat [\_]
                                 (map char (range (int \A) (inc (int \Z))))
                                 (map char (range (int \a) (inc (int \z)))))
              delimiter (first (remove letters candidates))]
          (str (if (keyword? referent) "K" "S") delimiter name delimiter))
        (nil? referent)
        "N"
        (= referent true)
        "T"
        (= referent false)
        "F"
        ;; Things that look like lists can be all sort of things, so just
        ;; check for them not being vectors.
        (and (seq? referent) (not (vector? referent)))
        (let [member-strings (map referent->string referent)]
          (when (not-any? nil? member-strings)
            (str "L" (apply str member-strings) "l")))))

(defn- add-to-partial
  "Add an entity to the partial referent."
  [entity partial-referent]
  (if (vector? partial-referent)
    (conj partial-referent entity)
    (apply list (concat partial-referent [entity]))))

(defn string->referent
  "Parse a string representation of a referent.
  Return nil if the string is not a valid representation of a referent."
  [rep]
  (loop [index 0 ; Next index in the string to look at
         partial-referent [:start] ; The referent we are constructing
         pending-partials nil ; Partial referents the current one
                              ; is embedded in.
         ]
    (if (= index (count rep))
      (when (and (= (count partial-referent) 2)
                 (empty? pending-partials))
        (second partial-referent))
      (let [first-letter (nth rep index)]
        (cond (= first-letter \r)
              (when (and (not (empty? pending-partials))
                         (vector? partial-referent)
                         (let [num-args (dec (count partial-referent))]
                           (case (first partial-referent)
                             nil nil
                             (:exemplar :elements :difference) (= num-args 2)
                             :query (= num-args 1)
                             (>= num-args 1))))
                (recur (inc index)
                       (conj (first pending-partials) partial-referent)
                       (rest pending-partials)))
              (letters->type first-letter)
              (recur (inc index)
                     [(letters->type first-letter)]
                     (cons partial-referent pending-partials))
              (= first-letter \I)
              (let [digits (re-find #"\d+" (subs rep (inc index)))
                    number (try (Integer/parseInt digits)
                                (catch Exception e nil))]
                (when number
                  (let [id (->ItemId number)]
                    (recur (+ index (inc (count digits)))
                           (add-to-partial id partial-referent)
                           pending-partials))))
              (#{\K \S} first-letter)
              (let [delimiter (nth rep (inc index))
                    end-index (clojure.string/index-of
                               rep delimiter (+ index 2))]
                (when end-index
                  (let [name (subs rep (+ index 2) end-index)
                        result (({\K keyword \S symbol} first-letter) name)]
                    (recur (inc end-index)
                           (add-to-partial result partial-referent)
                           pending-partials))))
              (#{\N \T \F} first-letter)
              (let [value ({\N nil \T true \F false} first-letter)]
                (when (sequential? partial-referent))
                (recur (inc index)
                       (add-to-partial value partial-referent)
                       pending-partials))
              (= first-letter \L)
              (recur (inc index)
                     '()
                     (cons partial-referent pending-partials))
              (= first-letter \l)
              (when (and (not (empty? pending-partials))
                         (list? partial-referent))
                (let [popped (first pending-partials)]
                  (recur (inc index)
                         (if (list? popped)
                           (apply list (concat popped [partial-referent]))
                           (conj popped partial-referent))
                         (rest pending-partials))))
              true nil)))))

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
                                  matches)]
      (first (if (empty? perfect-matches) matches perfect-matches)))
    (first matches)))

(defn best-matching-element
  "Given an immutable template, find an element of the item that
  matches it. Return a seq of the best matching element if there is one,
  otherwise return nil. Only works on immutable templates."
  [template subject]
  (let [matches (matching-elements template subject)]
    (when (not (empty? matches))
      [(best-match template matches)])))

(defn condition-to-list
  "If the condition has any item ids, look them up in the store and replace
  them with the semantic list form of what is in the store."
  [condition immutable-store]
  (flatten-nested-content
   (clojure.walk/postwalk
    (fn [referent]
      (if (item-referent? referent)
        (when (id-valid? immutable-store referent)
          (immutable-semantic-to-list
           (description->entity referent immutable-store)))
        referent))
    condition)))

(defn instantiate-referent
  "Return the items that the referent refers to. Does not handle
  virtual referents."
  [referent immutable-store]
  (case (referent-type referent)
    :item (when (id-valid? immutable-store referent)
            [(description->entity referent immutable-store)])
    :exemplar (let [[_ exemplar subject-ref] referent
                    query (pattern-to-query
                           (condition-to-list exemplar immutable-store))
                    picker (if (and (item-referent? exemplar)
                                    (id-valid? immutable-store exemplar))
                             ;; If the exemplar is an item, then always
                             ;; return it for its subject, even if another
                             ;; element of its subject is as good a match.
                             (let [exemplar-item
                                   (description->entity
                                    exemplar immutable-store)
                                   subj (Entity/subject exemplar-item)]
                               #(if (= % subj)
                                  [exemplar-item]
                                  (best-matching-element query %)))
                             #(best-matching-element query %))]
                (mapcat picker
                        (instantiate-referent subject-ref immutable-store)))
    :elements (let [[_ condition subject-ref] referent
                    query (pattern-to-query
                           (condition-to-list condition immutable-store))]
                (mapcat #(matching-elements query %)
                        (instantiate-referent subject-ref immutable-store)))
    :query (let [[_ condition] referent
                 query (pattern-to-query
                        (condition-to-list condition immutable-store))]
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
  [template subjects adjacents position use-bigger store]
  (let [[specialized-template store] (specialize-template template store)
        flattened-template (flatten-nested-content specialized-template)
        selector-template (query-to-template flattened-template 'anything)
        non-selector-template (pattern-to-possible-non-selector-template
                               flattened-template)
        [new-ids store]
        (thread-map
         (fn [[subject adjacent] store]
           (let [[store id] (create-selector-or-non-selector-element
                             selector-template non-selector-template
                             subject adjacent position use-bigger store)]
             [id store]))
         (map vector subjects adjacents)
         store)]
    [(map #(description->entity % store) new-ids)
     store]))

(declare instantiate-or-create-referent)

(defn instantiate-or-create-exemplar
  "Run instantiate-or-create-referent on all referents in the exemplar."
  [exemplar original-store store]
  (cond (referent? exemplar)
        (let [[items new-ids store]
              (instantiate-or-create-referent exemplar original-store store)]
          [(semantic-to-list-R (first items)) new-ids store])
        (seq? exemplar)
        (let [[exemplar [new-ids store]]
              (thread-recursive-map
               (fn [exemplar [new-ids store]]
                 (if (referent? exemplar)
                   (let [[items newest-ids store]
                         (instantiate-or-create-referent
                          exemplar original-store store)]
                     [(semantic-to-list-R (first items))
                      [(concat new-ids newest-ids) store]])
                   [exemplar [nil store]]))
               exemplar [nil store])]
          [exemplar new-ids store])
        true
        [exemplar nil store]))

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
      (apply map (fn [& items]
                   (furthest-item items position))
             adjacentses))))

(defn create-virtual-referent
  "Create items for virtual referents. Return the
  new items, a seq of ids of the first item created for this referent
  and each nested virtual referent, and the updated store."
  [referent original-store store]
  (assert (virtual-referent? referent))
  (let [[_ exemplar subject-referent adjacent-referents position use-bigger]
        referent
        [template new-template-ids store]
        (instantiate-or-create-exemplar exemplar original-store store)
        [subjects new-subject-ids store]
        (if (nil? subject-referent)
          [nil nil store]
          (instantiate-or-create-referent
           subject-referent original-store store))
        subjects (map #(in-different-store % store) subjects)
        adjacents (find-adjacents adjacent-referents subjects
                                  position original-store store)
        subjects
        (if (nil? subject-referent)
          (map (constantly nil) adjacents)
          subjects)
        [items store] (create-possible-selector-elements
                        template subjects adjacents
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
    ;; Some items may have the original store or only the partially
    ;; updated store. Make them all have the latest store.
    [(map #(in-different-store % store) items) ids store]))

(defn virtual-union-referent? [referent]
  (and (union-referent? referent)
       (or (virtual-referent? (second referent))
           (virtual-union-referent? (second referent)))))

(defn instantiate-or-create-referent
  "Find the items that the referent refers to, creating items
  for virtual referents. Return the items a seq of the ids of first item
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

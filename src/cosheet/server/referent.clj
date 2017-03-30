(ns cosheet.server.referent
  (:require (cosheet [utils :refer [multiset replace-in-seqs prewalk-seqs
                                    map-map thread-map thread-recursive-map]]
                     [debug :refer [simplify-for-print]]             
                     [expression :refer [expr expr-let expr-seq]]
                     [entity :refer [atom? elements content label->elements
                                     call-with-immutable description->entity
                                     StoredEntity]]
                     [canonical :refer [canonicalize-list]]
                     [store :as store :refer [id-valid? StoredItemDescription
                                              update-content]]
                     [store-utils :refer [add-entity]]
                     [store-impl :refer [->ItemId]]
                     [query :refer [matching-elements matching-items
                                    template-matches]])
            (cosheet.server
             [order-utils :refer [update-add-entity-adjacent-to
                                  furthest-item]]
             [model-utils :refer [specialize-template]])))

;;; Commands are typically run with respect to a referent, which
;;; describes what the command should act on.

;;; The semantics of a referent is a sequence of groups of items. The
;;; sequence supports DOM elements, like labels, that can refer to
;;; several parallel parts of the database, while the groups support
;;; things, like adjacent element groups, that need to refer to a
;;; collection of items. The most common situation is a singleton
;;; sequence of a singleton group. The groups may be empty, so that
;;; sequences of groups can be aligned.

;;; When a referent contains another referent as a sequence-referent,
;;; it coerces that that referent into a sequence of individual items,
;;; by concatenating the items in each group together to for a single
;;; sequence.

;;; When a referent contains a <condition>, it can be either an item
;;; id, the list form of an element, or a list of an item id and
;;; the list form of additional elements. Item ids are turned into the
;;; condition corresponding to their list expansion.

;;; The kinds of referent are:
;;;            item  <an item-id>
;;;                  Refers to the singlton group of the item.
;;;        exemplar  [:exemplar <item-id> <sequence-referent>]
;;;                  For each group of items refered to by sequence-referent,
;;;                  refers to a group of an element of each member
;;;                  whose semantic information is the same as that of
;;;                  the item.
;;;        elements  [:elements <condition> <sequence-referent>]
;;;                  For each group refered to by sequence-referent,
;;;                  refers to the group of all elements of its items
;;;                  whose semantic information matches the condition.
;;;           query  [:query <condition>]
;;;                  Refers to the group of items that satisfies the condition.
;;;           union  [:union  <referent> ...]
;;;                  Refers to one group of items per referent, each
;;;                  containg all items that referent refers to
;;;  parallel-union  [:parallel-union <referent> ...]
;;;                  Refers to a sequence of groups. Each referent must
;;;                  produce the same number of groups, and the union
;;;                  refers to the sequence of groups formed by unioning
;;;                  corresponding groups from the sequences.
;;;      difference  [:difference <referent> <referent>]
;;;                  Returns a single group of all items refered to by the
;;;                  first referent and not by the second.
;;;                  refers to the sequence of groups
;;;         virtual  [:virtual <exemplar-referent> <subject-referent>
;;;                            <adjacent-referent> position use-bigger selector]
;;;                  For each group of items refered to by sequence-referent
;;;                  refers to a group of new items matching the exemplar
;;;                  referent. The exemplar may be a condition, with nils,
;;;                  and may be a template, with 'anything or refer to an
;;;                  item that is a template. It can also be
;;;                  virtual or contain virtual referents. The subject
;;;                  referent can be virtual, and may be nil, in which case
;;;                  one element is created for each item in adjacent.
;;;                  Adjacent-referent gives items the new items should
;;;                  be adjacent to in the order. It must either
;;;                  have the same group structure as subject-referent,
;;;                  or it must have one group for each item of sequence
;;;                  referent. It may be nil, in which case subject-referent
;;;                  must not be nil, and the subject is used for adjacent.
;;;                  Position and use-bigger say where the new
;;;                  item should go relative to the adjacent item. If selector
;;;                  has the value :first-group, it means that the first
;;;                  group of items will be selectors, and so don't need
;;;                  'anything turned into space.
;;;                  Virtual referents may never be nested inside non-virtual
;;;                  referents.

(defn item-referent? [referent]
  (satisfies? StoredItemDescription referent))

(defn exemplar-referent? [referent]
  (and (sequential? referent) (= (first referent) :exemplar)))

(defn virtual-referent? [referent]
  (and (sequential? referent) (= (first referent) :virtual)))

(defn referent? [referent]
  (or (item-referent? referent)
      (and (sequential? referent)
           (#{:exemplar :elements :query :union :parallel-union :difference
              :virtual}
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

(defn parallel-union-referent
  "Create a referent for the parallel union of the referents."
  [referents]
  (doseq [referent referents]
    (assert (referent? referent)))
  (if (= (count referents) 1)
    (first referents)
    (vec (cons :parallel-union referents))))

(defn difference-referent
  "Create a difference referent."
  [plus minus]
  (assert (referent? plus))
  (assert (referent? minus))
  [:difference plus minus])

(defn virtual-referent
  "Create a virtual referent."
  [exemplar subject adjacent & {:keys [position use-bigger selector]
                                :or {position :after
                                     use-bigger false}
                                :as options}]
  (when subject (assert (referent? subject)))
  (when adjacent (assert (referent? adjacent)))
  (assert (or subject adjacent))
  (assert (#{:before :after} position))
  (assert (contains? #{true false} use-bigger))
  (assert (every? #{:position :use-bigger :selector} (keys options)))
  (when selector (assert (= selector :first-group)))
  [:virtual (coerce-item-to-id exemplar) (coerce-item-to-id subject)
            (coerce-item-to-id adjacent) position use-bigger selector])

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
  item-or-exemplar-referent could reconstruct the referent
  Returns nil for anything but item and exemplar referents."
  [referent]
  (cond (item-referent? referent) [referent nil]
        (exemplar-referent? referent) (rest referent)))

(defn first-group-referent
  "Given a referent, return a referent whose instantiation
  is just the first group of the instantiation of the referent"
  [referent]
  (case (referent-type referent)
    :item referent
    :exemplar (let [[_ exemplar subject] referent]
                    (exemplar-referent exemplar (first-group-referent subject)))
    :elements (let [[_ condition subject] referent]
                (elements-referent condition (first-group-referent subject)))
    :query referent
    :union (let [[_ & referents] referent]
             (first referents))
    :parallel-union (let [[_ & referents] referent]
                      (parallel-union-referent
                       (map first-group-referent referents)))
    :difference referent
    :virtual (let [[_ exemplar subject adjacent position use-bigger selector]
                   referent]
               (virtual-referent exemplar
                                 (when subject (first-group-referent subject))
                                 (when adjacent (first-group-referent adjacent))
                                 :position position
                                 :use-bigger use-bigger
                                 :selector selector))))

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
;;; The type type letter for the various types of referent is:
(def letters->type
  {\X :exemplar
   \E :elements
   \Q :query
   \U :union
   \P :parallel-union
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
  Return or nil if the string is not a valid representation of a referent."
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

;;; For purposes of comparing two entities, not all of their elements
;;; matter. In particular, order information, or other information
;;; about how to display the elements is considered irrelevant for
;;; matching a condition. We call the elements that matter the semantic
;;; elements. Non-semantic elements will always themselves have an
;;; element with :non-semantic as content.

(defn semantic-element?-R
  "Return true if an element counts as semantic information for its subject.
  (Doesn't have a :non-semantic element.)"
  [entity]
  (expr-let [elements (elements entity)
             element-contents (expr-seq map content elements)]
    (not-any? #(= % :non-semantic) element-contents)))

(defn semantic-elements-R
  "Return the elements of an entity that are semantic to it."
  [entity]
  (expr-let [elements (elements entity)
             non-semantic (label->elements entity :non-semantic)]
    (remove (set non-semantic) elements)))

(defn immutable-semantic-to-list
  "Given an immutable item, make a list representation of the
  semantic information of the item."
  [item]
  (if (atom? item)
    (content item)
    (let [content (content item)
          elements (semantic-elements-R item)
          content-semantic (immutable-semantic-to-list content)
          element-semantics (map immutable-semantic-to-list elements)]
      (if (empty? element-semantics)
        content-semantic
        (apply list (into [content-semantic] element-semantics))))))

(defn semantic-to-list-R
  "Given an item, make a list representation of the
  semantic information of the item."
  [item]
  (call-with-immutable item immutable-semantic-to-list))

(defn item->canonical-semantic
  "Return the canonical form of the semantic information for the item.
  Only works on immutable items."
  [item]
  (canonicalize-list (immutable-semantic-to-list item)))

(defn flatten-nested-content
  "If item has a form anywhere like ((a ...b...) ...c...), turn that into
  (a ...b... ...c...)"
  ;; This case handles adding (:top-level :non-semantic) to row referents.
  [item]
  (clojure.walk/postwalk
   (fn [item]
     (if (and (seq? item) (seq? (first item)))
       (apply list (concat (first item) (rest item)))
       item))
   item))

(defn condition-to-list
  "If the condition has an item id, look it up in the store and replace it with
  the semantic list form of what is in the store."
  [condition immutable-store]
  (flatten-nested-content
   (clojure.walk/postwalk
    (fn [referent]
      (if (item-referent? referent)
        (when (id-valid? immutable-store referent)
          (semantic-to-list-R (description->entity referent immutable-store)))
        referent))
    condition)))

(defn pattern-to-condition
  "Given a pattern, alter it to work as a condition. Specifically,
  replace 'anything and 'anything-immutable by (nil (nil :order :non-semantic))
  to make them work as a wild card that avoids matching non-user editable
  elements."
  [condition]
  (if (sequential? condition)
    (let [elements (map pattern-to-condition (rest condition))]
      (if (#{'anything 'anything-immutable} (first condition))
        (apply list (list* nil '(nil :order :non-semantic) elements))
        (cons (first condition) elements)))
    (if (#{'anything 'anything-immutable} condition)
      '(nil (nil :order :non-semantic))
      condition)))

(defn condition-to-template
  "Given a condition, turn it into a template by removing (nil :order
  :non-semantic) elements (which get added by template to condition)
  and by then replacing any nil by the specified replacement, or the empty
  string by default."
  ([condition]
   (condition-to-template condition ""))
  ([condition nil-replacement]
   (prewalk-seqs
    (fn [condition]
      (cond (sequential? condition)
            (remove #(= % '(nil :order :non-semantic)) condition)
            (nil? condition) nil-replacement
            true condition))
    condition)))

(defn best-matching-element
  "Given the list form of a template, find an element of the item that
  matches it. Return the best matching element in a list if there is one,
  otherwise return nil. Only works on immutable templates."
  [template subject]
  (let [matches (matching-elements template subject)]
    (when (not (empty? matches))
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
          [(first (if (empty? perfect-matches) matches perfect-matches))])
        matches))))

(def instantiate-referent)

(defn instantiate-to-items
  "Same as instantiate referent, except returns a list of all the
  unique items, rather than a list of groups."
  [referent immutable-store]
  (distinct (apply concat (instantiate-referent referent immutable-store))))

(defn map-over-instances
  "Instantiate the referent and call the function on each resulting item,
   collecting the results into a sequence of groups."
  [fun referent immutable-store]
  (map fun (instantiate-to-items referent immutable-store)))

(defn instantiate-referent
  "Return the groups of items that the referent refers to. Does not handle
  virtual referents."
  [referent immutable-store]
  (case (referent-type referent)
    :item (when (id-valid? immutable-store referent)
            [[(description->entity referent immutable-store)]])
    :exemplar (let [[_ exemplar subject] referent
                    condition (pattern-to-condition
                               (condition-to-list exemplar immutable-store))]
                (map (fn [group] (mapcat #(best-matching-element condition %)
                                         group))
                     (instantiate-referent subject immutable-store)))
    :elements (let [[_ condition subject] referent
                    condition (pattern-to-condition
                               (condition-to-list condition immutable-store))]
                (map (fn [group] (mapcat #(matching-elements condition %)
                                         group))
                     (instantiate-referent subject immutable-store)))
    :query (let [[_ condition] referent
                 condition (pattern-to-condition
                            (condition-to-list condition immutable-store))]
             [(matching-items condition immutable-store)])
    :union (let [[_ & referents] referent]
             (map #(instantiate-to-items % immutable-store)
                  referents))
    :parallel-union (let [[_ & referents] referent
                          groupses (map #(instantiate-referent
                                          % immutable-store)
                                        referents)]
                      (apply mapcat (fn [& groups] (apply map vector groups))
                             groupses))
    :difference (let [[_ plus minus] referent]
                  [(remove
                    (set (instantiate-to-items minus immutable-store))
                    (instantiate-to-items plus immutable-store))])
    :virtual []))

(defn adjust-adjacents
  "If there is one group of adjacents per subject, choose the one in
  each group the furthest in the direction of the position."
  [subject-groups adjacent-groups position]
  (if (= (count adjacent-groups)
         (apply + (map count subject-groups)))
    ;; We have to find the correct adjacent in each group, then
    ;; group those according to the number of items in each group
    ;; of subjects.
    (second (reduce (fn [[adjacents result] n]
                      [(nthnext adjacents n) (conj result (take n adjacents))])
                    [(map #(furthest-item % position) adjacent-groups) []]
                    (map count subject-groups)))
    adjacent-groups))

(defn create-elements-satisfying
  "Make a series of element satisfying the template.
  Return the new elements and the updated store.
  subjects and adjacent-items must be seqs of groups of items, with
  corresponding entries.
  adjacent-item and position give order information for a new elements."
  [template subject-groups adjacent-groups position use-bigger store]
  (let [[id-groups store]
        (thread-map
         (fn [[subject-group adjacent-group] store]
           (thread-map
            (fn [[subject-item adjacent-item] store]
              (let [[store id] (update-add-entity-adjacent-to
                                store (:item-id subject-item)
                                template adjacent-item position use-bigger)]
                [id store]))
            (map vector subject-group adjacent-group)
            store))
         (map vector subject-groups adjacent-groups)
         store)
        ]
    [(map (fn [group] (map #(description->entity % store) group))
          id-groups)
     store]))

(defn create-possible-selector-elements
  "Given that the first group of the subject may describe a selector, and
  the rest its selected, create appropriate elements both for the
  selector and its selected. Return the new items and new store and chosen
  specilized ids."
  [template subject-groups adjacent-groups position use-bigger selector
   store-and-chosen]
  (let [[specialized-template [store chosen]]
        (specialize-template template store-and-chosen)
        flattened-template (flatten-nested-content specialized-template)
        ;; The template for a non-selector
        template (-> flattened-template
                     pattern-to-condition
                     condition-to-template)]
    (if (= selector :first-group)
      (let [sel-template (condition-to-template specialized-template 'anything)
            [items1 store] (create-elements-satisfying
                            sel-template
                            [(first subject-groups)] [(first adjacent-groups)]
                            position use-bigger store)
            [items2 store] (create-elements-satisfying
                            template (rest subject-groups)
                            (rest adjacent-groups) position use-bigger store)]
        [(concat items1 items2) [store chosen]])
      (let [[items store] (create-elements-satisfying
                           template subject-groups adjacent-groups
                           position use-bigger store)]
        [items [store chosen]]))))

(def instantiate-or-create-referent)

(defn instantiate-or-create-exemplar
  "Run instantiate-or-create-referent on all referents in the exemplar."
  [exemplar store-and-chosen]
  (cond (referent? exemplar)
        (let [[groups store-and-chosen]
              (instantiate-or-create-referent exemplar store-and-chosen)]
          [(semantic-to-list-R (first (first groups))) store-and-chosen])
        (seq? exemplar)
        (thread-recursive-map
         (fn [exemplar store-and-chosen]
           (if (referent? exemplar)
             (let [[groups store-and-chosen]
                   (instantiate-or-create-referent exemplar store-and-chosen)]
               [(semantic-to-list-R (first (first groups))) store-and-chosen])
             [exemplar store-and-chosen]))
         exemplar
         store-and-chosen)
        true
        [exemplar store-and-chosen]))

(defn create-virtual-referent
  "Create items for virtual referents. The second argument is a pair
  of an immutable store and a map of chosen new ids. Return the groups
  of new items, and the updated second argument."
  [referent store-and-chosen]
  (assert (virtual-referent? referent))
  (let [[_ exemplar subject-referent adjacent-referent
         position use-bigger selector]
        referent
        [template [store chosen]]
        (instantiate-or-create-exemplar exemplar store-and-chosen)
        [subject-groups store-and-chosen]
        (if (nil? subject-referent)
          [nil [store chosen]]
          (instantiate-or-create-referent subject-referent [store chosen]))
        adjacent-groups (if (nil? adjacent-referent)
                          subject-groups
                          (instantiate-referent adjacent-referent store))
        subject-groups
        (if (nil? subject-referent)
          [(map-map (constantly nil) adjacent-groups)]
          subject-groups)
        adjacent-groups (adjust-adjacents
                         subject-groups adjacent-groups position)]
    (create-possible-selector-elements
     template subject-groups adjacent-groups
     position use-bigger selector store-and-chosen)))

(defn instantiate-or-create-referent
  "Find the groups of items that the referent refers to, creating items
  for virtual referents. The second argument is a pair of an immutable store
  and a map of chosen new ids. Return the groups, and the updated
  second argument."
  [referent store-and-chosen]
  (if (virtual-referent? referent)
    (create-virtual-referent referent store-and-chosen)
    [(instantiate-referent referent (first store-and-chosen)) store-and-chosen]))



 




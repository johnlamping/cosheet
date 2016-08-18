(ns cosheet.server.referent
  (:require (cosheet [utils :refer [multiset replace-in-seqs]]
                     [debug :refer [simplify-for-print]]             
                     [expression :refer [expr expr-let expr-seq]]
                     [entity :refer [atom? elements content label->elements
                                     call-with-immutable description->entity
                                     StoredEntity]]
                     [store  :as store :refer [id-valid? StoredItemDescription]]
                     [store-impl :refer [->ItemId]]
                     [query :refer [matching-elements matching-items
                                    template-matches]])))

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
;;; id or the list form of an element.

;;; The kinds of referent are:
;;;            item  <an item-id>
;;;                  Refers to the singlton group of the item.
;;;        exemplar  [:exemplar <item-id> <sequence-referent>]
;;;                  For each group of items refered to by sequence-referent,
;;;                  refers to a group of an element of each member
;;;                  whose semantic information is the same as that of
;;;                  the item.
;;;        elements  [:elements <condition> <sequence-referent>]
;;;                  For each item refered to by sequence-referent,
;;;                  refers to the group of elements of it each of whose
;;;                  semantic information matches the condition.
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

(defn item-referent? [referent]
  (satisfies? StoredItemDescription referent))

(defn exemplar-referent? [referent]
  (and (sequential? referent) (= (first referent) :exemplar)))

(comment
  (defn elements-referent? [referent]
    (and (sequential? referent) (= (first referent) :elements)))

  (defn query-referent? [referent]
    (and (sequential? referent) (= (first referent) :query)))

  (defn union-referent? [referent]
    (and (sequential? referent) (= (first referent) :union)))

  (defn parallel-union-referent? [referent]
    (and (sequential? referent) (= (first referent) :parallel-union)))

  (defn difference-referent? [referent]
    (and (sequential? referent) (= (first referent) :difference))))

(defn referent? [referent]
  (or (item-referent? referent)
      (and (sequential? referent)
           (#{:exemplar :elements :query :union :parallel-union :difference}
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
  "Create an elements referent."
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
  (for [referent referents]
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
  (for [referent referents]
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

(defn item-or-exemplar-referent
  "Make an item or an exemplar referent, as necessary,
   depending on the subject."
  [item subject]
  (if (or (nil? subject) (item-referent? subject))
    (item-referent item)
    (exemplar-referent item subject)))

(defn referent->exemplar-and-subject
  "Given a referent, return an exemplar and subject.
   Returns nil for anything but item and exemplar referents."
  [referent]
  (cond (item-referent? referent) [referent nil]
        (exemplar-referent? referent) (rest referent)))

;;; The string format of a referent is
;;;     for item referents: I<the digits of their id>
;;;    for other referents: <type letter><referent>r
;;;           for keywords: K<delimiter><keyword letters><delimiter>
;;;                for nil: N
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
   \D :difference})
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
        (keyword? referent)
        ;; For a keyword, we find a letter not in the keyword,
        ;; and use that as the delimiter.
        (let [name (name referent)
              letters (set (seq name))
              candidates (concat [\_]
                                 (map char (range (int \A) (inc (int \Z))))
                                 (map char (range (int \a) (inc (int \z)))))
              delimiter (first (remove letters candidates))]
          (str "K" delimiter name delimiter))
        (nil? referent)
        "N"
        ;; Things that look like lists can be all sort of things, so just
        ;; check for them not being vectors.
        (and (seq? referent) (not (vector? referent)))
        (let [member-strings (map referent->string referent)]
          (when (not-any? nil? member-strings)
            (str "L" (apply str member-strings) "l")))))

(defn string->referent
  "Parse a string representation of a referent.
  Return or nil if the string is not a valid representation of a referent."
  [rep]
  (loop [index 0 ; Next index in the string to look at
         partial-referent [:start] ; The referent we are constructing
         pending-partials nil] ; Partial referents the current one
                               ; is embedded in
    (if (= index (count rep))
      (when (and (= (count partial-referent) 2)
                 (empty? pending-partials))
        (second partial-referent))
      (let [first-letter (nth rep index)]
        (cond (= first-letter \r)
              (when (and (not (empty? pending-partials))
                         (vector? partial-referent)
                         (let [num_args (dec (count partial-referent))]
                           (case (first partial-referent)
                             nil nil
                             (:exemplar :elements :difference) (= num_args 2)
                             :query (= num_args 1)
                             (>= num_args 1))))
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
                (when (and number (vector? partial-referent))
                  (recur (+ index (inc (count digits)))
                         (conj partial-referent (->ItemId number))
                         pending-partials)))
              (= first-letter \K)
              (let [delimiter (nth rep (inc index))
                    end-index (clojure.string/index-of
                               rep delimiter (+ index 2))]
                (when (and end-index
                           (list? partial-referent))
                  (let [keyword (keyword (subs rep (+ index 2) end-index))]
                    (recur (inc end-index)
                           (apply list (concat partial-referent [keyword]))
                           pending-partials))))
              (= first-letter \N)
              (when (list? partial-referent)
                (recur (inc index)
                       (apply list (concat partial-referent [nil]))
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

(defn canonicalize-list
  "Given the list form of an entity, return a canonical representation of it."
  ;; We record the elements as a map from element to multiplicities,
  ;; so that we are not sensitive to the order of the elements.
  ;; That is easier than sorting, because Clojure doesn't define
  ;; a sort order between heterogenous types, like strings and ints.
  [entity]
  (if (sequential? entity)
    [(canonicalize-list (first entity))
     (multiset (map canonicalize-list (rest entity)))]
    entity))

(defn item->canonical-semantic
  "Return the canonical form of the semantic information for the item.
  Only works on immutable items."
  [item]
  (canonicalize-list (immutable-semantic-to-list item)))

(defn condition-to-list
  "If the condition is an item id, return the list form of its semantics,
   otherwise, return the condition, assuming it is already in
   semantic list form."
  [condition immutable-store]
  (replace-in-seqs
   (if (satisfies? StoredItemDescription condition)
     (when (id-valid? immutable-store condition)
       (semantic-to-list-R (description->entity condition immutable-store)))
     condition)
   :none nil))

(defn best-matching-element
  "Given the list form of semantic information and an item,
  find an element of the item that matches the semantic information.
  Return the best matching element in a list if there is one, otherwise
  return nil.
  Only works on immutable items."
  [semantic-info subject]
  (let [matches (matching-elements semantic-info subject)]
    (when (not (empty? matches))
      (if (> (count matches) 1)
        ;; Prefer one with no extra semantic info.
        (let [canonical-semantic (canonicalize-list semantic-info)
              perfect-matches (filter #(= (item->canonical-semantic %)
                                          canonical-semantic)
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
  "Return the groups of items that the referent refers to."
  [referent immutable-store]
  (case (referent-type referent)
    :item (when (id-valid? immutable-store referent)
            [[(description->entity referent immutable-store)]])
    :exemplar (let [[_ exemplar subject] referent
                    template (condition-to-list exemplar immutable-store)]
                (map (fn [group] (mapcat #(best-matching-element template %)
                                         group))
                     (instantiate-referent subject immutable-store)))
    :elements (let [[_ condition subject] referent
                    template (condition-to-list condition immutable-store)]
                (map #(matching-elements template %)
                     (instantiate-to-items subject immutable-store)))
    :query (let [[_ condition] referent
                 template (condition-to-list condition immutable-store)]
             [(matching-items condition immutable-store)])
    :union (let [[_ & referents] referent]
             (map #(instantiate-to-items % immutable-store)
                  referents))
    :parallel-union (let [[_ & referents] referent
                          groupses (map #(instantiate-referent
                                          % immutable-store)
                                        referents)]
                      (apply map (fn [& groups] (apply concat groups))
                             groupses))
    :difference (let [[_ plus minus] referent]
                  [(remove
                    (set (instantiate-to-items minus immutable-store))
                    (instantiate-to-items plus immutable-store))])))


 




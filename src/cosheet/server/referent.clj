(ns cosheet.server.referent
  (:require (cosheet [utils :refer [multiset]]
                     [expression :refer [expr expr-let expr-seq]]
                     [entity :as entity :refer [call-with-immutable
                                                StoredEntity]]
                     [store  :as store :refer [id-valid? StoredItemDescription]]
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
;;;        item: <an item-id>
;;;              Refers to the single item.
;;;    exemplar: [:exemplar <item-id> <sequence-referent>]
;;;              For each item refered to by referent, refers to an
;;;              element of it whose semantic information is the same
;;;              as that of the exemplar.
;;;    elements: [:elements <condition> <sequence-referent>]
;;;              For each item refered to by referent, refers to the
;;;              group of elements of it whose semantic information
;;;              matches the condition.
;;;       query: [:query <condition>]
;;;              Refers to the group of items that satisfies the condition.
;;;       union: [:union <referent> ...]
;;;              Refers to a group of items. Each referent must
;;;              produce the same number of groups, and the union
;;;              refers to the sequence of groups formed by unioning
;;;              corresponding groups from the sequences.
;;;  difference: [:difference <referent> <referent>]
;;;              The two referents must produce the same number of
;;;              groups of items. Refers to the sequence of
;;;              differences of those groups.

(defn item-referent? [referent]
  (satisfies? StoredItemDescription referent))

(comment

  (defn exemplar-referent? [referent]
    (and (sequential? referent) (= ( first referent) :exemplar)))

  (defn elements-referent? [referent]
    (and (sequential? referent) (= ( first referent) :elements)))

  (defn query-referent? [referent]
    (and (sequential? referent) (= ( first referent) :query)))

  (defn union-referent? [referent]
    (and (sequential? referent) (= ( first referent) :union)))

  (defn difference-referent? [referent]
    (and (sequential? referent) (= ( first referent) :difference))))

(defn referent? [referent]
  (or (item-referent? referent)
      (and (sequential? referent)
           (#{:exemplar :elements :query :union :difference}
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
    (assert (satisfies? StoredItemDescription id))
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
  [& referents]
  (for [referent referents]
    (assert (referent? referent)))
  (into [:union] referents))

(defn difference-referent
  "Create a difference referent."
  [plus minus]
  (assert (referent? plus))
  (assert (referent? minus))
  [:difference plus minus])

(defn make-item-or-exemplar
  "Make an item or an exemplar, as necessary, depending on the subject."
  [item subject]
  (if (or  (empty? subject) (item-referent? subject))
    (item-referent item)
    (exemplar-referent item subject)))

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
  (expr-let [elements (entity/elements entity)
             element-contents (expr-seq map entity/content elements)]
    (not-any? #(= % :non-semantic) element-contents)))

(defn semantic-elements-R
  "Return the elements of an entity that are semantic to it."
  [entity]
  (expr-let [elements (entity/elements entity)
             non-semantic (entity/label->elements entity :non-semantic)]
    (remove (set non-semantic) elements)))

(defn immutable-semantic-to-list
  "Given an immutable item, make a list representation of the
  semantic information of the item."
  [item]
  (if (entity/atom? item)
    (entity/content item)
    (let [content (entity/content item)
          elements (semantic-elements-R item)
          content-semantic (immutable-semantic-to-list content)
          element-semantics (map immutable-semantic-to-list elements)]
      (if (empty? element-semantics)
        content-semantic
        (list* (into [content-semantic] element-semantics))))))

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

(defn replace-nones
  "Replace any :none in the seq with nil"
  [x]
  (cond (sequential? x) (map replace-nones x)
        (= x :none) nil
        true x))

(defn condition-to-list
  "If the condition is an item id, return the list form of its semantics,
   otherwise, return the condition, assuming it is already in
   semantic list form."
  [condition immutable-store]
  (replace-nones
   (if (satisfies? StoredItemDescription condition)
     (when (id-valid? immutable-store condition)
       (semantic-to-list-R (description->entity condition immutable-store)))
     condition)))

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

(defn map-over-instances
  "Instantiate the referent and call the function on each resulting item,
   collecting the results into a sequence of groups."
  [fun referent immutable-store]
  (map fun
       (apply concat (instantiate-referent referent immutable-store))))

(defn instantiate-referent
  "Return the groups of items that the referent refers to."
  [referent immutable-store]
  (case (referent-type referent)
    :item (when (id-valid? immutable-store referent)
            [[(description->entity referent immutable-store)]])
    :exemplar (let [[_ exemplar subject] referent
                    template (condition-to-list exemplar immutable-store)]
                (map-over-instances #(best-matching-element template %)
                                    subject immutable-store))
    :elements (let [[_ condition subject] referent
                    template (condition-to-list condition immutable-store)]
                (map-over-instances #(matching-elements template %)
                                    subject immutable-store))
    :query (let [[_ condition] referent
                 template (condition-to-list condition immutable-store)]
             [(matching-items condition immutable-store)])
    :union (let [[_ & referents] referent
                 groups (map #(instantiate-referent % immutable-store)
                             referents)]
             (apply map (fn [& groups] (apply concat groups)) groups))
    :difference (let [[_ plus minus] referent]
                  (map (fn [p m] (remove (set m) p))
                       (instantiate-referent plus immutable-store)
                       (instantiate-referent minus immutable-store)))))


 




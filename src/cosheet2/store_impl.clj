(ns cosheet2.store-impl
  (:require (cosheet2 [store :refer :all :as store]
                      [entity :refer [StoredEntity]
                              :as entity]
                      [utils :refer [pseudo-set-set
                                     pseudo-set-seq
                                     pseudo-set-set-membership
                                     pseudo-set-conj
                                     pseudo-set-disj
                                     pseudo-set-contains?
                                     union-seqs
                                     dissoc-in
                                     update-in-clean-up]]
                      [canonical :refer [canonical-primitive-form]]
                      [orderable :refer [->Orderable]])
            clojure.edn))

;;; The data in a store logically consists of a bunch of links
;;; each of which is completely determined by its id,
;;; target, and source. For efficiency, a store maintains indexes on
;;; that data.

(declare anonymous-object-id?)
(declare has-link-to-anonymous-object?)
(declare add-link-from-triple)
(declare add-or-defer-link)
(declare candidate-matching-ids-and-estimate)
(declare all-temporary-ids)
(declare add-modified-id)
(declare index-all)

(defrecord ElementStoreImpl
    ^{:doc
      "An immutable store with some indexing."}
   [;;; These first two maps give the primitive facts about the store's
    ;;; links.
    
    ;;; Map from a link's ItemId to its target
    id->target
    
    ;;; Map from a link's ItemId to its source
    id->source

    ;;; A set of ids that have been declared temporary.
    temporary-ids

    ;;; A derived map from ItemId to a pseudo-set of the ids of the links
    ;;; that target it.
    target->ids

    ;;; A derived map from the canonical-primitive-form of source to a
    ;;; pseudo-set of ids with that source.
    ;;; TODO: !!! Remove the following once sources can no longer be nil.
    ;;; Nil source is not indexed.
    source->ids

    ;;; A derived set of the ItemIds of links that are are marked as
    ;;; being labels. This doesn't count ids that are labels by virtue
    ;;; of having their source be a symbol.
    marked-as-type

    ;;; A derived map from ItemId to a pseudo-set of the keywords that
    ;;; are the source of at least one of its elements.
    id->keywords

    ;;; A derived map that indexes everything that looks like
    ;;;    target <- link1 <- link2 -o label
    ;;; The map takes the target, then the label and returns the ids of
    ;;; the link2s in the diagram, whose source is that label.
    ;;; In other words, given a target id and a label, this map give
    ;;; the pseudo set of all links whose source is that label, and
    ;;; that target links that target to the provided target id.
    ;;; This is priarily used to find link1s in the diagram, all links
    ;;; with a given target and that have a given label. But this
    ;;; index is easier to maintain than than an index to the link1s,
    ;;; because it lists the liks that make them have those labels, so
    ;;; it is easier to check for changes.
    target->label->label-ids

    ;;; This is the analogue of target->label->label-ids, but for
    ;;;    source o- link1 <- link2 -o label
    source->label->label-ids

    ;;; The next number to assign to a new link or object.
    next-number

    ;;; A set of ids that have been updated since the last call to
    ;;; clear-modified-ids. This is only present if track-modified-ids
    ;;; has been called.
    modified-ids

    ;;; Whether this store state is an equivalent undo point to the
    ;;; previous state.  (Starts out false.)
    equivalent-undo-point

    ;;; A list of [function arg arg ...] calls that need to be
    ;;; performed. (These have no effect on the store, but can be added
    ;;; as a store is modified inside an atomic action, then done after
    ;;; the action finishes.)
    further-actions
   ]

  Store

  (id-valid-link? [this id]
    (and (link-id? id)
         (contains? (:id->source this) id)))

  (id-described-object? [this id]
    (and (object-id? id))
    (contains? (:target->ids this) id))

  (id->target [this id]
    (when (link-id? id)
      (get-in this [:id->target id])))

  (id->source [this id]
    (when (link-id? id)
      (get-in this [:id->source id])))

  (target->ids [this target]
    (pseudo-set-seq (get-in this [:target->ids
                                  (canonical-primitive-form target)])))

  (source->ids [this source]
    (pseudo-set-seq (get-in this [:source->ids
                                  (canonical-primitive-form source)])))

  (target-source->ids [this target source]
    (let [target-ids (get-in this [:target->ids
                                   (canonical-primitive-form target)])
          source-ids (get-in this [:source->ids
                                   (canonical-primitive-form source)])]
      ;; Do the intersection in the most efficient way, and do it lazily.
      (if (set? target-ids)
        (if (set? source-ids)
          (if (< (count source-ids) (count target-ids))
            (filter target-ids source-ids)
            (filter source-ids target-ids))
          (filter target-ids (pseudo-set-seq source-ids)))
        (if (set? source-ids)
          (filter source-ids (pseudo-set-seq target-ids))
          (filter #(pseudo-set-contains? source-ids %)
                  (pseudo-set-seq target-ids))))))

  (target-label->ids [this target label]
    (seq
     (distinct ; In case one id has more than one matching label.
      (map #(get-in this [:id->target %])
           (pseudo-set-seq
            (get-in this [:target->label->label-ids
                          (canonical-primitive-form target)
                          (canonical-primitive-form label)]))))))

  (source-label->ids [this source label]
    (seq
     (distinct ; In case one id has more than one matching label.
      (map #(get-in this [:id->target %])
           (pseudo-set-seq
            (get-in this [:source->label->label-ids
                          (canonical-primitive-form source)
                          (canonical-primitive-form label)]))))))

  (id->marked-as-type? [this id]
    (contains? (:marked-as-type this) id))

  (candidate-matching-ids [this template]
    (if (nil? template)
      ;; The template is vacuous. Return all ids we know of.
      [(seq (union-seqs (keys id->source) ;; This picks up all elements.
                        ;; These pick up all objects.
                        (filter object-id?
                                (union-seqs (keys target->ids)
                                            (keys source->ids)))))
       false]
      (let [[estimate ids precise]
            (candidate-matching-ids-and-estimate this template)
            id-filter (if (entity/object? template)
                        object-id?
                        link-id?)]
        (if (nil? estimate)
          ;; The template is so generic that none of our indices can narrow
          ;; it down based on any of its elements. Return basically everything.
          [(seq (if (seq (entity/elements template))
                  ;; The template has an element.
                  ;; Return all ids of the right kind that have elements.
                  (filter id-filter (keys target->ids))
                  ;; Nothing in the index helps. Find all of the right kind
                  ;; of ids that the store knows about.
                  (filter id-filter (if (entity/object? template)
                                      (union-seqs (keys target->ids)
                                                  (keys source->ids))
                                      (keys id->source)))))
           false]
          [(seq (filter id-filter ids)) precise]))))

  (mutable-store? [this] false)
  
  ImmutableStore

  (add-link [this target source]
    ;; TODO: !!! Once we are using objects at top level, assert that
    ;;       neither target nor source are nil.
    
    ;; Disallow links between two anonymous objects that both already
    ;; have links to anonymous objects. This ensures that the
    ;; relationships between anonymous objects will not have any
    ;; loops. Both queries and Entity/to-list rely on that.
    ;; We can't put this test in add-link-from-triple because that is
    ;; called to read in a store, where, depending on the order links
    ;; are read, this condition might be violated, even though there
    ;; are no loops throuch anonymous objects.
    (when (and (object-id? target) ;; Redundant, but fast.
               (object-id? source) ;; Redundant, but fast.
               (anonymous-object-id? this target)
               (anonymous-object-id? this source))
      (assert (not (and (has-link-to-anonymous-object? this target)
                        (has-link-to-anonymous-object? this source)))))
    (let [item-id (->ItemId (:next-number this))]
      [(-> this
           (update-in [:next-number] inc)
           (add-link-from-triple item-id target source))
       item-id]))

  (remove-link [this id]
    (assert (link-id? id)
            ["Not a link id "id])
    ;; We have to explicitly ask for the versions of id->source and
    ;; target->ids from the store namespace, because inside
    ;; ElementStoreImpl, they reference its fields of those names.
    (assert (not (nil? (store/id->source this id)))
            ["Removed id not present." id])
    (assert (nil? (store/target->ids this id))
            ["Removed id is a target." id])
    (-> this
        (dissoc-in [:id->source id])
        (dissoc-in [:id->target id])
        (index-all this id)
        (add-modified-id id)))

  (update-target [this id target]
    (assert (not (nil? (store/id->source this id)))
            ["Link id not present." id])
    (assert (not (nil? target)))
    (assert (not (link-id? target)))
    (assert (not (link-id? (store/id->target this id))))
    (-> this
        (assoc-in [:id->target id] target)
        (index-all this id)
        (add-modified-id id)))

  (update-source [this id source]
    (assert (not (nil? (store/id->source this id)))
            ["Link id not present." id])
    (assert (not (nil? source)))
    (assert (not (link-id? source)))
    (-> this
        (assoc-in [:id->source id] source)
        (index-all this id)
        (add-modified-id id)))

  (get-new-object-id [this]
    (let [next (:next-number this)]
      [(update-in this [:next-number] inc)
       (->ItemId (- next))])) ; Object ids are negative numbers.

  (track-modified-ids [this]
    (assoc this :modified-ids #{}))

  (fetch-and-clear-modified-ids [this]
    [(assoc this :modified-ids #{})
     (:modified-ids this)])

  (update-equivalent-undo-point [this equivalent]
    (assoc this :equivalent-undo-point equivalent))

  (equivalent-undo-point? [this]
    (:equivalent-undo-point this))

  (store-fetch-and-clear-further-actions [this]
    ;; We can't dissoc :further-actions, or we will get back a map,
    ;; not a store record.
    [(assoc this :further-actions nil) (:further-actions this)])

  (declare-temporary-id [this id]
    (assert (:id->source this))
    (update this :temporary-ids #(conj % id)))

  (store-to-data [this]
    "Extract just the essential data from the store, in preparation for
     writing it out. The data consists of the next id, and a vector of
     (link-id target source) triples for its links. A few things are
     represented as vectors that start with a keyword:
       ItemId [:id (:id ?])
       Orderable [:ord (left ?) (right ?)]
       Vector [:vec * ?]"
    (let [temporary-ids (all-temporary-ids this)]
      [(:next-number this)
       (for [[id source]
             (seq (:id->source this))
             :when (not (temporary-ids id))]
         [(:id id)
          (:id (get-in this [:id->target id]))
          (cond (item-id? source)
                [:id (:id source)]
                (instance? cosheet2.orderable.Orderable source)
                [:ord (:left source) (:right source)]
                (vector? source)
                (into [:vec] source)
                true
                source)])]))

  (write-store [this stream]
    (with-open [writer (clojure.java.io/writer stream)]
      (binding [*out* writer]
        (pr (store-to-data this)))))

  (data-to-store [this data]
    "Given a store's essential data, add it to a store."
    (let [[next-number links] data]
      (let [[store deferred]
            (reduce (fn [[store deferred] [id target source]]
                      (let [id (->ItemId id)
                            target (when target (->ItemId target))
                            source (if (vector? source)
                                      (apply (case (first source)
                                               :id ->ItemId
                                               :ord ->Orderable
                                               :vec vector)
                                             (rest source))
                                      source)]
                        (add-or-defer-link
                         store deferred id target source)))
                    [(assoc (new-element-store) :next-number next-number) {}]
                    links)]
        (assert (empty? deferred) deferred)
        store)))

  (read-store [this stream]
    (with-open [reader (java.io.PushbackReader.
                        (java.io.InputStreamReader. stream))]
      (binding [*in* reader]
        (data-to-store this (clojure.edn/read reader))))))

;;; These are utility functions for abstracting from target and source
;;; to endpoint.
(defn endpoint-fetcher
  "Return the accessor for the given endpoint"
  [endpoint]
  (case endpoint :target id->target :source id->source))

(defn endpoint-value-key
    "Return the key in a store for the map that holds id->endpoint."
  [endpoint]
  (case endpoint :target :id->target :source :id->source))

(defn endpoint-index-key
  "Return the key in a store for the map that holds endpoint->ids."
  [endpoint]
  (case endpoint :target :target->ids :source :source->ids))

(defn endpoint-label-index-key
  "Return the key in a store for the map that holds endpoint->label->label-ids."
  [endpoint]
  (case endpoint
    :target :target->label->label-ids
    :source :source->label->label-ids))

(defn index-endpoint->ids
  "Reflect this link correctly in either the target->ids or source->ids index,
   depending on the value of endpoint."
  [store old-store endpoint id]
  (let [fetcher (endpoint-fetcher endpoint)
        index-key (endpoint-index-key endpoint)
        new-endpoint-value (canonical-primitive-form (fetcher store id))
        old-endpoint-value (canonical-primitive-form (fetcher old-store id))]
    (if (= new-endpoint-value old-endpoint-value)
      store
      (cond-> store
        old-endpoint-value
        (update-in-clean-up [index-key old-endpoint-value]
                            #(pseudo-set-disj % id))
        new-endpoint-value
        (update-in [index-key new-endpoint-value]
                   #(pseudo-set-conj % id))))))

(defn index-marked-as-type
  "Reflect a link in marked-as-type.
  The id argument is the id that might do the marking.
  Requires that target->ids and source->ids be valid."
  [store old-store id]
  (let [marks-as-type (= (id->source store id) :label)
        old-marks-as-type (= (id->source old-store id) :label)
        target (id->target store id)
        old-target (id->target old-store id)]
    (if (and (= marks-as-type old-marks-as-type)
             (= target old-target))
      store
      (cond-> store
        (and old-marks-as-type
             (empty? (target-source->ids store old-target :label)))
        (update-in [:marked-as-type] #(disj % old-target))
        ;; TODO: !!! Get rid of the possibility the target is nil,
        ;;       once that is forbidden.
        (and marks-as-type target)
        (update-in [:marked-as-type] #(conj % target))))))

;;; NOTE: This definition must be kept in synch with entity/label?
;;; TODO: !!! Get rid of the :marked-as-type test (and its index),
;;;       which gets rid of :label marking labels
(defn id-is-label?
  "Return whether the id counts as a label. A label is a link under
  which its target should be indexed, starting from either of the
  target's endpoints.
  A link is a label if:
     * It's source is either
        * a keyword that is not :label
        * the object with item-id 'name'.
        * an object that has an element whose content has an item-id of
         'link-type' or 'object-type'.
     * Has an element whose content is :label (obsolete)
  Requires that :marked-as-type is up to date.
  Requires that :target->ids and :source->ids are up to date."
  [store id]
  (or (let [source (id->source store id)]
        (cond (object-id? source)
              (or (= source name-label-id)
                  (seq (target-source->ids store source link-type-id))
                  (seq (target-source->ids store source object-type-id)))
              (keyword? source)
              (not= source :label)))
      (contains? (:marked-as-type store) id)))

(defn index-endpoint->label->label-ids-from-label
  "Reflect a label in endpoint->label->label-ids.
  The id argument is the link that might be a label for its target. Go
  to the target of that link, and index by either its target or source
  endpoint."
  [store old-store endpoint id]
  (let [fetcher (endpoint-fetcher endpoint)
        index-key (endpoint-label-index-key endpoint)
        is-label (id-is-label? store id)
        old-is-label (id-is-label? old-store id)
        label-value (canonical-primitive-form (id->source store id))
        old-label-value (canonical-primitive-form (id->source old-store id))
        endpoint-value (canonical-primitive-form
                        (fetcher store (id->target store id)))
        old-endpoint-value (canonical-primitive-form
                            (fetcher old-store (id->target old-store id)))]
    (if (or (and (= is-label old-is-label)
                 (= label-value old-label-value)
                 (= endpoint-value old-endpoint-value))
            (and (nil? endpoint-value) (nil? old-endpoint-value)))
      store
      (cond-> store
        (and old-is-label old-endpoint-value)
        (update-in-clean-up [index-key old-endpoint-value old-label-value]
                            #(pseudo-set-disj % id))
        (and is-label endpoint-value)
        (update-in [index-key endpoint-value label-value]
                   #(pseudo-set-conj % id))))))

(defn index-endpoint->label->label-ids
  "Reflect the effects of this link in the endpoint->label->label-ids index.
  The indices :target->ids, Lsource->ids, and :marked-as-type
  index must be valid when this is called. (This function uses
  id-is-label?, which uses all of those indices.)"
  [store old-store endpoint id]
  (as-> store store
      ;; Handle when id is a label.
      (index-endpoint->label->label-ids-from-label store old-store endpoint id)
      ;; Handle when id makes its target a label.
      (index-endpoint->label->label-ids-from-label
       store old-store
       endpoint (or (id->target store id) (id->target old-store id)))
      ;; Handle when id is a link that got a label, and its endpoint changed.
      (let [fetcher (endpoint-fetcher endpoint)]
        (if (= (fetcher store id) (fetcher old-store id))
          store
          (let [label-ids (filter #(id-is-label? store %)
                                  (target->ids store id))]
            (reduce
             (fn [store label-id]
               (index-endpoint->label->label-ids-from-label
                store old-store endpoint label-id))
             store label-ids))))))

(defn index-all
  "Do all indexing for adding, removing or changing the id in the store."
  [store old-store id]
  (-> store 
      (index-endpoint->ids old-store :target id)
      (index-endpoint->ids old-store :source id)
      ;; This must be done before the next two, as they depend on labels.
      (index-marked-as-type old-store id)
      (index-endpoint->label->label-ids old-store :target id)
      (index-endpoint->label->label-ids old-store :source id)))

(defn add-modified-id
  "Add the id to the modified id set of the store,
  if we are tracking modified ids."
  [store id]
  (if (:modified-ids store)
    (update-in store [:modified-ids] #(conj % id))
    store))

;;; Note: This must be kept in synch with Entity/anonymous-object?
(defn anonymous-object-id?
  "Return true if the item id represents an anonymous object."
  [store item-id]
  (and (object-id? item-id)
       ;; Doesn't have a special id.
       (not (string? (:id item-id)))
       ;; Doesn't have a name.
       (not (when-let [name-ids (target-label->ids
                                 store item-id (make-item-id "name"))]
              (some #(not (contains? #{nil "" 'anything} %))
                    (map #(id->source store %) name-ids))))))

(defn has-link-to-anonymous-object?
  "Return true if the item id has a link to an id representing an
  anonymous object."
  [store item-id]
  (let [anonymous-id? #(anonymous-object-id? store %)]
    (or (some anonymous-id? (map #(id->source store %)
                                 (target->ids store item-id)))
        (some anonymous-id? (map #(id->target store %)
                                 (source->ids store item-id))))))

(defn has-name-link?
  "Return true if the item id has a link that gives it a non-trivial name."
  [store item-id])

(defn add-link-from-triple
  "Add a link to the store, given its target, source, and id. And do all
  necessary indexing."
  [store item-id target source]
  ;; TODO: !!! disallow nil once objects are supported.
  (assert (or (nil? target)
              (object-id? target)
              (id-valid-link? store target))
          [item-id target source])
  (assert (and (not (nil? source))
               (not (link-id? source)))
          [item-id target source])
  (assert (not= target source))
  (when (number? (:id item-id))
    (assert (< (:id item-id) (:next-number store)) [item-id target source])
    (when (number? (:id target))
      (assert (< (:id target) (:id item-id)) [item-id target source])))
  (-> (if (nil? target)
        store
        (assoc-in store [:id->target item-id] target))
      (assoc-in [:id->source item-id] source)
      (index-all store item-id)
      (add-modified-id item-id)))

(defn descendant-ids [store id]
  "Given a link id, return a seq of the id and the ids of all its
   descendant elements, including elements of its content if that is
   an anonymous object."
  (concat [id]
          (mapcat #(descendant-ids store %) (target->ids store id))
          (let [content (id->source store id)]
            (when (anonymous-object-id? store content)
              (descendant-ids store content)))))

(defn all-temporary-ids [store]
  "Return a set of all declared temporary ids and their descendant elements."
  (set (mapcat #(descendant-ids store %) (:temporary-ids store))))

(defn add-or-defer-link
  ;; Utility function for read-store.  The links may have been
  ;; written out in any order, but we cannot add a link until after
  ;; its target has been added. When we encounter a link that can't
  ;; yet be added, we save it in deferred, indexed under what it is
  ;; waiting for, then add it when we get what it needs.  Return the
  ;; new store and new deferred.
  [store deferred id target source]
  (let [waiting-for (first (filter #(and (link-id? %)
                                         (not ((:id->source store) %)))
                                   [target source]))]
    (if waiting-for
      [store (update-in deferred [waiting-for]
                        #(conj % [id target source]))]
      (reduce (fn [[store deferred] [id target source]]
                (add-or-defer-link store deferred id target source))
              [(add-link-from-triple store id target source)
               (dissoc deferred id)]
              (deferred id)))))

;;; TODO: If there are precise lists for each element, but the
;;; elements don't have distinct contents, group the elements that
;;; might overlap, and if they have reasonably similar costs, do
;;; sort-by their targets, then run utils/disjoint_combinations for
;;; each target to see if it qualifies.
(defn subsuming-ids-and-estimates-from-elements
  "Given a template, return a seq of pairs: <estimate of number of
  candidates, a lazy seq of the candidate matching ids>, with one pair
  for each informative element of the template. The candidate ids of
  each pair include the ids of all possible entities with an element
  possibly matching that element of the template. This means that each
  list will include all possible ids matching the template. Also
  return a boolean that is true if an id in the intersection of the
  candidate lists is always a match."
  [store template]
  (let [elements (entity/elements template)]
    (if (empty? elements)
      [nil true]
      (let [candidates
            (keep (fn [element]
                    (let [[estimate element-ids precise]
                          (candidate-matching-ids-and-estimate store element)]
                      (when estimate
                        (let [subject-getter (if (= (entity/orientation element)
                                                    :target)
                                               id->source
                                               id->target)
                              ids (keep #(subject-getter store %) element-ids)]
                          [estimate ids precise]))))
                  elements)]
        [(map (fn [[estimate ids precise]] [estimate ids])
              candidates)
         ;; For us to be precise we require
         ;;   * Our id lists for each element are precise for that element.
         ;;   * A match for one element is never a match for another.
         ;;     (Otherwise, we might, for example, return a one
         ;;     element item for a template that requires two elements.)
         (and (= (count elements) (count candidates))
              (every? (fn [[estimate ids precise]] precise) candidates)
              (let [contents (map entity/content elements)]
                (and (not-any? #(or (nil? %)
                                    (and (entity/object? %)
                                         (not (satisfies? StoredEntity %))))
                               contents)
                     (apply distinct? contents))))]))))

(defn subsuming-ids-and-estimates
  "Return a seq of pairs <estimate of number of candidates,
  a lazy seq of the candidate matching ids>. Each list will include all
  possible matches. Also return a boolean that is true if an id in the
  intersection of the candidate lists is always a match."
  [store template]
  (let [[element-matches element-matches-precise]
        (subsuming-ids-and-estimates-from-elements store template)
        content (entity/content template)]
    (cond
      (nil? content)
      [element-matches element-matches-precise]
      
      ;; TODO: When the content is an anonymous object, get candidate
      ;;       ids for it, then use those as if they were content?
      (and (entity/object? content)
           (not (satisfies? StoredEntity content)))
      [element-matches false]
      
      true
      (let [content-index (if (= (entity/orientation template) :target)
                            target->ids
                            source->ids)
            content-ids (content-index store (entity/entity-key content))]
        [(concat [[(count content-ids) content-ids]]
                 element-matches)
         element-matches-precise]))))

;;; TODO: Instead using an estimate of the number of final candidates,
;;; the estimate should use the number of candidates that need to be
;;; considered to produce the final list. That is what you want when
;;; you trade-off pruning candidates against lists vs checking each of
;;; them against the store.

;;; TODO: Make the intermediate candidate computations take one more
;;; argument: whether they need to return a precise result, or can
;;; drop precision for more efficiency. That gives the later code the
;;; option of asking for the less precise result if it is going to
;;; drop precision anyway.

;;; TODO: If a template element has a label, filter with
;;; target->label->label-ids if the label intersection list would be
;;; too large. Likewise, if the template is tagged :label, filter with
;;; marked-as-type
(defn candidate-matching-ids-and-estimate
  "Given a template that is valid as a query to the store, Return
   a triple consisting of:
     * an estimate of number of candidates,
     * a lazy seq of the candidate matching ids,
     * a boolean that is true if an id that is in the candidates
       is always a match.
  But if the template provides no information, return nil.
  If the template has any StoredEntity objects, they must match exactly."
  [store template]
  (let [[possibilities precise] (subsuming-ids-and-estimates store template)]
    (when (not (empty? possibilities))
      (let [lowest (apply min (map first possibilities))
            threshold (* (if precise 20 10) lowest)
            good? #(<= (first %) threshold)]
        [lowest
         ;; Intersect all the candidate lists that aren't more than
         ;; 10 times the size of the smallest.
         (lazy-seq (->> possibilities
                        (filter good?)
                        (map second)
                        (map set)
                        (apply clojure.set/intersection)
                        (filter (if (entity/object? template)
                                  object-id?
                                  link-id?))))
         (and precise
              (every? good? possibilities))]))))

(defmethod print-method ElementStoreImpl [s ^java.io.Writer w]
  (.write w "ElementStore"))

(defmethod new-element-store true []
  (map->ElementStoreImpl {:id->target {}
                          :id->source {}
                          :target->ids {}
                          :source->ids {}
                          :marked-as-type #{}
                          :target->label->label-ids {}
                          :source->label->label-ids {}
                          :temporary-ids #{}
                          :next-number 1
                          :modified-ids nil
                          :equivalent-undo-point false}))


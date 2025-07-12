(ns cosheet2.store-impl
  (:require (cosheet2 [store :refer :all :as store]
                      [entity :as entity]
                      [utils :refer [pseudo-set-set
                                     pseudo-set-seq
                                     pseudo-set-set-membership
                                     pseudo-set-conj
                                     pseudo-set-disj
                                     pseudo-set-contains?
                                     dissoc-in
                                     update-in-clean-up]]
                      [canonical :refer [canonical-primitive-form]]
                      [orderable :refer [->Orderable]])
            clojure.edn))

;;; The data in a store logically consists of a bunch of links
;;; each of which is completely determined by its id,
;;; target, and source. For efficiency, a store maintains indexes on
;;; that data.

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

    ;;; The next id to assign to an item to be stored here.
    next-id

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
    (contains? (:id->source this) id))

  (id-described-object? [this id]
    (and (is-object-id? id))
    (contains? (:target->ids this) id))

  (id->target [this id]
    (when (is-link-id? id)
      (get-in this [:id->target id])))

  (id->source [this id]
    (when (is-link-id? id)
      (get-in this [:id->source id])))

  (target->ids [this target]
    (pseudo-set-seq (get-in this [:target->ids
                                  (canonical-primitive-form target)])))

  (source->ids [this source]
    (pseudo-set-seq (get-in this [:source->ids
                                  (canonical-primitive-form source)])))

  (target-label->ids [this target label]
    (seq
     (map #(get-in this [:id->target %])
          (pseudo-set-seq
           (get-in this [:target->label->label-ids
                         (canonical-primitive-form target)
                         (canonical-primitive-form label)])))))

  (source-label->ids [this source label]
    (seq
     (map #(get-in this [:id->target %])
          (pseudo-set-seq
           (get-in this [:source->label->label-ids
                         (canonical-primitive-form source)
                         (canonical-primitive-form label)])))))


  (id->marked-as-type? [this id]
    (contains? (:marked-as-type this) id))

  (candidate-matching-ids [this template]
    (let [[estimate ids precise]
          (candidate-matching-ids-and-estimate this template)]
      (if (nil? estimate)
        ;; The template is so generic that none of our indices can narrow
        ;; it down based on any of its sources. Return basically everything.
        [(if (and (sequential? template) (seq (rest template)))
            ;; The template has an element.
            ;; Return all items that have elements.
            (keys target->ids)
            (keys (:id->source this)))
         false]
        [ids precise])))

  (mutable-store? [this] false)
  
  ImmutableStore

  (add-link [this target source]
    (let [item-id (->ItemId (:next-id this))]
      [(-> this
           (update-in [:next-id] inc)
           (add-link-from-triple item-id target source))
       item-id]))

  (remove-link [this id]
    (assert (is-link-id? id)
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

  (update-source [this id source]
    (assert (not (nil? source)))
    (assert (not (is-link-id? source)))
    (-> this
        (assoc-in [:id->source id] source)
        (index-all this id)
        (add-modified-id id)))

  (get-unique-number [this]
    [(:next-id this) (update-in this [:next-id] inc)])

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
      [(:next-id this)
       (for [[id source]
             (seq (:id->source this))
             :when (not (temporary-ids id))]
         [(:id id)
          (:id (get-in this [:id->target id]))
          (cond (is-item-id? source)
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
    (let [[next-id links] data]
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
                    [(assoc (new-element-store) :next-id next-id) {}]
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

;; NOTE: This definition must be kept in synch with entity/label?
(defn id-is-label?
  "Return whether the given link counts as a label (either has source
  that is a keyword and is not :label, or has an element whose source
  is :label)."
  [store id]
  (or (let [source (id->source store id)]
        (and (keyword? source) (not= source :label)))
      (contains? (:marked-as-type store) id)))

(defn index-marked-as-type
  "Reflect this link's effect on the labels."
  [store old-store id]
  (let [marks-type (= (id->source store id) :label)
        old-marks-type (= (id->source old-store id) :label)]
    (if (= marks-type old-marks-type)
      store
      (if marks-type
        (let [target (id->target store id)]
          (if target
            (update-in store [:marked-as-type]
                       #(conj % target))
            store))
        ;; There might be another mark.
        (if (some #(= (id->source store %) :label)
                  (target->ids store (id->target old-store id)))
          store
          (update-in store [:marked-as-type]
                     #(disj % (id->target old-store id))))))))

(defn index-endpoint->label->label-ids-from-label
  "Reflect a label in endpoint->label->label-ids."
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
  The target->ids index and the marked-as-type index must be valid when
  this is called. (This function uses id-is-label, which uses
  marked-as-type.)"
  [store old-store endpoint id]
  (as-> store store
      ;; Handle when id is a label.
      (index-endpoint->label->label-ids-from-label store old-store endpoint id)
      ;; Handle when id makes its target a label.
      (index-endpoint->label->label-ids-from-label
       store old-store
       endpoint (or (id->target store id) (id->target old-store id)))
      ;; Handle when id is a link that got a label, and its endpoint changed
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

(defn add-link-from-triple
  "Add a link to the store, given its target, source, and id. And do all
  necessary indexing."
  [store item-id target source]
  ;; TODO: !!! disallow nil once objects are supported.
  (assert (or (nil? target)
              (is-item-id? target))
          [item-id target source])
  (assert (and (not (nil? source))
               (not (is-link-id? source)))
          [item-id target source])
  (when (number? (:id item-id))
    (assert (< (:id item-id) (:next-id store)) [item-id target source])
    (when (number? (:id target))
      (assert (< (:id target) (:id item-id)) [item-id target source])))
  (-> (if (nil? target)
        store
        (assoc-in store [:id->target item-id] target))
      (assoc-in [:id->source item-id] source)
      (index-all store item-id)
      (add-modified-id item-id)))

(defn descendant-ids [store id]
  "Return a seq of the id and ids of all its descendant elements."
  (cons id (mapcat #(descendant-ids store %) (target->ids store id))))

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
  (let [waiting-for (first (filter #(and (is-link-id? %)
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
;;; elements don't have distinct sources, group the elements that
;;; might overlap, and if they have reasonably similar costs, do
;;; sort-by their targets, then run utils/disjoint_combinations for
;;; each target to see if it qualifies.
(defn subsuming-elements-ids-and-estimates
  "Return a seq of pairs, <estimate of number of candidates, a lazy
  seq of the candidate matching ids>, one pair for each informative
  element. Each list will subsume all possible matches. Also return a
  boolean that is true if an id in the intersection of the candidate
  lists is always a match."
  [store elements]
  (if (empty? elements)
    [nil true]
    (let [candidates (map #(candidate-matching-ids-and-estimate store %)
                          elements)]
      [(keep (fn [[estimate ids precise]]
               (when estimate [estimate (keep #(id->target store %) ids)]))
             candidates)
       ;; We are precise if we have precise id lists for each element,
       ;; and a match for one element is never a match for
       ;; another. (Otherwise, we might, for example, return a one
       ;; element item for a template that requires two elements.)
       (and (every? (fn [[estimate ids precise]] precise) candidates)
            (let [contents (map entity/content elements)]
              (and (not-any? nil? contents)
                   (apply distinct? contents))))])))

(defn subsuming-ids-and-estimates
  "Return a seq of pairs <estimate of number of candidates,
  a lazy seq of the candidate matching ids>. Each list will include all
  possible matches. Also return a boolean that is true if an id in the
  intersection of the candidate lists is always a match."
  [store template]
  (let [content (entity/content template)
        elements (entity/elements template)
        [element-matches element-matches-precise]
        (subsuming-elements-ids-and-estimates store elements)]
    (if (nil? content)
      [element-matches element-matches-precise]
      (let [source-ids (source->ids store content)]
        [(concat [[(count source-ids) source-ids]]
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
  "Return a triple consisting of:
     * an estimate of number of candidates,
     * a lazy seq of the candidate matching ids,
     * a boolean that is true if an id that is in all the candidates
       is always a match.
  But if the template provides no information, return nil."
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
                        (apply clojure.set/intersection)))
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
                          :next-id 1
                          :modified-ids nil
                          :equivalent-undo-point false}))


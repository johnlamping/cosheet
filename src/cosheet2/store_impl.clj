(ns cosheet2.store-impl
  (:require (cosheet2 [store :refer :all]
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

(declare add-link-impl)
(declare remove-link-impl)
(declare add-or-defer-link)
(declare candidate-matching-ids-and-estimate)
(declare all-forward-reachable-ids)
(declare all-temporary-ids)
(declare add-modified-ids-for-id-and-containers)
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

  (target->ids [this id]
    (pseudo-set-seq (get-in this [:target->ids id])))

  (target-label->ids [this id label]
    (let [canonnical-label (canonical-primitive-form label)]
      (seq
       (map #(get-in this [:id->target %])
            (pseudo-set-seq
             (get-in this [:target->label->label-ids id canonnical-label]))))))

  (id->has-keyword? [this id keyword]
    (pseudo-set-contains? (get-in this [:id->keywords id]) keyword))

  (source->ids [this id]
    ;; TODO: !!! Remove this assert
    (assert (is-item-id? id))
    (pseudo-set-seq (get-in this [:source->ids id])))

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
    (assert (or (nil? target)
                (is-item-id? target)))
    (assert (not (nil? source)))
    (assert (not (is-link-id? source)))
    (let [item-id (->ItemId (:next-id this))]
      [(-> this
           (update-in [:next-id] inc)
           (add-link-impl item-id target source))
       item-id]))

  (remove-link [this id]
    (remove-link-impl this id))

  (update-source [this id source]
    (assert (not (nil? source)))
    (assert (not (is-link-id? source)))
    (-> this
        (assoc-in [:id->source id] source)
        (index-all this id)
        (add-modified-ids-for-id-and-containers id)))

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

;;; TODO: This needs to generalize to include objects too.
(defn all-ids-eventually-holding-source
  "Return all links that contain the source, possibly through
   a chain of containment."
  [store source]
  (let [links (pseudo-set-seq
               (get-in store [:source->ids (canonical-primitive-form
                                             source)]))]
    (concat links
            (mapcat #(all-ids-eventually-holding-source store %) links))))

(defn all-ids-eventually-holding-id
  "Return a seq of the ids of all links whose source chain goes
  through this link. That includes the link, all links whose source
  is this link, and all links eventually holding them."
  [store id]
  (conj (all-ids-eventually-holding-source store id) id))

(defn all-forward-reachable-ids
  "Return a seq of all the ids that can be reached from this id
   via target or source links. It includes the id, itself."
  [store id]
  (when id
    (concat [id]
            (mapcat #(when (is-item-id? %)
                       (all-forward-reachable-ids store %))
                    [(id->target store id)
                     (id->source store id)]))))

(defn index-endpoint->ids
  "Reflect this link correctly in either the target->ids or source->ids index,
   depending on the value of endpoint."
  [store old-store endpoint id]
  (let [fetcher (case endpoint :target id->target :source id->source)
        index-key (case endpoint :target :target->ids :source :source->ids)
        new-endpoint (fetcher store id)
        old-endpoint (fetcher old-store id)]
    (if (= new-endpoint old-endpoint)
      store
      (cond-> store
        old-endpoint
        (update-in-clean-up [index-key (canonical-primitive-form old-endpoint)]
                            #(pseudo-set-disj % id))
        ;; TODO: !!! Remove this condition once links must have both endpoints.
        new-endpoint
        (update-in [index-key (canonical-primitive-form new-endpoint)]
                   #(pseudo-set-conj % id))))))

(defn index-id->keywords
  "Reflect this link's source in the id->keywords index.
   The target->ids index must be valid when this is called."
  [store old-store id]
  (let [source (id->source store id)
        old-source (id->source old-store id)
        target (or (id->target store id) (id->target old-store id))]
    (if (or (= source old-source) (not target))
      store
      (cond-> store
        (and (keyword? old-source)
             (not-any? #(= (id->source store %) old-source)
                       (target->ids store target)))
        (update-in-clean-up [:id->keywords target]
                            #(pseudo-set-disj % old-source))
        (keyword? source)
        (update-in [:id->keywords target]
                   #(pseudo-set-conj % source))))))

;; NOTE: This definition must be kept in synch with entity/label?
(defn id-is-label?
  "Return whether the given link counts as a label (either has source
  that is a keyword and is not :label, or has an element whose source
  is :label)."
  [store id]
  (or (let [source (id->source store id)]
        (and (keyword? source) (not= source :label)))
      (pseudo-set-contains? (get-in store [:id->keywords id]) :label)))

(defn index-grandtarget-target->label->label-ids
  "Reflect this link in target->label->label-ids for its grand-target."
  [store old-store id]
  (let [is-label (id-is-label? store id)
        old-is-label (id-is-label? old-store id)
        canonical (canonical-primitive-form (id->source store id))
        old-canonical (canonical-primitive-form (id->source old-store id))
        grandtarget (or (id->target store (id->target store id))
                          (id->target old-store (id->target old-store id)))]
    (if (or (and (= is-label old-is-label)
                 (= canonical old-canonical))
            (not grandtarget))
      store
      (cond-> store
        old-is-label
        (update-in-clean-up [:target->label->label-ids
                             grandtarget
                             old-canonical]
                            #(pseudo-set-disj % id))
        is-label
        (update-in [:target->label->label-ids grandtarget canonical]
                   #(pseudo-set-conj % id))))))

(defn index-target->label->label-ids
  "Reflect the effects of this link in the target->label->label-ids index.
  The id->keywords index must be valid when this is called."
  [store old-store id]
  (-> store
      ;; Our link
      (index-grandtarget-target->label->label-ids old-store id)
      ;; Our target, which we may affect being a label
      (index-grandtarget-target->label->label-ids
       old-store (or (id->target store id) (id->target old-store id)))))

(defn index-all
  "Do all indexing for adding, removing or changing the id in the store."
  [store old-store id]
  (-> store 
      (index-endpoint->ids old-store :target id)
      (index-endpoint->ids old-store :source id)
      (index-id->keywords old-store id)
      (index-target->label->label-ids old-store id)))

(defn add-modified-id
  "Add the id to the modified id set of the store,
  if we are tracking modified ids."
  [store id]
  (if (:modified-ids store)
    (update-in store [:modified-ids] #(conj % id))
    store))

(defn add-modified-ids-for-id-and-containers
  "Add the id to the modified ids,
   and add any id that recursively contains it."
  [store id]
  (if (:modified-ids store)
    (update-in store [:modified-ids]
               #(into % (all-ids-eventually-holding-id store id)))
    store))

(defn add-link-impl
  "Add a link to the store, and do all necessary indexing."
  [store item-id target source]
  (assert (not (nil? source)) [item-id target source])
  (assert (not= item-id target) [item-id target source])
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

(defn remove-link-impl [store id]
    (assert (not (nil? (id->source store id)))
            "Removed id not present.")
    (assert (nil? (target->ids store id))
            "Removed id is a target.")
    (assert (nil? (get-in store [:source->ids id]))
            "Removed id is the source of another.")
    (-> store
        (dissoc-in [:id->source id])
        (dissoc-in [:id->target id])
        (index-all store id)
        (add-modified-id id)))

(defn descendant-ids [store id]
  "Return a seq of the id and ids of all its descendant elements."
  (cons id (mapcat #(descendant-ids store %) (target->ids store id))))

(defn all-temporary-ids [store]
  "Return a set of all declared temporary ids and their descendant elements."
  (set (mapcat #(descendant-ids store %) (:temporary-ids store))))

(defn add-or-defer-link
  ;; Utility function for read-store.  The liniks may have been
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
              [(add-link-impl store id target source)
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
      (let [source-ids (all-ids-eventually-holding-source store content)]
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
;;; id->keywords.
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
                          :id->keywords {}
                          :target->label->label-ids {}
                          :temporary-ids #{}
                          :next-id 1
                          :modified-ids nil
                          :equivalent-undo-point false}))


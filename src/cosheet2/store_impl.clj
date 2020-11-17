(ns cosheet2.store-impl
  (:require (cosheet2 [store :refer :all]
                      [utils :refer [canonical-atom-form
                                     pseudo-set-set
                                     pseudo-set-seq
                                     pseudo-set-set-membership
                                     pseudo-set-contains?
                                     parse-string-as-number
                                     dissoc-in
                                     update-in-clean-up]]
                      [orderable :refer [->Orderable]])
            clojure.edn))
;;; TODO: Have candidate-matching-ids return whether its result
;;;       is exact.
;;;       Add method to check for keyword.

;;; Data in a store consists of ItemId objects. All that the system
;;; needs to know about an ItemId is what its content is, and what
;;; other item it is an element of. Everything else follows from what
;;; it means to be an item and an element of an item. In practice, the
;;; store saves indexing information as well.

;;; If the content of an item is a constant, the user might want to
;;; add elements to it, as if it were an item with that content. The
;;; store supports treating the content as if it were an item, by
;;; creating a ImplicitContentId. It will turn this into it into an
;;; actual item if elements are added.

(def id->string)

(defrecord
    ^{:doc
      "The id of an item in a store."}
    ItemId
  [id]

  StoredItemDescription

  (stored-item-description-name [this]
    (clojure.string/join ["Id-" (id->string this)])))

(defn id->string
  "Return a string representation of an id."
  [id]
  (assert (instance? ItemId id))
  (let [id (:id id)]
    (if (integer? id) (str id) (str "I" id))))

(defn string->id
  "Given the string representation of an id, return the id."
  [rep]
  (->ItemId (if (= (first rep) \I) (subs rep 1) (parse-string-as-number rep))))

(defn all-ids-eventually-holding-content
  "Return all items that contain the content, possibly through
   a chain of containment."
  [store content]
  (let [items (pseudo-set-seq
               (get-in store [:content->ids (canonical-atom-form content)]))]
    (concat items
            (mapcat #(all-ids-eventually-holding-content store %) items))))

(defn all-ids-eventually-holding-id
  "Return a seq of the ids of all items whose atomic value chain goes
  through this item. That includes the item, all items whose content
  is this item, and all items eventually holding them."
  [store id]
  (conj (all-ids-eventually-holding-content store id) id))

(defn all-forward-reachable-ids
  "Return a seq of all the ids that can be reached from this id
   via subject or content links. It includes the id, itself."
  [store id]
  (when id
    (concat [id]
            (mapcat #(when (instance? ItemId %)
                       (all-forward-reachable-ids store %))
                    [(id->subject store id)
                     (id->content store id)]))))

(defn index-id->elements
  "Reflect this item in the id->elements index."
  [store old-store id]
  (let [subject (id->subject store id)
        old-subject (id->subject old-store id)]
    (if (= subject old-subject)
      store
      (let [adding (not old-subject)]
        (update-in-clean-up store [:id->elements (or subject old-subject)]
                            #(pseudo-set-set-membership % id adding))))))

(defn index-content->ids
  "Put this item in the content->ids index."
  [store old-store id]
  (let [content (id->content store id)
        old-content (id->content old-store id)]
    (if (= content old-content)
      store
      (cond-> store
        old-content
        (update-in-clean-up [:content->ids (canonical-atom-form old-content)]
                            #(pseudo-set-set-membership % id false))
        content
        (update-in-clean-up [:content->ids (canonical-atom-form content)]
                            #(pseudo-set-set-membership % id true))))))

(defn index-id->keywords
  "Reflect this item's content in the id->keywords index.
   The id->elements index must be valid when this is called."
  [store old-store id]
  (let [content (id->content store id)
        old-content (id->content old-store id)
        subject (or (id->subject store id) (id->subject old-store id))]
    (if (or (= content old-content) (not subject))
      store
      (cond-> store
        (and (keyword? old-content)
             (not-any? #(= (id->content store %) old-content)
                       (id->element-ids store subject)))
        (update-in-clean-up [:id->keywords subject]
                            #(pseudo-set-set-membership % old-content false))
        (keyword? content)
        (update-in-clean-up [:id->keywords subject]
                            #(pseudo-set-set-membership % content true))))))

;; NOTE: This definition must be kept in synch with entity/label?
(defn id-is-label?
  "Return whether the given item counts as a label."
  [store id]
  (or (let [content (id->content store id)]
        (and (keyword? content) (not= content :label)))
      (pseudo-set-contains? (get-in store [:id->keywords id]) :label)))

(defn index-one-item-id->label->ids
  "Return an indexer that adds or removes the given item, which must
   be a label, to id->label->ids for its grand-subject."
  [store old-store id]
  (let [is-label (id-is-label? store id)
        old-is-label (id-is-label? old-store id)
        canonical (canonical-atom-form (id->content store id))
        old-canonical (canonical-atom-form (id->content old-store id))
        grand-subject (or (id->subject store (id->subject store id))
                          (id->subject old-store (id->subject old-store id)))]
    (if (or (and (= is-label old-is-label)
                 (= canonical old-canonical))
            (not grand-subject))
      store
      (cond-> store
        old-is-label
        (update-in-clean-up [:id->label->ids grand-subject old-canonical]
                            #(pseudo-set-set-membership % id false))
        is-label
        (update-in-clean-up [:id->label->ids grand-subject canonical]
                            #(pseudo-set-set-membership % id true))))))

(defn index-id->label->ids
  "Reflect the effects of this item in the id->label->ids index.
  The id->keywords index must be valid when this is called."
  [store old-store id]
  (-> store
      ;; Our item
      (index-one-item-id->label->ids old-store id)
      ;; Our subject, which we may affect being a label
      (index-one-item-id->label->ids
       old-store (or (id->subject store id) (id->subject old-store id)))))

(defn index-all
  "Do all indexing for adding, removing or changing the id in the store."
  [store old-store id]
  (-> store 
      (index-id->elements old-store id)
      (index-content->ids old-store id)
      (index-id->keywords old-store id)
      (index-id->label->ids old-store id)))

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

(defn add-triple
  "Add a triple to the store, and do all necessary indexing."
  [store item-id subject content]
  (assert (not (nil? content)) [item-id subject content])
  (assert (not= item-id subject) [item-id subject content])
  (when (number? (:id item-id))
    (assert (< (:id item-id) (:next-id store)) [item-id subject content])
    (when (number? (:id subject))
      (assert (< (:id subject) (:id item-id)) [item-id subject content])))
  (-> (if (nil? subject)
        store
        (assoc-in store [:id->subject item-id] subject))
      (assoc-in [:id->content-data item-id] content)
      (index-all store item-id)
      (add-modified-id item-id)))

(defn remove-triple [store id]
    (assert (not (nil? (id->content store id)))
            "Removed id not present.")
    (assert (nil? (id->element-ids store id))
            "Removed id has elements.")
    (assert (nil? (get-in store [:content->ids id]))
            "Removed id is the content of another.")
    (-> store
        (dissoc-in [:id->content-data id])
        (dissoc-in [:id->subject id])
        (index-all store id)
        (add-modified-id id)))

(defn descendant-ids [store id]
  "Return a seq of the id and ids of all its descendant elements."
  (cons id (mapcat #(descendant-ids store %) (id->element-ids store id))))

(defn all-temporary-ids [store]
  "Return a set of all declared temporary ids and their descendant elements."
  (set (mapcat #(descendant-ids store %) (:temporary-ids store))))

(defn add-or-defer-triple
  ;; Utility function for read-store.
  ;; The triples may have been written out in any order, but we cannot
  ;; add a triple until its subject has been added (and after its
  ;; content has been added, if the content is an id). When we encounter
  ;; a triple that can't yet added, we save it in deferred,
  ;; indexed under what it is waiting for, then add it when we get what
  ;; it needs.
  ;; Return the new store and new deferred.
  [store deferred id subject content]
  (let [needed (first (filter #(and (instance? ItemId %)
                                    (not ((:id->content-data store) %)))
                              [subject content]))]
    (if needed
      [store (update-in deferred [needed]
                        #(conj % [id subject content]))]
      (reduce (fn [[store deferred] [id subject content]]
                (add-or-defer-triple store deferred id subject content))
              [(add-triple store id subject content)
               (dissoc deferred id)]
              (deferred id)))))

(def candidate-matching-ids-and-estimate)

(defn- coerce-template-to-list
  "Turn an atomic template into a list of one item. That form supports
  deconstruction."
  [template]
  (if (seq? template) template [template]))

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
               (when estimate [estimate (keep #(id->subject store %) ids)]))
             candidates)
       ;; We are precise if we have precise id lists for each element,
       ;; and a match for one element is never a match for
       ;; another. (Otherwise, we might, for example, return a one
       ;; element item for a template that requires two elements.)
       (and (every? (fn [[estimate ids precise]] precise) candidates)
            ;; We can't use the entity code to extract the content and
            ;; elements, because that code depends on this file.
            (let [contents (map #(first (coerce-template-to-list %)) elements)]
              (and (not-any? nil? contents)
                   (apply distinct? contents))))])))

(defn subsuming-ids-and-estimates
  "Return a seq of pairs <estimate of number of candidates,
  a lazy seq of the candidate matching ids. Each list will include all
  possible matches. Also return a boolean that is true if an id in the
  intersection of the candidate lists is always a match."
  [store template]
  ;; We can't use the entity code to extract the content and elements,
  ;; because that code depends on this file.
  (let [[content & elements]
        (coerce-template-to-list template)
        [element-matches element-matches-precise]
        (subsuming-elements-ids-and-estimates store elements)]
    (if (nil? content)
      [element-matches element-matches-precise]
      (let [content-ids (all-ids-eventually-holding-content store content)]
        [(concat [[(count content-ids) content-ids]]
                 element-matches)
         element-matches-precise]))))

;;; TODO: If a template element has a label, filter with id->label->ids
;;; if the label intersection list would be too large. Likewise, if the
;;; template is tagged :label, filter with id->keywords.
(defn candidate-matching-ids-and-estimate
  "Return a triple
     <an estimate of number of candidates,
      a lazy seq of the candidate matching ids,
      a boolean that is true if an id in the candidates is always a match.
  But if the template provides no information, return nil."
  [store template]
  (let [[possibilities precise] (subsuming-ids-and-estimates store template)]
    (when (not (empty? possibilities))
      (let [lowest (apply min (map first possibilities))
            threshold (* 10 lowest)
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

(defrecord ElementStoreImpl
    ^{:doc
      "An immutable store with some indexing."}
   [;;; These first two maps give the primitive facts about the store's
    ;;; triples.
    
    ;;; Map from ItemId to its subject
    id->subject
    
    ;;; Map from ItemId to its content
    ;;; We have to call it id->content-data, to avoid conflicting with
    ;;; the method id->content
    id->content-data

    ;;; A set of ids that have been declared temporary.
    temporary-ids

    ;;; A derived map from item id to a pseudo-set of the ids of its
    ;;; elements.
    id->elements

    ;;; A derived index from the canonical-atom-form of content to a
    ;;; pseudo-set of ids with that content. Nil content is not
    ;;; indexed.
    content->ids

    ;;; A derived map from item id to a pseudo-set of the keywords that
    ;;; are the content of at least one of its elements.
    id->keywords

    ;;; A derived map from item id, then label to a pseudo-set of the
    ;;; elements of elements of the id that have label as content and
    ;;; are considered to be labels. A label is either an element of
    ;;; the form (? :label) or with content :order
    id->label->ids

    ;;; The next id to assign to an item to be stored here.
    next-id

    ;;; A set of ids that have been updated since the last call to
    ;;; clear-modified-ids. This is only present if track-modified-ids
    ;;; has been called.
    modified-ids

    ;;; Whether this store state is an equivalent undo point.
    ;;; (Starts out false.)
    equivalent-undo-point

    ;;; A list of [function arg arg ...] calls that need to be
    ;;; performed. (These have no effect on the store, but can be added
    ;;; as a store is modified inside an atomic action, then done after
    ;;; the action finishes.)
    further-actions
   ]

  Store

  (id-valid? [this id]
    (contains? (:id->content-data this) id))

  (id->subject [this id]
    (when (instance? ItemId id)
      (get-in this [:id->subject id])))

  (id->content [this id]
    (if (instance? ItemId id)
      (get-in this [:id->content-data id])
      id))

  (id->element-ids [this id]
    (pseudo-set-seq (get-in this [:id->elements id])))

  (id-label->element-ids [this id label]
    (seq
     (map #(get-in this [:id->subject %])
          (pseudo-set-seq
           (get-in this [:id->label->ids id (canonical-atom-form label)])))))

  (id->has-keyword? [this id keyword]
    (pseudo-set-contains? (get-in this [:id->keywords id]) keyword))

  (id->containing-ids [this id]
    (assert (satisfies? StoredItemDescription id))
    (pseudo-set-set (get-in this [:content->ids id])))

  (candidate-matching-ids [this template]
    (let [[estimate ids precise]
          (candidate-matching-ids-and-estimate this template)]
      (if (nil? estimate)
        ;; The template is so generic that none of our indices can narrow
        ;; it down based on any of its contents. Return basically everything.
        [(if (and (sequential? template) (seq (rest template)))
            ;; The template has an element.
            ;; Return all items that have elements.
            (keys id->elements)
            (keys id->content-data))
         false]
        [ids precise])))

  (mutable-store? [this] false)
  
  ImmutableStore

  (add-simple-item [this subject content]
    (assert (not (nil? content)))
    (let [item-id (->ItemId (:next-id this))]
      [(-> this
           (update-in [:next-id] inc)
           (add-triple item-id subject content))
       item-id]))

  (remove-simple-item [this id]
    (remove-triple this id))

  (update-content [this id content]
    (assert (not (nil? content)))
    ;; Check that we are not creating a forward cycle.
    (when (instance? ItemId content)
      (assert (not-any? #{id} (all-forward-reachable-ids this content))))
    (-> this
        (assoc-in [:id->content-data id] content)
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
    [(assoc this :further-actions nil) (:further-actions this)])

  (declare-temporary-id [this id]
    (assert (:id->content-data this))
    (update this :temporary-ids #(conj % id)))

  (store-to-data [this]
    "Extract just the essential data from the store, in preparation
     for writing it out. If the content of an item is an id, it is
     represented as [:id <ItemId>] and if it is a Orderable, it is represented
     as [:ord <left> <right>]."
    (let [temporary-ids (all-temporary-ids this)]
      [(:next-id this)
       (for [[id content]
             (seq (:id->content-data this))
             :when (not (temporary-ids id))]
         [(:id id)
          (:id (get-in this [:id->subject id]))
          (cond (instance? ItemId content)
                [:id (:id content)]
                (instance? cosheet2.orderable.Orderable content)
                [:ord (:left content) (:right content)]
                (vector? content)
                (into [:vec] content)
                true
                content)])]))

  (write-store [this stream]
    (with-open [writer (clojure.java.io/writer stream)]
      (binding [*out* writer]
        (pr (store-to-data this)))))

  (data-to-store [this data]
    "Given a store's essential data, add it to a store."
    (let [[next-id items] data]
      (let [[store deferred]
            (reduce (fn [[store deferred] [id subject content]]
                      (let [id (->ItemId id)
                            subject (when subject (->ItemId subject))
                            content (if (vector? content)
                                      (apply (case (first content)
                                               :id ->ItemId
                                               :ord ->Orderable
                                               :vec vector)
                                             (rest content))
                                      content)]
                        (add-or-defer-triple
                         store deferred id subject content)))
                    [(assoc (new-element-store) :next-id next-id) {}]
                    items)]
        (assert (empty? deferred) deferred)
        store)))

  (read-store [this stream]
    (with-open [reader (java.io.PushbackReader.
                        (java.io.InputStreamReader. stream))]
      (binding [*in* reader]
        (data-to-store this (clojure.edn/read reader))))))

(defmethod print-method ElementStoreImpl [s ^java.io.Writer w]
  (.write w "ElementStore"))

(defmethod new-element-store true []
  (map->ElementStoreImpl {:id->subject {}
                          :id->content-data {}
                          :id->elements {}
                          :content->ids {}
                          :id->keywords {}
                          :id->label->ids {}
                          :temporary-ids #{}
                          :next-id 0
                          :modified-ids nil
                          :equivalent-undo-point false}))

(defmethod make-id true [id]
  ;; Integers are reserved for creation by the store
  (assert (not (integer? id)))
  (->ItemId id))

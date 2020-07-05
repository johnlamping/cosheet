(ns cosheet2.store-impl
  (:require (cosheet2 [store :refer :all]
                      [utils :refer [canonical-atom-form
                                     pseudo-set-seq
                                     pseudo-set-conj
                                     pseudo-set-disj
                                     pseudo-set-set-membership
                                     pseudo-set-contains?
                                     parse-string-as-number
                                     dissoc-in
                                     update-in-clean-up]]
                      [orderable :refer [->Orderable]])
            clojure.edn))
;;; TODO: add test for failing on trying to add a content loop
;;;       Have candidate-matching-ids return whether its result
;;;       is exact.
;;;       Replace (get-in store [:id->subject x]) with id->subject
;;;       Same for id->content

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

;;; Chase contents until an atomic value is found.
(defn atomic-value [store description]
  (if (satisfies? StoredItemDescription description)
    (atomic-value store (get-in store [:id->content-data description]))
    description))

(defn all-ids-eventually-holding-content
  "Return all items that contain the content, possibly through
   a chain of containment."
  [store content]
  (let [items (pseudo-set-seq
               (get-in store [:content->ids (canonical-atom-form content)]))]
    (concat items (mapcat #(eventually-containing-items store %) items))))

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
                    [(get-in store [:id->subject id])
                     (get-in store [:id->content-data id])]))))

(defn index-id->elements
  "Put this item in the id->elements index."
  [store id add?]
  (if-let [subject (get-in store [:id->subject id])]
    (update-in-clean-up store [:id->elements subject]
                        #(pseudo-set-set-membership % id add?))
    store))

(defn index-content->ids
  "Put this item in the content->ids index."
  [store id add?]
  (let [content (get-in store [:id->content-data id])]
    (if (nil? content)
      store
      (update-in-clean-up store [:content->ids (canonical-atom-form content)]
                          #(pseudo-set-set-membership % id add?)))))

(defn independently-keyworded?
  "Return true if the item has another element that gives it the same
   label as the specified element."
  [store id element-id keyword]
  (some #(and (not= % element-id)
              (= (get-in store [:id->content-data %]) keyword))
        (pseudo-set-seq (get-in store [:id->elements id]))))

(defn index-id->keywords
  "Reflect this item's content in the id->keywords index.
   The id->elements index must be valid when this is called."
  [store id add?]
  (let [content (get-in store [:id->content-data id])]
    (if (keyword? content)
      (if-let [subject (get-in store [:id->subject id])]
        (if (and (not add?)
                 (independently-keyworded? store subject id content))
          store ;; Another element has the same keyword as content.
          (update-in-clean-up store [:id->keywords subject]
                              #(pseudo-set-set-membership % content add?)))
        store)
      store)))

(defn one-item-indexer-id->label->ids
  "Return an indexer that reflects the given item, which must have
  a :label element, to id->label->ids for its grand-subject."
  [add?]
  (fn [store id]
    (let [canonical (canonical-atom-form (atomic-value store id))
          subject (get-in store [:id->subject id])]
      (if-let [grand-subject (get-in store [:id->subject subject])]
        (update-in-clean-up store [:id->label->ids grand-subject canonical]
                            #(pseudo-set-set-membership % id add?))
        store))))

(defn index-id->label->ids
  "Reflect the effects of this item in the id->label->ids index.
  The id->keywords index must be valid when this is called."
  [store id add?]
  (let [indexer (one-item-indexer-id->label->ids add?) 
        labeled (concat
                 ;; Labels with our content as their atomic-value
                 (filter #(pseudo-set-contains?
                           (get-in store [:id->keywords %]) :label)
                         (all-ids-eventually-holding-id store id))
                 ;; The item that we make a label
                 (when (= (atomic-value store id) :label)
                   (when-let [subject (get-in store [:id->subject id])]
                     (when (not (independently-keyworded?
                                 store subject id :label))
                       [subject]))))]
    (reduce indexer store labeled)))

(defn index-all
  "Do all indexing for adding or removing the id from the store."
  [store id add?]
  ;; We have to do the indexing in the opposite order for adding vs
  ;; removing, to satisfy the constraints of what indexes must be
  ;; valid.
  (if add?
    (-> store 
        (index-id->elements id true)
        (index-content->ids id true)
        (index-id->keywords id true)
        (index-id->label->ids id true))
    (-> store 
        (index-id->label->ids id false)
        (index-id->keywords id false)
        (index-content->ids id false)
        (index-id->elements id false))))

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

(defn check-content
  "Make sure that the content is non-nil, and doesn't introduce
  cycles in the subject and content links"
  [store id content]
  (assert (not (nil? content)))
  (when (instance? ItemId content)
    (assert (not-any? #{id} (all-forward-reachable-ids store content)))))

(defn add-triple
  "Add a triple to the store, and do all necessary indexing."
  [store item-id subject content]
  (check-content store item-id content)
  (-> (if (nil? subject)
        store
        (assoc-in store [:id->subject item-id] subject))
      (assoc-in [:id->content-data item-id] content)
      (index-all item-id true)
      (add-modified-id item-id)))

(defn remove-triple [this id]
    (assert (not (nil? (get-in this [:id->content-data id])))
            "Removed id not present.")
    (assert (nil? (get-in this [:id->elements id]))
            "Removed id has elements.")
    (assert (nil? (get-in this [:content->ids id]))
            "Removed id is the content of another.")
    (-> this
        (index-all id false)
        (dissoc-in [:id->content-data id])
        (dissoc-in [:id->subject id])
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

(defn subsuming-elements-ids-and-estimates
   "Return a seq of pairs of an estimate of number of candidates and
    a lazy seq of the candidate matching ids, one pair for each informative
    element. Each list will subsume all possible matches."
  [store elements]
  (keep (fn [element]
          (let [[estimate ids] (candidate-matching-ids-and-estimate
                                store element)]
            (when estimate
              [estimate
               (keep #(get-in store [:id->subject %]) ids)])))
        elements))

(defn subsuming-ids-and-estimates
   "Return a seq of pairs of an estimate of number of candidates and
    a lazy seq of the candidate matching ids. Each list will
    include all possible matches."
  [store template]
  ;; We can't use the entity code to extract the content and elements,
  ;; because that code depends on this file.
  (let [[content elements] (if (seq? template)
                             [(first template) (rest template)]
                             [template nil])
        element-matches (subsuming-elements-ids-and-estimates store elements)]
    (if (nil? content)
      element-matches
      (let [content-ids (all-ids-eventually-holding-content store content)]
        (concat [[(count content-ids) content-ids]]
                element-matches)))))

;;; TODO: If a template element has a label, filter with id->label->ids
(defn candidate-matching-ids-and-estimate
  "Return a pair of an estimate of number of candidates and a lazy seq of
   the candidate matching ids. But if the template
   provides no information, return nil."
  [store template]
  (let [possibilities (subsuming-ids-and-estimates store template)]
    (when (not (empty? possibilities))
      (let [lowest (apply min (map first possibilities))
            threshold (* 10 lowest)]
        [lowest
         ;; Intersect all the candidate lists that aren't more than
         ;; 10 times the size of the smallest.
         (lazy-seq (->> possibilities
                        (filter #(<= (first %) threshold))
                        (map second)
                        (map set)
                        (apply clojure.set/intersection)))]))))

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
    ;;; elements of elements of the id that have label as atomic-value and
    ;;; that have :label as the content of one of their elements. That
    ;;; is, all elements of elements of the form (<label> :label)
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
   ]

  Store

  (id-valid? [this id]
    (contains? (:id->content-data this) id))

  (id-label->element-ids [this id label]
    (seq
     (map #(get-in this [:id->subject %])
          (pseudo-set-seq
           (get-in this [:id->label->ids id (canonical-atom-form label)])))))

  (id->element-ids [this id]
    (pseudo-set-seq (get-in this [:id->elements id])))

  (id->content [this id]
    (if (instance? ItemId id)
      (get-in this [:id->content-data id])
      id))

  (candidate-matching-ids [this template]
    (let [pair (candidate-matching-ids-and-estimate this template)]
      (if (nil? pair)
        ;; The template is so generic that none of our indices can narrow
        ;; it down based on any of its contents. Return basically everything.
        (if (and (sequential? template) (seq (rest template)))
          ;; The template has an element.
          ;; Return all items that have elements.
          (keys id->elements)
          (keys id->content-data))
        (second pair))))

  (mutable-store? [this] false)

  (id->subject [this id]
    (when (instance? ItemId id)
      (get-in this [:id->subject id])))
  
  ImmutableStore

  (add-simple-item [this subject content]
    (assert (not (nil? content)))
    (let [item-id (->ItemId (:next-id this))]
      [(-> this
           (update-in [:next-id] inc)
           (add-triple item-id subject content))
       item-id]))

  (remove-simple-item [this id]
    (assert (not (nil? (get-in this [:id->content-data id])))
            "Removed id not present.")
    (assert (nil? (get-in this [:id->elements id]))
            "Removed id has elements.")
    (remove-triple this id))

  (update-content [this id content]
    (check-content this id content)
    (-> this
        (index-all id false)
        (assoc-in [:id->content-data id] content)
        (index-all id true)
        (add-modified-ids-for-id-and-containers id)))

  (get-unique-number [this]
    [(:next-id this) (update-in this [:next-id] inc)])

  (track-modified-ids [this]
    (assoc this :modified-ids #{}))

  (equivalent-undo-point? [this]
    (:equivalent-undo-point this))

  (update-equivalent-undo-point [this equivalent]
    (assoc this :equivalent-undo-point equivalent))

  (fetch-and-clear-modified-ids [this]
    [(assoc this :modified-ids #{})
     (:modified-ids this)])

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
        (assert (empty? deferred))
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

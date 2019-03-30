(ns cosheet.store-impl
  (:require (cosheet [store :refer :all]
                     [utils :refer :all]
                     [orderable :refer [->Orderable]])
            clojure.edn))

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

(defrecord
    ^{:doc
      "The id of an item in a store."}
    ItemId
  [id]

  StoredItemDescription

  (non-implicit-id [this] this)

  (atom-description? [this] false)

  (stored-item-id-string [this] (str id))

  (stored-item-description-name [this] (clojure.string/join ["Id-" id])))

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

(defrecord
    ^{:doc
      "The description of the content of an item whose current content
       is not yet represented by an explicitly stored item, but only
       by an atomic value. Adding an elaboration to this item will
       cause the content to be switched to an item."}
    ImplicitContentId
  [containing-item-id]

  StoredItemDescription

  (non-implicit-id [this] (non-implicit-id containing-item-id))

  (atom-description? [this] true)

  (stored-item-id-string [this]
    (clojure.string/join ["content-"
                          (stored-item-id-string containing-item-id)]))

  (stored-item-description-name [this]
    (clojure.string/join ["content-"
                          (stored-item-description-name containing-item-id)])))

(defn ensure-in-vector [v item]
  (cond (nil? v) [item]
        (some #{item} v) v
        :else (conj v item)))

(defn remove-from-vector [v item]
  (when v
    (let [result (filterv #(not (#{item} %)) v)]
      (when (not (empty? result))
        result))))

;;; A psuedo-set is either nil, a non-nil atom, or a set, representing
;;; either the empty set, a singleton item, or a set with multiple items.
;;; TODO: If the set is a small, use a vector?
(defn psuedo-set-seq [psuedo-set]
  (cond (nil? psuedo-set)
        nil
        (set? psuedo-set)
        (seq psuedo-set)
        true
        (seq [psuedo-set])))

(defn psuedo-set-contains? [psuedo-set item]
  (cond (nil? psuedo-set)
        false
        (set? psuedo-set)
        (contains? psuedo-set item)
        true
        (= psuedo-set item)))

(defn psuedo-set-conj [psuedo-set item]
  (cond (nil? psuedo-set)
        item
        (set? psuedo-set)
        (conj psuedo-set item)
        true
        #{psuedo-set item}))

(defn psuedo-set-disj [psuedo-set item]
  (cond (nil? psuedo-set)
        nil
        (set? psuedo-set)
        (let [result (disj psuedo-set item)]
          (cond (empty? result)
                nil
                (= (count result) 1)
                (first result)
                true
                result))
        (= item psuedo-set)
        nil
        true
        psuedo-set))

;;; Chase contents until an atomic value is found.
(defn atomic-value [store description]
  (if (satisfies? StoredItemDescription description)
    (atomic-value store (id->content store description))
    description))

(defn index-subject
  "Do indexing to reflect the effect of the item on its subject.
   The subject must not be implicit."
  [store id]
  (let [subject (get-in store [:id->subject id])
        subject-of-subject (get-in store [:id->subject subject])
        atomic-value (atomic-value store id)]
    (if (or (nil? subject)
            (nil? subject-of-subject)
            (nil? atomic-value))
      store
      (-> store
          (update-in-clean-up
           [:subject->label->ids subject-of-subject nil]
           #(remove-from-vector % subject))
          (update-in
           [:subject->label->ids
            subject-of-subject (canonical-atom-form atomic-value)]
           #(ensure-in-vector % subject))))))

(defn index-in-subject
  "Record this item as an element of its subject."
    [store id]
    (let [subject (get-in store [:id->subject id])]
      (if (nil? subject)
        store
        (update-in store [:subject->label->ids subject nil]
                   #(ensure-in-vector % id)))))

(defn index-content
  "Do indexing to reflect the item having the content.
  The content must not be implicit."
  [store id]
  (let [content (get-in store [:id->content-map id])]
    (update-in store [:content->ids (canonical-atom-form content)]
               #(psuedo-set-conj % id))))

(defn deindex-subject
  "Undo indexing to reflect the effect of the item on its subject.
   The subject must not be implicit and the item must have no elements."    
  [store id]
  (let [subject (get-in store [:id->subject id])
        subject-of-subject (get-in store [:id->subject subject])
        label (atomic-value store id)]
    (if (or (nil? subject)
            (nil? subject-of-subject) ; We didn't add a label.
            (nil? atomic-value) ; We didn't add a label.
            (some #(= (atomic-value store %) label) ; Redundant label.
                  (remove #{id} (id->element-ids store subject))))
      store ; We have no effect on the index
      (-> (if (some #(not (nil? (atomic-value store %)))
                    (remove #{id} (id->element-ids store subject)))
            store
            ;; There are no other labels for our subject, so put
            ;; it under nil.
            (update-in store
                       [:subject->label->ids subject-of-subject nil]
                       #(ensure-in-vector % subject)))
          (update-in-clean-up
           [:subject->label->ids subject-of-subject (canonical-atom-form label)]
           #(remove-from-vector % subject))))))

(defn deindex-in-subject
  "Remove the item, which must have no elements, as an element of its subject."
  [store id]
  (let [subject (get-in store [:id->subject id])]
    (if (nil? subject)
      store ; We have no effect on the index
       ;; Since we have no elements, we should have been indexed
       ;; under nil.      
      (update-in-clean-up store [:subject->label->ids subject nil]
                          #(remove-from-vector % id)))))

(defn deindex-content
  "Undo indexing to reflect the item having the content.
  The content must not be implicit."
  [store id]
  (let [content (get-in store [:id->content-map id])]
    (update-in-clean-up store [:content->ids (canonical-atom-form content)]
                        #(psuedo-set-disj % id))))

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
    (reduce (fn [store, subject] (add-modified-id store subject))
            store (psuedo-set-seq (:containing-ids store id)))
    store))

(defn add-triple
  "Add a triple to the store, and do all necessary indexing."
  [store item-id subject content]
  (assert (not (nil? content)))
  (let [data (if (nil? subject)
               {:content content}
               {:subject subject :content content})]
    (-> (if (nil? subject)
          store
          (assoc-in store [:id->subject item-id] subject))
        (assoc-in [:id->content-map item-id] content)
        (index-subject item-id)
        (index-in-subject item-id)
        (index-content item-id)
        (add-modified-id item-id))))

(defn promote-implicit-item [store id]
  (if (instance? ImplicitContentId id)
     (let [container (:containing-item-id id)
           content (id->content store container)]
       (if (instance? ItemId content)
         [store content]
         (let [[promoted-store promoted-container]
               (promote-implicit-item store container)
               content-id
               (->ItemId (:next-id promoted-store))]
           [(-> promoted-store
                (update-in [:next-id] inc)
                (add-triple content-id nil content)
                (update-content promoted-container content-id))
            content-id])))
     [store id]))

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
  ;; indexed under what it is waiting for, then add it when we see what
  ;; it needs.
  ;; Return the new store and new deferred.
  [store deferred id subject content]
  (let [needed (first (filter #(and (instance? ItemId %)
                                    (not ((:id->content-map store) %)))
                              [subject content]))]
    (if needed
      [store (update-in deferred [needed]
                        #(conj % [id subject content]))]
      (reduce (fn [[store deferred] [id subject content]]
                (add-or-defer-triple store deferred id subject content))
              [(add-triple store id subject content)
               (dissoc deferred id)]
              (deferred id)))))

(defn eventually-containing-items
  "Return all items that contain the content, possibly through
   a chain of containment."
  [store content]
  (let [items (psuedo-set-seq
               (get-in store [:content->ids (canonical-atom-form content)]))]
    (concat items (mapcat #(eventually-containing-items store %) items))))

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
    subsume all possible matches."
  [store template]
  ;; We can't use the entity code to extract the content and elements,
  ;; because that code depends on this file.
  (let [[content elements] (if (seq? template)
                             [(first template) (rest template)]
                             [template nil])
        element-matches (subsuming-elements-ids-and-estimates store elements)]
    (if (nil? content)
      element-matches
      (let [content-ids (eventually-containing-items store content)]
        (concat [[(count content-ids) content-ids]]
                element-matches)))))

;;; TODO: If the estimate for an element is too large, but the element
;;;       has a label, filter with subject->label->ids
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
         (lazy-seq (->> possibilities
                        (filter #(<= (first %) threshold))
                        (map second)
                        (map set)
                        (apply clojure.set/intersection)))]))))

(defrecord ElementStoreImpl
    ^{:doc
      "An immutable store that has only enough indexing
       to find elements of items."}
   [
   ;;; These two maps give the facts about the store's triples.
   ;;; Map from ItemId to its subject
   id->subject
   ;;; Map from ItemId to its content
   ;;; We have to call it id->content-map, to avoid conflicting with
   ;;; the method id->content
   id->content-map

   ;;; A derived index from content to a psuedo-set of ids with that content.
   content->ids

   ;;; A set of ids that have been declared temporary.
   temporary-ids

   ;;; Index from item then label to all elements that belong to the
   ;;; item and have the label.
   ;;; Subject must be an id, while label can be an id or a primitives.
   subject->label->ids

   ;;; The next id to assign to an item to be stored here.
   next-id

   ;;; A set of ids that have been updated since the last call to
   ;;; clear-modified-ids. This is only present if track-modified-ids
   ;;; has been called.
   modified-ids

   ;;; Whether this store state is a valid undo point. (Starts out true.)
   valid-undo-point
   ]

  Store

  (id-valid? [this id]
    (contains? (:id->content-map this) id))

  (id-label->element-ids [this id label]
    (get-in this [:subject->label->ids id (canonical-atom-form label)]))

  (id->element-ids [this id]
    (seq (distinct (apply concat
                          (vals (get-in this [:subject->label->ids id]))))))

  (id->content [this id]
    (cond (instance? ImplicitContentId id)
          (id->content this (id->content this (:containing-item-id id)))
          (instance? ItemId id)
          (get-in this [:id->content-map id])
          :else
          id))

  (id->content-reference [this id]
    (let [content (id->content this id)]
      (if (instance? ItemId content)
        content
        (->ImplicitContentId id))))

  (candidate-matching-ids [this template]
    (let [pair (candidate-matching-ids-and-estimate this template)]
      (if (nil? pair)
        ;; The template is so generic that none of our indices can narrow
        ;; it down based on any of its contents. Return basically everything.
        (if (and (sequential? template) (seq (rest template)))
          ;; The item has an element.
          ;; Return all items that have elaborations.
          (keys subject->label->ids)
          (keys id->content-map))
        (second pair))))

  (mutable-store? [this] false)

  ElementStore

  (id->subject [this id]
    (when
      (instance? ItemId id)
      (get-in this [:id->subject id])))
  
  ImmutableStore

  (id-is-content? [this id exceptions]
    (not-every? (set exceptions)
                (psuedo-set-seq (get-in this [:content->ids id]))))

  (add-simple-element [this subject content]
    (assert (not (nil? content)))
    (let [[promoted1 revised-subject] (promote-implicit-item this subject)
          [promoted revised-content] (promote-implicit-item promoted1 content)]
      (let [item-id (->ItemId (:next-id promoted))]
        [(-> promoted
             (update-in [:next-id] inc)
             (add-triple item-id revised-subject revised-content))
         item-id])))

  (remove-simple-id [this id]
    (assert (not (nil? (get-in this [:id->content-map id])))
            "Removed id not present.")
    (assert (nil? (get-in this [:subject->label->ids id]))
            "Removed id has elements.")
    (-> this
        (deindex-subject id)
        (deindex-in-subject id)
        (deindex-content id)
        (dissoc-in [:id->content-map id])
        (dissoc-in [:id->subject id])
        (add-modified-id id)))

  (update-content [this id content]
    (assert (not (nil? content)))
    (-> this
        (deindex-subject id)
        (deindex-content id)
        (assoc-in [:id->content-map id] content)
        (index-subject id)
        (index-content id)
        (add-modified-ids-for-id-and-containers id)))

  (get-unique-number [this]
    [(:next-id this) (update-in this [:next-id] inc)])

  (track-modified-ids [this]
    (assoc this :modified-ids #{}))

  (valid-undo-point? [this]
    (:valid-undo-point this))

  (update-valid-undo-point [this valid]
    (assoc this :valid-undo-point valid))

  (fetch-and-clear-modified-ids [this]
    [(assoc this :modified-ids #{})
     (:modified-ids this)])

  (declare-temporary-id [this id]
    (assert (:id->content-map this))
    (update this :temporary-ids #(conj % id)))

  (store-to-data [this]
    "Extract just the essential data from the store, in preparation
     for writing it out. If the content of an item is an id, it is
     represented as [:id <ItemId>] and if it is a Orderable, it is represented
     as [:ord <left> <right>]."
    (let [temporary-ids (all-temporary-ids this)]
      [(:next-id this)
       (for [[id content]
             (seq (:id->content-map this))
             :when (not (temporary-ids id))]
         [(:id id)
          (:id (get-in this [:id->subject id]))
          (cond (instance? ItemId content)
                [:id (:id content)]
                (instance? cosheet.orderable.Orderable content)
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
                          :id->content-map {}
                          :content->ids {}
                          :temporary-ids #{}
                          :subject->label->ids {}
                          :next-id 0
                          :modified-ids nil
                          :valid-undo-point true}))

(defmethod make-id true [id]
  ;;Integers are reserved for the store
  (assert (not (integer? id)))
  (->ItemId id))

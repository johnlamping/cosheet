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

  (atom-description? [this] false)

  (stored-item-id-string [this] (str id))

  (stored-item-description-name [this] (clojure.string/join ["Id-" id])))

(defrecord
    ^{:doc
      "The description of the content of an item whose current content
       is not yet represented by an explicitly stored item, but only
       by an atomic value. Adding an elaboration to this item will
       cause the content to be switched to an item."}
    ImplicitContentId
  [containing-item-id]

  StoredItemDescription

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

;;; Chase contents until an atomic value is found.
(defn atomic-value [store description]
  (if (satisfies? StoredItemDescription description)
    (atomic-value store (id->content store description))
    description))

(defn index-subject
  "Do indexing to reflect the effect of the item on its subject.
   The subject must not be implicit."
  [store id]
  (let [subject (get-in store [:id->data id :subject])
        subject-of-subject (get-in store [:id->data subject :subject])
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
           [:subject->label->ids subject-of-subject atomic-value]
           #(ensure-in-vector % subject))))))

(defn index-in-subject
  "Record this item as an element of its subject."
    [store id]
    (let [subject (get-in store [:id->data id :subject])]
      (if (nil? subject)
        store
        (update-in store [:subject->label->ids subject nil]
                   #(ensure-in-vector % id)))))

(defn index-content
  "Do indexing to reflect the item having the content.
  The content must not be implicit."
  [store id]
  (let [content (get-in store [:id->data id :content])]
    (if (instance? ItemId content)
      (do
        (assert (contains? (:id->data store) content) "Content item unknown.")
        (update-in store [:id->data content :containers]
                   #((fnil conj #{}) % id)))
      store)))

(defn deindex-subject
  "Undo indexing to reflect the effect of the item on its subject.
   The subject must not be implicit and the item must have no elements."    
  [store id]
  (let [subject (:subject (get-in store [:id->data id]))
        subject-of-subject (get-in store [:id->data subject :subject])
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
           [:subject->label->ids subject-of-subject label]
           #(remove-from-vector % subject))))))

(defn deindex-in-subject
  "Remove the item, which must have no elements, as an element of its subject."
  [store id]
  (let [subject (:subject (get-in store [:id->data id]))]
    (if (nil? subject)
      store ; We have no effect on the index
       ;; Since we have no elements, we should have been indexed
       ;; under nil.      
      (update-in-clean-up store [:subject->label->ids subject nil]
                          #(remove-from-vector % id)))))

(defn index-content
  "Do indexing to reflect the item having the content.
  The content must not be implicit."
  [store id]
  (let [content (get-in store [:id->data id :content])]
    (if (instance? ItemId content)
      (do
        (assert (contains? (:id->data store) content) "Content item unknown.")
        (update-in store [:id->data content :containers]
                   #((fnil conj #{}) % id)))
      store)))

(defn deindex-content
  "Undo indexing to reflect the item having the content.
  The content must not be implicit."
  [store id]
  (let [content (get-in store [:id->data id :content])]
    (if (instance? ItemId content)
      (update-in-clean-up store [:id->data content :containers] #(disj % id))
      store))  )

(defn add-modified-id
  "Add the id to the modified id set of the store,
  if we are tracking modified ids."
  [store id]
  (if (:modified-ids store)
    (update-in store [:modified-ids] #(conj % id))
    store))

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
                (assoc-in [:id->data content-id]
                          {:content content :containers #{promoted-container}})
                (assoc-in [:id->data promoted-container :content] content-id)
                (add-modified-id content-id))
            content-id])))
     [store id]))

(defn add-triple
  "Add a triple to the store, and do all necessary indexing."
  [store item-id subject content]
  (assert (not (nil? content)))
  (let [data (if (nil? subject)
               {:content content}
               {:subject subject :content content})]
    (-> store
         (assoc-in [:id->data item-id] data)
         (index-subject item-id)
         (index-in-subject item-id)
         (index-content item-id)
         (add-modified-id item-id))))

(defn add-or-defer-triple
  ;; Utility function for read-store.
  ;; The triples may have been written out in any order, but we cannot
  ;; add a triple until its subject has been added (and after its
  ;; content has been added, if the content is an id). When we encounter
  ;; a triple that can't yet added, we save it in deferred,
  ;; indexed under what it is waiting for, then add it when we see what
  ;; it needs.
  ;; Return the new store and new deferred
  [store deferred id subject content]
  (let [needed (first (filter #(and (instance? ItemId %)
                                    (not ((:id->data store) %)))
                              [subject content]))]
    (if needed
      [store (update-in deferred [needed]
                        #(conj % [id subject content]))]
      (reduce (fn [[store deferred] [id subject content]]
                (add-or-defer-triple store deferred id subject content))
              [(add-triple store id subject content)
               (dissoc deferred id)]
              (deferred id)))))

(defrecord ElementStoreImpl
    ^{:doc
      "An immutable store that has only enough indexing
       to find elements of items."}

  [;;; A map from ItemId to a map of their
   ;;;   :content, the content of the item.
   ;;;   :subject, the item this item is an element of.
   ;;;   :containers, a set of the items that this item is the content
   ;;;   of. This is redundant information.
   id->data
   
   ;;; Index from item then label to all elements that belong to the
   ;;; item and have the label.
   ;;; Subject and label can be ids or primitives. (Or maybe subject
   ;;; must be an id.)
   subject->label->ids

   ;;; The next id to assign to an item to be stored here
   next-id

   ;;; A set of ids that have been updated since the last call to
   ;;; clear-modified-ids. This is only present if track-modified-ids
   ;;; has been called.
   modified-ids]

  Store

  (id-valid? [this id]
    (contains? (:id->data this) id))

  (id-label->element-ids [this id label]
    (get-in this [:subject->label->ids id label]))

  (id->element-ids [this id]
    (seq (distinct (apply concat
                          (vals (get-in this [:subject->label->ids id]))))))

  (id->content [this id]
    (cond (instance? ImplicitContentId id)
          (id->content this (id->content this (:containing-item-id id)))
          (instance? ItemId id)
          (get-in this [:id->data id :content])
          :else
          id))

  (id->content-reference [this id]
    (let [content (id->content this id)]
      (if (instance? ItemId content)
        content
        (->ImplicitContentId id))))

  ;;; TODO: make this actually filter based on the item.
  ;;; Notice that as currently passed by
  ;;; query-impl/item-matches-in-store, it could be a mutable entity.
  (candidate-matching-ids [this item]
    ;; Return all items that have elaborations.
    (keys subject->label->ids))

  (mutable-store? [this] false)

  ElementStore

  (id->subject [this id]
    (if (instance? ItemId id)
      (get-in this [:id->data id :subject])
      nil))

  (id-is-content? [this id exceptions]
    (not-every? (set exceptions) (get-in this [:id->data id :containers])))
  
  ImmutableStore

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
    (assert (not (nil? (get-in this [:id->data id])))
            "Removed id not present.")
    (assert (nil? (get-in this [:subject->label->ids id]))
            "Removed id has elements.")
    (-> this
        (deindex-subject id)
        (deindex-in-subject id)
        (deindex-content id)
        (dissoc-in [:id->data id])
        (add-modified-id id)))

  (update-content [this id content]
    (assert (not (nil? content)))
    (-> this
        (deindex-subject id)
        (deindex-content id)
        (assoc-in [:id->data id :content] content)
        (index-subject id)
        (index-content id)
        (add-modified-id id)))

  (track-modified-ids [this]
    (assoc this :modified-ids #{}))

  (fetch-and-clear-modified-ids [this]
    [(assoc this :modified-ids #{})
     (:modified-ids this)])

  (write-store [this stream]
    (binding [*out* stream]
      (prn [(:next-id this)
            (for [[id {:keys [subject content] :or {subject nil}}]
                  (seq (:id->data this))]
              [(:id id)
               (:id subject)
               (cond (instance? ItemId content)
                     [:id (:id content)]
                     (instance? cosheet.orderable.Orderable content)
                     [:ord (:left content) (:right content)]
                     true
                     content)])])))

  (read-store [this stream]
    (binding [*in* stream]
      (let [[next-id items] (clojure.edn/read stream)]
        (let [[store deferred]
              (reduce (fn [[store deferred] [id subject content]]
                   (let [id (->ItemId id)
                         subject (when subject (->ItemId subject))
                         content (if (vector? content)
                                   (apply (case (first content)
                                            :id ->ItemId
                                            :ord ->Orderable)
                                          (rest content))
                                   content)]
                     (add-or-defer-triple store deferred id subject content)))
                 [(assoc (new-element-store) :next-id next-id) {}]
                 items)]
          (assert (empty? deferred))
          store)))))

(defmethod print-method ElementStoreImpl [s ^java.io.Writer w]
  (.write w "ElementStore"))

(defmethod new-element-store true []
  (map->ElementStoreImpl {:id->data {}
                          :subject->label->ids {}
                          :next-id 0
                          :modified-ids nil}))

(defmethod make-id true [id]
  ;;Integers are reserved for the store
  (assert (not (integer? id)))
  (->ItemId id))

(defn get-unique-id-number
  "Return an id number and an updated store
  that will never return that number again."
  [store]
  [(:next-id store) (update-in store [:next-id] inc)])

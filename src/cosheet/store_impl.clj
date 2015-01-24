(ns cosheet.store-impl
  (:require (cosheet [store :refer :all]
                     [utils :refer :all])))

;;; Data in a store is in terms of ItemId objects. All that the system
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

  (atom-description? [this] false))

(defrecord
    ^{:doc
      "The description of the content of an item whose current content
       is not yet represented by an explicitly stored item, but only
       by an atomic value. Adding an elaboration to this item will
       cause the content to be switched to an item."}
    ImplicitContentId
  [containing-item-id]

  StoredItemDescription

  (atom-description? [this] true))

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
                (assoc-in [:id->data promoted-container :content] content-id))
            content-id])))
     [store id]))

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
   next-id]

  Store

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
      (let [data (if (nil? revised-subject)
                   {:content content}
                   {:subject revised-subject :content content})
            item-id (->ItemId (:next-id promoted))]
        [(-> promoted
             (update-in [:next-id] inc)
             (assoc-in [:id->data item-id] data)
             (index-subject item-id)
             (index-in-subject item-id)
             (index-content item-id))
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
        (dissoc-in [:id->data id])))

  (update-content [this id content]
    (assert (not (nil? content)))
    (-> this
        (deindex-subject id)
        (deindex-content id)
        (assoc-in [:id->data id :content] content)
        (index-subject id)
        (index-content id))))

(defmethod new-element-store true []
  (map->ElementStoreImpl {:id->data {} :subject->label->ids {} :next-id 0}))

(defmethod make-id true [id]
  ;;Integers are reserved for the store
  (assert (not (integer? id)))
  (->ItemId id))

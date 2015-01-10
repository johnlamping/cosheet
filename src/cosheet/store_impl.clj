(ns cosheet.store-impl
  (:require (cosheet [store :refer :all]
                     [item-store :refer :all]
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
        subject-of-subject
        (get-in store [:id->data subject :subject])
        atomic-value (atomic-value store id)]
    (if (nil? subject)
      store
      (-> ;; Index the id for which this id provides a new label.
          (if (or (nil? subject-of-subject)
                  (nil? atomic-value))
            store
            (-> store
                (update-in
                 [:subject->label->ids]
                 (fn [sli]
                   (update-in-clean-up
                    sli [subject-of-subject nil]
                    #(remove-from-vector % subject))))
                (update-in
                 [:subject->label->ids subject-of-subject atomic-value]
                 #(ensure-in-vector % subject))))
          ;; Index the new id.
         (update-in [:subject->label->ids subject nil]
                    #(ensure-in-vector % id))))))

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
   The subject must not be implicit."    
  [store id]
  (let [subject (:subject (get-in store [:id->data id]))
        subject-of-subject
        (get-in store [:id->data subject :subject])
        label (atomic-value store id)]
    (if (nil? subject)
      store
      (-> ;; Remove indexing for the label provided by this item.
       (if (or (nil? subject-of-subject)
               (nil? atomic-value))
            store ; We have no effect on the index
            (if (some #(and (not= % id)
                            (= (atomic-value store %) label))
                      (id->element-ids store subject))
              store ; Another element contributes the same label.
              (-> (update-in
                   store [:subject->label->ids]
                   (fn [sli] (update-in-clean-up
                              [subject-of-subject label]))
                             #(remove-from-vector % subject))
                  ((fn [updated]
                     (if (some #(and (not= % id)
                                     (not (nil? (atomic-value updated %))))
                               (id->element-ids store subject))
                       updated
                       ;; There are no other labels for our subject.
                       (update-in updated
                                  [:subject->label->ids subject-of-subject nil]
                                  #(ensure-in-vector % nil subject))))))))
       ;; Remove this item from the subject
       (update-in [:subject->label->ids]
                  (fn [sli] (update-in-clean-up
                             [ subject nil]
                             #(remove-from-vector % id))))))))

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

(defrecord ElementStore
    ^{:doc
      "A store that has only enough indexing to find elements of items."}

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

  StoreForItem

  (id-label->element-ids [this id label]
    (get-in this [:subject->label->ids id label]))

  (id->element-ids [this id]
    (seq (distinct (apply concat
                          (vals (get-in this [:subject->label->ids id]))))))

  (id->content [this id]
    (cond (instance? ImplicitContentId id)
          (id->content this (:containing-item-id id))
          (instance? ItemId id)
          (get-in this [:id->data id :content])
          :else
          id))

  (id->content-reference [this id]
    (let [content (id->content this id)]
      (if (instance? ItemId content)
        content
        (->ImplicitContentId id))))

  BasicStore

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
             (index-content item-id))
         item-id])))

  (remove-id [this id]
    (let [data (get-in this [:id->data id])
          subject (:subject data)
          value (atomic-value this id)
          removed (update-in this [:id->data] #(dissoc % id))]
      (assert (not (nil? data)) "Removed id not present.")
      (if (and subject value data)
        (update-in removed [:subject->label->ids subject value]
                   ))))
  ;;; TODO: make this actually filter based on the item.
  (candidate-matching-ids [this item]
    ;; Return all items that have elaborations.
    (keys subject->label->ids))

  (mutable-store? [this] nil))

(defmethod new-element-store true []
  (->ElementStore {} {} 0))

(defmethod make-id true [id]
  ;;Integers are reserved for the store
  (assert (not (integer? id)))
  (->ItemId id))

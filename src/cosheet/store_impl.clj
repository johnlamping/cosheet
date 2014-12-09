(ns cosheet.store-impl
  (:require [cosheet.store :refer :all]
            [cosheet.item-store :refer :all]))

;;; Information about the elements of an item is stored in other
;;; items. The only information stored directly with an item is its 
;;; ItemId, and the store keeps track of which way the item is stored.

;;; There is a third way to describe items, which doesn't store them.
;;; These are simple elements that correspond to elements of items
;;; that describe other simple elements. These aren't usually stored,
;;; but references to them are generated when a request comes in for
;;; the elements of an item whose description is not stored. These
;;; elements are described by ImplicitElementId, which specified
;;; an item and the label of one of its elements. Any operations to
;;; the database referencing an implicit item will cause the database
;;; to reify it into a described item.

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
  (cond (= v nil) [item]
        (some #{item} v) v
        :else (conj v item)))

(defn remove-from-key-vector [map key item]
  (if map
    (let [new-vec (filterv #(not (#{item} %)) (map key))]
      (if (= new-vec [])
        (dissoc map key)
        (assoc map key new-vec)))
    {}))

(defn non-nil-seq [x] (if (nil? x) nil [x]))

;;; Chase contents until an atomic value is found.
(defn atomic-value [store description]
  (if (satisfies? StoredItemDescription description)
    (atomic-value store (id->content store description))
    description))

(defn index-new-element [store id]
  (assert (instance? ItemId id))
  (let [subject (get-in store [:id->data id :subject])]
    (if (not (nil? subject))
      (let [subject-indexed
            (let [subject-of-subject
                  (get-in store [:id->data subject :subject])
                  atomic-value (atomic-value store id)]
              (if (and (not (nil? subject-of-subject)) (not (nil? atomic-value)))
                (-> store
                    (update-in
                     [:subject->label->ids subject-of-subject]
                     #(remove-from-key-vector % nil subject))
                    (update-in
                     [:subject->label->ids subject-of-subject atomic-value]
                     #(ensure-in-vector % subject)))
                store))]
        (update-in subject-indexed [:subject->label->ids subject nil]
                   #(ensure-in-vector % id)))
      store)))

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
                          {:content content :container promoted-container})
                (assoc-in [:id->data promoted-container :content] content-id))
            content-id])))
     [store id]))

(defrecord ElementStore
    ^{:doc
      "A store that has only enough indexing to find elements of items."}

  [;;; Map from subject then label to all elements that belong
   ;;; to the subject and have the label.
   ;;; Subject and label can be ids or primitives. Subject must be a
   ;;; described id, not one of the ids in id->simple-element
   subject->label->ids

   ;;; A map from ItemId to a map of their content, subject, and container.
   id->data
   
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

  (add-simple-element [store subject content]
    (assert (not (nil? content)))
    (let [[promoted revised-subject] (promote-implicit-item store subject)]
      (let [data (if (nil? revised-subject)
                   {:content content}
                   {:subject revised-subject :content content})
            item-id (->ItemId (:next-id promoted))]
        [(-> promoted
             (update-in [:next-id] inc)
             (assoc-in [:id->data item-id] data)
             (index-new-element item-id))
         item-id])))

  ;;; TODO: make this actually filter based on the item.
  (candidate-ids [this item]
    (keys subject->label->ids)))

(defmethod new-element-store true []
  (->ElementStore {} {} 0))

(defmethod make-id true [id]
  ;;Integers are reserved for the store
  (assert (not (integer? id)))
  (->ItemId id))

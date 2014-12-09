(ns cosheet.item-store)

;;; The methods necessary for collaboration between items and stores.

(defprotocol StoreForItem
  "The methods that Item and Entity rely on stores having.
   These take and return descriptions."
  
  (id-label->element-ids [this id label]
    "Returns a seq the ids of all ids that have the given subject and label.")

  (id->element-ids [this id]
    "Returns a seq of all ids that have the id as a subject.")

  (id->content [this id]
    "Given the id of an element, return a description of its content.")

  (id->content-reference [this id]
    "Given an item, return a reference to its content."))





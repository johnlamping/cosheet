(ns cosheet.server.render
  (:require (cosheet [query :refer [matching-elements]]
                     [debug :refer [simplify-for-print]]
                     [entity :refer [subject label->elements]]
                     [expression :refer [expr expr-let expr-seq cache]]
                     [expression-manager :refer [current-value]]
                     [dom-utils :refer [add-attributes into-attributes]]
                     [query :refer [matching-elements]])
            (cosheet.server 
             [referent :refer [item-referent referent->exemplar-and-subject]]
             [item-render :refer [item-without-labels-DOM-R
                                  item-DOM-R must-show-label-item-DOM-R]]
             [table-render :refer [table-DOM-R]])))

;;; Code to create hiccup style dom for a database entity.

;;; In the following, as well as in other parts of the server code,
;;; most functions don't take reporters as arguments, but may return
;;; reporters. Their names have a suffix of -R.

;;; For a basic entity, we show its contents and its semantic
;;; elements. We don't show its non-semantic elements, which are
;;; identified by, themselves, having :non-semantic elements.  An
;;; element may be marked as a tag by having an element whose content
;;; is :tag. This make that element displayed like a tag, so the :tag
;;; mark is semantic.
;;; So, for example, the entity:
;;;    ("Joe"
;;;        ("married" (1 :order :non-semantic)
;;;        (39 (2 :order :non-semantic)
;;;            ("age" :tag)
;;;            "doubtful"))
;;; would be rendered to dom that tries to convey:
;;;   Joe
;;;     married
;;;     age: 39
;;;             doubtful

;;; We use attributes, as supported by hiccup, to store both html
;;; attributes, and additional attributes that are used by the server.
;;; There are removed by the dom manager before dom is sent to the client.
(def server-specific-attributes
  [      :key  ; A unique client side key (further described below).
      :target  ; The item (or virtual new item) that the dom refers to
               ; It is itself a map, with some of these keys
               ; :item-referent             Item(s) referred to
               ; :subject-referent          Subject(s) of the virtual item(s)
               ; :adjacent-referent         item(s) adjacent to new item(s)
               ; :adjacent-groups-referent  Groups of item(s) adjacent to
               ;                            new item(s)
               ; :position                  :before or :after item/adjacent
               ; :template                  Added item(s) should satisfy this.
               ; :parent-key                The key of the parent of a virtual
               ;                            new item.
               ; :select-pattern            The pattern to use to generate the
               ;                            key to select part of a new item.
               ;                            Will have at most one of this and
               ;                            :parent-key
     :sibling  ; A special target to use for add-sibling commands.
      :delete  ; A special target to use for deletion, if it should be
               ; different from the :target.
      :expand  ; A special target to use for expansion, if it should be
               ; different from the :target.
         :row  ; The row (or virtual new row) that the dom belongs to,
               ; a map with the same keys as :target
      :column  ; The analog of :row for a column.
   ])

;;; The value of the style attribute is represented with its own map,
;;; rather than as a string, so it can be accumulated. Conviently,
;;; reagent accepts that format too.

;;; We don't create the entire dom in one call, because we want to be
;;; able to reuse subsidiary parts of the dom that the client has,
;;; even if a containing level of the dom changes. For example, if the
;;; containing dom node adds a new child, we don't want to re-compute,
;;; or re-transmit its other children.

;;; Instead, we generate dom that has subsidiary components. These are
;;; specified as
;;;   [:component {:key <key>  (See below.)
;;;                <other attributes to add to the definition's
;;;                 result>}
;;;               definition}

;;; The dom_tracker code understands components. It will give
;;; the client a dom with these subsidiary components, with the initially
;;; provided attributes already present, and it will create additional
;;; computations to compute the dom for the components, passing them
;;; as updates to the client once they are computed.

;;; Each component is uniquely identified with a key, as is any other
;;; dom node that the user might interact with. (The dom for a
;;; component need not have a key; the component can add it when
;;; necessary.) There must never be two components or doms with the
;;; same key, even during updates, or all sorts of confusion can
;;; result. The key of a component must also not change throughout the
;;; life of its parent dom, because we keep a mapping between client
;;; dom ids and server keys, which will be broken if the key changes.

;;; The heart of a key is the id of the item the dom is about. But
;;; since there can be several dom nodes about same item, we need more
;;; than that. We thus use a sequence of the path of containment in
;;; the dom, with additional information added to the sequence if
;;; containment is not sufficient to fully disambiguate.

;;; Many DOM generating functions take a map argument, inherited, that
;;; gives information determined by their container. This includes:
(def starting_inherited
  {            :width 1.0  ; A float, giving the width of this dom element
                           ; compared to the minimum width for two column
                           ; format.
              :priority 0  ; How important it is to render this item earlier.
                           ; (Lower is more important.)
           :parent-key []  ; The key of the parent dom of the dom.
                           ; The parent might not actually have a key, if the
                           ; user can't interact with it, but this is a
                           ; that is unique to the parent, and can thus be
                           ; used to generate unique keys for its children.
;                :subject  ; The referent of the subject(s) of the item
                           ; the dom is about, if any. Only required to
                           ; be present if the item is an exemplar.
;               :template  ; The template that the twins of this dom,
                           ; must satisfy. If not present, then twins
                           ; may not be created.
;  :selectable-attributes  ; Attributes that the topmost selectable parts
                           ; of the dom should have, if any. Typically,
                           ; these are commands for things like new-row.
;      :element-attributes : Should become :selectable-attributes of elements.
;            :is-selector  ; If true, this dom represents a selector, which
                           ; means that new elements should get 'anything
                           ; rather than "", if they are part of the selector,
                           ; and not part of what is selected. When the
                           ; target referent is instantiated, the first group
                           ; items must be the selector.
;             :immmutable  ; If true, the content of this dom should never
                           ; change.
   })

(defn user-visible-item?
  "Return true if the item is user visible."
  ;; TODO: This will also need to check for selectors that the user
  ;;       should see, even though they don't have :order.
  [item]
  (let [order-elements (current-value (label->elements item :order))
        non-semantic (current-value (matching-elements :non-semantic item))]
    (and (seq order-elements) (empty? non-semantic))))

;;; --- Top level item ---

(defn top-level-item-DOM-R
  "Make a dom for an item, testing the item to see what sort of dom to make."
  [item referent inherited]
  (let [inherited (into {:priority 0 :width 1.5 :parent-key []} inherited)]
    (expr-let [table (matching-elements :table item)
               tags (matching-elements :tag item)]
      (if (empty? table)
        (let [subject-ref (or (:subject inherited)
                              (let [[exemplar subject-ref]
                                    (referent->exemplar-and-subject referent)]
                                (or subject-ref
                                    (when-let [subject (subject item)]
                                      (when (user-visible-item? subject)
                                        (item-referent subject))))))
              inherited (cond-> inherited
                          subject-ref
                          (update-in
                           [:selectable-attributes]
                           #(into-attributes
                             % {:expand {:item-referent subject-ref}})))
              dom ((if (empty? tags) must-show-label-item-DOM-R item-DOM-R)
                   item referent tags inherited)]
          (if (empty? tags)
            dom
            (expr-let [dom dom]
              (add-attributes dom {:class "tag"}))))
        (table-DOM-R item inherited)))))

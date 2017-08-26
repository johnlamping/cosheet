(ns cosheet.server.render
  (:require (cosheet [query :refer [matching-elements matching-items]]
                     [debug :refer [simplify-for-print]]
                     [store :refer [call-dependent-on-id id-valid?]]
                     [store-impl :refer [id->string string->id]]
                     [entity :refer [subject content label->elements
                                     description->entity]]
                     [expression :refer [expr expr-let expr-seq cache]]
                     [expression-manager :refer [current-value]]
                     [hiccup-utils :refer [add-attributes into-attributes]]
                     [state-map :refer [state-map-get]])
            (cosheet.server 
             [referent :refer [item-referent referent->exemplar-and-subject
                               item-referent? instantiate-to-items]]
             [model-utils :refer [ tabs-holder-item-R first-tab-R]]
             [render-utils :refer [make-component]]
             [item-render :refer [item-DOM-R must-show-label-item-DOM-R]]
             [table-render :refer [table-DOM-R]]
             [tabs-render :refer [tabs-DOM-R]]
             [batch-edit-render :refer [batch-edit-DOM-R]])))

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
               ; It is itself a map, with some of these keys:
               ; :special         A keyword indicating a special action
               ;                  to perform on commands.
               ; :referent        Item(s) referred to
               ; :select-pattern  The pattern to use to generate the key to
               ;                  select part of a new item.
               ; :alternate       If true, this target has an alternate
               ;                  interpretation, by narrowing the referent.
               ;                  :selector-category must be present.
               ;                  If an atom, that atom overrides
               ;                  :selector-category.
 :add-sibling  ; A special target to use for add-sibling commands.
      :delete  ; A special target to use for deletion, if it should be
               ; different from the :target.
      :expand  ; A special target to use for expansion, if it should be
               ; different from the :target.
     :add-row  ; A special target to use for add-row commands.
  :add-column  ; The analog of :row for a column.
   :selector-category  ; If this dom is a selector, this field
                       ; will be present,  and hold an atom characterizing
                       ; the kind of selector.
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
(def starting-inherited
  {            :width 1.5  ; A float, giving the width of this dom element
                           ; compared to the minimum width for two column
                           ; format.
              :priority 0  ; How important it is to render this item earlier.
                           ; (Lower is more important.)
           :key-prefix []  ; A prefix for the keys the dom that will make them
                           ; distinct from other keys. Typically reflects the
                           ; containment path in the dom.
;       :subject-referent  ; The referent of the subject(s) of the item
                           ; the dom is about, if any. Only required to
                           ; be present if the item is an exemplar.
;               :template  ; The template that the twins of this dom
                           ; must satisfy. If not present, then twins
                           ; may not be created.
;             :attributes  ; A set of attribute descriptions that parts
                           ; of the dom should have. Typically,
                           ; these are targets like row. See the comment in
                           ; render_utils for the exact format.
;       :selector-category ; In addition to giving the category of a selector,
                           ; this means that new elements should get 'anything
                           ; rather than "", if they are part of the selector,
                           ; and not part of what is selected. When the
                           ; target referent is instantiated, the first group
                           ; items must be the selector.
;        :alternate-target ; If true, there is an alternate interpretation
                           ; of the item. :selector-category must be present.
   })
;;; In some cases, the inherited information is halfway between being about
;;; an item and its children. In this case, :template and :attributes are
;;; about the item, while :key-prefix and :subject-referent are about
;;; the children.

(defn key->string
  "Return a string representation of the key that can be passed to the client."
  [key]
  (clojure.string/join
   "_"
   (map #(cond (keyword? %) (str ":" (name %))
               (item-referent? %) (id->string %)
               true (assert false (str "unknown key component " %)))
        key)))

(defn string->key
  "Given a string representation of a key, return the key."
  [rep]
  (vec (map
        #(if (= (first %) \:)
           (keyword (subs % 1))
           (string->id %))
        (clojure.string/split rep #"_"))))

(defn user-visible-item?
  "Return true if the item is user visible."
  [item]
  (let [order-elements (current-value (label->elements item :order))
        non-semantic (current-value (matching-elements :non-semantic item))
        row-condition (current-value (matching-elements :row-condition item))]
    (and (or (seq order-elements) (seq row-condition))
         (empty? non-semantic))))

;;; --- Top level item ---

(defn top-level-item-DOM-R
  "Make a dom for an item, testing the item to see what sort of dom to make."
  [item referent inherited]
  (let [inherited (into starting-inherited inherited)]
    (expr-let [table (matching-elements :table item)
               tags (matching-elements :tag item)]
      (if (empty? table)
        (let [subject-ref (or (:subject-referent inherited)
                              (let [[exemplar subject-ref]
                                    (referent->exemplar-and-subject referent)]
                                (or subject-ref
                                    (when-let [subject (subject item)]
                                      (when (user-visible-item? subject)
                                        (item-referent subject))))))
              inherited (cond-> inherited
                          subject-ref
                          (update
                           :attributes
                           #(conj (or % [])
                                  [#{:label :optional} #{:content}
                                   {:expand {:referent subject-ref}}])))
              dom ((if (empty? tags) must-show-label-item-DOM-R item-DOM-R)
                   item referent tags inherited)]
          (expr-let [dom dom]
            (cond-> dom
              (seq tags)
              (add-attributes {:class "tag"})
              (:selector-category inherited)
              (add-attributes {:class "selectors"}))))
        (table-DOM-R item inherited)))))

(defn label-datalist-DOM-R
  "Return dom for a datalist of the content of all labels."
  [store]
  ;; TODO: This reruns anytime anything changes. Put support for gathering all
  ;;       values in the store, so it can be more efficient by looking at
  ;;       the changed items. (The store needs a way to tell a reporter
  ;;       about which items changed, not just that something it cared about
  ;;       changed.
  (expr-let [labels (matching-items '(nil :tag) store)
             contents (expr-seq map content labels)]
    (let [content-names (map str contents)
          sorted-contents (sort (vals (zipmap (map clojure.string/lower-case
                                                   content-names)
                                              content-names)))]
      (into [:datalist] (map (fn [name] [:option name]) sorted-contents)))))

;;; TODO: Add a unit test for this.
(defn top-level-DOM-R
  [store transient-id client-state selector-category]
  (expr-let [batch-editing (state-map-get client-state :batch-editing)]
    (if batch-editing
      (let [transient-item (description->entity transient-id store)]
        (expr-let [query (expr first (label->elements transient-item :query))
                   selected-id (state-map-get client-state
                                              :selected-batch-edit-id)
                   selected-valid (when selected-id
                                    (id-valid? store selected-id))]
          (batch-edit-DOM-R query
                            (when selected-valid
                              (description->entity selected-id store)))))
      (expr-let [referent (state-map-get client-state :referent)
             subject-referent (state-map-get client-state :subject-referent)
             immutable-item (call-dependent-on-id
                             store nil
                             (fn [immutable-store]
                               (or (when referent
                                     (first (instantiate-to-items
                                             referent immutable-store)))
                                   (first-tab-R immutable-store))))]
        (if immutable-item
          (let [item (description->entity (:item-id immutable-item) store)
                inherited (cond-> starting-inherited
                            subject-referent
                            (assoc :subject-referent subject-referent)
                            selector-category
                            (assoc :selector-category selector-category
                                   :alternate-target true))]
            (expr-let [tab-tags (matching-elements :tab item)
                       content (content item)]
              (if (empty? tab-tags)
                ;; Show just the item.
                (top-level-item-DOM-R item referent inherited)
                ;; Show a selection of tabs.
                (expr-let [topic (expr first (label->elements item :tab-topic))
                           subject (cosheet.entity/subject item)]
                  [:div {:class "tabbed"}
                   (make-component {:key [:tabs]}
                                   [tabs-DOM-R subject item inherited])
                   (make-component
                    {:key [:tab (:item-id topic)]}
                    [top-level-item-DOM-R topic nil
                     (assoc inherited :key-prefix [:tab])])]))))
          ;; Show a virtual tab.
          (do (println "showing virtual")
              (expr-let [holder (tabs-holder-item-R store)]
                [:div {:class "tabbed"}
                 (make-component
                  {:key [:tabs]}
                  [tabs-DOM-R holder nil starting-inherited])])))))))

(defn DOM-for-client-R
  "Return a reporter giving the DOM specified by the client."
  [store transient-id client-state selector-category]
  (expr-let [dom (top-level-DOM-R
                  store transient-id client-state selector-category)]
    (into dom [(make-component {:key [:label-values]}
                                [label-datalist-DOM-R store])])))

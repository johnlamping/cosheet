(ns cosheet2.server.render
  (:require (cosheet2 [query :refer [matching-elements matching-items]]
                      [debug :refer [simplify-for-print]]
                      [store :refer [id-valid? StoredItemDescription]]
                      [store-impl :refer [id->string string->id]]
                      [entity :refer [subject content label->elements
                                      description->entity]]
                      [expression :refer [expr expr-let expr-seq cache]]
                      [calculator :refer [current-value]]
                      [hiccup-utils :refer [add-attributes into-attributes]])
            (cosheet2.server
             [order-utils :refer [semantic-entity?]]
             [model-utils :refer [tabs-holder-id-R ordered-tabs-ids-R
                                  semantic-to-list]]
             ; [render-utils :refer [make-component]]
             ; [item-render :refer [item-DOM-R]]
             ; [table-render :refer [table-DOM-R]]
             ; [tabs-render :refer [tabs-DOM-R]]
             ; [Batch-edit-render :refer [batch-edit-DOM-R]]
             )))

;;; Code to create hiccup style dom for a database entity.

;;; For a basic entity, we show its contents and its semantic
;;; elements, but not its non-semantic elements. Semantic elements
;;; have a content of a number, a string, 'anything, 'anything-immutable,
;;; :label, or :category,

;;; An element may be marked as a label or a category by having an
;;; element whose content is :label or :category, respectively. It is
;;; because this affects how the element is displayed that these
;;; sub-elements are semantic.

;;; Every semantic element that is not :label or :category must have
;;; an :order sub-element, to indicate its display position relative
;;; to the other elements. And only semantic elements can have :order
;;; sub-elements.
;;; So, for example, the entity:
;;;    ("Joe"
;;;        ("married" (->Orderable 1 2) :order)
;;;        (39 ((->Orderable 5 6) :order)
;;;            ("age" :label ((->Orderable 7 8) :order))
;;;            ("doubtful" ((->Orderable 9 10) :order)))
;;; would be rendered to dom that tries to convey:
;;;   Joe
;;;     married
;;;     age: 39
;;;       doubtful

;;; The overall architecture of the system is a dom renderer that
;;; knows how to generate various kinds of dom to display parts of the
;;; store, and a dom-tracker that keeps track of what dom might need
;;; to be recomputed and what dom has changed and needs to be sent to
;;; the client.

;;; The dom is generated and sent to the client as a tree of
;;; components, each component possibly containing others. Each
;;; component is rendered separately, and is the unit of information
;;; passed to the client. Internally, each component is identified by
;;; a unique id relative to its containing component. When
;;; communicating about a component with the client, the path of ids
;;; from the root component to it are concatenated together to become
;;; the component's identifier.

;;; By breaking the dom into components, we able to reuse subsidiary
;;; parts of the dom that the client already has, even if a containing
;;; level of the dom changes. For example, if the containing dom node
;;; adds a new child, we don't need to re-compute or re-transmit its
;;; other children.

;;; We use attributes, as supported by hiccup, to store html
;;; attributes. A sub-component looks like hiccup, except that it has
;;; this format, which is recognized and processed by the tracker:
;;;   [:component {
;;;            :relative-id <id relative to containing component>
;;;      :relative-identity <optional unique determiner of the data>
;;;                  :class <optional subset of classes the DOM will have>
;;;                     ... <other attributes that help define the component>
;;;    }]
;;; While a dom can change, the information for a sub-component with a
;;; given :relative-identity must always be the same, given the
;;; component. This allows the tracker to only notices changes to
;;; relative-identity. If :relative-identity isn't present, then
;;; :relative-id functions as :relative-identity.

;;; The dom_tracker will give the client a dom with these subsidiary
;;; components, with the initially specified class, and it will create
;;; additional computations to compute the dom for the components,
;;; passing them as updates to the client once they are computed.

;;; The store is always kept in memory, as is which parts of the store
;;; the rendering of each component depends on. But the information
;;; that describes how each part of the dom should be rendered and to
;;; interpret actions on it is recreated on demand as needed.

;;; A specification map is used describe how to turn part of the
;;; store into a component of dom. The map holds the information for
;;; what is to be rendered, such as an item id, and what style of
;;; rendering to use. Generating the dom will require addition
;;; information from the store, such as the content and elements of
;;; the item to be rendered.

;;; To maximize reuse, the dom specification should not have any
;;; extraneous information, as any change to the specification
;;; requires a re-rendering of the dom. The specification should focus
;;; exclusively on what is to be shown and how it should be
;;; formatted. The actual user information conveyed by the dom should
;;; come from the store.

;;; To ask to render a dom, the dom tracker uses two functions, stored
;;; in the spec map under :rendering-data and :render-dom. The
;;; :rendering-data function takes the mutable store and returns a seq
;;; of <reporter, categories> pairs, which give the information that
;;; the rendering requires and gives what categories of changes it is
;;; sensitive to. The tracker then registers for updates to those
;;; categories for those reporters, gets the current values of the
;;; reporters, and calls the :render-dom function with the spec map
;;; and those values.

;;; By doing it this way, the dom tracker will learn of any changes
;;; that require recomputing the dom, and will have registered for
;;; those changes before getting the data the renderer will
;;; use. Usually, :rendering-data will usually just return the mutable
;;; store and the ids there that it depends on, but the protocol gives
;;; it the option to create additional reporters that are smart about
;;; tracking the store. For example, reporters can track the result of
;;; a query on the store, so the query doesn't have to be re-run for
;;; every change to the store.

;;; If the resulting dom contains components, the dom spec will have a
;;; function under :sub-specification that takes the parent dom spec
;;; and the component description from the hiccup and returns the spec
;;; for that component. It may only depend on the component
;;; description (which will only depend on its id). The general
;;; principle is that what sub-specifications there are can depend on
;;; the store, but given that a sub-specifications with a given id
;;; appears, its dom spec should be fixed, given the id. Typically,
;;; the id is the item id of what the component is supposed to show.

;;; When a spec changes, any of its sub-component specs may have
;;; changed, so all have to be regenerated. But when component's dom
;;; changes, the tracker only needs to compute specs of sub-components
;;; with new ids and render them. It can assume that any pre-existing
;;; components and their renderings haven't changed. (This means that
;;; a component that is one of several siblings has to be rendered
;;; identically to one that is by itself, or needs a different id for
;;; the two cases. For items in table cells, which need different
;;; formatting if they are an entire cell vs part of an item stack,
;;; inherited CSS can handle the formatting. In other cases, the
;;; sub-component's id may need to change between the two different
;;; rendering situations.)

;;; In addition to rendering dom, a dom specification knows how to
;;; interpret actions on its dom. That interpretation typically
;;; requires additional information that is not necessary for
;;; rendering the dom. This information is primarily what in the store
;;; the dom refers to, which can depend in complicated ways on the
;;; context of the dom. For example, an action in batch render needs
;;; to know what items are in the batch, and then trace the pattern of
;;; the edited dom in each of them.

;;; Rather than compute this information during rendering, it is
;;; computed as actions are done, using with two more functions in the
;;; spec. :sub-action-info holds a function that takes a subsidiary
;;; id, the current store, the current action, and the action
;;; information for the current dom, and returns the action info for
;;; the subsidary dom. Then the function in :handle-action takes the
;;; action information for the dom, a user action, and the current
;;; store and returns a store with the appropriate changes.

;;; This protocol allows :sub-action-info to pass down a modified
;;; store if it needs, for example, to create some items for the
;;; action to act on.

;;; There is a default for each of the functions that a dom
;;; specification can have. For :rendering-data, the default returns
;;; the store and a dependency on the :relative-id, checking that it
;;; is an item id. For :render-dom, the default renders the item
;;; corresponding to the :relative-id, minus any elements in
;;; :excluded-elements. For :sub-action-info, the default returns the
;;; set of items that are represented, in parallel, by the displayed
;;; item. For :sub-specification, the default copies some fields from
;;; the containing specification, and adds any additional fields from
;;; the component.

;;; As a rule, there should be a separate component for every thing
;;; that the user can interact with. But if the code would prefer to
;;; add something that can be interacted with and that is not a
;;; component, this protocol supports that, as long as
;;; :sub-action-info can handle its id, as well as the ids of actual
;;; components.

;;; Each component is uniquely identified with a id, as is any other
;;; dom node that the user might interact with. (The dom for a
;;; component need not have a key; the component can add it when
;;; necessary.) There must never be two components or doms with the
;;; same id, even during updates, or all sorts of confusion can
;;; result. The id of a component must also not change throughout the
;;; life of its parent dom, because conserving it is how we reuse
;;; subsidiary doms.

;;; The heart of a key is the id of the item the dom is about. But
;;; since there can be several dom nodes about same item, we need more
;;; than that. We thus use a sequence of the relative ids on the path
;;; of containment in the dom.

;;; Here is some of the information in a dom specification:
(def basic-dom-spec
  {            :width 1.5  ; A float, giving the width of this dom element
                           ; compared to the minimum width for two column
                           ; format.
              :priority 0  ; How important it is to render this item earlier.
                           ; (Lower is more important.)
         :twin-template "" ; The template that the twins of this dom
                           ; must satisfy.
  :elements-template 'anything ; elements should use this
;                               as the template for their content.
   })
;;; In some cases, the inherited information is halfway between being about
;;; an item and its children. In this case, :template and :attributes are
;;; about the item, while :key-prefix and :subject-referent are about
;;; the children.

(defn ids->string
  "Given a sequence of relative ids, return a string representation
  that can be passed to the client."
  [ids]
  (clojure.string/join
   "_"
   (map #(cond (keyword? %) (str ":" (name %))
               (satisfies? StoredItemDescription %) (id->string %)
               true (assert false (str "unknown key component " %)))
        ids)))

(defn string->ids
  "Given a string representation of a client id, return the relative ids."
  [rep]
  (vec (map
        #(if (= (first %) \:)
           (keyword (subs % 1))
           (string->id %))
        (clojure.string/split rep #"_"))))

;;; --- Top level item ---

(comment
  (defn top-level-item-DOM-R
    "Make a dom for an item, testing the item to see what sort of dom to make."
    [item referent inherited]
    (let [inherited (into starting-inherited inherited)]
      (expr-let [table (matching-elements :table item)
                 top-level (matching-elements :top-level item)
                 tags (matching-elements :tag item)]
        (if (empty? table)
          (let [subject-ref (or (:subject-referent inherited)
                                (let [[exemplar subject-ref]
                                      (referent->exemplar-and-subject referent)]
                                  (or subject-ref
                                      (when-let [subject (subject item)]
                                        (when (current-value
                                               (semantic-entity? subject))
                                          (item-referent subject))))))
                inherited (cond-> inherited
                            subject-ref
                            (update
                             :attributes
                             #(conj (or % [])
                                    [#{:label :optional} #{:content}
                                     {:expand {:referent subject-ref}}])))
                dom (item-DOM-R item tags inherited
                                :referent referent
                                :must-show-label (empty? tags)
                                :do-not-show-content (not (empty? top-level)))]
            (expr-let [dom dom]
              (cond-> dom
                (seq tags)
                (add-attributes {:class "tag"}))))
          (table-DOM-R item inherited))))))

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

;;; If we are batch editing and there is a non-trivial batch edit selector,
;;; return the batch edit selector items.
(comment
  (defn batch-editing-selector-items [store session-temporary-id client-state]
    (expr-let [batch-editing (state-map-get client-state :batch-editing)]
      (when batch-editing
        (let [temporary-item (description->entity session-temporary-id store)]
          (expr-let [selector-items (label->elements
                                     temporary-item :batch-selector)
                     row-selector (expr first
                                    (label->elements
                                     temporary-item :batch-row-selector))
                     query-content (semantic-to-list row-selector)]
            (when (and query-content (not= query-content 'anything))
              selector-items)))))))

;;; TODO: Add a unit test for this.
(comment
  (defn top-level-DOM-R
    [store session-temporary-id client-state]
    (expr-let [batch-editing-items (batch-editing-selector-items
                                    store session-temporary-id client-state)]
      (if (seq batch-editing-items)
        (batch-edit-DOM-R batch-editing-items store starting-inherited)
        (expr-let [referent (state-map-get client-state :referent)
                   subject-referent (state-map-get client-state :subject-referent)
                   immutable-item (call-dependent-on-id
                                   store nil
                                   (fn [immutable-store]
                                     (or (when referent
                                           (first (instantiate-referent
                                                   referent immutable-store)))
                                         (first-tab-R immutable-store))))]
          (if immutable-item
            (let [item (description->entity (:item-id immutable-item) store)
                  inherited (cond-> starting-inherited
                              subject-referent
                              (assoc :subject-referent subject-referent))]
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
                    [tabs-DOM-R holder nil
                     (assoc starting-inherited :key-prefix [:tab])])]))))))))

(comment
  (defn DOM-for-client-R
    "Return a reporter giving the DOM specified by the client."
    [store session-temporary-id client-state]
    (expr-let [dom (top-level-DOM-R
                    store session-temporary-id client-state)]
      (into dom [(make-component {:key [:label-values]}
                                 [label-datalist-DOM-R store])]))))
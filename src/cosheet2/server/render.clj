(ns cosheet2.server.render
  (:require (cosheet2 [query :refer [matching-elements matching-items]]
                      [debug :refer [simplify-for-print]]
                      [store :refer [id-valid? StoredItemDescription]]
                      [entity :refer [subject content label->elements
                                      label->element
                                      description->entity updating-immutable]]
                      [reporter :refer [reporter-value universal-category]]
                      [expression :refer [expr expr-let expr-seq cache
                                          category-change]]
                      [calculator :refer [current-value]]
                      [map-state :refer [map-state-get]]
                      [hiccup-utils :refer [add-attributes into-attributes]])
            (cosheet2.server
             [order-utils :refer [semantic-entity?]]
             [model-utils :refer [tabs-holder-id-R ordered-tabs-ids-R
                                  semantic-to-list]]
             [render-utils :refer [make-component]]
             [item-render :refer [render-item-DOM]]
             [table-render :refer [render-table-DOM]]
             [tabs-render :refer [render-tabs-DOM]]
             [batch-edit-render :refer [render-batch-edit-DOM
                                        get-batch-edit-rendering-data]]
             [action-data :refer [get-id-action-data
                                  get-empty-action-data]]
             ; [tabs-render :refer [tabs-DOM-R]]
             ; [Batch-edit-render :refer [batch-edit-DOM-R]]
             )))

;;; Code to create hiccup style dom for a database entity.

;;; For a basic entity, we show its contents and its semantic
;;; elements, but not its non-semantic elements. Semantic elements
;;; have a content of a number, a string, 'anything,
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
;;; store, and a dom-manager that keeps track of what dom might need
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

;;; We use attributes, as supported by hiccup, to store information
;;; about components. So a sub-component looks like hiccup, with this
;;; format which is recognized and processed by the dom manager:
;;;   [:component {
;;;                 :class  Optional subset of classes the DOM will have
;;;           :relative-id  The id relative to containing component
;;;                         This is also the id the dom is about, unless
;;;                         overridden by :item-id
;;;                    ...  Any attribute that a dom specification (see
;;;                         below) can have.
;;;                         
;;;    }]

;;; The dom manager will give the client a dom with these subsidiary
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
;;; rendering to use. Generating the dom will require additional
;;; information from the store, such as the content and elements of
;;; the item to be rendered.

;;; To maximize reuse, the dom specification should not have any
;;; extraneous information, as any change to the specification
;;; requires a re-rendering of the dom. The specification should focus
;;; exclusively on what is to be shown and how it should be
;;; formatted. The actual user information conveyed by the dom should
;;; come from the store.

;;; To ask to render a dom, the dom manager uses two functions, stored
;;; in the spec map under :get-rendering-data and :render-dom. The
;;; :get-rendering-data function takes specification and the mutable
;;; store and returns a seq of <reporter, categories> pairs, which
;;; give the information that the rendering requires and gives what
;;; categories of changes it is sensitive to. The manager then
;;; registers for updates to those categories for those reporters,
;;; gets the current values of the reporters, and calls the
;;; :render-dom function with the spec map and those values.

;;; By doing it this way, the dom manager will learn of any changes
;;; that require recomputing the dom, and will have registered for
;;; those changes before getting the data the renderer will
;;; use. Usually, :get-rendering-data will usually just return the
;;; mutable store and the ids there that it depends on, but the
;;; protocol gives it the option to create additional reporters that
;;; are smart about tracking the store. For example, reporters can
;;; track the result of a query on the store, so the query doesn't
;;; have to be re-run for every change to the store.

;;; When a component's dom changes, the manager only needs to
;;; re-render sub-components with new ids. It can assume that any
;;; pre-existing components and their renderings haven't
;;; changed. (This means that a component that is one of several
;;; siblings has to be rendered identically to one that is by itself,
;;; or needs a different id for the two cases. For items in table
;;; cells, which need different formatting if they are an entire cell
;;; vs part of an item stack, inherited CSS can handle the
;;; formatting. In other cases, the sub-component's id may need to
;;; change between the two different rendering situations.)

;;; TODO: Optionally, a dom specification can have a
;;; :sub-dom-specification method, which gives the specification of a
;;; subsidiary component, given its relative id. This way, if the
;;; manager needs a subsidiary component of a very large dom, it
;;; doesn't have to render the entire dom.

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
;;; spec. :get-action-data holds a function that takes a dom
;;; specification, the action data for the containing dom, a user
;;; action, and the current store, and returns the action data for the
;;; given dom, which will be a map. Then the function in
;;; :handle-action takes the action information for the dom, a user
;;; action, and the current store and returns a store with the
;;; appropriate changes.

;;; Rather than a function, :get-action-data may alternatively hold a
;;; vector, where the first element is the function, and the rest is
;;; extra arguments beyond the usual for :get-action-data. While
;;; putting in a closure, rather than a vector could do the same
;;; thing, the vector is easier to read in debugging output.

;;; This protocol allows :get-action-data to pass down a modified
;;; store as part of its output. For example, it might want to create
;;; some items for the action to act on.

;;; There is a default for each of the functions that a dom
;;; specification can have. For :get-rendering-data, the default
;;; returns the store and a dependency on the :item-id or
;;; :relative-id, checking that it is an item id. For :render-dom, the
;;; default renders the item corresponding to the :relative-id, minus
;;; any elements in :excluded-element-ids. For :get-action-data, the
;;; default returns the set of ids that are all represented by the
;;; displayed item.

;;; Each component is uniquely identified with a client id, which is
;;; added by the dom manager. There must never be two components or
;;; doms with the same id, even during updates, or all sorts of
;;; confusion can result. The id of a component must also not change
;;; throughout the life of its parent dom, because conserving it is
;;; how we reuse subsidiary doms.

;;; The heart of the id is typically the :relative-id, which is the id
;;; of the item the dom is about. But since there can be several dom
;;; nodes about same item, we need more than that. We thus use the
;;; sequence of the relative ids on the path of containment in the
;;; dom.

;;; As a rule, there should be a separate component for every thing
;;; that the user can interact with. But the dom manager is free to
;;; elide out components when it sends them to the client, as long as
;;; it includes their ids.

;;; A dom specification can contain any of these fields.
;;; Any of the fields that expect functions will also accept a list,
;;; where the first element is the function, and the rest of the list
;;; is additional arguments. (This approach is better than closures, which
;;; are hard to display and to test.)
;;;           :relative-id  The id relative to containing component
;;;                         This is normally the id the dom is about, or an
;;;                         exemplar element of one of the ids the containing
;;;                         component is about.
;;;                         However, this may overridden by :item-id or
;;;                         :item-ids. If overridden, it may be a keyword,
;;;                         an ItemId, or a vec of those.
;;;               :item-id  The id of the item the dom is about, if
;;;                         :relative-id is not an id or needs to be
;;;                         overridden.
;;;              :item-ids  If the dom pertains to more ids than its parent,
;;;                         this will list them. In some cases, both this
;;;                         and :item-id may be present, in which case
;;;                         :item-id will be an exemplar element of one of
;;;                         the :item-ids, and the dom will pertain to
;;;                         a matching element of each
;;;                 :class  Optional subset of classes the DOM will have.
;;;            :render-dom  Optional function that takes this specification
;;;                         and additional reporter values, then produces
;;;                         the dom.
;;;    :get-rendering-data  Optional function that takes this map and the
;;;                         mutable store and returns a seq of pairs of the
;;;                         a reporter whose values is needed by :render-dom
;;;                         and the categories of that reporter it depends on.
;;;         :handle-action  Optional function that takes data about how to
;;;                         interpret actions, a user action, and the current
;;;                         store, and returns a store with the appropriate
;;;                         changes.
;;;       :get-action-data  Optional function that takes a dom specification,
;;;                         the action data for the containing dom, a user
;;;                         action, and the current store, and returns a map
;;;                         consisting of the action data for the given dom.
;;;       :must-show-label  If true, a virtual label should be shown
;;;                         if there are no labels.
;;;                 :width  A float, giving the width of this dom element
;;;                         compared to the minimum width for two column
;;;                         format.
;;;              :template  The template that elements in this position
;;;                         must start out satisfying. For a regular DOM,
;;;                         this means any twins it gets, while for a virtual
;;;                         DOM, it means its new item.
;;;                         If the value of :template is :singular, then
;;;                         twins may not be created, and the item may
;;;                         not be deleted.
;;;           :adjacent-id  For a virtual item, the id of the item to be
;;;                         adjacent to.
;;;        :adjacent-order  Whether a new virtual item should come :before
;;;                         or :after the adjacent item.
;;;                     ...  <other attributes that help define the component>
;;;    }]

;;; Here is a minimal dom specification, lacking its :relative-id:
(def basic-dom-specification
  {            :width 1.5 ; A float, giving the width of this dom element
                          ; compared to the minimum width for two column
                          ; format.
            :template ""  ; The template that the twins of this dom
                          ; must start out satisfying.
   })

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

(defn top-level-id-R
  "Return a reporter whose value is the id to be displayed at the top level."
  [store client-state]
  (expr-let [id (map-state-get client-state :root-id)
             id-valid (id-valid? store id)]
    (or (when id-valid id)
        (expr first (ordered-tabs-ids-R store)))))

(defn batch-editing-component
  [store temporary-id]
  ;; The batch edit ids never change, so we can pick them out of the
  ;; current store.
  (let [immutable-store (current-value store)
        temporary-item (description->entity temporary-id immutable-store)
        query-item (label->element temporary-item :batch-query)
        stack-selector-item (label->element temporary-item
                                            :batch-stack-selector)]
    (make-component {:relative-id :batch-edit
                     :query-id (:item-id query-item)
                     :stack-selector-id (:item-id stack-selector-item)
                     :render-dom render-batch-edit-DOM
                     :get-rendering-data get-batch-edit-rendering-data
                     :get-action-data get-empty-action-data})))

;;; TODO: Add a unit test for this.
(defn top-level-DOM-R
  "Return a reporter whose value is the DOM for tabs and the top level
  component."
  [store temporary-id client-state id-R]
  (expr-let [id id-R
             batch-editing (map-state-get client-state :batch-editing)]
    (println "top level DOM id:" id "  Batch editing:" batch-editing)
    (if batch-editing
      (batch-editing-component store temporary-id)
      (when id
        (expr-let [immutable-store (category-change [id] store)]
          (let [immutable-item (description->entity id immutable-store)
                is-tab (seq (matching-elements :tab immutable-item))]
            [:div {}
             (if is-tab
               ;; Show the tabs, plus the topic of the selected tab
               (let [topic (first (label->elements immutable-item :tab-topic))
                     subject (subject immutable-item)]
                 [:div {:class "tabbed"}
                  (make-component {:relative-id (:item-id subject)
                                   :client-state client-state
                                   :chosen-tab-id id
                                   :render-dom render-tabs-DOM
                                   :get-action-data [get-id-action-data
                                                     (:item-id subject)]})
                  (make-component (assoc basic-dom-specification        
                                         :relative-id (:item-id topic)
                                         :render-dom render-table-DOM))])
               ;; No tab is selected. Show just the item.
               (make-component
                (assoc basic-dom-specification        
                       :relative-id (:item-id immutable-item)
                       :must-show-label true
                       :width 0.75
                       :get-action-data [get-id-action-data
                                         (:item-id immutable-item)])))]))))))

(defn top-level-get-action-data
  "Return a function giving the action data for the top level component"
  [specification containing-action-data action immutable-store]
  {:target-ids [(reporter-value (:id-R specification))]})

(defmethod print-method
  cosheet2.server.render$top_level_get_action_data
  [v ^java.io.Writer w]
  (.write w "top-level-AD"))

(defn reporter-specification-get-rendering-data
  "Return the rendering data for a component whose dom is what a
  reporter returns"
  [spec store]
  (println "getting top level DOM rendering data")
  [[(:reporter spec) [universal-category]]])

(defmethod print-method
  cosheet2.server.render$reporter_specification_get_rendering_data
  [v ^java.io.Writer w]
  (.write w "rep-RD"))

(defn reporter-specification-render-dom
  "Make the component's dom be what a reporter returns."
  [spec reporter-value]
  reporter-value)

(defmethod print-method
  cosheet2.server.render$reporter_specification_render_dom
  [v ^java.io.Writer w]
  (.write w "rep-DOM"))

(defn top-level-DOM-spec
  [store session-temporary-id client-state]
  (let [id-R (top-level-id-R store client-state)]
    (assoc basic-dom-specification
           :reporter (top-level-DOM-R
                      store session-temporary-id client-state id-R)
           :id-R id-R ; used by top-level-get-action-data
           :get-action-data top-level-get-action-data
           :get-rendering-data reporter-specification-get-rendering-data
           :render-dom reporter-specification-render-dom)))

(comment ;; Copy stuff out of here as we support more kinds of top levels.
  (defn top-level-DOM-R
    [store session-temporary-id client-state]
    (expr-let [batch-editing-items (batch-editing-selector-items
                                    store session-temporary-id client-state)]
      (if (seq batch-editing-items)
        (batch-edit-DOM-R batch-editing-items store starting-inherited)
        (expr-let [referent (state-map-get client-state :referent)
                   subject-referent (state-map-get client-state
                                                   :subject-referent)
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

(comment
  (defn spec-for-client-R
    "Return a specification for the DOM indicated by the client."
    [store session-temporary-id client-state]
    (expr-let [dom (top-level-DOM-spec
                    store session-temporary-id client-state)]
      (into dom [(make-component {:key [:label-values]}
                                 [label-datalist-DOM-R store])]))))

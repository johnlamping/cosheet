(ns cosheet.server.render
  (:require (cosheet [query :refer [matching-elements matching-items]]
                      [debug :refer [simplify-for-print]]
                      [store :refer [id-valid-link? id-known?]]
                      [entity :refer [target-entity content label->elements
                                      label->element id->entity]]
                      [reporter :refer [reporter-value-or-invalid universal-category]]
                      [reporter-macros :refer [app-R let-R seq-R]]
                      [category-change-calculator :refer [category-change-R]]
                      [calculator :refer [current-value]]
                      [map-reporter :refer [map-reporter-get]]
                      [hiccup-utils :refer [add-attributes into-attributes]])
            (cosheet.server
             [order-utils :refer [semantic-element?]]
             [model-utils :refer [tabs-holder-id-R ordered-tabs-ids-R
                                  semantic-to-list]]
             [render-utils :refer [make-component]]
             [item-render :refer [render-item-DOM-R]]
             [table-render :refer [render-table-DOM-R get-table-rendering-data]]
             [tabs-render :refer [render-tabs-DOM]]
             [batch-edit-render :refer [render-batch-edit-DOM
                                        get-batch-edit-rendering-data]]
             [action-data :refer [default-get-action-data
                                  get-id-action-data
                                  get-empty-action-data]]
             ; [tabs-render :refer [tabs-DOM-R]]
             ; [Batch-edit-render :refer [batch-edit-DOM-R]]
             )))

;;; Code to create hiccup style dom for a database entity.

;;; For a basic element, we show its contents and its semantic
;;; elements, but not its non-semantic elements. Semantic elements
;;; have a content of a number, a string, an object, or 'anything.

;;; An element is a label or a category if it's content is an object
;;; that is labeled withs link-type or object-type, respectively.

;;; Every semantic element must have an :order sub-element, to
;;; indicate its display position relative to the other elements. And
;;; only semantic elements can have :order sub-elements.
;;; So, for example, the entity:
;;;   ("Joe"
;;;        ("married" ((->Orderable 1 2) :order)
;;;        (39 ((->Orderable 5 6) :order)
;;;            ((link-type-object "age") ((->Orderable 7 8) :order))
;;;            ("doubtful" ((->Orderable 9 10) :order)))
;;; would be rendered to dom that tries to convey:
;;;   Joe
;;;     married
;;;     age: 39
;;;       doubtful

;;; The overall architecture of the system is a dom renderer that
;;; knows how to generate various kinds of dom to display parts of the
;;; store, and a dom-manager that keeps track of what dom might need
;;; to be recomputed and what dom has changed under recomputation and
;;; needs to be sent to the client.

;;; The dom is generated and sent to the client as a logical tree of
;;; components, with branches of the tree corresponding to contained
;;; components. The component is the unit of information passed to the
;;; client.

;;; Each component is rendered independently of the rendering of
;;; others, including independently of its contained
;;; components. Internally, each component is identified by a unique
;;; id relative to its containing component. When communicating about
;;; a component with the client, the path of ids from the root
;;; component to it are concatenated together to become the
;;; component identifier the client sees.

;;; By breaking the dom into components, we are able to reuse
;;; subsidiary parts of the dom that the client already has, even if a
;;; containing level of the dom changes. For example, if the
;;; containing dom node adds a new child, we don't need to re-compute
;;; or re-transmit its other children.

;;; We use attributes, as supported by hiccup, to store information
;;; about components. A sub-component looks like hiccup with this
;;; format, which is recognized and processed by the dom manager:
;;;   [:component <dom-specification>]

;;; When the dom manager first mentions a component to the client, it
;;; will generally be as a subsidiary component of a dom it is
;;; sending. It will give the client the initially specified class of
;;; each subsidiary component, but not necessarily its dom
;;; yet. Rather, it will create computations to compute the dom for
;;; the subsidiary components, and pass their doms as updates to the
;;; client once they are computed.

;;; The <dom-specification> of a component is a map holding the
;;; information that describes how to turn part of the store into a
;;; dom, and how to interpret actions on that dom. It thus holds the
;;; information for what is to be rendered, such as an item id, and
;;; what style of rendering to use.

;;; To maximize reuse, the dom specification should not have any
;;; extraneous information, because any change to the specification
;;; requires a re-rendering of the dom. In particular, while it will
;;; typically indicate an id from the store, it shouldn't reflect
;;; anything the store knows about that id. Rather, when the dom is
;;; generated, the store will provide the substance of what is shown,
;;; such as the content and elements of the item to be rendered.

;;; The store is always kept in memory, along with which parts of the
;;; store the rendering of each component depends on. But dom
;;; specifications may be removed from memory once their dom has been
;;; generated. If they are needed later, for example because the store
;;; has changed for something they show, they can be recreated, using
;;; the the dom specification of their parent. In general, this
;;; requires walking up the containment tree to the root dom
;;; specification, which is always kept, and then walking back down,
;;; creating dom specifications on the way. Fortunately the
;;; containment depth is usually not very deep, so this is fast. This
;;; is not currently being done, but there is a todo to do it.

;;; To ask to render a component, the dom manager calls its
;;; specification's rendering function, which is stored in
;;; :render-dom, giving it the specification and the mutable store.
;;; That function returns a reporter whose value is the current dom
;;; for the specification.

;;; As a rule, rendering functions are named like render-...-DOM-R. They
;;; get the information they need our
;;; of the store, with a let-R, and then run with the immutable
;;; information it retrieved. This lets their subsidiary functions
;;; work on immutable data.

;;; The subsidiary functions can thus take immutable entities as
;;; arguments, rather than the store. They are typically named like
;;; ...-DOM.  In the case they the need to generate dom that further
;;; depends on the store, they can create components, which get their
;;; own chance to access the store when the dom manager calls their
;;; renderers.

;;; The dom manager registers for the reporter it gets back from the
;;; renderer, so whenever the dom changes, the manager can send the
;;; updated dom to the client. The dom manager also has to note any
;;; changes to what sub-components are needed, based on their ids. It
;;; deactivates no longer needed ones, and activates new ones, while
;;; subcomponents common to both the old and new doms don't need to be
;;; changed.

;;; (This means that if a component's appearance needs to change based
;;; on its context, the change must either be handled by css, or the
;;; component needs to have different ids for the different
;;; appearances. For items in table cells, which need different
;;; formatting if they are an entire cell vs part of an item stack,
;;; inherited CSS can handle the formatting. In other cases, the
;;; sub-component's id may need to change between the two different
;;; rendering situations.)

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
;;; computed as actions are done, using two more functions in the
;;; spec. :get-action-data holds a function that takes a dom
;;; specification, the action data for the containing dom, a user
;;; action, and the current store, and returns the action data for the
;;; given dom, which will be a map. Then the function in
;;; :handle-action takes the action information for the dom, a user
;;; action, and the current store and returns a store with the
;;; appropriate changes.

;;; The value of :get-action-data may be a pseudo-closure, a sequence
;;; where the first element is the function, and the rest is extra
;;; arguments beyond the usual for :get-action-data.

;;; This protocol allows :get-action-data to pass down a modified
;;; store as part of its output. For example, it might want to create
;;; some items for the action to act on.

;;; Each component is uniquely identified by a client id, which is
;;; added by the dom manager. There must never be two components or
;;; doms with the same id, even during the middle of updates, or all
;;; sorts of confusion can result. The id of a component must also not
;;; change throughout the life of its parent dom, because conserving
;;; it is how we reuse subsidiary doms.

;;; The heart of the id is typically the :relative-id, which is the id
;;; of the item the dom is about. But since there can be several dom
;;; nodes about same item, we need more than that. We thus use the
;;; sequence of the relative ids on the path of containment in the
;;; dom.

;;; As a rule, there should be a separate component for every thing
;;; that the user can interact with. But when the dom manager sends
;;; components to the client, it is free to elide out components whose
;;; dom is simply another component, as long as it doesn't affect the
;;; ids of components it does send.

;;; A dom specification can contain any of these fields.  Any of the
;;; fields that expect functions will also accept a pseudo closure, a
;;; sequence, where the first element is the function, and the rest of
;;; the list is additional arguments. (This approach is easier to to
;;; display and to debug than closures.)
;;;           :relative-id  The id relative to the containing component for
;;;                         identifying this component in the dom.
;;;                         This is normally the id the dom is about, or an
;;;                         exemplar element of one of the ids the containing
;;;                         component is about.
;;;                         However, in some cases, this will be a
;;;                         keyword, like :content or :virtual, which
;;;                         will be enough to uniquely indicate how
;;;                         this component relates to its parent. If a
;;;                         keyword wouldn't be unique, this can be a
;;;                         pair of a keyword and an id. In these
;;;                         cases, if an id is still needed,
;;;                         :auxiliary-item-id will hold id.
;;;                 :class  Optional. A subset of the CSS classes the DOM
;;;                         will have. The dom may have additional classes.
;;; :omit-universal-elements  If true, don't show elements of the objects
;;;                         whose contents are one of the universal
;;;                         objects: name-label, link-type, and
;;;                         object-type. This is used when showing
;;;                         just the name of a named object, because
;;;                         we don't also want to show that it is
;;;                         labeled as a name. That's clear from
;;;                         context, and would lead to infinite
;;;                         recursion, since the name label also has
;;;                         name "name". In contrast, when showing
;;;                         all of a named object, we want to show
;;;                         the label of the name, so then we don't
;;;                         use this.
;;; :exclude-elements-by-ids  If present, this is a list of ids of
;;;                         elements of the entity being shown that should
;;;                         not be displayed.
;;;     :auxiliary-item-id  If this is present, :relative-id will be a
;;;                         keyword, and this field will give an id
;;;                         needed by the component. Its meaning
;;;                         depends on :relative-id's keyword:
;;;                         :content - This is the id of the element whose
;;;                         content should be shown.
;;;                         :virtual - This is the id of the item that
;;;                         the new item should be adjacent to in the store.
;;;          :parallel-ids  Sometimes a dom pertains to more ids than its
;;;                         parent does, like a label dom that wraps
;;;                         several items. In that case, :parallel-ids
;;;                         gives a sequence of ids that are
;;;                         intermediary between this dom's parent's
;;;                         items, and this dom's items. The context
;;;                         for this dom has an item for each id of
;;;                         the parent context, for each id in
;;;                         :parallel-ids.
;;;                         TODO: Get rid of this, and just make :item-id
;;;                               accept a sequence (for sequential
;;;                               sub-elements) with sub-sequenques
;;;                               (for parallelism).
;;;            :render-dom  Function that takes this specification and
;;;                         the mutable store and returns a reporter
;;;                         whose value is the dom.
;;;         :handle-action  Optional function that takes data about how to
;;;                         interpret actions, a user action, and the current
;;;                         store, and returns a store with the appropriate
;;;                         changes.
;;;                         TODO: not currently implemented
;;;             :immutable  If true, the user cannot change anything about
;;;                         this item, and can't even select it. This
;;;                         property is inherited to child elements.
;;;       :get-action-data  Optional pseudo function that takes a dom
;;;                         specification, the action data for the
;;;                         containing dom, a user action, and the
;;;                         current store, and returns a map
;;;                         consisting of the action data for the
;;;                         given dom.
;;;                         defaults to action-data/default-get-action-data
;;;       :must-show-label  If true, a virtual label should be shown
;;;                         if there are no labels. If, additionally, it is
;;;                         :wide, show it with substantial space, if there
;;;                         is significant space available.
;;;                 :width  A float, giving the width of this dom element
;;;                         compared to the minimum width for two column
;;;                         format.
;;;              :template  The template that elements in this position
;;;                         must start out satisfying. For a regular DOM,
;;;                         this means any twins it gets, while for a virtual
;;;                         DOM, it means its new item.
;;;                         The value of :template may instead be
;;;                         :singular.  That means twins may not be
;;;                         created, and the item may not be deleted
;;;                         with a simple delete action.
;;;                         Or the value of the template may be a
;;;                         SequentialTemplate.  That means an item
;;;                         matching the first element of its template
;;;                         sequence must be created, using that as
;;;                         the target of the next, etc. Except if a
;;;                         template is an object, it becomes the
;;;                         content of the target, rather than a
;;;                         sub-element.
;;;        :is-object-name  This cell holds the name of a named object,
;;;                         and while the cell's relative id is for
;;;                         the name of the object, the cell logically
;;;                         refers to the whole object. Specifically,
;;;                         editing the name in cell should not change
;;;                         the name of the object, but should make
;;;                         the enclosing element refer to the object
;;;                         with the name the user entered. The
;;;                         template is what the object referred to
;;;                         must satisfy.
;;;           :adjacent-id  For a virtual item, the id of the item to be
;;;                         adjacent to.
;;;        :adjacent-order  Whether a new virtual item should come :before
;;;                         or :after the adjacent item.
;;;               :table id The id of the table, if any, that the dom is in.
;;;                 :row-id The id of the row, if any, that the dom is in.
;;;             :column-ids The ids of the columns, if any, that the dom is
;;;                         in. For most cells, this will hold just a single
;;;                         column. But header doms can span multiple
;;;                         columns that share one of their labels
;;;                    ...  <other attributes that help define the component>
;;;    }]

(defn dom-renderer
  [dom-specification]
  (if-let [renderer (:render-dom dom-specification)]
    renderer
    (assert false dom-specification)))

;;; NOTE: action-data-getter is defined in action_data.clj, because it
;;; both needs a function defined there and is used there. So putting
;;; it here would make a circular dependency.

;;; Here is a minimal dom specification, but lacking its :relative-id:
(def basic-dom-specification
  {            :width 1.5 ; A float, giving the width of this dom element
                          ; compared to the minimum width for two column
                          ; format.
            :template ""  ; The template that the twins of this dom
                          ; must start out satisfying.
   })

;;; --- Top level item ---

(comment
  ;; NOTE: subject-referent in here is obsolete.
  (defn top-level-item-DOM-R
    "Make a dom for an item, testing the item to see what sort of dom to make."
    [item referent inherited]
    (let [inherited (into starting-inherited inherited)]
      (let-R [table (matching-elements :table item)
              top-level (matching-elements :top-level item)
              tags (matching-elements :tag item)]
        (if (empty? table)
          (let [subject-ref (or (:subject-referent inherited)
                                (let [[exemplar subject-ref]
                                      (referent->exemplar-and-subject referent)]
                                  (or subject-ref
                                      (when-let [target (target-entity item)]
                                        (when (current-value
                                               (semantic-element? target))
                                          (item-referent target))))))
                dom (item-DOM-R item tags inherited
                                :referent referent
                                :must-show-label (empty? tags)
                                :do-not-show-content (not (empty? top-level)))]
            (let-R [dom dom]
              (cond-> dom
                (seq tags)
                (add-attributes {:class "tag"}))))
          (table-DOM-R item inherited))))))

;;; If we are batch editing and there is a non-trivial batch edit selector,
;;; return the batch edit selector items.
(comment
  (defn batch-editing-selector-items [store session-ephemeral-id client-state]
    (let-R [batch-editing (state-map-get client-state :batch-editing)]
      (when batch-editing
        (let [ephemeral-item (id->entity session-ephemeral-id store)]
          (let-R [selector-items (label->elements
                                  ephemeral-item :batch-selector)
                  row-selector (app-R first
                                 (label->elements
                                  ephemeral-item :batch-row-selector))
                  query-content (semantic-to-list row-selector)]
            (when (and query-content (not= query-content 'anything))
              selector-items)))))))

(defn top-level-id-R
  "Return a reporter whose value is the id to be displayed at the top level."
  [store client-state]
  (let-R [id (map-reporter-get client-state :root-id)
          known (id-known? store id)]
    (if known
      id
      (app-R first (ordered-tabs-ids-R store)))))

(defn batch-editing-component
  [store ephemeral-id]
  ;; The batch edit ids never change, so we can pick them out of the
  ;; current store.
  (let [immutable-store (current-value store)
        ephemeral-item (id->entity ephemeral-id immutable-store)
        query-item (label->element ephemeral-item :batch-query)
        stack-item (label->element ephemeral-item :batch-stack)]
    (make-component {:relative-id :batch-edit
                     :query-id (:item-id query-item)
                     :stack-id (:item-id stack-item)
                     :render-dom render-batch-edit-DOM
                     :get-action-data get-empty-action-data})))

;;; TODO: Add a unit test for this.
(defn top-level-DOM-R
  "Return a reporter whose value is the DOM for tabs and the top level
  component."
  [store ephemeral-id client-state id-R]
  (let-R [id id-R
          batch-editing (map-reporter-get client-state :batch-editing)]
    (println "top level DOM id:" id "  Batch editing:" batch-editing)
    (if batch-editing
      (batch-editing-component store ephemeral-id)
      (when id
        (let-R [immutable-store (category-change-R [id] store)]
          (let [immutable-item (id->entity id immutable-store)
                is-tab (seq (matching-elements :tab immutable-item))]
            [:div {}
             (if is-tab
               ;; Show the tabs, plus the topic of the selected tab
               (let [topic (first (label->elements immutable-item :tab-topic))
                     target (target-entity immutable-item)]
                 [:div {:class "tabbed"}
                  (make-component
                   {:relative-id (:item-id target)
                    :chosen-tab-id id
                    :render-dom render-tabs-DOM
                    :get-action-data [get-id-action-data (:item-id target)]})
                  (make-component
                   {:relative-id (:item-id topic)
                    :table-id (:item-id topic)
                    :render-dom render-table-DOM-R
                    :get-action-data default-get-action-data})])
               ;; No tab is selected. Show just the item.
               (make-component
                (assoc basic-dom-specification        
                       :relative-id (:item-id immutable-item)
                       :render-dom render-item-DOM-R
                       :get-action-data default-get-action-data
                       :must-show-label true
                       :width 0.75
                       :get-action-data [get-id-action-data
                                         (:item-id immutable-item)])))]))))))

(defn top-level-get-action-data
  "Return a function giving the action data for the top level component"
  [specification containing-action-data action immutable-store]
  {:subject-ids [(reporter-value-or-invalid (:id-R specification))]})

(defmethod print-method
  cosheet.server.render$top_level_get_action_data
  [v ^java.io.Writer w]
  (.write w "top-level-AD"))

(defn reporter-specification-render-dom
  "Make the component's dom be what a reporter returns."
  [spec ms]
  (:reporter spec))

(defmethod print-method
  cosheet.server.render$reporter_specification_render_dom
  [v ^java.io.Writer w]
  (.write w "rep-DOM"))

(defn top-level-DOM-spec
  [store session-ephemeral-id client-state]
  (let [id-R (top-level-id-R store client-state)]
    (assoc basic-dom-specification
           :relative-id :root
           :reporter (top-level-DOM-R
                      store session-ephemeral-id client-state id-R)
           :id-R id-R ; used by top-level-get-action-data
           :get-action-data top-level-get-action-data
           :render-dom reporter-specification-render-dom)))

(comment ;; Copy stuff out of here as we support more kinds of top levels.
  ;; NOTE: subject-referent in here is obsolete.
  (defn top-level-DOM-R
    [store session-ephemeral-id client-state]
    (let-R [batch-editing-items (batch-editing-selector-items
                                store session-ephemeral-id client-state)]
      (if (seq batch-editing-items)
        (batch-edit-DOM-R batch-editing-items store starting-inherited)
        (let-R [referent (state-map-get client-state :referent)
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
            (let [item (id->entity (:item-id immutable-item) store)
                  inherited (cond-> starting-inherited
                              subject-referent
                              (assoc :subject-referent subject-referent))]
              (let-R [tab-tags (matching-elements :tab item)
                      content (content item)]
                (if (empty? tab-tags)
                  ;; Show just the item.
                  (top-level-item-DOM-R item referent inherited)
                  ;; Show a selection of tabs.
                  (let-R [topic (app-R first (label->elements item :tab-topic))
                          target (target-entity item)]
                    [:div {:class "tabbed"}
                     (make-component {:key [:tabs]}
                                     [tabs-DOM-R target item inherited])
                     (make-component
                      {:key [:tab (:item-id topic)]}
                      [top-level-item-DOM-R topic nil
                       (assoc inherited :key-prefix [:tab])])]))))
            ;; Show a virtual tab.
            (do (println "showing virtual")
                (let-R [holder (tabs-holder-item-R store)]
                  [:div {:class "tabbed"}
                   (make-component
                    {:key [:tabs]}
                    [tabs-DOM-R holder nil
                     (assoc starting-inherited :key-prefix [:tab])])]))))))))

(comment
  (defn label-datalist-DOM-R
    "Return dom for a datalist of the content of all labels."
    [store]
    ;; TODO: This reruns anytime anything changes. Put support for gathering all
    ;;       values in the store, so it can be more efficient by looking at
    ;;       the changed items. (The store needs a way to tell a reporter
    ;;       about which items changed, not just that something it cared about
    ;;       changed.
    (let-R [labels (matching-items '(nil :tag) store)
            contents (seq-R (map content labels))]
      (let [content-names (map str contents)
            sorted-contents (sort (vals (zipmap (map clojure.string/lower-case
                                                     content-names)
                                                content-names)))]
        (into [:datalist] (map (fn [name] [:option name]) sorted-contents))))))


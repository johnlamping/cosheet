(ns cosheet.server.dom-manager
  (:require [clojure.data.priority-map :refer [priority-map]]
            (cosheet [task-queue :refer [add-task-with-priority]]
                      [reporter :refer [remove-attendee! set-attendee!
                                        set-attendee-and-call-if-valid!
                                        reporter?
                                        reporter-value-when-valid
                                        value-category]]
                      [calculator :refer [propagate-calculator-data!]]
                      [store :refer [item-id? id->string string->id
                                     mutable-store?]]
                      [utils :refer [swap-control-return!
                                     swap-and-act!
                                     swap-and-act-control-return!
                                     with-latest-value
                                     update-in-clean-up
                                     update-new-further-action
                                     update-new-further-actions
                                     dissoc-in
                                     call-pseudo-closure
                                     pseudo-closure-application]]
                      [hiccup-utils :refer [dom-attributes add-attributes
                                            into-attributes]])
            (cosheet.server
             [render :refer [dom-renderer]]
             [action-data :refer [get-item-or-exemplar-action-data
                                  update-action-data-for-component]])))

(def verbose false)

;;; We record what needs to be rendered, and what it depends on.
;;; Whenever a piece of dom changes, we check all the sub-components
;;; it specifies, and update our information.

;;; As renderings are done, we update the client.

;;; The basic data structure is a component. It's like a reporter, except:
;;;   * It makes sub-components that depend on its value (a dom), but
;;;     its value doesn't depend on them.
;;;   * It notifies the client of changes, not other code.

;;; The dom manager tries to reuse sub-components as much as
;;; possible. First, when a new dom arrives for a component, if any of
;;; the specs of its subcomponents match the specs for subcomponents
;;; of its old dom, it reuses those subcomponents. But it also goes a
;;; step deeper. To see why, suppose a table gets a new column. Each
;;; of its rows will need to be recalculated, since they need to show
;;; the new column. But most of their cells don't need recalculating,
;;; since they are still under the old columns. This is an instance of
;;; the overall idea that when a component's dom changes, it may no
;;; longer need some of its subcomponents (the old rows), but some of
;;; its new subcomponents (the new rows) may have use for some of the
;;; subcomponents of the subcomponents it no longer needs (table
;;; cells). So we transfer those sub-subcomponents to become
;;; subcomponents of the relevant new subcomponents (the new rows),
;;; while garbage collecting the rest. In other words, although the
;;; old subcomponents can't be reused directly, they can be salvaged,
;;; so that some of their subcomponents can be reused.

;;; Over its life cycle, a component goes through these stages, in order
;;; (it may skip some):
;;;   * Unstarted.  A new unstarted component is created, with its
;;;     unchanging data filled in, and is added to its parent's active
;;;     subcomponents, all inside a single swap! on its parent.
;;;     The only change that can happen in this state is to receive
;;;     subcomponents from components that it made obsolete.
;;;   * Active.  A component's transition to active is only scheduled
;;;     when it is a current subcomponent of its parent, and its
;;;     parent is active and has no obsolete subcomponents.
;;;     An active component has a reporter calculating its dom, and
;;;     can send updates to the client. This is the only stage where
;;;     new subcomponents are created, which happens then the
;;;     component's dom changes to require different subcomponents.
;;;   * Salvaging.  A salvaging component is obsolete, because it's
;;;     parent has a made a new subcomponent with the same relative
;;;     id. A component will only start to transition to salvaging if
;;;     its parent is active.
;;;     It will ignore any dom updates it gets, it will never send
;;;     updates to the client, and the reporter that calculates its
;;;     dom is shut down, or in the process of being shut down.
;;;     Although the component is not active, it may have current
;;;     subcomponents, which should be transferred to the component
;;;     that replaced it, if possible. Until they can be transferred,
;;;     the active ones are kept active, so they are still up to date
;;;     when they get to their new destination. Even if they are
;;;     unstarted, they should be transferred, since they might have
;;;     gotten some active subcomponents.
;;;     But before any current subcomponents are transferred, all of a
;;;     salvaging component's obsolete subcomponents are themselves
;;;     salvaged, so that their reusable parts can been transfered to
;;;     this component's current subcomponents. Once there are no
;;;     obsolete subcomponents left, its current subcomponents are
;;;     transferred, if possible, to become subcomponents of the
;;;     component that replaced it in its parent's affections. If they
;;;     can't be transferred, they are finalized.
;;;   * Finalizing.  A finalizing component is also obsolete, and has
;;;     all the same restrictions as salvaging, but there is nothing
;;;     to salvage. It has no current subcomponents. All of its
;;;     subcomponents are obsolete, and there is a process running to
;;;     finalize them.
;;;   * Defunct.  A component will start to transition to state
;;;     defunct if it is in state salvaging or finalizing and it has
;;;     no subcomponents.
;;;     After it becomes defunct, a process is started to remove it as
;;;     a subcomponent of its parent. That will remove the last link
;;;     to the defunct component, letting it be garbage collected.

;;; A component is represented with an atom that holds a
;;; ComponentData, which contains information about one component we
;;; are tracking. We use ComponentData, rather than a map, so we can
;;; simplify print-out.
(defrecord ComponentData
    [;; These fields will never change once the component data is created.
     dom-manager           ; Our dom manager.
     parent                ; The atom for the component that we are a
                           ; subcomponent of. This is filled in when
                           ; the component is created. A root component
                           ; has no parent, so this is nil for them.
                           ; When a component is moving from one
                           ; parent to another, this field is changed
                           ; first, then the destination parent is
                           ; updated, and finally the former
                           ; parent. So while this move in in
                           ; progress, there is a disagreement between
                           ; parent and child. In that case, only the
                           ; action that is doing the moving is
                           ; allowed to alter any of that information.
     client-id             ; The id this component will have in the client.
                           ; It is the concatenation of the relative
                           ; ids of the doms of all the components on
                           ; the path from the root to here (with a
                           ; little processing to avoid ambiguity and
                           ; HTML issues).
     elided-from           ; If the dom of one component consists of
                           ; nothing but another component, then the
                           ; inner component is elided away, as far as
                           ; the client sees.  The elided component's
                           ; dom is what gets sent to the client, but
                           ; under the id of the containing dom. (That
                           ; dom's container refers to it by that id,
                           ; so that's the id that has to be sent.)
                           ; If elided-from is present, our component is
                           ; elided, and elided-from is the nearest
                           ; non-elided containing component. Our dom
                           ; will be sent to the client as the dom of
                           ; that component.
     depth                 ; The depth of this component in the component
                           ; hierarchy, used to make parents get sent
                           ; to the client before their children.

     ;; This field normally holds the full dom spec. Once the component
     ;; starts dismantling, it holds only the relative-id.
     dom-specification     ; The dom spec for this component.

     ;; These fields can change.
     dismantling-state     ; Nil while the component is the :unstarted or
                           ; :active stage of its life cycle.
                           ; Otherwise (meaning the component is
                           ; dismantling) it holds the keyword for its
                           ; current life-cycle stage.
     dom-R                 ; A reporter that calculates this component's dom.
                           ; This field is filled in when the
                           ; component is first activated, and is
                           ; cleared when it is deactivated. Those are
                           ; the only two times it changes.
     id->subcomponent      ; A map from :relative-id to the current
                           ; subcomponents of the component. These are
                           ; the subcomponents called for by its
                           ; current dom. They are active or
                           ; unstarted, and waiting to become active.
                           ; Only subcomponents in this map can start
                           ; being activated, or start being
                           ; transferred to another component.
                           ; Each time a new dom is computed for the
                           ; component, this field is recalculated, to
                           ; hold an entry for each sub-component of
                           ; the new dom, reusing the previous
                           ; components whose specifications match the
                           ; specifications provided by the current
                           ; dom.
                           ; Even before a component is activated,
                           ; this field may hold sub-components that
                           ; were carried over from the component this
                           ; component replaced.  That lets a newly
                           ; activated component reuse them rather
                           ; than have to recompute them and resend
                           ; them to the client.
     dismantling           ; A set of subcomponents that are obsolete,
                           ; because the not needed by the component's
                           ; current dom. They can be in any state,
                           ; but there is a process to eventually
                           ; salvage or finalize them.
                           ; They were current at some point, but now
                           ; need to be deactivated before any new
                           ; subcomponents can be activated. This is
                           ; because they might have the same client
                           ; id as a new subcomponent, and if they
                           ; were active they might send their dom to
                           ; the client with a higher version number
                           ; than the current subcomponents,
                           ; overriding the correct dom.
                           ; Components here will never be
                           ; re-activated or transferred. And only the
                           ; salvaging subcomponents in this set will
                           ; transfer any of their subcomponents to
                           ; become components of other components.
                           ; Whenever this set is emptied, the emptier
                           ; must check to see if additional actions
                           ; have become possible, now that there are
                           ; no dismantling subcomponents, and must
                           ; start them.
     salvage-recipient     ; Filled in when the component starts being
                           ; salvaged. It holds the component that will
                           ; be the recipient of this component's
                           ; subcomponents when they are transferred.
     dom-version           ; If this component's dom has ever been sent to
                           ; the client, then this is equal to the
                           ; last version sent to the client, if the
                           ; dom hasn't changed since then. And is
                           ; larger than that version if the dom has
                           ; changed. Each time the dom is sent to the
                           ; client, this version will be sent, so the
                           ; client knows it is getting a new version.
     further-actions       ; A list of [function arg arg ...] calls that
                           ; need to be performed. The function will be
                           ; called with the atom, and the additional
                           ; arguments. (These actions are not actually
                           ; stored in the atom, but are added to the
                           ; data before it is stored, to request actions.)
     ])

;;; Components obey the following invariants:
;;; A. An unstarted component has an empty dismantling field.
;;;    Justification: Unstarted components start out with no
;;;    subcomponents, and as long as they are unstarted, they can only
;;;    gain current subcomponents.
;;; B. Every component in an id->subcomponent field will be unstarted
;;;    or active.
;;;    Justification: Components in id->subcomponent start out as
;;;    unstated, and only transition beyond active after they are moved
;;;    out.
;;; C. A component will never have subcomponents moved out of its
;;;    id->subcomponent field as long as the component's dom references
;;;    them.
;;;    Justification: A subcomponent is only moved when the
;;;    component's dom changes to no longer reference the
;;;    subcomponent.
;;; D. A defunct component has no subcomponents.
;;;    Justification. A component only starts to transition to defunct
;;;    when it is salvaging or finalizing has no subcomponents. And
;;;    salvaging and finalizing components can never add
;;;    subcomponents, so there will still be no subcomponents when the
;;;    transition to defunct finishes.

;;; E. Every component that isn't defunct or transitioning to defunct
;;;    will be a subcomponent of some other component that isn't defunct
;;;    (or be a root of the dom manager).
;;;     Justification: There are only two cases where a component is
;;;     disconnected:
;;;     *  It is defunct, so by D, it has no subcomponents, so
;;;        removing it can't eliminate paths to other components.
;;;     *  Its parent field was changed to become a sibling of its old
;;;        parent, then it was added to the id->subcomponents of its new
;;;        parent, and then removed from its old parent's finalizing
;;;        components. At each point, there is a path to the
;;;        component. (If the addition to its parent fails, it reverts
;;;        its parent back to its old parent and then starts finalizing.)
;;; F. For any sequence of relative-ids, and any starting component,
;;;    all paths starting from there and going through successive
;;;    subcomponents that follow that sequence of relative-ids will
;;;    traverse the same active components, in the same order.
;;;    (The paths might diverge when they go to non-active components,
;;;    but will reconverge when they reach an active component again.)
;;;    Justification: This condition starts out true for new
;;;    components, and there are only five points in the code that
;;;    add, remove, or move components, or activate or deactivate
;;;    them.
;;;    *  A dom update arrives:
;;;       This may make some current subcomponents transition to
;;;       quiescing, but that doesn't affect who their parent is, so
;;;       it preserves the invariant. And the new subcomponents are
;;;       not active and have no subcomponents, so they can't violate
;;;       it since no paths extend past them. And this all happens
;;;       atomically.
;;;    *  One of a component's current subcomponents is scheduled to
;;;       become active:
;;;       The scheduling only happens in code that atomically checks
;;;       that the component is active and has no quiescing
;;;       subcomponents. The activation itself is deferred; when it
;;;       runs it re-checks only that the subcomponent is still
;;;       unstarted, so a subcomponent is activated at most once. The
;;;       subcomponent occupies a fixed position in its parent's
;;;       id->subcomponent, and quiescing components are never active,
;;;       so activating it only extends paths through it and beyond,
;;;       preserving the invariant.
;;;    *  A component is transferred from another component:
;;;       This is only done when both components have the same
;;;       grandparent, and neither's parent is active, so the
;;;       condition is maintained, since the path changes only go
;;;       through non-active components.
;;;       This is the case even though the transfer happens by first
;;;       adding to the recipient component and then removing from the
;;;       donor component. At one point there are multiple paths, but
;;;       neither parent can become activated during the
;;;       transition. The donor can't because there is no transition
;;;       from salvaging to active. And the recipient can't because
;;;       the donor is in the quiescing set of the grandparent, and
;;;       that set being non-empty prevents starting activation of the
;;;       grandparent's current components.
;;;    *  A quiescing component transitions to not be active:
;;;       Making a component not be active can't invalidate the
;;;       invariant, since the paths don't change, and whenever an
;;;       active component would satisfy it, so would an inactive one.
;;;    *  A component is disconnected from its parent.
;;;       Since this only removes paths, it can't invalidate the
;;;       invariant, which universally quantifies over paths.
;;; G. There is at most one active component for any client id, and it
;;;    will stay active as long as it is relevant.
;;;    (You don't want multiple active components with the same
;;;    client-id, because their version can keep increasing, so there
;;;    is no way to ensure that the client will end up with the
;;;    current version.)
;;;    Justification: Follows immediately from C, E and F.

(defmethod print-method ComponentData [s ^java.io.Writer w]
  ;; Avoid huge print-outs.
  (.write w (str "<ComponentData>" (:client-id s))))

(defn component-data? [c]
  (= (type c) ComponentData))

(defn component-atom? [c]
  (and (= (type c) clojure.lang.Atom)
       (component-data? @c)))

(defn component-data-state
  "This function takes a component's component-data and returns which
  state of its life cycle that it is in."
  [component-data]
  (or (:dismantling-state component-data)
      (if (nil? (:dom-R component-data))
        :unstarted
        :active)))

;;; The information for interfacing between the client and the
;;; components is stored in an atom, containing a record with these
;;; fields. By using a record, we can define our own print method to
;;; avoid dumping this out when printing every component.
(defrecord DOMManagerData
    [root-components    ; A map from the client id of each root component
                        ; to its component atom. Not all components with
                        ; fixed client ids need to be here, just the roots.
     highest-version    ; The highest version number of any dom we have sent
                        ; to the client. Any new component starts out with a
                        ; version number one higher, because we might have
                        ; forgetten about it and then reconstructed it, all
                        ; while the client kept ahold of it. This way, our
                        ; next version will be larger that whatever the
                        ; client has.
   components-to-send   ; A priority queue of components that may have a dom
                        ; version that the client hasn't acknowledged
                        ; yet.  The components are prioritized by
                        ; depth (lower earlier), and they are sent to
                        ; the client in that order, so it will get
                        ; higher level doms before it is sent their
                        ; sub-doms.
                        ; The dom of each client here will be sent
                        ; under that client's id, so only non-elided
                        ; components show up here. When an elided
                        ; component gets a dom update, it adds its
                        ; elided-from component here, after
                        ; incrementing that component's dom version.
                        ; We record component atoms, rather than ids,
                        ; because it is possible to temporarily have
                        ; several component atoms with the same id,
                        ; all for the same component, until obsolete
                        ; ones get cleaned up. This way, removing an
                        ; obsolete component atom from the queue will
                        ; never take out a live one.
     calculator-data    ; The calculator data we use. (Currently, we only
                        ; use its queue.)
     client-lock        ; An atom used for locking access while doing a
                        ; client interatcion.
                        ; TODO: !!! Is this even necessary? Everything
                        ; done under the lock seems monotonic.
     mutable-store      ; The mutable store that holds the data the doms
                        ; rely on.
     further-actions    ; A list of [function arg arg ...] calls that
                        ; need to be performed. The function will be
                        ; called with the atom, and the additional
                        ; arguments. (These actions are not actually
                        ; stored in the atom, but are added to the
                        ; data before it is stored, to request actions.)
     ])

(defmethod print-method DOMManagerData [s ^java.io.Writer w]
  ;; Avoid huge print-outs.
  (.write w "<DOMManagerData>"))

;;; We build up client ids from a sequence of :relative-id values,
;;; which are either a keyword, an item id, or a sequence of those
;;; things. Since client ids must be strings, we turn all the pieces
;;; in the sequence of relative ids into strings (prefixing the names
;;; of keywords with "K"), concatenate the subparts of any
;;; sub-sequences with ".", then concatenate the overall sequence with
;;; "_".  For example, the sequence [:a [<id 2> :b]] becomes "Ka_2.Kb"

(defn valid-id-subpart?
  [id]
  (if (keyword? id)
    (and
     ;; Only characters allowed in all HTML versions, and
     ;; not one of our separators between parts.
     (re-matches #"^[a-zA-Z0-9\-]*$" (name id))
     ;; Not a character that an item id's representation
     ;; could start with.
     (not (re-matches #"^[0-9IM].*" (name id))))
    (item-id? id)))

(defn valid-relative-id? [id]
  (every? valid-id-subpart? (if (sequential? id) id [id])))

(defn concatenate-client-id-parts [client-id-parts]
  (clojure.string/join "_" client-id-parts))

(defn split-client-id-parts [client-id]
  (clojure.string/split client-id #"_"))

(defn concatenate-client-id-subparts [client-id-subparts]
  (clojure.string/join "." client-id-subparts))

(defn split-client-id-subparts [client-id-part]
  (clojure.string/split client-id-part #"\."))

(defn id-subpart->client-id-subpart
  "Turn a subpart of a :relative-id to its client form."
  [id]
  (cond (keyword? id) (name id)  ; ":" was illegal until HTML5.
        (item-id? id) (id->string id)
        true (assert false (str "unknown relative id subpart:"
                                [(type id) id]))))

(defn client-id-subpart->id-subpart
  "Turn a subpart of a client id into a :relative-id"
  [client-id-subpart]
  (if (and (string? client-id-subpart)
           ;; Item id numbers can start with any of these letters.
           (re-matches #"^[0-9IM].*" client-id-subpart))
    (string->id client-id-subpart)
    (keyword client-id-subpart)))

(defn relative-id->client-id-part
  "Turn a :relative-id to its client form."
  [id]
  (concatenate-client-id-subparts
   (map id-subpart->client-id-subpart (if (sequential? id) id [id]))))

(defn client-id-part->relative-id
  "Turn a part of a client id into a :relative-id"
  [client-id-part]
  (let [subparts (map client-id-subpart->id-subpart
                      (split-client-id-subparts client-id-part))]
    (if (= (count subparts) 1) (first subparts) (vec subparts))))

(defn subcomponent-client-id
  "Given the client-id of a component, and the relative-id of one of its
  subcomponents, return the client-id of the subcomponent."
  [containing-client-id relative-id]
  (concatenate-client-id-parts [containing-client-id
                                (relative-id->client-id-part relative-id)]))

(defn client-id->relative-ids
  "Given a string representation of a client id, return the relative ids."
  [client-id]
  (vec (map client-id-part->relative-id (split-client-id-parts client-id))))

(defn relative-ids->client-id
  "Given a sequence of relative ids, return a string representation that
  can be passed to the client. This is not used in dom-manager, but
  actions uses it."
  [ids] (assert (not (some nil? ids)) ids)
  (concatenate-client-id-parts (map relative-id->client-id-part ids)))

(defn make-component-atom
  "Given a component specification, create a component data atom. The
  component must not be transitioned from the :unstarted to the :active
  state until it is recorded in the id->subcomponent of its containing
  component. That is handled by activate-component."
  [specification dom-manager parent client-id depth elided-from]
  (assert (map? specification))
  (assert (instance? DOMManagerData @dom-manager))
  (atom
   (map->ComponentData
    {:dom-manager dom-manager
     :parent parent
     :dom-specification specification
     :client-id client-id
     :elided-from elided-from
     :depth depth
     :dom-version nil})))

(defn reuse-or-make-component-atom
  "Given the particulars for a component, plus an existing component atom,
  return the existing atom if it matches the particulars, otherwise
  make a new one and return it."
  [specification dom-manager new-parent new-client-id new-depth new-elided-from
   old-component-atom]
  (if (when old-component-atom
        (let [{:keys [dom-specification elided-from depth client-id parent]}
              @old-component-atom]
          (and (= dom-specification specification)
               (= depth new-depth)
               (= client-id new-client-id)
               ;; TODO: !!! Add this once parent gets updated.
               ; (= parent new-parent)
               ;; We don't currently update the elision in the
               ;; component atom, so if the elision has changed, we
               ;; need a new one.
               (= elided-from new-elided-from))))
    old-component-atom
    (make-component-atom
     specification dom-manager new-parent new-client-id new-depth
     new-elided-from)))

(defn increment-if-non-nil
  [n]
  (when n (inc n)))

(def handle-dom-change)

(defn dom-calculator-callback
  "This is the callback for the reporter that calculates the dom."
  [& {:keys [key]}]
  (handle-dom-change key))

(defn deactivate-dom-R
  "Given the atom's dom-R, Remove our callback to it. That callback
  should be the reporter's only attendee, so it should stop updating
  when we remove the callback."
  [component-atom dom-R]
  (when (reporter? dom-R)
    (remove-attendee! dom-R component-atom)))

(defn activate-dom-R
  "Give the atom's dom-R its calculator-data, and set up a callback for
  when its value changes.
  This can't be done at the time the reporter is created, as that
  happens during the component atom's activation, inside a swap!. The
  swap!'s function might run several times, creating a new reporter
  each time, and we only want to activate the one that actually ended
  up getting stored in the atom."
  [component-atom]
  (let [{:keys [dom-R dom-manager]} @component-atom
        calculator-data (:calculator-data @dom-manager)]
    (when dom-R
      (if (reporter? dom-R)
        (do
          (propagate-calculator-data! dom-R calculator-data)
          (set-attendee-and-call-if-valid!
           dom-R component-atom (* 10 (:depth @component-atom))
           [value-category] ; We don't care when doms go invalid.
           dom-calculator-callback)
          ;; It is possible that the component was already
          ;; deactivated, and we are running late. So we could have
          ;; activated a dom-R that should be deactivated. Detect
          ;; that, and fix it. It is good enough to check just this
          ;; once, because once deactivated, a component can't be
          ;; activated again.
          (when (not (:dom-R @component-atom))
            (deactivate-dom-R component-atom dom-R)))
        ;; Our dom-R is a constant. We need to handle its value just this once.
        (handle-dom-change component-atom)))))

(defn activate-component
  "Make a reporter to calculate the component's DOM, and activate it.
  This can't be done at the time the component-atom is created, as
  that typically happens during a dom update for this component's
  containing component, inside a swap!. The swap!'s function might run
  several times, creating a new component-atom each time, and we only
  want to activate the one that actually ended up getting used by the
  containing component."
  [component-atom]
  (swap-and-act!
   component-atom
   (fn [component-data]
     (if (= (component-data-state component-data) :unstarted)
       (let [{:keys [dom-specification dom-manager]} component-data
             {:keys [mutable-store]} @dom-manager
             dom-R ((dom-renderer dom-specification)
                    dom-specification mutable-store)]
         (-> component-data
             (assoc :dom-R dom-R)
             (update-new-further-action activate-dom-R component-atom)))
       component-data))))

(def finalize)

(def remove-from-components-to-send)

(def update-next-step-when-dismantling-empty)

(defn remove-from-parent
  "Given a component that is in state :defunct, remove it from its
  parent's id->subcomponent and dismantling fields. If that leaves
  dismantling empty, and changes the emptyness of either field, do the
  next step. A root component has no parent, so there is nothing to do."
  [component]
  (let [component-data @component
        parent (:parent component-data)
        id (:relative-id (:dom-specification component-data))]
    (assert (= (component-data-state component-data) :defunct))
    (when parent
      (swap-and-act!
       parent
       (fn [parent-data]
         (let [dismantling (:dismantling parent-data)
               id->subcomponent (:id->subcomponent parent-data)
               remove-id? (= (get id->subcomponent id) component)
               new-dismantling (disj dismantling component)
               new-id->subcomponent (cond-> id->subcomponent
                                      remove-id? (dissoc id))]
           (cond-> (assoc parent-data
                          :dismantling new-dismantling
                          :id->subcomponent new-id->subcomponent)
             (and (empty? new-dismantling)
                  (or (seq dismantling)
                      (and (seq id->subcomponent)
                           (empty? new-id->subcomponent))))
             (update-next-step-when-dismantling-empty parent))))))))

(defn remove-from-dismantling
  "Given a component and a parent, remove the component from the
  parent's dismantling field. If that leaves the parent with no
  subcomponents or dismantling components and it is :salvaging or
  :finalizing, switch it to :defunct and remove it from its parent too."
  [component parent]
  (swap-and-act!
   parent
   (fn [parent-data]
     (let [dismantling (:dismantling parent-data)
           new-dismantling (disj dismantling component)]
       (cond-> (assoc parent-data :dismantling new-dismantling)
         (and (empty? new-dismantling) (seq dismantling))
         (update-next-step-when-dismantling-empty parent))))))

(defn revert-parent-and-finalize
  "If the component's parent is presumed-parent, point it back at parent
  and set a further action to finalize the component. Used when a parent
  change could not complete."
  [component presumed-parent parent]
  (swap-and-act!
   component
   (fn [component-data]
     (if (= (:parent component-data) presumed-parent)
       (-> component-data
           (assoc :parent parent)
           (update-new-further-action finalize component parent))
       component-data))))

(defn attach-to-new-parent
  "Further action for change-parent, run after the component's parent
  has been pointed at future-parent. If all the conditions for
  belonging to the future parent are met, we add the component to
  future-parent's id->subcomponent, and set a future action to remove
  the component from its current parent. Otherwise, we revert the
  component's parent to current-parent and finalize it."
  [component current-parent future-parent common-grandparent]
  (let [id (:relative-id (:dom-specification @component))]
    (swap-and-act!
     future-parent
     (fn [future-parent-data]
       (if (and (= (:parent future-parent-data) common-grandparent)
                (= :unstarted (component-data-state future-parent-data))
                (not (get (:id->subcomponent future-parent-data) id))
                (not (some #(= id (:relative-id (:dom-specification @%)))
                           (:dismantling future-parent-data))))
         (-> future-parent-data
             (assoc-in [:id->subcomponent id] component)
             (update-new-further-action
              remove-from-dismantling component current-parent))
         (update-new-further-action
          future-parent-data
          revert-parent-and-finalize component future-parent current-parent)))
     )))

(defn change-parent
  "Move a component from current-parent to future-parent, which are both
  expected to be subcomponents of common-grandparent. Further, the
  component must be in dismantling of its current-parent, not in its
  id->subcomponents, because we may need to finalize it, which is only
  allowed for components in dismantling.
  This is done in three steps, each on a different component:
    * The parent field of the component is changed.
    * The component is added to the id->subcomponent of the new parent.
    * The component is removed from the id->subcomponent of the old parent.
  Each of these steps requires a separate swap-and-act!, to modify its
  component. And since other threads can be acting on the components,
  each step must check that nothing has invalidated the requirements
  of its change. If a step succeeds, it sets up a further action to
  take the next step, while if it fails, it sets up a further action
  to undo any previous steps, if necessary.
  This function does the first step."
  [component current-parent future-parent common-grandparent]
  (swap-and-act!
   component
   (fn [component-data]
     (if (and (= (:parent component-data) current-parent)
              (#{:unstarted :active} (component-data-state component-data)))
       (-> component-data
           (assoc :parent future-parent)
           (update-new-further-action
            attach-to-new-parent
            component current-parent future-parent common-grandparent))
       component-data))))

(defn deactivate-component-data
  "Given a component-data and its atom, clear everything related to its
  being active."
  [component-data component]
  (let [{:keys [dom-R dom-specification]} component-data]
    (-> component-data
        (assoc :dom-R nil
               :elided-from nil
               :dom-specification (select-keys dom-specification
                                               [:relative-id]))
        (update-new-further-action
            remove-from-components-to-send
            (:dom-manager component-data) component)
        (cond-> dom-R
          (update-new-further-action deactivate-dom-R component dom-R)))))

(defn approximately-deterministic-sort
  "When we are about to take an action over each a set of components, we
  do the actions in an approximately deterministic order, to reduce
  the indeterminancy of unit tests. We support that by sorting the
  components by their relative-id.
  This is not needed for correctness."
  [components]
  (sort-by (fn [c] (:id (:relative-id (:dom-specification @c))))
           components))

(defn finalize
  "Switch the component to :finalizing, and start finalizing all its
  subcomponents. If the component's parent is no longer
  presumed-parent, the component has been transferred elsewhere, so
  make no changes, some other process is now in charge. Likewise, if
  the component is already finalizing or defunct, make no changes; it
  is already being torn down."
  [component presumed-parent]
  (swap-and-act!
   component
   (fn [component-data]
     (if (or (not= (:parent component-data) presumed-parent)
             (#{:finalizing :defunct} (component-data-state component-data)))
       component-data
       (let [{:keys [dom-manager id->subcomponent dismantling]} component-data
             ;; All the subcomponents need to be in dismantling, since we
             ;; will get rid of all of them.
             new-dismantling (not-empty (into (set dismantling)
                                              (vals id->subcomponent)))]
         (-> component-data
             (assoc :dismantling-state :finalizing
                    :id->subcomponent nil
                    :dismantling new-dismantling)
             (deactivate-component-data component)
             (update-new-further-actions
              (map (fn [subcomponent] [finalize subcomponent component])
                   (approximately-deterministic-sort new-dismantling)))
             (update-next-step-when-dismantling-empty component)))))))

(defn transfer-subcomponents
  "Given a donor component which is in state salvaging, whose
  salvage-recipient is a recipient component in state unstarted that
  has the same parent as the donor, move the donor's id->subcomponent
  entries into its dismantling field. Then, for each of those
  subcomponents, if the recipient doesn't already have a subcomponent
  with the same relative-id, try to transfer the component to the
  recipient. Finally, finalize any of them that are still in the
  donor's dismantling field (which must be because they couldn't be
  transferred)."
  [donor]
  (let [[transferable-id->component donor-parent recipient]
        (swap-control-return!
         donor
         (fn [{:keys [parent salvage-recipient id->subcomponent dismantling]
               :as donor-data}]
           (if (and (= (component-data-state donor-data) :salvaging)
                    (empty? dismantling))
             [(-> donor-data
                  (assoc :id->subcomponent nil
                         :dismantling (set (vals id->subcomponent))))
              [id->subcomponent parent salvage-recipient]]
             [donor-data [nil parent salvage-recipient]])))
        {recipient-id->subcomponent :id->subcomponent} @recipient]
    (doseq [[id component] transferable-id->component]
      (when-not (contains? recipient-id->subcomponent id)
        (change-parent component donor recipient donor-parent)))
    ;; Remove any components that couldn't be transferred.
    (let [donor-dismantling (:dismantling @donor)]
      (doseq [component (vals transferable-id->component)]
        (when (contains? donor-dismantling component)
          (finalize component donor))))
    ;; The donor shold be defunct now.
    (swap-and-act!
     donor
     (fn [donor-data]
       (update-next-step-when-dismantling-empty donor-data donor)))))

(defn salvage
  "If the component is :active or :unstarted, switch it to :salvaging,
  deactivate it, and set it up to do its next step (transferring its
  components) if it is ready to."
  [component recipient]
  (swap-and-act!
   component
   (fn [component-data]
     (if (#{:active :unstarted} (component-data-state component-data))
       (-> component-data
           (assoc :dismantling-state :salvaging
                  :salvage-recipient recipient)
           (deactivate-component-data component)
           (update-next-step-when-dismantling-empty component))
       component-data))))

(defn pair-and-salvage-or-finalize
  "Given a set of dismantling components and an id->subcomponent map of
  active components, for each dismantling component look for an active
  component with the same relative-id. If there is one and it is
  :unstarted, salvage the dismantling component into it; otherwise
  finalize the dismantling component."
  [dismantling-components id->active-subcomponent presumed-parent]
  (doseq [dismantling (approximately-deterministic-sort dismantling-components)]
    (let [id (:relative-id (:dom-specification @dismantling))
          active (get id->active-subcomponent id)]
      (if (and active (= (component-data-state @active) :unstarted))
        (salvage dismantling active)
        (finalize dismantling presumed-parent)))))

(defn update-next-step-when-dismantling-empty
  "Given a component's data and its atom, update the data to take the
  next step for the component, which must not be :unstarted. If it
  still has dismantling subcomponents, do nothing. Otherwise, if it is
  :active, set a further action to activate each of its current
  subcomponents.  If it has no subcomponents, switch it to :defunct
  and set a further action to remove it from its parent. If it does
  still have subcomponents, it must be :salvaging, so set a further
  action to transfer them."
  [component-data component]
  (let [state (component-data-state component-data)]
    (assert (not= state :unstarted))
    (if (seq (:dismantling component-data))
      component-data
      (cond
        (= state :active)
        (update-new-further-actions
         component-data
         (map (fn [subcomponent] [activate-component subcomponent])
              (vals (:id->subcomponent component-data))))
        (empty? (:id->subcomponent component-data))
        (-> component-data
            (assoc :dismantling-state :defunct)
            (update-new-further-action remove-from-parent component))
        :else
        (do (assert (= state :salvaging))
            (update-new-further-action
             component-data transfer-subcomponents component))))))

(defn subcomponent-specifications
  "Given a dom that may contain subcomponents, return a vector of their
  specifications."
  [dom]
  (when (vector? dom)
    (if (= (first dom) :component)
      [(second dom)]
      (mapcat subcomponent-specifications dom))))

(defn get-id->subcomponent-specifications
  "Given a dom that may contain subcomponents, return a map
  from :relative-id to their specifications."
  [dom]
  (let [specs (subcomponent-specifications dom)
        answer (zipmap (map :relative-id specs) specs)]
    (assert (= (count answer) (count specs))
            [dom (vec specs)])
    answer))

(defn remove-from-components-to-send
  [dom-manager component-atom]
  ;; There is no need to check the component's current state, as this
  ;; function is only called after a component is made inactive, and a
  ;; component can never leave that state.
  (swap! dom-manager
         (fn [data] (dissoc-in data [:components-to-send component-atom]))))

(defn add-to-components-to-send
  [dom-manager component-atom]
  (when (= (component-data-state @component-atom) :active)
    (swap! dom-manager
           (fn [manager-data]
             (assoc-in manager-data [:components-to-send component-atom]
                       (:depth @component-atom))))
    ;; Since we copied data from one atom to another, we would
    ;; normally have to operate inside a with-latest-value, checking
    ;; that the component atom was still activate, to make sure we
    ;; didn't step on some thread with more recent data. But since a
    ;; component can only transition from active to inactive, and
    ;; never back, it is sufficient to check once that it hasn't gone
    ;; inactive.
    (when (not= (component-data-state @component-atom) :active)
      (remove-from-components-to-send dom-manager component-atom))))

(defn process-dom-ready-for-client
  "Record in the dom manager that the client needs to hear about our
  dom. If we are elided, that means it will be given our dom, but
  under the key of our elided-from containing component."
  [dom-manager component-atom]
  (let [elided-from (:elided-from @component-atom)]
    (if elided-from
      ;; Since the component we are elided-from sends our dom to the
      ;; client, that compoment's dom, as seen by the client, has
      ;; logically changed. So we need to increment that component's
      ;; dom version, and then add that component to the ones to send
      ;; to the client. We don't add ourselves to be sent to the
      ;; client, since our dom will be sent by our elided-from component.
      (do (swap! elided-from #(update % :dom-version increment-if-non-nil))
          (add-to-components-to-send dom-manager elided-from))
      ;; Our dom version was already incremented when we heard about
      ;; the new dom. We just have to add ourselves to the dom
      ;; manager's outgoing queue.
      (add-to-components-to-send dom-manager component-atom))))

(defn update-dom
  "Update the component data to reflect having the given dom,
  and start up computations to handle consequences of that."
  [component-data component-atom dom]
  (if (not= (component-data-state component-data) :active)
    component-data
    (let [{:keys [dom-manager client-id depth]}
          component-data
          subcomponent-elided-from (when (= (first dom) :component)
                                     (or (:elided-from component-data)
                                         component-atom))
          old-id->subcomponent (or (:id->subcomponent component-data) {})
          subcomponent-specs (get-id->subcomponent-specifications dom)
          subcomponent-ids (keys subcomponent-specs)
          subcomponents (map (fn [id]
                               (let [spec (subcomponent-specs id)
                                     {:keys [relative-id]} spec]
                                 (reuse-or-make-component-atom
                                  spec
                                  dom-manager
                                  component-atom
                                  (subcomponent-client-id client-id relative-id)
                                  (inc depth)
                                  subcomponent-elided-from
                                  (old-id->subcomponent id))))
                             subcomponent-ids)
          id->subcomponent (zipmap subcomponent-ids subcomponents)
          id->new-subcomponent (into {} (filter
                                         (fn [[id subcomponent]]
                                           (not= subcomponent
                                                 (old-id->subcomponent id))))
                                     id->subcomponent)
          dropped-subcomponents (map old-id->subcomponent
                                     (filter #(not= (id->subcomponent %)
                                                    (old-id->subcomponent %))
                                             (keys old-id->subcomponent)))
          dismantling (not-empty (into (set (:dismantling component-data))
                                       dropped-subcomponents))
          ;; When a component gets a new dom, we can't activate its
          ;; new sub-components until we have deactivated all its no
          ;; longer needed sub-components. Otherwise, we could have
          ;; more than one sub-component active with the same client
          ;; id, and the dismantling one may end up getting sent to
          ;; the client with a later dom-version than the current one
          ;; has, precluding the client from accepting the current
          ;; one's dom.  So in that case, we set up a task to first
          ;; deactivate the old ones. Once they are all deactivated, a
          ;; task will fire to activate the new ones.
          follow-ons (if dismantling
                       [[pair-and-salvage-or-finalize
                         dismantling id->new-subcomponent component-atom]]
                       ;; We activate all current subcomponents,
                       ;; because we might be reusing a subcomponent
                       ;; that was transferred in from a salvaged
                       ;; donor while it was still inactive.
                       ;; activate-component is a no-op on
                       ;; already-active ones.
                       (map (fn [component]
                              [activate-component component])
                            (vals id->subcomponent)))]
      ;; Check that each subcomponent has a different id. Otherwise, two
      ;; components will share an id, which will mess up communications
      ;; with the client.
      (assert (= (count subcomponent-ids) (count (set subcomponent-ids)))
              subcomponent-ids)
      (-> component-data
          (assoc :id->subcomponent id->subcomponent
                 :dismantling dismantling)
          (update :dom-version increment-if-non-nil)
          (update-new-further-action
           process-dom-ready-for-client dom-manager component-atom)
          (update-new-further-actions follow-ons)))))

(defn handle-dom-change
  [component-atom]
  ;; It is possible for a dom update to arrive at a disabled
  ;; component, which has no :dom-R.  So we tolerate that.
  (when-let [reporter (:dom-R @component-atom)]
    (with-latest-value [dom (reporter-value-when-valid reporter)]
      (when dom
        (swap-and-act!
         component-atom
         #(let [result (update-dom % component-atom dom)]
            ;; Check for problems where an update to the component-data, like
            ;; a dissoc, turned it into a map.
            (assert (instance? ComponentData result))
            result))))))

(defn make-dom-manager
  "Return a new dom-manager object for doms over the store."
  [mutable-store calculator-data]
  (assert (instance? cosheet.calculator.CalculatorData calculator-data))
  (assert (mutable-store? mutable-store))
   (atom
    (map->DOMManagerData
     {:root-components {}
      :highest-version 0
      :components-to-send (priority-map)
      :calculator-data calculator-data
      :mutable-store mutable-store
      :client-lock (atom 0)
      :further-actions nil})))

(defn client-id->component
  "Returns the component for the given client id."
  [manager-data client-id]
  (let [id-sequence (client-id->relative-ids client-id)
        root ((:root-components manager-data) (first id-sequence))
        result (reduce
                (fn [component id]
                  (when component
                    (when-let [id->subcomponent (:id->subcomponent @component)]
                      (id->subcomponent id))))
                root
                (rest id-sequence))]
    (when result
      (assert (= (:client-id @result) client-id))
      result)))

(defn components-up-to-client-id
  "Return a chain of components from the root to the active component
  with the specified client-id, or nil if there is no such active
  component."
  [manager-data client-id]
  (let [id-sequence (client-id->relative-ids client-id)
        root ((:root-components manager-data) (first id-sequence))]
  ;; Intermediate components on the chain need not be active (an
  ;; active component can sit beneath dismantling or unstarted ones),
  ;; so we track candidate chains, extending each by every component
  ;; with the next relative-id, from the last component's
  ;; id->subcomponent or dismantling. As soon as an extension reaches
  ;; an active component we keep only that chain, since by the
  ;; invariants the active path is unique; so branching only persists
  ;; across non-active stretches. Further, the common all-active path
  ;; never branches so we typically run in linear time in the length
  ;; of the chain.
    (when root
      (let [chains
            ;; Run over each id, given the chains up to but not
            ;; including the id, return the chains up to and including
            ;; the id.
            (reduce
             (fn [chains id]
               ;; Run over each chain and return all its extensions
               ;; that include the id.
               (reduce
                (fn [acc chain]
                  (let [{:keys [id->subcomponent dismantling]} @(peek chain)
                        c (id->subcomponent id)]
                    (if (and c (= (component-data-state @c) :active))
                      ;; Common case: id->subcomponent has an active
                      ;; component. It is the whole result for this step,
                      ;; so skip the slower dismantling scan and stop
                      ;; extending the other chains.
                      (reduced [(conj chain c)])
                      ;; c, if any, is not active. Scan dismantling,
                      ;; dereferencing each component just once to check
                      ;; both its relative-id and its state. On an active
                      ;; match, shortcut both this scan and the outer
                      ;; reduce over chains (hence the doubled reduced).
                      (reduce
                       (fn [acc d]
                         (let [dd @d]
                           (if (= (:relative-id (:dom-specification dd)) id)
                             (if (= (component-data-state dd) :active)
                               (reduced (reduced [(conj chain d)]))
                               (conj acc (conj chain d)))
                             acc)))
                       (if c (conj acc (conj chain c)) acc)
                       dismantling))))
                []
                chains))
             [[root]]
             (rest id-sequence))]
        ;; We only have a result if we ended on an active
        ;; component. In that case, there will be exactly one chain,
        ;; as well.
        (when (and (= (count chains) 1)
                   (= (component-data-state @(peek (first chains))) :active))
          (first chains))))))

(defn elided-subcomponent-chain
  "Return the sequence of sub-components starting from component-atom,
  following through each single elided-from sub-component."
  [component-atom]
  (loop [component component-atom
         chain []]
    (let [{:keys [id->subcomponent]} @component]
      (if (= (count id->subcomponent) 1)
        (let [subcomponent (first (vals id->subcomponent))]
          (if (:elided-from @subcomponent)
            (recur subcomponent (conj chain subcomponent))
            chain))
        chain))))

(defn action-data-for-component-chain
  "Reduce over a sequence of components to accumulate action-data."
  [chain action-data action immutable-store]
  (reduce (fn [ad component]
            (or (update-action-data-for-component
                 component ad action immutable-store)
                (reduced nil)))
          action-data
          chain))

(defn client-id->action-data
  "Returns the action data map for the component that generated the
  final dom for the given client id. Return nil if that fails, which
  only happens if the store is not consistent with the chain."
  [manager-data client-id action immutable-store]
  (when-let [components (components-up-to-client-id manager-data client-id)]
    (action-data-for-component-chain
     (concat components (elided-subcomponent-chain (last components)))
     {} action immutable-store)))

(defn preferred-selection
  "Return whichever of client-id-1 or client-id-2 has the longest
  common prefix with current-selection, breaking ties by returning
  the longer client id."
  [current-selection client-id-1 client-id-2]
  (let [prefix-length (fn [s]
                        (->> (map = current-selection s)
                             (take-while true?)
                             count))
        p1 (prefix-length client-id-1)
        p2 (prefix-length client-id-2)]
    (cond
      (> p1 p2) client-id-1
      (> p2 p1) client-id-2
      (>= (count client-id-1) (count client-id-2)) client-id-1
      :else client-id-2)))

(defn component-is-monitored?
  "Return true if the component, or any of its elided sub-components,
  targets one of the monitored ids or shows the content of one of them."
  [component-atom monitored-ids]
  (some (fn [c]
          (let [{:keys [target-item-id relative-id]}
                (:dom-specification @c)]
            ;; We check for target-item-id first, because relative-id can
            ;; be :content, or other markers that don't indicate an item.
            (when-let [target (or target-item-id relative-id)]
              (assert (not= :content target)
                      (:dom-specification @c))
              (some #{target} monitored-ids))))
        (cons component-atom (elided-subcomponent-chain component-atom))))

(defn adjust-subdom-for-client
  "Given a piece of dom and the client id for its component,
   adjust the dom to the form the client needs, turning subcomponents
   into [:component {:id ... class ...}]."
  [component-client-id dom]
  (if (vector? dom)
    (if (= (first dom) :component)
      (let [{:keys [relative-id class width]} (second dom)]
        [:component (cond-> {:id (subcomponent-client-id
                                  component-client-id relative-id)}
                      class (assoc :class class)
                      width (assoc :width width))])
      (vec (map (partial adjust-subdom-for-client component-client-id)
                dom)))
    dom))

(defn adjust-dom-for-client
  "Given a component's dom, adjust it to the form the client needs,
  turning subcomponents into [:component {:id ...}]. Don't set the
  overall dom's :id, or :version, though. They depend on whether this
  component gets elided."
  [component-atom dom]
   (when dom
     (let [{:keys [client-id]} @component-atom]
        (into [(first dom)
               (second dom)]
              (map (partial adjust-subdom-for-client client-id)
                   (rest (rest dom)))))))

(defn find-displayed-dom
  "Find the displayed dom for the given component: the dom of the last
  component in its elided-subcomponent chain, with classes from all
  preceding components added."
  [component-atom]
  (let [chain (elided-subcomponent-chain component-atom)
        full-path (cons component-atom chain)
        providing-component (last full-path)
        {:keys [dom-R] :as component-data} @providing-component
        ;; We get whatever the latest reporter value is. It is possible
        ;; that our reporter is temporarily invalid, in which case
        ;; we will have no dom for now.
        ;; Or the dom-R reporter may have gotten ahead of the current
        ;; dom-version number, but that is OK. Worst case, we will
        ;; send the same dom more than once, until the version number
        ;; catches up with it.
        dom (when (= (component-data-state component-data) :active)
              (reporter-value-when-valid dom-R))]
    (when dom
      ;; Add in any classes that were elided out.
      (reduce (fn [d c]
                (if-let [c-dom (reporter-value-when-valid (:dom-R @c))]
                  (add-attributes d (select-keys (dom-attributes c-dom)
                                                 [:class]))
                  d))
              (adjust-dom-for-client providing-component dom)
              (butlast full-path)))))

(defn prepare-dom-for-client
  "Given a component-atom, prepare its dom to send to the client."
  [component-atom]
  ;; We have to get the dom after getting the dom version. That's
  ;; because it's OK to assign an older version to a new dom, but bad
  ;; to assign a newer version to an old dom.
  (let [{:keys [client-id dom-version elided-from]} @component-atom]
    ;; We should normally never be called with an elided-from dom. But
    ;; the unit test can do that sometimes, since it can have a record
    ;; of a client-id that it is no longer getting.
    (when (not elided-from)
      (when-let [dom (find-displayed-dom component-atom)]
        (add-attributes dom {:id client-id :version dom-version})))))

(defn get-response-doms
  "Return a seq of doms for the client containing up to num components.
  Also, if any of the monitored ids are the :item-id or :relative-id of any
  of the components, return the client id of that component, preferring
  the one with the longest prefix overlap with current-selection and
  breaking ties by choosing the longer client id.
  Add dom-version to any components sent that don't have one yet. And
  finally, do the side-effect of updating :highest-version."
  [dom-manager monitored-ids current-selection num]
  ;; We run this function under a lock. This lets us move dom version
  ;; data between components and the dom manager without race
  ;; conditions, since this is the only function that moves this
  ;; information between them.
  (locking (:client-lock @dom-manager)
    (let [manager-data @dom-manager
          ;; This is the starting version for every dom we send the
          ;; client whose component doesn't have a version number yet.
          starting-dom-version (+ 1 (:highest-version manager-data))
          [response monitored-client-id highest-version]
          (loop [response []
                 monitored-client-id nil
                 highest-version starting-dom-version
                 components (keys (:components-to-send manager-data))]
            (if (or (>= (count response) num) (empty? components))
              [response monitored-client-id highest-version]
              (let [[component & remaining-components] components]
                (when (nil? (:dom-version @component))
                  ;; The client has never gotten a dom from this
                  ;; component. But it may have gotten doms from other
                  ;; components with the same id. So we set the
                  ;; component's version to one higher than the
                  ;; highest the client has received so far. That way,
                  ;; the client will recognize its dom as new.
                  (swap! component
                         (fn [component-data]
                           (update component-data :dom-version
                                   #(or % starting-dom-version)))))
                (let [dom (prepare-dom-for-client component)
                      monitored (component-is-monitored? component monitored-ids)]
                  (recur
                   ;; The dom might be temporarily invalid. (It can't
                   ;; be permanently disabled, as disabling removes it
                   ;; from components-to-send, and adding to
                   ;; components-to-send checks for a disablement
                   ;; while the addition is taking place.)
                   (cond-> response
                     dom (conj dom))
                   (preferred-selection current-selection
                                        monitored-client-id
                                        (when monitored
                                          (:client-id @component)))
                   (max highest-version
                        (if dom (:version (dom-attributes dom)) 0))
                   remaining-components)))))]
      (swap! dom-manager
             (fn [manager-data]
               (-> manager-data
                   (update :highest-version
                           #(max % highest-version)))))
      [response monitored-client-id])))

(defn need-to-send-to-client-given-acknowledgement?
  "Return whether we need to send the component to the client, given a
  dom version that the client acknowledges getting."
  [component-atom ack-version]
  (let [{:keys [elided-from dom-version]} @component-atom]
    (and
     ;; We never send elided-from components; the client must be
     ;; acknowledging from when it wasn't elided.
     (not elided-from)
     (or
      ;; If there is no dom-version, the component must have been
      ;; reconstructed, and never sent anything to the client.
      (not dom-version)
      ;; If the component's dom version is later, the client hasn't
      ;; seen that version yet.
      (< ack-version dom-version)))))

(defn process-acknowledgements
  "Modify the the dom-manager to reflect the acknowledgements."
  [dom-manager acknowledgements]
  (locking (:client-lock @dom-manager)
    ;; Determine which of the acknowledged doms no longer need to be
    ;; sent to the client. Make a list of them and their acknowledged
    ;; versions.
    (let [components-to-remove
          (let [manager-data @dom-manager] ; Only used for client-id->component.
            (mapcat
             (fn [[client-id version]]
               (when-let [component-atom (client-id->component
                                          manager-data client-id)]
                 (when (not (need-to-send-to-client-given-acknowledgement?
                             component-atom version))
                   [[component-atom version]])))
             acknowledgements))]
      (swap!
       dom-manager
       (fn [manager-data]
         (-> manager-data
             (update :components-to-send
                     #(apply dissoc % (map first components-to-remove))))))
      ;; It's possible that a component got updated between the time
      ;; we decided that it needed to be removed and the swap! In that
      ;; case, we may have removed it incorrectly. So we go back
      ;; through the component data of the components we removed, and
      ;; see if they need to go back in the components-to-send.  We
      ;; use add-to-components-to-send, since it makes sure not to add
      ;; a component that has been deactivated, which would otherwise
      ;; stay in components-to-send forever.
      (let [components-to-add-back
            (filter #(apply need-to-send-to-client-given-acknowledgement? %)
                    components-to-remove)]
        (doseq [[component _] components-to-add-back]
          (add-to-components-to-send dom-manager component))))))

(defn add-root-dom
  "Add dom with the given specification to the dom-manager.
  This is how the manager is bootstrapped with top level doms."
  [dom-manager specification]
  (let [top-id (:relative-id specification)
        component (make-component-atom
                   specification dom-manager nil
                   (id-subpart->client-id-subpart top-id) 1 false)]
    (assert (keyword? top-id) (str top-id))
    (swap-and-act!
     dom-manager
     (fn [manager-data]
       (let [old-component (get-in manager-data [:root-components top-id])]
         (assert (nil? old-component)
                 ["Root component already exists" top-id])
         (-> manager-data
             (assoc-in [:root-components top-id] component)
             (update :highest-version inc)
             (update-new-further-action
              activate-component component)))))))

(defn remove-all-doms
   "Remove all the doms from the dom-manager. This will cause it to release
    all its reporters."
   [dom-manager]
   (swap-and-act!
    dom-manager
    (fn [manager-data]
      (reduce (fn [manager-data component]
                (update-new-further-action manager-data
                                           finalize component nil))
              (assoc manager-data
                     :root-components {}
                     :components-to-send (priority-map))
              (vals (:root-components manager-data))))))

;;; TODO: Make a function to copy the :components-to-send value to the
;;; dom manager.
(defn mark-component-tree-as-needed
  "Mark the component and all its descendants as needing to be sent to
  the client. Return a list of pairs of all those components with ready
  dom and their depth."
  [component-atom]
  (let [[depth dom subcomponents]
        (swap-control-return!
         component-atom
         (fn [component-data]
           [(assoc component-data :client-needs-dom
                   (not (:elided-from component-data)))
            [(:depth component-data)
             (reporter-value-when-valid (:dom-R component-data))
             (vals (:id->subcomponent component-data))]]))]
    (doall (concat (when dom [[component-atom depth]])
                   (mapcat mark-component-tree-as-needed subcomponents)))))

(defn add-components-to-send
  "Given a dom manager and a seq of pairs of [component, priority], add
  all the components to its components-to-send. Then remove any of
  them that are inactive. This handles the case where they were
  deactivated while we were adding them."
  [dom-manager components-and-depths]
  (swap! dom-manager
         (fn [data] (update data :components-to-send
                            #(reduce
                              (fn [priority-map [component depth]]
                                (if (:elided-from @component)
                                  priority-map
                                  (assoc priority-map component depth)))
                              %
                              components-and-depths))))
  (let [defunct (filter #(= (component-data-state @%) :defunct)
                        (keys (:components-to-send @dom-manager)))]
    (when (seq defunct)
      (swap! dom-manager
             (fn [data]
               (update data :components-to-send
                       #(apply dissoc % defunct)))))))

(defn request-client-refresh
  "Mark all components as needing to be sent to the client."
  [dom-manager]
  (let [manager-data @dom-manager
        components-and-depths (mapcat mark-component-tree-as-needed
                                      (vals (:root-components manager-data)))]
    (add-components-to-send dom-manager components-and-depths)))

(ns cosheet2.server.dom-manager
  (:require [clojure.data.priority-map :refer [priority-map]]
            (cosheet2 [task-queue :refer [add-task-with-priority]]
                      [reporter :refer [remove-attendee! set-attendee!
                                        set-attendee-and-call!
                                        reporter?
                                        reporter-value 
                                        reporter-value-when-valid valid?
                                        universal-category]]
                      [expression :refer [new-application category-change]]
                      [calculator :refer [propagate-calculator-data!]]
                      [store :refer [is-item-id? id->string string->id
                                     mutable-store?]]
                      [utils :refer [swap-control-return!
                                     swap-and-act!
                                     with-latest-value
                                     update-in-clean-up
                                     update-new-further-action
                                     update-new-further-actions
                                     dissoc-in
                                     call-pseudo-closure
                                     pseudo-closure-application]]
                      [hiccup-utils :refer [dom-attributes add-attributes
                                            into-attributes]])
            (cosheet2.server
             [render :refer [dom-renderer]]
             [action-data :refer [get-item-or-exemplar-action-data
                                  update-action-data-for-component]])))

(def verbose false)

;;; TODO: Mark some components as not being worth their descendants
;;; being saved. Once those are computed and sent to the client, they
;;; are thrown out. Whatever component is saved has to be marked as
;;; dependent on anything the subcomponents were dependent
;;; on. Whenever it needs to be recomputed, all their descendents have
;;; to be recomputed too.

;;; We record what needs to be rendered, and what it depends on.
;;; Whenever a piece of dom changes, we check all the sub-components
;;; it specifies, and update our information.

;;; As renderings are done, we update the client.

;;; The basic data structure is a component. It's like a reporter, except:
;;;   * It makes sub-components depend on its value, but its value
;;;     doesn't depend on them,
;;;   * It notifies the client of changes, not other code.

;;; A component is represented with an atom that holds a
;;; ComponentData, which contains information about one component we
;;; are tracking. We use ComponentData, rather than a map, so we can
;;; simplify print-out.
(defrecord ComponentData
    [;; These fields will never change once the component data is created
     dom-manager           ; Our dom manager.
     client-id             ; The id this component will have in the client.
                           ; It is the concatenation of the relative ids
                           ; of all the components on the path from the root
                           ; to here (with a little processing to avoid
                           ; ambiguity and HTML issues).
                           ; then client-id must be present.
     elided-from           ; If the dom of one component consists of
                           ; nothing but another component, then the
                           ; inner component is elided away, as far as
                           ; the client sees.  The elided component's
                           ; dom is what gets sent to the client, but
                           ; under the id of the containing dom. (That
                           ; dom's container refers to it by that id,
                           ; so that's the id that has to be senf.)
                           ; If elided-from present, our component is
                           ; elided, and elided-from is the nearest
                           ; non-elided containing component. Our dom
                           ; will be sent to the client as the dom of
                           ; that component.
     depth                 ; The depth of this component in the component
                           ; hierarchy, used to make parents get sent
                           ; to the client before their children.

     ;; This field normally doesn't change, but if the component has
     ;; been permanently disabled, this field is cleared
     dom-specification     ; The dom spec for this component.

     ;; These fields can change
     dom-R                 ; A reporter that calculates this component's dom.
                           ; This field is filled in when the
                           ; component is first activated, and is
                           ; cleared when it is deactivated. Those are
                           ; the only two times it changes.
     id->subcomponent      ; A map from :relative-id to the component data
                           ; of each sub-component. This is filled in once
                           ; the dom is computed, and can change if the dom
                           ; changes.
     obsolete-components   ; A seq of subcomponent component atoms that
                           ; need to be deactivated before any new
                           ; subcomponents can be activated. The issue
                           ; is that they might have the same client
                           ; id as a new subcomponent, and they
                           ; might send their dom to the client with a
                           ; higher version number than the current
                           ; subcomponent.
     dom-version           ; A monotonically increasing version number
                           ; for the current dom. It goes up every
                           ; time the dom changes.  It is sent by the
                           ; client, which uses it to acknowledge
                           ; which version they got.
     further-actions       ; A list of [function arg arg ...] calls that
                           ; need to be performed. The function will be
                           ; called with the atom, and the additional
                           ; arguments. (These actions are not actually
                           ; stored in the atom, but are added to the
                           ; data before it is stored, to request actions.)
     ])

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
  one of these three stages of its life cycle it is in.
     :created  The unchanging part of the component's data has been
               filled in, but the reporter that calculates its dom
               hasn't been made yet.
      :active  A reporter is running to update the component's dom
               whenever something it depends on changes.
    :inactive  This component's dom is no longer needed by the client.
               Either the client no longer needs a dom with this
               reporter's client id, or a different component
               atom is now in charge of calculating that dom. This
               component's reporter is no longer running, or is
               about to be shut down. It will never be active again.
  Each component goes through these three states, in this order; it
  never moves back to a previous stage."
  [component-data]
  (if (nil? (:dom-specification component-data))
    :inactive
    (if (nil? (:dom-R component-data))
      :created
      :active)))

;;; The information for interfacing between the client and the
;;; components is stored in an atom, containing a record with these
;;; fields. By using a record, we can define our own print method to
;;; avoid dumping this out when printing every component.
(defrecord DOMManagerData
    [root-components    ; A map from the client id of each root component
                        ; to its component atom. Not all components with
                        ; fixed client ids need to be here, just the roots.
 obsolete-components    ; A seq of root component atoms that
                        ; need to be deactivated before any new
                        ; root components can be activated. The issue
                        ; is that they might have the same client
                        ; id as a new root atom, and they
                        ; might send their dom to the client with a
                        ; higher version number than the new one.
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
     calculator-data    ; The calculator data we use. (Currently, we only use
                        ; its queue.)
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
     (not (re-matches #"^[0-9I].*" (name id))))
    (is-item-id? id)))

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
        (is-item-id? id) (id->string id)
        true (assert false (str "unknown relative id subpart:"
                                [(type id) id]))))

(defn client-id-subpart->id-subpart
  "Turn a subpart of a client id into a :relative-id"
  [client-id-subpart]
  (if (and (string? client-id-subpart)
           (re-matches #"[I0-9]" (subs client-id-subpart 0 1)))
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
  component must not be transitioned from the :created to the :active
  state until it is recorded in the id->subcomponent of its containing
  component. That is handled by activate-component."
  [specification dom-manager client-id depth elided-from]
  (assert (map? specification))
  (assert (instance? DOMManagerData @dom-manager))
  (atom
   (map->ComponentData
    {:dom-manager dom-manager
     :dom-specification specification
     :client-id client-id
     :elided-from elided-from
     :depth depth
     ;; Deactivation expects a dom version. And we can be deactivated
     ;; before we even get activated.
     :dom-version 0})))

(defn reuse-or-make-component-atom
  "Given the particulars for a component, plus an existing component atom,
  return the existing atom if it matches the particulars, otherwise
  make a new one and return it."
  [specification dom-manager new-client-id new-depth new-elided-from
   old-component-atom]
  (if (when old-component-atom
        (let [{:keys [dom-specification elided-from depth client-id]}
              @old-component-atom]
          (and (= dom-specification specification)
               (= depth new-depth)
               (= client-id new-client-id)
               ;; We don't currently update the elision in the
               ;; component atom, so if the elision has changed, we
               ;; need a new one.
               (= elided-from new-elided-from))))
    old-component-atom
    (make-component-atom
     specification dom-manager new-client-id new-depth new-elided-from)))

(def handle-dom-change)

(defn dom-calculator-callback
  "This is the callback for the reporter that calculates the dom."
  [& {:keys [key]}]
  (handle-dom-change key))

(defn deactivate-dom-R
  "Remove our callback to the atom's dom-R. That should be its only
  attendee, so it should stop updating at that point."
  [component-atom dom-R]
  (when (reporter? dom-R)
    (remove-attendee! dom-R component-atom)))

(defn activate-dom-R
  "Give the atom's dom-R its calculator-data, and set up a callback for
  when its value changes.
  This can't be done at the time the reporter is created, as that
  happens during the component atom's activation, inside a
  swap-control-return!. The swap-control-return!'s function might run
  several times, creating a new reporter each time, and we only want
  to activate the one that actually ended up getting stored in the
  atom."
  [component-atom]
  (let [{:keys [dom-R dom-manager]} @component-atom
        calculator-data (:calculator-data @dom-manager)]
    (when dom-R
      (if (reporter? dom-R)
        (do
          (propagate-calculator-data! dom-R calculator-data)
          (set-attendee-and-call!
           dom-R component-atom (* 10 (:depth @component-atom))
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
  Also set up the current dom version.
  This can't be done at the time the component-atom is created, as
  that typically happens during a dom update for this component's
  containing component, inside a swap-control-return!. The
  swap-control-return!'s function might run several times, creating a
  new component-atom each time, and we only want to activate the one
  that actually ended up getting used by the containing component."
  [component-atom]
  (swap-and-act!
   component-atom
   (fn [component-data]
     (let [{:keys [dom-specification dom-manager]} component-data]
       (if (= (component-data-state component-data) :created)
         (let [{:keys [mutable-store highest-version]} @dom-manager
               dom-R ((dom-renderer dom-specification)
                      dom-specification mutable-store)]
           (-> component-data
               (assoc :dom-R dom-R)
               (assoc :dom-version (+ 1 highest-version))
               (update-new-further-action activate-dom-R component-atom)))
         ;; The atom has already been activated. Don't do anything.
         component-data)))))

(def remove-from-components-to-send)

(defn deactivate-component
  "Deactivate the component and all its descendant components, and
  remove all its links to descendant components, so they can be GCed."
  [component-atom]
  (swap-and-act!
   component-atom
   (fn [component-data]
     (if (= (component-data-state component-data) :inactive)
       component-data ; This component has already been deactivated.
       (let [{:keys [id->subcomponent obsolete-components dom-R dom-manager]}
             component-data
             result (-> component-data
                        ;; Rather than dissoc, we assoc with nil, so we
                        ;; don't turn the record into a map.
                        (assoc :dom-specification nil
                               :id->subcomponent nil
                               :obsolete-components nil
                               :dom-R nil)
                        (update-new-further-actions
                         (map (fn [ca] [deactivate-component ca])
                              (concat (vals id->subcomponent)
                                      obsolete-components)))
                        (update-new-further-action
                         deactivate-dom-R component-atom dom-R)
                        (update-new-further-action
                         remove-from-components-to-send
                         dom-manager component-atom))]
         ;; Check for errors where we made it not be a ComponentData.
         (assert (instance? ComponentData result))
         result)))))

(defn deactivate-then-activate
  "The atom-with-obsolete must hold something with
  an :obsolete-components field. Deactivate all the components listed
  there, then make sure the dom manager's highest version is at least
  as big as their dom-versions, then remove the deactivated components
  from the field, and finally activate the components-to-activate.
  See the explanation in update-dom for why we need to deactivate
  obsolete components first, if they might be identified with the same
  client id as the a one. (It's OK if still newer components become
  obsolete later, because they will deactivate our new ones.)"
  [dom-manager atom-with-obsolete components-to-activate]
  (when-let [obsolete (:obsolete-components @atom-with-obsolete)]
    ;; First, deactivate the subcomponents so they won't send any more
    ;; messages to the client.
    (doseq [subcomponent obsolete]
      (deactivate-component subcomponent))
    ;; Now we can get the final dom versions for each of them and make
    ;; sure the dom-manager's highest version dom is at least that
    ;; high.
    (let [max-of-dom-versions
          (apply max (map (fn [component] (:dom-version @component))
                          obsolete))]
      (swap! dom-manager
             (fn [manager-data]
               (update manager-data :highest-version
                       #(max % max-of-dom-versions)))))
    ;; Next, remove these subcomponents from the list of obsolete
    ;; ones. (The set of obsolete ones might have changed from when we
    ;; started running.)
    (swap! atom-with-obsolete
           (fn [atom-data]
             (update atom-data :obsolete-components
                     #(seq (apply disj (set %) obsolete))))))
  ;; Now, we can safely active the waiting components, and we
  ;; are guaranteed that they will have higher numbers than what
  ;; they replaced. (It is possible that they have gone obsolete by
  ;; the time we get to here, but either they will have already been
  ;; deactivated, and activation will do nothing, or there is a
  ;; waiting task that will deactivate them.)
  (doseq [subcomponent components-to-activate]
    (activate-component subcomponent)))

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
    ;; didn't stepping on some thread with more recent data. But since
    ;; a component can only transition from active to inactive, and
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
      ;; dom version, and then add the component to the ones to send
      ;; to the client. We don't add ourselves to be sent to the
      ;; client, since our elided-from component will send our dom.
      (do (swap! elided-from #(update % :dom-version inc))
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
    (let [{:keys [obsolete-components dom-manager client-id depth]}
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
                                  (subcomponent-client-id client-id relative-id)
                                  (inc depth)
                                  subcomponent-elided-from
                                  (old-id->subcomponent id))))
                             subcomponent-ids)
          id->subcomponent (zipmap subcomponent-ids subcomponents)
          new-subcomponents (map id->subcomponent
                                 (filter #(not= (id->subcomponent %)
                                                (old-id->subcomponent %))
                                         subcomponent-ids))
          dropped-subcomponents (map old-id->subcomponent
                                     (filter #(not= (id->subcomponent %)
                                                    (old-id->subcomponent %))
                                             (keys old-id->subcomponent)))
          obsolete (seq (into (set obsolete-components) dropped-subcomponents))
          ;; When a component gets a new dom, we can't activate its
          ;; new sub-components until we have deactivated all its no
          ;; longer needed sub-components. Otherwise, we could have
          ;; more than one sub-component active with the same client
          ;; id, and the obsolete one may end up getting sent to the
          ;; client with a later dom-version than the current one has,
          ;; precluding the client from accepting the current one's
          ;; dom.  So in that case, we set up a task to first
          ;; deactivate the old ones, then activate the new ones.
          follow-ons (if obsolete
                       [[deactivate-then-activate
                         dom-manager component-atom new-subcomponents]]
                       (map (fn [component]
                              [activate-component component])
                            new-subcomponents))]
      ;; Check that each subcomponent has a different id. Otherwise, two
      ;; components will share an id, which will mess up communications
      ;; with the client.
      (assert (= (count subcomponent-ids) (count (set subcomponent-ids)))
              subcomponent-ids)
      (-> component-data
          (assoc :id->subcomponent id->subcomponent
                 :obsolete-components obsolete)
          (update :dom-version inc)
          (update-new-further-action
           process-dom-ready-for-client dom-manager component-atom)
          (update-new-further-actions follow-ons)))))

(defn handle-dom-change
  [component-atom]
  (with-latest-value [dom (reporter-value-when-valid (:dom-R @component-atom))]
    (when dom
      (swap-and-act!
       component-atom
       #(let [result (update-dom % component-atom dom)]
          ;; Check for problems where an update to the component-data, like
          ;; a dissoc, turned it into a map.
          (assert (instance? ComponentData result))
          result)))))

(defn new-dom-manager
  "Return a new dom-manager object for doms over the store."
  [mutable-store calculator-data]
  (assert (instance? cosheet2.calculator.CalculatorData calculator-data))
  (assert (mutable-store? mutable-store))
   (atom
    (map->DOMManagerData
     {:root-components {}
      :highest-version 0
      :components-to-send (priority-map)
      :calculator-data calculator-data
      :mutable-store mutable-store
      :further-actions nil})))

(defn client-id->component
  "Returns the component for the given client id."
  [manager-data client-id]
  (let [id-sequence (client-id->relative-ids client-id)
        root ((:root-components manager-data) (first id-sequence))
        result (reduce (fn [component id]
                         (when component
                           (when-let [id->subcomponent (:id->subcomponent
                                                        @component)]
                             (id->subcomponent id))))
                root
                (rest id-sequence))]
    (when result
      (assert (= (:client-id @result) client-id))
      result)))

(defn action-data-to-providing-dom
  "Given the action data for the component atom, get it for the
  component that provides its dom."
  [component-atom action-data action immutable-store]
  (loop [component component-atom
         action-data action-data]
    (when component
      (let [{:keys [id->subcomponent]} @component]
        (if (= (count id->subcomponent) 1)
          (let [subcomponent (first (vals id->subcomponent))]
            (if (:elided-from @subcomponent)
              (recur subcomponent
                     (update-action-data-for-component
                      subcomponent action-data action immutable-store))
              action-data))
          action-data)))))

(defn action-data-up-to-client-id
  "Returns the action data map for the component with the specified id."
  [manager-data client-id action immutable-store]
  (let [id-sequence (client-id->relative-ids client-id)
        root ((:root-components manager-data) (first id-sequence))]
    (reduce (fn [action-data id]
              (when action-data
                (let [component-data @(:component action-data)
                      {:keys [dom-R id->subcomponent]} component-data
                      dom (reporter-value-when-valid dom-R)]
                  (when dom
                    (when-let [subcomponent (id->subcomponent id)]
                      (update-action-data-for-component
                       subcomponent action-data action immutable-store))))))
            (update-action-data-for-component
             root {} action immutable-store)
            (rest id-sequence))))

(defn client-id->action-data
  "Returns the action data map for the component that generated the
  final dom for the given client id."
  [manager-data client-id action immutable-store]
  (let [up-to-client-id (action-data-up-to-client-id
                         manager-data client-id action immutable-store)]
    (when up-to-client-id
      (action-data-to-providing-dom
       (:component up-to-client-id) up-to-client-id action immutable-store))))

(defn longer-string
  [s1 s2]
  (if (>= (count s1) (count s2)) s1 s2))

(defn component-is-monitored?
  "Return true if the component targets one of the monitored ids, or
  shows the content of one of them. This function doesn't go through
  elided components."
  [component-atom monitored-ids]
  (let [{:keys [item-id relative-id]} (:dom-specification @component-atom)]
    ;; We check for item-id first, because relative-id can be :content,
    ;; or other markers that don't indicate an item.
    (when-let [target (or item-id relative-id)]
      (assert (not= target :content))
      (some #{target} monitored-ids))))

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
  "Find the displayed dom corresponding to the given component. (The
  last of the contained chain of elided doms.) Return the displayed
  dom, with all classes along the path added and with the :id of all
  its subcomponents set to their client ids. Don't set the overall
  dom's :id, or :version though. They depend on whether this component
  gets elided. Also return whether some dom in the path displays a
  monitored id."
  [component-atom monitored-ids]
  (let [{:keys [dom-R id->subcomponent]
         :as component-data}
        @component-atom 
        ;; We get whatever the latest reporter value is. It is possible
        ;; That our reporter is temporarily invalid, in which case
        ;; we will have no dom for now.
        ;; The dom-R reporter may have gotten ahead of the current
        ;; dom-version number, but that is OK. Worst case, we will
        ;; send the same dom more than once, until the version number
        ;; catches up with it.
        dom (when (= (component-data-state component-data) :active)
              (reporter-value-when-valid dom-R))
        monitored (component-is-monitored? component-atom monitored-ids)]
    (when dom
      (if (= (first dom) :component)
        ;; It's possible that id->subcomponent isn't up to date with the dom.
        ;; If it doesn't match, we'll get called again.
        (when (= (count id->subcomponent) 1)
          (let [class-attribute (select-keys (dom-attributes dom) [:class])
                [inner-dom _ inner-monitored] (find-displayed-dom
                                               (first (vals id->subcomponent))
                                               monitored-ids)]
            [(add-attributes inner-dom class-attribute)
             (or monitored inner-monitored)]))
        [(adjust-dom-for-client component-atom dom)
         monitored]))))

(defn prepare-dom-for-client
  "Given a component-atom, prepare its dom to send to the client. Also
  return whether it presents one of the monitored ids."
  [component-atom monitored-ids]
  ;; We have to get the dom after getting the dom version. That's
  ;; because it's OK to assign an older version to a new dom, but bad
  ;; to assign a newer version to an old dom.
  (let [{:keys [client-id dom-version elided-from]} @component-atom]
    ;; We should normally never be called with an elided-from dom. But
    ;; the unit test can do that sometimes, since it can have a record
    ;; of a client-id that it is no longer getting.
    (when (not elided-from)
      (let [[dom monitored] (find-displayed-dom
                             component-atom monitored-ids)]
        (when dom
          [(add-attributes dom {:id client-id
                                :version dom-version})
           monitored])))))

(defn get-response-doms
  "Return a seq of doms for the client containing up to num components.
  Also, if any of the monitored ids are the :item-id or :relative-id of any
  of the components, return the client id of that component.
  Finally, do the side-effect of updating :highest-version."
  [dom-manager monitored-ids num]
  (swap-control-return!
   dom-manager
   (fn [manager-data]
     (loop [response []
            monitored-client-id nil
            highest-version (:highest-version manager-data)
            components (map first (:components-to-send manager-data))]
       (if (or (>= (count response) num) (empty? components))
         [(assoc manager-data :highest-version highest-version)
          [response monitored-client-id]]
         (let [[component & remaining-components] components
               [dom monitored] (prepare-dom-for-client component monitored-ids)]
           (recur
            ;; The dom might be temporarily invalid.
            (cond-> response dom (conj dom))
            (longer-string monitored-client-id
                           (when monitored (:client-id @component)))
            (max highest-version
                 (if dom (:version (dom-attributes dom)) 0))
            remaining-components)))))))

(defn need-to-send-to-client-given-acknowledgement?
  "Return whether we need to send the component to the client, given a
  dom version that the client acknowledges getting."
  [component-atom ack-version]
  (let [{:keys [elided-from dom-version]} @component-atom]
    (and (not elided-from) (< ack-version dom-version))))

(defn process-acknowledgements
  "Modify the the dom-manager to reflect the acknowledgements."
  [dom-manager acknowledgements]
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
    (swap! dom-manager
           (fn [manager-data]
             (update manager-data :components-to-send
                     #(apply dissoc % (map first components-to-remove)))))
    ;; It's possible that a component got updated between the time we
    ;; decided that it needed to be removed and the swap! In that
    ;; case, we may have removed it incorrectly. So we go back through
    ;; the component data of the components we removed, and see if
    ;; they need to go back in the components-to-send. It is OK to use
    ;; out of date information here, because it is OK to unnecessarily
    ;; add a component to send.
    (let [components-to-add-back
          (map first
               (filter #(apply need-to-send-to-client-given-acknowledgement? %)
                       components-to-remove))]
      (when components-to-add-back
        (swap! dom-manager
               (fn [manager-data]
                 (update manager-data :components-to-send
                         (fn [components-to-send]
                           (reduce (fn [components-to-send component]
                                     (assoc components-to-send component
                                            (:depth @component)))
                                   components-to-send
                                   components-to-add-back)))))))))

(defn add-root-dom
  "Add dom with the given specification to the dom-manager.
  This is how the manager is bootstrapped with top level doms."
  [dom-manager specification]
  (let [top-id (:relative-id specification)
        component (make-component-atom
                   specification dom-manager
                   (id-subpart->client-id-subpart top-id) 1 false)]
    (assert (keyword? top-id) (str top-id))
    (swap-and-act!
     dom-manager
     (fn [manager-data]
       (let [{:keys [obsolete-components]}  manager-data
             old-component (get-in manager-data [:root-components top-id])
             obsolete (seq (into (set obsolete-components)
                                 (when old-component [old-component])))
             follow-on (if obsolete
                         [deactivate-then-activate
                          dom-manager dom-manager [component]]
                         [activate-component component])]
         (-> manager-data
             (assoc-in [:root-components top-id] component)
             (assoc :obsolete-components obsolete)
             (update :highest-version inc)
             (update-new-further-actions [follow-on])))))))

(defn remove-all-doms
   "Remove all the doms from the dom-manager. This will cause it to release
    all its reporters."
   [dom-manager]
   (swap-and-act!
    dom-manager
    (fn [manager-data]
      (reduce (fn [manager-data component]
                (update-new-further-action manager-data
                                           deactivate-component component))
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
  all the components to its components-to-send"
  [dom-manager components-and-depths]
  (swap! dom-manager
         (fn [data] (update data :components-to-send
                            #(reduce
                              (fn [priority-map [component depth]]
                                (if (:elided-from @component)
                                  priority-map
                                  (assoc priority-map component depth)))
                              %
                              components-and-depths)))))

(defn request-client-refresh
  "Mark all components as needing to be sent to the client."
  [dom-manager]
  (let [manager-data @dom-manager
        components-and-depths (mapcat mark-component-tree-as-needed
                                      (vals (:root-components manager-data)))]
    (add-components-to-send dom-manager components-and-depths)))

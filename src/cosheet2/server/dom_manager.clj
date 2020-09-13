(ns cosheet2.server.dom-manager
  (:require [clojure.data.priority-map :refer [priority-map]]
            (cosheet2 [task-queue :refer [add-task-with-priority]]
                      [reporter :refer [remove-attendee! set-attendee!
                                        reporter-value]]
                      [store :refer [StoredItemDescription]]
                      [utils :refer [swap-control-return!
                                     swap-and-act!
                                     with-latest-value
                                     update-in-clean-up
                                     update-new-further-action
                                     update-new-further-actions
                                     dissoc-in]]
                      [debug :refer [simplify-for-print]]
                      [hiccup-utils :refer [dom-attributes add-attributes
                                            into-attributes]])
            (cosheet2.server
             [render :refer [concatenate-client-id-parts
                             id->client-id-part ids->client-id
                             client-id->ids]]
             [item-render :refer [render-item-DOM]])))

(def verbose false)

;;; TODO: Mark some components as not being worth their descendants
;;; being saved. Once those are computed, they are thrown out. When
;;; the dom is recomputed, all their descendents have to be recomputed
;;; too.

;;; We record what needs to be rendered, and what it depends on.
;;; Whenever a piece of dom changes, we check all the sub-components
;;; it specifies, and update our information.

;;; As renderings are done, we update the client.

;;; The basic data structure is a component data atom. It holds a map
;;; of ComponentData, which contains information about one component
;;; we are tracking. We use ComponentData, rather than a map, so we can
;;; simplify print-out.
(defrecord ComponentData
    [;; These fields will never change once the component data is created
     dom-manager           ; Our dom manager.
     client-id             ; If non-nil, gives a keyword id to use for
                           ; communicating with the client about this
                           ; component.
     containing-component  ; The component that contains this one. Nil
                           ; if this component is a root.
     depth                 ; The depth of this component in the component
                           ; hierarchy, used to make sure that parents are
                           ; sent to the client before their children.

     ;; This field normally doesn't change, but if the component has
     ;; been permanently disabled, this field is cleared
     dom-specification     ; The dom spec for this component.

     ;; These fields can change
     reporters             ; The reporters that we are attending to for
                           ; this component. They are the ones returned by
                           ; :rendering-data   
     id->subcomponent         ; A map from :relative-id to the component data
                           ; of each sub-component. This is filled in once
                           ; the dom is computed, and can change if the dom
                           ; changes.
     dom                   ; The rendered dom for this client.
     dom-version           ; A monotonically increasing version number
                           ; for the current dom. It goes up every time
                           ; compute the dom, even if the dom doesn't change.
     client-needs-dom      ; True if the client has not been sent the dom
                           ; that would currently be computed, or
                           ; has not acknowledged receiving it.
     further-actions       ; A list of [function arg arg ...] calls that
                           ; need to be performed. The function will be
                           ; called with the atom, and the additional
                           ; arguments. (These actions are not actually
                           ; stored in the atom, but are added to the
                           ; data before it is stored, to request actions.)
     ])

(defmethod print-method ComponentData [s ^java.io.Writer w]
  ;; Avoid huge print-outs.
  (.write w (str "<ComponentData>" (:dom-specification s))))

;;; The component can be in several states:
;;;      prepared  The component's data has been filled in, but
;;;                it has not started computing.
;;;                Indicated by :reporters not being present.
;;;      awaiting  We are currently missing the component's dom.
;;;                Indicated by :client-needs-dom being present,
;;;                and :dom not being present.
;;;      complete  We have all information for the component, including
;;;                its DOM.
;;;                Indicated by :dom being present
;;;     suspended  We do not currently need the component's dom, but we
;;;                do need to know about changes to it
;;;      disabled  We will never need this component's dom again. It is
;;;                ready for garbage collection.
;;;                Indicated by :dom-specification being missing

(def dom-ready-for-client)

(defn make-component-data
  "Given a component specification, create a component data atom. The
  component must not be activated until it is recorded in its
  container."
  [specification containing-component-atom dom-manager]
  (assert (map? specification))
  (assert (or :client-id specification) (:relative-id specification))
  (atom
   (map->ComponentData
    {:dom-manager dom-manager
     :dom-specification specification
     :containing-component containing-component-atom
     :depth (if containing-component-atom
              (+ 1 (:depth @containing-component-atom))
              1)
     :client-needs-dom true
     :dom-version (+ 1 (:highest-version @dom-manager))})))

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
            "Error: duplicate subcomponent ids")
    answer))

(defn reuse-or-make-component-atom
  [specification dom-manager containing-component-atom old-component-atom]
  (if (and old-component-atom
           (= (:dom-specification @old-component-atom) specification))
    old-component-atom
    (make-component-data specification containing-component-atom dom-manager)))

(def compute-dom-unless-newer)

(defn schedule-compute-dom-unless-newer
  [component-atom]
  (let [{:keys [dom-manager dom-version depth]} @component-atom
        queue (:queue (:calculator-data @dom-manager))]
    (add-task-with-priority
     queue depth
     compute-dom-unless-newer component-atom dom-version)))

(defn reporter-changed-callback
  [& {:keys [key]}]
  (schedule-compute-dom-unless-newer key))

(defn default-get-rendering-data
  [specification mutable-store]
  (let [id (or (:item-id specification) (:relative-id specification))]
    (assert (satisfies? StoredItemDescription id))
    [[mutable-store [id]]]))

(defn update-register-for-reporters
  "Find out what the reporters the component's renderer needs,
  and register for them."
  [component-data component-atom]
  (let [{:keys [reporters dom-specification dom-manager]} component-data
        mutable-store (:mutable-store @dom-manager)]
    (if (or reporters (not dom-specification))
      component-data
      (let [getter (or (:get-rendering-data dom-specification)
                       default-get-rendering-data)
            pairs (getter dom-specification mutable-store)]
        (-> component-data
            (assoc :reporters (map first pairs))
            (update-new-further-actions
             (map (fn [[r categories]]
                    [set-attendee!
                     r component-atom (:depth component-data) categories
                     reporter-changed-callback])
                  pairs)))))))

(defn update-unregister-for-reporters
  "Remove the registrations from the component data's reporters"
  [component-data component-atom]
  (let [reporters (:reporters component-data)]
    (if reporters
      (-> component-data
          ;; We assoc with nil, rather than dissoc, so we don't turn
          ;; the record into a map.
          (assoc :reporters nil)
          (update-new-further-actions
           (map (fn [r] [remove-attendee! r component-atom])
                reporters)))
      component-data)))

(defn activate-component
  "Register for component for change notifications,
  and set an action to get its dom."
  [component-atom]
  (swap-and-act!
   component-atom
   (fn [component-data]
     (let [result
           (-> component-data
               (update-register-for-reporters component-atom)
               (update-new-further-action
                schedule-compute-dom-unless-newer component-atom))]
       result))))

(defn disable-component
  "Deactivate the component and all its descendant components."
  [component-atom]
  (swap-and-act!
   component-atom
   #(-> %
        ;; We assoc with nil, rather than dissoc, so we don't turn
        ;; the record into a map.
        (assoc :dom-specification nil
               :dom nil
               :dom-version nil
               :client-needs-dom nil)
        (update-unregister-for-reporters component-atom)
        (update-new-further-actions
         (map (fn [comp] [disable-component comp])
              (vals (:id->subcomponent %)))))))

(defn update-dom
  [component-data component-atom dom]
  (if (= dom (:dom component-data))
    ;; The dom hasn't changed. Bump the version to note that we have done
    ;; a recomputation.
    (update component-data :dom-version inc)
    (let [subcomponent-specs (get-id->subcomponent-specifications dom)
          ids (keys subcomponent-specs)
          old-id->subcomponent (or (:id->subcomponent component-data) {})
          id->subcomponent (zipmap
                            ids
                            (map (fn [id] (reuse-or-make-component-atom
                                           (subcomponent-specs id)
                                           (:dom-manager component-data)
                                           component-atom
                                           (old-id->subcomponent id)))
                                 ids))
          dropped-subcomponent-ids (filter #(not= (id->subcomponent %)
                                                  (old-id->subcomponent %))
                                           (keys old-id->subcomponent))
          new-subcomponent-ids (filter #(not= (id->subcomponent %)
                                              (old-id->subcomponent %))
                                       (keys id->subcomponent))]
      (-> component-data
          (assoc :dom dom
                 :id->subcomponent id->subcomponent
                 :client-needs-dom true)
          (update :dom-version inc)
          (update-new-further-action
           dom-ready-for-client
           (:dom-manager component-data) component-atom)
          (update-new-further-actions
           (map (fn [id] [activate-component (id->subcomponent id)])
                new-subcomponent-ids))
          (update-new-further-actions
           (map (fn [id] [disable-component (old-id->subcomponent id)])
                dropped-subcomponent-ids))))))

(defn compute-dom-unless-newer
  "Compute the dom, unless if its current version is greater than old-version."
  [component-atom old-dom-version]
  (let [{:keys [reporters dom-specification dom-version]} @component-atom
        renderer (or (:render-dom dom-specification) render-item-DOM)]
    (when (and dom-specification (<= dom-version old-dom-version))
      (with-latest-value [reporter-values (map reporter-value reporters)]
        (let [dom (apply renderer dom-specification reporter-values)]
          (swap-and-act!
           component-atom
           #(update-dom % component-atom dom)))))))

(defn mark-component-tree-as-needed
  "Mark the component and all its descendants as needing to be sent to
  the client. Return a list of pairs of all those components with ready
  dom and their depth."
  [component-atom task-queue]
  (let [[depth dom id->subcomponent]
        (swap-control-return!
         component-atom
         (fn [component-data] [(assoc component-data :client-needs-dom true)
                               [(:depth component-data)
                                (:dom component-data)
                                (:id->subcomponent component-data)]]))]
    (when (not dom)
      (schedule-compute-dom-unless-newer component-atom))
    (concat (when dom [[component-atom depth]])
            (mapcat #(mark-component-tree-as-needed % task-queue)
                    (vals id->subcomponent)))))

;;; The information for interfacing between the client and the
;;; components is stored in an atom, containing a record with these
;;; fields. By using a record, we can define our own print method to
;;; avoid dumping this out when printing every component.
(defrecord DOMManagerData
    [root-components    ; A map from client id of root components to their
                        ; component atoms. Not all components with
                        ; client ids need to be here, just the roots.
     highest-version    ; The highest version number of any dom we have sent
                        ; to the client. Any new component starts out with a
                        ; version number one higher, because we might have
                        ; forgetten about it and then reconstructed it, all
                        ; while the client kept ahold of it. This way, our
                        ; next version will be larger that whatever the
                        ; client has.
     client-ready-dom   ; A priority map of client-id for which we have
                        ; dom that the client needs to know about,
                        ; prioritized by their depth.
     calculator-data    ; The calculator data whose queue we use.
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

(defn new-dom-manager
   "Return a new dom-manager object"
   [cd mutable-store]
   (atom
    (map->DOMManagerData
     {:root-components {}
      :highest-version 0
      :client-ready-dom (priority-map)
      :calculator-data cd
      :mutable-store mutable-store
      :further-actions nil})))

(defn component->id-sequence
  [component-atom]
  (let [data @component-atom]
    (if-let [client-id (:client-id data)]
      [client-id]
      (conj (component->id-sequence (:containing-component))
            (:relative-id (:dom-specification data))))))

(defn component->client-id
  [component-atom]
  (ids->client-id (component->id-sequence component-atom)))

(defn client-id->component
  [manager-data client-id]
  (let [id-sequence (client-id->ids client-id)
        root ((:root-components manager-data) (first id-sequence))]
    (reduce (fn [component id]
              (when component ((:id->subcomponent @component) id)))
            root (rest id-sequence))))

(defn dom-ready-for-client
  [dom-manager component-atom]
  (swap! dom-manager
         (fn [manager-data]
           (update manager-data :client-ready-dom
                   #(assoc % component-atom (:depth @component-atom))))))

(defn adjust-subdom-for-client
  "Given a piece of dom and the client id for its container,
   adjust the dom to the form the client needs, turning subcomponents
   into [:component {:key ... :class ...}]."
  [container-client-id dom]
  (if (vector? dom)
    (if (= (first dom) :component)
      (let [{:keys [relative-id client-id class]} (second dom)]
        [:component (cond-> {:id (if client-id
                                   (id->client-id-part client-id)
                                   (concatenate-client-id-parts
                                    [container-client-id
                                     (id->client-id-part relative-id)]))}
                      class (assoc :class class))])
      (vec (map (partial adjust-subdom-for-client container-client-id)
                dom)))
    dom))

(defn prepare-dom-for-client
  "Given a component-atom, prepare its dom to send to the client."
  [component-atom]
  (let [{:keys [dom dom-version]} @component-atom]
    (when dom
      (let [client-id (component->client-id component-atom)
            added (add-attributes dom {:id client-id
                                       :version dom-version})]
        (into [(first added) (second added)]
              (map (partial adjust-subdom-for-client client-id)
                   (rest (rest dom))))))))

(defn get-response-doms
  "Return a seq of doms for the client containing up to num components.
  This has the side-effect of updating :highest-version."
  [dom-manager num]
  (swap-control-return!
   (fn [manager-data]
     (loop [response []
            highest-version (:highest-version manager-data)
            components (map first (:client-ready-dom manager-data))]
       (if (or (>= (count response) num) (empty? components))
         [(assoc manager-data :highest-version highest-version)
          response]
         (let [[component & remaining-components] components
               dom (prepare-dom-for-client component)]
           (recur (if dom (conj response dom) response)
                  (max highest-version
                       (if dom (:version (dom-attributes dom)) 0))
                  remaining-components)))))))

(defn update-acknowledgements
  "Given a map of acknowledgements from client id to version,
   Remove the acknowledged components from the ones that need updating
   to the client, provided the version the client acknowledged is up to
   date."
  [manager-data acknowledgements]
   (reduce
    (fn [manager-data [client-id version]]
      (if-let [component-atom (client-id->component manager-data)]
        (let [version-matched
              ;; We may run this multiple times, since we run inside
              ;; another swap!. But that is OK, as the only side effect
              ;; we have is idempotent.
              (swap-control-return!
               component-atom
               (fn [component-data]
                 (if (= version (:dom-version component-data))
                   [(dissoc component-data :client-needs-dom) true]
                   [component-data false])))]
          (cond-> manager-data
            version-matched (dissoc-in [:client-ready-dom component-atom])))
        manager-data))
    manager-data acknowledgements))

(defn process-acknowledgements
  "Update the atom to reflect the acknowledgements."
  [dom-manager acknowledgements]
  (swap-and-act! dom-manager #(update-acknowledgements % acknowledgements)))

(defn add-root-dom
  "Add dom with the given client id and specification to the dom-manager.
  This is how the manager is bootstrapped with top level doms."
  [dom-manager client-id specification]
  (assert (keyword? client-id))
  (let [spec (assoc specification :client-id client-id)
        component (make-component-data spec nil dom-manager)]
    (swap-and-act!
     dom-manager
     #(let [old-component (get-in % [:root-components client-id]) ]
        (cond-> (-> %
                    (assoc-in [:root-components client-id] component)
                    (update :highest-version inc))
          old-component
          (update-new-further-action disable-component old-component))))
    (activate-component)))

(defn remove-all-doms
   "Remove all the doms from the dom-manager. This will cause it to release
    all its reporters."
   [dom-manager]
   (swap-and-act!
    dom-manager
    (fn [manager-data]
      (reduce (fn [manager-data component]
                (update-new-further-action manager-data
                                           disable-component component))
              (assoc manager-data
                     :root-components {}
                     :client-ready-dom (priority-map))
              (vals (:root-components manager-data))))))

;;; TODO: Code here

(defn request-client-refresh
  "Mark all components as needing to be sent to the client."
  [dom-manager]
  (let [manager-data @dom-manager
        queue (:queue (:calculator-data manager-data))
        component-and-depths (mapcat #(mark-component-tree-as-needed % queue)
                                     (vals (:root-components manager-data)))]
    (swap! dom-manager
           (fn [data] (update data :client-ready-dom
                              #(reduce (fn [priority-map [component depth]]
                                         (assoc priority-map component depth))
                                       %
                                       component-and-depths))))))

(ns cosheet2.server.dom-tracker
  (:require [clojure.data.priority-map :refer [priority-map]]
            (cosheet2 [task-queue :refer [add-task-with-priority]]
                      [reporter :refer [remove-attendee! set-attendee!
                                        reporter-value]]
                      [calculator :refer [update-new-further-action
                                          update-new-further-actions
                                          modify-and-act!]]
                      [utils :refer [swap-control-return!
                                     with-latest-value
                                     update-in-clean-up
                                     dissoc-in]]
                      [debug :refer [simplify-for-print]]
                      [hiccup-utils :refer [dom-attributes add-attributes
                                            into-attributes]])
            (cosheet2.server
             [render :refer [default-get-rendering-data
                             concatenate-client-id-parts
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

;;; The basic data structure is a component data atom. It holds a map,
;;; which contains information about one component we are tracking.
;;; Its fields can include:
;;;          :dom-tracker  Our dom tracker.
;;;            :client-id  Optional field that is in present in components
;;;                        that are not contained in other components that
;;;                        the tracker manages. It gives a keyword id to use
;;;                        for communicating with the client about this
;;;                        component.
;;; :containing-component  The component that contains this one. Not present
;;;                        if this component is a root.
;;;    :dom-specification  The dom spec for this component. If the component
;;;                        has been permanently disabled, this is missing.
;;;                :depth  The depth of this component in the component
;;;                        hierarchy, used to make sure that parents are
;;;                        sent to the client before their children.
;;;            :reporters  The reporters that we are attending to for
;;;                        this component. They are the ones returned by
;;;                        :rendering-data
;;;        :subcomponents  A map from :relative-id to the component data
;;;                        of each sub-component. This is filled in once
;;;                        the dom is computed.
;;;                  :dom  The rendered dom for this client.
;;;          :dom-version  The version number of the current dom.
;;;     :client-needs-dom  True if the client has not been sent the dom
;;;                        that would currently be computed, or
;;;                        has not acknowledged receiving it.
;;;      :further-actions  A list of [function arg arg ...] calls that
;;;                        need to be performed. The function will be
;;;                        called with the atom, and the additional
;;;                        arguments. (These actions are not actually
;;;                        stored in the atom, but are added to the
;;;                        data before it is stored, to request actions.)

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

(def new-dom-for-client)

(defn make-component-data
  "Given a component specification, create a component data atom. The
  component must not be activated until it is recorded in its
  container."
  [specification containing-component-atom dom-tracker]
  (assert (map? specification))
  (assert (or :client-id specification) (:relative-id specification))
  (atom
   {:dom-tracker dom-tracker
    :dom-specification specification
    :containing-component containing-component-atom
    :depth (if containing-component-atom
             (+ 1 (:depth @containing-component-atom))
             1)
    :client-needs-dom true
    :dom-version (+ 1 (:highest-version @dom-tracker))}))

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

(defn specification-for-subcomponent
  [{:keys [dom]} subcomponent-id]
  (when dom
    ((get-id->subcomponent-specifications dom) subcomponent-id)))

(defn reuse-or-make-component-atom
  [specification dom-tracker containing-component-atom old-component-atom]
  (if (and old-component-atom
           ((:dom-specificationification @old-component-atom) specification))
    old-component-atom
    (make-component-data specification containing-component-atom dom-tracker)))

(def compute-dom)

(defn schedule-compute-dom
  [component-atom]
  (let [{:keys [dom-tracker depth]} @component-atom
        queue (:queue (:calculator-data dom-tracker))]
    (add-task-with-priority queue depth compute-dom component-atom)))

(defn reporter-changed-callback
  [& {:keys [key]}]
  (schedule-compute-dom key))

(defn update-register-for-reporters
  "Find out what the reporters the component's renderer needs,
  and register for them."
  [component-data component-atom]
  (let [{:keys [reporters dom-specification dom-tracker]} component-data
        mutable-store (:mutable-store dom-tracker)]
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
          (dissoc :reporters)
          (update-new-further-actions
           (map (fn [r] [remove-attendee! r component-atom])
                reporters)))
      component-data)))

(defn activate-component
  "Register for component for change notifications,
  and set an action to get its dom."
  [component-atom]
  (modify-and-act!
   component-atom
   #(-> %
        (update-register-for-reporters component-atom)
        (update-new-further-action schedule-compute-dom component-atom))))

(defn disable-component
  "Deactivate the component and all its descendant components."
  [component-atom]
  (modify-and-act!
   component-atom
   #(-> %
        (dissoc :dom-specification)
        (update-unregister-for-reporters % component-atom)
        (update-new-further-actions
         (map (fn [comp] [disable-component comp])
              (vals (:subcomponents %)))))))

(defn update-dom
  [component-data component-atom dom]
  (let [subcomponent-specs (get-id->subcomponent-specifications dom)
        ids (keys subcomponent-specs)
        old-subcomponents (:subcomponents component-data)
        subcomponents (zipmap ids
                              (map (fn [id] (reuse-or-make-component-atom
                                             (subcomponent-specs id)
                                             (:dom-tracker component-data)
                                             component-atom
                                             (old-subcomponents id)))))
        dropped-subcomponent-ids (filter #(not= (subcomponents %)
                                                (old-subcomponents %))
                                         (keys old-subcomponents))
        new-subcomponent-ids (filter #(not= (subcomponents %)
                                            (old-subcomponents %))
                                     (keys subcomponents))]
    (-> component-data
        (assoc :dom dom
               :subcomponents subcomponents
               :client-needs-dom true)
        (update :dom-version #(+ % 1))
        (update-new-further-action
         new-dom-for-client
         (:dom-tracker component-data) component-atom)
        (update-new-further-actions
         (map (fn [id] [activate-component (subcomponents id)])
              new-subcomponent-ids))
        (update-new-further-actions
         (map (fn [id] [disable-component (old-subcomponents id)])
              dropped-subcomponent-ids)))))

(defn compute-dom
  [component-atom]
  (let [{:keys [reporters dom-specification]} @component-atom
        renderer (or (:render-dom dom-specification) render-item-DOM)]
    (when dom-specification
      (with-latest-value [reporter-values (map reporter-value reporters)]
        (let [dom (apply renderer dom-specification reporter-values)]
          (modify-and-act!
           component-atom
           #(update-dom % component-atom dom)))))))

;;; The information for interfacing between the client and the
;;; components is stored in an atom, containing a map with these
;;; elements:
;;;    :root-components  A map from client id of root components to their
;;;                      component atoms.
;;;    :highest-version  The highest version number of any dom we have sent
;;;                      to the client. Any new component starts out with a
;;;                      version number one higher, because we might have
;;;                      forgetten about it and then reconstructed it, all
;;;                      while the client kept ahold of it. This way, our
;;;                      next version will be larger that whatever the client
;;;                      has.
;;;   :client-ready-dom  A priority map of client-id for which we have
;;;                      dom that the client needs to know about,
;;;                      prioritized by their depth.
;;;    :calculator-data  The calculator data whose queue we use.
;;;      :mutable-store  The mutable store that holds the data the doms
;;;                      rely on.
;;;    :further-actions  A list of [function arg arg ...] calls that
;;;                      need to be performed. The function will be
;;;                      called with the atom, and the additional
;;;                      arguments. (These actions are not actually
;;;                      stored in the atom, but are added to the
;;;                      data before it is stored, to request actions.)

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
  [tracker-data client-id]
  (let [id-sequence (client-id->ids client-id)
        root ((:root-components tracker-data) (first id-sequence))]
    (reduce (fn [component id]
              (when component ((:subcomponents @component) id)))
            root (rest id-sequence))))

 (defn adjust-subdom-for-client
   "Given a piece of dom and the client id for its container,
    adjust the dom to the form the client needs, turning subcomponents
    into [:component {:key ... :class ...}]."
   [container-client-id dom]
   (if (vector? dom)
     (if (= (first dom) :component)
       (let [{:keys [relative-id client-id class]} (second dom)]
         [:component (cond-> {:id (or client-id
                                      (concatenate-client-id-parts
                                       [container-client-id
                                        (id->client-id-part relative-id)]))}
                       class (assoc :class class))]
         (vec (map (partial adjust-subdom-for-client container-client-id)
                   dom)))
       dom)))

(defn dom-for-client
  "Given a component-atom prepare the dom for that atom to send to the
   client."
  [component-atom]
  (let [{:keys [dom dom-version]} @component-atom ]
    (when dom
      (let [client-id (component->client-id component-atom)
            added (add-attributes dom {:id client-id
                                       :version dom-version})]
        (into [(first added) (second added)]
              (map (partial adjust-subdom-for-client client-id)
                   (rest (rest dom))))))))

(defn response-doms
   "Return a seq of doms for the client for up to num components."
  [data num]
  (loop [response []
         components (map first (:client-ready-dom data))]
    (if (or (>= (count response)) num) (empty? components))
      response
      (let [[component & remaining-components] components
            dom (dom-for-client component)]
        (recur (if dom (conj response dom) response)
               remaining-components))))

;;; TODO: Does this work right if it gets re-run multiple times?
(defn update-acknowledgements
  "Given a map of acknowledgements from client id to version,
   Remove the acknowledged components from the ones that need
   updating in the client, provided the acknowledged version is up to date."
  [data acknowledgements]
   (reduce
    (fn [data [client-id version]]
      (if-let [component-atom (client-id->component data)]
        (let [version-matched
              (swap-control-return!
               component-atom
               (fn [component-data]
                 (if (= version (:dom-version component-data))
                   [(dissoc component-data :client-needs-dom) true]
                   [component-data false])))]
          (cond-> data
            version-matched (dissoc-in [:client-ready-dom component-atom])))
        data))
    data acknowledgements))

(comment
 (defn dom->keys-and-dom
   "Given a dom, return a map from key to dom for keys in the dom,
   but don't descend into subcomponents."
   [dom]
   (if (and (vector? dom) (not= (first dom) :component))
     (let [key (let [attributes (second dom)]
                 (when (map? attributes) (:key attributes)))]
       (reduce (fn [keys dom] (into keys (dom->keys-and-dom dom)))
               (if (nil? key) {} {key dom}) (rest dom)))
     {}))

 (defn update-acknowledgements
   "Given a map of acknowledgements from id to version,
   Remove the acknowledged components from the ones that need
   updating in the client, provided the acknowledged version is up to date."
   [data acknowledgements]
   (reduce
    (fn [data [id version]]
      (let [key (or (get-in data [:id->key id])
                    (when (string? id) (string->key id)))]
        (if key
          (cond-> data
            (and (number? version)
                 (>= version (get-in data [:components key :version])))
            (update-in [:out-of-date-keys] #(dissoc % key)))
          (do (println "Warning: unknown id in acknowledgement" [id version])
              data))))
    data acknowledgements))

 (defn process-acknowledgements
   "Update the atom to reflect the acknowledgements."
   [dom-tracker acknowledgements]
   (modify-and-act! dom-tracker #(update-acknowledgements % acknowledgements)))

 (defn update-unneeded-subcomponents
   "Remove all subcomponents that were in the old version of the component map
   but are not in the new one."
   [data old-component-data new-component-data]
   (reduce (fn [data key] (update-clear-component data key))
           data
           (clojure.set/difference  (:subcomponents old-component-data)
                                    (:subcomponents new-component-data))))

 (defn check-subcomponents-stored
   "Given data, dom that is stored there and its depth,
  check that all the subcomponents of the dom are stored there."
   [data dom depth]
   (when dom
     (let [subcomponents (dom->subcomponent-datas dom depth)]
       (doseq [subcomponent subcomponents]
         (let [key (:key subcomponent)]
           (let [stored-map (get-in data [:components key])]
             (when stored-map
               (assert (= (:definition stored-map)
                          (:definition subcomponent))
                       (str "differing definitions for " key
                            "\ndom" (simplify-for-print
                                     (:definition subcomponent))
                            "\nstored" (simplify-for-print
                                        (:definition stored-map)))))))))))

 (defn update-next-version
   "Increment the version number of the data, returning the updated data
  and the next free version number"
   [data]
   (let [version (:next-version data)]
     [(assoc data :next-version (inc version)) version]))

 (defn update-dom
   "Given the data, a key, and the latest dom for the key,
   record the key in the dom, and do all necessary updates."
   [data key dom]
   (let [;; The key of the dom we got might be missing, or might be our key
         ;; with :content added. Make key match the component.
         dom (add-attributes dom {:key key})
         component-data (get-in data [:components key])
         old-dom (get-in data [:key->dom key])
         depth (:depth component-data)]
     (if (and component-data (not= dom old-dom))
       (let [subcomponent-datas (dom->subcomponent-datas dom depth)
             [data version] (update-next-version data)
             new-map (-> component-data
                         (assoc :subcomponents
                                (set (map :key subcomponent-datas)))
                         (assoc :version version))]
         (check-subcomponents-stored data old-dom depth)
         (cond->
             (-> (reduce update-set-component data subcomponent-datas)
                 (update-in [:key->dom]
                            #(into (apply dissoc %
                                          (keys (dom->keys-and-dom old-dom)))
                                   (dom->keys-and-dom dom)))
                 (update-unneeded-subcomponents component-data new-map)
                 (assoc-in [:components key] new-map))
           (not= (adjust-dom-for-client data dom)
                 (adjust-dom-for-client data old-dom)) 
           (update-in [:out-of-date-keys] #(assoc % key depth))))
       data)))

 (defn dom-callback
   "Record a new value for the dom."
   [[_ key] reporter data-atom]
   (with-latest-value [dom (reporter/value reporter)]
     (when verbose
       (println "got dom value for key" (simplify-for-print key)))
     ;;; TODO: When not valid, but we have a previous dom,
     ;;; set a style for the dom to indicate invalidity.
     (when (reporter/valid? dom)
       (when verbose
         (println "value is valid"))
       (let [dom-key (:key (dom-attributes dom))]
         (assert (or (nil? dom-key)
                     (= (seq key) (seq dom-key))
                     (= (seq (conj key :content)) (seq dom-key)))
                 [key dom]))
       (modify-and-act!
        data-atom
        (fn [data]
          (when (= reporter (get-in data [:components key :reporter]))
            (when verbose
              (println "reporter is current")))
          (cond-> data
            (= reporter (get-in data [:components key :reporter]))
            (update-dom key dom)))))))

 (defn set-attending
   "Set whether or not we are attending to the reporter for the dom
     for the key."
   [data-atom reporter key]
   (with-latest-value
     [[should-attend priority]
      (let [component-data (get-in @data-atom [:components key])]
        [(= (:reporter component-data) reporter)
         (* 1000 (or (:depth component-data) 0))])]
     ;; Note: The key we use to subscribe might be the same as other
     ;; dom trackers use, but we only subscribe to reporters that we
     ;; create, so there will never be a conflict.
     (apply reporter/set-attendee! reporter [:dom-request key] priority
            (when should-attend
              [dom-callback data-atom]))))

 (defn update-request-set-attending
   "Add a further action to start or stop attending to the reporter
   of the given component-data, depending on whether the map is still active."
   [data component-data]
   (let [reporter (:reporter component-data)
         key (:key component-data)]
     (if reporter
       (update-new-further-action data set-attending reporter key)
       data)))

 (defn update-ensure-component
   "Make sure there is a component with the given key."
   [data key]
   (if (get-in data [:components key])
     data
     (let [[data version] (update-next-version data)]
       (assoc-in data [:components key] {:key key :version version :depth 0}))))

 (defn update-clear-component
   "Remove the component with the given key."
   [data key]
   (let [component-data (get-in data [:components key])
         id (get-in data [:key->id key])]
     (if component-data
       (-> (cond-> data id (update-in [:id->key] #(dissoc % id)))
           (update-in [:out-of-date-keys] #(dissoc % key))          
           (update-in [:key->id] #(dissoc % key))
           (update-in [:key->dom] #(dissoc % key))
           (update-in [:components] #(dissoc % key))
           (update-request-set-attending component-data)
           (update-unneeded-subcomponents component-data {}))
       data)))

 (defn update-set-component
   "Set the information according to the given component map,
   creating the component if necessary."
   [data {:keys [key definition] :as component-data}]
   (let [data (update-ensure-component data key)
         original-component-data (get-in data [:components key])
         new-component-data (merge original-component-data component-data)]
     (if (= definition (:definition original-component-data))
       (assoc-in data [:components key] new-component-data)
       (let [reporter (new-expression definition)
             final-map (assoc new-component-data :reporter reporter)]
         (when verbose
           (println "created component map for" (simplify-for-print key)))
         (-> data
             (update-request-set-attending original-component-data)
             (assoc-in [:components key] final-map)
             (update-request-set-attending final-map)
             (update-new-further-action
              (fn [atom] (manage reporter (:manager-data data)))))))))

 (defn add-dom
   "Add dom with the given client id, key, and definition to the dom-tracker."
   [dom-tracker client-id key definition]
   ;; The id must not be one that we could generate.
   (assert (string? client-id))
   (assert (vector? key))
   (assert (not (#{\: \1 \2 \3 \4 \5 \6 \7 \8 \9 \0} (first client-id))))
   (modify-and-act!
    dom-tracker
    #(-> %
         ;; It is possible that the id was paired with
         ;; something different. Get rid of that pairing too.
         (update-clear-component (get-in % [:id->key client-id]))
         (update-clear-component key)
         (assoc-in [:key->id key] client-id)
         (assoc-in [:id->key client-id] key)
         (update-set-component {:definition definition :key key}))))

 (defn remove-all-doms
   "Remove all the doms from the dom-tracker. This will cause it to release
  all its reporters."
   [dom-tracker]
   (modify-and-act!
    dom-tracker
    (fn [data] (reduce (fn [data key] (update-clear-component data key))
                       data (vals (:id->key data))))))

 (defn id->key
   "Return the hiccup key for the client id."
   [dom-tracker id]
   (let [data @dom-tracker]
     (or (get-in data [:id->key id])
         (let [key (when (string? id) (string->key id))]
           ;; Make sure the key the client sent us is for a dom we know about.
           (when (or (get-in data [:key->dom key])
                     (get-in data [:components key]))
             key)))))

 (defn key->id
   "Return the client id for the hiccup key."
   [dom-tracker key]
   (or (get-in @dom-tracker [:key->id key])
       (key->string key)))

 (defn key->attributes
   "Return the attributes for the dom with the given key,
   include both attributes specified by the dom definition and by
   any component that gave rise to it."
   [dom-tracker key]
   (let [data @dom-tracker
         dom (get-in data [:key->dom key])]
     (into-attributes (or (and dom (dom-attributes dom)) {})
                      (get-in data [:components key :attributes]))))

 (defn request-client-refresh
   "Mark all components as needing to be sent to the client."
   [dom-tracker]
   (swap! dom-tracker
          (fn [data] (assoc data :out-of-date-keys
                            (reduce (fn [map [key component]]
                                      (if (get-in data [:key->dom key])
                                        (assoc map key (:depth component))
                                        map))
                                    (priority-map/priority-map)
                                    (:components data))))))

 (defn new-dom-tracker
   "Return a new dom-tracker object"
   [md]
   (atom
    {:components {}
     :id->key {}
     :key->id {}
     :key->dom {}
     :next-version 1
     :out-of-date-keys (priority-map/priority-map)
     :manager-data md})))

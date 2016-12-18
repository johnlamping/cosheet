
(ns cosheet.server.dom-tracker
  (:require [clojure.data.priority-map :as priority-map]
            (cosheet [reporters :as reporter]
                     [expression :refer [new-expression]]
                     [utils :refer [swap-control-return!
                                    call-with-latest-value]]
                     [debug :refer [simplify-for-print]]
                     [dom-utils :refer [dom-attributes add-attributes
                                        into-attributes]]
                     [expression-manager :refer [manage]])
            
            (cosheet.server [render :refer [server-specific-attributes
                                            key->string string->key]])))

(def verbose false)

;;; Records the current state of the dom, and which items need to be
;;; sent to the client. Has the manager compute subcomponents as
;;; needed. (Where a computation manager's job is to update a bunch of
;;; reporters, the dom tracker's job is to set up a reporter for
;;; each visible dom item, and to inform the client of changes.)

;;; Note that the identity of a dom component is determined by its
;;; containing dom, not by the definition that yields the
;;; component. There are thus two ways that a dom for a particular
;;; identity can change: the containing dom can change the definition
;;; for the dom it wants in that spot, or the database can change to
;;; content that that definition displays. We use a map to track the
;;; former, which points to a reporter that tracks the
;;; latter. Whenever a piece of dom changes, we check all the
;;; sub-components it specifies, and update our map, possibly creating
;;; new reporters, or ignoring obsolete ones.

;;; The basic data structure is a component map, which contains dom
;;; for a component and information about it. It fields can include:
;;;            :key  A unique key for this component.
;;;          :depth  The depth of this component in the component
;;;                  hierarchy, used to make sure that parents are
;;;                  sent to the client before their children.
;;;     :definition  An application that will compute dom or
;;;                  return a reporter to compute it. We record the
;;;                  definition so we know that if the component is
;;;                  updated with the same definition, we don't have
;;;                  to recompute. The dom this yields can then get
;;;                  additional attributes added at the site of the
;;;                  subcomponent that calls for it.
;;;       :reporter  The result of running the definition, either the
;;;                  dom, or a reporter that computes it.
;;;     :attributes  A map of attributes to be added to the result of
;;;                  running the definition. These attributes are
;;;                  provided to the client even before the
;;;                  component's dom is ready. 
;;;  :subcomponents  A set of the keys of the subcomponents of this
;;;                  component.
;;;        :version  An version number for client coordination. It
;;;                  increases each time the dom or attributes change.

;;; The information for all components is stored in an atom,
;;; containing a map with these elements:
;;;       :components  A map from key to component map.
;;;          :key->id  A map from key to client id, for those few components
;;;                    where add-dom was called directly.
;;;          :id->key  The inverse of the key->id map
;;;         :key->dom  A map from key to the server version of the dom
;;;                    for that key. The dom also provides access to any extra
;;;                    information needed to interpret actions on the key.
;;;                    The dom is in hiccup format, as returned by the
;;;                    definition or the reporter it returns.
;;;                    Inside this dom, subcomponents are annotated as
;;;                    [:component {:key <globally unique for each subcomponent>
;;;                                 <Additional attributes to add to the
;;;                                  dom produced by the definition.
;;;                                  These are typically things like
;;;                                  display, that say how the component
;;;                                  should fit into its parent. These
;;;                                  are sent to the client as part of
;;;                                  the component definition, before
;;;                                  the client gets the rest of its dom.>}
;;;     :next-version  The next free version number.
;;;                    We use a global version because we might forget about
;;;                    a component, and then reconstruct it, all while the
;;;                    client keeps ahold of it. We need to be sure that its
;;;                    next version will be larger that whatever the client
;;;                    has.
;;; :out-of-date-keys  A priority queue of ids that the client
;;;                    needs to know about, prioritized by their depth.
;;;     :manager-data  The expression manager-data for our reporters
;;;                    on the server.
;;;  :further-actions  A list of [function arg arg ...] calls that
;;;                    need to be performed. The function will be
;;;                    called with the atom, and the additional
;;;                    arguments. (These actions are not actually
;;;                    stored in the atom, but are added to the
;;;                    data before it is stored to request actions.)

(defn update-new-further-action
  "Given a component map, add an an action to the further actions.
   The action will be called with the atom, followed by any additional
   arguments specified."
  [data & action]
  (update-in data [:further-actions] (fnil conj []) (vec action)))

(defn swap-and-act
  "Atomicly call the function on the atom's data.
   The function should return the new data for the atom,
   which may also contain a temporary field, :further-actions, with
   a list of actions that should be performed. Perform those actions,
   also passing in the atom to each action."
  [atom f]
  (let [actions (swap-control-return!
                 atom
                 (fn [data] (let [new-data (f data)]
                              [(dissoc new-data :further-actions)
                               (:further-actions new-data)])))]
    (doseq [action actions]
      (apply (first action) atom (rest action)))))

(def update-set-component)
(def update-clear-component)

(defn component->component-map
  "Given a subcomponent specified inside a dom,
   create a component map."
  [[_ attributes definition] parent-depth]
  (assert (map? attributes))
  (assert (:key attributes))
  {:key (:key attributes)
   :definition definition
   :attributes (dissoc attributes :key)
   :depth (inc parent-depth)})

(defn dom->subcomponents
  "Given a dom that may contain subcomponents, return a list of them."
  [dom]
  (if (vector? dom)
    (if (= (first dom) :component)
      [dom]
      (reduce (fn [subcomponents dom]
                (into subcomponents (dom->subcomponents dom)))
              [] dom))
    []))

(defn dom->subcomponent-maps
  "Given a dom that may contain subcomponents, and its depth
   return a list of their component maps."
  [dom depth]
  (map #(component->component-map % depth) (dom->subcomponents dom)))

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

(defn adjust-attributes-for-client
  "Given the data and a dom, which must be a vector, remove dom attributes
   not intended for the client, and if it has a :key in its attributes,
   replace it with the corresponding id."
  [data dom]
  (let [attributes (second dom)]
    (if (not (map? attributes))
      dom
      (let [key (:key attributes)
            pruned-attributes (apply dissoc attributes
                                     server-specific-attributes)
            client-attributes (if (nil? key)
                                pruned-attributes
                                (let [id (or (get-in data [:key->id key])
                                             (key->string key))]
                                  (assert (not (nil? id)))
                                  (assoc pruned-attributes :id id)))]
        (assoc dom 1 client-attributes)))))

(defn adjust-dom-for-client
  "Given the data, and a piece of dom,
   adjust the dom to the form the client needs, replacing keys by ids,
   and putting subcomponents into the form [:component <attributes>]."
  [data dom]
  (if (vector? dom)
    (if (= (first dom) :component)
      (let [attributes (second dom) 
            id (or (get-in data [:key->id (:key attributes)])
                   (key->string (:key attributes)))]
        (assert (not (nil? id)) ["No id found for key" (:key attributes)])
        (adjust-attributes-for-client data [:component attributes]))
      (vec (map (partial adjust-dom-for-client data)
                (adjust-attributes-for-client data dom))))
    dom))

(defn dom-for-client
  "Given the data and a key,
   prepare the dom for that key to send to the client."
  [data key]
  (let [component-map (get-in data [:components key])]
    (assert (not (nil? component-map)))
    (add-attributes
     (adjust-dom-for-client data (get-in data [:key->dom key]))
     {:version (:version component-map)})))

(defn response-doms
  "Return a seq of doms for the client for up to num components."
  [data num]
  (for [[key priority] (take num (:out-of-date-keys data))]
    (dom-for-client data key)))

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
  [tracker acknowledgements]
  (swap-and-act tracker #(update-acknowledgements % acknowledgements)))

(defn update-unneeded-subcomponents
  "Remove all subcomponents that were in the old version of the component map
   but are not in the new one."
  [data old-component-map new-component-map]
  (reduce (fn [data key] (update-clear-component data key))
          data
          (clojure.set/difference  (:subcomponents old-component-map)
                                   (:subcomponents new-component-map))))

(defn check-subcomponents-stored
  "Given data, dom that is stored there and its depth,
  check that all the subcomponents of the dom are stored there."
  [data dom depth]
  (when dom
    (let [subcomponents (dom->subcomponent-maps dom depth)]
      (doseq [subcomponent subcomponents]
        (let [key (:key subcomponent)]
          (let [stored-map (get-in data [:components key])]
            (when stored-map
              (assert (= (:definition stored-map)
                         (:definition subcomponent))
                      (str "differing definitions for " key
                           "\ndom" (:definition subcomponent)
                           "\nstored" (:definition stored-map))))))))))

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
  (let [;; The key of the dom we got might be our key with :content added.
        ;; Take that off, so the key will match the component.
        dom (add-attributes dom {:key key})
        component-map (get-in data [:components key])
        old-dom (get-in data [:key->dom key])
        depth (:depth component-map)]
    (if (and component-map (not= dom old-dom))
      (do (check-subcomponents-stored data old-dom depth)
          (let [subcomponent-maps (dom->subcomponent-maps dom depth)
                [data version] (update-next-version data)
                new-map (-> component-map
                            (assoc :subcomponents
                                   (set (map :key subcomponent-maps)))
                            (assoc :version version))]
            (-> (reduce update-set-component data subcomponent-maps)
                (update-in [:key->dom]
                           #(into (apply dissoc %
                                         (keys (dom->keys-and-dom old-dom)))
                                  (dom->keys-and-dom dom)))
                (update-unneeded-subcomponents component-map new-map)
                (update-in [:out-of-date-keys] #(assoc % key depth))
                (assoc-in [:components key] new-map))))
      data)))

(defn dom-callback
  "Record a new value for the dom."
  [[_ key] reporter data-atom]
  (call-with-latest-value
   #(reporter/value reporter)
   (fn [dom]
     (when verbose
       (println "got dom value for key" (simplify-for-print key)))
     ;;; TODO: When not valid, but we have a previous dom,
     ;;; set a style for the dom to indicate invalidity.
     (when (reporter/valid? dom)
       (when verbose
         (println "value is valid"))
       (let [dom-key (:key (dom-attributes dom))]
         (assert (or (empty? key)
                     (= (seq key) (seq dom-key))
                     (= (seq (conj key :content)) (seq dom-key)))
                 [key dom]))
       (swap-and-act
        data-atom
        (fn [data]
          (when (= reporter (get-in data [:components key :reporter]))
            (when verbose
              (println "reporter is current")))
          (cond-> data
            (= reporter (get-in data [:components key :reporter]))
            (update-dom key dom))))))))

(defn set-attending
  "Set whether or not we are attending to the reporter for the dom for the key."
  [data-atom reporter key]
  (call-with-latest-value
   #(= (get-in @data-atom [:components key :reporter]) reporter)
   (fn [should-attend]
     ;; Note: The key we use to subscribe might be the same as other
     ;; dom trackers use, but we only subscribe to reporters that we
     ;; create, so there will never be a conflict.
     (apply reporter/set-attendee! reporter [:dom-request key]
            (when should-attend
              [dom-callback data-atom])))))

(defn update-request-set-attending
  "Add a further action to start or stop attending to the reporter
   of the given component-map, depending on whether the map is still active."
  [data component-map]
  (let [reporter (:reporter component-map)
        key (:key component-map)]
    (if reporter
      (update-new-further-action data set-attending reporter key)
      data)))

(defn update-ensure-component
  "Make sure there is a component with the given key.."
  [data key]
  (if (get-in data [:components key])
    data
    (let [[data version] (update-next-version data)]
      (assoc-in data [:components key] {:key key :version version :depth 0}))))

(defn update-clear-component
  "Remove the component with the given key."
  [data key]
  (let [component-map (get-in data [:components key])
        id (get-in data [:key->id key])]
    (if component-map
      (-> (cond-> data id (update-in [:id->key] #(dissoc % id)))
          (update-in [:out-of-date-keys] #(dissoc % key))          
          (update-in [:key->id] #(dissoc % key))
          (update-in [:key->dom] #(dissoc % key))
          (update-in [:components] #(dissoc % key))
          (update-request-set-attending component-map)
          (update-unneeded-subcomponents component-map {}))
      data)))

(defn update-set-component
  "Set the information according to the given component map,
   creating the component if necessary."
  [data {:keys [key definition] :as component-map}]
  (let [data (update-ensure-component data key)
        original-component-map (get-in data [:components key])
        new-component-map (merge original-component-map component-map)]
    (if (= definition (:definition original-component-map))
      (assoc-in data [:components key] new-component-map)
      (let [reporter (new-expression definition)
            final-map (assoc new-component-map :reporter reporter)]
        (when verbose
          (println "created component map for" (simplify-for-print key)))
        (-> data
            (update-request-set-attending original-component-map)
            (assoc-in [:components key] final-map)
            (update-request-set-attending final-map)
            (update-new-further-action
             (fn [atom] (manage reporter (:manager-data data)))))))))

(defn add-dom
  "Add dom with the given client id, key, and definition to the tracker."
  [tracker client-id key definition]
  ;; The id must not be one that we could generate.
  (assert (string? client-id))
  (assert (vector? key))
  (assert (not (#{\: \1 \2 \3 \4 \5 \6 \7 \8 \9 \0} (first client-id))))
  (swap-and-act
   tracker
   #(-> %
        (assoc-in [:key->id key] client-id)
        (assoc-in [:id->key client-id] key)
        (update-set-component {:definition definition :key key}))))

(defn id->key
  "Return the hiccup key for the client id."
  [tracker id]
  (let [data @tracker]
    (or (get-in data [:id->key id])
        (let [key (when (string? id) (string->key id))]
          ;; Make sure the key the client sent us is for a dom we know about.
          (when (or (get-in data [:key->dom key])
                    (get-in data [:components key]))
            key)))))

(defn key->id
  "Return the client id for the hiccup key."
  [tracker key]
  (or (get-in @tracker [:key->id key])
      (key->string key)))

(defn key->attributes
  "Return the attributes for the dom with the given key,
   include both attributes specified by the dom definition and by
   any component that gave rise to it."
  [tracker key]
  (let [data @tracker
        dom (get-in data [:key->dom key])]
    (into-attributes (or (and dom (dom-attributes dom)) {})
                     (get-in data [:components key :attributes]))))

(defn request-client-refresh
  "Mark all components as needing to be sent to the client."
  [tracker]
  (swap! tracker
         (fn [data] (assoc data :out-of-date-keys
                           (reduce (fn [map [key component]]
                                     (if (get-in data [:key->dom key])
                                       (assoc map key (:depth component))
                                       map))
                                   (priority-map/priority-map)
                                   (:components data))))))

(defn new-dom-tracker
  "Return a new dom tracker object"
  [md]
  (atom
   {:components {}
    :id->key {}
    :key->id {}
    :key->dom {}
    :next-version 1
    :out-of-date-keys (priority-map/priority-map)
    :manager-data md}))



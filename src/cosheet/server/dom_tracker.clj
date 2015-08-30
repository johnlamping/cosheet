(ns cosheet.server.dom-tracker
  (:require [clojure.data.priority-map :as priority-map]
            (cosheet [reporters
                      :as reporters
                      :refer [value new-expression]]
                     [utils :refer [swap-control-return!
                                    call-with-latest-value]]
                     [dom-utils :refer [add-attributes]]
                     [computation-manager :refer [manage]])))

;;; Records the current state of the dom, and which items need to be
;;; sent to the client. Has the manager compute subcomponents as
;;; needed. (Where a computation manager's job is to update a bunch of
;;; reporters, the dom tracker's job is to set up a reporter for
;;; each visible dom item, and to inform the client of changes.)

;;; The key here is that the identity of a dom component is
;;; determined by its containing dom, not by the definition that
;;; yields the component. There are thus two ways that a dom for a
;;; particular identity can change: the containing dom can change the
;;; definition for the dom it wants in that spot, or the database can
;;; change to content that that definition displays. We use a map to
;;; track the former, which points to a reporter that tracks the
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
;;;            :dom  The dom in hiccup format, as returned by the
;;;                  definition or the reporter it returns.
;;;                  Inside this dom, subcomponents are annotated as
;;;                  [:component {:sibling-key
;;;                               <A distinct object for each subcomponent>
;;;                               :definition <as above>
;;;                               :attributes
;;;                               <Additional attributes to add to the
;;;                                dom produced by the definition.
;;;                                These are typically things like
;;;                                display, that say how the component
;;;                                should fit into its parent. These
;;;                                are sent to the client as part of
;;;                                the component definition, before
;;;                                the client gets the rest of the dom.>}]
;;;  :subcomponents  A set of the keys of the subcomponents of this
;;;                  component.
;;;        :version  An version number for client coordination. It
;;;                  increases each time the dom or attributes change.

;;; The information for all components is stored in an atom,
;;; containing a map with these elements:
;;;       :components  A map from key to component map.
;;;          :id->key  A map from client id to key.
;;;          :key->id  A map from key to client id. Unlike the key,
;;;                    the client id must be a string that is allowed as a
;;;                    DOM id.
;;;          :next-id  The next free client id number.
;;; :out-of-date-keys  A priority queue of ids that the client
;;;                    needs to know about.
;;;       :management  The management for our reporters on the server.
;;;  :further-actions  A list of [function arg arg ...] calls that
;;;                    need to be performed. The function will be
;;;                    called with the atom, and the additional
;;;                    arguments. (These actions are actually
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

(defn subcomponent->component-map
  "Given a subcomponent specified inside a dom,
   create a component map."
  [{:keys [key definition]} parent-key parent-depth]
  (assert (not (nil? key)))
  (assert (not (nil? definition)))
  {:key key
   :definition definition
   :depth (inc parent-depth)})

(defn dom->subcomponents
  "Given a dom that may contain subcomponents,
   return a list of their component maps."
  [dom]
  (if (vector? dom)
    (if (= (first dom) :component)
      [(second dom)]
      (reduce (fn [subcomponents dom]
                (into subcomponents (dom->subcomponents dom)))
              [] dom))
    []))

(defn dom->keys
  "Given a dom, return all keys in the dom."
  [dom]
  (if (vector? dom)
    (let [key (let [attributes (second dom)]
                (when (map? attributes) (:key attributes)))]
      (reduce (fn [keys dom] (into keys (dom->keys dom)))
              (if (nil? key) #{} #{key}) (rest dom)))
    []))

(defn replace-key-with-id
  "Given a dom, which must be a vector, if it has a :key in its attributes,
   replace it with the corresponding id."
  [data dom]
  (let [key (:key (second dom))]
    (if (nil? key)
      dom
      (let [id (get-in data [:key->id key])]
        (assert (not (nil? id)))
        (update-in dom [1] #(-> % (assoc :id id) (dissoc :key)))))))

(defn adjust-dom-for-client
  "Given the data, the key of the containing dom, and a piece of dom,
   adjust the dom to the form the client needs, replacing keys by ids,
   and putting subcomponents into the form [:component <attributes> <id>]."
  [data dom]
  (if (vector? dom)
    (if (= (first dom) :component)
      (let [component-map (second dom) 
            id (get-in data [:key->id (:key component-map)])]
        (assert (not (nil? id)))
        [:component (:attributes component-map) id])
      (reduce (fn [subcomponents dom]
                (conj subcomponents (adjust-dom-for-client data dom)))
              [] (replace-key-with-id data dom)))
    dom))

(defn dom-for-client
  "Given the data and a key,
   prepare the dom for that key to send to the client."
  [data key]
  (let [component-map (get-in data [:components key])]
    (assert (not (nil? component-map)))
    (add-attributes
     (adjust-dom-for-client data (:dom component-map))
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
     (let [key (get-in data [:id->key id])]
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

(defn update-associate-key-to-id
  "Record that the key has the given client id."
  [data key id]
  (-> data
      (assoc-in [:key->id key] id)
      (assoc-in [:id->key id] key)))

(defn update-ensure-id-for-key
  "Make sure the data has an id for the given key."
  [data key]
  (if (get-in data [:key->id key])
    data
    (let [id (str "id" (:next-id data))]
      (-> data
          (update-associate-key-to-id key id)
          (update-in [:next-id] inc)))))

(defn update-ensure-ids-for-keys
  "Make sure the data has an id for the given key."
  [data keys]
  (reduce update-ensure-id-for-key data keys))

(defn update-unneeded-subcomponents
  "Remove all subcomponents that were in the old version of the component map
   but are not in the new one."
  [data old-component-map new-component-map]
  (reduce (fn [data key] (update-clear-component data key))
          data
          (clojure.set/difference  (:subcomponents old-component-map)
                                   (:subcomponents new-component-map))))

(defn update-dom
  "Given the data, a key, and the latest dom for the key,
   do all necessary updates."
  [data key dom]
  (let [component-map (get-in data [:components key])
        depth (:depth component-map)]
    (if (and component-map (not= dom (:dom component-map)))
      (let [subcomponent-maps (map #(subcomponent->component-map % key depth)
                                   (dom->subcomponents dom))
            new-map (-> component-map
                        (assoc :dom dom)
                        (assoc :subcomponents
                               (set (map :key subcomponent-maps)))
                        (update-in [:version] inc))]
        (-> (reduce update-set-component data subcomponent-maps)
            (update-unneeded-subcomponents component-map new-map)
            (update-ensure-ids-for-keys (dom->keys dom))
            (update-in [:out-of-date-keys] #(assoc % key depth))
            (assoc-in [:components key] new-map)))
      data)))

(defn dom-callback
  "Record a new value for the dom."
  [[_ key] reporter data-atom]
  (call-with-latest-value
   #(reporters/value reporter)
   (fn [dom]
     ;;; TODO: When not valid, but we have a previous dom,
     ;;; set a style for the dom to indicate invalidity.
     (when (reporters/valid? dom)
       (swap-and-act
        data-atom
        (fn [data]
          (when (= reporter (get-in @data-atom [:components key :reporter]))
            (update-dom data key dom))))))))

(defn set-attending
  "Set whether or not we are attending to the reporter for the dom for the key."
  [data-atom reporter key]
  (call-with-latest-value
   #(= (get-in @data-atom [:components key :reporter]) reporter)
   (fn [should-attend]
     (apply reporters/set-attendee! reporter [:dom-request key]
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
  "Make sure there is a component with the given key.
   Make an id for the key if there isn't already one."
  [data key]
  (if (get-in data [:components key])
    data
    (-> data
        (update-ensure-id-for-key key)
        (assoc-in [:components key] {:key key :version 0 :depth 0}))))

(defn update-clear-component
  "Remove the component with the given key."
  [data key]
  (let [component-map (get-in data [:components key])
        id (get-in data [:key->id key])]
    (if component-map
      (-> data
          (update-in [:out-of-date-keys] #(dissoc % key))
          (update-in [:id->key] #(dissoc % id))
          (update-in [:key->id] #(dissoc % key))
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
        (-> data
            (update-request-set-attending original-component-map)
            (assoc-in [:components key] final-map)
            (update-request-set-attending final-map)
            (update-new-further-action
             (fn [atom] (manage reporter (:management data)))))))))

(defn add-dom
  "Add dom with the given client id and definition to the tracker."
  [tracker client-id key definition]
  (swap-and-act tracker
                #(-> %
                     (update-associate-key-to-id key client-id)
                     (update-set-component {:definition definition :key key}))))

(defn id->key
  "Return the hiccup key for the client id"
  [tracker id]
  (get-in @tracker [:id->key id]))

(defn new-dom-tracker
  "Return a new dom tracker object"
  [management]
  (atom
   {:components {}
    :id->key {}
    :key->id {}
    :next-id 0
    :out-of-date-keys (priority-map/priority-map)
    :management management}))



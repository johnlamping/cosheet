(ns cosheet.server.dom-tracker
  (:require [clojure.data.priority-map :as priority-map]
            (cosheet [reporters
                      :as reporters
                      :refer [value new-expression]]
                     [utils :refer [swap-control-return!
                                    call-with-latest-value]]
                     [computation-manager :refer [manage]])
            (cosheet.server [render :as render])))

;;; Records the current state of the dom, and which items need to be
;;; sent to the client. Has the manager compute subcomponents as needed.

;;; The basic data structure is a component map, which contains dom
;;; for a component and information about it. It fields can include:
;;;            :key  A unique key for this component.
;;;          :depth  The depth of this component in the component
;;;                  hierarchy, used to make sure that parents are
;;;                  sent to the client before their children.
;;;     :definition  An application that will compute the dom or
;;;                  return a reporter to compute it. We record the
;;;                  definition so we know that if the component is
;;;                  updated with the same definition, we don't have
;;;                  to recompute.
;;;       :reporter  The result of running the definition, either the
;;;                  dom, or a reporter that computes it.
;;;     :attributes  Additional attributes to add to the dom produced
;;;                  by the definition. These are typically things
;;;                  like display, that say how the component should
;;;                  fit into its parent.
;;;            :dom  The dom in hiccup format, not including the id or
;;;                  added attributes.
;;;                  Inside this dom, subcomponents are annotated as
;;;                  [:component {:sibling-key sibling-key
;;;                               :definition definition
;;;                               :attributes <attributes to add>}].
;;;  :subcomponents  A set of the keys of the subcomponents of this
;;;                  component.
;;;             :id  The id of this component in the client.
;;;        :version  An increasing version number for client
;;;                  coordination.

;;; This information is stored in an atom, containing a map with these
;;; elements:
;;;      :components  A map from key to component map.
;;;         :id->key  A map from client id to key.
;;;         :next-id  The next free client id.
;;; :out-of-date-ids  A priority queue of ids that the client
;;;                   needs to know about
;;;      :management  The management that runs our tasks on the server.
;;; :pending-actions  A list of [function arg arg ...] calls that
;;;                   need to be performed. The function will be
;;;                   called with the atom, and the additional
;;;                   arguments. (These actions are actually
;;;                   be stored in the atom, but are added to the
;;;                   data before it is stored to request actions.)

;;; TODO: write a converter from our format to the format to send the client.

(defn update-new-pending-action
  "Given a component map, add an an action to the pending actions.
   The action will be called with atom, followed by any additional arguments
   specified."
  [data & action]
  (update-in data [:pending-actions] (fnil conj []) (vec action)))

(defn swap-and-act
  "Atomicly call the function on the atom's data.
   The function should return the new data for the atom,
   which may also contain a temporary field, :pending-actions, with
   a list of actions that should be performed. Perform those actions,
   also passing in the atom to each action."
  [atom f]
  (let [actions (swap-control-return!
                 atom
                 (fn [data] (let [new-data (f data)]
                              [(dissoc new-data :pending-actions)
                               (:pending-actions new-data)])))]
    (doseq [action actions]
      (apply (first action) atom (rest action)))))

(defn make-key
  "Make a key from a parent key and the sibling key within that parent."
  [parent-key sibling-key]
  (if (or (nil? parent-key) (empty? parent-key))
    [sibling-key]
    (conj parent-key sibling-key)))

(def update-set-component)
(def update-clear-component)

(defn dom->subcomponents
  "Given a dom containing subcomponents,
   return a list of their component maps."
  [dom]
  (if (vector? dom)
    (if (= (first dom) :component)
      [(second dom)]
      (reduce (fn [subcomponents dom]
                (into subcomponents (dom->subcomponents dom)))
              [] dom))
    []))

(defn contextualize-subcomponent
  "Given a subcomponent straight from a dom,
   flesh out the rest of its information."
  [{:keys [sibling-key] :as subcomponent-map} parent-key parent-depth]
  (-> subcomponent-map
      (dissoc :sibling-key)
      (assoc :key (make-key parent-key sibling-key))
      (assoc :depth (inc parent-depth))))

(defn update-unneeded-subcomponents
  "Remove all components that were in the old version of the component map
   but are not in the new one."
  [data old-component-map new-component-map]
  (reduce (fn [data key] (update-clear-component data key))
          data
          (clojure.set/difference  (:subcomponents old-component-map)
                                   (:subcomponents new-component-map))))

(defn update-dom
  "Given the data, a key, and the latest dom for key, do all necessary updates."
  [data key dom]
  (let [component-map (get-in data [:components key])
        depth (:depth component-map)]
    (if (and component-map (not= dom (:dom component-map)))
      (let [subcomponent-maps (map #(contextualize-subcomponent % key depth)
                                   (dom->subcomponents dom))
            new-map (-> component-map
                        (assoc :dom dom)
                        (assoc :subcomponents
                               (set (map :key subcomponent-maps)))
                        (update-in [:version] inc))]
        (-> (reduce update-set-component data subcomponent-maps)
            (update-unneeded-subcomponents component-map new-map)
            (update-in [:out-of-date-ids] #(assoc % (:id component-map) depth))
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

(defn update-request-attending
  "Add a pending action to start or stop attending to the reporter
   of the given component-map, depending on whether the map is still active."
  [data component-map]
  (let [reporter (:reporter component-map)
        key (:key component-map)]
    (if reporter
      (update-new-pending-action data set-attending reporter key)
      data)))

(defn update-ensure-component
  "Make sure there is a component with the given key."
  [data key]
  (if (get-in data [:components key])
    data
    (let [id (:next-id data)]
      (-> data
          (assoc-in [:components key] {:id id :key key :version 0 :depth 0})
          (assoc-in [:id->key id] key)
          (update-in [:next-id] inc)))))

(defn update-clear-component
  "Remove the component with the given key."
  [data key]
  (let [component-map (get-in data [:components key])]
    (if component-map
      (let [id (:id component-map)]
        (-> data
            (update-in [:out-of-date-ids] #(dissoc % id))
            (update-in [:id->key] #(dissoc % id))
            (update-in [:components] #(dissoc % key))
            (update-request-attending component-map)
            (update-unneeded-subcomponents component-map {})))
      data)))

(defn update-set-component
  "Set the information according to the given component map,
   creating the component if necessary."
  [data {:keys [key definition attributes] :as component-map}]
  (let [data (update-ensure-component data key)
        original-component-map (get-in data [:components key])
        new-component-map (merge original-component-map component-map)]
    (if (= definition (:definition original-component-map))
      ;; Even though the definition is unchanged, the client still needs to
      ;; be updated if the attributes have changed.
      (-> (cond-> data
            (not= attributes (:attributes original-component-map)) 
            (update-in [:out-of-date-ids]
                       #(let [{:keys [id depth]} new-component-map]
                          (assoc % id depth))))
          (assoc-in [:components key] new-component-map))
      (let [reporter (new-expression definition)
            final-map (assoc new-component-map :reporter reporter)]
        (-> data
            (update-request-attending original-component-map)
            (assoc-in [:components key] final-map)
            (update-request-attending final-map)
            (update-new-pending-action
             (fn [atom] (manage reporter (:management data)))))))))

(defn new-dom-tracker
  "Return a new dom tracker object"
  [management]
  (atom
   {:components {}
    :id->key {}
    :next-id 0
    :out-of-date-ids (priority-map/priority-map)
    :management management}))



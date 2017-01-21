(ns cosheet.server.tabs-render
  (:require (cosheet [entity :as entity]
                     [debug :refer [simplify-for-print]]
                     [dom-utils :refer [dom-attributes
                                        into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq cache]]
                     [canonical :refer [canonicalize-list canonical-to-list
                                        canonical-set-to-list
                                        common-canonical-multisets]])
            (cosheet.server
             [referent :refer [item-referent virtual-referent
                               item-or-exemplar-referent
                               semantic-elements-R semantic-to-list-R]]
             [hierarchy :refer [hierarchy-by-all-elements
                                hierarchy-node?
                                hierarchy-node-descendants
                                hierarchy-node-items-referent
                                hierarchy-last-item-referent
                                hierarchy-node-next-level
                                hierarchy-node-example-elements]]
             [order-utils :refer [order-items-R]]
             [render-utils :refer [make-component vertical-stack
                                   virtual-item-DOM item-stack-DOM
                                   condition-satisfiers-R]]
             [render :refer [starting-inherited]]
             [item-render :refer [elements-DOM-R]])))

(def tabs-subtree-DOM-R)

(def base-tab-width 150)

(defn new-tab-virtual-referent
  [adjacent-referent inherited]
  (virtual-referent (:template inherited)
                    (:subject-referent inherited)
                    adjacent-referent))

(defn add-column-command
  [adjacent-referent inherited]
  {:add-column {:referent (new-tab-virtual-referent
                           adjacent-referent inherited)}})

(defn tabs-node-DOM-R
  [node inherited]
  "Generate the dom for a node of the tabs hierarchy, but not any of its
  children."
  (expr-let [elements (hierarchy-node-example-elements node)]
    (let [subject-referent (:subject-referent inherited)
          tabs-referent (hierarchy-node-items-referent node subject-referent)
          inherited-down
          (-> inherited
              (assoc :subject tabs-referent)
              (update :selectable-attributes
                      #(cond-> (into-attributes
                                % (add-column-command  tabs-referent inherited))
                         (= (count (hierarchy-node-next-level node)) 1)
                         (into-attributes {:delete {:referent tabs-referent}
                                           :select {:special :tab}})))
              (dissoc :template :chosen-tab))]
      (elements-DOM-R elements false nil inherited-down))))

(defn tabs-member-DOM
  "Generate the DOM for a member in a hierarchy that is not the only
  descendant of its parent. It will be displayed under its parent but
  has no elements of its own to show."
  [tab-item inherited]
  (let [tab-referent (item-or-exemplar-referent
                      tab-item (:subject-referent inherited))
        inherited-down (-> inherited
                           (assoc :subject-referent tab-referent)
                           (update-in
                            [:selectable-attributes]
                            #(into-attributes
                              (into-attributes % (add-column-command
                                                  tab-referent inherited))
                              {:delete {:referent tab-referent}})))
        key (conj (:key-prefix inherited) (:item-id tab-item))]
    (add-attributes
     (virtual-item-DOM key tab-referent :after inherited-down)
     (let [is-chosen (= tab-item (:chosen-tab inherited))]
       {:class (if is-chosen "tab chosen" "tab")}))))

(defn tabs-node-or-element-DOM-R
  "Given something that is either a hieararchy node or element,
  generate its DOM, but not the DOM for any children."
  [node-or-element inherited]
  (if (hierarchy-node? node-or-element)
    (tabs-subtree-DOM-R
     node-or-element
     ;; The child nodes might use the same item in their keys as their parent,
     ;; so add to the prefix to make their keys distinct.
     (update inherited :key-prefix #(conj % :nested)))
    (tabs-member-DOM
     (:item node-or-element) inherited)))

(defn virtual-tab-DOM
  [adjacent-referent inherited]
  (let [key (conj (:key-prefix inherited) :virtualTab)
        inherited (assoc inherited
                         :subject-referent (new-tab-virtual-referent
                                            adjacent-referent inherited))]))

(defn tabs-subtree-DOM-R
  ;; :template in inherited gives the template for a new tab.
  [node inherited]
  (expr-let [node-dom (tabs-node-DOM-R node inherited)]
    (let [next-level (hierarchy-node-next-level node)]
      (expr-let
          [dom
           (if (= (count next-level) 1)
             ;; We have only one descendant; it must be the column request.
             (let [is-chosen (= (:item (first next-level))
                                (:chosen-tab inherited))]
               (add-attributes node-dom
                               {:class (if is-chosen "tab chosen" "tab")}))
             (let [properties-list (canonical-set-to-list (:properties node))
                   inherited-down (update
                                   inherited :template
                                   #(list* (concat % properties-list)))]
               (expr-let
                   [dom-seqs (expr-seq map  #(tabs-node-or-element-DOM-R
                                              % inherited)
                                       next-level)]
                 [:div {}
                  (add-attributes node-dom {:class "multi-tab"})
                  (into [:div {:class "tab-sequence"}]
                        dom-seqs)])))]
        (let [num-tabs (count (hierarchy-node-descendants node))
              width (+ (* num-tabs (- base-tab-width 2)) 2)]
          (add-attributes dom {:class (cond-> "column-header")
                               :style {:width (str width "px")}}))))))

(defn tabs-DOM-R
  "Return a reporter giving the DOM for the elements of the given item as tabs."
  [tabs-subject chosen-tab inherited]
  (let [inherited (assoc starting-inherited
                         :selector-category :tab
                         :template '(nil (nil (:table :non-semantic)
                                              (:non-semantic :non-semantic))
                                         (:tab :non-semantic))
                         :subject-referent (item-referent tabs-subject)
                         :chosen-tab chosen-tab)]
    (expr-let [tabs (expr order-items-R
                      (entity/label->elements tabs-subject :tab))
               hierarchy (hierarchy-by-all-elements tabs)]
      (expr-let [doms (expr-seq map #(tabs-subtree-DOM-R % inherited)
                                hierarchy)]
        (into [:div {:class "tabs-holder"}]
              (concat doms [(virtual-tab-DOM
                             (or (hierarchy-last-item-referent hierarchy)
                                 (item-referent tabs-subject))
                             inherited)]))))))

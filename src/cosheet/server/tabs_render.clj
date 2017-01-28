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
             [referent :refer [item-referent? exemplar-referent?
                               item-referent virtual-referent
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

(defn inherited-for-tab-elements
  "Return the information to be inherited down to the elements of a tabs DOM."
  [tabs-referent select-selects-tab delete-deletes-tab inherited]
  (-> inherited
      (assoc :subject tabs-referent)
      (update :selectable-attributes
              #(into-attributes %
                (cond-> (add-column-command tabs-referent inherited)
                  select-selects-tab
                  (assoc :select {:referent tabs-referent
                                  :special :tab})
                  delete-deletes-tab
                  (assoc :delete {:referent tabs-referent}))))
      (dissoc :template :chosen-tab)))

(defn tabs-node-or-member-DOM-R
  [node-or-member inherited]
  "Generate the dom for a node of the tabs hierarchy, but not any of its
  children."
  (expr-let [elements (hierarchy-node-example-elements node-or-member)]
    (let [subject-referent (:subject-referent inherited)
          tabs-referent (hierarchy-node-items-referent
                         node-or-member subject-referent)
          descendants (hierarchy-node-descendants node-or-member)
          inherited-down (inherited-for-tab-elements
                          tabs-referent
                          (= (count descendants) 1) (<= (count elements) 1)
                          inherited)]
      (if (hierarchy-node? node-or-member)
        (elements-DOM-R elements false nil inherited-down)
        (let [key (conj (:key-prefix inherited)
                        (:item-id (:item node-or-member)))]
          (virtual-item-DOM key tabs-referent :after inherited-down))))))

(defn tabs-tree-DOM-R
  "Given something that is either a hieararchy node or element,
  generate its DOM, but not the DOM for any children."
  [node-or-member inherited]
  (if (hierarchy-node? node-or-member)
    (tabs-subtree-DOM-R
     node-or-member
     ;; The child nodes might use the same item in their keys as their parent,
     ;; so add to the prefix to make their keys distinct.
     (update inherited :key-prefix #(conj % :nested)))
    (add-attributes
     (tabs-node-or-member-DOM-R node-or-member inherited)
     (let [is-chosen (= (:item node-or-member) (:chosen-tab inherited))]
       {:class (if is-chosen "tab chosen" "tab")}))))

(defn virtual-tab-DOM
  [adjacent-referent inherited]
  (let [key (conj (:key-prefix inherited) :virtualTab)
        inherited (assoc inherited
                         :subject-referent (new-tab-virtual-referent
                                            adjacent-referent inherited))]))

(defn tabs-subtree-DOM-R
  ;; :template in inherited gives the template for a new tab.
  [node inherited]
  (expr-let [node-dom (tabs-node-or-member-DOM-R node inherited)]
    (let [next-level (hierarchy-node-next-level node)]
      (expr-let
          [dom
           (if (= (count next-level) 1)
             ;; We have only one descendant; it must be the tab request.
             (let [is-chosen (= (:item (first next-level))
                                (:chosen-tab inherited))]
               (add-attributes node-dom
                               {:class (if is-chosen "tab chosen" "tab")}))
             (let [properties-list (canonical-set-to-list (:properties node))
                   inherited-down (update
                                   inherited :template
                                   #(list* (concat % properties-list)))]
               (expr-let
                   [dom-seqs (expr-seq map #(tabs-tree-DOM-R % inherited)
                                       next-level)]
                 [:div {}
                  (add-attributes node-dom {:class "multi-tab"})
                  (into [:div {:class "tab-sequence"}]
                        dom-seqs)])))]
        (let [num-tabs (count (hierarchy-node-descendants node))
              width (+ (* num-tabs (- base-tab-width 2)) 2)]
          (add-attributes dom {:style {:width (str width "px")}}))))))

(def new-tab-elements '((:tab :non-semantic)
                        (""
                         (:tab-topic :non-semantic)
                         (:table :non-semantic)
                         (anything (??? :tag)
                                   (:row-condition :non-semantic)
                                   (:non-semantic :non-semantic))
                         (anything (??? :tag)
                                   (:column :non-semantic)
                                   (:non-semantic :non-semantic)))))

(defn tabs-DOM-R
  "Return a reporter giving the DOM for the elements of the given item as tabs."
  [tabs-subject chosen-tab inherited]
  (let [inherited (assoc starting-inherited
                         :selector-category :tab
                         :template (cons nil new-tab-elements)
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

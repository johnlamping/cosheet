(ns cosheet.server.tabs-render
  (:require (cosheet [entity :as entity]
                     [debug :refer [simplify-for-print]]
                     [hiccup-utils :refer [dom-attributes
                                           into-attributes add-attributes]]
                     [expression :refer [expr expr-let expr-seq cache]]
                     [canonical :refer [canonicalize-list canonical-to-list
                                        canonical-set-to-list
                                        common-canonical-multisets]])
            (cosheet.server
             [referent :refer [item-referent? exemplar-referent?
                               item-referent virtual-referent
                               union-referent-if-needed union-referent
                               item-or-exemplar-referent
                               semantic-elements-R semantic-to-list-R]]
             [hierarchy :refer [hierarchy-by-all-elements-R
                                hierarchy-node?
                                hierarchy-node-descendants
                                hierarchy-node-items-referent
                                hierarchy-last-item-referent
                                hierarchy-node-next-level
                                hierarchy-node-example-elements
                                replace-hierarchy-leaves-by-nodes]]
             [order-utils :refer [order-items-R]]
             [model-utils :refer [new-tab-elements]]
             [render-utils :refer [virtual-item-DOM condition-satisfiers-R]]
             [item-render :refer [elements-DOM-R]])))

(def tabs-tree-DOM-R)

(def base-tab-width 150)

(defn inherited-for-tab-elements
  "Return the information to be inherited down to the elements of a tabs DOM.
  tabs-referent gives the tab or tabs that these elements apply to."
  [tab-items tabs-elements example-elements tabs-referent inherited]
  (let [subject-referent (:subject-referent inherited)
        ;; Make sure the adjacent referent returns just one group, since
        ;; the subject is just one group.
        adjacent-referent (if (item-referent? tabs-referent)
                            tabs-referent
                            (union-referent [tabs-referent]))
        ;; For tabs with just one element (or none), delete the tab.
        delete-referent
        (when (<= (count example-elements) 1)
          (union-referent-if-needed
           (map (fn [tab-item tab-elements]
                  (if (or (= (count tab-elements) 1)
                          ;; This case can come up while responding to a change.
                          (empty? example-elements))
                    (item-referent tab-item)
                    (item-or-exemplar-referent
                     (first example-elements) (item-referent tab-item))))
                tab-items tabs-elements)))]
    (-> inherited
        (assoc :subject-referent tabs-referent
               :template '(nil))
        (update
         :attributes
         #(conj (or % [])
                [#{:label :optional} #{:content}
                 (cond-> {:add-column
                          {:referent (virtual-referent
                                      (:template inherited)
                                      (:subject-referent inherited)
                                      adjacent-referent
                                      :selector :first-group)}}
                   (= (count tab-items) 1)
                   (assoc :selected {:referent tabs-referent
                                     :special :tab})
                   delete-referent
                   (assoc :delete  {:referent delete-referent}))]))
        (dissoc :chosen-tab))))

(defn tabs-node-or-member-DOM-R
  [node-or-member inherited]
  "Generate the dom for a node of the tabs hierarchy, but not any of its
  children."
  (let [subject-referent (:subject-referent inherited)
        tabs-referent (hierarchy-node-items-referent
                       node-or-member subject-referent)
        tab-items (map :item (hierarchy-node-descendants node-or-member))]
    (expr-let [example-elements (hierarchy-node-example-elements node-or-member)
               tabs-elements (expr-seq map semantic-elements-R tab-items)]
      (let [inherited-down (inherited-for-tab-elements
                            tab-items tabs-elements example-elements
                            tabs-referent inherited)]
        (if (hierarchy-node? node-or-member)
          (expr-let [dom (elements-DOM-R example-elements false nil
                                         inherited-down)
                     elements-elements (expr-seq map semantic-elements-R
                                                 example-elements)]
            (cond-> dom
              (not (empty? (apply concat elements-elements)))
              (add-attributes {:class "complex"})))        
          (let [key (conj (:key-prefix inherited)
                          (:item-id (first tab-items)))]
            (add-attributes
             (virtual-item-DOM key tabs-referent :after inherited-down)
             {:class "empty-child"})))))))

(defn tabs-subtree-DOM-R
  ;; :template in inherited gives the template for a new tab.
  [node inherited]
  (expr-let [node-dom (tabs-node-or-member-DOM-R node inherited)]
    (let [next-level (hierarchy-node-next-level node)]
      (expr-let
          [dom
           (if (= (count next-level) 1)
             ;; We have only one descendant; it must be the tab request.
             (add-attributes node-dom {:class "tab"})
             (let [properties-list (canonical-set-to-list (:properties node))
                   inherited-down
                   (-> inherited
                       (update :template
                               #(list* (concat % properties-list)))
                       ;; The child nodes can use the same tab in their keys
                       ;; as their parent,  so add to the prefix to make their
                       ;; keys distinct.
                       (update :key-prefix #(conj % :nested)))]
               (expr-let
                   [next-doms (expr-seq map #(tabs-tree-DOM-R % inherited-down)
                                        next-level)]
                 [:div {:class "tab-tree"}
                  (add-attributes node-dom {:class "multi-tab"})
                  (into [:div {:class "tab-sequence"}]
                        ;; We list the tabs in reverse order, so the
                        ;; logically first tab will have priority in
                        ;; the stacking order.  Then in the style, we
                        ;; say to lay them out in reverse row order.
                        (reverse next-doms))])))]
        dom))))

(defn tabs-tree-DOM-R
  "Given something that is either a hieararchy node or element,
  generate its DOM."
  [node-or-member inherited]
  (let [is-chosen (seq (filter #(= (:item %) (:chosen-tab inherited))
                               (hierarchy-node-descendants node-or-member)))]
    (expr-let
        [dom (if (hierarchy-node? node-or-member)
               (tabs-subtree-DOM-R
                node-or-member inherited)
               (expr-let [dom (tabs-node-or-member-DOM-R
                               node-or-member inherited)]
                 (add-attributes dom {:class "tab"})))]
      (cond-> dom is-chosen (add-attributes {:class "chosen"})))))

(defn virtual-tab-DOM
  [subject-referent hierarchy inherited]
  (let [key (conj (:key-prefix inherited) :virtualTab)
        v-ref (virtual-referent
                     (cons "" new-tab-elements)
                     subject-referent
                     (hierarchy-last-item-referent hierarchy)
                     ;; Avoid turning 'anything in the template into ""
                     :selector :first-group)
        inherited (assoc inherited :subject-referent v-ref)
        virtual-inherited (assoc inherited :template "")]
    (add-attributes
     (virtual-item-DOM key nil :after virtual-inherited)
     {:class "tab virtualTab"
      :selected {:special :new-tab}})))

(defn tabs-DOM-R
  "Return a reporter giving the DOM for the elements of the given item as tabs."
  [tabs-subject chosen-tab inherited]
  (let [subject-referent (item-referent tabs-subject)
        tabs-inherited (assoc inherited
                              :template (cons "" (cons "" new-tab-elements))
                              :subject-referent subject-referent
                              :chosen-tab chosen-tab)]
    (expr-let [tabs (expr order-items-R
                      (entity/label->elements tabs-subject :tab))
               hierarchy (hierarchy-by-all-elements-R tabs)]
      (expr-let [doms (expr-seq map #(tabs-tree-DOM-R % tabs-inherited)
                                hierarchy)]
        (let [virtual-tab (virtual-tab-DOM
                           subject-referent hierarchy inherited)]
          (into [:div {:class "tabs-holder"}]
                ;; We list the tabs in reverse order, so the logically first
                ;; tab will have priority in the stacking order.
                ;; Then in the style, we say to lay them out in reverse
                ;; row order.
                ;; We put an empty div at the end, so clicks beyond the
                ;; virtual tab won't be referred to the virtual tab, which
                ;; would otherwise be the closest element.
                (concat [[:div] virtual-tab] (reverse doms))))))))


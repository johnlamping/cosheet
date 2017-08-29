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
             [render-utils :refer [virtual-item-DOM hierarchy-node-DOM-R]]
             [item-render :refer [elements-DOM-R]])))

(def base-tab-width 150)

(defn inherited-for-tab-elements
  "Return the information to be inherited down to the elements of a tabs DOM.
  tabs-referent gives the tab or tabs that these elements apply to."
  [tab-items tabs-element-counts example-elements tabs-referent inherited]
  (let [subject-referent (:subject-referent inherited)
        ;; Make sure the adjacent referent returns just one group, since
        ;; the subject is just one group.
        adjacent-referent (if (item-referent? tabs-referent)
                            tabs-referent
                            (union-referent [tabs-referent]))
        ;; For tabs with just one element (or none), delete deletes the tab.
        delete-referent
        (when (<= (count example-elements) 1)
          (union-referent-if-needed
           (map (fn [tab-item tab-element-count]
                  (if (or (= tab-element-count 1)
                          ;; This case can come up while responding to a change.
                          (empty? example-elements))
                    (item-referent tab-item)
                    (item-or-exemplar-referent
                     (first example-elements) (item-referent tab-item))))
                tab-items tabs-element-counts)))]
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
                   (assoc :delete {:referent delete-referent}))]))
        (dissoc :chosen-tab))))

(defn tab-elements-DOM-R
  [node inherited]
  "Generate the dom for a node of the tabs hierarchy, but not any of its
  children."
  (let [subject-referent (:subject-referent inherited)
        tabs-referent (hierarchy-node-items-referent
                       node subject-referent)
        tab-items (map :item (hierarchy-node-descendants node))]
    (expr-let [example-elements (hierarchy-node-example-elements node)
               tabs-elements (expr-seq map semantic-elements-R tab-items)]
      (let [inherited-down (inherited-for-tab-elements
                            tab-items (map count tabs-elements)
                            example-elements tabs-referent inherited)]
        (if (seq (:properties node))
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

(defn tabs-child-info
  "Adjust the information for child tabs."
  [node function-info inherited]
  [function-info
   (-> inherited
       (update :template
               #(list* (concat % (canonical-set-to-list (:properties node)))))
       ;; The child nodes can use the same tab request in their keys
       ;; as their parent, so add to the prefix to make their
       ;; keys distinct.
       (update :key-prefix #(conj % :nested)))])

(defn tabs-node-DOM-R
  ;; :template in inherited gives the template for a new tab.
  [node child-doms function-info inherited]
  (expr-let [elements-dom (tab-elements-DOM-R node inherited)]
    (expr-let
        [dom
         (if (empty? child-doms)
           (add-attributes elements-dom {:class "tab"})
           [:div {:class "tab-tree"}
            (add-attributes elements-dom {:class "multi-tab"})
            (into [:div {:class "tab-sequence"}]
                  ;; We list the tabs in reverse order, so the
                  ;; logically first tab will have priority in
                  ;; the stacking order.  Then in the style, we
                  ;; say to lay them out in reverse row order.
                  (reverse child-doms))])]
      (let [is-chosen (seq (filter #(= (:item %) (:chosen-tab inherited))
                                   (hierarchy-node-descendants node)))]
        (cond-> dom is-chosen (add-attributes {:class "chosen"}))))))

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
      (let [hierarchy (replace-hierarchy-leaves-by-nodes hierarchy)]
        (expr-let [doms (expr-seq map #(hierarchy-node-DOM-R
                                        % tabs-node-DOM-R tabs-child-info
                                        nil tabs-inherited)
                                  hierarchy)]
          (let [virtual-tab (virtual-tab-DOM
                             subject-referent hierarchy inherited)]
            [:div {:class "tabs-wrapper"}
             [:div#batch-edit.tool
              [:img {:src "../icons/edit.gif"}]
              [:div.tooltip "batch edit (C-B)"]]
             [:div.toolgap]
             (into [:div {:class "tabs-holder"}]
                   ;; We list the tabs in reverse order, so the logically first
                   ;; tab will have priority in the stacking order.
                   ;; Then in the style, we say to lay them out in reverse
                   ;; row order.
                   ;; We put an empty div at the end, so clicks beyond the
                   ;; virtual tab won't be referred to the virtual tab, which
                   ;; would otherwise be the closest element.
                   (concat [[:div] virtual-tab] (reverse doms)))]))))))


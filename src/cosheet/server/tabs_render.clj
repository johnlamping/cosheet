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
                               union-referent-if-needed
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
             [item-render :refer [elements-DOM-R]])))

(def tabs-tree-DOM-R)

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
  "Return the information to be inherited down to the elements of a tabs DOM.
  tabs-referent gives the tab or tabs that these elements apply to."
  [tab-items tabs-elements example-elements tabs-referent inherited]
  (println "inherited for " (count tab-items) (count example-elements))
  (let [subject-referent (:subject-referent inherited)
        ;; For tabs with just one element (or none), delete the tab.
        delete-referent
        (when (<= (count example-elements) 1)
          (union-referent-if-needed
           (map (fn [tab-item tab-elements]
                  (if (= (count tab-elements) 1)
                    (item-referent tab-item)
                    (item-or-exemplar-referent
                     (first example-elements) (item-referent tab-item))))
                tab-items tabs-elements)))]
    (-> inherited
        (assoc :subject-referent tabs-referent
               :template '(nil))
        (update
         :selectable-attributes
         #(into-attributes
           %
           (cond-> (add-column-command tabs-referent inherited)
             (= (count tab-items) 1)
             (assoc :selected {:referent tabs-referent
                               :special :tab})
             delete-referent
             (assoc :delete  {:referent delete-referent}))))
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
                          (:item-id (:item node-or-member)))]
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
                   inherited-down (update
                                   inherited :template
                                   #(list* (concat % properties-list)))]
               (expr-let
                   [next-doms (expr-seq map #(tabs-tree-DOM-R % inherited)
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
                node-or-member
                ;; The child nodes might use the same item in their keys as
                ;; their parent,  so add to the prefix to make their keys
                ;; distinct.
                (update inherited :key-prefix #(conj % :nested)))
               (expr-let [dom (tabs-node-or-member-DOM-R node-or-member
                                                         inherited)]
                 (add-attributes dom {:class "tab"})))]
      (cond-> dom is-chosen (add-attributes {:class "chosen"})))))

;;; TODO: Put more of the work here.
(defn virtual-tab-DOM
  [subject-referent adjacent-referent inherited]
  (let [key (conj (:key-prefix inherited) :virtualTab)
        inherited (assoc inherited :subject-referent subject-referent)]
    (add-attributes
     (virtual-item-DOM key adjacent-referent :after inherited)
           {:class "tab virtualTab"})))

(def new-tab-elements '((:tab :non-semantic)
                        (""
                         (:non-semantic :non-semantic)
                         (:tab-topic :non-semantic)
                         (:table :non-semantic)
                         (anything (??? :tag)
                                   (:row-condition :non-semantic)
                                   (:non-semantic :non-semantic))
                         (anything-immutable (??? :tag)
                                             (:column :non-semantic)
                                             (:non-semantic :non-semantic)))))

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
               hierarchy (hierarchy-by-all-elements tabs)]
      (expr-let [doms (expr-seq map #(tabs-tree-DOM-R % tabs-inherited)
                                hierarchy)]
        (let [v-ref (virtual-referent
                     (cons "" new-tab-elements)
                     subject-referent
                     (hierarchy-last-item-referent hierarchy))
              virtual-inherited (assoc inherited :template "")
              virtual-tab (virtual-tab-DOM v-ref nil
                                           virtual-inherited)]
          (into [:div {:class "tabs-holder"}]
                ;; We list the tabs in reverse order, so the logically first
                ;; tab will have priority in the stacking order.
                ;; Then in the style, we say to lay them out in reverse
                ;; row order.
                (concat [virtual-tab] (reverse doms))))))))

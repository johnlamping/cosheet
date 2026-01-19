(ns cosheet2.server.item-render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet2
             [orderable :as orderable]
             [query :refer [matching-elements]]
             [debug :refer [envs-to-list simplify-for-print]]
             [entity :refer [id->entity name-label link-type object-type
                             make-object-list]]
             entity-impl
             [store :refer [new-element-store get-new-object-id]]
             store-impl
             mutable-store-impl
             [calculator :refer [new-calculator-data computation-value]]
             [task-queue :refer [new-priority-task-queue]]
             [store-utils :refer [add-element add-object
                                  add-universal-objects]]
             [test-utils :refer [check any as-set]])
            (cosheet2.server
             [model-utils :refer [semantic-label-elements]]
             [hierarchy :refer [item-maps-by-elements
                                hierarchy-by-canonical-info]]
             [render :refer [basic-dom-specification]]
             [render-utils :refer [make-sequential-template
                                   make-placeholder-object-template
                                   make-object-reference-template
                                   make-virtual-label-template
                                   ensure-label-object]]
             [action-data :refer [default-get-action-data
                                  composed-get-action-data
                                  parallel-items-get-action-data
                                  get-item-or-exemplar-action-data
                                  get-item-do-batch-edit-action-data
                                  parallel-items-get-do-batch-edit-action-data
                                  get-pass-through-action-data
                                  get-virtual-action-data]]
             [item-render :refer :all])
            ; :reload
            ))

(def orderables (reduce (fn [os _]
                          (vec (concat (pop os)
                                       (orderable/split (peek os) :after))))
                        [orderable/initial]
                        (range 6)))
(def o1 (nth orderables 0))
(def o2 (nth orderables 1))
(def o3 (nth orderables 2))
(def o4 (nth orderables 3))
(def o5 (nth orderables 4))
(def o6 (nth orderables 5))
(def unused-orderable (nth orderables 6))

(def label-object-template (ensure-label-object 'anything))
(def label-template `(~label-object-template))
(def virtual-label-template (make-virtual-label-template 'anything))

;;; We make functions that abbreviate the common functions that can be
;;; embedded in components.
;;; (We use functions, rather than constants, so this file doesn't have
;;; to be reloaded if any of the files that defines the underlying
;;; functions is reloaded.)

(defn virt-DOM [] render-virtual-DOM)

(defn default-AD [] default-get-action-data)
(defn comp-AD [] composed-get-action-data)
(defn item-AD [] get-item-or-exemplar-action-data)
(defn pass-AD [] get-pass-through-action-data)
(defn parallel-AD [] parallel-items-get-action-data)
(defn virt-AD [] get-virtual-action-data)
(defn item-do-batch-AD []
  get-item-do-batch-edit-action-data)
(defn parallel-do-batch-AD []
  parallel-items-get-do-batch-edit-action-data)

(defn run-renderer
  "Run the renderer on the output of the data getter, then run the
  resulting dom-R, to get the final dom."
  [renderer spec mutable-store]
  (let [queue (new-priority-task-queue 0)
        cd (new-calculator-data queue)
        dom-R (renderer spec mutable-store)]
    (computation-value dom-R cd)))

(deftest virtual-DOM-test
  (is (check (virtual-DOM-component {:template "foo"
                                     :relative-id :bar
                                     :position :before})
             [:component {:template "foo"
                          :position :before
                          :relative-id :bar
                          :render-dom (virt-DOM)
                          :get-action-data (virt-AD)}])))

(defn add-label-object-to-store
  "Add a label object with the given name to the store.
   Return the updated store and the id of the label object."
  [store name]
  (let [[s1 label-oid] (get-new-object-id store)
        [s2 _] (add-element s1 label-oid link-type)
        [s3 label-name-id] (add-element s2 label-oid name)
        [s4 _] (add-element s3 label-name-id name-label)]
    [s4 label-oid]))

(defn make-joe-and-jane-store
  "Make a store with labels test-label and foo-label and with
    [:object 'Joe' test-label foo-label
    [:object 'Jane' test-label
  Return the store and a map from names of objects and elements to their ids."
  []
  (let [s (add-universal-objects (new-element-store))
        [s1 test-label-oid] (add-label-object-to-store s "test")
        test-label-object (id->entity test-label-oid s1)
        [s2 foo-label-oid] (add-label-object-to-store s1 "foo")
        foo-label-object (id->entity foo-label-oid s2)
        [s4 joe-id] (add-element s2 nil "Joe")
        [s5 joe-test-id] (add-element s4 joe-id test-label-object)
        [s7 joe-foo-id] (add-element s5 joe-id foo-label-object)
        [s9 jane-id] (add-element s7 nil "Jane")
        [store jane-test-id] (add-element s9 jane-id test-label-object)]
    [store {:test-label-oid test-label-oid
            :foo-label-oid foo-label-oid
            :joe-id joe-id
            :joe-test-id joe-test-id
            :joe-foo-id joe-foo-id
            :jane-id jane-id
            :jane-test-id jane-test-id}]))

(deftest horizontal-label-hierarchy-node-DOM-test
  (let [[store ids] (make-joe-and-jane-store)
        ordered-entities [(id->entity (:joe-id ids) store)
                          (id->entity (:jane-id ids) store)]
        labelses (map semantic-label-elements ordered-entities)
        item-maps (item-maps-by-elements ordered-entities labelses)
        hierarchy (hierarchy-by-canonical-info item-maps)
        node (first hierarchy)]
    ;; A node with no leaves.
    (is (check
         (horizontal-label-hierarchy-node-DOM node {:width 0.75})
         [:component
          {:template label-template
           :width 1.5
           :parallel-ids [(:joe-id ids) (:jane-id ids)]
           :relative-id (:joe-test-id ids)
           :render-dom render-item-DOM
           :get-action-data (default-AD)
           :class "label"}]))
    ;; A node with a leaf,  properties, and no children
    (is (check
         (horizontal-label-hierarchy-node-DOM (first (:child-nodes node))
                                              {:width 0.75})
         [:component {:relative-id (:joe-id ids)
                      :width 0.75
                      :excluded-element-ids [(:joe-test-id ids)]
                      :render-dom render-item-DOM
                      :get-action-data (default-AD)}]))
    ;; A node with leaves, no properties, and no children
    (is (check
         (horizontal-label-hierarchy-node-DOM (second (:child-nodes node))
                                              {:width 0.75})
         [:div {:class
                "label wrapped-element virtual-wrapper merge-with-parent"}
          [:component {:template virtual-label-template
                       :width 0.75
                       :parallel-ids [(:jane-id ids)]
                       :get-action-data [(comp-AD)
                                         [(parallel-AD) (item-AD)]
                                         (virt-AD)]
                       :position :after
                       :relative-id [(:jane-id ids) :nested]
                       :class "label merge-with-parent"
                       :render-dom (virt-DOM)}]
          [:div {:class "indent-wrapper label"}
           [:component {:relative-id (:jane-id ids)
                        :render-dom render-item-DOM
                        :get-action-data (default-AD)
                        :width 0.75
                        :excluded-element-ids [(:jane-test-id ids)]}]]]))))

(deftest labels-and-elements-DOM-test
  (let [[s ids] (make-joe-and-jane-store)
        [store sally-id] (add-element s nil "Sally")
        joe (id->entity (:joe-id ids) store)
        joe-test (id->entity (:joe-test-id ids) store)
        joe-foo (id->entity (:joe-foo-id ids) store)
        jane (id->entity (:jane-id ids) store)
        jane-test (id->entity (:jane-test-id ids) store)
        sally (id->entity sally-id store)
        test-label-object (id->entity (:test-label-oid ids) store)
        foo-label-object (id->entity (:foo-label-oid ids) store)]
    ;; Test two non-labels.
    (is (check
         (labels-and-elements-DOM
          [joe jane] nil false false :vertical
          {:template 'anything :width 0.8})
         [:div {:class "wrapped-element label"}
          [:component {:width 0.8, :template label-template
                       :parallel-ids [(:joe-id ids) (:jane-id ids)]
                       :class "label"
                       :omit-universal-elements true
                       :relative-id (:joe-test-id ids)
                       :render-dom render-item-DOM
                       :get-action-data (default-AD)}]
          [:div {:class "indent-wrapper"}
           [:div {:class "vertical-stack"}
            [:div {:class "wrapped-element label"}
             [:component {:width 0.8, :template label-template
                          :parallel-ids [(:joe-id ids)]
                          :class "label"
                          :omit-universal-elements true
                          :relative-id (:joe-foo-id ids)
                          :render-dom render-item-DOM
                          :get-action-data (default-AD)}]
             [:div {:class "indent-wrapper"}
              ;; TODO: !!! Should these objects be in parens, like
              ;;       object elements should be?
              [:component {:template (as-set `(~'anything
                                               ~test-label-object
                                               ~foo-label-object))
                           :width 0.8
                           :excluded-element-ids (as-set [(:joe-test-id ids)
                                                          (:joe-foo-id ids)])
                           :relative-id (:joe-id ids)
                           :render-dom render-item-DOM
                           :get-action-data (default-AD)}]]]
            [:component {:template `(~'anything ~test-label-object)
                         :width 0.8
                         :excluded-element-ids [(:jane-test-id ids)]
                         :relative-id (:jane-id ids)
                         :render-dom render-item-DOM
                         :get-action-data (default-AD)}]]]]))
    ;; Test two non-labels, laid out horizontally
    (is (check
         (labels-and-elements-DOM
          [joe jane] nil false false :horizontal
          {:template 'anything :width 0.8})
         [:div {:class "wrapped-element label"}
          [:component {:width 0.8
                       :template label-template
                       :parallel-ids [(:joe-id ids) (:jane-id ids)]
                       :class "label"
                       :omit-universal-elements true
                       :relative-id (:joe-test-id ids)
                       :render-dom render-item-DOM
                       :get-action-data (default-AD)}]
          [:div {:class "indent-wrapper"}
           [:div {:class "horizontal-stack"}
            [:div {:class "wrapped-element label"}
             [:component {:width 0.8
                          :template label-template
                          :parallel-ids [(:joe-id ids)]
                          :class "label"
                          :omit-universal-elements true
                          :relative-id (:joe-foo-id ids)
                          :render-dom render-item-DOM
                          :get-action-data (default-AD)}]
             [:div {:class "indent-wrapper"}
              [:component {:template (as-set `(~'anything
                                               ~test-label-object
                                               ~foo-label-object))
                           :width 0.8
                           :excluded-element-ids (as-set [(:joe-test-id ids)
                                                          (:joe-foo-id ids)])
                           :relative-id (:joe-id ids)
                           :render-dom render-item-DOM
                           :get-action-data (default-AD)}]]]
            [:component {:template `(~'anything ~test-label-object)
                         :width 0.8
                         :excluded-element-ids [(:jane-test-id ids)]
                         :relative-id (:jane-id ids)
                         :render-dom render-item-DOM
                         :get-action-data (default-AD)}]]]]))
    ;; Test two labels.
    (is (check
         (labels-and-elements-DOM
                    [joe-test joe-foo] nil false false :vertical
          {:template ' anything :width 0.8})
         [:div {:class "vertical-stack"}
          [:component {:template label-template
                       :width 0.8
                       :class "label"
                       :relative-id (:joe-test-id ids)
                       :render-dom render-item-DOM
                       :get-action-data (default-AD)}]
          [:component {:template label-template
                       :width 0.8
                       :class "label"
                       :relative-id (:joe-foo-id ids)
                       :render-dom render-item-DOM
                       :get-action-data (default-AD)}]]))
    ;; Test a label and a non-label
    (is (check
         (labels-and-elements-DOM
          [sally joe-test] nil false false :vertical
          {:template ' anything :width 0.8})
         [:div {:class "wrapped-element label"}
          [:component {:template label-template
                       :width 0.8
                       :class "label"
                       :relative-id (:joe-test-id ids)
                       :render-dom render-item-DOM
                       :get-action-data (default-AD)}]
          [:div {:class "indent-wrapper"}
           [:component {:template 'anything
                          :width 0.8
                        :relative-id sally-id
                        :render-dom render-item-DOM
                        :get-action-data (default-AD)}]]]))
    ;; Test a non-label with must-show-label and elements-must-show-labels
    (is (check
         (labels-and-elements-DOM
          [sally] nil true true :vertical
          {:template 'anything :width 0.8})
         [:div {:class "wrapped-element label"}
          [:component {:template virtual-label-template
                       :width 0.8
                       :relative-id :virtual-label
                       :class "label"
                       :render-dom (virt-DOM)
                       :position :after
                       :get-action-data (virt-AD)}]
          [:div {:class "indent-wrapper"}
           [:div {:class
                  "horizontal-labels-element virtual-wrapper narrow"}
            [:component {:width 0.8
                         :template virtual-label-template
                         :relative-id [sally-id :virtual-label]
                         :parallel-ids [sally-id]
                         :get-action-data [(comp-AD)
                                           [(parallel-AD) (item-AD)]
                                           (virt-AD)]
                         :render-dom (virt-DOM)
                         :class "label"
                         :omit-universal-elements true}]
            [:component {:template 'anything
                         :width 0.8
                         :relative-id sally-id
                         :render-dom render-item-DOM
                         :get-action-data (default-AD)}]]]]))
    ;; Test including a virtual-dom
    (is (check
         (labels-and-elements-DOM
          [sally] [:div "virtual"] false false :vertical
          {:template 'anything :width 0.8})
         [:div {:class "vertical-stack"}
          [:component {:template 'anything
                       :width 0.8
                       :relative-id sally-id
                       :render-dom render-item-DOM
                       :get-action-data (default-AD)}]
          [:div "virtual"]]))))

(deftest virtual-entity-and-label-DOM-test
  (is (check (virtual-entity-and-label-DOM
              {:template `(~(make-object-list ["foo"]))
               :relative-id :bar
               :position :before}
              :horizontal)
             [:div {:class "horizontal-labels-element"}
              [:component
               {:relative-id :virtual-label
                :template (make-sequential-template
                           [`(~(make-object-list ["foo"]))
                            '("")
                            (make-placeholder-object-template)
                            (make-object-reference-template
                             (make-object-list [`(~link-type)]))])
                :position :after
                :get-action-data (virt-AD)
                :class "label"
                :render-dom (virt-DOM)}]
              [:component {:template `(~(make-object-list ["foo"]))
                           :relative-id :bar
                           :position :before
                           :render-dom (virt-DOM)
                           :get-action-data (virt-AD)}]])))

(deftest named-object-DOM-test
  (let [[s1 oid] (get-new-object-id (new-element-store))
        [store fred-id] (add-element s1 oid `("Fred" ~name-label))
        [two-name-store friedrich-id] (add-element store oid
                                                   `("Friedrich" ~name-label))
        [label-store _] (add-element store oid link-type)
        [class-store _] (add-element store oid object-type)]
    (is (check (named-object-DOM (id->entity oid store)
                                 (assoc basic-dom-specification
                                        :template (make-object-list [5])))
               [:component {:width 1.5
                            :template `(~(make-object-reference-template
                                          (make-object-list [5])))
                            :class "name named-object"
                            :relative-id fred-id
                            :omit-universal-elements true
                            :render-dom render-item-DOM
                            :get-action-data (default-AD)}]))
    (is (check (named-object-DOM (id->entity oid two-name-store)
                                 (assoc basic-dom-specification
                                        :template (make-object-list [5])))
               (as-set
                [:div {:class "named-object vertical-stack"}
                 [:component {:width 1.5
                              :template `(~(make-object-reference-template
                                            (make-object-list [5])))
                              :class "name"
                              :relative-id fred-id
                              :omit-universal-elements true
                              :render-dom render-item-DOM
                              :get-action-data (default-AD)}]
                 [:component {:width 1.5
                              :template `(~(make-object-reference-template
                                            (make-object-list [5])))
                              :class "name"
                              :relative-id friedrich-id
                              :omit-universal-elements true
                              :render-dom render-item-DOM
                              :get-action-data (default-AD)}]])))
    (is (check (named-object-DOM (id->entity oid label-store)
                                 (assoc basic-dom-specification
                                        :template (make-object-list [5])))
               [:component {:width 1.5
                            :template `(~(make-object-reference-template
                                          (make-object-list [5])))
                            :class "label named-object"
                            :relative-id fred-id
                            :omit-universal-elements true
                            :render-dom render-item-DOM
                            :get-action-data (default-AD)}]))
    (is (check (named-object-DOM (id->entity oid class-store)
                                 (assoc basic-dom-specification
                                        :template (make-object-list [5])))
               [:component {:width 1.5
                            :template `(~(make-object-reference-template
                                          (make-object-list [5])))
                            :class "class named-object"
                            :relative-id fred-id
                            :omit-universal-elements true
                            :render-dom render-item-DOM
                            :get-action-data (default-AD)}]))))

(deftest render-item-DOM-test-simple
  ;; Test a simple cell
  (let [[store fred-id] (add-element (new-element-store) nil "Fred")
        dom (run-renderer render-item-DOM
                          (assoc basic-dom-specification
                                 :relative-id fred-id)
                          store)]
    (is (check dom
               [:div {:class "editable content-text item"} "Fred"])))
  
  ;; Test a named object
  (let [[s1 oid] (get-new-object-id (new-element-store))
        [store fred-id] (add-element s1 oid `("Fred" ~name-label))
        dom (run-renderer render-item-DOM
                          (assoc basic-dom-specification
                                 :relative-id oid
                                 :template (make-object-list [5]))
                          store)]
    (is (check dom
               [:component {:width 1.5
                            :template `(~(make-object-reference-template
                                          (make-object-list [5])))
                            :class "name named-object"
                            :relative-id fred-id
                            :omit-universal-elements true
                            :render-dom render-item-DOM
                            :get-action-data (default-AD)}])))

  ;; Test an entity holding a named object
  (let [[s1 fred-id] (-> (new-element-store)
                         (add-universal-objects)
                         (get-new-object-id))
        [s2 fred-name-id] (add-element s1 fred-id `("Fred" (~name-label)))
        [store fred-holder-id] (add-element s2 nil `(~(id->entity fred-id nil)))
        dom (run-renderer render-item-DOM
                           (assoc basic-dom-specification
                                  :relative-id fred-holder-id
                                  :template `(~(make-object-list [5])))
                           store)
        ;; We expect a component, which we also run, to make sure it is right.
        [_ spec] dom
        inner-dom (run-renderer render-item-DOM spec store)]
    (is (check dom
               [:component {:width 1.5
                            :class "editable item"
                            :relative-id fred-id,
                            :template (make-object-list [5])
                            :render-dom render-item-DOM
                            :get-action-data (default-AD)}]))
    (is (check inner-dom
               [:component {:width 1.5
                            :class "editable item name named-object"
                            :template `(~(make-object-reference-template
                                          (make-object-list [5])))
                            :relative-id fred-name-id
                            :omit-universal-elements true
                            :render-dom render-item-DOM
                            :get-action-data (default-AD)}])))
  
  ;; Test a cell with a couple of labels, one excluded.
  (let [[store fred-id] (add-element (new-element-store) nil
                                     `("Fred"
                                       (1 :label (~o1 :order))
                                       (2 :label (~o2 :order))))
        fred (id->entity fred-id store)
        id1 (:item-id (first (matching-elements 1 fred)))
        id2 (:item-id (first (matching-elements 2 fred)))
        id-tag2 (:item-id (first (matching-elements
                                  :label (id->entity id2 store))))
        dom (run-renderer render-item-DOM
                          (assoc basic-dom-specification
                                 :relative-id fred-id
                                 :excluded-element-ids [id1])
                          store)]
    (is (check dom
               [:div {:class "wrapped-element label item"}
                [:component {:template label-template
                             :relative-id id2
                             :omit-universal-elements true
                             :render-dom render-item-DOM
                             :get-action-data (default-AD)
                             :excluded-element-ids [id-tag2]
                             :class "label"
                             :width 1.5}]
                [:div {:class "indent-wrapper"}
                 [:component {:template (as-set '("" (1 :label) (2 :label)))
                              :relative-id :content
                              :item-id fred-id
                              :render-dom render-content-only-DOM
                              :get-action-data (pass-AD)
                              :width 1.5}]]])))
  
  ;; Test must-show-label.
  (let [[store fred-id] (add-element (new-element-store) nil
                                     "Fred")
        dom (run-renderer render-item-DOM
                          (assoc basic-dom-specification
                                 :relative-id fred-id
                                 :must-show-label true)
                          store)]
    (is (check
         dom
         [:div
          {:class
           "horizontal-labels-element virtual-wrapper narrow item"}
          [:component {:template virtual-label-template
                       :position :after
                       :relative-id :virtual-label
                       :omit-universal-elements true
                       :class "label"
                       :render-dom (virt-DOM)
                       :get-action-data (virt-AD)
                       :width 1.5}]
          [:component {:template ""
                       :relative-id :content
                       :item-id fred-id
                       :render-dom render-content-only-DOM
                       :get-action-data (pass-AD)
                       :width 1.5}]]))))

(deftest item-DOM-test-one-column
  ;; Try a couple of elements with no labels
  (let [[store fred-id] (add-element (new-element-store) nil
                                    `("Fred"
                                      (2 (~o2 :order))
                                      (1 (~o1 :order))))
        fred (id->entity fred-id store)
        id1 (:item-id (first (matching-elements 1 fred)))
        id2 (:item-id (first (matching-elements 2 fred)))
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id fred-id
                                    :width 0.9)
                             store)]
    (is (check dom
               [:div {:class "with-elements item"}
                [:component {:template ""
                             :width 0.9
                             :relative-id :content
                             :item-id fred-id
                             :render-dom render-content-only-DOM
                             :get-action-data (pass-AD)}]
                [:div {:class "vertical-stack"}
                 [:div {:class
                        "horizontal-labels-element virtual-wrapper narrow"}
                  [:component
                   {:width 0.9
                    :template virtual-label-template
                    :relative-id [id1 :virtual-label]
                    :render-dom (virt-DOM)
                    :parallel-ids [id1]
                    :get-action-data [(comp-AD)
                                      [(parallel-AD) (item-AD)]
                                      (virt-AD)]
                    :class "label"
                    :omit-universal-elements true}]
                  [:component {:width 0.9
                               :template 'anything
                               :relative-id id1
                               :render-dom render-item-DOM
                               :get-action-data (default-AD)}]]
                 [:div {:class
                        "horizontal-labels-element virtual-wrapper narrow"}
                  [:component
                   {:width 0.9
                    :template virtual-label-template
                    :relative-id [id2 :virtual-label]
                    :render-dom (virt-DOM)
                    :parallel-ids [id2]
                    :get-action-data [(comp-AD)
                                      [(parallel-AD) (item-AD)]
                                      (virt-AD)]
                    :class "label"
                    :omit-universal-elements true}]
                  [:component {:width 0.9
                               :template 'anything
                               :relative-id id2
                               :render-dom render-item-DOM
                               :get-action-data (default-AD)}]]]])))
  ;; Test an item with two elements, each with one distinct label.
  (let [[store fred-id] (add-element (new-element-store) nil
                                    `("Fred"
                                      (2 ("two" :label) (~o2 :order))
                                      (1 ("one" :label) (~o1 :order))))
        fred (id->entity fred-id store)
        item1 (first (matching-elements 1 fred))
        label1 (first (matching-elements "one" item1))
        tag1 (first (matching-elements :label label1))
        id1 (:item-id item1)
        id-label1 (:item-id label1)
        id-tag1 (:item-id tag1)
        item2 (first (matching-elements 2 fred))
        label2 (first (matching-elements "two" item2))
        tag2 (first (matching-elements :label label2))
        id2 (:item-id item2)
        id-label2 (:item-id label2)
        id-tag2 (:item-id tag2)
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id fred-id
                                    :width 0.9)
                             store)]
    (is (check dom
               [:div {:class "with-elements item"}
                [:component {:template ""
                             :width 0.9
                             :relative-id :content
                             :item-id fred-id
                             :render-dom render-content-only-DOM
                             :get-action-data (pass-AD)}]
                [:div {:class "vertical-stack"}
                 [:div {:class "wrapped-element label"}
                  [:component {:width 0.9
                               :template label-template
                               :parallel-ids [id1]
                               :class "label"
                               :omit-universal-elements true
                               :excluded-element-ids [id-tag1]
                               :relative-id id-label1
                               :render-dom render-item-DOM
                               :get-action-data (default-AD)}]
                  [:div {:class "indent-wrapper"}
                   [:component {:width 0.9
                                :template '(anything ("one" :label))
                                :excluded-element-ids [id-label1]
                                :relative-id id1
                                :render-dom render-item-DOM
                                :get-action-data (default-AD)}]]]
                 [:div {:class "wrapped-element label"}
                  [:component {:width 0.9
                               :template label-template
                               :parallel-ids [id2]
                               :class "label"
                               :omit-universal-elements true
                               :excluded-element-ids [id-tag2]
                               :relative-id id-label2
                               :render-dom render-item-DOM
                               :get-action-data (default-AD)}]
                  [:div {:class "indent-wrapper"}
                   [:component {:width 0.9
                                :template '(anything ("two" :label))
                                :excluded-element-ids [id-label2]
                                :relative-id id2
                                :render-dom render-item-DOM
                                :get-action-data (default-AD)}]]]]])))
  ;; Test an item with four elements, with label sharing among them.
  (let [[store fred-id] (add-element (new-element-store) nil
                                    `("Fred"
                                      (0 ("zero" :label)
                                         (~o1 :order))
                                      (2 ("two" :label (~o1 :order))
                                         ("both" :label (~o2 :order))
                                         (~o3 :order))
                                      (1 ("one" :label (~o1 :order))
                                         ("both" :label (~o2 :order))
                                         (~o2 :order))
                                      (3 (~o4 :order))))
        fred (id->entity fred-id store)
        item0 (first (matching-elements 0 fred))
        label0 (first (matching-elements "zero" item0))
        tag0 (first (matching-elements :label label0))
        id0 (:item-id item0)
        id-label0 (:item-id label0)
        id-tag0 (:item-id tag0)
        item1 (first (matching-elements 1 fred))
        label1one (first (matching-elements "one" item1))
        tag1one (first (matching-elements :label label1one))
        label1both (first (matching-elements "both" item1))
        tag1both (first (matching-elements :label label1both))
        id1 (:item-id item1)
        id-label1one (:item-id label1one)
        id-tag1one (:item-id tag1one)
        id-label1both (:item-id label1both)
        id-tag1both (:item-id tag1both)
        item2 (first (matching-elements 2 fred))
        label2two (first (matching-elements "two" item2))
        tag2two (first (matching-elements :label label2two))
        label2both (first (matching-elements "both" item2))
        tag2both (first (matching-elements :label label2both))
        id2 (:item-id item2)
        id-label2two (:item-id label2two)
        id-tag2two (:item-id tag2two)
        id-label2both (:item-id label2both)
        id-tag2both (:item-id tag2both)
        item3 (first (matching-elements 3 fred))
        id3 (:item-id item3)
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id fred-id
                                    :width 0.9)
                             store)]
    (is (check dom
               [:div {:class "with-elements item"}
                [:component {:template ""
                             :width 0.9
                             :relative-id :content
                             :item-id fred-id
                             :render-dom render-content-only-DOM
                             :get-action-data (pass-AD)}]
                [:div {:class "vertical-stack"}
                 [:div {:class "wrapped-element label"}
                  [:component {:width 0.9
                               :template label-template
                               :parallel-ids [id0]
                               :class "label"
                               :omit-universal-elements true
                               :excluded-element-ids [id-tag0]
                               :relative-id id-label0
                               :render-dom render-item-DOM
                               :get-action-data (default-AD)}]
                  [:div {:class "indent-wrapper"}
                   [:component {:width 0.9
                                :template '(anything ("zero" :label))
                                :excluded-element-ids [id-label0]
                                :relative-id id0
                                :render-dom render-item-DOM
                                :get-action-data (default-AD)}]]]
                 [:div {:class "wrapped-element label"}
                  [:component {:width 0.9
                               :template label-template
                               :parallel-ids [id1 id2]
                               :class "label"
                               :omit-universal-elements true
                               :excluded-element-ids [id-tag1both]
                               :relative-id id-label1both
                               :render-dom render-item-DOM
                               :get-action-data (default-AD)}]
                  [:div {:class "indent-wrapper"}
                   [:div {:class "vertical-stack"}
                    [:div {:class "wrapped-element label"}
                     [:component {:width 0.9
                                  :template label-template
                                  :parallel-ids[id1]
                                  :class "label"
                                  :omit-universal-elements true
                                  :excluded-element-ids [id-tag1one]
                                  :relative-id id-label1one
                                  :render-dom render-item-DOM
                                  :get-action-data (default-AD)}]
                     [:div {:class "indent-wrapper"}
                      [:component {:width 0.9
                                   :template (as-set '(anything ("both" :label)
                                                               ("one" :label)))
                                   :excluded-element-ids (as-set [id-label1both
                                                                  id-label1one])
                                   :relative-id id1
                                   :render-dom render-item-DOM
                                   :get-action-data (default-AD)}]]]
                    [:div {:class "wrapped-element label"}
                     [:component {:width 0.9
                                  :template label-template
                                  :parallel-ids [id2]
                                  :class "label"
                                  :omit-universal-elements true
                                  :excluded-element-ids [id-tag2two]
                                  :relative-id id-label2two
                                  :render-dom render-item-DOM
                                  :get-action-data (default-AD)}]
                     [:div {:class "indent-wrapper"}
                      [:component {:width 0.9
                                   :template (as-set '(anything ("both" :label)
                                                                ("two" :label)))
                                   :excluded-element-ids (as-set [id-label2both
                                                                  id-label2two])
                                   :relative-id id2
                                   :render-dom render-item-DOM
                                   :get-action-data (default-AD)}]]]]]]
                [:div {:class (str "horizontal-labels-element"
                                   " virtual-wrapper narrow")}
                 [:component {:width 0.9
                              :template virtual-label-template
                              :parallel-ids [id3]
                              :get-action-data [(comp-AD)
                                                [(parallel-AD) (item-AD)]
                                                (virt-AD)]
                              :relative-id [id3 :virtual-label]
                              :render-dom (virt-DOM)
                              :class "label"
                              :omit-universal-elements true}]
                 [:component {:width 0.9
                              :template 'anything
                              :relative-id id3
                              :render-dom render-item-DOM
                              :get-action-data (default-AD)}]]]]))))

(deftest item-DOM-test-two-column
  ;; Try three elements with no labels, but one of them marked as excluded.
  (let [[store fred-id] (add-element (new-element-store) nil
                                    `("Fred"
                                      (3 (~o3 :order))
                                      (2 (~o2 :order))
                                      (1 (~o1 :order))))
        fred (id->entity fred-id store)
        id1 (:item-id (first (matching-elements 1 fred)))
        id2 (:item-id (first (matching-elements 2 fred)))
        id3 (:item-id (first (matching-elements 3 fred)))
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id fred-id
                                    :width 1.5
                                    :must-show-label :wide
                                    :excluded-element-ids [id3])
                             store)]
    ;; Test an item with two elements, neither with a label.
    (is (check
         dom
         [:div
          {:class "horizontal-labels-element virtual-wrapper narrow item"}
          [:component
           {:width 1.5
            :template virtual-label-template
            :relative-id :virtual-label
            :omit-universal-elements true
            :position :after
            :class "label"
            :render-dom (virt-DOM)
            :get-action-data (virt-AD)}]
          [:div {:class "with-elements"}
           [:component {:template ""
                        :width 1.5
                        :relative-id :content
                        :item-id fred-id
                        :render-dom render-content-only-DOM
                        :get-action-data (pass-AD)}]
           [:div {:class "vertical-stack"}
            [:div {:class "horizontal-labels-element label wide"}
             [:div {:class "label horizontal-header top-border bottom-border"}
              [:component {:width 0.375
                           :template virtual-label-template
                           :relative-id [id1 :virtual-label]
                           :parallel-ids [id1]
                           :get-action-data [(comp-AD)
                                             [(parallel-AD) (item-AD)]
                                             (virt-AD)]
                           :render-dom (virt-DOM)
                           :class "label"
                           :omit-universal-elements true}]]
             [:component {:width 1.03125
                          :template 'anything
                          :relative-id id1
                          :render-dom render-item-DOM
                          :get-action-data (default-AD)}]]
            [:div {:class "horizontal-labels-element label wide"}
             [:div {:class "label horizontal-header top-border bottom-border"}
              [:component {:width 0.375
                           :template virtual-label-template
                           :relative-id [id2 :virtual-label]
                           :parallel-ids [id2]
                           :get-action-data [(comp-AD)
                                             [(parallel-AD) (item-AD)]
                                             (virt-AD)]
                           :render-dom (virt-DOM)
                           :class "label"
                           :omit-universal-elements true}]]
             [:component {:width 1.03125
                          :template 'anything
                          :relative-id id2
                          :render-dom render-item-DOM
                          :get-action-data (default-AD)}]]]]])))
  ;; Test an item with two elements, each with one distinct label.
  (let [[store fred-id] (add-element (new-element-store) nil
                                    `("Fred"
                                      (2 ("two" :label) (~o2 :order))
                                      (1 ("one" :label) (~o1 :order))))
        fred (id->entity fred-id store)
        item1 (first (matching-elements 1 fred))
        label1 (first (matching-elements "one" item1))
        tag1 (first (matching-elements :label label1))
        id1 (:item-id item1)
        id-label1 (:item-id label1)
        id-tag1 (:item-id tag1)
        item2 (first (matching-elements 2 fred))
        label2 (first (matching-elements "two" item2))
        tag2 (first (matching-elements :label label2))
        id2 (:item-id item2)
        id-label2 (:item-id label2)
        id-tag2 (:item-id tag2)
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id fred-id
                                    :width 1.5)
                             store)]
    (is (check
         dom
         [:div {:class "with-elements item"}
          [:component {:template ""
                       :width 1.5
                       :relative-id :content
                       :item-id fred-id
                       :render-dom render-content-only-DOM
                       :get-action-data (pass-AD)}]
          [:div {:class "vertical-stack"}
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class (str "label horizontal-header"
                               " top-border bottom-border")}
             [:component {:width 0.375, :template label-template
                          :parallel-ids [id1]
                          :class "label"
                          :omit-universal-elements true
                          :excluded-element-ids [id-tag1]
                          :relative-id id-label1
                          :render-dom render-item-DOM
                          :get-action-data (default-AD)}]]
            [:component {:width 1.03125
                         :template '(anything ("one" :label))
                         :excluded-element-ids [id-label1]
                         :relative-id id1
                         :render-dom render-item-DOM
                         :get-action-data (default-AD)}]]
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class (str "label horizontal-header"
                               " top-border bottom-border")}
             [:component {:width 0.375, :template label-template
                          :parallel-ids [id2]
                          :class "label"
                          :omit-universal-elements true
                          :excluded-element-ids [id-tag2]
                          :relative-id id-label2
                          :render-dom render-item-DOM
                          :get-action-data (default-AD)}]]
            [:component {:width 1.03125
                         :template '(anything ("two" :label))
                         :excluded-element-ids [id-label2]
                         :relative-id id2
                         :render-dom render-item-DOM
                         :get-action-data (default-AD)}]]]])))
  ;; Test an item with four elements, with label sharing among them.
  (let [[store fred-id] (add-element (new-element-store) nil
                                    `("Fred"
                                      (0 ("zero" :label)
                                         (~o1 :order))
                                      (2 ("two" :label (~o1 :order))
                                         ("both" :label (~o2 :order))
                                         (~o3 :order))
                                      (1 ("one" :label (~o1 :order))
                                         ("both" :label (~o2 :order))
                                         (~o2 :order))
                                      (3 (~o4 :order))))
        fred (id->entity fred-id store)
        item0 (first (matching-elements 0 fred))
        label0 (first (matching-elements "zero" item0))
        tag0 (first (matching-elements :label label0))
        id0 (:item-id item0)
        id-label0 (:item-id label0)
        id-tag0 (:item-id tag0)
        item1 (first (matching-elements 1 fred))
        label1one (first (matching-elements "one" item1))
        tag1one (first (matching-elements :label label1one))
        label1both (first (matching-elements "both" item1))
        tag1both (first (matching-elements :label label1both))
        id1 (:item-id item1)
        id-label1one (:item-id label1one)
        id-tag1one (:item-id tag1one)
        id-label1both (:item-id label1both)
        id-tag1both (:item-id tag1both)
        item2 (first (matching-elements 2 fred))
        label2two (first (matching-elements "two" item2))
        tag2two (first (matching-elements :label label2two))
        label2both (first (matching-elements "both" item2))
        tag2both (first (matching-elements :label label2both))
        id2 (:item-id item2)
        id-label2two (:item-id label2two)
        id-tag2two (:item-id tag2two)
        id-label2both (:item-id label2both)
        id-tag2both (:item-id tag2both)
        item3 (first (matching-elements 3 fred))
        id3 (:item-id item3)
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id fred-id
                                    :width 1.5)
                             store)]
    (is (check
         dom
         [:div {:class "with-elements item"}
          [:component {:template ""
                       :width 1.5
                       :relative-id :content
                       :item-id fred-id
                       :render-dom render-content-only-DOM
                       :get-action-data (pass-AD)}]
          [:div {:class "vertical-stack"}
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class "label horizontal-header top-border bottom-border"}
             [:component {:width 0.375
                          :template label-template
                          :parallel-ids [id0]
                          :class "label"
                          :omit-universal-elements true
                          :excluded-element-ids [id-tag0]
                          :relative-id id-label0
                          :render-dom render-item-DOM
                          :get-action-data (default-AD)}]]
            [:component {:width 1.03125
                         :template '(anything ("zero" :label))
                         :excluded-element-ids [id-label0]
                         :relative-id id0
                         :render-dom render-item-DOM
                         :get-action-data (default-AD)}]]
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class "label horizontal-header top-border"}
             [:component {:width 0.375
                          :template label-template
                          :parallel-ids [id1 id2]
                          :class "label"
                          :omit-universal-elements true
                          :excluded-element-ids [id-tag1both]
                          :relative-id id-label1both
                          :render-dom render-item-DOM
                          :get-action-data (default-AD)}]]
            [:component {:width 1.03125
                         :template '(anything ("both" :label))
                         :relative-id :virtual
                         :position :after
                         :sibling true
                         :render-dom (virt-DOM)
                         :get-action-data (virt-AD)}]]
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class "label horizontal-header indent"}
             [:div {:class "label horizontal-header top-border bottom-border"}
              [:component {:width 0.375
                           :template label-template
                           :parallel-ids [id1]
                           :class "label"
                           :omit-universal-elements true
                           :excluded-element-ids [id-tag1one]
                           :relative-id id-label1one
                           :render-dom render-item-DOM
                           :get-action-data (default-AD)}]]]
            [:component {:width 1.03125
                         :template (as-set '(anything ("both" :label)
                                                      ("one" :label)))
                         :excluded-element-ids (as-set [id-label1both
                                                        id-label1one])
                         :relative-id id1
                         :render-dom render-item-DOM
                         :get-action-data (default-AD)}]]
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class "label horizontal-header indent bottom-border"}
             [:div {:class "label horizontal-header top-border bottom-border"}
              [:component {:width 0.375
                           :template label-template
                           :parallel-ids [id2]
                           :class "label"
                           :omit-universal-elements true
                           :excluded-element-ids [id-tag2two]
                           :relative-id id-label2two
                           :render-dom render-item-DOM
                           :get-action-data (default-AD)}]]]
            [:div {:class "horizontal-value-last"}
             [:component {:width 1.03125
                          :template '(anything ("both" :label) ("two" :label))
                          :excluded-element-ids (as-set [id-label2both
                                                         id-label2two])
                          :relative-id id2
                          :render-dom render-item-DOM
                          :get-action-data (default-AD)}]]]
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class "label horizontal-header top-border bottom-border"}
             [:component {:width 0.375
                          :template virtual-label-template
                          ;; TODO: This breaks the relative id convention.
                          :relative-id [id3 :virtual-label]
                          :parallel-ids [id3]
                          :render-dom (virt-DOM)
                          :get-action-data [(comp-AD)
                                            [(parallel-AD) (item-AD)]
                                            (virt-AD)]
                          :class "label"
                          :omit-universal-elements true}]]
            [:component {:width 1.03125
                         :template 'anything
                         :relative-id id3
                         :render-dom render-item-DOM
                         :get-action-data (default-AD)}]]]]))))

(deftest render-virtual-DOM-test
  (is (check (render-virtual-DOM {:class "foo"} new-element-store)
             [:div {:class "foo editable virtual"}])))

(comment

;;; Test an item that needs to be wrapped in labels.
  (deftest item-DOM-R-test-labels
    ;; First, test when there is a label.
    (let [element-as-list `(39
                            ("age" :tag (~o1 :order))
                            ("Ke"
                             ("according-to" :tag (~o1 :order))
                             (~o1 :order)))
          inherited  (assoc base-inherited :width 1.0)
          [dom element] (let-mutated [element element-as-list]
                          (expr-let [dom (item-DOM-R element [] inherited)]
                            [dom element]))]
      (let [element-key [:root (:item-id element)]
            age (first (current-value (matching-elements "age" element)))
            age-tag (first (current-value (matching-elements :tag age)))
            age-key [:root (:item-id age)]]
        (is (check dom
                   [:div {:class "wrapped-element tag"}
                    [:div
                     {:key (conj age-key :content)
                      :class "content-text editable item tag"
                      :target {:template '(anything :tag)
                               :referent (item-referent age)}
                      :expand {:referent (item-referent element)}}
                     "age"]
                    [:div {:class "indent-wrapper"}
                     [:div {:class "item with-elements"}
                      [:div {:class "content-text editable"
                             :target {:referent (item-referent element)
                                      :template ""}
                             :key (conj element-key :content)}
                       "39"]
                      [:div {:class "horizontal-tags-element tag wide"}
                       (any)
                       [:div (any) "Ke"]]]]]))))
    ;; Then test when there is no label, but labels must be shown.
    (let [item-as-list `(39 ("Ke"
                             ("according-to" :tag (~o1 :order))
                             (~o1 :order)))
          inherited  (assoc base-inherited :width 1.0)
          [dom item] (let-mutated [item item-as-list]
                       (expr-let [dom (item-DOM-R
                                       item [] inherited :must-show-label true)]
                         [dom item]))]
      (let [item-key [:root (:item-id item)]]
        (is (check dom
                   [:div {:class
                          "horizontal-tags-element tag virtual-wrapper narrow"}
                    [:div {:class "editable tag"
                           :key (conj item-key :tags :virtual)
                           :expand {:referent (item-referent item)}
                           :target {:referent (virtual-referent
                                               '(anything :tag)
                                               (item-referent item)
                                               nil
                                               :position :after)
                                    :select-pattern (conj item-key
                                                          :tags [:pattern])}}]
                    [:div {:class "item with-elements"}
                     [:div {:class "content-text editable"
                            :target {:referent (item-referent item)
                                     :template ""}
                            :key (conj item-key :content)}
                      "39"]
                     [:div {:class "horizontal-tags-element tag wide"}
                      (any)
                      [:div (any) "Ke"]]]])))))

;;; TODO: Add a test where some of the arguments are true.
  (deftest labels-and-elements-DOM-R-test
    (let [element-as-list `(39
                            ("age" :tag (~o1 :order))
                            ("Ke"
                             ("according-to" :tag (~o1 :order))
                             (~o1 :order)))

          element (let-mutated [element element-as-list]
                    element)
          element-key [:root (:item-id element)]
          element-referent (item-referent element)
          age (first (current-value (matching-elements "age" element)))
          age-tag (first (current-value (matching-elements :tag age)))
          age-key (conj element-key (:item-id age))
          qualifier (first (current-value (matching-elements "Ke" element)))
          according-to (first (current-value (matching-elements
                                              "according-to" qualifier)))
          according-to-id (:item-id according-to)
          inherited  (assoc base-inherited
                            :width 1.0
                            :key-prefix element-key
                            :subject-referent element-referent
                            :attributes [[#{:content} {:class "placeholder"}]])
          dom (current-value
               (labels-and-elements-DOM-R
                [age qualifier] false false false :vertical inherited))]
      (is (check dom
                 [:div {:class "wrapped-element tag"}
                  [:div {:key (conj age-key :content)
                         :class "content-text editable item tag"
                         :target {:template '(anything :tag)
                                  :referent (item-referent age)}}
                   "age"]
                  [:div {:class "indent-wrapper"}
                   [:div {:class "item elements-wrapper"}
                    [:div {:class "horizontal-tags-element tag wide"}
                     [:div {:class
                            "tag horizontal-header top-border bottom-border"}
                      [:div (any) "according-to"]]
                     [:div (any) "Ke"]]]]]))))


  (deftest item-content-and-elements-DOM-R-test
    (let [element-as-list `(39
                            ("age" :tag (~o1 :order))
                            ("Ke"
                             ("according-to" :tag (~o1 :order))
                             (~o1 :order)))

          element (let-mutated [element element-as-list]
                    element)
          element-key [:root (:item-id element)]
          element-referent (item-referent element)
          age (first (current-value (matching-elements "age" element)))
          age-tag (first (current-value (matching-elements :tag age)))
          age-key (conj element-key (:item-id age))
          qualifier (first (current-value (matching-elements "Ke" element)))
          according-to (first (current-value (matching-elements
                                              "according-to" qualifier)))
          according-to-id (:item-id according-to)
          inherited  (assoc base-inherited
                            :width 1.0
                            :key-prefix element-key
                            :subject-referent element-referent
                            :attributes [[#{:content} {:class "placeholder"}]])
          dom (current-value
               (item-content-and-elements-DOM-R
                age 39 [age qualifier] false inherited))]
      (is (check dom
                 [:div {:class "wrapped-element tag"}
                  (any)
                  [:div {:class "indent-wrapper"}
                   [:div {:class "item with-elements"}
                    [:div {:class "placeholder content-text editable"
                           :target {:referent element-referent
                                    :template ""}
                           :key (conj element-key :content)}
                     "39"]
                    [:div {:class "horizontal-tags-element tag wide"}
                     [:div {:class
                            "tag horizontal-header top-border bottom-border"}
                      [:div (any) "according-to"]]                   
                     [:div (any) "Ke"]]]]]))))
  )

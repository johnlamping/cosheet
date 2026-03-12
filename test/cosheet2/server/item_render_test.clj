(ns cosheet2.server.item-render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet2
             [orderable :as orderable]
             [query :refer [matching-elements]]
             [debug :refer [envs-to-list simplify-for-print]]
             [entity :refer [id->element id->object
                             name-label link-type object-type
                             make-object-list uniquely-identified-object?
                             to-list]]
             entity-impl
             [store :refer [new-element-store get-new-object-id
                            ->ItemId]]
             store-impl
             mutable-store-impl
             [calculator :refer [new-calculator-data computation-value]]
             [task-queue :refer [new-priority-task-queue]]
             [store-utils :refer [add-element add-object
                                  add-universal-objects
                                  add-label-object]]
             [test-utils :refer [check any as-set]])
            (cosheet2.server
             [model-utils :refer [semantic-label-elements]]
             [hierarchy :refer [item-maps-by-elements
                                hierarchy-by-canonical-info]]
             [render :refer [basic-dom-specification]]
             [render-utils :refer [make-sequential-template
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

(defn make-joe-and-jane-store
  "Make a store with labels test-object and foo-object and with
    [:object 'Joe' test-object foo-object]
    [:object 'Jane' test-object]
  Return the store and a map from names of objects and elements to their ids."
  []
  (let [s (add-universal-objects (new-element-store))
        [s1 test-label-oid] (add-label-object s "test")
        test-label-object (id->object test-label-oid nil)
        [s2 foo-label-oid] (add-label-object s1 "foo")
        foo-label-object (id->object foo-label-oid nil)
        [s4 joe-id] (add-element s2 nil "Joe")
        [s5 joe-test-id] (add-element s4 joe-id `(~test-label-object))
        [s7 joe-foo-id] (add-element s5 joe-id `(~foo-label-object))
        [s9 jane-id] (add-element s7 nil "Jane")
        [store jane-test-id] (add-element s9 jane-id `(~test-label-object))]
    [store {:test-label-oid test-label-oid
            :foo-label-oid foo-label-oid
            :joe-id joe-id
            :joe-test-id joe-test-id
            :joe-foo-id joe-foo-id
            :jane-id jane-id
            :jane-test-id jane-test-id}]))

(defn make-fred-one-two-store
   "Make a store with labels one-object and two-object and with
      [:object 'Fred' one-object two-object]
    and with the elements in that order.
    Return the store and a map from names of objects and elements to their ids."
  []
  (let [s (add-universal-objects (new-element-store))
        [s1 one-oid] (add-label-object s "one")
        [s2 two-oid] (add-label-object s1 "two")
        [s3 fred-id] (add-element s2 nil "Fred")
        [s4 label-one-id] (add-element s3 fred-id `(~(id->object one-oid nil)
                                                   (~o1 :order)))
        [store label-two-id] (add-element s4 fred-id `(~(id->object two-oid nil)
                                                      (~o2 :order)))]
    [store {:fred-id fred-id
            :one-oid one-oid
            :two-oid two-oid
            :label-one-id label-one-id
            :label-two-id label-two-id}]))

(defn make-fred-1-one-2-two-store
   "Make a store with label objects one-object and two-object and with
      [:object 'Fred' (1 one-object) (2 two-object)]
    and with the elements in that order.
    Return the store and a map from names of objects and elements to their ids."
  []
  (let [s (add-universal-objects (new-element-store))
        [s1 one-oid] (add-label-object s "one")
        [s2 two-oid] (add-label-object s1 "two")
        [s3 fred-id] (add-element s2 nil "Fred")
        [s4 element-1-id] (add-element s3 fred-id `(1 (~o1 :order)))
        [s5 label-one-id] (add-element s4 element-1-id
                                       `(~(id->object one-oid nil)))
        [s6 element-2-id] (add-element s5 fred-id `(2 (~o2 :order)))
        [store label-two-id] (add-element s6 element-2-id
                                          `(~(id->object two-oid nil)))]
    [store {:fred-id fred-id
            :one-oid one-oid
            :two-oid two-oid
            :element-1-id element-1-id
            :element-2-id element-2-id
            :label-one-id label-one-id
            :label-two-id label-two-id}]))

(defn make-fred-4-elements-store
  "Make a store with label objects one-object two-object, zero-object
   and both-object and with
     [:object 'Fred' (0 zero-object)
                     (1 one-object both-object)
                     (2 two-object both-object)
                     3)]
    and with the elements in that order.
    Return the store and a map from names of objects and elements to their ids."
  []
  (let [s (add-universal-objects (new-element-store))
        [s1 one-oid] (add-label-object s "one")
        [s2 two-oid] (add-label-object s1 "two")
        [s3 zero-oid] (add-label-object s2 "zero")
        [s4 both-oid] (add-label-object s3 "both")
        [s5 fred-id] (add-element s4 nil "Fred")
        [s6 element-0-id] (add-element s5 fred-id `(0 (~o1 :order)))
        [s7 label-zero-id] (add-element s6 element-0-id
                                        `(~(id->object zero-oid s6)))
        [s8 element-2-id] (add-element s7 fred-id `(2 (~o3 :order)))
        [s9 label-two-id] (add-element s8 element-2-id
                                       `(~(id->object two-oid s8)
                                         (~o1 :order)))
        [s10 label-2-both-id] (add-element s9 element-2-id
                                           `(~(id->object both-oid s9)
                                             (~o2 :order)))
        [s11 element-1-id] (add-element s10 fred-id `(1 (~o2 :order)))
        [s12 label-one-id] (add-element s11 element-1-id
                                       `(~(id->object one-oid s11)
                                         (~o1 :order)))
        [s13 label-1-both-id] (add-element s12 element-1-id
                                           `(~(id->object both-oid s12)
                                             (~o2 :order)))
        [store element-3-id] (add-element s13 fred-id `(3 (~o4 :order)))]
    [store {:fred-id fred-id
            :one-oid one-oid
            :two-oid two-oid
            :zero-oid zero-oid
            :both-oid both-oid
            :element-0-id element-0-id
            :label-zero-id label-zero-id
            :element-1-id element-1-id
            :label-one-id label-one-id
            :label-1-both-id label-1-both-id
            :element-2-id element-2-id
            :label-two-id label-two-id
            :label-2-both-id label-2-both-id
            :element-3-id element-3-id}]))

(deftest virtual-DOM-test
  (is (check (virtual-DOM-component {:template "foo"
                                     :relative-id :bar
                                     :position :before})
             [:component {:template "foo"
                          :position :before
                          :relative-id :bar
                          :render-dom (virt-DOM)
                          :get-action-data (virt-AD)}])))

(deftest horizontal-label-hierarchy-node-DOM-test
  (let [[store ids] (make-joe-and-jane-store)
        ordered-entities [(id->element (:joe-id ids) store)
                          (id->element (:jane-id ids) store)]
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
                       :is-object-name true
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
        joe (id->element (:joe-id ids) store)
        joe-test (id->element (:joe-test-id ids) store)
        joe-foo (id->element (:joe-foo-id ids) store)
        jane (id->element (:jane-id ids) store)
        jane-test (id->element (:jane-test-id ids) store)
        sally (id->element sally-id store)
        test-label-object (id->object (:test-label-oid ids) store)
        foo-label-object (id->object (:foo-label-oid ids) store)]
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
              [:component {:template (as-set `(~'anything
                                               (~test-label-object)
                                               (~foo-label-object)))
                           :width 0.8
                           :excluded-element-ids (as-set [(:joe-test-id ids)
                                                          (:joe-foo-id ids)])
                           :relative-id (:joe-id ids)
                           :render-dom render-item-DOM
                           :get-action-data (default-AD)}]]]
            [:component {:template `(~'anything (~test-label-object))
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
                                               (~test-label-object)
                                               (~foo-label-object)))
                           :width 0.8
                           :excluded-element-ids (as-set [(:joe-test-id ids)
                                                          (:joe-foo-id ids)])
                           :relative-id (:joe-id ids)
                           :render-dom render-item-DOM
                           :get-action-data (default-AD)}]]]
            [:component {:template `(~'anything (~test-label-object))
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
                       :is-object-name true
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
                         :is-object-name true
                         :relative-id [sally-id :virtual-label]
                         :parallel-ids [sally-id]
                         :get-action-data [(comp-AD)
                                           [(parallel-AD) (item-AD)]
                                           (virt-AD)]
                         :render-dom (virt-DOM)
                         :position :after
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
                            (make-object-list [`(~link-type)
                                               `("" (~name-label))])])
                :is-object-name true
                :position :after
                :get-action-data (virt-AD)
                :class "label"
                :render-dom (virt-DOM)}]
              [:component {:template `(~(make-object-list ["foo"]))
                           :relative-id :bar
                           :position :before
                           :render-dom (virt-DOM)
                           :get-action-data (virt-AD)}]])))

(deftest render-content-object-by-name-DOM-test
  (let [[s1 oid] (get-new-object-id (new-element-store))
        [store fred-id] (add-element s1 oid `("Fred" (~name-label)))
        [two-name-store friedrich-id] (add-element store oid
                                                   `("Friedrich" (~name-label)))
        [label-store _] (add-element store oid `(link-type))
        [class-store _] (add-element store oid `(object-type))
        template (make-object-list [`(~name-label)])
        spec (assoc basic-dom-specification
                    :relative-id :content
                    :auxiliary-item-id oid
                    :template template)]
    (is (check (render-content-object-by-name-DOM spec store)
               [:component {:width 1.5
                            :template template
                            :is-object-name true
                            :class "name named-object"
                            :relative-id fred-id
                            :omit-universal-elements true
                            :render-dom render-item-DOM
                            :get-action-data (pass-AD)}]))
    (is (check (render-content-object-by-name-DOM
                spec two-name-store)
               (as-set
                [:div {:class "named-object vertical-stack"}
                 [:component {:width 1.5
                              :template template
                              :is-object-name true
                              :class "name"
                              :relative-id fred-id
                              :omit-universal-elements true
                              :render-dom render-item-DOM
                              :get-action-data (pass-AD)}]
                 [:component {:width 1.5
                              :template template
                              :is-object-name true
                              :class "name"
                              :relative-id friedrich-id
                              :omit-universal-elements true
                              :render-dom render-item-DOM
                              :get-action-data (pass-AD)}]])))
    (is (check (render-content-object-by-name-DOM spec label-store)
               [:component {:width 1.5
                            :template template
                            :is-object-name true
                            :class "name named-object"
                            :relative-id fred-id
                            :omit-universal-elements true
                            :render-dom render-item-DOM
                            :get-action-data (pass-AD)}]))
    (is (check (render-content-object-by-name-DOM spec class-store)
               [:component {:width 1.5
                            :template template
                            :is-object-name true
                            :class "name named-object"
                            :relative-id fred-id
                            :omit-universal-elements true
                            :render-dom render-item-DOM
                            :get-action-data (pass-AD)}]))))

(deftest render-item-DOM-test-simple
  ;; Test a simple cell
  (let [[store fred-id] (add-element (new-element-store) nil "Fred")
        dom (run-renderer render-item-DOM
                          (assoc basic-dom-specification
                                 :relative-id fred-id)
                          store)]
    (is (check dom
               [:div {:class "editable content-text item"} "Fred"])))

  ;; Test an entity holding an interned object
  (let [[s1 fred-oid] (-> (new-element-store)
                          (add-universal-objects)
                          (get-new-object-id))
        [s2 fred-name-id] (add-element s1 fred-oid `("Fred" (~name-label)))
        [store fred-holder-id] (add-element s2 nil `(~(id->object fred-oid nil)))
        dom (run-renderer render-item-DOM
                          (assoc basic-dom-specification
                                 :relative-id fred-holder-id
                                 :template `(~(make-object-list
                                               [`("" (~name-label))])))
                          store)
        ;; We expect a component, which we also run, to make sure it is right.
        [_ spec] dom
        inner-dom (run-renderer (:render-dom spec) spec store)]
    (is (check dom
               [:component {:width 1.5
                            :class "editable item"
                            :auxiliary-item-id fred-oid,
                            :relative-id :content
                            :template (make-object-list
                                       [`("" (~name-label))])
                            :render-dom render-content-object-by-name-DOM
                            :get-action-data (pass-AD)}]))
    (is (check inner-dom
               [:component {:width 1.5
                            :class "editable item name named-object"
                            :template (make-object-list [`("" (~name-label))])
                            :is-object-name true
                            :relative-id fred-name-id
                            :omit-universal-elements true
                            :render-dom render-item-DOM
                            :get-action-data (pass-AD)}])))
  ;; Test an entity that is not interned, but that should be displayed
  ;; as if it is.
  (let [[s1 anonymous-oid] (-> (new-element-store)
                          (add-universal-objects)
                          (get-new-object-id))
        [s2 anonymous-name-id] (add-element
                                s1 anonymous-oid `("" (~name-label)))
        [store anonymous-holder-id] (add-element
                                     s2 nil `(~(id->object anonymous-oid nil)))
        dom (run-renderer render-item-DOM
                          (assoc basic-dom-specification
                                 :relative-id anonymous-holder-id
                                 :template `(~(make-object-list
                                               [`("" (~name-label))])))
                          store)
        ;; We expect a component, which we also run, to make sure it is right.
        [_ spec] dom
        inner-dom (run-renderer (:render-dom spec) spec store)]
    ;; We should not have a uniquely identified object
    (is (not (uniquely-identified-object? (id->object anonymous-oid store))))
    (is (check dom
               [:component {:width 1.5
                            :class "editable item"
                            :auxiliary-item-id anonymous-oid,
                            :relative-id :content
                            :template (make-object-list
                                       [`("" (~name-label))])
                            :render-dom render-content-object-by-name-DOM
                            :get-action-data (pass-AD)}]))
    (is (check inner-dom
               [:component {:width 1.5
                            :class "editable item name named-object"
                            :template (make-object-list [`("" (~name-label))])
                            :is-object-name true
                            :relative-id anonymous-name-id
                            :omit-universal-elements true
                            :render-dom render-item-DOM
                            :get-action-data (pass-AD)}])))
  
  ;; Test a cell with a couple of labels, one excluded.
  (let [[store ids] (make-fred-one-two-store)
        one-object (id->object (:one-oid ids) store)
        two-object (id->object (:two-oid ids) store)
        dom (run-renderer render-item-DOM
                          (assoc basic-dom-specification
                                 :relative-id (:fred-id ids)
                                 :excluded-element-ids [(:label-one-id ids)])
                          store)]
    (is (check dom
               [:div {:class "wrapped-element label item"}
                [:component {:template label-template
                             :relative-id (:label-two-id ids)
                             :omit-universal-elements true
                             :render-dom render-item-DOM
                             :get-action-data (default-AD)
                             :class "label"
                             :width 1.5}]
                [:div {:class "indent-wrapper"}
                 [:component {:template (as-set `(""
                                                  (~one-object)
                                                  (~two-object)))
                              :relative-id :content
                              :auxiliary-item-id (:fred-id ids)
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
                       :is-object-name true
                       :position :after
                       :relative-id :virtual-label
                       :omit-universal-elements true
                       :class "label"
                       :render-dom (virt-DOM)
                       :get-action-data (virt-AD)
                       :width 1.5}]
          [:component {:template ""
                       :relative-id :content
                       :auxiliary-item-id fred-id
                       :render-dom render-content-only-DOM
                       :get-action-data (pass-AD)
                       :width 1.5}]]))))

(deftest item-DOM-test-one-column
  ;; Try a couple of elements with no labels
  (let [[store fred-id] (add-element (new-element-store) nil
                                    `("Fred"
                                      (2 (~o2 :order))
                                      (1 (~o1 :order))))
        fred (id->element fred-id store)
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
                             :auxiliary-item-id fred-id
                             :render-dom render-content-only-DOM
                             :get-action-data (pass-AD)}]
                [:div {:class "vertical-stack"}
                 [:div {:class
                        "horizontal-labels-element virtual-wrapper narrow"}
                  [:component
                   {:width 0.9
                    :template virtual-label-template
                    :is-object-name true
                    :relative-id [id1 :virtual-label]
                    :render-dom (virt-DOM)
                    :parallel-ids [id1]
                    :get-action-data [(comp-AD)
                                      [(parallel-AD) (item-AD)]
                                      (virt-AD)]
                    :class "label"
                    :position :after
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
                    :is-object-name true
                    :relative-id [id2 :virtual-label]
                    :render-dom (virt-DOM)
                    :parallel-ids [id2]
                    :get-action-data [(comp-AD)
                                      [(parallel-AD) (item-AD)]
                                      (virt-AD)]
                    :class "label"
                    :position :after
                    :omit-universal-elements true}]
                  [:component {:width 0.9
                               :template 'anything
                               :relative-id id2
                               :render-dom render-item-DOM
                               :get-action-data (default-AD)}]]]])))
  ;; Test an item with two elements, each with one distinct label.
  (let [[store ids] (make-fred-1-one-2-two-store)
        one-object (id->object (:one-oid ids) store)
        two-object (id->object (:two-oid ids) store)
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id (:fred-id ids)
                                    :width 0.9)
                             store)]
    (is (check dom
               [:div {:class "with-elements item"}
                [:component {:template ""
                             :width 0.9
                             :relative-id :content
                             :auxiliary-item-id (:fred-id ids)
                             :render-dom render-content-only-DOM
                             :get-action-data (pass-AD)}]
                [:div {:class "vertical-stack"}
                 [:div {:class "wrapped-element label"}
                  [:component {:width 0.9
                               :template label-template
                               :parallel-ids [(:element-1-id ids)]
                               :class "label"
                               :omit-universal-elements true
                               :relative-id (:label-one-id ids)
                               :render-dom render-item-DOM
                               :get-action-data (default-AD)}]
                  [:div {:class "indent-wrapper"}
                   [:component {:width 0.9
                                :template `(~'anything (~one-object))
                                :relative-id (:element-1-id ids)
                                :excluded-element-ids [(:label-one-id ids)]
                                :render-dom render-item-DOM
                                :get-action-data (default-AD)}]]]
                 [:div {:class "wrapped-element label"}
                  [:component {:width 0.9
                               :template label-template
                               :parallel-ids [(:element-2-id ids)]
                               :class "label"
                               :omit-universal-elements true
                               :relative-id (:label-two-id ids)
                               :render-dom render-item-DOM
                               :get-action-data (default-AD)}]
                  [:div {:class "indent-wrapper"}
                   [:component {:width 0.9
                                :template `(~'anything (~two-object))
                                :relative-id (:element-2-id ids)
                                :excluded-element-ids [(:label-two-id ids)]
                                :render-dom render-item-DOM
                                :get-action-data (default-AD)}]]]]])))
  ;; Test an item with four elements, with label sharing among them.
  (let [[store ids] (make-fred-4-elements-store)
        zero-object (id->object (:zero-oid ids) store)
        one-object (id->object (:one-oid ids) store)
        two-object (id->object (:two-oid ids) store)
        both-object (id->object (:both-oid ids) store)
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id (:fred-id ids)
                                    :width 0.9)
                             store)]
    (is (check dom
               [:div {:class "with-elements item"}
                [:component {:template ""
                             :width 0.9
                             :relative-id :content
                             :auxiliary-item-id (:fred-id ids)
                             :render-dom render-content-only-DOM
                             :get-action-data (pass-AD)}]
                [:div {:class "vertical-stack"}
                 [:div {:class "wrapped-element label"}
                  [:component {:width 0.9
                               :template label-template
                               :parallel-ids [(:element-0-id ids)]
                               :class "label"
                               :omit-universal-elements true
                               :relative-id (:label-zero-id ids)
                               :render-dom render-item-DOM
                               :get-action-data (default-AD)}]
                  [:div {:class "indent-wrapper"}
                   [:component {:width 0.9
                                :template `(~'anything (~zero-object))
                                :excluded-element-ids [(:label-zero-id ids)]
                                :relative-id (:element-0-id ids)
                                :render-dom render-item-DOM
                                :get-action-data (default-AD)}]]]
                 [:div {:class "wrapped-element label"}
                  [:component {:width 0.9
                               :template label-template
                               :parallel-ids (as-set [(:element-1-id ids)
                                                      (:element-2-id ids)])
                               :class "label"
                               :omit-universal-elements true
                               :relative-id (:label-1-both-id ids)
                               :render-dom render-item-DOM
                               :get-action-data (default-AD)}]
                  [:div {:class "indent-wrapper"}
                   [:div {:class "vertical-stack"}
                    [:div {:class "wrapped-element label"}
                     [:component {:width 0.9
                                  :template label-template
                                  :parallel-ids[(:element-1-id ids)]
                                  :class "label"
                                  :omit-universal-elements true
                                  :relative-id (:label-one-id ids)
                                  :render-dom render-item-DOM
                                  :get-action-data (default-AD)}]
                     [:div {:class "indent-wrapper"}
                      [:component {:width 0.9
                                   :template (as-set `(~'anything
                                                       (~one-object)
                                                       (~both-object)))
                                   :excluded-element-ids
                                   (as-set [(:label-1-both-id ids)
                                            (:label-one-id ids)])
                                   :relative-id (:element-1-id ids)
                                   :render-dom render-item-DOM
                                   :get-action-data (default-AD)}]]]
                    [:div {:class "wrapped-element label"}
                     [:component {:width 0.9
                                  :template label-template
                                  :parallel-ids [(:element-2-id ids)]
                                  :class "label"
                                  :omit-universal-elements true
                                  :relative-id (:label-two-id ids)
                                  :render-dom render-item-DOM
                                  :get-action-data (default-AD)}]
                     [:div {:class "indent-wrapper"}
                      [:component {:width 0.9
                                   :template (as-set `(~'anything
                                                       (~two-object)
                                                       (~both-object)))
                                   :excluded-element-ids
                                   (as-set [(:label-2-both-id ids)
                                            (:label-two-id ids)])
                                   :relative-id (:element-2-id ids)
                                   :render-dom render-item-DOM
                                   :get-action-data (default-AD)}]]]]]]
                [:div {:class (str "horizontal-labels-element"
                                   " virtual-wrapper narrow")}
                 [:component {:width 0.9
                              :template virtual-label-template
                              :is-object-name true
                              :parallel-ids [(:element-3-id ids)]
                              :get-action-data [(comp-AD)
                                                [(parallel-AD) (item-AD)]
                                                (virt-AD)]
                              :relative-id [(:element-3-id ids) :virtual-label]
                              :render-dom (virt-DOM)
                              :class "label"
                              :position :after
                              :omit-universal-elements true}]
                 [:component {:width 0.9
                              :template 'anything
                              :relative-id (:element-3-id ids)
                              :render-dom render-item-DOM
                              :get-action-data (default-AD)}]]]]))))

(deftest item-DOM-test-two-column
  ;; Try three elements with no labels, but one of them marked as excluded.
  (let [[store fred-id] (add-element (new-element-store) nil
                                    `("Fred"
                                      (3 (~o3 :order))
                                      (2 (~o2 :order))
                                      (1 (~o1 :order))))
        fred (id->element fred-id store)
        id1 (:item-id (first (matching-elements 1 fred)))
        id2 (:item-id (first (matching-elements 2 fred)))
        id3 (:item-id (first (matching-elements 3 fred)))
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id fred-id
                                    :width 1.5
                                    :must-show-label :wide
                                    :excluded-element-ids [id3])
                             store)]
    (is (check
         dom
         [:div
          {:class "horizontal-labels-element virtual-wrapper narrow item"}
          [:component
           {:width 1.5
            :template virtual-label-template
            :is-object-name true
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
                        :auxiliary-item-id fred-id
                        :render-dom render-content-only-DOM
                        :get-action-data (pass-AD)}]
           [:div {:class "vertical-stack"}
            [:div {:class "horizontal-labels-element label wide"}
             [:div {:class "label horizontal-header top-border bottom-border"}
              [:component {:width 0.375
                           :template virtual-label-template
                           :is-object-name true
                           :relative-id [id1 :virtual-label]
                           :parallel-ids [id1]
                           :get-action-data [(comp-AD)
                                             [(parallel-AD) (item-AD)]
                                             (virt-AD)]
                           :render-dom (virt-DOM)
                           :class "label"
                           :position :after
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
                           :is-object-name true
                           :relative-id [id2 :virtual-label]
                           :parallel-ids [id2]
                           :get-action-data [(comp-AD)
                                             [(parallel-AD) (item-AD)]
                                             (virt-AD)]
                           :render-dom (virt-DOM)
                           :class "label"
                           :position :after
                           :omit-universal-elements true}]]
             [:component {:width 1.03125
                          :template 'anything
                          :relative-id id2
                          :render-dom render-item-DOM
                          :get-action-data (default-AD)}]]]]])))
  ;; Test an item with two elements, each with one distinct label.
  (let [[store ids] (make-fred-1-one-2-two-store)
        one-object (id->object (:one-oid ids) store)
        two-object (id->object (:two-oid ids) store)
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id (:fred-id ids)
                                    :width 1.5)
                             store)]
    (is (check
         dom
         [:div {:class "with-elements item"}
          [:component {:template ""
                       :width 1.5
                       :relative-id :content
                       :auxiliary-item-id (:fred-id ids)
                       :render-dom render-content-only-DOM
                       :get-action-data (pass-AD)}]
          [:div {:class "vertical-stack"}
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class (str "label horizontal-header"
                               " top-border bottom-border")}
             [:component {:width 0.375, :template label-template
                          :parallel-ids [(:element-1-id ids)]
                          :class "label"
                          :omit-universal-elements true
                          :relative-id (:label-one-id ids)
                          :render-dom render-item-DOM
                          :get-action-data (default-AD)}]]
            [:component {:width 1.03125
                         :template `(~'anything (~one-object))
                         :excluded-element-ids [(:label-one-id ids)]
                         :relative-id (:element-1-id ids)
                         :render-dom render-item-DOM
                         :get-action-data (default-AD)}]]
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class (str "label horizontal-header"
                               " top-border bottom-border")}
             [:component {:width 0.375, :template label-template
                          :parallel-ids [(:element-2-id ids)]
                          :class "label"
                          :omit-universal-elements true
                          :relative-id (:label-two-id ids)
                          :render-dom render-item-DOM
                          :get-action-data (default-AD)}]]
            [:component {:width 1.03125
                         :template `(~'anything (~two-object))
                         :excluded-element-ids [(:label-two-id ids)]
                         :relative-id (:element-2-id ids)
                         :render-dom render-item-DOM
                         :get-action-data (default-AD)}]]]])))
  ;; Test an item with four elements, with label sharing among them.
  (let [[store ids] (make-fred-4-elements-store)
        zero-object (id->object (:zero-oid ids) store)
        one-object (id->object (:one-oid ids) store)
        two-object (id->object (:two-oid ids) store)
        both-object (id->object (:both-oid ids) store)
        dom (render-item-DOM (assoc basic-dom-specification
                                    :relative-id (:fred-id ids)
                                    :width 1.5)
                             store)]
    (is (check
         dom
         [:div {:class "with-elements item"}
          [:component {:template ""
                       :width 1.5
                       :relative-id :content
                       :auxiliary-item-id (:fred-id ids)
                       :render-dom render-content-only-DOM
                       :get-action-data (pass-AD)}]
          [:div {:class "vertical-stack"}
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class "label horizontal-header top-border bottom-border"}
             [:component {:width 0.375
                          :template label-template
                          :parallel-ids [(:element-0-id ids)]
                          :class "label"
                          :omit-universal-elements true
                          :relative-id (:label-zero-id ids)
                          :render-dom render-item-DOM
                          :get-action-data (default-AD)}]]
            [:component {:width 1.03125
                         :template `(~'anything (~zero-object))
                         :excluded-element-ids [(:label-zero-id ids)]
                         :relative-id (:element-0-id ids)
                         :render-dom render-item-DOM
                         :get-action-data (default-AD)}]]
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class "label horizontal-header top-border"}
             [:component {:width 0.375
                          :template label-template
                          :parallel-ids (as-set [(:element-1-id ids)
                                                 (:element-2-id ids)])
                          :class "label"
                          :omit-universal-elements true
                          :relative-id (:label-1-both-id ids)
                          :render-dom render-item-DOM
                          :get-action-data (default-AD)}]]
            [:component {:width 1.03125
                         :template `(~'anything (~both-object))
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
                           :parallel-ids [(:element-1-id ids)]
                           :class "label"
                           :omit-universal-elements true
                           :relative-id (:label-one-id ids)
                           :render-dom render-item-DOM
                           :get-action-data (default-AD)}]]]
            [:component {:width 1.03125
                         :template (as-set `(~'anything
                                             (~both-object)
                                             (~one-object)))
                         :excluded-element-ids (as-set [(:label-1-both-id ids)
                                                        (:label-one-id ids)])
                         :relative-id (:element-1-id ids)
                         :render-dom render-item-DOM
                         :get-action-data (default-AD)}]]
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class "label horizontal-header indent bottom-border"}
             [:div {:class "label horizontal-header top-border bottom-border"}
              [:component {:width 0.375
                           :template label-template
                           :parallel-ids [(:element-2-id ids)]
                           :class "label"
                           :omit-universal-elements true
                           :relative-id (:label-two-id ids)
                           :render-dom render-item-DOM
                           :get-action-data (default-AD)}]]]
            [:div {:class "horizontal-value-last"}
             [:component {:width 1.03125
                          :template (as-set `(~'anything
                                              (~both-object)
                                              (~two-object)))
                          :excluded-element-ids (as-set [(:label-2-both-id ids)
                                                         (:label-two-id ids)])
                          :relative-id (:element-2-id ids)
                          :render-dom render-item-DOM
                          :get-action-data (default-AD)}]]]
           [:div {:class "horizontal-labels-element label wide"}
            [:div {:class "label horizontal-header top-border bottom-border"}
             [:component {:width 0.375
                          :template virtual-label-template
                          :is-object-name true
                          :relative-id [(:element-3-id ids) :virtual-label]
                          :parallel-ids [(:element-3-id ids)]
                          :render-dom (virt-DOM)
                          :get-action-data [(comp-AD)
                                            [(parallel-AD) (item-AD)]
                                            (virt-AD)]
                          :class "label"
                          :position :after
                          :omit-universal-elements true}]]
            [:component {:width 1.03125
                         :template 'anything
                         :relative-id (:element-3-id ids)
                         :render-dom render-item-DOM
                         :get-action-data (default-AD)}]]]]))))

(deftest render-virtual-DOM-test
  (is (check (render-virtual-DOM {:class "foo"} new-element-store)
             [:div {:class "foo editable virtual"}])))


(ns cosheet.server.server-test-setup-test
  (:require [clojure.test :refer [deftest is]]
            (cosheet
             [orderable :as orderable]
             [store :refer [new-element-store make-item-id]]
             [entity :refer [elements id->entity make-tree-object]]
             entity-impl
             store-impl
             [store-utils :refer [add-universal-objects
                                  add-link-type-object
                                  find-object-by-name
                                  link-type-object]]
             [test-utils :refer [check any]])
            (cosheet.server
             [order-utils :refer [ordered-entities]]
             [server-test-setup :refer [add-order-elements]])))

(deftest add-order-elements-test
  ;; Pre-store the link-type-object so add-order-elements does not
  ;; recurse into it and pollute its internal structure with orders.
  (let [s0 (-> (new-element-store)
               add-universal-objects
               (add-link-type-object "e") first)
        e-label (find-object-by-name s0 "e" (link-type-object ""))
        ordered (add-order-elements
                 `("a"
                   ("b" "c")
                   "d"
                   (~e-label)
                   (~(make-tree-object `("f")))
                   (~(id->entity (make-item-id "test") nil))))]
    (is (check ordered
               `("a" ("b" ("c" (~(any) :order))
                      (~(any) :order))
                 ("d" (~(any) :order))
                 (~e-label (~(any) :order))
                 (~(make-tree-object `(("f" (~(any) :order))))
                  (~(any) :order))
                 (~(id->entity (make-item-id "test") nil)
                  (~(any) :order))
                 (~(any) :order))))
    (is (orderable/earlier? (-> ordered second second second first)
                            (-> ordered second (nth 2) first)))
    (is (orderable/earlier? (-> ordered second (nth 2) first)
                            (-> ordered (nth 2) second first)))
    ;; The last element is not semantic, as it is order information.
    (let [semantic-elements (butlast (elements ordered))]
      (is (check (ordered-entities semantic-elements)
                 semantic-elements))
      (is (check (ordered-entities (reverse semantic-elements))
                 semantic-elements)))))

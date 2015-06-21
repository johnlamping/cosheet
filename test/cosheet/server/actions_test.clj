(ns cosheet.server.actions-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet
             [utils :refer [dissoc-in]]
             [entity :as entity :refer [description->entity content]]
             [computation-manager :refer [new-management compute]]
             [debug :refer [current-value]]
             entity-impl
             [store :refer [new-element-store new-mutable-store current-store]]
             store-impl
             [store-utils :refer [add-entity]]
             mutable-store-impl)
            (cosheet.server
             [render :refer [item-DOM]]
             [dom-tracker :refer [new-dom-tracker add-dom]]
             [actions :refer :all])
            ; :reload
            ))

(deftest set-content-test
  (let [[store joe-id] (add-entity (new-element-store) nil "Joe")
        mutable-store (new-mutable-store store)
        management (new-management)
        tracker (new-dom-tracker management)
        joe (description->entity joe-id mutable-store)]
    (add-dom tracker "root" [joe] [item-DOM joe #{} {}])
    (compute management)
    (set-content mutable-store tracker "root" "Joe" "Jim")
    (is (= (current-value (content joe)) "Jim"))
    (do-actions  mutable-store tracker {1 [:set-content "root" "Jim" "Fred"]})
    (is (= (current-value (content joe)) "Fred"))
))

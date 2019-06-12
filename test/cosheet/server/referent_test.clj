(ns cosheet.server.referent-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet [entity :refer [description->entity]]
                     [store :refer [new-element-store]]
                     [store-impl :refer [->ItemId]]
                     [store-utils :refer [add-entity]]
                     [test-utils :refer [check]])
            (cosheet.server [referent :refer :all])
            ; :reload
            ))

(deftest string-conversion-test
  (let [referent (exemplar-referent
                  (item-referent (->ItemId 0))
                  (union-referent
                   [(elements-referent (item-referent (->ItemId 1))
                                       (element-restriction-referent
                                        '(nil :foo)
                                        (item-referent (->ItemId 3))))
                    (difference-referent
                     (query-referent (list (item-referent (->ItemId 3))
                                           '(nil (:root :A_a))
                                           'b))
                     (item-referent (->ItemId 6789)))
                    (non-competing-elements-referent
                     'a
                     (item-referent (->ItemId 11))
                     [(item-referent (->ItemId 33))
                      (item-referent (->ItemId 44))])
                    (virtual-referent (item-referent (->ItemId 1234))
                                      (item-referent (->ItemId 2345))
                                      [(item-referent (->ItemId 3456))]
                                      :use-bigger true)]))
        serialized (referent->string referent)
        parsed (string->referent serialized)]
    (is (check parsed referent))))

(def t1 (add-entity (new-element-store) nil "Joe"))
(def store (first t1))
(def joe (description->entity (second t1) store))

(deftest item-or-exemplar-referent-test
  (is (= (item-or-exemplar-referent joe nil)
         (item-referent joe)))
   (is (= (item-or-exemplar-referent joe (query-referent '(:hi :there)))
          (exemplar-referent joe (query-referent '(:hi :there))))))


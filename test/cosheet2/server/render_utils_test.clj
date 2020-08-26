(ns cosheet2.server.render-utils-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            (cosheet2
             [store :refer [new-element-store]]
             [entity :as entity  :refer [to-list description->entity]]
             [canonical :refer [canonicalize-list]]
             [store-utils :refer [add-entity]]
             [debug :refer [simplify-for-print]]
             [test-utils :refer [check any as-set]])
            (cosheet2.server
             [render-utils :refer :all])
            ; :reload
            ))

(deftest condition-satisfiers-test
  (is (check (map canonicalize-list
                  (condition-satisfiers '("age" "A" ("B" 1) "C")
                                        '(nil "A")))
             ["a"]))
  (is (check (map canonicalize-list
                  (condition-satisfiers '("age" "A" ("B" 1) "C")
                                        '(nil "A" "C")))
             (as-set ["a" "c"])))
  (is (check (map canonicalize-list
                  (condition-satisfiers '("age" "A" ("B" 1) "C")
                                        '(nil "A" "B")))
             ["a"]))
  (is (check (map canonicalize-list
                  (condition-satisfiers '("age" "A" "A" ("B" 1) "C")
                                        '(nil "A" "B")))
             ["a"]))
  (is (check (map canonicalize-list
                  (condition-satisfiers '("age" "A" "A" ("B" 1) "C")
                                        '(nil "A" "A" "B")))
             ["a" "a"]))
  (is (check (map canonicalize-list
                  (condition-satisfiers '("age" "A" ("B" 1) "C")
                                        '(nil "A" "A" "B")))
             ["a"])))

(deftest competing-siblings-test
  (let [[s1 joe-id] (add-entity
                     (new-element-store) nil
                     '("joe"
                       anything (anything 1) (anything 1)
                       (2 1) (2 3)))
        [s2 item-a1-id] (add-entity s1 joe-id '(anything 1))
        [s3 item-b3-id] (add-entity s2 joe-id '("" 3))
        [store item-21-id] (add-entity s3 joe-id '(2 1))
        joe (description->entity joe-id store)
        item-a1 (description->entity item-a1-id store)
        item-b3 (description->entity item-b3-id store)
        item-21 (description->entity item-21-id store)]
    (let [competing (competing-siblings item-a1)]
      (is (check (map entity/to-list competing)
                 (as-set ['(anything 1) '(2 1) '(2 3) '("" 3)])))
      (is (not-any? #(= % item-a1) competing)))
    (let [competing (competing-siblings item-21)]
      (is (check (map entity/to-list competing)
                 (as-set ['(2 1) '(2 3)])))
      (is (not-any? #(= % item-21) competing)))
    (let [competing (competing-siblings item-b3)]
      (is (check (map entity/to-list competing)
                 ['(anything 1)])))))
 


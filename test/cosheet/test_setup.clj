(ns cosheet.test-setup
  (:require (cosheet
             [store :refer [new-element-store add-link get-new-object-id]]
             store-impl)))

(defn cyclic-and-shared-a-b-stores
  "Make a pair of stores, each with two non-interned objects a and b.
  In :cyclic-store, a has an \"x\" element and an element with content
  b, while b has a \"y\" element and a \"z\" element. Since the link
  from a to be can be traversed in either direction, it forms a
  cycle. But each object is reached only once during
  repetition-avoiding traversal.

  :cyclic-shared-store extends :cyclic-store with an additional sub-
  element on the \"z\" element whose content is a, so a is reachable
  via two paths from b and so appears twice in the tree starting from
  b.

  Returns {:cyclic-store ...
           :cyclic-shared-store ...
           :a-id a-id
           :b-id b-id}."
  []
  (let [[s1 a-id]      (get-new-object-id (new-element-store))
        [s2 b-id]      (get-new-object-id s1)
        [s3 _]         (add-link s2 a-id "x")
        [s4 _]         (add-link s3 b-id "y")
        [s5 z-link-id] (add-link s4 b-id "z")
        [cyclic _]     (add-link s5 a-id b-id)
        [cyclic-shared _]     (add-link cyclic z-link-id a-id)]
    {:cyclic-store cyclic
     :cyclic-shared-store cyclic-shared
     :a-id a-id
     :b-id b-id}))

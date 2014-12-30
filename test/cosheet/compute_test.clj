(ns cosheet.compute-test
  (:require [clojure.test :refer [deftest is]]
            [cosheet.compute :refer :all]
            [cosheet.compute-impl :refer :all]
            :reload))

(deftest valid?-test
  (is (valid? {}))
  (is (not (valid? {:value-inputs-changed true})))
  (is (not (valid? {:uncertain-inputs #{5}}))))

(comment "How to do local redefinitions in a rest"
  (def ^:dynamic counter 0)
  (deftest conflicts-mutable-map-test
    (let [mm (new-mutable-map)
          orig-update-non-nil update-non-nil
          orig-add-mutable-value add-mutable-value]
      (binding [counter 0]
        (with-redefs [update-non-nil
                      (fn [va f]
                        (set! counter (inc counter))
                        (if (not= (mod counter 5) 0)
                          (swap! va (fn [x] nil)))
                        (orig-update-non-nil va f))
                      add-mutable-value
                      (fn [mm key value]
                        (set! counter (inc counter))
                        (if (not= (mod counter 6) 0)
                          (swap! mm (fn [m] (assoc m key (atom 3)))))
                        (orig-add-mutable-value mm key value))]
          (update! mm :foo (fn [x] 5))
          (is (= (get! mm :foo)) 5)
          (is (< 5 counter 15)))))))

(comment
 (deftest updated-dependency-info-test
   (is (= (updated-dependency-info
           {:used-info {:a {:value "a"}}
            :uncertain-inputs #{:a}}
           {:expression :a :value "b"})
          {:used-info {:a {:value "a"}}
           :changed-inputs #{:a}}))
   (is (= (updated-dependency-info
           {:used-info {:a {:value "a"}}
            :changed-inputs #{:a}}
           {:expression :a :value "a"
            :changed-inputs #{:x}})
          {:used-info {:a {:value "a"}}
           :uncertain-inputs #{:a}}))
   (is (= (updated-dependency-info
           {:used-info {:a {:value "a"}}
            :changed-inputs #{:a}}
           {:expression :z :value "a"
            :changed-inputs #{:x}})
          {:used-info {:a {:value "a"}}
           :changed-inputs #{:a}}))))


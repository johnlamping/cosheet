(ns cosheet.dom-utils-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            [cosheet.dom-utils :refer :all]
            ; :reload
            ))

(deftest into-attributes-test
  (is (= (-> {}
             (into-attributes {:class "foo"
                               :style {:font "bold"}
                               :other "abc"})
             (into-attributes {:class "bar"
                               :style {:text "large"}
                               :misc "junk"})
             (into-attributes {:class "baz"
                               :style {:color "red"}})
             (into-attributes {:class ""})
             (into-attributes {:class nil}))
         {:class "foo bar baz"
          :style {:font "bold" :text "large" :color "red"}
          :other "abc"
          :misc "junk"})))

(deftest dom-attributes-test
  (let [attributes {:class "foo" :style {:font "bold"} :other "abc"}]
    (is (= (dom-attributes [:div "hi"]) {}))
    (is (= (dom-attributes [:div attributes])
           attributes))
    (is (= (dom-attributes [:component {:attributes attributes :definition 5}])
           attributes))))

(deftest add-attributes-test
  (is (= (-> [:component {:definition [:foo :bar]}]
             (add-attributes {:class "foo"
                              :style {:font "bold"}
                              :other "abc"})
             (add-attributes {:class "bar"
                              :style {:text "large"}
                              :misc "junk"}))
         [:component {:definition [:foo :bar]
                      :attributes {:class "foo bar"
                                   :style {:font "bold" :text "large"}
                                   :other "abc"
                                   :misc "junk"}}]))
  (is (= (-> [:div "hi there"]
             (add-attributes {:class "foo"
                              :style {:font "bold"}
                              :other "abc"})
             (add-attributes {:class "bar"
                              :style {:text "large"}
                              :misc "junk"}))
         [:div {:class "foo bar"
                :style {:font "bold" :text "large"}
                :other "abc"
                :misc "junk"}
          "hi there"])))

(ns cosheet2.reporter-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            (cosheet2 [reporter :refer :all]
                      [utils :refer [multiset]]
                      [test-utils :refer [check]])
            ; :reload
            ))

(deftest valid-test
  (is (valid? 1))
  (is (not (valid? invalid))))

(deftest constant-test
  (is (= (value 2) 2))
  (let [history (atom [])
        callback (fn [& args] (swap! history #(conj % args)))]
    (set-attendee-and-call! 2 :key 5 callback)
    (is (= @history
           [[:key :key :reporter 2 :description nil :categories nil]]))))

(deftest update-add-remove-attendee-test
  (let [data {:value invalid :priority Double/MAX_VALUE}
        added1 (update-add-attendee
                data :key1 2 [::universal-category] inc)
        added2 (update-add-attendee
                added1 :key2 3 [:a :b] dec)
        removed0 (update-remove-attendee added2 :key3)
        removed1 (update-remove-attendee removed0 :key1)
        removed2 (update-remove-attendee removed1 :key2)]
    (is (check added1
           {:value invalid
            :priority 2
            :attendees {:key1 [2 [::universal-category] inc]}
            :selections {::universal-category #{:key1}}}))
    (is (check added2
           {:value invalid
            :priority 2
            :attendees {:key1 [2 [::universal-category] inc]
                        :key2 [3 [:a :b] dec]}
            :selections {::universal-category #{:key1}
                         :a #{:key2}
                         :b #{:key2}}}))
    (is (check removed0 added2))
    (is (check removed1
           {:value invalid
            :priority 3
            :attendees {:key2 [3 [:a :b] dec]}
            :selections {:a #{:key2}
                         :b #{:key2}}}))
    (is (check removed2 data))))

(deftest reporter-test
  (let [history (atom [])
        callback (fn [& args] (swap! history #(conj % args)))
        calculator (partial callback :c)
        r (new-reporter :value 2
                        :calculator calculator
                        :extra :e)]
    (is (reporter? r))
    (is (not (reporter? 2)))
    (is (not (attended? r)))
    (set-calculator-data! r :cd)
    (set-attendee-and-call! r :foo 1 (partial callback :f))
    (is (= (value r) 2))
    (is (= (:extra (data r)) :e))
    (is (= (:priority (data r)) 1))
    (is (check @history
               [[:c :cd r]
                [:f :key :foo :reporter r :description nil :categories nil]]))
    (set-value! r 3)
    (is (= (value r) 3))
    (is (check @history
               [[:c :cd r]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:f :key :foo :reporter r :description nil :categories nil]]))
    (set-attendee! r :foo)
    (is (not (attended? r)))
    (is (> (:priority (data r)) 1e20))
    (is (check @history
               [[:c :cd r]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:c :cd r]]))
    (reset! history [])
    (set-attendee! r :bar 3 (partial callback :b))
    (is (check @history
               [[:c :cd r]]))
    (set-attendee-and-call! r :tst 5 (partial callback :t))
    (is (check @history
               [[:c :cd r]
                [:t :key :tst :reporter r :description nil :categories nil]]))
    (is (= (:priority (data r)) 3))
    (set-value! r 4)
    (is (check (multiset @history)
               (multiset
                [[:c :cd r]
                 [:t :key :tst :reporter r :description nil :categories nil]
                 [:b :key :bar :reporter r :description nil :categories nil]
                 [:t :key :tst :reporter r :description nil :categories nil]])))
    ;; Check priority updates
    (reset! history [])
    (set-attendee-and-call! r :foo 1 (partial callback :f))
    (is (= (:priority (data r)) 1))
    (is (check @history
               [[:c :cd r]
                [:f :key :foo :reporter r :description nil :categories nil]]))
    (set-attendee! r :bar)
    (is (= (:priority (data r)) 1))
    (is (check @history
               [[:c :cd r]
                [:f :key :foo :reporter r :description nil :categories nil]]))
    (set-attendee-and-call! r :foo 4 (partial callback :f))
    (is (= (:priority (data r)) 4))
    (is (check @history
               [[:c :cd r]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:c :cd r]
                [:f :key :foo :reporter r :description nil :categories nil]]))
    (remove-attendee! r :foo)
    (is (= (:priority (data r)) 5))
    (is (check @history
               [[:c :cd r]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:c :cd r]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:c :cd r]]))
    
    (is (thrown? java.lang.AssertionError (set-attendee! r :foo 0 1)))
    (is (thrown? java.lang.AssertionError (set-calculator-data! r :stuff))))

  ;; Check selective attending.
  (let [history (atom [])
        callback (fn [& args] (swap! history #(conj % args)))
        calculator (partial callback :c)
        r (new-reporter :value 2 :calculator calculator)]
    (is (thrown? java.lang.AssertionError
                 (set-calculator! r 1)))
    (set-calculator-data! r :cd)
    (set-attendee-and-call! r :sel 1 [:a :b] (partial callback :s))
    (set-attendee! r :all 1 (partial callback :a))
    (is (check @history
               [[:c :cd r]
                [:s :key :sel :reporter r :description nil :categories nil]
                [:c :cd r]]))
    (set-value! r 3)
    (is (check (multiset @history)
               (multiset
                [[:c :cd r]
                 [:s :key :sel :reporter r :description nil :categories nil]
                 [:c :cd r]
                 [:s :key :sel :reporter r :description nil :categories nil]
                 [:a :key :all :reporter r :description nil :categories nil]])))
    (change-value! r (fn [v] [(+ v 1) :increment [:c]]))
    (is (= (value r) 4))
    (is (check (multiset @history)
               (multiset
                [[:c :cd r]
                 [:s :key :sel :reporter r :description nil :categories nil]
                 [:c :cd r]
                 [:s :key :sel :reporter r :description nil :categories nil]
                 [:a :key :all :reporter r :description nil :categories nil]
                 [:a :key :all :reporter r
                  :description :increment :categories [:c]]])))
    (change-value! r (fn [v] [(* v 2) :double [:c :a]]))
    (is (= (value r) 8))
    (is (check (multiset @history)
               (multiset
                [[:c :cd r]
                 [:s :key :sel :reporter r :description nil :categories nil]
                 [:c :cd r]
                 [:s :key :sel :reporter r :description nil :categories nil]
                 [:a :key :all :reporter r :description nil :categories nil]
                 [:a :key :all :reporter r
                  :description :increment :categories [:c]]
                 [:s :key :sel :reporter r
                  :description :double :categories [:c :a]]
                 [:a :key :all :reporter r
                  :description :double :categories [:c :a]]])))
    ))


;; TODO: Check descriptive updates








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
    (set-attendee! 2 :key 5 callback)
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
        calculator (partial callback :m)
        r (new-reporter :value 2
                        :calculator calculator
                        :extra :e)]
    (is (reporter? r))
    (is (not (reporter? 2)))
    (is (not (attended? r)))
    (set-attendee! r :foo 1 (partial callback :f))
    (is (= (value r) 2))
    (is (= (:extra (data r)) :e))
    (is (= (:priority (data r)) 1))
    (is (check @history
               [[:m r]
                [:f :key :foo :reporter r :description nil :categories nil]]))
    (set-value! r 3)
    (is (= (value r) 3))
    (is (check @history
               [[:m r]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:f :key :foo :reporter r :description nil :categories nil]]))
    (set-attendee! r :foo)
    (is (not (attended? r)))
    (is (> (:priority (data r)) 1e20))
    (is (check @history
               [[:m r]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:m r]]))
    (reset! history [])
    (set-attendee! r :bar 3 (partial callback :b))
    (is (check @history
               [[:m r]
                [:b :key :bar :reporter r :description nil :categories nil]]))
    (set-attendee! r :tst 5 (partial callback :t))
    (is (check @history
               [[:m r]
                [:b :key :bar :reporter r :description nil :categories nil]
                [:t :key :tst :reporter r :description nil :categories nil]]))
    (is (= (:priority (data r)) 3))
    (set-value! r 4)
    (is (check (multiset @history)
               (multiset
                [[:m r]
                 [:b :key :bar :reporter r :description nil :categories nil]
                 [:t :key :tst :reporter r :description nil :categories nil]
                 [:b :key :bar :reporter r :description nil :categories nil]
                 [:t :key :tst :reporter r :description nil :categories nil]])))
    ;; Check priority updates
    (reset! history [])
    (set-attendee! r :foo 1 (partial callback :f))
    (is (= (:priority (data r)) 1))
    (is (check @history
               [[:m r]
                [:f :key :foo :reporter r :description nil :categories nil]]))
    (set-attendee! r :bar)
    (is (= (:priority (data r)) 1))
    (is (check @history
               [[:m r]
                [:f :key :foo :reporter r :description nil :categories nil]]))
    (set-attendee! r :foo 4 (partial callback :f))
    (is (= (:priority (data r)) 4))
    (is (check @history
               [[:m r]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:m r]
                [:f :key :foo :reporter r :description nil :categories nil]]))
    (set-attendee! r :foo)
    (is (= (:priority (data r)) 5))
    (is (check @history
               [[:m r]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:m r]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:m r]]))
    
    (is (thrown? java.lang.AssertionError (set-attendee! r :foo 0 1)))
    (set-calculator! r calculator)
    (is (thrown? java.lang.AssertionError (set-calculator! r callback))))

  ;; Check selective attending.
  (let [history (atom [])
        callback (fn [& args] (swap! history #(conj % args)))
        calculator (partial callback :m)
        r (new-reporter :value 2)]
    (is (thrown? java.lang.AssertionError
                 (set-calculator! r 1)))
    (set-calculator! r calculator)
    (set-selective-attendee! r :sel 1 [:a :b] (partial callback :s))
    (set-attendee! r :all 1 (partial callback :a))
    (is (check @history
               [[:m r]
                [:s :key :sel :reporter r :description nil :categories nil]
                [:a :key :all :reporter r :description nil :categories nil]]))
    (set-value! r 3)
    (is (check (multiset @history)
               (multiset
                [[:m r]
                 [:s :key :sel :reporter r :description nil :categories nil]
                 [:a :key :all :reporter r :description nil :categories nil]
                 [:s :key :sel :reporter r :description nil :categories nil]
                 [:a :key :all :reporter r :description nil :categories nil]])))
    (make-change! r (fn [v] [(+ v 1) :increment [:c]]))
    (is (= (value r) 4))
    (is (check (multiset @history)
               (multiset
                [[:m r]
                 [:s :key :sel :reporter r :description nil :categories nil]
                 [:a :key :all :reporter r :description nil :categories nil]
                 [:s :key :sel :reporter r :description nil :categories nil]
                 [:a :key :all :reporter r :description nil :categories nil]
                 [:a :key :all :reporter r
                  :description :increment :categories [:c]]])))
    (make-change! r (fn [v] [(* v 2) :double [:c :a]]))
    (is (= (value r) 8))
    (is (check (multiset @history)
               (multiset
                [[:m r]
                 [:s :key :sel :reporter r :description nil :categories nil]
                 [:a :key :all :reporter r :description nil :categories nil]
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








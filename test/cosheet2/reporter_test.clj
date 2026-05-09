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
  (is (= (reporter-value 2) 2))
  (let [history (atom [])
        callback (fn [& args] (swap! history #(conj % args)))]
    (set-attendee-and-call! 2 :key 5 callback)
    (is (= @history
           [[:key :key :reporter 2 :description nil :categories nil]]))))

(deftest update-attendee-test
  (let [data {:value invalid :priority Double/MAX_VALUE}
        added1 (update-attendee
                data :key1 2 [::universal-category] inc)
        added2 (update-attendee
                added1 :key2 3 [:a :b] dec)
        removed0 (update-attendee added2 :key3 Double/MAX_VALUE [] nil)
        removed1 (update-attendee removed0 :key1 Double/MAX_VALUE [] nil)
        removed2 (update-attendee removed1 :key2 Double/MAX_VALUE [:foo] nil)]
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
        r (make-reporter :value invalid
                        :calculator calculator
                        :extra :e)]
    (is (reporter? r))
    (is (not (reporter? 2)))
    (is (not (attended? r)))
    (is (= (reporter-value r) invalid))
    (is (= (reporter-value-when-valid r) nil))
    (set-value! r 2)
    (set-calculator-data! r :cd)
    (set-attendee-and-call! r :foo 1 (partial callback :f))
    (is (= (reporter-value r) 2))
    (is (= (reporter-value-when-valid r) 2))
    (is (= (:extra (reporter-data r)) :e))
    (is (= (:priority (reporter-data r)) 1))
    (is (check @history
               [[:c r :cd]
                [:f :key :foo :reporter r :description nil :categories nil]]))
    (set-value! r 3)
    (is (= (reporter-value r) 3))
    (is (= (reporter-value-when-valid r) 3))
    (is (check @history
               [[:c r :cd]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:f :key :foo :reporter r :description nil :categories nil]]))
    (set-attendee! r :foo)
    (is (not (attended? r)))
    (is (> (:priority (reporter-data r)) 1e20))
    (is (check @history
               [[:c r :cd]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:c r :cd]]))
    (reset! history [])
    (set-attendee! r :bar 3 (partial callback :b))
    (is (check @history
               [[:c r :cd]]))
    (set-attendee-and-call! r :tst 5 (partial callback :t))
    (is (check @history
               [[:c r :cd]
                [:t :key :tst :reporter r :description nil :categories nil]]))
    (is (= (:priority (reporter-data r)) 3))
    (set-value! r 4)
    (is (check (multiset @history)
               (multiset
                [[:c r :cd]
                 [:t :key :tst :reporter r :description nil :categories nil]
                 [:b :key :bar :reporter r :description nil :categories nil]
                 [:t :key :tst :reporter r :description nil :categories nil]])))
    ;; Check priority updates
    (reset! history [])
    (set-attendee-and-call! r :foo 1 (partial callback :f))
    (is (= (:priority (reporter-data r)) 1))
    (is (check @history
               [[:c r :cd]
                [:f :key :foo :reporter r :description nil :categories nil]]))
    (set-attendee! r :bar)
    (is (= (:priority (reporter-data r)) 1))
    (is (check @history
               [[:c r :cd]
                [:f :key :foo :reporter r :description nil :categories nil]]))
    (set-attendee-and-call! r :foo 4 (partial callback :f))
    (is (= (:priority (reporter-data r)) 4))
    (is (check @history
               [[:c r :cd]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:c r :cd]
                [:f :key :foo :reporter r :description nil :categories nil]]))
    (set-attendee-and-call! r :foo 1 nil) ; Should remove attendee.
    (is (= (:priority (reporter-data r)) 5))
    (is (check @history
               [[:c r :cd]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:c r :cd]
                [:f :key :foo :reporter r :description nil :categories nil]
                [:c r :cd]]))
    
    (is (thrown? java.lang.AssertionError (set-attendee! r :foo 0 1)))
    (is (thrown? java.lang.AssertionError (set-calculator-data! r :stuff))))

  ;; Check selective attending.
  (let [calculator-history (atom [])
        universal-history (atom [])
        selective-history (atom [])
        value-history (atom [])
        validity-history (atom [])
        make-callback (fn [history]
                        (fn [& args] (swap! history #(conj % args))))
        r (make-reporter :value 2
                        :calculator (make-callback calculator-history))]

    (set-calculator-data! r :cd)
    (is (thrown? java.lang.AssertionError
                 (set-calculator-data! r 1)))
    (set-attendee! r :all 1
                   (make-callback universal-history))
    (set-attendee-and-call! r :sel 1 [:a :b]
                            (make-callback selective-history))
    (set-attendee-and-call! r :value 1 [value-category]
                            (make-callback value-history))
    (set-attendee-and-call! r :validity 1 [:never validity-category]
                            (make-callback validity-history))
    ;; Each attendee adds a different demand. So the calculator should
    ;; have been notified after each of them.
    (is (check @calculator-history
               [[r :cd] [r :cd] [r :cd] [r :cd]]))
    (is (check @universal-history
               []))
    (is (check @selective-history
               [[:key :sel :reporter r :description nil :categories nil]]))
    (is (check @value-history
               [[:key :value :reporter r :description nil :categories nil]]))
    (is (check @validity-history
               ;; An uncharacterized change matches the :never option
               ;; of the validity callback.
               [[:key :validity :reporter r :description nil :categories nil]]))

    (set-value! r invalid)
    (is (not (reporter-valid? r)))
    (is (= (reporter-value r) invalid))
    (is (= (reporter-latest-value r) 2))
    (is (= (reporter-value-when-valid r) nil))
    (is (check @calculator-history
               [[r :cd] [r :cd] [r :cd] [r :cd]]))
    (is (check @universal-history
               [[:key :all :reporter r :description nil
                 :categories [validity-category]]]))
    (is (check @selective-history
               [[:key :sel :reporter r :description nil :categories nil]]))
    (is (check @value-history
               [[:key :value :reporter r :description nil :categories nil]]))
    (is (check @validity-history
               ;; an uncharacterized change matches the other option
               ;; of the validity callback
               [[:key :validity :reporter r :description nil :categories nil]
                [:key :validity :reporter r :description nil
                 :categories [validity-category]]]))
    
    (set-value! r 3)
    (is (reporter-valid? r))
    (is (= (reporter-value r) 3))
    (is (= (reporter-latest-value r) 3))
    (is (= (reporter-value-when-valid r) 3))
    (is (check @calculator-history
               [[r :cd] [r :cd] [r :cd] [r :cd]]))
    (is (check @universal-history
               [[:key :all :reporter r :description nil
                 :categories [validity-category]]
                [:key :all :reporter r :description nil :categories nil]]))
    (is (check @selective-history
               [[:key :sel :reporter r :description nil :categories nil]
                [:key :sel :reporter r :description nil :categories nil]]))
    (is (check @value-history
               [[:key :value :reporter r :description nil :categories nil]
                [:key :value :reporter r :description nil :categories nil]]))
    (is (check @validity-history
               ;; an uncharacterized change matches the other option
               ;; of the validity callback
               [[:key :validity :reporter r :description nil :categories nil]
                [:key :validity :reporter r :description nil
                 :categories [validity-category]]
                [:key :validity :reporter r :description nil
                 :categories nil]]))
    
    (change-value! r (fn [v] [(+ v 1) :increment [:a]]))
    (is (reporter-valid? r))
    (is (= (reporter-value r) 4))
    (is (check @calculator-history
               [[r :cd] [r :cd] [r :cd] [r :cd]]))
    (is (check @universal-history
               [[:key :all :reporter r :description nil
                 :categories [validity-category]]
                [:key :all :reporter r :description nil :categories nil]
                [:key :all :reporter r :description :increment
                 :categories [:a]]]))
    (is (check @selective-history
               [[:key :sel :reporter r :description nil :categories nil]
                [:key :sel :reporter r :description nil :categories nil]
                [:key :sel :reporter r :description :increment
                 :categories [:a]]]))
    (is (check @value-history
               [[:key :value :reporter r :description nil :categories nil]
                [:key :value :reporter r :description nil :categories nil]
                [:key :value :reporter r :description :increment
                 :categories [:a]]]))
    (is (check @validity-history
               ;; an uncharacterized change matches the other option
               ;; of the validity callback
               [[:key :validity :reporter r :description nil :categories nil]
                [:key :validity :reporter r :description nil
                 :categories [validity-category]]
                [:key :validity :reporter r :description nil
                 :categories nil]]))

    (change-value! r (fn [v] [(+ v 1) :increment [:c]]))
    (is (reporter-valid? r))
    (is (= (reporter-value r) 5))
    (is (check @calculator-history
               [[r :cd] [r :cd] [r :cd] [r :cd]]))
    (is (check @universal-history
               [[:key :all :reporter r :description nil
                 :categories [validity-category]]
                [:key :all :reporter r :description nil :categories nil]
                [:key :all :reporter r :description :increment
                 :categories [:a]]
                [:key :all :reporter r :description :increment
                 :categories [:c]]]))
    (is (check @selective-history
               [[:key :sel :reporter r :description nil :categories nil]
                [:key :sel :reporter r :description nil :categories nil]
                [:key :sel :reporter r :description :increment
                 :categories [:a]]]))
    (is (check @value-history
               [[:key :value :reporter r :description nil :categories nil]
                [:key :value :reporter r :description nil :categories nil]
                [:key :value :reporter r :description :increment
                 :categories [:a]]
                [:key :value :reporter r :description :increment
                 :categories [:c]]]))
    (is (check @validity-history
               ;; an uncharacterized change matches the other option
               ;; of the validity callback
               [[:key :validity :reporter r :description nil :categories nil]
                [:key :validity :reporter r :description nil
                 :categories [validity-category]]
                [:key :validity :reporter r :description nil
                 :categories nil]]))

    ;; Should not cause a calculator call, because the selections
    ;; don't change.
    (set-attendee! r :sel2 1 [:a]
                   (make-callback selective-history))
    (is (check @calculator-history
               [[r :cd] [r :cd] [r :cd] [r :cd]]))
    (set-attendee! r :sel2 1 [:a] nil)
    (is (check @calculator-history
               [[r :cd] [r :cd] [r :cd] [r :cd]]))
    ;; Removes the last demand for :b
    (set-attendee! r :sel)
    (is (check @calculator-history
               [[r :cd] [r :cd] [r :cd] [r :cd] [r :cd]]))))

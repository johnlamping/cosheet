(ns cosheet.server.session-state-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            [clojure.data.priority-map :as priority-map]
            (cosheet
             [utils :refer [parse-string-as-number]]
             [debug :refer [simplify-for-print]]
             [test-utils :refer [check any as-set let-mutated]]
             [mutable-manager :refer [current-mutable-value]]
             [store :refer [new-element-store new-mutable-store]]
             [query :refer [matching-items]]
             [state-map :refer [new-state-map state-map-get-current-value]]
             [expression-manager :refer [new-expression-manager-data]])
            (cosheet.server
             [session-state :refer :all]
             [model-utils :refer [starting-store add-table first-tab-R]]
             [referent :refer [immutable-semantic-to-list]])
            ; :reload
            ))

(deftest url-path-to-file-path-test
  (let [home (System/getProperty "user.home")]
    (is (= (url-path-to-file-path "foo/") nil))
    (is (= (url-path-to-file-path "/cosheet/foo")
           (str home "/cosheet/foo")))
    (is (= (url-path-to-file-path "/~/foo")
           (str home "/foo")))
    (is (= (url-path-to-file-path "//foo")
           "/foo"))))

(deftest remove-url-file-extension-test
  (is (= (remove-url-file-extension "a/b.c/c.foo?bar=1&baz=2?baz")
         "a/b.c/c?bar=1&baz=2?baz"))
  (is (= (remove-url-file-extension "a/b.c/c?bar=1&baz=2?baz")
         "a/b.c/c?bar=1&baz=2?baz")))

(deftest read-csv-reader-test
  (let [store (read-csv-reader (new java.io.StringReader "a, b
                                                          1, 2
                                                          3")
                               "Hello")
        row1 (first (matching-items '(nil (1 ("a" :tag))) store))
        row2 (first (matching-items '(nil (3 ("a" :tag))) store))]
    (is (= (immutable-semantic-to-list row1)
           '("" ("Hello" :tag) (1 ("a" :tag)) (2 ("b" :tag)))))
    (is (= (immutable-semantic-to-list row2)
           '("" ("Hello" :tag) (3 ("a" :tag)))))))

(deftest create-client-state-test
  (let [store (add-table (starting-store nil) "Hello" [["a" "b"] [1 2] [3]])
        state-map (create-client-state
                   (new-mutable-store store) "I5")]
    (is (check (current-mutable-value state-map)
               {:last-time (any)
                :referent (any #(= (:id %) 5))
                :subject-referent nil
                :last-action nil
                :batch-editing false
                :in-sync false}))))

(deftest new-id-test
  (let [session-map {"1" :a "2" :b}
        id (new-id session-map)]
    (is (number? (parse-string-as-number id)))
    (is (not (#{"1" "2"} id)))))


(deftest get-session-state-test
  (reset! session-info
          {:sessions {123 {:client-state (new-state-map {})}}})
  (let [state (get-session-state 123)
        diff (- (System/currentTimeMillis)
                (state-map-get-current-value
                 (:client-state state) :last-time))]
    (is (#{0 1} diff))))

(deftest prune-old-sessions-test
  (let [now (System/currentTimeMillis)]
    (reset! session-info
            {:sessions {123 {:client-state (new-state-map
                                            {:last-time (- now 10000)})}
                        789 {:client-state (new-state-map
                                            {:last-time (- now 100000)})}}}))
  (prune-old-sessions 200000)
  (is (check (keys (:sessions @session-info)) (as-set [123 789])))
  (prune-old-sessions 20000)
  (is (check (keys (:sessions @session-info)) [123]))
  (prune-old-sessions 2000)
  (is (check (keys (:sessions @session-info)) nil )))

(deftest prune-unused-stores-test
  (let [stream (new java.io.StringReader "a, b")]
    (is (.ready stream))
    (reset! session-info {:sessions {123 {:store 1}
                                     789 {:store 1}}
                          :stores {:a {:store 1}
                                   :b {:store 2
                                       :log-agent (agent stream)}}})
    (prune-unused-stores)
    (is (check (keys (:stores @session-info)) [:a]))
    (Thread/sleep 10) ;; Wait for the agent to get the close message
    (is (try (.ready stream)
             (catch java.io.IOException e "Exception") )
        "Exception")))

(deftest forget-session-test
  (let [stream (new java.io.StringReader "a, b")
        store (add-table (starting-store nil) "Hello" [["a" "b"] [1 2] [3]])
        ms (new-mutable-store store)
        md (new-expression-manager-data)]
    (reset! session-info {:sessions {}
                          :stores {"/foo" {:store ms
                                          :log-agent (agent stream)}}})
    (let [state (ensure-session nil "//foo" nil md nil)]
      (is (= (vals (:sessions @session-info)) [state]))
      (is (seq (:subscriptions @(:manager-data ms))))
      (forget-session (first (keys (:sessions @session-info))))
      (is (= (:sessions @session-info) {}))
      (is (not (seq (:subscriptions @(:manager-data ms))))))))

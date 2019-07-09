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
             [mutable-store-impl :refer [stop-speculative-tracking]]
             [query :refer [matching-items]]
             [state-map :refer [new-state-map state-map-get-current-value]]
             [expression-manager :refer [new-expression-manager-data compute]]
             [task-queue :refer [new-priority-task-queue]])
            (cosheet.server
             [session-state :refer :all]
             [model-utils :refer [immutable-semantic-to-list
                                  starting-store add-table first-tab-R]])
            ; :reload
            ))

(deftest url-path-to-file-path-test
  (let [home (System/getProperty "user.home")]
    (is (= (url-path-to-file-path "foo/" "john") nil))
    (is (clojure.string/ends-with? (url-path-to-file-path "/cosheet/foo" "john")
                                   "/cosheet/userdata/john/foo"))))

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
  (let [queue (new-priority-task-queue 0)
        store (add-table (starting-store nil) "Hello" [["a" "b"] [1 2] [3]])
        state-map (create-client-state
                   (new-mutable-store store queue) "I5" queue)]
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
          {:sessions {123 {:client-state
                           (new-state-map {} (new-priority-task-queue 0))}}})
  (let [state (get-session-state 123)
        diff (- (System/currentTimeMillis)
                (state-map-get-current-value
                 (:client-state state) :last-time))]
    (is (#{0 1} diff))))

(deftest prune-old-sessions-test
  (let [now (System/currentTimeMillis)
        queue (new-priority-task-queue 0)]
    (reset! session-info
            {:sessions {123 {:client-state (new-state-map
                                            {:last-time (- now 10000)}
                                            queue)}
                        789 {:client-state (new-state-map
                                            {:last-time (- now 100000)}
                                            queue)}}}))
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
        store (add-table (starting-store nil)
                         "Hello" [["a" "b"] [1 2] [3]])
        queue (new-priority-task-queue 0)
        ms (new-mutable-store store queue)
        md (new-expression-manager-data queue)]
    (add-session-temporary-element! ms)
    (reset! session-info {:sessions {}
                          :stores {"/foo" {:store ms
                                          :log-agent (agent stream)}}})
    (let [state (ensure-session nil "/foo" nil queue md)]
      (is (= (vals (:sessions @session-info)) [state]))
      (is (seq (:subscriptions @(:manager-data ms))))
      (forget-session (first (keys (:sessions @session-info))))
      (is (= (:sessions @session-info) {}))
      (compute md)
      (stop-speculative-tracking ms)
      (is (not (seq (:subscriptions @(:manager-data ms))))))))

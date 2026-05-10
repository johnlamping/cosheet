(ns cosheet.server.session-state-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            [clojure.data.priority-map :as priority-map]
            (cosheet
             [utils :refer [parse-string-as-number]]
             [debug :refer [simplify-for-print]]
             [test-utils :refer [check any as-set]]
             [store :refer [->ItemId new-element-store new-mutable-store]]
             store-impl
             [reporter :refer [reporter-data reporter-value]]
             [query :refer [matching-items]]
             [canonical :refer [canonicalize]]
             [map-reporter :refer [make-map-reporter map-reporter-get-current]]
             [calculator :refer [make-calculator-data compute]]
             [task-queue :refer [make-priority-task-queue]])
            (cosheet.server
             [session-state :refer :all]
             [item-render :refer [render-item-DOM]]
             [model-utils :refer [semantic-to-list
                                  starting-store add-table]]
             [dom-manager :refer [relative-ids->client-id
                                  client-id->component]]
             [action-data :refer [get-id-action-data]])
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
        row1 (first (matching-items '(nil (1 ("a" :label))) store))
        row2 (first (matching-items '(nil (3 ("a" :label))) store))]
    (is (= (canonicalize (semantic-to-list row1))
           (canonicalize
            '("" ("Hello" :label) (1 ("a" :label)) (2 ("b" :label))))))
    (is (= (canonicalize (semantic-to-list row2))
           (canonicalize '("" ("Hello" :label) (3 ("a" :label))))))))

(deftest create-client-state-test
  (let [store (add-table (starting-store nil) "Hello" [["a" "b"] [1 2] [3]])
        state (create-client-state
               (new-mutable-store store) (->ItemId 5))]
    (is (check (reporter-value state)
               {:last-time (any)
                :root-id (->ItemId 5)
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
          {:sessions {123 {:client-state (make-map-reporter {})}}})
  (let [state (get-session-state 123)
        diff (- (System/currentTimeMillis)
                (map-reporter-get-current (:client-state state) :last-time))]
    (is (#{0 1} diff))))

(deftest prune-old-sessions-test
  (let [now (System/currentTimeMillis)
        queue (make-priority-task-queue 0)]
    (reset! session-info
            {:sessions {123 {:client-state (make-map-reporter
                                            {:last-time (- now 10000)})}
                        789 {:client-state (make-map-reporter
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

(deftest create-and-forget-session-test
  (let [stream (new java.io.StringReader "a, b")
        store (add-table (starting-store nil)
                         "Hello" [["a" "b"] [1 2] [3]])
        row1 (first (matching-items '(nil (1 ("a" :label))) store))
        ms (new-mutable-store store)
        queue (make-priority-task-queue 0)
        cd (make-calculator-data queue)]
    (add-session-ephemeral-element! ms)
    (reset! session-info {:sessions {}
                          :stores {"/foo" {:store ms
                                           :log-agent (agent stream)}}})
    (let [state (ensure-session 123 "/foo" (relative-ids->client-id
                                            [(:item-id row1)])
                                queue cd)
          dom-manager (:dom-manager state)]
      (is (= (vals (:sessions @session-info)) [state]))
      ;; Two attendees for the overall DOM, and three for each of the
      ;; two elements: the element, its content, and its label.
      (is (= (count (:attendees (reporter-data ms))) 9))
      (let [root-component (client-id->component @dom-manager "root")]
        (is (check (reporter-value (:dom-R @root-component))
                   [:div {}
                    [:component {:width 0.75,
                                 :template ""
                                 :relative-id (:item-id row1)
                                 :must-show-label true
                                 :render-dom render-item-DOM
                                 :get-action-data [get-id-action-data
                                                   (:item-id row1)]}]])))      
      (forget-session (first (keys (:sessions @session-info))))
      (is (= (:sessions @session-info) {}))
      (compute cd)
      (is (empty? (:attendees (reporter-data ms)))))))

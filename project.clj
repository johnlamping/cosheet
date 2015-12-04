(defproject cosheet "0.1.0-SNAPSHOT"
  :description "Free form spreadsheet"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.priority-map "0.0.5"]
                 ;; Server side
                 [ring/ring-core "1.3.2"]
                 [ring/ring-jetty-adapter "1.3.2"]
                 [ring-transit "0.1.3"]
                 [compojure "1.3.2"]
                 [hiccup "1.0.5"]
                 ;; Client side
                 [org.clojure/clojurescript "0.0-2913"]
                 [reagent "0.5.0-alpha3"]
                 [cljs-ajax "0.3.10"]
                 [com.keminglabs/cljx "0.6.0"]
                 ]
  :plugins [[lein-cljsbuild "1.0.5"]
            [lein-ring "0.9.2"]
            [cider/cider-nrepl "0.10.0"]]
  :profiles {:uberjar {:aot :all}
             :dev {:plugins [[com.keminglabs/cljx "0.6.0"]]}}
  :cljx {:builds [{:source-paths ["src_cljx"]
                   :output-path "target/classes"
                   :rules :clj}
                  {:source-paths ["src_cljx"]
                   :output-path "target/classes"
                   :rules :cljs}]}
  :prep-tasks [["cljx" "once"] "javac" "compile"]
  :source-paths ["src" "target/classes"]
  ;; Keep lein ring from reloading tests.
  :reload-paths ["src" "target/classes"] 
  :hooks [leiningen.cljsbuild]
  :cljsbuild {
    :builds [{:source-paths ["src_cljs" "target/classes"]
              :compiler {:output-to "resources/public/js/main.js"
                         :optimizations :whitespace
                         :pretty-print true
                         :preamble ["reagent/react.js"]}}]}
  :ring {:handler cosheet.server.routes/app}
  :main ^:skip-aot cosheet.core
  :target-path "target/%s")

(defproject cosheet "0.1.0-SNAPSHOT"
  :description "Free form spreadsheet"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"] ; 1.8.0
                 [org.clojure/data.priority-map "0.0.5"]
                 ;; Server side
                 [ring/ring-core "1.3.2"]
                 [ring/ring-jetty-adapter "1.3.2"]
                 [ring-transit "0.1.3"]
                 [compojure "1.3.4"]
                 [hiccup "1.0.5"]
                 ;; Client side
                 [org.clojure/clojurescript "0.0-2913"] ; 3211; 1.8.40
                 [reagent "0.5.0-alpha3"]
                 [cljs-ajax "0.3.10"]
                 [com.keminglabs/cljx "0.6.0"] ;; get rid of this.
                 ]
  :plugins [[lein-cljsbuild "1.0.5"] ; 1.1.3
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
  :hooks [leiningen.cljsbuild]
  :cljsbuild {
    :builds [{:source-paths ["src_cljs" "target/classes"]
              :compiler {:output-to "resources/public/js/main.js"
                         :optimizations :whitespace
                         :pretty-print true
                         :preamble ["reagent/react.js"]}}]}
  :ring {:handler cosheet.server.routes/app
         ;; Keep lein ring from reloading tests.
         :reload-paths ["src" "target/classes"]}
  :main ^:skip-aot cosheet.core
  :target-path "target/%s")

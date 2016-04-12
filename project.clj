(defproject cosheet "0.1.0-SNAPSHOT"
  :description "Free form spreadsheet"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"] ; 1.8.0
                 [org.clojure/data.priority-map "0.0.5"]
                 ;; Server side
                 [ring/ring-core "1.4.0"]
                 [ring/ring-jetty-adapter "1.3.2"]
                 [ring-transit "0.1.4"]
                 [compojure "1.3.4"]
                 [hiccup "1.0.5"]
                 ;; Client side
                 [org.clojure/clojurescript "1.7.170" :exclusions [org.apache.ant/ant]] ; 3211; 1.8.40
                 [reagent "0.5.1" :exclusions [org.clojure/tools.reader]]
                 [cljs-ajax "0.5.4" :exclusions [org.clojure/tools.reader]]
                 ]
  :plugins [[lein-cljsbuild "1.0.6"] ; 1.1.3
            [lein-ring "0.9.2"]
            [cider/cider-nrepl "0.10.0" :exclusions [org.clojure/tools.nrepl]]]
  :profiles {:uberjar {:aot :all}
             ;:dev {:plugins [[com.keminglabs/cljx "0.6.0"]]}
             }
  :prep-tasks ["javac" "compile"]
  :source-paths ["src" "src_cljc"]
  :hooks [leiningen.cljsbuild]
  :cljsbuild {:builds [{:source-paths ["src_cljc" "src_cljs"]
                        :compiler {:output-to "resources/public/js/main.js"
                                   :optimizations :whitespace
                                   :pretty-print true
                                   :preamble ["reagent/react.js"]}}]}
  :ring {:handler cosheet.server.routes/app
         ;; Keep lein ring from reloading tests.
         :reload-paths ["src" "src_cljc"]}
  :main ^:skip-aot cosheet.core
  :target-path "target/%s")

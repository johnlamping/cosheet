(defproject cosheet "0.1.0-SNAPSHOT"
  :description "Free form spreadsheet"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [ring/ring-core "1.3.2"]
                 [ring/ring-jetty-adapter "1.3.2"]
                 [org.clojure/clojurescript "0.0-2913"]
                 [compojure "1.3.2"]
                 [hiccup "1.0.5"]]
  :plugins [[lein-cljsbuild "1.0.5"]
            [lein-ring "0.9.2"]]
  :cljsbuild {
    :builds [{:source-paths ["src-cljs"]
              :compiler {:output-to "resources/public/js/main.js"
                         :optimizations :whitespace
                         :pretty-print true}}]}
  :ring {:handler cosheet.server.routes/app}
  :main ^:skip-aot cosheet.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
